# API-layer tests. The package (`app/test/runtests.jl`) covers Cecelia headless; this covers the thin
# HTTP adapters in `api/src`. We load the server module WITHOUT binding a socket (`CECELIA_NO_SERVE`)
# and call the handler functions directly — no live server, no ports, so it runs in CI headless.
#
# Run: `julia --project=api api/test/runtests.jl`  (or `pixi run test-api`).
ENV["CECELIA_NO_SERVE"] = "1"

using Test
include(joinpath(@__DIR__, "..", "src", "server.jl"))   # defines handlers + shared state; does not start
using JSON3

# call a POST handler the way the router does: JSON body → Vector{UInt8}
_post(f, obj) = f(Vector{UInt8}(JSON3.write(obj)))
_repl(code) = _post(api_repl, Dict("code" => code))

@testset "API: diagnostics" begin
    st, body = api_diagnostics(HTTP.Request("GET", "/api/diagnostics"))
    @test st == 200
    d = JSON3.read(body)
    @test d.threads >= 1
    @test !isempty(String(d.julia))
    @test haskey(d, :replAvailable) && haskey(d, :loopback) && haskey(d, :replEnabled)
    # service ports surfaced for the System panel
    @test d.port > 0 && d.napariPort == 7655 && d.notebooksPort == 7660
    # installed-build provenance (.cecelia-version at the install root); a source checkout has no
    # such file → the fallback string. Either way the field must be present and non-empty.
    @test haskey(d, :version) && !isempty(String(d.version))
    # first-launch setup flag drives the frontend /setup redirect
    @test haskey(d, :setupRequired) && d.setupRequired isa Bool
end

@testset "API: update scope" begin
    # _install_scope drives whether the in-app updater self-updates (user), defers to an admin
    # (system), or is hidden (dev checkout). Parameterised on a temp root so we don't touch _APP_ROOT.
    mktempdir() do root
        @test _install_scope(root) == "dev"                              # bare dir → not installed
        write(joinpath(root, "VERSION"), "v9.9.9")
        @test _install_scope(root) == "user"                             # installed, no marker → user
        write(joinpath(root, ".cecelia-scope"), "system\n")
        @test _install_scope(root) == "system"
        write(joinpath(root, ".cecelia-scope"), "user\n")
        @test _install_scope(root) == "user"
        mkdir(joinpath(root, ".git"))
        @test _install_scope(root) == "dev"                              # source checkout → never installed
    end
    # apply must be refused outside a user install — in this (dev/git) checkout that's a 4xx, and it
    # must NOT reach the network or stage anything.
    st, body = api_update_apply(Vector{UInt8}(JSON3.write(Dict("version" => "v9.9.9"))))
    @test st in (400, 403)
    @test haskey(JSON3.read(body), :error)
end

@testset "API: setup wizard" begin
    st, body = api_setup_defaults(HTTP.Request("GET", "/api/setup/defaults"))
    @test st == 200
    @test endswith(String(JSON3.read(body).projectsDir), "cecelia-projects")

    # validate is a pure check (no side effects)
    mktempdir() do tmp
        st, body = api_setup_validate(HTTP.Request("GET", "/api/setup/validate?path=$tmp"))
        d = JSON3.read(body)
        @test st == 200 && d.ok == true && d.willCreate == false
        st, body = api_setup_validate(HTTP.Request("GET", "/api/setup/validate?path=$(joinpath(tmp, "sub"))"))
        d = JSON3.read(body)
        @test d.ok == true && d.willCreate == true                 # child of an existing writable dir
    end
    @test JSON3.read(api_setup_validate(HTTP.Request("GET", "/api/setup/validate?path=notabsolute"))[2]).ok == false
    @test JSON3.read(api_setup_validate(HTTP.Request("GET", "/api/setup/validate"))[2]).ok == false

    # init writes custom.toml + hot-reloads config → isolate in a temp CECELIA_DEV_DIR, then restore
    mktempdir() do tmp
        proj = joinpath(tmp, "myprojects")
        try
            withenv("CECELIA_DEV_DIR" => tmp) do
                st, body = api_setup_init(Vector{UInt8}(JSON3.write(Dict("projectsDir" => proj))))
                d = JSON3.read(body)
                @test st == 200 && d.ok == true && d.restartRequired == false
                @test isdir(proj)                                  # created
                @test String(d.projectsDir) == proj                # hot-reloaded, no restart
                @test isfile(joinpath(tmp, "custom.toml"))
                @test api_setup_init(Vector{UInt8}("{}"))[1] == 400 # missing projectsDir → 400
            end
        finally
            init_cecelia!()   # restore the real dev/prod config regardless of outcome
        end
    end
end

@testset "API: app lifecycle" begin
    # dev detection + restart availability are pure env readers
    withenv("CECELIA_DEV" => nothing) do; @test _is_dev() == false; end
    withenv("CECELIA_DEV" => "1")     do; @test _is_dev() == true;  end
    withenv("CECELIA_DEV" => "0")     do; @test _is_dev() == false; end
    withenv("CECELIA_SUPERVISED" => nothing) do; @test _can_restart() == false; end
    withenv("CECELIA_SUPERVISED" => "1")     do; @test _can_restart() == true;  end

    # restart when NOT supervised → 409, and (crucially) must NOT exit the process.
    # (We never call api_app_shutdown, nor restart while supervised — those call exit().)
    st, body = withenv("CECELIA_SUPERVISED" => nothing) do
        api_app_restart(Vector{UInt8}("{}"))
    end
    @test st == 409
    @test haskey(JSON3.read(body), :error)

    # the console backfill endpoint is a safe read
    st2, body2 = api_logs_recent()
    @test st2 == 200
    @test haskey(JSON3.read(body2), :logs)
end

@testset "API: packages" begin
    st, body = api_packages(HTTP.Request("GET", "/api/diagnostics/packages"))
    @test st == 200
    d = JSON3.read(body)
    @test haskey(d, :julia) && haskey(d, :python) && haskey(d, :pythonError)
    # Julia list is in-process (Pkg.dependencies) → always populated & well-formed; the server dep set
    # includes HTTP.
    @test !isempty(d.julia)
    @test all(p -> haskey(p, :name) && haskey(p, :version), d.julia)
    @test any(p -> p.name == "HTTP", d.julia)
    # Python list comes from `pixi list`; it's populated when pixi is on PATH (it is under
    # `pixi run test-api`) and otherwise reports pythonError rather than throwing.
    if d.pythonError === nothing
        @test !isempty(d.python)
        @test all(p -> haskey(p, :name) && haskey(p, :version) && haskey(p, :kind), d.python)
    end
end

@testset "API: debug console gating" begin
    # disabled (default) → refused
    _repl_on[] = false; _BOUND_HOST[] = "127.0.0.1"
    st, _ = _repl("1 + 1"); @test st == 403
    @test !_repl_available()

    # enabled but the server is network-bound → refused (loopback is the hard gate)
    _repl_on[] = true; _BOUND_HOST[] = "0.0.0.0"
    st, body = _repl("1 + 1")
    @test st == 403
    @test occursin("loopback", JSON3.read(body).error)
    @test !_repl_available()

    # enabled AND loopback-bound → available
    _BOUND_HOST[] = "127.0.0.1"
    @test _repl_available()
end

@testset "API: debug console eval" begin
    _repl_on[] = true; _BOUND_HOST[] = "127.0.0.1"

    # value
    st, body = _repl("1 + 1")
    r = JSON3.read(body)
    @test st == 200 && r.ok == true && r.value == "2"

    # captured stdout + last value from a multi-statement block
    r = JSON3.read(_repl("println(\"hi\"); 3 + 4")[2])
    @test r.value == "7" && occursin("hi", r.output)

    # error path: ok=false + message, still HTTP 200
    r = JSON3.read(_repl("sqrt(-1)")[2])
    @test r.ok == false && occursin("DomainError", r.error)

    # empty code → 400
    @test _repl("   ")[1] == 400
end

@testset "API: repl config toggle" begin
    _BOUND_HOST[] = "127.0.0.1"
    st, body = _post(api_repl_config, Dict("enabled" => false))
    @test st == 200 && JSON3.read(body).replEnabled == false
    @test _repl("1+1")[1] == 403                      # now disabled

    st, body = _post(api_repl_config, Dict("enabled" => true))
    @test st == 200 && JSON3.read(body).replEnabled == true
    @test _repl("1+1")[1] == 200                      # enabled again
end

@testset "API: notebooks registry + versioning" begin
    # Pure name sanitisation: reject path-like input + dotfiles, accept plain names.
    @test _safe_nb_file("../evil") === nothing
    @test _safe_nb_file("a/b")     === nothing
    @test _safe_nb_file("a\\b")    === nothing
    @test _safe_nb_file(".hidden") === nothing
    @test _safe_nb_file("my nb")   == "my nb.jl"
    @test _safe_nb_file("a.b.jl")  == "a.b.jl"

    # Redirect projects_dir() → a temp dir so we never touch the real dev projects dir.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir()
    dirs["projects"] = tmp
    try
        uid = "TESTNB"
        mkpath(joinpath(tmp, uid))
        list()  = JSON3.read(api_notebooks_list(HTTP.Request("GET", "/api/notebooks?projectUid=$uid"))[2]).notebooks
        find(f) = (ns = filter(n -> n.file == f, list()); isempty(ns) ? nothing : ns[1])
        snaps() = JSON3.read(api_notebooks_snapshots(HTTP.Request("GET", "/api/notebooks/snapshots?projectUid=$uid&file=nb1.jl"))[2]).snapshots

        # create (+ duplicate-name 409, bad-name 400)
        @test _post(api_notebooks_create, Dict("projectUid"=>uid, "name"=>"nb1", "description"=>"first"))[1] == 200
        @test _post(api_notebooks_create, Dict("projectUid"=>uid, "name"=>"nb1"))[1] == 409
        @test _post(api_notebooks_create, Dict("projectUid"=>uid, "name"=>"../x"))[1] == 400
        nb = find("nb1.jl")
        @test nb !== nothing && nb.version == 0 && nb.description == "first"   # fresh → v0

        # snapshot advances the current-version pointer; number derived from disk
        @test JSON3.read(_post(api_notebooks_snapshot, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[2]).version == 1
        @test JSON3.read(_post(api_notebooks_snapshot, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[2]).version == 2
        @test find("nb1.jl").version == 2
        @test [s.version for s in snaps()] == [2, 1]

        # restore: pointer back to 1, no new snapshot, repeatable (no churn), bad version 404
        @test JSON3.read(_post(api_notebooks_restore, Dict("projectUid"=>uid,"file"=>"nb1.jl","version"=>1,"force"=>true))[2]).version == 1
        @test find("nb1.jl").version == 1
        @test _post(api_notebooks_restore, Dict("projectUid"=>uid,"file"=>"nb1.jl","version"=>1,"force"=>true))[1] == 200
        @test [s.version for s in snaps()] == [2, 1]
        @test _post(api_notebooks_restore, Dict("projectUid"=>uid,"file"=>"nb1.jl","version"=>99,"force"=>true))[1] == 404

        # next snapshot after restore = max-on-disk + 1 (→ 3, not "current+1")
        @test JSON3.read(_post(api_notebooks_snapshot, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[2]).version == 3

        # describe + duplicate
        @test _post(api_notebooks_describe, Dict("projectUid"=>uid,"file"=>"nb1.jl","description"=>"updated"))[1] == 200
        @test find("nb1.jl").description == "updated"
        @test JSON3.read(_post(api_notebooks_duplicate, Dict("projectUid"=>uid,"file"=>"nb1.jl","scope"=>"project"))[2]).file == "nb1-copy.jl"
        @test find("nb1-copy.jl") !== nothing

        # delete (server not running in tests → guard doesn't require force)
        @test !isempty(snaps())    # nb1 has snapshots on disk before delete
        @test _post(api_notebooks_delete, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[1] == 200
        @test find("nb1.jl") === nothing
        @test isempty(snaps())     # delete also removes the notebook's snapshot history

        # errors
        @test api_notebooks_list(HTTP.Request("GET", "/api/notebooks?projectUid=NOPE"))[1] == 404
        @test api_notebooks_list(HTTP.Request("GET", "/api/notebooks"))[1] == 400
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: notebooks sysimage status" begin
    # status always carries a `sysimage` field, one of the valid states (machine-independent: deps.so
    # may or may not exist here). Pins the response contract the frontend's first-run build reads.
    d = JSON3.read(api_notebooks_status(HTTP.Request("GET", "/api/notebooks/status"))[2])
    @test haskey(d, :sysimage)
    @test String(d.sysimage) in ("ready", "building", "error", "absent", "stale")

    # Pure staleness classifier — the update-safety logic, tested without touching disk.
    stamp(j, m) = "{\"julia\":\"$j\",\"manifest\":\"$m\"}"
    @test _classify_sysimage(false, nothing, false, false, "1.11", "abc") == "absent"
    @test _classify_sysimage(false, nothing, true,  false, "1.11", "abc") == "building"
    @test _classify_sysimage(false, nothing, false, true,  "1.11", "abc") == "error"
    @test _classify_sysimage(true,  nothing, false, false, "1.11", "abc") == "stale"     # unstamped ⇒ rebuild
    @test _classify_sysimage(true,  stamp("1.11","abc"), false, false, "1.11", "abc") == "ready"
    @test _classify_sysimage(true,  stamp("1.10","abc"), false, false, "1.11", "abc") == "stale"  # Julia bumped
    @test _classify_sysimage(true,  stamp("1.11","zzz"), false, false, "1.11", "abc") == "stale"  # Manifest changed
    @test _classify_sysimage(true,  stamp("1.11","abc"), true,  false, "1.11", "abc") == "ready"  # fresh wins over building
    @test _classify_sysimage(true,  stamp("1.10","abc"), true,  false, "1.11", "abc") == "building" # stale + rebuilding

    # status wiring reads the right paths: "ready" on disk iff the image exists AND its stamp matches
    # this Julia + Manifest (no build running in tests).
    onstamp = isfile(_sysimage_stamp()) ? read(_sysimage_stamp(), String) : nothing
    @test (_sysimage_status() == "ready") ==
          (isfile(_sysimage_path()) && _stamp_matches(onstamp, string(VERSION), _manifest_hash()))
end

@testset "API: module-canvas persistence" begin
    # Redirect projects_dir() → temp so we don't touch the dev projects dir.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTCANVAS"
        mkpath(joinpath(tmp, uid, "1", "IMG1"))   # the object (image) dir must exist
        write(joinpath(tmp, uid, "project.json"),
              JSON3.write((; uid = uid, name = "T", kind = "static", set_uids = String[])))
        entry = Dict("panels" => [], "activeId" => 0, "nextId" => 0, "arrangeSeq" => 0, "shared" => Dict())
        payload = Dict("projectUid" => uid, "objects" => Dict(
            "IMG1" => Dict("entries" => Dict("summary:behaviour:IMG1" => entry), "geom" => Dict())))
        # save writes 1/IMG1/moduleCanvases.json (with the object), verbatim
        @test _post(api_projects_canvases, payload)[1] == 200
        mc_file = joinpath(tmp, uid, "1", "IMG1", "moduleCanvases.json")
        @test isfile(mc_file)
        @test haskey(JSON3.read(read(mc_file, String)).entries, Symbol("summary:behaviour:IMG1"))
        # object dir absent → skipped (no crash, no stray file)
        @test _post(api_projects_canvases,
                    Dict("projectUid" => uid, "objects" => Dict("GHOST" => Dict("entries" => Dict(), "geom" => Dict()))))[1] == 200
        @test !isfile(joinpath(tmp, uid, "1", "GHOST", "moduleCanvases.json"))
        # load reassembles the per-object files into one keyed map
        st, body = api_projects_load(Vector{UInt8}(JSON3.write(Dict("uid" => uid))))
        @test st == 200
        mc = JSON3.read(body).moduleCanvases
        @test mc !== nothing && haskey(mc.entries, Symbol("summary:behaviour:IMG1"))
        # error paths
        @test _post(api_projects_canvases, Dict("objects" => Dict()))[1] == 400          # no projectUid
        @test _post(api_projects_canvases, Dict("projectUid" => "NOPE", "objects" => Dict()))[1] == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: plotmeta gate-autoscale helpers" begin
    # _gates_bbox: display-space bbox over a mixed rectangle + polygon gate list
    @test _gates_bbox([]) == (Inf, -Inf, Inf, -Inf)          # nothing to enclose
    rect = Dict{String,Any}("kind" => "rectangle", "x_min" => 1.0, "x_max" => 3.0,
                            "y_min" => -2.0, "y_max" => 0.5)
    poly = Dict{String,Any}("kind" => "polygon", "vertices" => [[5.0, 1.0], [6.0, -4.0], [4.5, 2.0]])
    @test _gates_bbox([rect]) == (1.0, 3.0, -2.0, 0.5)
    bb = _gates_bbox([rect, poly])
    @test bb == (1.0, 6.0, -4.0, 2.0)                        # union across both gate kinds

    # _include_range: only the side a gate actually exceeds moves; margin = fraction of the span
    @test _include_range((0.0, 10.0), Inf, -Inf) == (0.0, 10.0)   # no finite gate → unchanged
    @test _include_range((0.0, 10.0), 2.0, 8.0)  == (0.0, 10.0)   # gate inside → unchanged
    lo, hi = _include_range((0.0, 10.0), -5.0, 20.0)              # exceeds both sides
    @test lo == -5.0 - 0.5 && hi == 20.0 + 0.5                    # margin = 0.05 * span(10) = 0.5
    lo2, hi2 = _include_range((0.0, 10.0), -5.0, 8.0)            # exceeds low side only
    @test lo2 == -5.5 && hi2 == 10.0
end

@testset "API: lab log" begin
    # Redirect projects_dir() → a temp dir so we never touch the real dev projects dir.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir()
    dirs["projects"] = tmp
    try
        proj = create_project!(name="api-lablog", kind="static")
        uid  = proj.uid
        read_ll() = JSON3.read(api_lablog_read(HTTP.Request("GET", "/api/lablog?projectUid=$uid"))[2])

        # empty to start
        r0 = read_ll()
        @test r0.content == "" && length(r0.entries) == 0

        # bad requests
        @test api_lablog_read(HTTP.Request("GET", "/api/lablog"))[1] == 400              # projectUid missing
        @test _post(api_lablog_append, Dict("projectUid"=>uid))[1] == 400                # author+lines missing
        @test _post(api_lablog_append, Dict("author"=>"User","lines"=>"x"))[1] == 400    # projectUid missing
        @test _post(api_lablog_append, Dict("projectUid"=>"nope","author"=>"User","lines"=>"x"))[1] == 404

        # append accepts a string OR an array; server injects the date + author tag
        @test _post(api_lablog_append, Dict("projectUid"=>uid,"author"=>"User","lines"=>"single line"))[1] == 200
        st, body = _post(api_lablog_append, Dict("projectUid"=>uid,"author"=>"Claude","lines"=>["a","b"]))
        @test st == 200
        j = JSON3.read(body)
        @test startswith(j.block, "## ") && occursin("[Claude]", j.block)
        @test length(j.entries) == 2 && j.entries[1].author == "Claude"   # newest-first

        # empty/whitespace-only content rejected by append_lab_log! → 400
        @test _post(api_lablog_append, Dict("projectUid"=>uid,"author"=>"User","lines"=>["   "]))[1] == 400

        # read reflects appends
        r = read_ll()
        @test occursin("[User]", r.content) && occursin("[Claude]", r.content)
        @test length(r.entries) == 2

        # ── capture (auto [Cecelia] activity digest) ──
        # no task activity yet → captured=false, nothing appended
        let cap = JSON3.read(_post(api_lablog_capture, Dict("projectUid"=>uid))[2])
            @test cap.ok == true && cap.captured == false
        end
        # add run-log activity, then capture → captured=true with a [Cecelia] entry
        let s = add_set!(proj; name="set-A"),
            img = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
            append_run_log!(img, "segment.cellpose", "default")
            cap = JSON3.read(_post(api_lablog_capture, Dict("projectUid"=>uid))[2])
            @test cap.captured == true
            @test occursin("[Cecelia]", cap.block)
            @test any(e -> e.author == "Cecelia", cap.entries)
        end
        # bad requests
        @test _post(api_lablog_capture, Dict())[1] == 400              # projectUid missing
        @test _post(api_lablog_capture, Dict("projectUid"=>"nope"))[1] == 404

        # ── tuning ratings (entry-type feedback → config sidecar) ──
        let t = JSON3.read(_post(api_lablog_tune, Dict("projectUid"=>uid, "id"=>"abc123", "vote"=>"down"))[2])
            @test t.ok == true && t.tuning.abc123 == "down"
        end
        # surfaced on read
        @test JSON3.read(api_lablog_read(HTTP.Request("GET", "/api/lablog?projectUid=$uid"))[2]).tuning.abc123 == "down"
        # clear
        let t = JSON3.read(_post(api_lablog_tune, Dict("projectUid"=>uid, "id"=>"abc123", "vote"=>""))[2])
            @test !haskey(t.tuning, :abc123)
        end
        # bad requests
        @test _post(api_lablog_tune, Dict("projectUid"=>uid, "id"=>"x", "vote"=>"sideways"))[1] == 400
        @test _post(api_lablog_tune, Dict("projectUid"=>uid, "vote"=>"up"))[1] == 400   # id missing
        @test _post(api_lablog_tune, Dict("id"=>"x", "vote"=>"up"))[1] == 400           # projectUid missing

        # ── mutes (category suppression → config sidecar); categories are task-manager tags ──
        let m = JSON3.read(_post(api_lablog_mute, Dict("projectUid"=>uid, "category"=>"Segment", "muted"=>true))[2])
            @test m.ok == true && "Segment" in m.mutes
        end
        # read exposes the mutes and the two category groups the panel's mute chips render (module pages
        # vs operations) — both dynamic from task specs.
        let r = JSON3.read(api_lablog_read(HTTP.Request("GET", "/api/lablog?projectUid=$uid"))[2])
            @test "Segment" in r.mutes
            @test "Segment" in r.pageCategories && "Gating" in r.pageCategories   # module pages
            @test "Edit" in r.operationCategories                                 # operations group
        end
        let m = JSON3.read(_post(api_lablog_mute, Dict("projectUid"=>uid, "category"=>"Segment", "muted"=>false))[2])
            @test !("Segment" in m.mutes)
        end
        @test _post(api_lablog_mute, Dict("projectUid"=>uid, "category"=>"", "muted"=>true))[1] == 400   # empty rejected
        @test _post(api_lablog_mute, Dict("projectUid"=>uid, "muted"=>true))[1] == 400                   # category missing

        # ── dismiss (hide an entry → config sidecar; the log file stays append-only) ──
        let d = JSON3.read(_post(api_lablog_dismiss, Dict("projectUid"=>uid, "id"=>"ff00aa", "dismissed"=>true))[2])
            @test d.ok == true && "ff00aa" in d.dismissed
        end
        @test "ff00aa" in JSON3.read(api_lablog_read(HTTP.Request("GET", "/api/lablog?projectUid=$uid"))[2]).dismissed  # surfaced on read
        let d = JSON3.read(_post(api_lablog_dismiss, Dict("projectUid"=>uid, "id"=>"ff00aa", "dismissed"=>false))[2])
            @test !("ff00aa" in d.dismissed)                                                             # un-hidden
        end
        @test _post(api_lablog_dismiss, Dict("projectUid"=>uid, "dismissed"=>true))[1] == 400            # id missing
        @test _post(api_lablog_dismiss, Dict("id"=>"x", "dismissed"=>true))[1] == 400                    # projectUid missing
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

@testset "API: project delete" begin
    # Redirect projects_dir() → a temp dir so we never touch the real dev projects dir.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name="api-del", kind="static")
        uid  = proj.uid
        @test isdir(proj.root)

        @test _post(api_projects_delete, Dict())[1] == 400                    # uid missing
        @test _post(api_projects_delete, Dict("uid"=>"nope"))[1] == 404       # not found
        st, body = _post(api_projects_delete, Dict("uid"=>uid))
        @test st == 200 && JSON3.read(body).ok == true
        @test !isdir(proj.root)                                               # gone from disk
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

@testset "API: task log + history" begin
    # Redirect projects_dir() → a temp dir so we never touch the real dev projects dir.
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir()
    dirs["projects"] = tmp
    try
        proj = create_project!(name="api-tasklog", kind="static")
        uid  = proj.uid
        s    = add_set!(proj; name="set-A")
        img1 = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))
        img2 = add_image!(s; name="img-2", meta=Dict{String,Any}("ori_path"=>"/tmp/b.tif"))

        # ── image list (read-only, no lastOpenedAt bump) ──
        let r = JSON3.read(api_images_list(HTTP.Request("GET", "/api/images?projectUid=$uid"))[2])
            @test r.name == "api-tasklog" && r.count == 2
            @test length(r.sets) == 1 && r.sets[1].imageCount == 2
            names = [i.name for i in r.images]
            @test "img-1" in names && "img-2" in names
            @test all(i -> i.setName == "set-A", r.images)
            @test all(i -> i.included == true, r.images)          # included surfaced (default true)
        end
        # excluding an image surfaces as included:false (so the observer can see the silent member)
        img1.included = false; save!(img1)
        let r = JSON3.read(api_images_list(HTTP.Request("GET", "/api/images?projectUid=$uid"))[2])
            byname = Dict(i.name => i for i in r.images)
            @test byname["img-1"].included == false && byname["img-2"].included == true
        end
        img1.included = true; save!(img1)                          # restore for the rest of the testset
        @test api_images_list(HTTP.Request("GET", "/api/images"))[1] == 400          # projectUid missing
        @test api_images_list(HTTP.Request("GET", "/api/images?projectUid=nope"))[1] == 404

        # ── task log ──
        tl(q) = api_images_tasklog(HTTP.Request("GET", "/api/images/tasklog?$q"))
        # no log yet → exists=false, empty content
        let r = JSON3.read(tl("projectUid=$uid&imageUid=$(img1.uid)&fun=segment.cellpose")[2])
            @test r.exists == false && r.content == ""
        end
        # write a log the way the scheduler's _wrap_log_with_file would, then read it back
        logdir = joinpath(img1._dir, "logs"); mkpath(logdir)
        write(joinpath(logdir, "segment.cellpose.log"), "[2026-07-15 10:00:00] running cellpose\n")
        let r = JSON3.read(tl("projectUid=$uid&imageUid=$(img1.uid)&fun=segment.cellpose")[2])
            @test r.exists == true && occursin("running cellpose", r.content)
        end
        # bad requests + path-traversal guard (%2F decodes to '/', so fun becomes "../secret")
        @test tl("")[1] == 400                                             # projectUid missing
        @test tl("projectUid=$uid&imageUid=$(img1.uid)")[1] == 400         # fun missing
        @test tl("projectUid=$uid&imageUid=$(img1.uid)&fun=..%2Fsecret")[1] == 400   # traversal blocked
        @test tl("projectUid=$uid&imageUid=nope&fun=x")[1] == 404          # image missing
        @test tl("projectUid=nope&imageUid=$(img1.uid)&fun=x")[1] == 404   # project missing

        # ── task history ──
        hist(q) = api_tasks_history(HTTP.Request("GET", "/api/tasks/history?$q"))
        # empty when no run-log activity
        let r = JSON3.read(hist("projectUid=$uid")[2])
            @test r.count == 0 && length(r.history) == 0
        end
        # activity across two images, aggregated — including a FAILED run (visible to the observer)
        append_run_log!(img1, "segment.cellpose", "default")
        append_run_log!(img2, "tracking.bayesian_tracking", "default", "failed")
        let r = JSON3.read(hist("projectUid=$uid")[2])
            @test r.count == 2
            funs = [h.fun for h in r.history]
            @test "segment.cellpose" in funs && "tracking.bayesian_tracking" in funs
            @test all(h -> h.imageUid in (img1.uid, img2.uid), r.history)
            # per-run outcome surfaced under runStatus (distinct from the image's `status`)
            byfun = Dict(String(h.fun) => String(h.runStatus) for h in r.history)
            @test byfun["segment.cellpose"] == "done"          # default
            @test byfun["tracking.bayesian_tracking"] == "failed"
        end
        # limit caps rows
        @test JSON3.read(hist("projectUid=$uid&limit=1")[2]).count == 1
        # bad requests
        @test hist("")[1] == 400                     # projectUid missing
        @test hist("projectUid=nope")[1] == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

@testset "API: batch-movie output naming" begin
    attr = Dict("Day" => "3", "Treatment" => "CNO", "Blank" => "  ")
    # attrs joined in the requested order, uid always terminates → unique name
    @test _movie_basename(attr, "AbC123", ["Day", "Treatment"]) == "3_CNO_AbC123.mp4"
    # no attrs → just the uid
    @test _movie_basename(attr, "AbC123", String[]) == "AbC123.mp4"
    # blank / missing attr values are dropped (never leaves a dangling separator)
    @test _movie_basename(attr, "AbC123", ["Blank", "Missing", "Day"]) == "3_AbC123.mp4"
    # unsafe characters in an attr value are sanitised to underscores
    @test _movie_basename(Dict("T" => "a/b c:d"), "u1", ["T"]) == "a_b_c_d_u1.mp4"
end

# The single-image recorders (timelapse / animation) name by IMAGE via the shared _movies_dir +
# _movie_named_path (img._dir = {proj}/1/{uid} → {proj}/movies/). Mock img with a NamedTuple.
@testset "API: single-image movie naming" begin
    mktempdir() do tmp
        img = (; _dir = joinpath(tmp, "proj", "1", "uid7"), name = "My Image")
        @test _movie_named_path(img, "uid7") == joinpath(tmp, "proj", "movies", "My_Image.mp4")
        @test _movie_named_path(img, "uid7"; suffix = "_animation") ==
              joinpath(tmp, "proj", "movies", "My_Image_animation.mp4")
        @test isdir(joinpath(tmp, "proj", "movies"))   # _movies_dir created it
        # blank / unsafe name falls back to the uid
        blank = (; _dir = joinpath(tmp, "proj", "1", "uid7"), name = "   ")
        @test _movie_named_path(blank, "uid7") == joinpath(tmp, "proj", "movies", "uid7.mp4")
    end
end

# Observer (mcp/) event broadcasts — Slice B. Capture WS frames by registering a private queue in
# `_ws_clients` (broadcast_ws puts a serialised frame per client). These frames drive the observer's
# 10-attempts pattern + note/lab-log surfacing (docs/ai-assist/OBSERVER.md §4-5).
@testset "API: observer event broadcasts" begin
    # register a capture client; drain returns the parsed frames seen since the last drain
    cap = Channel{String}(64)
    key = gensym("test-observer")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    drain() = (frames = []; while isready(cap); push!(frames, JSON3.read(take!(cap))); end; frames)

    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir()
    dirs["projects"] = tmp
    try
        proj = create_project!(name="api-observer", kind="static")
        uid  = proj.uid
        s    = add_set!(proj; name="set-A")
        img  = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))

        # ── ws_status carries `fun` so a module-page run is attributable to a function ──
        drain()
        ws_status(nothing, "task-1", "done", img.uid; fun="segment.cellpose")
        let f = drain()
            @test length(f) == 1
            @test f[1].type == "task:status" && f[1].fun == "segment.cellpose"
            @test f[1].status == "done" && f[1].imageUid == img.uid
        end

        # ── image_note_added fires when a note is set ──
        drain()
        @test _post(api_images_inclusion_set,
                    Dict("projectUid"=>uid, "values"=>Dict(img.uid=>Dict("note"=>"odd cells"))))[1] == 200
        let f = drain()
            note = filter(x -> x.type == "image_note_added", f)
            @test length(note) == 1
            @test note[1].imageUid == img.uid && note[1].note == "odd cells" && note[1].projectUid == uid
        end
        # setting only `included` (no note) does NOT broadcast a note event
        drain()
        @test _post(api_images_inclusion_set,
                    Dict("projectUid"=>uid, "values"=>Dict(img.uid=>Dict("included"=>false))))[1] == 200
        @test isempty(filter(x -> x.type == "image_note_added", drain()))

        # ── lab_log_entry_added fires for USER entries only (anti-loop); lab_log_updated (the panel-
        # reload signal) fires for EVERY append so an external Chat-to-Claude append still refreshes ──
        drain()
        @test _post(api_lablog_append, Dict("projectUid"=>uid, "author"=>"User", "lines"=>["switched to diam 30"]))[1] == 200
        let f = drain()
            ea = filter(x -> x.type == "lab_log_entry_added", f)
            @test length(ea) == 1 && occursin("diam 30", ea[1].summary) && ea[1].projectUid == uid
            @test length(filter(x -> x.type == "lab_log_updated" && x.projectUid == uid, f)) == 1
        end
        # the observer's own [Claude] append must NOT re-broadcast entry_added (would loop) — but it
        # STILL emits lab_log_updated so an open panel reloads (the external-append bug fix)
        drain()
        @test _post(api_lablog_append, Dict("projectUid"=>uid, "author"=>"Claude", "lines"=>["noted"]))[1] == 200
        let f = drain()
            @test isempty(filter(x -> x.type == "lab_log_entry_added", f))
            @test length(filter(x -> x.type == "lab_log_updated", f)) == 1
        end
        # [Cecelia] auto-digests: no entry_added either, but still a panel reload
        drain()
        @test _post(api_lablog_append, Dict("projectUid"=>uid, "author"=>"Cecelia", "lines"=>["digest"]))[1] == 200
        let f = drain()
            @test isempty(filter(x -> x.type == "lab_log_entry_added", f))
            @test length(filter(x -> x.type == "lab_log_updated", f)) == 1
        end
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

@testset "API: bad-param launch still emits [ERROR] + terminal failed frame" begin
    # run_task validates params FIRST and throws before any job runs. handle_task_run must catch that
    # and STILL emit a task log + a terminal task:status:failed frame — otherwise the throw dies in
    # the @spawn silently and the observer's "Watch" auto-trigger (which keys off the terminal frame)
    # never fires. This is the regression the HMM-with-no-params case exposed.
    cap = Channel{String}(64)
    key = gensym("test-taskfail")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    drain() = (fs = Any[]; while isready(cap); push!(fs, JSON3.read(take!(cap))); end; fs)

    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name="api-taskfail", kind="static")
        s    = add_set!(proj; name="set-A")
        img  = add_image!(s; name="img-1", meta=Dict{String,Any}("ori_path"=>"/tmp/a.tif"))

        # importImages.omezarr with pyramidScale=99 fails validate_params (min/max) before any job.
        drain()
        handle_task_run(nothing, Dict{Symbol,Any}(
            :taskId => "t-fail", :funName => "importImages.omezarr",
            :projectUid => proj.uid, :imageUid => img.uid,
            :params => Dict{String,Any}("pyramidScale" => 99)))

        # the handler runs the task on a @spawn — poll until the terminal frame lands (or time out)
        frames = Any[]
        for _ in 1:200
            append!(frames, drain())
            any(f -> f.type == "task:status" && f.status == "failed", frames) && break
            sleep(0.05)
        end
        status = filter(f -> f.type == "task:status", frames)
        @test any(f -> f.status == "failed" && f.fun == "importImages.omezarr", status)
        # the [ERROR] log names the offending param → confirms we reached (and reported) validation
        errs = filter(f -> f.type == "task:log" && occursin("[ERROR]", String(f.line)), frames)
        @test any(f -> occursin("pyramidScale", String(f.line)), errs)
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive=true, force=true)
    end
end

@testset "API: custom modules status/reload" begin
    # Read-only status: shape is { dir, modules: [...], categories: [...] }; dir is <config_dir>/modules.
    st, body = api_custom_modules_status(HTTP.Request("GET", "/api/tasks/custom-modules"))
    @test st == 200
    d = JSON3.read(body)
    @test endswith(String(d.dir), joinpath("modules"))
    @test haskey(d, :modules)
    @test haskey(d, :categories)   # drives the generic new-category page + "Custom" nav group

    # Reload rescans; with no modules dir present it returns empty lists, never errors.
    st2, body2 = api_custom_modules_reload(Vector{UInt8}("{}"))
    @test st2 == 200
    d2 = JSON3.read(body2)
    @test haskey(d2, :loaded) && haskey(d2, :failed) && haskey(d2, :categories)
end

# Observer (in-app AI assistant) — status shape + request validation. The actual agent spawn (a real
# billed CLI call) is NOT exercised here; only the guard rails around it. See
# docs/todo/OBSERVER_INTEGRATION_PLAN.md + app/src/ai/agent_runner.jl (pure pieces tested in app/test).
@testset "API: observer status + feedback validation" begin
    # status: availability is a bool (true/false depending on whether `claude` is on PATH — don't
    # assert which, so it passes both in CI and on a dev box with Claude Code installed).
    st, body = api_observer_status(HTTP.Request("GET", "/api/observer/status"))
    @test st == 200
    let s = JSON3.read(body)
        @test s.available isa Bool
        # the picker's choices + shipped default are exposed so the panel can populate the dropdown
        @test Set(String.(s.models)) == Set(["haiku", "sonnet", "opus"])
        @test String(s.defaultModel) in Set(["haiku", "sonnet", "opus"])
    end

    # feedback: validated before anything is spawned.
    @test _post(api_observer_feedback, Dict())[1] == 400                       # projectUid missing
    @test _post(api_observer_feedback, Dict("projectUid" => "nope"))[1] == 404 # unknown project

    # clear context: same validation, no spawn.
    @test _post(api_observer_clear, Dict())[1] == 400                          # projectUid missing
    @test _post(api_observer_clear, Dict("projectUid" => "nope"))[1] == 404    # unknown project
end

@testset "API: cohort QC" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _qc(t) = api_qc_cohort(HTTP.Request("GET", "/api/qc/cohort" * t))
    _check(b) = api_qc_cohort_check(Vector{UInt8}(JSON3.write(b)))
    try
        proj = create_project!(name = "api-cohort", kind = "static")
        s    = add_set!(proj; name = "set-A")
        for (nm, n) in [("i1", 800), ("i2", 810), ("i3", 790), ("i4", 805)]
            img = add_image!(s; name = nm, meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
            write_qc(img, "segment.measureLabels", "default", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nCells" => n))
        end
        # clustering banks PER LABEL SET (T/B), not "default" — bank some so discovery has >1 value_name
        for (i, img) in enumerate(images(s))
            write_qc(img, "clustTracks.cluster", "T", Dict{String,Any}[]; metrics = Dict{String,Any}("nTracks" => 40))
            write_qc(img, "clustTracks.cluster", "B", Dict{String,Any}[];
                     metrics = Dict{String,Any}("nTracks" => i == 1 ? 9 : 23))   # i1 sparse in B
        end
        base = "?projectUid=$(proj.uid)&setUid=$(s.uid)"
        sidecar = joinpath(tmp, proj.uid, "1", s.uid, "qc", "cohort",
                           "segment.measureLabels", "default.json")
        # GET validation
        @test _qc("")[1] == 400                                              # missing params
        @test _qc("$base&funName=bad.fun")[1] == 400                         # not a metric producer
        @test _qc("?projectUid=$(proj.uid)&setUid=nope&funName=segment.measureLabels")[1] == 404
        # GET with an explicit valueName → single doc; READ-ONLY (no sidecar)
        st, body = _qc("$base&funName=segment.measureLabels&valueName=default")
        @test st == 200
        d = JSON3.read(body)
        @test d.nIncluded == 4 && d.metrics.nCells.n == 4
        @test d.metrics.nCells.mean == 801.25                               # (800+810+790+805)/4
        @test !isfile(sidecar)                                              # a GET must not write
        # GET with NO valueName → per-value_name map (byValueName). segment banks under "default"…
        dv = JSON3.read(_qc("$base&funName=segment.measureLabels")[2])
        @test collect(dv.valueNames) == ["default"] && dv.byValueName.default.nIncluded == 4
        # …clustering under T and B — both discovered, the sparse i1 flags in B only
        dc = JSON3.read(_qc("$base&funName=clustTracks.cluster")[2])
        @test Set(String.(dc.valueNames)) == Set(["B", "T"])
        i1 = images(s)[1].uid
        @test haskey(dc.byValueName.B.metrics.nTracks.outliers, Symbol(i1))
        @test isempty(dc.byValueName.T.metrics.nTracks.outliers)
        # POST /check (no valueName) → checks every label set, persists each sidecar
        @test _check((;))[1] == 400                                          # missing params
        @test _check((; projectUid = proj.uid, setUid = s.uid, funName = "bad.fun"))[1] == 400
        stc, bc = _check((; projectUid = proj.uid, setUid = s.uid, funName = "segment.measureLabels"))
        @test stc == 200 && isfile(sidecar)
        @test haskey(JSON3.read(bc), :byValueName)
        _check((; projectUid = proj.uid, setUid = s.uid, funName = "clustTracks.cluster"))
        @test isfile(joinpath(tmp, proj.uid, "1", s.uid, "qc", "cohort", "clustTracks.cluster", "B.json"))
        # the cross-image detail lands in the lab log under a "[Cecelia — Cohort check]" entry, by image
        # NAME (i1), with the label set and value-vs-median — not just a bare count
        ll = JSON3.read(api_lablog_read(HTTP.Request("GET", "/api/lablog?projectUid=$(proj.uid)"))[2]).content
        @test occursin("Cohort check", ll) && occursin("clustTracks.cluster (B)", ll)
        @test occursin("i1 — nTracks", ll) && occursin("cohort median", ll)   # image NAME + detail
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: cohort runs (per clustering run selector)" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _runs(t)  = api_qc_cohort_runs(HTTP.Request("GET", "/api/qc/cohort/runs" * t))
    _check(b) = api_qc_cohort_check(Vector{UInt8}(JSON3.write(b)))
    try
        proj = create_project!(name = "api-cohort-runs", kind = "static")
        s    = add_set!(proj; name = "set-A")
        imgs = [add_image!(s; name = "i$i", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif")) for i in 1:3]
        # two clustering RUNS (movement, test) over label sets T & B, banked via write_cluster_qc! so the
        # composite {labelSet}.{suffix} keys + runSuffix land on disk (what the real task does)
        mkqc(path) = open(path, "w") do io
            JSON3.write(io, Dict("nClusters" => 4, "perSegment" =>
                [Dict("uID" => img.uid, "valueName" => vn, "n" => 40, "nClusters" => 4, "largestClusterFrac" => 0.4)
                 for img in imgs for vn in ("T", "B")]))
        end
        qcdir = mktempdir()
        for suf in ("movement", "test")
            p = joinpath(qcdir, "$suf.json"); mkqc(p)
            Cecelia.write_cluster_qc!(collect(images(s)), "clustTracks.cluster", p; unit = "tracks", suffix = suf)
        end
        base = "?projectUid=$(proj.uid)&setUid=$(s.uid)"
        # GET /runs → both runs, each with its composite value_names
        str, br = _runs("$base&funName=clustTracks.cluster")
        @test str == 200
        rr = JSON3.read(br)
        @test Set(r.run for r in rr.runs) == Set(["movement", "test"])
        testrun = first(r for r in rr.runs if r.run == "test")
        @test sort(String.(testrun.valueNames)) == ["B.test", "T.test"]
        @test isempty(JSON3.read(_runs("$base&funName=segment.cellpose")[2]).runs)   # a fun with no runs → []
        @test _runs("?projectUid=$(proj.uid)&setUid=$(s.uid)")[1] == 400             # missing funName
        # POST /check with run=test persists ONLY the test run's sidecars, not movement's
        _check((; projectUid = proj.uid, setUid = s.uid, funName = "clustTracks.cluster", run = "test"))
        cdir = joinpath(tmp, proj.uid, "1", s.uid, "qc", "cohort", "clustTracks.cluster")
        @test isfile(joinpath(cdir, "T.test.json")) && isfile(joinpath(cdir, "B.test.json"))
        @test !isfile(joinpath(cdir, "T.movement.json"))
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: analysis lineage" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _lin(t) = api_analysis_lineage(HTTP.Request("GET", "/api/analysis/lineage" * t))
    try
        proj = create_project!(name = "api-lineage", kind = "live")
        s    = add_set!(proj; name = "set-A")
        img  = add_image!(s; name = "i1", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
        append_run_log!(img, "importImages.omezarr", "default", "done")
        append_run_log!(img, "segment.cellpose", "default", "done")
        append_run_log!(img, "tracking.bayesian_tracking", "default", "failed")
        @test _lin("")[1] == 400                                            # missing projectUid
        @test _lin("?projectUid=nope")[1] == 404
        st, body = _lin("?projectUid=$(proj.uid)")
        @test st == 200
        d = JSON3.read(body)
        @test d.projectUid == proj.uid && length(d.images) == 1
        e = d.images[1]
        @test [String(x.stage) for x in e.steps] == ["import", "segment", "track"]   # ordered pipeline
        @test any(x -> x.status == "failed", e.steps)                                # the failed track surfaces
        @test "import" in d.rollup.pipeline && "track" in d.rollup.pipeline
        # scope to one image
        @test length(JSON3.read(_lin("?projectUid=$(proj.uid)&imageUid=$(img.uid)")[2]).images) == 1
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: analysis populations" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _pops(t) = api_analysis_populations(HTTP.Request("GET", "/api/analysis/populations" * t))
    try
        proj = create_project!(name = "api-pops", kind = "live")
        s    = add_set!(proj; name = "set-A")
        img  = add_image!(s; name = "i1", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
        img.label_props = Dict("A" => "A.h5ad"); save!(img)
        m = Cecelia.PopulationMap(; pop_type = "flow", value_name = "A")
        Cecelia.add_pop!(m, "CD3"; gate = Cecelia.RectangleGate("c1", "c2", 0.0, 1.0, 0.0, 1.0))
        Cecelia.save_pop_map!(m, img)
        @test _pops("")[1] == 400                                            # missing projectUid
        @test _pops("?projectUid=nope")[1] == 404
        st, body = _pops("?projectUid=$(proj.uid)")
        @test st == 200
        d = JSON3.read(body)
        @test d.projectUid == proj.uid && length(d.images) == 1
        pops = d.images[1].populations
        cd3 = pops[findfirst(p -> p.name == "CD3", pops)]
        @test cd3.popType == "flow" && cd3.gate.kind == "rectangle" && cd3.gate.x_channel == "c1"
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: analysis measures" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _meas(t) = api_analysis_measures(HTTP.Request("GET", "/api/analysis/measures" * t))
    try
        proj = create_project!(name = "api-measures", kind = "live")
        s    = add_set!(proj; name = "set-A")
        add_image!(s; name = "i1", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
        @test _meas("")[1] == 400                                            # missing projectUid
        @test _meas("?projectUid=nope")[1] == 404
        # 200 + shape; no label props on disk → summaries empty (the deep read path is the pkg fixture test)
        st, body = _meas("?projectUid=$(proj.uid)")
        @test st == 200
        d = JSON3.read(body)
        @test d.projectUid == proj.uid && length(d.images) == 1
        @test haskey(d.images[1], :summaries) && haskey(d.images[1], :truncated)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: analysis behaviour + clusters" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _beh(t) = api_analysis_behaviour(HTTP.Request("GET", "/api/analysis/behaviour" * t))
    _clu(t) = api_analysis_clusters(HTTP.Request("GET", "/api/analysis/clusters" * t))
    try
        proj = create_project!(name = "api-behclust", kind = "live")
        s    = add_set!(proj; name = "set-A")
        add_image!(s; name = "i1", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
        @test _beh("")[1] == 400 && _clu("")[1] == 400                        # missing projectUid
        @test _beh("?projectUid=nope")[1] == 404 && _clu("?projectUid=nope")[1] == 404
        # 200 + shape; no obs on disk → empty lists (the read path is validated off-suite / pkg fixture)
        bd = JSON3.read(_beh("?projectUid=$(proj.uid)")[2])
        @test bd.projectUid == proj.uid && length(bd.images) == 1 && haskey(bd.images[1], :behaviour)
        cd = JSON3.read(_clu("?projectUid=$(proj.uid)")[2])
        @test cd.projectUid == proj.uid && length(cd.images) == 1 && haskey(cd.images[1], :clusters)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: analysis chains" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _ch(t) = api_analysis_chains(HTTP.Request("GET", "/api/analysis/chains" * t))
    try
        proj = create_project!(name = "api-chains", kind = "live")
        Cecelia.save_chain_template!(proj, Cecelia.ChainTemplate("pipe",
            [Cecelia.ChainNode(; id = "n1", fn = "segment.cellpose")], Cecelia.ChainEdge[]))
        @test _ch("")[1] == 400                                              # missing projectUid
        @test _ch("?projectUid=nope")[1] == 404
        st, body = _ch("?projectUid=$(proj.uid)")
        @test st == 200
        d = JSON3.read(body)
        @test d.projectUid == proj.uid && haskey(d, :runs)
        @test d.templates[findfirst(t -> t.name == "pipe", d.templates)].nodes[1].fun == "segment.cellpose"
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: repl api surface" begin
    # Project-independent: the notebook/REPL data-access surface backing the get_repl_api MCP tool.
    st, body = api_repl_api(HTTP.Request("GET", "/api/repl/api"))
    @test st == 200
    d = JSON3.read(body)
    @test !isempty(d.api)
    names = Set(String(e.name) for e in d.api)
    @test "pop_df" in names && "label_props" in names && "load_project" in names
    @test all(e -> e.documented, d.api)                 # every listed accessor is documented
    # the cookbook rides along (dev checkout ships docs/REPL.md) and carries the write rules
    @test occursin("using Cecelia", d.doc)
    @test occursin("figures", d.doc) && occursin("CSV", d.doc)
end
