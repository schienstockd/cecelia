# API-layer tests. The package (`app/test/runtests.jl`) covers Cecelia headless; this covers the thin
# HTTP adapters in `api/src`. We load the server module WITHOUT binding a socket (`CECELIA_NO_SERVE`)
# and call the handler functions directly — no live server, no ports, so it runs in CI headless.
#
# Run: `julia --project=api api/test/runtests.jl`  (or `pixi run test-api`).
ENV["CECELIA_NO_SERVE"] = "1"

# HERMETIC BY DEFAULT — same guard as `app/test/runtests.jl`, and here it fixes a real leak, not just
# CI. Testsets redirect `dirs["projects"]` to a temp dir individually and restore it in a `finally`;
# anything that forgets, or any `create_project!` on a path between one restore and the next redirect,
# writes into the DEVELOPER'S REAL projects dir and shows up in their project list. (Dominik has been
# seeing these: `apiqc-7602` was still sitting there, from a testset since renamed.) Pointing config at
# a throwaway dir for the whole run makes the whole class impossible instead of per-testset diligence.
#
# Set `CECELIA_DEV_DIR` yourself to run against a specific config. Julia deletes both temp dirs at exit.
# Paths go in TOML *literal* strings (single quotes) so Windows backslashes are not escapes.
if !haskey(ENV, "CECELIA_DEV_DIR")
    let cfg = mktempdir(), proj = mktempdir()
        write(joinpath(cfg, "custom.toml"), "[dirs]\nprojects = '" * proj * "'\n")
        ENV["CECELIA_DEV_DIR"] = cfg
    end
end

using Test
include(joinpath(@__DIR__, "..", "src", "server.jl"))   # defines handlers + shared state; does not start
using JSON3

# ── Test data fixtures ────────────────────────────────────────────────────────
# Same committed fixtures the package suite uses (see test-data/README.md); resolved here too because
# the Julia OME-ZARR readers under test live in `api/src/image_geometry.jl`. Override with
# CECELIA_TEST_DATA. (@__DIR__ = api/test → ../../.. = workspace root.)
api_test_projects_dir() = get(ENV, "CECELIA_TEST_DATA",
    normpath(joinpath(@__DIR__, "..", "..", "test-data", "projects")))
api_fixture(relparts...) = joinpath(api_test_projects_dir(), relparts...)
# A store is a DIRECTORY; warn once and let the caller @test_skip rather than fail a partial checkout.
const _API_WARNED_FIXTURES = Set{String}()
function api_have_fixture(path::AbstractString)::Bool
    (isfile(path) || isdir(path)) && return true
    if !(path in _API_WARNED_FIXTURES)
        push!(_API_WARNED_FIXTURES, path)
        @warn "TEST FIXTURE MISSING — dependent tests SKIPPED. Expected: $path (restore with `git checkout -- test-data`)"
    end
    false
end

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
    @test d.port > 0 && d.napariPort == 7655 && d.previewPort == 7656 && d.notebooksPort == 7660
    # installed-build provenance (.cecelia-version at the install root); a source checkout has no
    # such file → the fallback string. Either way the field must be present and non-empty.
    @test haskey(d, :version) && !isempty(String(d.version))
    # first-launch setup flag drives the frontend /setup redirect
    @test haskey(d, :setupRequired) && d.setupRequired isa Bool
end

@testset "API: pool limit set — guards" begin
    # unknown pool rejected (nothing persisted); the success path is covered in app/test where the
    # config dir can be redirected to a temp so the real custom.toml is untouched.
    st,  _ = _post(api_pool_set, Dict("name" => "nope", "limit" => 4))
    @test st == 400
    st2, _ = _post(api_pool_set, Dict("limit" => 4))            # missing name
    @test st2 == 400
end

@testset "API: maintenance patches" begin
    st, body = api_maintenance_patches(HTTP.Request("GET", "/api/maintenance/patches"))
    @test st == 200
    r = JSON3.read(body)
    @test length(r.patches) >= 1
    ids = [String(p.id) for p in r.patches]
    @test "store-debris" in ids                        # the shipped leftover-store sweep
    cp = r.patches[findfirst(==("store-debris"), ids)]
    @test !isempty(String(cp.title)) && !isempty(String(cp.description))
    for p in r.patches
        @test !isempty(String(p.id)) && !isempty(String(p.title))
    end
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

@testset "API: update version ordering (rcN sorts numerically)" begin
    # THE BUG THIS PINS. Julia parses `v"0.1.0-rc10"`'s prerelease as the single STRING `("rc10",)`,
    # and strings compare lexicographically — so `"rc10" < "rc9"` and rc10 sorted BELOW rc9.
    # `api_update_check` reports the MAX release, so once rc10 existed the max stayed rc9: rc9
    # clients saw "up to date" and older clients were updated *to* rc9 and stuck there. Silent, no
    # error, latent from rc1, triggered at the 9→10 boundary. `_parse_ver` now rewrites `-rc10` →
    # `-rc.10` so the digits are a NUMERIC identifier.
    @test _parse_ver("v0.1.0-rc10") > _parse_ver("v0.1.0-rc9")
    @test _parse_ver("v0.1.0-rc11") > _parse_ver("v0.1.0-rc2")
    @test _parse_ver("v0.1.0-rc100") > _parse_ver("v0.1.0-rc99")

    # ...without disturbing the orderings that were already right.
    @test _parse_ver("v0.1.0-rc9") > _parse_ver("v0.1.0-rc8")
    @test _parse_ver("v0.1.0") > _parse_ver("v0.1.0-rc10")     # a release outranks its prereleases
    @test _parse_ver("v0.2.0") > _parse_ver("v0.1.0")
    @test _parse_ver("v0.1.1") > _parse_ver("v0.1.0")

    # Shape + tolerance: `v`/`V` prefix and surrounding space are stripped, junk is `nothing`
    # (so a "dev" checkout never reports an update rather than erroring).
    @test _parse_ver("V0.1.0") == _parse_ver(" v0.1.0 ") == VersionNumber("0.1.0")
    @test _parse_ver("dev") === nothing
    @test _parse_ver("") === nothing
    @test _parse_ver("v0.1.0-rc10").prerelease == ("rc", 10)   # numeric identifier, not "rc10"

    # An already-dotted tag must not be rewritten twice.
    @test _parse_ver("v0.1.0-rc.10") == _parse_ver("v0.1.0-rc10")

    # End-to-end over a release LIST, the way `api_update_check` picks a winner: the newest tag must
    # win regardless of the order GitHub returns it in.
    pick(tags) = argmax(t -> _parse_ver(t), tags)
    @test pick(["v0.1.0-rc8", "v0.1.0-rc10", "v0.1.0-rc9"]) == "v0.1.0-rc10"
    @test pick(["v0.1.0-rc10", "v0.1.0", "v0.1.0-rc9"]) == "v0.1.0"
end

@testset "API: update apply guard rails" begin
    # `api_update_apply` refuses to run in a git checkout — which is where these tests live — so the
    # guards are extracted into `_apply_precheck` to be reachable at all. Pure: no network, no root.
    ok(tag) = _apply_precheck(tag; scope = "user", installed = true)

    @test ok("v0.1.0") === nothing              # cleared to download
    @test ok("v0.1.0-rc9") === nothing
    @test ok("v0.1.0-rc.10") === nothing
    @test ok("0.1.0") === nothing               # the `v` is optional

    # Scope/install guards still fire, and in priority order — a system install is refused even
    # with a perfectly good tag.
    @test _apply_precheck("v0.1.0"; scope = "system", installed = true)[1] == 403
    @test _apply_precheck("v0.1.0"; scope = "dev", installed = false)[1] == 400
    @test _apply_precheck(""; scope = "user", installed = true)[1] == 400

    # `tag` is interpolated into the download URL and written to `.pending-update`, so free-form
    # input must not survive: traversal, a second path segment, a query string or whitespace would
    # each point the download somewhere other than this release's asset.
    for bad in ["../../etc/passwd", "v0.1.0/../../other", "v0.1.0?x=1", "v0.1.0 rm -rf",
                "latest", "main", "v0.1", "v0.1.0;whoami", "v0.1.0\nx", "https://evil/x"]
        r = _apply_precheck(bad; scope = "user", installed = true)
        @test r !== nothing && r[1] == 400
    end

    # Anchoring: a valid tag with junk appended must NOT pass (an unanchored regex would let it).
    @test _apply_precheck("v0.1.0/evil"; scope = "user", installed = true) !== nothing
    @test _apply_precheck("xv0.1.0"; scope = "user", installed = true) !== nothing
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

@testset "API: shutdown stops EVERY resident child" begin
    # The preview worker was added and this function was not updated, so Quit left it alive on :7656
    # holding a warm cellpose model's VRAM that nothing could reach. Zombie children are the reason
    # the start/stop logic exists at all, so the coverage is asserted rather than remembered.
    #
    # Source-level on purpose: actually exercising it would kill the developer's own running napari
    # and preview worker, since the ports are fixed and these tests share the machine.
    src  = read(joinpath(@__DIR__, "..", "src", "app_api.jl"), String)
    body = src[findfirst("function _stop_children_for_exit()", src)[1]:end]
    body = body[1:findfirst("\nend", body)[1]]

    # `api_diagnostics` is the de-facto registry of resident children — it reports one `*Port` per
    # child for the System panel. Anything it lists must be freed on exit, so ADDING a child to
    # diagnostics without adding it here fails this test rather than shipping another zombie.
    diag  = JSON3.read(api_diagnostics(HTTP.Request("GET", "/api/diagnostics"))[2])
    child = [k for k in keys(diag) if endswith(String(k), "Port") && String(k) != "port"]
    @test length(child) == 3            # napari, preview, notebooks — update deliberately, not silently
    @test count(_ -> true, eachmatch(r"_kill_listeners_on_port\(", body)) == length(child)

    for c in ("NAPARI_PORT", "PREVIEW_PORT", "NOTEBOOKS_PORT")
        @test occursin(c, body)
    end
    # and the graceful stop, not only the port-level kill, for each child that has a handle
    @test occursin("close!(v)", body)
    @test occursin("_shutdown_notebook_server!()", body)
    @test occursin("_stop_preview_worker!()", body)

    # the shared stop must be reachable from the toggle-off route too — one meaning of "stop the
    # worker", however it is reached
    @test occursin("_stop_preview_worker!()",
                   read(joinpath(@__DIR__, "..", "src", "preview_api.jl"), String))
    @test isdefined(Main, :_stop_preview_worker!)

    # The dev supervisor frees the same children on Ctrl-C / crash, where nothing runs the route above.
    # It cannot load Cecelia (standalone script), so it repeats the port numbers as literals — assert the
    # copies agree, because a renumbered port that only ONE of them knows about is a silent zombie.
    dev = read(joinpath(@__DIR__, "..", "dev.jl"), String)
    m   = match(r"const CHILD_PORTS = \(([^)]*)\)", dev)
    @test m !== nothing
    dev_ports = sort(parse.(Int, strip.(split(m.captures[1], ","))))
    @test dev_ports == sort([Cecelia.NAPARI_PORT, Cecelia.PREVIEW_PORT, NOTEBOOKS_PORT])
    @test occursin("for p in CHILD_PORTS", dev)          # …and they are actually freed, not just listed

    # PROD's supervisor (`app.py`) had the same hole: `proc.terminate()` kills the Julia server and
    # leaves its three grandchildren running. It closes it by REUSING the route above rather than
    # carrying a third copy of platform port-killing — so assert the reuse and, crucially, the ORDER:
    # attempting a graceful stop AFTER terminate would be pointless, and the diff that introduces that
    # mistake looks almost identical to the correct one.
    app = read(joinpath(@__DIR__, "..", "..", "app.py"), String)
    @test occursin("/api/app/shutdown", app)
    @test occursin("_stop_gracefully(proc)", app)
    # Assert the order inside the teardown block itself: the graceful attempt must come before the
    # terminate it is meant to avoid. Comparing positions in the whole file would pass even if the two
    # were in unrelated places, which is exactly the bug being guarded against.
    let tail = app[findlast("finally", app)[1]:end]
        i_graceful = findfirst("_stop_gracefully(proc)", tail)
        i_term     = findfirst("proc.terminate()", tail)
        @test i_graceful !== nothing
        @test i_term !== nothing
        @test i_graceful[1] < i_term[1]
    end
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

        # revise: SNAPSHOTS the current notebook (freezes it) then overwrites its cells — a real new
        # version, not a "-v2" copy. 409 if the file is absent; 400 without cells.
        let before = length(snaps())
            r = JSON3.read(_post(api_notebooks_revise, Dict("projectUid"=>uid, "file"=>"nb1.jl",
                                  "cells"=>["using Cecelia", "df = 1 + 1"], "description"=>"revised"))[2])
            @test r.ok == true
            @test length(snaps()) == before + 1                 # pre-revision state was frozen as a version
            @test find("nb1.jl").description == "revised"
        end
        @test _post(api_notebooks_revise, Dict("projectUid"=>uid, "file"=>"nope.jl", "cells"=>["x"]))[1] == 409  # must exist
        @test _post(api_notebooks_revise, Dict("projectUid"=>uid, "file"=>"nb1.jl"))[1] == 400                   # cells required

        # description cap: a long blurb is truncated at _NB_DESC_MAX (create/describe/write/revise all cap)
        @test _post(api_notebooks_create, Dict("projectUid"=>uid, "name"=>"nbcap", "description"=>repeat("x", 300)))[1] == 200
        @test length(find("nbcap.jl").description) == _NB_DESC_MAX

        # prune: keep ONLY the current version's snapshot, drop the older ones; description is untouched.
        # State here: several snapshots on disk, current pointer set by the revise above.
        let cur = find("nb1.jl").version
            @test cur > 1 && length(snaps()) > 1              # precondition: history to prune
            @test _post(api_notebooks_describe, Dict("projectUid"=>uid,"file"=>"nb1.jl","description"=>"keep me"))[1] == 200
            pr = JSON3.read(_post(api_notebooks_prune, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[2])
            @test pr.ok == true && pr.kept == cur
            @test [s.version for s in snaps()] == [cur]       # only the current version survives
            @test find("nb1.jl").version == cur               # pointer unchanged
            @test find("nb1.jl").description == "keep me"      # description NOT pruned
            # idempotent: pruning again removes nothing (single snapshot = the current one)
            @test JSON3.read(_post(api_notebooks_prune, Dict("projectUid"=>uid,"file"=>"nb1.jl"))[2]).removed |> length == 0
        end
        # prune with no current version (never snapshotted) aborts rather than wiping — 409, history intact
        @test _post(api_notebooks_create, Dict("projectUid"=>uid, "name"=>"nbfresh"))[1] == 200
        @test _post(api_notebooks_prune, Dict("projectUid"=>uid,"file"=>"nbfresh.jl"))[1] == 409

        # delete — pass force=true so this is deterministic regardless of whether a Pluto server is
        # running locally. The guard 409s on a live server without force (a dev machine with the
        # notebook server up would otherwise fail this + the two asserts below); force is what the
        # UI's confirm supplies anyway.
        @test !isempty(snaps())    # nb1 has snapshots on disk before delete
        @test _post(api_notebooks_delete, Dict("projectUid"=>uid,"file"=>"nb1.jl","force"=>true))[1] == 200
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

@testset "API: notebooks write (generate from cells)" begin
    # Pure serialiser: cells → valid Pluto source (activation prepended, markers + Cell order block).
    src = _pluto_notebook_source(["using Cecelia", "df = pop_df(img, \"flow\", [\"/T\"])"])
    @test occursin("### A Pluto.jl notebook ###", src)
    @test occursin("Pkg.activate", src)                       # env-activation cell prepended
    @test occursin("df = pop_df(img", src)                    # caller cell present, verbatim
    @test occursin("# ╔═╡ Cell order:", src)
    @test count("# ╠═", src) == 3                             # activation + 2 caller cells, listed once each

    # Pluto is one-expression-per-cell: a multi-statement cell must be wrapped in begin…end so it
    # loads (otherwise "Multiple expressions in one cell"). Single expressions stay bare.
    @test _wrap_multi_expr("df = 1") == "df = 1"                          # single expr → untouched
    @test _wrap_multi_expr("# just a comment") == "# just a comment"     # no expr → untouched
    @test !occursin("begin", _wrap_multi_expr("plot(df.x, df.y)"))       # single call → bare
    multi = _wrap_multi_expr("a = 1\nb = 2\na + b")
    @test startswith(multi, "begin\n") && endswith(multi, "\nend")       # multi-statement → wrapped
    @test occursin("a = 1\nb = 2\na + b", multi)                         # body preserved verbatim
    @test _wrap_multi_expr("x = (\n  1 + 2)") == "x = (\n  1 + 2)"       # one expr spanning lines → bare
    # end-to-end through the serialiser: a multi-statement caller cell comes out wrapped
    @test occursin("begin\nusing Cecelia\ndf = pop_df", _pluto_notebook_source(["using Cecelia\ndf = pop_df(img, \"flow\", [\"/T\"])"]))

    # Cell-id preservation across a revise (so Pluto's auto_reload can update an OPEN notebook in place).
    ids_of(src) = (p = tempname() * ".jl"; write(p, src); v = _content_cell_ids(p); rm(p; force = true); v)
    # A fresh serialise → the activation id is pinned + excluded; content cells get distinct v4 uuids.
    src1 = _pluto_notebook_source(["using Cecelia", "df = 1", "plot(df)"])
    ids1 = ids_of(src1)
    @test length(ids1) == 3 && allunique(ids1) && _NB_ACTIVATION_ID ∉ ids1
    # Re-serialising WITH the prior ids reuses them positionally → the same ids come back out.
    src2 = _pluto_notebook_source(["using Cecelia", "df = 2", "plot(df)"]; reuse_ids = ids1)
    @test ids_of(src2) == ids1                                # ids stable ⇒ auto_reload matches cells
    @test occursin("df = 2", src2)                            # …but the code did change
    # Extra cells beyond the reused set get fresh ids; the reused prefix stays put.
    src3 = _pluto_notebook_source(["using Cecelia", "df = 3", "plot(df)", "extra = 4"]; reuse_ids = ids1)
    ids3 = ids_of(src3)
    @test ids3[1:3] == ids1 && length(ids3) == 4 && ids3[4] ∉ ids1

    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    w(b) = _post(api_notebooks_write, b)
    # capture WS frames so we can assert the create nudges an open Notebooks page to refresh
    cap = Channel{String}(64); key = gensym("test-nbwrite")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    drain() = (fs = []; while isready(cap); push!(fs, JSON3.read(take!(cap))); end; fs)
    try
        uid = "TESTNBW"; mkpath(joinpath(tmp, uid))
        @test w(Dict("projectUid"=>uid, "name"=>"gen"))[1] == 400              # cells required
        @test w(Dict("projectUid"=>"NOPE", "name"=>"gen", "cells"=>["x=1"]))[1] == 404
        drain()   # clear any frames from the failing calls above
        st, body = w(Dict("projectUid"=>uid, "name"=>"gen",
                          "cells"=>["using Cecelia", "df = 1"], "description"=>"speed over time"))
        @test st == 200 && JSON3.read(body).file == "gen.jl"
        # a notebooks_changed frame for this project → an open Notebooks page auto-refreshes
        @test any(f -> String(get(f, :type, "")) == "notebooks_changed" &&
                       String(get(f, :projectUid, "")) == uid, drain())
        dest = joinpath(tmp, uid, "notebooks", "gen.jl")
        @test isfile(dest)
        content = read(dest, String)
        @test occursin("Pkg.activate", content) && occursin("df = 1", content)   # runnable + caller code
        # registered + snapshotted v1 (an immediate restore point)
        nb = JSON3.read(api_notebooks_list(HTTP.Request("GET", "/api/notebooks?projectUid=$uid"))[2]).notebooks
        g  = nb[findfirst(n -> n.file == "gen.jl", nb)]
        @test g.version == 1 && g.description == "speed over time"
        # create-only: never clobbers
        @test w(Dict("projectUid"=>uid, "name"=>"gen", "cells"=>["y=2"]))[1] == 409

        # content read (the "have a look" flow): returns the notebook's current source
        cget(q) = api_notebooks_content(HTTP.Request("GET", "/api/notebooks/content?$q"))
        st2, cbody = cget("projectUid=$uid&file=gen.jl")
        @test st2 == 200
        cd = JSON3.read(cbody)
        @test cd.file == "gen.jl" && cd.scope == "project" && occursin("df = 1", cd.content)
        @test cget("projectUid=$uid&file=nope.jl")[1] == 404      # missing notebook
        @test cget("projectUid=$uid")[1] == 400                    # file required
        @test cget("projectUid=NOPE&file=gen.jl")[1] == 404        # missing project
        @test cget("projectUid=$uid&file=../secret")[1] == 400     # path traversal rejected
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
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
          (isfile(_sysimage_path()) && stamp_matches(onstamp, string(VERSION), _manifest_hash()))
end

@testset "API: task definitions carry the previewable trait" begin
    # The trait is declared in Julia beside the task and STAMPED onto the spec here, rather than written
    # into the JSON (which is the param spec — a capability of the compute doesn't belong in it, and two
    # copies could disagree). The frontend reads this instead of sniffing the params for a
    # cellpose-shaped `models` bag.
    st, body = api_task_definitions(HTTP.Request("GET", "/api/tasks/definitions?category=segment"))
    @test st == 200
    specs = JSON3.read(body).segment
    by_fun = Dict(String(s.fun_name) => s for s in specs if haskey(s, :fun_name))

    # every spec gets the key, so the frontend never has to treat "absent" as a third state
    for s in specs
        haskey(s, :fun_name) && @test haskey(s, :previewable)
    end
    @test by_fun["segment.cellpose"].previewable == true
    # the composite the module page actually runs — the #421 trap
    @test by_fun["segment.cellposeMeasure"].previewable == true
    # and a task the worker can't run says so
    @test by_fun["segment.measureLabels"].previewable == false
end

@testset "API: task preview never guesses which image is open" begin
    # The property this whole route exists for. `napari_api` tracked the open image but exposed
    # nothing, so callers had to be told which image to act on — and guessing wrong wrote scratch
    # stores into images the user wasn't looking at. Every branch below is a REFUSAL, because the
    # alternative to refusing is acting on the wrong image.
    saved = (_current_image_uid[], _current_zarr_path[], _current_task_dir[])
    try
        # ── nothing open → the status route says so in every field, so a client can't read a
        #    plausible-looking default out of it
        _current_image_uid[] = nothing; _current_zarr_path[] = nothing; _current_task_dir[] = nothing
        st, body = api_preview_status(HTTP.Request("GET", "/api/preview/status"))
        @test st == 200
        d = JSON3.read(body)
        @test d.imageUid === nothing && d.zarrPath === nothing && d.taskDir === nothing
        @test d.port == 7656 && d.port != 7655        # its own port, not the bridge's
        @test d.alive == false

        # ...and a run refuses rather than picking something
        st, body = _post(api_preview_run, Dict("projectUid" => "p", "imageUid" => "i",
                                               "params" => Dict("models" => Dict())))
        @test st == 409
        @test JSON3.read(body).code == "no-image-open"

        # ── a DIFFERENT image open → refuse, and name what is actually open
        _current_image_uid[] = "openImg"; _current_zarr_path[] = "/x/openImg.ome.zarr"
        _current_task_dir[]  = "/x/meta"
        st, body = _post(api_preview_run, Dict("projectUid" => "p", "imageUid" => "otherImg",
                                               "params" => Dict("models" => Dict())))
        @test st == 409
        d = JSON3.read(body)
        @test d.code == "image-mismatch" && d.openImageUid == "openImg"

        # ── missing required fields are 400s, not silent defaults
        @test _post(api_preview_run, Dict("imageUid" => "i", "params" => Dict()))[1] == 400
        @test _post(api_preview_run, Dict("projectUid" => "p", "params" => Dict()))[1] == 400
        @test _post(api_preview_run, Dict("projectUid" => "p", "imageUid" => "i"))[1] == 400

        # ── the open image, but the task reads a DIFFERENT VERSION of it → refuse and say which to
        #    open. Previewing anyway would either segment pixels the user can't see or pair the
        #    region with a differently-shaped store.
        mktempdir() do proj_root
            conf  = cecelia_conf()
            pdirs = get!(conf, "dirs", Dict{String,Any}())
            had   = haskey(pdirs, "projects"); prev = get(pdirs, "projects", nothing)
            pdirs["projects"] = proj_root
            try
                uid = "img9"
                meta = joinpath(proj_root, "p", "1", uid); mkpath(meta)
                write(joinpath(meta, "ccid.json"), JSON3.write(Dict(
                    "uid" => uid,
                    "filepath" => Dict("default" => "orig.ome.zarr",
                                       "corrected" => "drift.ome.zarr", "_active" => "default"))))
                _current_image_uid[] = uid
                _current_zarr_path[] = joinpath(proj_root, "p", "0", uid, "orig.ome.zarr")
                _current_task_dir[]  = meta

                st, body = _post(api_preview_run, Dict(
                    "projectUid" => "p", "imageUid" => uid,
                    "params" => Dict("valueName" => "corrected", "models" => Dict())))
                @test st == 409
                d = JSON3.read(body)
                @test d.code == "version-mismatch" && d.wantedValueName == "corrected"
                # The frontend renders `code` as the short amber label and this message as the
                # tooltip detail, so the message must carry the SPECIFICS — both names — rather than
                # restate the problem. See `previewNotice`/`ERROR_SHORT` in utils/taskPreview.ts.
                @test occursin("corrected", d.error) && occursin("orig.ome.zarr", d.error)
                @test d.openZarr == "orig.ome.zarr"

                # an unknown valueName is a 404, not a preview of the active version
                st, _ = _post(api_preview_run, Dict(
                    "projectUid" => "p", "imageUid" => uid,
                    "params" => Dict("valueName" => "nope", "models" => Dict())))
                @test st == 404
            finally
                had ? (pdirs["projects"] = prev) : delete!(pdirs, "projects")
            end
        end

        # ── a RUNNING segmentation puts the viewer on the staging store while ccid.json still
        #    resolves the final path. Same store mid-write, so this must NOT be a mismatch.
        @test _same_store("/a/b/X.ome.zarr", "/a/b/X.ome.zarr" * Cecelia.STORE_STAGING_SUFFIX)
        @test _same_store("/a/b/X.ome.zarr", "/a/b/X.ome.zarr")
        @test !_same_store("/a/b/X.ome.zarr", "/a/b/Y.ome.zarr")
    finally
        _current_image_uid[], _current_zarr_path[], _current_task_dir[] = saved
    end
end

@testset "API: a built preview request is always sent" begin
    # `preview_request` BUILDS a request; `send(w, …)` runs it. Returning the request instead is a
    # 200 full of plausible-looking JSON — right imPath, right params, right funName — that simply
    # has no result in it, so the caller renders an empty panel and nothing anywhere reports an
    # error. `api_optical_flow_inspect` shipped exactly that: reviewed, type-checked, and dead.
    #
    # Building one without sending it has no other use, so the rule is total: every call site is an
    # argument to `send`.
    for file in filter(f -> endswith(f, ".jl"), readdir(joinpath(@__DIR__, "..", "src"); join = true))
        src = read(file, String)
        for m in eachmatch(r"preview_request\(", src)
            # the enclosing call — `send(w, preview_request(…))` — sits just before it; allow for the
            # keyword-heavy wrapping the real call sites use
            before = src[max(1, m.offset - 240):m.offset]
            @test occursin("send(", before) ||
                  occursin("function preview_request", before)   # the definition itself
        end
    end
end

@testset "API: image geometry (axis mapping + version resolution)" begin
    # Pure parts of image_geometry.jl — no zarr, no IO. These were `_crop_*` privates until a second
    # consumer showed none of it was crop-specific (docs: the anisotropy grid advisory).

    # Zarr.jl is column-major and presents the array REVERSED, so the C-order axis at position i sits
    # at Julia dim ndims-i+1. Getting this backwards silently swaps x and y — and a square frame
    # would hide it, so assert with a NON-square rank-5 layout.
    d = axis_dims(["t", "c", "z", "y", "x"], 5)
    @test d["t"] == 5 && d["c"] == 4 && d["z"] == 3 && d["y"] == 2 && d["x"] == 1

    d3 = axis_dims(["c", "y", "x"], 3)
    @test d3["x"] == 1 && d3["y"] == 2 && d3["c"] == 3

    # no axes in .zattrs → fall back to the conventional order for that rank, not an error
    @test axis_dims(String[], 5)["x"] == 1
    @test axis_dims(String[], 2)["y"] == 2
    @test !haskey(axis_dims(String[], 2), "z")

    # absent .zattrs → empty, and the caller falls back rather than throwing
    mktempdir() do dir
        @test read_ngff_axes(dir) == String[]
    end
    # malformed .zattrs must not take the request down
    mktempdir() do dir
        write(joinpath(dir, ".zattrs"), "{not json")
        @test read_ngff_axes(dir) == String[]
    end
    mktempdir() do dir
        write(joinpath(dir, ".zattrs"),
              """{"multiscales":[{"axes":[{"name":"T"},{"name":"Y"},{"name":"X"}]}]}""")
        @test read_ngff_axes(dir) == ["t", "y", "x"]     # lowercased
    end

    # version resolution reports WHY it failed instead of throwing — the route maps it to a status
    _, _, e1 = resolve_image_version("", "", nothing)
    @test e1 == "projectUid + imageUid required"
    _, _, e2 = resolve_image_version("no-such-project", "no-such-image", nothing)
    @test e2 == "Image not found"
end

@testset "API: store compression (what a version is encoded with)" begin
    # The label a version shows in the metadata modal must be the SAME name Settings uses, or the two
    # surfaces describe one codec two ways.
    known = Dict(c.label => c for c in Cecelia.IMAGE_COMPRESSOR_CHOICES)

    d = _describe_compressor(Dict(:id => "blosc", :cname => "zstd", :clevel => 3, :shuffle => 1))
    @test haskey(known, d.label)                       # it resolved to a Settings choice, not a guess
    @test d.codec == "zstd" && d.level == 3 && d.shuffle

    # zstd level 0 IS the library default (3) — a store written at 0 must not read as a DIFFERENT
    # setting from one written at 3, or every store predating the explicit choice looks non-canonical.
    z0 = _describe_compressor(Dict(:id => "zstd", :level => 0))
    z3 = _describe_compressor(Dict(:id => "zstd", :level => 3))
    @test z0.level == 3 && z0.label == z3.label

    # The three codecs actually on disk today all happen to BE selectable choices, so they resolve to
    # Settings labels — verified against real stores (bioformats2raw/zarr-2 `lz4 + shuffle`, zarr-3
    # `zstd`, and the canonical `zstd + shuffle`).
    @test haskey(known, _describe_compressor(
        Dict(:id => "blosc", :cname => "lz4", :clevel => 5, :shuffle => 1)).label)

    # Something genuinely OUTSIDE the table must get an honest descriptive label rather than being
    # silently mapped onto the nearest option — a wrong name here would misreport what is on disk.
    for spec in (Dict(:id => "blosc", :cname => "lz4", :clevel => 9, :shuffle => 1),   # same codec, other level
                 Dict(:id => "zlib", :level => 6),                                     # not offered at all
                 Dict(:id => "blosc", :cname => "zstd", :clevel => 3, :shuffle => 0))  # blosc-wrapped, unshuffled
        d2 = _describe_compressor(spec)
        @test !haskey(known, d2.label)
        @test occursin(string(d2.level), d2.label)      # the label carries the level it found
    end

    # `compressor: null` is a real value — an uncompressed store, not an error
    @test _describe_compressor(nothing).label == "none"
    @test _describe_compressor("garbage") === nothing

    # store_compression: BOTH layouts, detected structurally (a flat store's level 0 is `0`, a
    # bioformats2raw series' is `0/0`, and both have a `0/` child so the path says nothing)
    mktempdir() do dir
        flat = joinpath(dir, "flat.ome.zarr"); mkpath(joinpath(flat, "0"))
        write(joinpath(flat, "0", ".zarray"),
              """{"compressor":{"id":"blosc","cname":"zstd","clevel":3,"shuffle":1}}""")
        @test store_compression(flat).codec == "zstd"

        series = joinpath(dir, "series.ome.zarr"); mkpath(joinpath(series, "0", "0"))
        write(joinpath(series, "0", "0", ".zarray"),
              """{"compressor":{"id":"blosc","cname":"lz4","clevel":5,"shuffle":1}}""")
        @test store_compression(series).codec == "lz4"

        # unreadable / absent / malformed → nothing, never a throw: this is display-only and the
        # caller is listing every version of an image
        @test store_compression(joinpath(dir, "nope.ome.zarr")) === nothing
        bad = joinpath(dir, "bad.ome.zarr"); mkpath(joinpath(bad, "0"))
        write(joinpath(bad, "0", ".zarray"), "{not json")
        @test store_compression(bad) === nothing
    end
end

@testset "API: image render composite" begin
    # Pure colourise/blend for the server-side preview render (image_render.jl) — no zarr/IO. (C,H,W) float +
    # per-channel (lo,hi,cmap,visible) → H×W RGB, clip-to-contrast + additive blend.
    r(x) = Float64(ColorTypes.red(x)); g(x) = Float64(ColorTypes.green(x)); b(x) = Float64(ColorTypes.blue(x))

    # one red channel, mid intensity, full-range contrast → mid red, no green/blue
    img = composite_rgb(fill(0.5f0, 1, 2, 2), [(0.0, 1.0, "red", true)])
    @test size(img) == (2, 2)
    @test isapprox(r(img[1, 1]), 0.5; atol = 0.01) && g(img[1, 1]) == 0 && b(img[1, 1]) == 0

    # contrast clip: value below lo → 0, above hi → 1
    chw = reshape(Float32[0.0 1.0; 0.2 0.8], 1, 2, 2)
    im2 = composite_rgb(chw, [(0.2, 0.8, "green", true)])
    @test isapprox(g(im2[1, 1]), 0.0; atol = 0.01)     # 0.0 < lo → 0
    @test isapprox(g(im2[1, 2]), 1.0; atol = 0.01)     # 1.0 > hi → 1

    # invisible channel contributes nothing
    dark = composite_rgb(fill(1.0f0, 1, 1, 1), [(0.0, 1.0, "red", false)])
    @test r(dark[1, 1]) == 0

    # additive blend: red + green channels → yellow-ish
    two = composite_rgb(cat(fill(1.0f0, 1, 1, 1), fill(1.0f0, 1, 1, 1); dims = 1),
                              [(0.0, 1.0, "red", true), (0.0, 1.0, "green", true)])
    @test r(two[1, 1]) > 0.9 && g(two[1, 1]) > 0.9 && b(two[1, 1]) == 0

    # ── Channel colour: napari's palette must NOT be guessed by name ──────────────────
    # `bop blue` was missing from CMAP_RGB and hit the unknown-name fallback (WHITE), which additively
    # washes the whole composite out — the SHG channel of every intravital image rendered white instead
    # of blue. Assert napari's own end colour (AVAILABLE_COLORMAPS["bop blue"].colors[-1]).
    bop = composite_rgb(fill(1.0f0, 1, 1, 1), [(0.0, 1.0, "bop blue", true)])
    @test isapprox(r(bop[1, 1]), 0.12549; atol = 0.01)
    @test isapprox(g(bop[1, 1]), 0.678431; atol = 0.01)
    @test isapprox(b(bop[1, 1]), 0.972549; atol = 0.01)
    @test b(bop[1, 1]) - r(bop[1, 1]) > 0.5            # unmistakably blue, not white

    # An explicit LUT (props `colormap_lut`) wins over any name table and is interpolated. A 2-stop
    # black→base ramp must reduce EXACTLY to `n .* base`, which is what additive primaries need.
    lut2 = [(0f0, 0f0, 0f0), (1f0, 0f0, 0f0)]
    half = composite_rgb(fill(0.5f0, 1, 1, 1), [(0.0, 1.0, lut2, true)])
    @test isapprox(r(half[1, 1]), 0.5; atol = 0.01) && g(half[1, 1]) == 0

    # 3-stop LUT: midpoint intensity lands on the middle stop, quarter interpolates into it
    lut3 = [(0f0, 0f0, 0f0), (0f0, 1f0, 0f0), (0f0, 0f0, 1f0)]
    mid = composite_rgb(fill(0.5f0, 1, 1, 1), [(0.0, 1.0, lut3, true)])
    @test isapprox(g(mid[1, 1]), 1.0; atol = 0.01) && isapprox(b(mid[1, 1]), 0.0; atol = 0.01)
    qtr = composite_rgb(fill(0.25f0, 1, 1, 1), [(0.0, 1.0, lut3, true)])
    @test isapprox(g(qtr[1, 1]), 0.5; atol = 0.01)
    # a white→colour LUT (napari's `I *` set) is honoured at zero intensity — no name table could do this
    inv = composite_rgb(fill(0.0f0, 1, 1, 1), [(0.0, 1.0, [(1f0, 1f0, 1f0), (0f0, 0f0, 1f0)], true)])
    @test r(inv[1, 1]) > 0.9 && g(inv[1, 1]) > 0.9

    # layer_display_specs prefers the saved LUT over the colormap NAME (same entry carries both)
    mktempdir() do d
        p = joinpath(d, "props.json")
        write(p, JSON3.write((; Image = [
            (; contrast_limits = [0.0, 10.0], colormap = "bop blue",
               colormap_lut = [[0.0, 0.0, 0.0], [1.0, 0.0, 0.0]], visible = true),
            (; contrast_limits = [1.0, 5.0], colormap = "magenta", visible = false),
        ])))
        specs = layer_display_specs(p)
        @test length(specs) == 2
        @test specs[1][3] isa AbstractVector && specs[1][3][2] == (1f0, 0f0, 0f0)  # LUT, not the name
        @test specs[2][3] == "magenta" && specs[2][4] == false                     # no LUT → name kept
        @test specs[2][1] == 1.0 && specs[2][2] == 5.0
    end
    @test layer_display_specs(joinpath(mktempdir(), "absent.json")) === nothing
end

@testset "API: zarr byte order" begin
    # `read_native` must apply the STORED byte order. bioformats2raw writes big-endian (`>u2`) and
    # Zarr.jl parses that for the eltype but hands back the bytes UNSWAPPED — so a raw `default` image
    # version read with plain `arr[...]` is byte-swapped garbage that renders as saturated white noise
    # (a true 63 reads as 16128; 98% of a real frame exceeded a contrast ceiling that should clip none).
    # Silent, and invisible in Python, which honours the descriptor. See docs/NAPARI.md → Byte order.
    # DETECTOR for the single Zarr.jl internal this depends on. `_zarr_byte_order` reads the raw numpy
    # dtype descriptor out of `arr.metadata.dtype`; if a Zarr.jl upgrade changes that field's shape the
    # guard falls back to '|' (never swap) and the big-endian bug returns. The swap assertions below DO
    # catch that — verified by mutating the guard to return '|', which fails 3 of them — but they fail as
    # an opaque UInt16 value mismatch. This one names the cause instead.
    mktempdir() do d
        a = zcreate(UInt16, Zarr.DirectoryStore(joinpath(d, "probe")), 2; chunks = (2,))
        dt = getfield(a.metadata, :dtype)
        @test dt isa AbstractString          # ← if THIS fails, read_native's byte-order guard is blind
        @test occursin(r"^[<>|]", String(dt))
    end

    # Re-stamp the dtype descriptor the way bioformats2raw would; Zarr.jl keeps it as the raw string.
    function stamp_order!(p, order)
        za = JSON3.read(read(joinpath(p, ".zarray"), String), Dict{String,Any})
        za["dtype"] = order
        write(joinpath(p, ".zarray"), JSON3.write(za))
    end
    vals = UInt16[0x0000, 0x003f, 0x00ff, 0x2800, 0xffff]      # 0, 63, 255, 10240, 65535
    mktempdir() do d
        for (i, (order, swaps)) in enumerate((">u2" => true, "<u2" => false))
            p = joinpath(d, "store$(i)")
            a = zcreate(UInt16, Zarr.DirectoryStore(p), length(vals); chunks = (length(vals),))
            a[:] = vals
            stamp_order!(p, order)
            got = read_native(zopen(p, "r"), :)
            @test got == (swaps ? ntoh.(vals) : vals)
            if order == ">u2"                  # the bug this pins: BE must NOT read as the raw bytes
                @test got != vals
                @test got[2] == 0x3f00         # 63 declared big-endian reads as 16128 if left unswapped
            end
        end
        # `|u1` (not-applicable, 1-byte) is passed through untouched — swapping a byte is a no-op, but
        # the descriptor must not be misread as an order either.
        p8 = joinpath(d, "store8")
        b = UInt8[0x00, 0x3f, 0xff]
        a8 = zcreate(UInt8, Zarr.DirectoryStore(p8), length(b); chunks = (length(b),))
        a8[:] = b
        stamp_order!(p8, "|u1")
        @test read_native(zopen(p8, "r"), :) == b
    end
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
              JSON3.write((; uid = uid, name = "T", set_uids = String[])))
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

@testset "API: image stores (codec + on-disk size per version)" begin
    # The whole route over a hand-built project: what the metadata modal renders per stored file. The
    # shapes matter as much as the numbers — a version whose store is GONE must keep its row (bytes 0,
    # no codec ⇒ "—" in the modal) instead of dropping it or failing the call for the versions that
    # do read. Sizes are asserted as lower bounds: `_dir_bytes` reports disk BLOCKS, so the walked
    # total is legitimately larger than the bytes written.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        puid, iuid = "TESTSTORES", "IMG1"
        mkpath(joinpath(tmp, puid, "1", iuid))
        write(joinpath(tmp, puid, "project.json"),
              JSON3.write((; uid = puid, name = "T", set_uids = String[])))
        # one real store (flat layout: level-0 array is `0`), one registered but absent
        store = joinpath(tmp, puid, "0", iuid, "live.ome.zarr")
        mkpath(joinpath(store, "0"))
        write(joinpath(store, "0", ".zarray"),
              """{"compressor":{"id":"blosc","cname":"zstd","clevel":3,"shuffle":1}}""")
        write(joinpath(store, "0", "0.0"), rand(UInt8, 20_000))
        # two label files under one value_name (base + nuc) — the row's size is their sum
        mkpath(joinpath(tmp, puid, "1", iuid, "labels"))
        write(joinpath(tmp, puid, "1", iuid, "labels", "A.zarr"), rand(UInt8, 8_000))
        write(joinpath(tmp, puid, "1", iuid, "labels", "A.nuc.zarr"), rand(UInt8, 4_000))
        write(state_file(joinpath(tmp, puid), iuid), JSON3.write(Dict{String,Any}(
            "class"    => "CciaImage",
            "filepath" => Dict{String,Any}("default" => "live.ome.zarr",
                                           "cpCorrected" => "gone.ome.zarr",
                                           "_active" => "default"),
            "labels"   => Dict{String,Any}("A" => ["A.zarr", "A.nuc.zarr"]))))

        st, body = api_image_stores(
            HTTP.Request("GET", "/api/images/stores?projectUid=$puid&imageUid=$iuid"))
        @test st == 200
        d = JSON3.read(body)
        # the store that reads: Settings' own label for that codec + a walked size
        @test d.versions.default.label == "zstd + shuffle"
        @test d.versions.default.bytes >= 20_000
        # the store that doesn't: row kept, size 0, codec fields absent (the modal shows "—")
        @test d.versions.cpCorrected.bytes == 0
        @test !haskey(d.versions.cpCorrected, :label)
        # label sets are sized too, summed across the value_name's files
        @test d.labels.A.bytes >= 12_000

        # Layout, not just codec: v2 and v3 stores coexist on disk permanently (no converter —
        # ZARR_V3_PLAN D7), so the modal has to be able to say which a store is and how it is chunked.
        # `shard` is present-and-null for an unsharded store rather than absent: "not sharded" and "we
        # could not read it" are different answers and the readout distinguishes them.
        # This fixture's `.zarray` is hand-written with only a `compressor` and no NGFF attrs, so
        # `ngffVersion`/`chunks` are legitimately empty here — the point asserted is that the fields are
        # REPORTED (the modal renders what it gets). Real values are asserted against the ZARRFMT
        # fixtures in *"API: zarr v2 and v3 read identically"*, which are real bioformats2raw stores.
        @test d.versions.default.zarrFormat == 2
        @test isnothing(d.versions.default.shard)
        @test haskey(d.versions.default, :ngffVersion)
        @test haskey(d.versions.default, :chunks)
        # the unreadable store carries none of them, same as its codec fields
        for k in (:zarrFormat, :ngffVersion, :chunks, :shard)
            @test !haskey(d.versions.cpCorrected, k)
        end

        @test api_image_stores(HTTP.Request("GET", "/api/images/stores"))[1] == 400
        @test api_image_stores(
            HTTP.Request("GET", "/api/images/stores?projectUid=NOPE&imageUid=NOPE"))[1] == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: plot-spec per-page popType narrowing" begin
    # ONE spec serves several pages, each offering its own subset of the population families. The
    # narrowing happens server-side so the frontend needs no per-page knowledge — it renders a picker
    # over whatever list it was handed. See docs/PLOTS.md → *Which page a plot belongs to*.
    spec = Dict{String,Any}(
        "id" => "population_summary",
        "dataSource" => Dict{String,Any}("popTypes" => Any[
            Dict{String,Any}("popType" => "flow", "granularity" => "cell"),
            Dict{String,Any}("popType" => "clust", "granularity" => "cell"),
            Dict{String,Any}("popType" => "live", "granularity" => "track"),
            Dict{String,Any}("popType" => "trackclust", "granularity" => "track"),
            Dict{String,Any}("popType" => "region", "granularity" => "cell")]),
        "modules" => Dict{String,Any}("phenotype" => ["flow", "clust"],
                                      "behaviourAnalysis" => ["live", "trackclust"],
                                      "spatialAnalysis" => ["region"]))
    pts(s) = [String(p["popType"]) for p in s["dataSource"]["popTypes"]]

    # each page sees only its own families, in the SPEC's order (the spec decides the default = first)
    @test pts(_narrow_spec_poptypes(spec, "phenotype")) == ["flow", "clust"]
    @test pts(_narrow_spec_poptypes(spec, "behaviourAnalysis")) == ["live", "trackclust"]
    @test pts(_narrow_spec_poptypes(spec, "spatialAnalysis")) == ["region"]
    # granularity travels with the family, so the panel can send the right one per pick
    ph = _narrow_spec_poptypes(spec, "phenotype")["dataSource"]["popTypes"]
    @test all(String(p["granularity"]) == "cell" for p in ph)

    # the universal board (no module) gets the FULL list — it hosts every family at once
    @test length(pts(_narrow_spec_poptypes(spec, ""))) == 5
    # narrowing must not mutate the spec it was handed (specs are re-read per request, but a shared
    # in-memory spec would otherwise be progressively emptied by successive page queries)
    @test length(spec["dataSource"]["popTypes"]) == 5
    # a page not listed at all is left untouched rather than silently emptied
    @test length(pts(_narrow_spec_poptypes(spec, "segment"))) == 5
    # a legacy single-`module` spec has no `modules` and passes straight through
    legacy = Dict{String,Any}("module" => "phenotype",
                              "dataSource" => Dict{String,Any}("popType" => "flow", "granularity" => "cell"))
    @test _narrow_spec_poptypes(legacy, "phenotype") === legacy
end

@testset "API: interaction matrix needs no population selection" begin
    # The interaction matrix's rows/columns come from the `neighbourStats` run it reads, so the panel
    # sends NO `series`/`pops`. The generic selector guard rejected that body before `plot_summary_data`
    # could intercept on matrixMode — "pops (or series) required" on a plot that has no pops to pick.
    pops_required(r) = occursin("pops (or series) required", String(r[2]))
    base = Dict("projectUid" => "nope-not-a-project", "popType" => "flow", "granularity" => "cell")

    inter = _post(api_plot_data, merge(base, Dict("chartType" => "matrix", "matrixMode" => "interaction")))
    @test !pops_required(inter)          # gets past the guard (then fails on the bogus project, as it should)

    # the guard must still hold for every OTHER plot — including the other matrix modes, which DO
    # aggregate a pop_df frame and are meaningless without a selection.
    for mode in ("profile", "crosstab")
        r = _post(api_plot_data, merge(base, Dict("chartType" => "matrix", "matrixMode" => mode)))
        @test pops_required(r)
    end
    @test pops_required(_post(api_plot_data, merge(base, Dict("chartType" => "bar"))))
    # an explicitly EMPTY series list is a different mistake (the user unticked everything) and keeps
    # its own message rather than being waved through as "precomputed"
    empty_series = _post(api_plot_data, merge(base, Dict("chartType" => "bar", "series" => [])))
    @test occursin("series required", String(empty_series[2]))
end

@testset "API: cluster/region run resolution is family-aware" begin
    # The channels endpoint enumerates a pop_type's OWN obs column family. Hardcoding "clusters." here
    # is why the Region-clustering page showed an empty run list (falling back to "default") while
    # `regions.immune` sat in obs — see docs/todo/SPATIAL_REGIONS_PLAN.md.
    obs = ["label", "clusters.myeloid", "regions.immune", "regions.niches", "live.cell.speed"]
    @test _cluster_suffixes(obs, "clust")      == ["myeloid"]
    @test _cluster_suffixes(obs, "trackclust") == ["myeloid"]
    @test Set(_cluster_suffixes(obs, "region")) == Set(["immune", "niches"])
    @test _cluster_suffixes(obs) == ["myeloid"]          # default stays the clusters family
    @test isempty(_cluster_suffixes(["label"], "region"))

    # sidecar reads are family-scoped too, and a missing file/run is empty rather than a throw
    lpdir = mktempdir(); props = joinpath(lpdir, "B.h5ad")
    Cecelia._write_clust_features!(props, "immune", ["spatial.comp.B_qc.immune"], ["u1", "u2"];
                                   family = "regions", labels = Dict("spatial.comp.B_qc.immune" => "B/qc"))
    @test _clust_features(props, ["immune"], "regions")["immune"] == ["spatial.comp.B_qc.immune"]
    @test _clust_members(props, ["immune"], "regions")["immune"]  == ["u1", "u2"]
    @test _clust_feature_labels(props, ["immune"], "regions")["immune"]["spatial.comp.B_qc.immune"] == "B/qc"
    @test isempty(_clust_features(props, ["immune"], "clusters")["immune"])   # different family
    @test isempty(_clust_feature_labels(props, ["immune"], "clusters"))      # no labels → key omitted
    @test isempty(_clust_members(joinpath(lpdir, "gone.h5ad"), ["immune"], "regions")["immune"])

    # REGRESSION: an entry written BEFORE the `labels` field existed — i.e. any image not re-run since.
    # Reading it must not throw. `something(get(...), nothing)` did: with every argument `nothing`,
    # `something()` raises ArgumentError("No value arguments present"), which surfaced as repeated 500s
    # from /api/gating/channels?popType=region.
    legacy = joinpath(lpdir, "L.h5ad")
    open(replace(legacy, r"\.h5ad$" => ".clustfeatures.json"), "w") do f
        JSON3.write(f, Dict("niches" => Dict("features" => ["B/qc"], "partOf" => ["u1"])))
    end
    @test _clust_feature_labels(legacy, ["niches"], "regions") == Dict{String,Any}()
    @test _clust_members(legacy, ["niches"], "regions")["niches"] == ["u1"]
    @test _clust_features(legacy, ["niches"], "regions")["niches"] == ["B/qc"]
    # …and the oldest layout of all: a bare feature ARRAY, with no partOf and no labels
    oldest = joinpath(lpdir, "O.h5ad")
    open(replace(oldest, r"\.h5ad$" => ".clustfeatures.json"), "w") do f
        JSON3.write(f, Dict("old" => ["f1", "f2"]))
    end
    @test _clust_feature_labels(oldest, ["old"], "regions") == Dict{String,Any}()
    @test _clust_members(oldest, ["old"], "regions")["old"] == String[]
    # a suffix with no entry at all must also be safe (the obs column exists, the sidecar lags)
    @test _clust_feature_labels(props, ["nosuchrun"], "regions") == Dict{String,Any}()
    @test _clust_members(props, ["nosuchrun"], "regions")["nosuchrun"] == String[]
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
        proj = create_project!(name="api-lablog")
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

        # [LabArchives] is a PROVENANCE claim and the caller picks it, so the server makes the one check
        # it honestly can: no linked notebook ⇒ no notebook provenance. 409 with an actionable message.
        st_la, body_la = _post(api_lablog_append,
            Dict("projectUid"=>uid,"author"=>"LabArchives","lines"=>["from the ELN"]))
        @test st_la == 409
        @test occursin("set_labarchives_context", JSON3.read(body_la).error)
        @test !occursin("[LabArchives]", read_ll().content)          # and nothing was written
        # …every other author is unaffected by the guard
        @test _post(api_lablog_append, Dict("projectUid"=>uid,"author"=>"Claude","lines"=>["ok"]))[1] == 200

        # once a notebook IS linked, the same append is accepted
        write_la_doc!(load_project(uid); source = Dict("notebookName" => "Ailsa"),
                      sections = [Dict("heading" => "Setup", "lines" => ["x"])])
        st_ok, body_ok = _post(api_lablog_append,
            Dict("projectUid"=>uid,"author"=>"LabArchives","lines"=>["from the ELN"]))
        @test st_ok == 200 && occursin("[LabArchives]", JSON3.read(body_ok).block)


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
            @test occursin(img.uid, cap.block) && !occursin("img-1", cap.block)   # digest refs by uid
            # read exposes the uid→name map the "Show names" toggle resolves against
            let r = read_ll()
                @test Symbol(img.uid) in propertynames(r.imageNames) && getproperty(r.imageNames, Symbol(img.uid)) == "img-1"
            end
        end
        # bad requests
        @test _post(api_lablog_capture, Dict())[1] == 400              # projectUid missing
        @test _post(api_lablog_capture, Dict("projectUid"=>"nope"))[1] == 404

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

# Chain authoring from outside the whiteboard (Claude via the MCP) + rename. The two properties that
# matter: create NEVER overwrites a chain the user wired, and an invalid template is rejected HERE
# rather than mid-run, after the user pressed Run on something they didn't write.
@testset "API: chain create (create-only + validated) and rename" begin
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name="api-chains")
        uid  = proj.uid
        rm_params = Dict("valueName"=>"default", "newDefault"=>"default")
        node(id) = Dict("id"=>id, "fn"=>"importImages.remove", "params"=>rm_params)
        tmpl(name, nodes, edges) = Dict("name"=>name, "nodes"=>nodes, "edges"=>edges)
        create(t) = _post(api_chains_create, Dict("projectUid"=>uid, "template"=>t))
        path(name) = joinpath(tmp, uid, "settings", "chains", "$(name).json")

        # guards
        @test create(nothing)[1] == 400
        @test _post(api_chains_create, Dict("projectUid"=>uid))[1] == 400            # no template
        @test _post(api_chains_create, Dict("template"=>tmpl("x", [node("n1")], [])))[1] == 400
        @test create(tmpl("", [node("n1")], []))[1] == 400                           # no name
        @test _post(api_chains_create,
                    Dict("projectUid"=>"nope",
                         "template"=>tmpl("x", [node("n1")], [])))[1] == 404

        # a name is a filename — path traversal must not resolve anywhere
        for bad in ("../../evil", "a/b", "..", ".hidden")
            @test create(tmpl(bad, [node("n1")], []))[1] == 400
        end

        # happy path
        st, body = create(tmpl("pipeline", [node("n1"), node("n2")],
                               [Dict("from"=>"n1", "to"=>"n2")]))
        @test st == 200
        @test JSON3.read(body).nodeCount == 2
        @test isfile(path("pipeline"))

        # CREATE-ONLY: the whole point — an outside author cannot replace the user's chain
        @test create(tmpl("pipeline", [node("n1")], []))[1] == 409

        # …while the whiteboard's own save still overwrites verbatim (unchanged behaviour)
        @test _post(api_chains_save,
                    Dict("projectUid"=>uid, "template"=>tmpl("pipeline", [node("n9")], [])))[1] == 200

        # VALIDATION, and the message names the offender so the author can fix it
        st, body = create(tmpl("bad-fn", [Dict("id"=>"oops", "fn"=>"importImages.nope")], []))
        @test st == 400
        err = String(JSON3.read(body).error)
        @test occursin("oops", err) && occursin("importImages.nope", err)
        @test create(tmpl("dangling", [node("n1")], [Dict("from"=>"n1","to"=>"ghost")]))[1] == 400
        @test create(tmpl("cyclic", [node("n1"), node("n2")],
                          [Dict("from"=>"n1","to"=>"n2"), Dict("from"=>"n2","to"=>"n1")]))[1] == 400
        @test !isfile(path("bad-fn"))          # a rejected template leaves nothing on disk

        # SPARSE params are accepted — an outside author sets only what it means to; the whiteboard
        # fills the rest from the spec defaults when it loads the template.
        @test create(tmpl("sparse", [Dict("id"=>"n1", "fn"=>"tracking.bayesian_tracking",
                                          "params"=>Dict("maxSearchRadius"=>35))], []))[1] == 200

        # startTargets is FILLED with the roots when the author omits it. Without it the whiteboard
        # draws no start dot (buildStartGraph returns null with no target and no saved position), so the
        # chain opens with nothing marking where a run begins — which is how the first authored chain
        # reached the user. Execution is unchanged either way; this is for the editor.
        @test create(tmpl("pipeline-is-rooted", [node("first"), node("second")],
                          [Dict("from"=>"first", "to"=>"second")]))[1] == 200
        @test JSON3.read(read(path("pipeline-is-rooted"), String)).startTargets == ["first"]
        # an explicit startTargets is respected (starting a run part-way in)
        rooted = tmpl("pipeline-mid", [node("a"), node("b")], [Dict("from"=>"a", "to"=>"b")])
        rooted["startTargets"] = ["b"]
        @test create(rooted)[1] == 200
        @test JSON3.read(read(path("pipeline-mid"), String)).startTargets == ["b"]

        # ── rename ──
        ren(from, to) = _post(api_chains_rename,
                              Dict("projectUid"=>uid, "name"=>from, "newName"=>to))
        @test ren("pipeline", "")[1] == 400                     # newName required
        @test ren("ghost", "whatever")[1] == 404                # source must exist
        @test ren("pipeline", "sparse")[1] == 409               # target must not
        @test ren("pipeline", "../evil")[1] == 400              # guarded on both names
        @test ren("pipeline", "pipeline")[1] == 200             # no-op, not an error

        st, body = ren("pipeline", "pipeline-v2")
        @test st == 200 && String(JSON3.read(body).name) == "pipeline-v2"
        @test isfile(path("pipeline-v2")) && !isfile(path("pipeline"))
        # the `name` FIELD moves too — else the whiteboard saves the renamed chain back under the old
        # name and the rename silently undoes itself on the next save
        @test String(JSON3.read(read(path("pipeline-v2"), String)).name) == "pipeline-v2"
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
        proj = create_project!(name="api-del")
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

# Deleting a label set must take its COMPANIONS with it. Before this, the route removed `labels[vn]`
# and `label_props[vn]` and left `{vn}__tracks.h5ad`, `{vn}__branch.h5ad`, the branch zarr and the
# clustfeatures sidecars behind as files nothing could reach — invisible, and counted as analysis
# forever. The prefix rule is what makes it complete; the "B2 survives" case is what stops it being
# too greedy.
@testset "API: deleting a label set sweeps its tracks/branch/cluster companions" begin
    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name="api-del-labels")
        s    = add_set!(proj; name="s")
        img  = add_image!(s; name="a")

        labels_dir = joinpath(img._dir, "labels");       mkpath(labels_dir)
        branch_dir = joinpath(img._dir, "branchLabels"); mkpath(branch_dir)
        props_dir  = joinpath(img._dir, "labelProps");   mkpath(props_dir)
        mkpath(joinpath(labels_dir, "B.zarr")); write(joinpath(labels_dir, "B.zarr", "c"), "x")
        mkpath(joinpath(branch_dir, "B.zarr")); write(joinpath(branch_dir, "B.zarr", "c"), "x")
        for f in ("B.h5ad", "B__tracks.h5ad", "B__branch.h5ad",
                  "B.clustfeatures.json", "B__tracks.clustfeatures.json",
                  "B2.h5ad")                                  # B2 must NOT be swept by the "B" prefix
            write(joinpath(props_dir, f), "x")
        end
        img.labels        = Dict("B"=>["B.zarr"], "B2"=>["B2.zarr"])
        img.label_props   = Dict("B"=>"B.h5ad", "B2"=>"B2.h5ad")
        img.branch_labels = Dict("B"=>["B.zarr"])
        save!(img)

        st, body = _post(api_images_delete_labels,
                         Dict("projectUid"=>proj.uid, "imageUid"=>img.uid, "valueName"=>"B"))
        @test st == 200 && JSON3.read(body).ok == true

        @test !ispath(joinpath(labels_dir, "B.zarr"))                       # cell labels
        @test !ispath(joinpath(branch_dir, "B.zarr"))                       # branch labels — the gap
        for f in ("B.h5ad", "B__tracks.h5ad", "B__branch.h5ad",
                  "B.clustfeatures.json", "B__tracks.clustfeatures.json")
            @test !isfile(joinpath(props_dir, f))
        end
        @test isfile(joinpath(props_dir, "B2.h5ad"))                        # a sibling name survives

        ri = init_object(proj.uid, img.uid)
        @test !haskey(ri.labels, "B") && !haskey(ri.branch_labels, "B")     # registrations cleared
        @test !haskey(ri.label_props, "B")
        @test haskey(ri.labels, "B2")                                       # B2 still registered
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
        proj = create_project!(name="api-tasklog")
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
        # per-image attribute ASSIGNMENT — the observer needs it to size the groups of a cross-image
        # plot (the AXES come from /api/plots/attrs, which stays the one discovery route). An image
        # with no attributes must surface an empty map, not a missing key.
        let r = JSON3.read(api_images_list(HTTP.Request("GET", "/api/images?projectUid=$uid"))[2])
            @test all(i -> isempty(i.attr), r.images)
        end
        img1.attr = Dict{String,Any}("Mouse" => "3", "Location" => "b"); save!(img1)
        let r = JSON3.read(api_images_list(HTTP.Request("GET", "/api/images?projectUid=$uid"))[2])
            byname = Dict(i.name => i for i in r.images)
            @test byname["img-1"].attr.Mouse == "3" && byname["img-1"].attr.Location == "b"
            @test isempty(byname["img-2"].attr)
        end
        @test api_images_list(HTTP.Request("GET", "/api/images"))[1] == 400          # projectUid missing
        @test api_images_list(HTTP.Request("GET", "/api/images?projectUid=nope"))[1] == 404

        # ── image metadata payload: original file location (oriPath) + filtered extraMeta ──
        # The image-info dialog needs the source file path (kept in meta as ori_path) and a generic
        # bucket for any other scalar meta — but NOT keys already surfaced as fields, nor internal
        # bookkeeping (funParams) or nested display config (channel_colormaps).
        img2.meta["SizeC"]            = 3                              # → sizeC field, must NOT double into extraMeta
        img2.meta["Objective"]        = "40x/1.3"                      # arbitrary scalar → surfaced generically
        img2.meta["funParams"]        = Dict{String,Any}("x" => 1)    # internal nested dict → excluded
        img2.meta["channel_colormaps"] = ["red", "green"]             # nested/display → excluded
        save!(img2)
        let r = JSON3.read(api_images_meta(HTTP.Request("GET", "/api/images/meta?projectUid=$uid&imageUid=$(img2.uid)"))[2])
            @test r.image.oriPath == "/tmp/b.tif"
            @test r.image.sizeC == 3
            @test r.image.extraMeta.Objective == "40x/1.3"
            @test !haskey(r.image.extraMeta, :SizeC)              # already a first-class field
            @test !haskey(r.image.extraMeta, :ori_path)           # surfaced as oriPath
            @test !haskey(r.image.extraMeta, :funParams)          # internal nested dict
            @test !haskey(r.image.extraMeta, :channel_colormaps)  # nested display config
        end
        @test api_images_meta(HTTP.Request("GET", "/api/images/meta?projectUid=$uid"))[1] == 400           # imageUid missing
        @test api_images_meta(HTTP.Request("GET", "/api/images/meta?projectUid=$uid&imageUid=nope"))[1] == 404

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
        append_run_log!(img2, "tracking.bayesian_tracking", "default", "failed",
                        Dict{String,Any}("maxSearchRadius" => 20, "maxLost" => 3))
        let r = JSON3.read(hist("projectUid=$uid")[2])
            @test r.count == 2
            funs = [h.fun for h in r.history]
            @test "segment.cellpose" in funs && "tracking.bayesian_tracking" in funs
            @test all(h -> h.imageUid in (img1.uid, img2.uid), r.history)
            # per-run outcome surfaced under runStatus (distinct from the image's `status`)
            byfun = Dict(String(h.fun) => h for h in r.history)
            @test String(byfun["segment.cellpose"].runStatus) == "done"          # default
            @test String(byfun["tracking.bayesian_tracking"].runStatus) == "failed"
            # the tuning trail rides along per row (Observer Phase 2 §1): the run's params
            @test byfun["tracking.bayesian_tracking"].params.maxSearchRadius == 20
            @test isempty(byfun["segment.cellpose"].params)                      # no params → {}
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

@testset "API: attribute normalisation on write" begin
    # The attr routes are the only place user-typed names/values enter the model, so they trim there.
    # Untrimmed, these pairs would each be TWO distinct values — two filter chips in the image table,
    # two segments in a generated movie name — or two separate attribute columns.
    @test _norm_attr("a") == "a"
    @test _norm_attr(" a ") == "a"
    @test _norm_attr("\tLocation\n") == "Location"
    @test _norm_attr(" a ") == _norm_attr("a")

    # Whitespace-only collapses to "" — the canonical UNSET that attr/create seeds a column with.
    # It must stay a value, not become a deletion: the key's presence is what makes the column exist.
    @test _norm_attr("") == ""
    @test _norm_attr("   ") == ""

    # interior whitespace is content, not padding
    @test _norm_attr(" day 3 ") == "day 3"

    # and the reason it matters downstream: a blank value is already dropped from movie names, so
    # normalising at the write is what keeps that defence from being needed in every consumer.
    @test _movie_basename(Dict("T" => _norm_attr("  ")), "u1", ["T"]) == "u1.mp4"
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
    # channels token expands to the shown channel names joined by '-', positioned by its order
    chans = ["CD3", "CD8"]
    @test _movie_basename(attr, "AbC123", ["Day", MOVIE_CHANNELS_TOKEN], chans) == "3_CD3-CD8_AbC123.mp4"
    @test _movie_basename(attr, "AbC123", [MOVIE_CHANNELS_TOKEN, "Treatment"], chans) == "CD3-CD8_CNO_AbC123.mp4"
    # token with no shown channels drops out cleanly (no dangling separator)
    @test _movie_basename(attr, "AbC123", ["Day", MOVIE_CHANNELS_TOKEN], String[]) == "3_AbC123.mp4"

    # What TERMINATES the name is a choice: a single viewer recording is named after the IMAGE
    # (`_movie_named_path`), a batch after the uid — so regenerating a restored viewer config wrote a
    # uid-named twin beside the original (Dominik, 2026-08-10). `name` ends it with the image instead.
    @test _movie_basename(attr, "AbC123", String[]; name = "M2b-MERTK_KAT (cropped)") ==
          "M2b-MERTK_KAT_cropped.mp4"
    @test _movie_basename(attr, "AbC123", ["Day"]; name = "my image") == "3_my_image.mp4"
    # …and this is exactly what the single-image recorder produces for the same image, which is the
    # whole point — the two namers must agree once the batch is asked to name by image
    @test _movie_basename(Dict{String,String}(), "AbC123", String[]; name = "My Image") ==
          basename(_movie_named_path((; name = "My Image", _dir = joinpath("p", "1", "u")), "AbC123"))
    # a name of pure punctuation sanitises to nothing, and a file still has to be written
    @test _movie_basename(attr, "AbC123", String[]; name = "()") == "AbC123.mp4"
    @test _movie_basename(attr, "AbC123", String[]; name = "   ") == "AbC123.mp4"
    # blank `name` is the default and keeps the uid — the safe one, since two images CAN share a name
    @test _movie_basename(attr, "AbC123", String[]) == "AbC123.mp4"
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
        # A name ENDING in a character a filename can't hold — the crop task's "(cropped)" is the one
        # that showed up in the movies list — must not leave the separator it collapses to. It used to,
        # and the animation variant then doubled it ("…_cropped__animation.mp4").
        cropped = (; _dir = joinpath(tmp, "proj", "1", "uid7"),
                     name = "M2b-MERTK_KAT-SWHL-GFP-Tom-res (cropped)")
        @test _movie_named_path(cropped, "uid7") ==
              joinpath(tmp, "proj", "movies", "M2b-MERTK_KAT-SWHL-GFP-Tom-res_cropped.mp4")
        @test _movie_named_path(cropped, "uid7"; suffix = "_animation") ==
              joinpath(tmp, "proj", "movies", "M2b-MERTK_KAT-SWHL-GFP-Tom-res_cropped_animation.mp4")
        # a name with nothing usable left also falls back to the uid
        parens = (; _dir = joinpath(tmp, "proj", "1", "uid7"), name = "()")
        @test _movie_named_path(parens, "uid7") == joinpath(tmp, "proj", "movies", "uid7.mp4")
    end
end

# ONE sanitiser behind the image name, the user's suffix and the attr-composed basename — they used to
# be three near-copies and only one of them stripped edge separators. Mirrored in the frontend by
# `safeNamePart` (frontend/src/utils/batchMovie.ts), whose testset asserts the same cases.
# The 3D detail level an authored/batch movie config asks for. Absent means FULL RESOLUTION, not
# napari's automatic choice: napari picks the coarsest level in 3D, which erases a strided label
# pyramid, and a config written before this control existed still wants visible masks.
@testset "API: 3D detail level from a movie config" begin
    @test _detail_3d(Dict(:detail3d => 0)) == 0
    @test _detail_3d(Dict(:detail3d => 2)) == 2
    @test _detail_3d(Dict(:detail3d => "3")) == 3          # JSON numbers may arrive as strings
    @test _detail_3d(Dict{Symbol,Any}()) == 0              # absent → full resolution
    @test _detail_3d(Dict(:detail3d => nothing)) === nothing   # explicit null → leave it to napari
    @test _detail_3d(Dict(:detail3d => -1)) == 0           # never a negative index
end

@testset "API: filename fragments are sanitised one way" begin
    @test _safe_name_part("M2b-MERTK_KAT-SWHL-GFP-Tom-res (cropped)") ==
          "M2b-MERTK_KAT-SWHL-GFP-Tom-res_cropped"
    @test _safe_name_part("a/b c:d") == "a_b_c_d"
    @test _safe_name_part("Day 3.v2-final") == "Day_3.v2-final"
    @test _safe_name_part("../../etc/passwd") == "etc_passwd"
    @test _safe_name_part("__x__") == "x"
    @test _safe_name_part("   ") == ""
    @test _safe_name_part("()") == ""
    @test _safe_name_part(nothing) == ""
end

# Side-by-side version comparison (docs/todo/MOVIE_COMPARE_PLAN.md). The pure parts: which versions a
# config asks for, what each column is, how the contrast toggle reads, and the frame arithmetic behind
# the single progress bar. The recording loop itself needs a live viewer and is not exercised here.
@testset "API: movie version comparison" begin
    # Which versions to record. A config from before comparisons existed carries one `valueName`, and
    # "" (the active version) is a perfectly good single column — the list is never empty, so the
    # caller always has something to record.
    @test _config_value_names(Dict(:valueNames => ["default", "af"])) == ["default", "af"]
    @test _config_value_names(Dict(:valueName => "af"))               == ["af"]
    @test _config_value_names(Dict{Symbol,Any}())                     == [""]
    @test _config_value_names(Dict(:valueNames => String[], :valueName => "af")) == ["af"]

    # Each column carries the WHOLE authored config with its own version pinned — the overlays and
    # channels must not differ between columns, only the version does.
    cols = _version_columns(Dict(:channels => Dict("CD3" => "green"), :showTracks => true),
                            ["default", " af_corrected ", ""])
    @test [c.label for c in cols] == ["default", "af_corrected", "active"]   # "" captions as "active"
    @test [String(c.config[:valueName]) for c in cols] == ["default", "af_corrected", ""]
    @test all(c -> c.config[:showTracks] === true, cols)
    @test all(c -> haskey(c.config, :channels), cols)
    # Symbol keys, because that is how every config reader here addresses them
    @test get(cols[1].config, :valueName, "MISSING") == "default"

    # D4 — the contrast toggle. Anything unrecognised reads as the default rather than failing a batch.
    @test _share_contrast("reference")
    @test _share_contrast("")
    @test _share_contrast("nonsense")
    @test !_share_contrast("version")

    # …and what "each version keeps its own settings" actually drops: the per-layer props, never the
    # camera or the timepoint (columns framed differently would not be a comparison).
    snap = Dict("camera" => Dict("zoom" => 2.0), "dims" => Dict("current_step" => [3]),
                "layers" => Dict("CD3" => Dict("contrast_limits" => [0, 100])))
    @test !haskey(_camera_only(snap), "layers")
    @test _camera_only(snap)["camera"] == Dict("zoom" => 2.0)
    @test _camera_only(snap)["dims"]   == Dict("current_step" => [3])

    # Frame arithmetic for ONE progress bar across the passes + the compose. Mirrors the bridge's own
    # range maths: one frame per timepoint, both ends inclusive.
    img20 = (; meta = Dict("SizeT" => 20))
    @test _t_sweep_frames(img20, 0, nothing) == 20
    @test _t_sweep_frames(img20, 5, nothing) == 15
    @test _t_sweep_frames(img20, 0, 9)       == 10
    @test _t_sweep_frames(img20, 0, 99)      == 20      # clamped to the stack
    @test _t_sweep_frames(img20, 8, 8)       == 0       # empty range
    @test _t_sweep_frames((; meta = Dict("SizeT" => 1)), 0, nothing) == 0
    @test _t_sweep_frames((; meta = Dict{String,Any}()), 0, nothing) == 0   # image doesn't say

    @test _comparison_frame_total(2, 20) == 60          # 2 passes + the compose
    @test _comparison_frame_total(3, 20) == 80
    @test _comparison_frame_total(1, 20) == 0           # one column = a plain record, own total
    @test _comparison_frame_total(2, 0)  == 0           # unknown T → let each pass report its own
end

# The comparison GRID (docs/todo/MOVIE_COMPARE_PLAN.md, generalised). `_record_grid!` and everything
# under it is blind to what made two cells differ, so what needs pinning is the layer above: what shape
# a pair of selections means, what each cell pins, and what a cell says about the masks it draws.
@testset "API: movie comparison grid" begin
    base = Dict{Symbol,Any}(:channels => Dict("CD3" => "green"))

    # 2+ of BOTH → the cross-product. One row per MASK; that row's cells are the VERSIONS, so versions
    # read across and masks read down.
    grid = _compare_grid(merge(base, Dict{Symbol,Any}(
        :valueNames => ["default", "af"], :labelValueNames => ["cellpose", "coastal"])))
    @test length(grid) == 2
    @test [r.label for r in grid] == ["cellpose", "coastal"]
    @test [c.label for c in grid[1].columns] == ["default", "af"]
    @test [String(c.config[:valueName]) for c in grid[1].columns] == ["default", "af"]
    # every cell of a row draws THAT row's mask, and only it
    @test all(c -> c.config[:labelValueNames] == ["cellpose"], grid[1].columns)
    @test all(c -> c.config[:labelValueNames] == ["coastal"],  grid[2].columns)
    @test all(r -> all(c -> haskey(c.config, :channels), r.columns), grid)   # the config rides along
    # rectangular: 2 x 2 is FOUR renders, not two — the cost is multiplicative
    @test sum(length(r.columns) for r in grid) == 4

    # 2+ of ONE only → a single row, side by side, whichever list it came from.
    vonly = _compare_grid(Dict{Symbol,Any}(:valueNames => ["default", "af"]))
    @test length(vonly) == 1
    @test [c.label for c in vonly[1].columns] == ["default", "af"]
    @test vonly[1].label == ""                       # no outer compose → nothing to caption

    monly = _compare_grid(Dict{Symbol,Any}(:valueNames => ["af"], :labelValueNames => ["a", "b"]))
    @test length(monly) == 1
    @test [c.label for c in monly[1].columns] == ["a", "b"]
    # …all on the ONE selected version, which is what keeps a mask row comparable
    @test all(c -> String(c.config[:valueName]) == "af", monly[1].columns)
    @test [c.config[:labelValueNames] for c in monly[1].columns] == [["a"], ["b"]]

    # A single mask is drawn in the one cell rather than becoming a row of its own.
    one = _compare_grid(Dict{Symbol,Any}(:labelValueNames => ["a"]))
    @test length(one) == 1 && length(one[1].columns) == 1
    @test one[1].columns[1].config[:labelValueNames] == ["a"]

    # Nothing selected is still ONE cell — a plain movie is a 1x1 grid, not a special case.
    plain = _compare_grid(Dict{Symbol,Any}())
    @test length(plain) == 1 && length(plain[1].columns) == 1
    @test String(plain[1].columns[1].config[:valueName]) == ""

    # D4 — the contrast toggle. Anything unrecognised reads as the default rather than failing a batch.
    @test _share_contrast("reference") && _share_contrast("") && _share_contrast("nonsense")
    @test !_share_contrast("version")

    # Frame arithmetic for ONE progress bar across every pass AND every compose.
    @test _grid_frame_total(1, 2, 20) == 60          # 2 passes + the compose = the old 1-D case
    @test _grid_frame_total(1, 3, 20) == 80
    @test _comparison_frame_total(2, 20) == _grid_frame_total(1, 2, 20)
    @test _grid_frame_total(2, 2, 20) == 140         # 4 cells + 2 row composes + 1 stack
    @test _grid_frame_total(3, 1, 20) == 80          # a column of 3 = 3 cells + the stack (no row composes)
    @test _grid_frame_total(1, 1, 20) == 0           # one cell = a plain record, own total
    @test _grid_frame_total(2, 2, 0)  == 0           # unknown T → let each pass report its own

    # The count `_record_grid!` actually uses is read off the grid, so it cannot drift from the loop
    # (which walks it with a running counter). It must agree with the rectangular form above on every
    # rectangular grid — that agreement IS the contract between the progress bar and the pass loop.
    for (vs, ms) in ((["a", "b"], ["s1", "s2"]), (["a", "b", "c"], ["s1", "s2"]),
                     (["a", "b"], String[]), (["a"], ["s1", "s2"]), (String[], String[]))
        g = _compare_grid(Dict{Symbol,Any}(:valueNames => vs, :labelValueNames => ms))
        @test _grid_frame_total(g, 20) == _grid_frame_total(length(g), length(g[1].columns), 20)
    end
    # …and a RAGGED grid (which `_compare_grid` never builds, but the loop tolerates) is counted by
    # what is in it, not by its widest row: 2 + 1 cells, one row compose, one stack = 5 units.
    ragged = MovieRow[(; label = "r1", columns = _version_columns(Dict{Symbol,Any}(), ["a", "b"])),
                      (; label = "r2", columns = _version_columns(Dict{Symbol,Any}(), ["a"]))]
    @test _grid_frame_total(ragged, 20) == 100

    # The column list is authored ONCE for a whole batch, so it does not vary per image — unlike the
    # per-image mask list below, which drops what an image hasn't got.
    @test _config_compare_segmentations(Dict(:labelValueNames => ["a", " b ", "a", ""])) == ["a", "b"]
    @test _config_compare_segmentations(Dict{Symbol,Any}()) == String[]

    # Masks per image: THREE-valued. `nothing` (key absent) must stay distinct from an empty list —
    # absent leaves the canvas alone (what "record what's on screen" needs), empty means no masks.
    img = (; labels = Dict{String,Vector{String}}("a" => ["a.zarr"], "b" => ["b.zarr", "b_nuc.zarr"]))
    @test _config_label_value_names(Dict{Symbol,Any}(), img)                  === nothing
    @test _config_label_value_names(Dict(:labelValueNames => String[]), img)  == String[]
    @test _config_label_value_names(Dict(:labelValueNames => ["b", "a"]), img) == ["b", "a"]
    # unregistered + duplicate names are dropped rather than handed to the bridge
    @test _config_label_value_names(Dict(:labelValueNames => ["a", "gone", "a"]), img) == ["a"]
    # an image with no label registry at all is not an error — it simply has no masks
    @test _config_label_value_names(Dict(:labelValueNames => ["a"]), (; name = "x")) == String[]

    # Mask OUTLINE width. Clamped, not validated: a bad value is a display nicety and must not fail a
    # whole batch, and napari has no meaning for a negative contour.
    @test _label_contour(Dict{Symbol,Any}())              == 0      # absent → filled, what it always was
    @test _label_contour(Dict(:labelContour => 3))        == 3
    @test _label_contour(Dict(:labelContour => -2))       == 0
    @test _label_contour(Dict(:labelContour => 999))      == LABEL_CONTOUR_MAX
    @test _label_contour(Dict(:labelContour => 2.7))      == 3      # _to_int rounds

    # How much of the z stack a movie shows. `show3D` WINS over a z index: the index is a leftover from
    # the last time 2D was chosen, and dropping it silently would lose the user's slice.
    @test !_show_3d(Dict{Symbol,Any}())
    @test _show_3d(Dict(:show3D => true))
    @test _z_slice(Dict{Symbol,Any}())                       === nothing   # "whatever is showing"
    @test _z_slice(Dict(:zSlice => 4))                       == 4
    @test _z_slice(Dict(:zSlice => -1))                      == 0          # floored, clamped again bridge-side
    @test _z_slice(Dict(:show3D => true, :zSlice => 4))      === nothing   # 3D ignores the index…
    @test _z_slice(Dict(:show3D => false, :zSlice => 4))     == 4          # …and keeps it for next time

    # …and the store files that go on the wire, in the {valueName => [files]} shape show-labels takes.
    @test _label_files_for(img, ["b"])   == Dict("b" => ["b.zarr", "b_nuc.zarr"])
    @test _label_files_for(img, nothing) == Dict{String,Vector{String}}()
    @test _label_files_for(img, ["gone"]) == Dict{String,Vector{String}}()

    # SKELETONS are the second registry with the same three-valued contract — separate stores, a
    # separate picker (BRANCHING_PLAN Decision 6), and they had the identical bug for the identical
    # reason. The two must not read each other's names.
    both = (; labels        = Dict{String,Vector{String}}("a" => ["a.zarr"]),
              branch_labels = Dict{String,Vector{String}}("sk" => ["sk.zarr"]))
    @test _config_branch_value_names(Dict{Symbol,Any}(), both)                    === nothing
    @test _config_branch_value_names(Dict(:branchValueNames => String[]), both)   == String[]
    @test _config_branch_value_names(Dict(:branchValueNames => ["sk"]), both)     == ["sk"]
    # a mask name is not a skeleton name, and vice versa — each is filtered by its OWN registry
    @test _config_branch_value_names(Dict(:branchValueNames => ["a"]), both)      == String[]
    @test _config_label_value_names(Dict(:labelValueNames => ["sk"]), both)       == String[]
    @test _branch_files_for(both, ["sk"]) == Dict("sk" => ["sk.zarr"])
    @test _branch_files_for(both, ["a"])  == Dict{String,Vector{String}}()
    # an image with no skeletons at all is not an error
    @test _config_branch_value_names(Dict(:branchValueNames => ["sk"]), img) == String[]
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
        proj = create_project!(name="api-observer")
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
        proj = create_project!(name="api-taskfail")
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

# ── Every status frame carries the task's timing ───────────────────────────────
# `ws_status` is the rail's one status sink, so it is where a client learns WHEN a task ran. Without
# this a client can only time a task from when its own socket happened to receive the frame — which
# restarts at zero on a page reload and overstates by the poll delay on a recovered frame.
#
# It is also the sink that covers the producers with NO scheduler record — background jobs
# (`pool="job"`) and batch movies (`pool="viewer"`) announce themselves only here — so `running`
# notes the start on the rail rather than assuming somebody upstream did.
@testset "API: status frames carry the task's timing" begin
    cap = Channel{String}(64)
    key = gensym("test-tasktime")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    drain() = (fs = Any[]; while isready(cap); push!(fs, JSON3.read(take!(cap))); end; fs)
    isots   = r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$"
    try
        tid = "wsdur$(rand(1000:9999))"
        forget_task_start!(tid)

        # queued: nothing has started, so the frame says so with "" — never a placeholder date
        drain()
        ws_status(nothing, tid, "queued"; fun="project:export", pool="job")
        let f = only(drain())
            @test f.startedAt == "" && f.finishedAt == ""
        end

        # running: the sink itself notes the start (this producer has no TaskRecord) and publishes it
        ws_status(nothing, tid, "running"; fun="project:export", pool="job")
        started = only(drain()).startedAt
        @test occursin(isots, started)
        @test iso_utc(task_started_at(tid)) == started

        # a re-announced running must not restart the clock — the same start comes back out
        ws_status(nothing, tid, "running"; fun="project:export", pool="job")
        @test only(drain()).startedAt == started

        # terminal: both ends, and they are the SAME values banked for replay — a client that missed
        # this frame and recovers it from /api/tasks/recent must not compute a different duration
        ws_status(nothing, tid, "done"; fun="project:export", pool="job")
        let f = only(drain())
            @test f.startedAt == started
            @test occursin(isots, f.finishedAt)
            row = only(filter(r -> r.id == tid, recent_tasks()))
            @test row.started_at == f.startedAt && row.finished_at == f.finishedAt
        end
        # …and the in-flight note is released once the row owns it
        @test isnothing(task_started_at(tid))
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
    end
end

# ── One run's slice of a cumulative task log ───────────────────────────────────
# `logs/{fun}.log` is appended to by EVERY run of that fun on that image, and its lines are stamped in
# LOCAL time. The GUI backfills the log of a task that was already running when the tab connected, and it
# knows only that task's UTC `started_at` — so the slice happens server-side, where the clock that wrote
# the stamps lives. Without it a backfilled row would show output from previous runs as its own.
@testset "API: task log sliced from a run's start" begin
    off  = _tasklog_local_offset()                       # what the writer's stamps are offset by
    # a stamp N seconds ago, written the way `_wrap_log_with_file` writes them
    stamp(secs) = Dates.format(Dates.now(UTC) + off - Dates.Second(secs), "yyyy-mm-dd HH:MM:SS")
    iso(secs)   = iso_utc(Dates.now(UTC) - Dates.Second(secs))

    log = """
    [$(stamp(600))] old run: starting
    [$(stamp(590))] old run: done
    [$(stamp(60))] this run: starting
    a bare continuation line
    [$(stamp(10))] this run: 5/20
    """
    log = join(lstrip.(split(strip(log), '\n')), '\n') * '\n'

    kept = _tasklog_since(log, iso(120))                 # this run started 2 minutes ago
    @test occursin("this run: starting", kept)
    @test occursin("this run: 5/20", kept)
    @test !occursin("old run", kept)                     # ← the whole point
    # an unstamped line belongs to the line above it, so a multi-line message isn't torn apart
    @test occursin("a bare continuation line", kept)

    # a start BEFORE everything keeps everything; one after everything keeps nothing
    @test occursin("old run: starting", _tasklog_since(log, iso(9999)))
    @test strip(_tasklog_since(log, iso(-60))) == ""

    # garbage `since` degrades to the whole file — showing too much beats showing nothing
    @test _tasklog_since(log, "not a timestamp") == log
    @test _tasklog_since("", iso(120)) == ""
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
        # the MCP config is written on STATUS (not only on a feedback run) so the info panel can always
        # offer `claude --mcp-config <path>` — the user never hand-registers an MCP server
        @test isfile(String(s.mcpConfigPath))
        let cfg = JSON3.read(read(String(s.mcpConfigPath), String))
            @test haskey(cfg.mcpServers, Symbol("cecelia-observer"))
        end
        # terminal-setup detection: which button the lab-log toolbar shows (setup vs Chat to Claude).
        # Don't assert WHICH state — it depends on the dev machine's ~/.claude.json — but `ready` must
        # mean exactly "current", since the UI treats a stale entry as not set up.
        @test String(s.terminal.state) in Set(["missing", "stale", "shadowed", "current"])
        @test s.terminal.ready isa Bool
        @test s.terminal.ready == (String(s.terminal.state) == "current")
        # a per-folder (`local`-scope) entry overrides our user-scope one, so "registered correctly"
        # is not the same as "the user's terminal works" — `shadowed` names the folders that break it
        # Asserted as implications, not an equality: a shadow can coexist with a missing/stale user-scope
        # entry, and then THAT is the headline state (setup still fixes both).
        @test s.terminal.shadowedDirs isa JSON3.Array
        String(s.terminal.state) == "shadowed" && @test !isempty(s.terminal.shadowedDirs)
        isempty(s.terminal.shadowedDirs) || @test !s.terminal.ready
    end

    # feedback: validated before anything is spawned.
    @test _post(api_observer_feedback, Dict())[1] == 400                       # projectUid missing
    @test _post(api_observer_feedback, Dict("projectUid" => "nope"))[1] == 404 # unknown project

    # clear context: same validation, no spawn.
    @test _post(api_observer_clear, Dict())[1] == 400                          # projectUid missing
    @test _post(api_observer_clear, Dict("projectUid" => "nope"))[1] == 404    # unknown project

    # register (one-click terminal setup) is deliberately NOT called here: on a machine with Claude
    # Code installed it would rewrite the developer's own ~/.claude.json. Its command builders are
    # pure and covered in app/test/runtests.jl (`_build_mcp_register_cmd`/`_build_mcp_remove_cmd`).
end

@testset "API: cohort QC" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _qc(t) = api_qc_cohort(HTTP.Request("GET", "/api/qc/cohort" * t))
    _check(b) = api_qc_cohort_check(Vector{UInt8}(JSON3.write(b)))
    try
        proj = create_project!(name = "api-cohort")
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
        # UID (refs are uid-based; the panel resolves uid→name on demand), with the label set and
        # value-vs-median — not just a bare count
        ll = JSON3.read(api_lablog_read(HTTP.Request("GET", "/api/lablog?projectUid=$(proj.uid)"))[2]).content
        @test occursin("Cohort check", ll) && occursin("clustTracks.cluster (B)", ll)
        @test occursin("$(i1) — nTracks", ll) && occursin("cohort median", ll)   # image UID (refs are uid-based) + detail
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
        proj = create_project!(name = "api-cohort-runs")
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
        proj = create_project!(name = "api-lineage")
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
        proj = create_project!(name = "api-pops")
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
        proj = create_project!(name = "api-measures")
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
        proj = create_project!(name = "api-behclust")
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
        proj = create_project!(name = "api-chains")
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

@testset "API: observer briefing" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    _b(t) = api_observer_briefing(HTTP.Request("GET", "/api/observer/briefing" * t))
    try
        proj = create_project!(name = "api-brief")
        s    = add_set!(proj; name = "set-A")
        img  = add_image!(s; name = "i1", meta = Dict{String,Any}("ori_path" => "/tmp/x.tif"))
        write_qc(img, "importImages.omezarr", "default", Dict{String,Any}[])   # suppress calibration fallback
        write_qc(img, "segment.measureLabels", "default",
                 [qc_finding("fail", "zero_cells", "No cells", "Segmentation produced 0 cells")])
        @test _b("")[1] == 400                                # projectUid missing
        @test _b("?projectUid=nope")[1] == 404
        st, body = _b("?projectUid=$(proj.uid)")
        @test st == 200
        d = JSON3.read(body)
        @test d.projectUid == proj.uid && d.imageCount == 1
        @test d.flagged[1].uid == img.uid && String(d.flagged[1].worst) == "fail"
        @test String(d.flagged[1].findings[1].short) == "No cells"
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

@testset "API: storage" begin
    # summary requires projectUid
    st, body = api_storage_summary(HTTP.Request("GET", "/api/storage/summary"))
    @test st == 400 && haskey(JSON3.read(body), :error)

    # reclaim requires projectUid + a non-empty imageUids list (a stale/empty request is rejected,
    # never allowed to touch disk)
    st, _ = _post(api_storage_reclaim, Dict("projectUid" => ""))
    @test st == 400
    st, _ = _post(api_storage_reclaim, Dict("projectUid" => "p", "imageUids" => String[]))
    @test st == 400
end

@testset "API: fs browser" begin
    tmp = mktempdir()
    mkdir(joinpath(tmp, "sub"))
    write(joinpath(tmp, "img.tif"), "x")
    write(joinpath(tmp, "notes.txt"), "y")

    st, body = api_fs_list(HTTP.Request("GET", "/api/fs/list?path=" * HTTP.URIs.escapeuri(tmp)))
    @test st == 200
    d = JSON3.read(body)
    @test String(d.current) == tmp
    @test String(d.parent)  == dirname(tmp)          # navigates UP out of tmp — NOT clamped to home
    ents = Dict(String(e.name) => e for e in d.entries)
    @test haskey(ents, "sub") && ents["sub"].isdir
    @test ents["img.tif"].isimage
    @test String(ents["img.tif"].path) == joinpath(tmp, "img.tif")   # absolute path
    @test !ents["notes.txt"].isimage
    @test any(s -> String(s.label) == "Home", d.shortcuts)

    # non-existent dir → 400 (not a 500)
    st2, _ = api_fs_list(HTTP.Request("GET", "/api/fs/list?path=" * HTTP.URIs.escapeuri(joinpath(tmp, "nope"))))
    @test st2 == 400
    rm(tmp; recursive=true)
end

@testset "API: movie range parsing + name guard" begin
    # _parse_range → inclusive (start, stop) clamped to the file, or nothing if unsatisfiable.
    @test _parse_range("bytes=0-99", 1000) == (0, 99)
    @test _parse_range("bytes=500-", 1000) == (500, 999)     # open-ended → to EOF
    @test _parse_range("bytes=0-", 1000)   == (0, 999)
    @test _parse_range("bytes=-100", 1000) == (900, 999)     # suffix: last 100 bytes
    @test _parse_range("bytes=990-100000", 1000) == (990, 999)  # end clamped to file
    @test _parse_range("", 1000)           === nothing       # no header
    @test _parse_range("bytes=1000-1100", 1000) === nothing  # start past EOF → unsatisfiable
    @test _parse_range("bytes=50-10", 1000) === nothing      # stop < start
    @test _parse_range("bytes=-0", 1000)   === nothing       # zero-length suffix
    @test _parse_range("bogus", 1000)      === nothing

    # _valid_movie_name accepts the sanitised names the recorders write; blocks traversal/other types.
    @test _valid_movie_name("myImage_animation.mp4")
    @test _valid_movie_name("A1_B2_x0f2Kd.mp4")
    @test !_valid_movie_name("../secret.mp4")
    @test !_valid_movie_name("movie.mp4/../../etc")
    @test !_valid_movie_name("note.txt")
    @test !_valid_movie_name("has space.mp4")
end

# ── Movie registry (settings/movies.json) ─────────────────────────────────────
# The registry DECORATES the movies dir; the directory listing is the truth
# (docs/todo/MOVIE_MANAGEMENT_PLAN.md Decision 1). What is worth pinning is the reconciliation, which
# is the part with a wrong answer available: an entry whose file is gone must disappear rather than
# render a row that plays nothing, and an entry older than its file must be flagged rather than offer
# a config that did not produce those bytes.
@testset "API: movie registry" begin
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had  = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp  = mktempdir(); dirs["projects"] = tmp
    try
        uid = "TESTMOV"
        mdir = joinpath(tmp, uid, "movies"); mkpath(mdir)
        write(joinpath(mdir, "a.mp4"), "a")
        write(joinpath(mdir, "b.mp4"), "b")

        # ── a project with no registry reads exactly as it did before one existed
        ms = movies_with_meta(uid)
        @test Set(m.name for m in ms) == Set(["a.mp4", "b.mp4"])
        @test all(m -> m.displayName == "" && !m.starred && isempty(m.tags) &&
                       m.producedBy == "" && !m.hasConfig && !m.configStale, ms)

        # ── the user-owned fields patch INDEPENDENTLY: setting tags must not clear a star
        @test _post(api_movies_meta_set, Dict("projectUid"=>uid, "name"=>"a.mp4", "starred"=>true))[1] == 200
        @test _post(api_movies_meta_set,
                    Dict("projectUid"=>uid, "name"=>"a.mp4", "tags"=>["figure 2", "figure 2", " "]))[1] == 200
        a = only(filter(m -> m.name == "a.mp4", movies_with_meta(uid)))
        @test a.starred && a.tags == ["figure 2"]        # deduped, blanks dropped

        # a display name never touches the file
        _post(api_movies_meta_set, Dict("projectUid"=>uid, "name"=>"a.mp4", "displayName"=>"  Day 3  CNO "))
        a = only(filter(m -> m.name == "a.mp4", movies_with_meta(uid)))
        @test a.displayName == "Day 3 CNO"               # trimmed, inner whitespace collapsed
        @test isfile(joinpath(mdir, "a.mp4"))

        # ── guards: traversal and a movie that doesn't exist are both "no such movie", and the
        #    offending names come BACK, so a caller can say which of a selection it could not touch
        st, body = _post(api_movies_meta_set, Dict("projectUid"=>uid, "name"=>"../x.mp4"))
        @test st == 404 && JSON3.read(body).rejected == ["../x.mp4"]
        @test _post(api_movies_meta_set, Dict("projectUid"=>uid, "name"=>"nope.mp4"))[1] == 404
        @test _post(api_movies_delete,   Dict("projectUid"=>uid, "name"=>"../a.mp4"))[1] == 404

        # ── BULK: one call, one read-modify-write. A client looping N requests would rewrite the
        #    registry N times, and two in flight at once lose one side's edit.
        write(joinpath(mdir, "c.mp4"), "c")
        # addTags is a set operation — it must not wipe tags a movie already carries ("figure 2" on a)
        st, body = _post(api_movies_meta_set,
                         Dict("projectUid"=>uid, "names"=>["a.mp4", "c.mp4"], "addTags"=>["cohort 1"]))
        @test st == 200
        byname = Dict(m.name => m for m in movies_with_meta(uid))
        @test byname["a.mp4"].tags == ["figure 2", "cohort 1"]
        @test byname["c.mp4"].tags == ["cohort 1"]
        # …and removeTags takes one back out without touching the others
        _post(api_movies_meta_set, Dict("projectUid"=>uid, "names"=>["a.mp4"], "removeTags"=>["figure 2"]))
        @test only(filter(m -> m.name == "a.mp4", movies_with_meta(uid))).tags == ["cohort 1"]
        # a bulk call carries the valid names through and reports the rest
        st, body = _post(api_movies_meta_set,
                         Dict("projectUid"=>uid, "names"=>["c.mp4", "ghost.mp4"], "starred"=>true))
        @test st == 200
        d = JSON3.read(body)
        @test d.names == ["c.mp4"] && d.rejected == ["ghost.mp4"]
        # a display name identifies ONE movie, so it is not applied across a selection
        _post(api_movies_meta_set, Dict("projectUid"=>uid, "names"=>["a.mp4","c.mp4"], "displayName"=>"X"))
        @test all(m -> m.displayName != "X", movies_with_meta(uid))
        # bulk delete removes every named file in one pass
        st, body = _post(api_movies_delete, Dict("projectUid"=>uid, "names"=>["c.mp4", "ghost.mp4"]))
        @test st == 200 && JSON3.read(body).deleted == ["c.mp4"]
        @test !isfile(joinpath(mdir, "c.mp4"))

        # ── config banking + the stale rule
        register_movie!(uid, "b.mp4"; produced_by = "batch",
                        config = Dict("fps" => 15), config_kind = "look")
        b = only(filter(m -> m.name == "b.mp4", movies_with_meta(uid)))
        @test b.producedBy == "batch" && b.hasConfig && b.configKind == "look" && !b.configStale
        # Re-recording replaces the bytes under an entry that stays put, and the config then no longer
        # describes the file. Aged by rewinding the ENTRY rather than the file's mtime — setting an
        # mtime portably is not something Julia offers, and the rule is a comparison either way.
        let reg = _read_movies_registry(uid)
            reg["b.mp4"]["recordedAt"] = time() - 3600
            _write_movies_registry!(uid, reg)
        end
        @test only(filter(m -> m.name == "b.mp4", movies_with_meta(uid))).configStale

        # …and a stamp that is not unix seconds (absent, or an ISO string) cannot be vouched for. The
        # units matter: `string(Dates.now())` is naive LOCAL time, which `datetime2unix` would read as
        # UTC and put hours in the FUTURE — on UTC+10 nothing would ever read as stale.
        for bad in (nothing, "2020-01-01T00:00:00")
            let reg = _read_movies_registry(uid)
                bad === nothing ? delete!(reg["b.mp4"], "recordedAt") : (reg["b.mp4"]["recordedAt"] = bad)
                _write_movies_registry!(uid, reg)
            end
            @test only(filter(m -> m.name == "b.mp4", movies_with_meta(uid))).configStale
        end
        register_movie!(uid, "b.mp4"; produced_by = "batch",
                        config = Dict("fps" => 15), config_kind = "look")   # re-stamp → fresh again
        @test !only(filter(m -> m.name == "b.mp4", movies_with_meta(uid))).configStale

        # a re-record MERGES: the user's name/star/tags outlive the new bytes. Asserted as SURVIVAL
        # against whatever they are now — a literal here would just re-encode the edits made above and
        # break every time one of them changes, which is not what this is pinning.
        before = only(filter(m -> m.name == "a.mp4", movies_with_meta(uid)))
        register_movie!(uid, "a.mp4"; produced_by = "viewer",
                        config = Dict("look" => Dict("channels" => Dict("CD3" => "green"))),
                        config_kind = "look")
        a = only(filter(m -> m.name == "a.mp4", movies_with_meta(uid)))
        @test a.displayName == before.displayName && !isempty(a.displayName)
        @test a.starred == before.starred && a.tags == before.tags && !isempty(a.tags)
        @test a.producedBy == "viewer" && a.configKind == "look"

        # ── which image, and what it shows: the two the Movies page joins against the project's images.
        # The BACK-FILL is the point of the fallbacks — neither field existed when the movies already on
        # disk were recorded, and the page still has to answer for them.
        register_movie!(uid, "a.mp4"; produced_by = "viewer", config_kind = "look",
                        config = Dict("imageUid" => "imgA",
                                      "look" => Dict("channels" => Dict("CD3"  => "green",
                                                                        "B220" => "magenta"))))
        a = only(filter(m -> m.name == "a.mp4", movies_with_meta(uid)))
        # the single recorder has banked the uid inside its config since Phase 4, so a viewer movie
        # answers with no migration; the channels come out of the `look` it read off the live view
        @test a.imageUid == "imgA" && a.channels == ["B220", "CD3"]   # sorted — a JSON object has no order
        # a BATCH banks the authored config one level in, under `config`
        register_movie!(uid, "b.mp4"; produced_by = "batch", config_kind = "look",
                        config = Dict("imageUids" => ["img1", "img2"],
                                      "config" => Dict("channels" => Dict("DAPI" => "blue"))))
        bm = only(filter(m -> m.name == "b.mp4", movies_with_meta(uid)))
        @test bm.channels == ["DAPI"]
        # …but its `imageUids` is the whole SELECTION, not this file's image. Reading it would label
        # every movie in the batch with the same wrong one, so it is deliberately not a fallback — the
        # filename is what identifies a batch movie, and that is resolved client-side.
        @test bm.imageUid == ""
        # Banked explicitly, both win — and the channel ORDER survives, which the config fallback cannot
        # give: the recorder lists them in the image's order, a JSON object has none.
        register_movie!(uid, "b.mp4"; produced_by = "batch", image_uid = "img2",
                        channels = ["CD8", "DAPI"], config_kind = "look",
                        config = Dict("config" => Dict("channels" => Dict("DAPI" => "blue"))))
        bm = only(filter(m -> m.name == "b.mp4", movies_with_meta(uid)))
        @test bm.imageUid == "img2" && bm.channels == ["CD8", "DAPI"]
        # A re-record by a producer that cannot say (an animation shows whatever its keyframes do) leaves
        # the banked answer standing rather than blanking it
        register_movie!(uid, "b.mp4"; produced_by = "batch", config_kind = "look",
                        config = Dict("fps" => 15))
        bm = only(filter(m -> m.name == "b.mp4", movies_with_meta(uid)))
        @test bm.imageUid == "img2" && bm.channels == ["CD8", "DAPI"]

        # the full entry (with the config the list omits) comes from the meta GET
        st, body = api_movies_meta_get(HTTP.Request("GET", "/api/movies/meta?projectUid=$uid&name=a.mp4"))
        @test st == 200
        @test haskey(JSON3.read(body).entry, :config)

        # ── the EDIT side (Phase 6): the config comes back VERBATIM, nesting and all.
        # `frontend/src/utils/movieRestore.ts` reads it field by field, so anything this route flattens,
        # renames or drops is a config that reopens wrong rather than one that fails to open.
        register_movie!(uid, "b.mp4"; produced_by = "animation", config_kind = "keyframes",
                        config = Dict("imageUid" => "img1", "fps" => 20,
                                      "keyframes"    => [Dict("viewState" => Dict("camera" => Dict("zoom" => 2)),
                                                              "steps" => 40)],
                                      "keyframeMeta" => [Dict("assetId" => "a1", "duration" => 2)]))
        st, body = api_movies_meta_get(HTTP.Request("GET", "/api/movies/meta?projectUid=$uid&name=b.mp4"))
        cfg = JSON3.read(body).entry.config
        @test st == 200
        @test cfg.imageUid == "img1" && cfg.fps == 20
        @test cfg.keyframes[1].viewState.camera.zoom == 2 && cfg.keyframes[1].steps == 40
        @test cfg.keyframeMeta[1].assetId == "a1" && cfg.keyframeMeta[1].duration == 2

        # ── delete removes the file AND the entry
        @test _post(api_movies_delete, Dict("projectUid"=>uid, "name"=>"a.mp4"))[1] == 200
        @test !isfile(joinpath(mdir, "a.mp4"))
        @test Set(m.name for m in movies_with_meta(uid)) == Set(["b.mp4"])

        # ── an entry whose file vanished outside the app (a manual rm, a moved folder) is PRUNED by
        #    the listing pass, not rendered as a row that plays nothing
        write(joinpath(mdir, "ghost.mp4"), "g")
        register_movie!(uid, "ghost.mp4"; produced_by = "batch")
        @test haskey(_read_movies_registry(uid), "ghost.mp4")
        rm(joinpath(mdir, "ghost.mp4"))
        @test Set(m.name for m in movies_with_meta(uid)) == Set(["b.mp4"])
        @test !haskey(_read_movies_registry(uid), "ghost.mp4")

        # ── a corrupt registry degrades to "no metadata", never to a broken page
        write(joinpath(tmp, uid, "settings", "movies.json"), "{not json")
        @test Set(m.name for m in movies_with_meta(uid)) == Set(["b.mp4"])
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

# ── The banked movie config is a CONTRACT with the edit page ──────────────────
# `movie_config` is assembled in sockets.jl and read, field by field, by
# `frontend/src/utils/movieRestore.ts`. Nothing type-checks across that boundary and nothing fails when
# a key goes missing — the page just quietly restores less and says so in a note nobody wrote. So the
# keys the edit path cannot work without are pinned here, at the one place that writes them.
#
# Source-level because the writers are two socket HANDLERS, not functions with a return value: they
# assemble the dict and hand it straight to an `@async` recorder that needs a live napari.
@testset "API: movie config banks what the edit page reads" begin
    src = read(joinpath(@__DIR__, "..", "src", "sockets.jl"), String)
    single = src[findfirst("function handle_movie_record", src)[1]:end]
    single = single[1:findfirst("\nend", single)[1]]
    batch  = src[findfirst("function handle_movie_batch", src)[1]:end]
    batch  = batch[1:findfirst("\nend", batch)[1]]

    # WHICH IMAGE. A movie is named after its image, but nothing can turn that name back into a uid, so
    # without this an edited look has no idea what it was recorded on.
    @test occursin("\"imageUid\" => image_uid", single)
    @test occursin("\"imageUids\" => image_uids", batch)
    # The editor's half of a keyframe — thumbnail, title, seconds. `keyframes` alone is the RENDER
    # payload, which restores a timeline with no strip and durations rounded to whole frames.
    @test occursin("\"keyframeMeta\"", single)
    # The look itself, and the kinds it is filed under (MOVIE_MANAGEMENT_PLAN Decision 7).
    @test occursin("\"look\"", single) && occursin("\"keyframes\"", single)
    @test occursin("\"config\" => config", batch) && occursin("\"fileAttrs\"", batch)
    # The output half both kinds share — restoring a look at the wrong size or fps is not restoring it.
    for k in ("\"fps\"", "\"sizeX\"", "\"sizeY\"", "\"suffix\"")
        @test occursin(k, single) && occursin(k, batch)
    end
    # The frame range: banked at the top level for a viewer recording, and inside the authored config
    # for a batch (`buildBatchMovieConfig` always emits the pair). A recreate that silently records the
    # whole timelapse is not a recreate.
    @test occursin("\"tStart\" => t_start", single) && occursin("\"tEnd\" => t_end", single)
    @test occursin("_t_range(data)", single)
end

# Which stretch of the timelapse a movie sweeps. ONE reader for both entry points — the viewer's
# recorder puts the pair on the request, the batch page puts it in its authored config — because they
# mean the same thing and a second parse is where the two would drift.
@testset "API: movie frame range" begin
    # absent = the whole thing, which is what every recording did before the control existed
    @test _t_range(Dict{Symbol,Any}()) == (0, nothing)
    # `nothing` for the end MEANS "the last frame" and must survive as `nothing` — clamping it to a
    # number here would truncate the same config the moment it ran on a longer image, which is exactly
    # what a batch does.
    @test _t_range(Dict(:tStart => 10, :tEnd => nothing)) == (10, nothing)
    @test _t_range(Dict(:tStart => 10, :tEnd => 60)) == (10, 60)
    # a negative start is a bad value, not a request — clamp rather than fail a render
    @test _t_range(Dict(:tStart => -5, :tEnd => 20)) == (0, 20)
    # an inverted range would sweep nothing at all; the end gives way to the start
    @test _t_range(Dict(:tStart => 30, :tEnd => 5)) == (30, 30)
    # the wire carries JSON numbers, which arrive as Float64 for a fractional value
    @test _t_range(Dict(:tStart => 2.0, :tEnd => 7.0)) == (2, 7)
    # and it reads a JSON3 object, not only a Dict — that is what a real request is
    @test _t_range(JSON3.read("""{"tStart":3,"tEnd":9}""")) == (3, 9)
    @test _t_range(JSON3.read("""{"tStart":3,"tEnd":null}""")) == (3, nothing)
end

# ── Napari: branch-labels payload parsing ─────────────────────────────────────
# The napari open + show-labels handlers accept an `allBranchLabels` dict in parallel to `allLabels`
# so skeleton labels from segment.branching are shown as a distinct layer type (`({vn}) Branches`),
# without leaking into the generic labels picker (BRANCHING_PLAN Decision 6). The full round-trip
# needs a live napari process, but the request parsing is pure and worth pinning: missing key →
# empty dict (legacy image / no branching run), well-formed dict → the same shape as _parse_all_labels.
@testset "API: napari branch-labels payload" begin
    # missing → empty (legacy image / no branching run)
    empty_data = JSON3.read(JSON3.write(Dict{String,Any}()))
    @test _parse_all_branch_labels(empty_data) == Dict{String,Vector{String}}()

    # well-formed
    data = JSON3.read(JSON3.write(Dict("allBranchLabels" =>
        Dict("default" => ["default.zarr"], "shg" => ["shg.zarr"]))))
    parsed = _parse_all_branch_labels(data)
    @test parsed["default"] == ["default.zarr"]
    @test parsed["shg"]     == ["shg.zarr"]

    # a scalar (non-array) filename is coerced to a single-element list — same
    # forgiving contract as _parse_all_labels
    scalar_data = JSON3.read(JSON3.write(Dict("allBranchLabels" => Dict("default" => "default.zarr"))))
    @test _parse_all_branch_labels(scalar_data)["default"] == ["default.zarr"]

    # non-dict payload → empty
    bad = JSON3.read(JSON3.write(Dict("allBranchLabels" => "nope")))
    @test _parse_all_branch_labels(bad) == Dict{String,Vector{String}}()
end

# ── Task console: snapshot reconciliation (the stale-"running"-row regression) ──
# `api/task_console.jl` is run by path (`pixi run console`), never imported, so this is the only
# automated coverage it can have: its entrypoint is guarded by `PROGRAM_FILE`, and the reconciliation
# half is split out as the socket-free `_reconcile_snapshot!(rows)` we drive with synthetic snapshots.
# Wrapped in a module because the script defines top-level consts (TASKS, LOCK, TALLY, …).
#
# The bug this pins: the console only ever ADDED rows from GET /api/tasks, and dropped one solely on a
# terminal task:status frame — which is lossy by design (per-client drop-on-full queue in server.jl,
# and nothing at all on a half-open socket). One missed frame stranded the row as "running" forever:
# six tasks listed as running while every pool read idle and the scheduler held none.
module TaskConsoleUT
    include(joinpath(@__DIR__, "..", "task_console.jl"))
end

@testset "API: task console reconciles snapshot removals" begin
    C = TaskConsoleUT
    row(id; status="running", fun="segment.branching", pool="cpu", img="EaMaVq") =
        (; id=id, status=status, fun_name=fun, pool_name=pool, image_uid=img, chain_run_id="")
    reset_console!() = (empty!(C.TASKS); empty!(C.SEEN_TERM); empty!(C.EVENTS); empty!(C.ENDED_IDS);
                        for k in keys(C.TALLY); C.TALLY[k] = 0; end)
    # retiring pushes an activity line, which STREAM_MODE prints — keep it out of the test output
    reconcile(rows) = redirect_stdout(devnull) do; C._reconcile_snapshot!(rows) end

    # a scheduler task appears, then vanishes with NO terminal frame ever delivered
    reset_console!()
    reconcile([row("t1")])
    @test haskey(C.TASKS, "t1") && C.TASKS["t1"].status == "running"
    @test C.TASKS["t1"].in_snapshot                       # eligible for retiring
    reconcile([])                            # miss 1 — not yet (poll/registration race)
    @test haskey(C.TASKS, "t1")
    reconcile([])                            # miss 2 — retire
    @test !haskey(C.TASKS, "t1")                          # ← the row used to live here forever
    @test C.TALLY["ended"] == 1                           # counted, and NOT guessed as done/failed
    @test C.TALLY["done"] == 0 && C.TALLY["failed"] == 0

    # a retired task must not be resurrected by a later snapshot (SEEN_TERM)
    reconcile([row("t1")])
    @test !haskey(C.TASKS, "t1") && C.TALLY["ended"] == 1

    # a WS-only producer (job / batch movie) never appears in the snapshot → never retired by it
    reset_console!()
    t = C._task!("job1"); t.fun_name = "project.export"; t.pool_name = "job"; t.status = "running"
    for _ in 1:5
        reconcile([])
    end
    @test haskey(C.TASKS, "job1") && C.TALLY["ended"] == 0

    # a terminal status seen IN the snapshot is counted for real, not as "ended"
    reset_console!()
    reconcile([row("t2")])
    reconcile([row("t2"; status="failed")])
    @test !haskey(C.TASKS, "t2")
    @test C.TALLY["failed"] == 1 && C.TALLY["ended"] == 0

    # a task still listed keeps its row and its miss counter resets (no drift toward retirement)
    reset_console!()
    reconcile([row("t3")])
    reconcile([])                            # one miss
    reconcile([row("t3")])                   # back in the snapshot → counter cleared
    @test C.TASKS["t3"].misses == 0
    reconcile([])
    @test haskey(C.TASKS, "t3")                           # would have been retired if it hadn't reset

    # an UNATTRIBUTED row (no fun, no pool — only ever log/progress frames) is prunable even though
    # the snapshot never listed it: nothing else could ever remove it, so it sat there forever.
    reset_console!()
    C._task!("ghost")                        # blank fun + pool, default status "queued"
    reconcile([])                            # miss 1
    @test haskey(C.TASKS, "ghost")
    reconcile([])                            # miss 2 → dropped
    @test !haskey(C.TASKS, "ghost")
    @test sum(values(C.TALLY)) == 0                       # no outcome claimed for a task we can't name
    @test !("ghost" in C.SEEN_TERM)                       # …and not suppressed, so a real task returns
end

# ── Task console: post-mortem log frames must not resurrect a finished task ────
# The zombie-queued-row regression. Cancelling a running task broadcasts the terminal `task:status`
# at once (cancel_task! → on_status_change), then the killed subprocess's reader flushes whatever was
# still in its pipe as `task:log` frames. Those carry no fun / pool / status, so each one minted a
# fresh blank row stuck at the default "queued" — and the snapshot could never retire it, because the
# scheduler had already deregistered the task. Six cancels, six immortal "queued / waiting" rows with
# every pool reading idle and GET /api/tasks returning [].
@testset "API: task console ignores post-mortem log frames" begin
    C = TaskConsoleUT
    row(id; status="running") = (; id=id, status=status, fun_name="spatialAnalysis.aggregatesMeshes",
                                  pool_name="cpu", image_uid="EaMaVq", chain_run_id="")
    feed(frame) = redirect_stdout(devnull) do; C.handle_ws(JSON3.write(frame)) end
    recon(rows)  = redirect_stdout(devnull) do; C._reconcile_snapshot!(rows) end
    reset_console!() = (empty!(C.TASKS); empty!(C.SEEN_TERM); empty!(C.EVENTS); empty!(C.LOGS);
                        empty!(C.ENDED_IDS); for k in keys(C.TALLY); C.TALLY[k] = 0; end)

    reset_console!()
    recon([row("k1")])
    feed((; type="task:status", taskId="k1", status="cancelled", imageUid="EaMaVq",
           fun="spatialAnalysis.aggregatesMeshes"))
    @test !haskey(C.TASKS, "k1") && C.TALLY["cancelled"] == 1

    # the dying subprocess's remaining stdout arrives AFTER the terminal frame
    nlogs = length(C.LOGS)
    feed((; type="task:log", taskId="k1", line=">> t74: 13 meshes, 0 aggregate(s)"))
    feed((; type="task:log", taskId="k1", line="[QC] mesh aggregates: 31, 18% of cells."))
    @test !haskey(C.TASKS, "k1")                          # ← used to reappear as a blank "queued" row
    @test length(C.LOGS) == nlogs + 2                     # still SHOWN, just not resurrected as a row
    @test C.TALLY["cancelled"] == 1                        # and not re-counted

    # a progress frame after the fact is likewise ignored (this half was already guarded — pin it)
    feed((; type="task:progress", taskId="k1", progress=0.9))
    @test !haskey(C.TASKS, "k1")

    # the row STAYS while the task is alive — a log frame for a live task is the normal case
    reset_console!()
    recon([row("k2")])
    feed((; type="task:log", taskId="k2", line=">> t01: 4 meshes"))
    @test haskey(C.TASKS, "k2") && C.TASKS["k2"].last_log == ">> t01: 4 meshes"
end

# ── Task console: a chain node's real outcome ─────────────────────────────────
# A chain run emits NO `task:status` frames (`handle_chain_run` passes no `on_status_change`), so a
# chain node's row can only leave the table via the snapshot-retire path — i.e. always
# "ended / outcome unseen", never done or failed. The `taskId` now carried on every `chain:node:*`
# frame is the correlation handle; these pin that the console uses it, and that a frame WITHOUT one
# (skipped before submission, set-scope node, hand-fired REPL event → JSON `null`) is harmless.
@testset "API: task console attributes chain-node outcomes" begin
    C = TaskConsoleUT
    row(id; status="running") = (; id=id, status=status, fun_name="segment.branching",
                                  pool_name="cpu", image_uid="EaMaVq", chain_run_id="run1")
    feed(frame) = redirect_stdout(devnull) do        # STREAM_MODE prints; keep test output clean
        C.handle_ws(JSON3.write(frame))
    end
    reset_console!() = (empty!(C.TASKS); empty!(C.SEEN_TERM); empty!(C.EVENTS); empty!(C.ENDED_IDS);
                        for k in keys(C.TALLY); C.TALLY[k] = 0; end)
    recon(rows) = redirect_stdout(devnull) do; C._reconcile_snapshot!(rows) end

    # node finishes → counted as DONE (not "ended"), row dropped at once
    reset_console!()
    recon([row("c1")])
    feed((; type="chain:node:done", runId="run1", chainName="ch", projectUid="p",
           imageUid="EaMaVq", nodeId="n1", fn="segment.branching", taskId="c1"))
    @test !haskey(C.TASKS, "c1")
    @test C.TALLY["done"] == 1 && C.TALLY["ended"] == 0
    # …and the snapshot's retire path must not then double-count it as ended
    recon([]); recon([])
    @test C.TALLY["ended"] == 0 && C.TALLY["done"] == 1

    # node:failed carries WHICH terminal it was — cancelled must not be counted as failed
    reset_console!()
    recon([row("c2")])
    feed((; type="chain:node:failed", runId="run1", imageUid="EaMaVq", nodeId="n1",
           fn="segment.branching", status="cancelled", taskId="c2"))
    @test C.TALLY["cancelled"] == 1 && C.TALLY["failed"] == 0
    reset_console!()
    recon([row("c3")])
    feed((; type="chain:node:failed", runId="run1", imageUid="EaMaVq", nodeId="n1",
           fn="segment.branching", status="failed", taskId="c3"))
    @test C.TALLY["failed"] == 1

    # taskId absent / JSON null (skipped node, set-scope node, hand-fired event) → no crash, no tally,
    # and the row is left for the snapshot to retire as before
    reset_console!()
    recon([row("c4")])
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f"))
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f",
           taskId=nothing))
    feed((; type="chain:node:failed", runId="run1", imageUid="EaMaVq", nodeId="n2", fn="f",
           status="skipped", taskId=""))
    @test haskey(C.TASKS, "c4")                       # untouched — nothing to correlate
    @test sum(values(C.TALLY)) == 0
    recon([]); recon([])
    @test !haskey(C.TASKS, "c4") && C.TALLY["ended"] == 1   # falls back to the retire path

    # a LATE terminal frame corrects an `ended` tally rather than leaving a number we know is wrong
    # (chain frame delayed past the 2-poll retire window). It must move the count, not add one.
    reset_console!()
    recon([row("c6")]); recon([]); recon([])
    @test C.TALLY["ended"] == 1
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f", taskId="c6"))
    @test C.TALLY["ended"] == 0 && C.TALLY["done"] == 1        # moved, not added
    # …and a further repeat of that frame changes nothing (no double count)
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f", taskId="c6"))
    @test C.TALLY["done"] == 1 && sum(values(C.TALLY)) == 1
    # a real outcome is NOT correctable by a later, different one — first sighting wins
    reset_console!()
    recon([row("c7")])
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f", taskId="c7"))
    feed((; type="chain:node:failed", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f",
           status="failed", taskId="c7"))
    @test C.TALLY["done"] == 1 && C.TALLY["failed"] == 0

    # a terminal chain frame for a task the console never saw still counts + blocks resurrection
    reset_console!()
    feed((; type="chain:node:done", runId="run1", imageUid="EaMaVq", nodeId="n1", fn="f", taskId="c5"))
    @test C.TALLY["done"] == 1
    recon([row("c5")])
    @test !haskey(C.TASKS, "c5") && C.TALLY["done"] == 1
end

# ── Task console: the elapsed clock ───────────────────────────────────────────
# Elapsed is measured client-side — nothing on the wire carries a start timestamp (the scheduler keeps
# none, and a record is deregistered the instant it finishes). So the two things worth pinning are the
# formatter and WHEN the clock restarts: only on a real status change, because the snapshot re-asserts
# the same status every 2s and resetting per poll would peg every row at "0s". Plus the honesty marker:
# a run whose start we didn't witness reads `≥` rather than passing a floor off as a measurement.
@testset "API: task console times each task" begin
    C = TaskConsoleUT
    row(id; status="running") = (; id=id, status=status, fun_name="segment.cellpose",
                                  pool_name="gpu", image_uid="EaMaVq", chain_run_id="")
    feed(frame) = redirect_stdout(devnull) do; C.handle_ws(JSON3.write(frame)) end
    recon(rows)  = redirect_stdout(devnull) do; C._reconcile_snapshot!(rows) end
    reset_console!() = (empty!(C.TASKS); empty!(C.SEEN_TERM); empty!(C.EVENTS); empty!(C.ENDED_IDS);
                        for k in keys(C.TALLY); C.TALLY[k] = 0; end)

    # formatter: seconds → minutes → hours, zero-padded so the column doesn't jitter. Same spelling as
    # the GUI's `formatTaskDuration` — a duration must not read two ways depending on where you look.
    @test C.dur_str(0)          == "0s"
    @test C.dur_str(42_400)     == "42s"
    @test C.dur_str(59_400)     == "59s"          # rounds to the second, no early rollover
    @test C.dur_str(60_000)     == "1m 00s"
    @test C.dur_str(252_000)    == "4m 12s"
    @test C.dur_str(3_600_000)  == "1h 00m"
    @test C.dur_str(5_430_000)  == "1h 30m"
    @test C.dur_str(-1)         == "0s"           # clock skew must not print a negative
    # a start we didn't witness is a FLOOR, and says so
    @test C.dur_str(252_000; exact = false) == "≥4m 12s"

    # a witnessed queued → running transition is exact and restarts the clock
    reset_console!()
    recon([row("e1"; status="queued")])
    waited = C.TASKS["e1"].since
    @test !C.TASKS["e1"].exact                    # the snapshot found it already queued
    recon([row("e1"; status="queued")])           # re-asserted, not changed…
    @test C.TASKS["e1"].since == waited           # …so the queue-wait clock keeps running
    feed((; type="task:status", taskId="e1", status="running", fun="segment.cellpose"))
    @test C.TASKS["e1"].status == "running"
    @test C.TASKS["e1"].exact                     # we saw it start
    @test C.TASKS["e1"].since >= waited           # and the clock restarted on the run
    since_run = C.TASKS["e1"].since
    recon([row("e1")])                            # snapshot agrees it is running — no reset
    @test C.TASKS["e1"].since == since_run && C.TASKS["e1"].exact

    # a task ALREADY running when the console connects: clocked from now, marked as a floor
    reset_console!()
    recon([row("e2")])
    @test C.TASKS["e2"].status == "running" && !C.TASKS["e2"].exact
    @test startswith(C.dur_since(C.TASKS["e2"]), "≥")

    # …and one whose first frame is the live `running` transition is exact even with no queued sighting
    reset_console!()
    feed((; type="task:status", taskId="e3", status="running", fun="segment.cellpose", pool="gpu"))
    @test C.TASKS["e3"].exact && !startswith(C.dur_since(C.TASKS["e3"]), "≥")

    # the outcome line reports the run time — the only place a finished task's elapsed can appear,
    # since the row is collapsed to a count. Read before the row is dropped, so it must be non-empty.
    reset_console!()
    recon([row("e4")])
    ran = C._ran_for("e4")
    @test occursin("in ", ran)
    feed((; type="task:status", taskId="e4", status="done", fun="segment.cellpose"))
    @test !haskey(C.TASKS, "e4") && C.TALLY["done"] == 1
    @test occursin("in ", last(C.EVENTS))         # …and it made it onto the announced line
    @test C._ran_for("e4") == ""                  # gone with the row

    # a task cancelled while still QUEUED never ran — no run time is claimed for it
    reset_console!()
    recon([row("e5"; status="queued")])
    @test C._ran_for("e5") == ""

    # ── the server's own timestamps, which is what makes it a measurement rather than an estimate ──
    iso(dt) = Dates.format(dt, C.TS_FORMAT)
    stamped(id; status="running", started="", queued="") =
        (; id=id, status=status, fun_name="segment.cellpose", pool_name="gpu",
           image_uid="EaMaVq", chain_run_id="", started_at=started, queued_at=queued)

    # a task that has been running for 20 minutes, first seen NOW: the console used to be able to say
    # only "≥0s" here — the whole point of `started_at` on the snapshot
    reset_console!()
    began = Dates.now(UTC) - Dates.Minute(20)
    recon([stamped("s1"; started = iso(began))])
    @test C.TASKS["s1"].exact
    @test C.TASKS["s1"].since == began
    @test C.dur_since(C.TASKS["s1"]) == "20m 00s"           # not "≥0s"

    # …re-asserted every poll without drifting or resetting
    recon([stamped("s1"; started = iso(began))])
    @test C.TASKS["s1"].since == began && C.TASKS["s1"].exact

    # a row the console had been timing ITSELF is upgraded the first time the rail supplies a real start
    reset_console!()
    recon([row("s2")])                                       # no timestamps (older server)
    @test !C.TASKS["s2"].exact
    recon([stamped("s2"; started = iso(began))])
    @test C.TASKS["s2"].exact && C.TASKS["s2"].since == began

    # queued rows are timed from `queued_at`, so the wait is real too
    reset_console!()
    enq = Dates.now(UTC) - Dates.Second(90)
    recon([stamped("s3"; status="queued", queued = iso(enq))])
    @test C.TASKS["s3"].exact && C.dur_since(C.TASKS["s3"]) == "1m 30s"

    # a garbage or empty timestamp must not take the reader down — it just means "not known"
    reset_console!()
    recon([stamped("s4"; started = "not a date")])
    @test haskey(C.TASKS, "s4") && !C.TASKS["s4"].exact      # fell back to the local clock

    # a live terminal frame carries both ends → the announced duration is exact
    reset_console!()
    recon([stamped("s5"; started = iso(began))])
    feed((; type="task:status", taskId="s5", status="done", fun="segment.cellpose",
           startedAt=iso(began), finishedAt=iso(began + Dates.Minute(25))))
    @test occursin("in 25m 00s", last(C.EVENTS))
    @test C.TALLY["done"] == 1

    # …and so is a RECOVERED one, for a task this console never even held a row for. Timing it locally
    # would have measured the poll delay, not the task.
    reset_console!()
    redirect_stdout(devnull) do
        C._apply_recent!([(; id="s6", status="done", image_uid="EaMaVq", image_uids=String[],
                            started_at=iso(began), finished_at=iso(began + Dates.Minute(3)))])
    end
    @test !haskey(C.TASKS, "s6") && C.TALLY["done"] == 1
    @test occursin("in 3m 00s", last(C.EVENTS))

    # an outcome row with no start (older server / never ran) still counts, just without a duration
    reset_console!()
    redirect_stdout(devnull) do
        C._apply_recent!([(; id="s7", status="failed", image_uid="", image_uids=String[],
                            started_at="", finished_at=iso(Dates.now(UTC)))])
    end
    @test C.TALLY["failed"] == 1 && !occursin("in ", last(C.EVENTS))
end

# ── Task console: the done counter must not depend on the WS stream ───────────
# The reported bug: nine images ran and finished, and the console read "0 done · 17 ended". The
# terminal `task:status` frame is the ONE frame per task that carries the outcome, and the server
# drops frames for a slow client by design (per-client drop-on-full queue in server.jl) — so every
# lost or late frame became a permanent "finished, outcome unseen".
#
# Two independent halves, one per failure mode:
#   1. a LATE frame (it did arrive, after the snapshot had already retired the row) was DISCARDED —
#      `handle_ws` returned on `id in SEEN_TERM` before reaching the ended→outcome correction, which
#      made that correction unreachable from the task:status path (only chain frames could use it).
#   2. a LOST frame can now be recovered at all: the outcome is polled from GET /api/tasks/recent.
@testset "API: task console counts outcomes without the WS frame" begin
    C = TaskConsoleUT
    row(id; status="running") = (; id=id, status=status, fun_name="cleanupImages.driftCorrect",
                                  pool_name="io", image_uid="EaMaVq", chain_run_id="")
    rec(id, status, ts) = (; id=id, status=status, finished_at=ts,
                             fun_name="cleanupImages.driftCorrect", pool_name="io",
                             image_uid="EaMaVq", image_uids=String[])
    feed(frame)  = redirect_stdout(devnull) do; C.handle_ws(JSON3.write(frame)) end
    recon(rows)  = redirect_stdout(devnull) do; C._reconcile_snapshot!(rows) end
    recent(rows; prime=false) = redirect_stdout(devnull) do
        C._apply_recent!(JSON3.read(JSON3.write(rows)); prime = prime)
    end
    reset_console!() = (empty!(C.TASKS); empty!(C.SEEN_TERM); empty!(C.EVENTS); empty!(C.LOGS);
                        empty!(C.ENDED_IDS); C.RECENT_SINCE[] = "";
                        for k in keys(C.TALLY); C.TALLY[k] = 0; end)

    # ── 1. a late terminal task:status frame corrects the "ended" it was retired as ──
    reset_console!()
    recon([row("t1")]); recon([]); recon([])
    @test C.TALLY["ended"] == 1 && C.TALLY["done"] == 0
    feed((; type="task:status", taskId="t1", status="done", imageUid="EaMaVq",
           fun="cleanupImages.driftCorrect"))
    @test C.TALLY["done"] == 1 && C.TALLY["ended"] == 0     # ← was silently discarded
    @test sum(values(C.TALLY)) == 1                          # moved, not added
    feed((; type="task:status", taskId="t1", status="done", imageUid="EaMaVq"))
    @test C.TALLY["done"] == 1                               # repeat frame changes nothing
    # a real outcome still can't be overwritten by a later, different one
    feed((; type="task:status", taskId="t1", status="failed", imageUid="EaMaVq"))
    @test C.TALLY["done"] == 1 && C.TALLY["failed"] == 0

    # ── 2. the frame never arrives at all → the outcome poll supplies it ──
    reset_console!()
    recon([row("t2")])
    recent([rec("t2", "done", "2026-07-31T04:50:20.100Z")])
    @test !haskey(C.TASKS, "t2") && C.TALLY["done"] == 1     # counted with no WS frame at all
    recon([]); recon([])
    @test C.TALLY["ended"] == 0 && C.TALLY["done"] == 1      # …and not re-retired as unseen

    # …and it corrects a row already retired as "ended" (poll landed after the retire)
    reset_console!()
    recon([row("t3")]); recon([]); recon([])
    @test C.TALLY["ended"] == 1
    recent([rec("t3", "failed", "2026-07-31T04:51:00.000Z")])
    @test C.TALLY["failed"] == 1 && C.TALLY["ended"] == 0

    # a whole batch: every task finishes, not one terminal frame gets through → all counted
    reset_console!()
    ids = ["b$i" for i in 1:9]
    recon([row(i) for i in ids])
    @test length(C.TASKS) == 9
    recent([rec(i, "done", "2026-07-31T04:5$(n):00.000Z") for (n, i) in enumerate(ids)])
    @test C.TALLY["done"] == 9 && C.TALLY["ended"] == 0 && isempty(C.TASKS)

    # ── `since` bookkeeping: newest wins, and a re-served row is not double-counted ──
    reset_console!()
    recent([rec("s1", "done", "2026-07-31T04:00:00.000Z"),
            rec("s2", "done", "2026-07-31T05:00:00.000Z")])
    @test C.RECENT_SINCE[] == "2026-07-31T05:00:00.000Z"
    recent([rec("s2", "done", "2026-07-31T05:00:00.000Z")])   # inclusive bound re-serves it
    @test C.TALLY["done"] == 2

    # ── the prime pass: outcomes that predate this console session are NOT counted ──
    # (the ring holds up to 500; crediting the session with work it never watched would be a lie)
    reset_console!()
    recent([rec("old1", "done", "2026-07-30T01:00:00.000Z"),
            rec("old2", "failed", "2026-07-30T02:00:00.000Z")]; prime = true)
    @test sum(values(C.TALLY)) == 0
    @test C.RECENT_SINCE[] == "2026-07-30T02:00:00.000Z"      # …but we resume from after them
    recent([rec("old2", "failed", "2026-07-30T02:00:00.000Z")])
    @test sum(values(C.TALLY)) == 0                           # primed ids stay uncounted
    recent([rec("new1", "done", "2026-07-30T03:00:00.000Z")])
    @test C.TALLY["done"] == 1                                # anything after it counts normally

    # a task still IN FLIGHT is never touched by the poll (nothing to report yet)
    reset_console!()
    recon([row("live")])
    recent(Any[])
    @test haskey(C.TASKS, "live") && C.TASKS["live"].status == "running"
end

# The route the poll above reads. `since` must reach `recent_tasks` (an unparsed one would re-serve
# the whole ring every 2s), and a missing param must mean "everything", not an error.
@testset "API: /api/tasks/recent" begin
    get_recent(q = "") = JSON3.read(api_tasks_recent(HTTP.Request("GET", "/api/tasks/recent$q"))[2])
    @test api_tasks_recent(HTTP.Request("GET", "/api/tasks/recent"))[1] == 200
    @test get_recent() isa JSON3.Array                       # no `since` → the whole ring
    @test isempty(get_recent("?since=9999-01-01T00:00:00.000Z"))
    @test get_recent("?since=") == get_recent()              # blank is "everything", not a filter
end

# ── Every terminal frame is banked, whoever emitted it ────────────────────────
# `ws_status` is the rail's ONE status sink, so banking the outcome there (rather than in the scheduler,
# where it started) is what makes recovery universal: background jobs (project export/import, data
# patches — `pool="job"`) and batch movies (`pool="viewer"`) never enter the scheduler's registry at all,
# so a dropped `done` frame used to strand their row with nothing able to correct it. These pin that the
# bank is fed by the sink and not by the producer.
@testset "API: ws_status banks every producer's outcome" begin
    empty!(Cecelia._OUTCOMES)
    banked(id) = filter(r -> r.id == id, recent_tasks())

    # a background job — the case that was previously uncoverable
    ws_status(nothing, "job-1", "done", "EaMaVq"; fun="project:export", pool="job")
    @test only(banked("job-1")).status    == "done"
    @test only(banked("job-1")).pool_name == "job"
    @test only(banked("job-1")).fun_name  == "project:export"

    # a batch movie (napari/viewer producer)
    ws_status(nothing, "movie-1", "failed", "EaMaVq"; fun="movie:batch", pool="viewer")
    @test only(banked("movie-1")).status == "failed"

    # a scheduler task, incl. a set-scope run's full member list (only ever present on this frame)
    ws_status(nothing, "task-9", "done", "a"; image_uids=["a", "b"], fun="behaviour.hmm")
    @test only(banked("task-9")).image_uids == ["a", "b"]

    # in-flight statuses are not outcomes — the sink hands over every frame, terminal or not
    ws_status(nothing, "task-live", "queued", "EaMaVq"; fun="segment.cellpose")
    ws_status(nothing, "task-live", "running", "EaMaVq"; fun="segment.cellpose")
    @test isempty(banked("task-live"))
    ws_status(nothing, "task-live", "cancelled", "EaMaVq"; fun="segment.cellpose")
    @test only(banked("task-live")).status == "cancelled"     # …until it ends

    # the frame still goes out to clients — banking must not replace broadcasting
    cap = Channel{String}(8)
    key = gensym("test-outcome")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    try
        ws_status(nothing, "task-bc", "done", "EaMaVq"; fun="segment.cellpose")
        @test isready(cap)
        let f = JSON3.read(take!(cap))
            @test f.type == "task:status" && f.taskId == "task-bc" && f.status == "done"
        end
        @test only(banked("task-bc")).status == "done"
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
    end
    empty!(Cecelia._OUTCOMES)
end

# ── Chain event → WS bridge: taskId degradation ───────────────────────────────
# The bridge reads `task_id` through `_ev_task_id`, not `p.task_id`, because two real payloads lack a
# usable one: a node with no task id yet (skipped before submission, set-scope/incremental nodes that
# bypass `run_task`) carries `nothing`, and a hand-fired REPL/test event may omit the field entirely.
# Either must degrade to "" — a bridge handler that throws would take down chain telemetry for every
# connected client.
@testset "API: chain bridge taskId degradation" begin
    @test _ev_task_id((; task_id = "abc123")) == "abc123"
    @test _ev_task_id((; task_id = nothing))  == ""       # node had no task id yet
    @test _ev_task_id((; run_id  = "r1"))     == ""       # field absent (hand-fired event)
    @test _ev_task_id(NamedTuple())            == ""
    @test _ev_task_id((; task_id = "x")) isa String
end

# ── Chain event → WS bridge: the frames that actually go out ───────────────────
# The four `subscribe_chain_events!` handlers were only covered through `_ev_task_id`, so a mistyped
# key ("taskID", "task_id") would have passed every other test. No socket needed: this harness already
# include-d server.jl, so the subscriptions are live and `broadcast_ws` writes a pre-serialised frame
# into each client's queue — register a Channel as a fake client and read the frame back.
@testset "API: chain bridge frames" begin
    q = Channel{String}(32)
    lock(_ws_clients_lock) do; _ws_clients[:probe] = q; end
    try
        base = (; run_id="r1", chain_name="ch", project_uid="p", image_uid="EaMaVq",
                 node_id="n1", fn="segment.branching", params=Dict{String,Any}("a"=>1),
                 task_id="tid123")
        fire(t, p) = (Cecelia._fire_chain_event!(t, p); JSON3.read(take!(q)))

        for (ev, wire) in (("node:queued", "chain:node:queued"), ("node:running", "chain:node:running"))
            f = fire(ev, base)
            @test String(f.type)       == wire
            @test String(f.taskId)     == "tid123"          # ← the correlation handle, right key name
            @test String(f.runId)      == "r1"
            @test String(f.chainName)  == "ch"
            @test String(f.projectUid) == "p"
            @test String(f.imageUid)   == "EaMaVq"
            @test String(f.nodeId)     == "n1"
            @test String(f.fn)         == "segment.branching"
            @test haskey(f, :params)
        end

        f = fire("node:done", (; base..., result=Dict{String,Any}("valueName"=>"B")))
        @test String(f.type) == "chain:node:done" && String(f.taskId) == "tid123"
        @test String(f.result.valueName) == "B"

        f = fire("node:failed", (; base..., status="cancelled"))
        @test String(f.type) == "chain:node:failed" && String(f.taskId) == "tid123"
        @test String(f.status) == "cancelled"               # console needs WHICH terminal it was

        # a node with no task id yet (skipped/set-scope) must broadcast "" — not null, not a throw
        f = fire("node:failed", (; run_id="r1", chain_name="ch", project_uid="p", image_uid="EaMaVq",
                                  node_id="n2", fn="f", status="skipped", task_id=nothing))
        @test String(f.taskId) == ""
        # …and a hand-fired REPL event that omits the field entirely must not take the bridge down
        f = fire("node:queued", (; run_id="r1", chain_name="ch", project_uid="p", image_uid="EaMaVq",
                                  node_id="n3", fn="f", params=Dict{String,Any}()))
        @test String(f.taskId) == ""

        # ── the bridge is the SECOND carrier of a terminal outcome, and banks it too ──
        # A chain run emits no `task:status` at all, so `ws_status` never sees a chain node: banking only
        # there left every chain node unrecoverable (a dropped `chain:node:done` = a row stuck at running
        # with nothing able to correct it, and the console back to "outcome unseen"). Keyed by the node's
        # scheduler task id — what a consumer correlates a chain row against.
        empty!(Cecelia._OUTCOMES)
        banked(id) = filter(r -> r.id == id, recent_tasks())
        fire("node:queued", base); fire("node:running", base)
        @test isempty(banked("tid123"))                       # in-flight is not an outcome
        fire("node:done", (; base..., result=nothing))
        @test only(banked("tid123")).status   == "done"
        @test only(banked("tid123")).fun_name == "segment.branching"
        @test only(banked("tid123")).image_uid == "EaMaVq"

        fire("node:failed", (; base..., task_id="tid456", status="cancelled"))
        @test only(banked("tid456")).status == "cancelled"     # not flattened to "failed"

        # a SKIPPED node never ran: no task id, and "skipped" is not a terminal task status
        fire("node:failed", (; base..., task_id=nothing, node_id="n9", status="skipped"))
        @test isempty(filter(r -> r.status == "skipped", recent_tasks()))
        @test isempty(banked(""))
        empty!(Cecelia._OUTCOMES)
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, :probe); end
        close(q)
    end
end

# ── END-TO-END: a real producer reaches the API layer's sinks ─────────────────
# THE SEAM BOTH SUITES USED TO STUB, from both directions. `app/test` runs a real `run_chain` and asserts
# the fired events carry a real `task_id` (producer → event bus) but has no API layer attached; the
# testsets above exercise the real bridge, `ws_status` and console with HAND-BUILT payloads (sink side)
# but nothing ever ran. So every claim of the form "when a real task finishes, the API layer does Y" was
# unasserted — and that is exactly where a regression hid: moving the outcome bank to `ws_status` silently
# un-banked every chain node, because a chain run passes no `on_status_change` and so never reaches
# `ws_status` at all. Both suites stayed green. These tests run the REAL producers with nothing mocked.
#
# Cheap because `server.jl` gates only `start()` on `CECELIA_NO_SERVE` — the `subscribe_chain_events!`
# handlers ran at include time, so the bridge is live in this process.
@testset "API: real producers reach the WS sinks" begin
    # capture client: broadcast_ws enqueues a serialised frame per registered client (as above)
    cap = Channel{String}(512)
    key = gensym("test-e2e")
    lock(_ws_clients_lock) do; _ws_clients[key] = cap; end
    frames() = (fs = []; while isready(cap); push!(fs, JSON3.read(take!(cap))); end; fs)
    banked(id) = filter(r -> r.id == id, recent_tasks())
    empty!(Cecelia._OUTCOMES)

    try
        proj = create_project!(name="api-e2e")
        s    = add_set!(proj; name="set-A")
        imgs = [add_image!(s; name="img-$i") for i in 1:2]
        for img in imgs; img.status = "done"; save!(img); end

        # ── 1. chain node: producer → event bus → bridge → bank + broadcast ──
        # The regression this pins. A chain node's outcome travels ONLY as chain:node:done, so the bank
        # has to be fed from the bridge; keyed by the node's real scheduler task id, which is what a
        # client correlates its (synthetically-keyed) chain row against.
        save_chain_template!(proj, ChainTemplate(
            "e2e-chain",
            [ChainNode(id="n1", fn="testTasks.image_task",
                       params=Dict{String,Any}("message"=>"e2e"))],
            ChainEdge[]))
        frames()
        run = run_chain(proj, [i.uid for i in imgs]; chain="e2e-chain", on_log=_->nothing)

        node_ids = [run.image_states[i.uid]["n1"].task_id for i in imgs]
        @test all(!isnothing, node_ids) && all(!isempty, node_ids)
        for tid in node_ids
            @test only(banked(tid)).status   == "done"      # ← was empty: nothing fed the bank
            @test only(banked(tid)).fun_name == "testTasks.image_task"
        end
        let fs = frames()
            @test count(f -> String(f.type) == "chain:node:done", fs) == 2   # …and still broadcast
            # every terminal frame the client saw is replayable from the log — the whole invariant
            for f in fs
                String(f.type) == "chain:node:done" || continue
                @test !isempty(banked(String(f.taskId)))
            end
        end
        # a chain node emits NO task:status (handle_chain_run passes no on_status_change) — the premise
        # the bridge-side bank exists for. If this ever fails, the second bank may be redundant.
        @test !any(f -> String(f.type) == "task:status", frames())

        # ── 2. module task: handle_task_run → run_task → ws_status → bank ──
        empty!(Cecelia._OUTCOMES); frames()
        tid = "e2e-image-task"
        handle_task_run(nothing, JSON3.read(JSON3.write((;
            taskId=tid, funName="testTasks.image_task", params=Dict("message"=>"e2e"),
            imageUid=imgs[1].uid, projectUid=proj.uid, setUid=s.uid, poolName=""))))
        @test timedwait(() -> !isempty(banked(tid)), 30.0) === :ok
        @test only(banked(tid)).status    == "done"
        @test only(banked(tid)).image_uid == imgs[1].uid
        let fs = filter(f -> String(f.type) == "task:status", frames())
            @test any(f -> String(f.status) == "done", fs)          # the live frame went out too
            @test String(last(fs).taskId) == tid
        end

        # ── 3. set-scope task: the real frame's image_uids, unfabricated ──
        # Every other test hands `image_uids` in by hand, so nothing checked that a real set run actually
        # reports all its members — and a replayed frame missing them refreshes the representative
        # image's plots only, leaving every other member's stale.
        empty!(Cecelia._OUTCOMES); frames()
        stid = "e2e-set-task"
        handle_task_run(nothing, JSON3.read(JSON3.write((;
            taskId=stid, funName="testTasks.set_task", params=Dict("message"=>"e2e-set"),
            imageUid="", imageUids=[i.uid for i in imgs], projectUid=proj.uid,
            setUid=s.uid, poolName=""))))
        @test timedwait(() -> !isempty(banked(stid)), 30.0) === :ok
        @test only(banked(stid)).status == "done"
        @test Set(only(banked(stid)).image_uids) == Set(i.uid for i in imgs)   # ← all members, banked
        @test only(banked(stid)).image_uid == first(imgs).uid                   # …plus the representative
        let f = last(filter(x -> String(x.type) == "task:status", frames()))
            @test Set(String.(f.imageUids)) == Set(i.uid for i in imgs)         # …and on the live frame
        end

        rm(proj.root; recursive=true)
    finally
        lock(_ws_clients_lock) do; delete!(_ws_clients, key); end
        close(cap)
        empty!(Cecelia._OUTCOMES)
    end
end

# api/src runs in `Main` with `using Cecelia`, so it can only call Cecelia functions UNQUALIFIED if
# they are EXPORTED. Miss an export and the code loads fine, the route registers fine, and the call
# dies at runtime with `UndefVarError: f not defined in Main` — which is exactly how the live-preview
# `refresh_labels!` shipped broken: defined next to `show_labels!`, but not added to the export list
# beside it. Nothing about that is specific to napari, so this checks the whole class rather than a
# hand-kept list: every function Cecelia OWNS and api/src calls unqualified must be exported.
@testset "API: Cecelia functions called unqualified from api/src are exported" begin
    src_dir = joinpath(@__DIR__, "..", "src")
    # comments stripped so a mention in prose can't trip the scan (crude, but `#` inside a string
    # would only ever cause a FALSE ALARM here, never a miss)
    sources = join([replace(read(joinpath(src_dir, f), String), r"#[^\n]*" => "")
                    for f in readdir(src_dir) if endswith(f, ".jl")], "\n")

    missing_exports = Symbol[]
    for sym in names(Cecelia; all = true)
        s = string(sym)
        (occursin("#", s) || startswith(s, "@")) && continue
        isdefined(Cecelia, sym) || continue
        f = getfield(Cecelia, sym)
        f isa Function || continue
        # only names Cecelia itself defines — `parentmodule` keeps Base/stdlib re-exports (length,
        # get, …) out, which would otherwise all look unexported
        parentmodule(f) === Cecelia || continue
        Base.isexported(Cecelia, sym) && continue
        # called unqualified: not preceded by a dot (`Cecelia.f(`, `obj.f(`) or another word char
        occursin(Regex("(?<![.\\w])" * escape_string(s) * "\\s*\\("), sources) || continue
        # …and not a same-named function the api layer defines for itself
        occursin(Regex("function\\s+" * escape_string(s) * "\\s*\\("), sources) && continue
        push!(missing_exports, sym)
    end

    @test isempty(missing_exports)
    isempty(missing_exports) ||
        @info "not exported from Cecelia but called unqualified in api/src" missing_exports
end

# ── Notebook sysimage: which recipe built the image ───────────────────────────────
# Deliberately at the END of this file rather than beside the `API: notebooks sysimage status`
# testset it belongs with: #437 inserts ~104 lines at line 486, exactly there, and a flat testset
# suite does not care about order. Move it back up once that has landed.
@testset "sysimage stamp records which recipe built the image" begin
    # Both builders write the SAME pluto/deps.so by design, but the deps-only one EXCLUDES Cecelia so
    # Revise can hot-reload it while -full bakes Cecelia in. Without a variant they are identical on
    # disk, so a -full build on a dev machine silently froze Cecelia in notebook workers while
    # launch.jl still logged "deps sysimage". These are the pure halves of that fix.
    # No include needed: notebooks_api.jl (loaded via server.jl above) now includes the shared
    # pluto/sysimage_stamp.jl itself, so these functions are already in scope. That IS the fix —
    # if this testset ever needs its own include again, the duplication has come back.

    d = mktempdir()
    write(joinpath(d, "Manifest.toml"), "dummy")
    touch(joinpath(d, "deps.so"))

    write_sysimage_stamp(d, "deps")
    @test sysimage_variant(d) == "deps"
    @test sysimage_fresh(d)
    write_sysimage_stamp(d, "full")
    @test sysimage_variant(d) == "full"
    @test sysimage_fresh(d)                        # variant must not affect staleness
    write_sysimage_stamp(d)                        # default is the safe one
    @test sysimage_variant(d) == "deps"
    @test_throws ArgumentError write_sysimage_stamp(d, "bogus")

    # An image stamped BEFORE `variant` existed must keep working — reported honestly as unknown
    # rather than mislabelled "deps", and crucially still FRESH (no forced ~10 min rebuild).
    write(joinpath(d, "deps.so.stamp"),
          "{\"julia\":\"$(VERSION)\",\"manifest\":\"$(string(hash("dummy")))\"}")
    @test sysimage_variant(d) == "unknown"
    @test sysimage_fresh(d)

    rm(joinpath(d, "deps.so.stamp"))
    @test sysimage_variant(d) == "unknown"
    @test !sysimage_fresh(d)

    # Writer and readers are now ONE implementation (pluto/sysimage_stamp.jl, included by the API
    # server rather than copied). These pin the round trip end to end: what the builder writes is
    # what the classifier and the rebuild path read back.
    full = "{\"julia\":\"1.11\",\"manifest\":\"abc\",\"variant\":\"full\"}"
    @test _classify_sysimage(true, full, false, false, "1.11", "abc") == "ready"
    @test _classify_sysimage(true, full, false, false, "1.10", "abc") == "stale"

    # The API reads the variant to rebuild LIKE FOR LIKE. Getting "unknown" wrong in the unreadable /
    # pre-variant / absent cases is what would silently downgrade a release's full image to deps.
    @test stamp_variant(full) == "full"
    @test stamp_variant("{\"julia\":\"1.11\",\"manifest\":\"abc\",\"variant\":\"deps\"}") == "deps"
    @test stamp_variant("{\"julia\":\"1.11\",\"manifest\":\"abc\"}") == "unknown"   # pre-variant stamp
    @test stamp_variant("{\"variant\":\"bogus\"}") == "unknown"                     # unrecognised
    @test stamp_variant("not json at all")         == "unknown"
    @test stamp_variant(nothing)                   == "unknown"                     # absent → first run

    # Round trip through the real writer: what a build stamps is what the readers report.
    for v in ("deps", "full")
        d2 = mktempdir(); write(joinpath(d2, "Manifest.toml"), "x"); touch(_sysimage_file(d2))
        write_sysimage_stamp(d2, v)
        @test stamp_variant(read_sysimage_stamp(d2)) == sysimage_variant(d2) == v
    end
end

@testset "the sysimage stamp format has exactly one implementation" begin
    # REPLACES a "both readers agree" assertion that went vacuous the moment they became the same
    # function. The live risk is no longer drift between two copies — it is someone re-deriving the
    # format a third time, which is exactly how the copy this consolidation deleted came to exist
    # (with a comment noting the two were "kept trivially in sync"). So detect that instead.
    canonical = normpath(joinpath(@__DIR__, "..", "..", "pluto", "sysimage_stamp.jl"))
    @test isfile(canonical)

    roots = [normpath(joinpath(@__DIR__, "..", "src")),
             normpath(joinpath(@__DIR__, "..", "..", "app", "src")),
             normpath(joinpath(@__DIR__, "..", "..", "pluto"))]

    # Knowledge belonging to the canonical file alone: the artefact filenames, the stamp's JSON field
    # spellings, and the Manifest fingerprint. Anything else deriving these is a second source of
    # truth, whether or not it happens to agree today.
    banned = [("image/stamp filename", r"\"deps\.so"),
              ("stamp field literal",  r"\\\"(julia|manifest|variant)\\\""),
              ("manifest fingerprint", r"hash\(read\(.*Manifest\.toml")]

    offenders, scanned = String[], 0
    for root in roots, (dir, _, files) in walkdir(root), f in files
        endswith(f, ".jl") || continue
        path = joinpath(dir, f)
        normpath(path) == canonical && continue
        scanned += 1
        for (i, line) in enumerate(eachline(path))
            startswith(strip(line), "#") && continue        # prose may name them freely
            for (what, re) in banned
                occursin(re, line) && push!(offenders, "$(basename(path))#$i — $what: $(strip(line))")
            end
        end
    end

    @test isempty(offenders)
    isempty(offenders) || @info "sysimage stamp format re-derived outside pluto/sysimage_stamp.jl — use its helpers (_sysimage_file / _sysimage_stamp / _manifest_fingerprint / stamp_matches / stamp_variant)" offenders

    # Anti-vacuity: a walk over nothing reports a clean bill of health, so pin that we really looked.
    @test scanned > 100
end

# ── HTTP router: the full route table dispatches ─────────────────────────────────
# SAFETY NET for turning the router from a 156-branch if/elseif chain into lookup tables. That chain
# was ONE method costing 42s of a 53s server boot (--trace-compile-timing); a table compiles only the
# handler you actually hit. Nothing tested ROUTING before this — the suite calls handlers directly —
# so the refactor could have dropped a route into a 404 that only shows up in the browser.
#
#  * DISPATCH — every (method, path) must reach a handler. `handle_http` is a plain function, so no
#    socket is needed. A router miss is 404 with body "Not found: <path>"; a handler's own 404 says
#    something else, so the two are distinguishable. Reaching a handler and throwing still counts —
#    the point is that routing happened.
#  * INVENTORY — the `/api/...` literals in server.jl must equal this table exactly, so adding or
#    removing a route without updating it fails here rather than in production.
@testset "HTTP router — the full route table still dispatches" begin
    GET_ROUTES = [
        "/api/analysis/behaviour", "/api/analysis/boards",
        "/api/analysis/chains",
        "/api/analysis/clusters", "/api/analysis/lineage",
        "/api/analysis/measures", "/api/analysis/populations",
        "/api/analysis/spatial", "/api/app/worktrees",
        "/api/chains", "/api/chains/get",
        "/api/chains/run", "/api/chains/runs",
        "/api/crop/frame", "/api/crop/info",
        "/api/diagnostics", "/api/diagnostics/packages",
        "/api/fs/list", "/api/gating/channels",
        "/api/gating/density", "/api/gating/membership",
        "/api/gating/plotdata", "/api/gating/plotmeta",
        "/api/gating/popmap", "/api/gating/stats",
        "/api/health", "/api/images",
        "/api/images/geometry", "/api/images/meta",
        "/api/images/stores",
        "/api/images/tasklog", "/api/lablog",
        "/api/logs/recent", "/api/maintenance/patches",
        "/api/mcp/connections",
        "/api/movies", "/api/movies/meta",
        "/api/napari/gpu",
        "/api/napari/status", "/api/notebooks",
        "/api/notebooks/content", "/api/notebooks/snapshots",
        "/api/notebooks/status", "/api/observer/briefing",
        "/api/observer/labarchives",
        "/api/optical-flow/models",
        "/api/observer/status", "/api/plots/attrs",
        "/api/plots/definitions", "/api/plots/populations",
        "/api/plots/umap", "/api/pools",
        "/api/preview/status", "/api/projects",
        "/api/projects/boards",   # GET; the POST at the same path is the autosave, listed below
        "/api/projects/bundle-info", "/api/projects/bundles",
        "/api/qc/cohort", "/api/qc/cohort/runs",
        "/api/repl/api", "/api/setup/defaults",
        "/api/setup/validate", "/api/storage/compressor", "/api/storage/layout",
        "/api/storage/summary",
        "/api/tasks", "/api/tasks/custom-modules",
        "/api/tasks/definitions", "/api/tasks/funparams",
        "/api/tasks/history", "/api/tasks/recent",
        "/api/tracking/motion-dims", "/api/update/check",
        "/api/version",
    ]
    POST_ROUTES = [
        "/api/app/restart", "/api/app/shutdown",
        "/api/app/switch-worktree", "/api/board-assets/copy",
        "/api/board-assets/delete", "/api/board-assets/save",
        "/api/boards/add",   # create-only board authoring (MCP write 6/6); NOT /api/projects/boards
        "/api/chains/create", "/api/chains/delete",
        "/api/chains/rename", "/api/chains/save",
        "/api/gating/copy", "/api/gating/pop/add",
        "/api/gating/pop/delete", "/api/gating/pop/rename",
        "/api/gating/pop/set-gate", "/api/gating/pop/update",
        "/api/images/attr/create", "/api/images/attr/delete",
        "/api/images/analysis/reset", "/api/images/attr/set",
        "/api/images/channelnames",
        "/api/images/delete", "/api/images/inclusion/set",
        "/api/images/labels/delete", "/api/images/meta/resync",
        "/api/images/meta/set", "/api/images/move",
        "/api/images/register", "/api/images/value-name-check",
        "/api/images/version/remove",
        "/api/import/register-legacy", "/api/import/scan-legacy",
        "/api/lablog/append", "/api/lablog/capture",
        "/api/lablog/dismiss", "/api/napari/apply-movie-config",
        "/api/napari/apply-view-state", "/api/napari/close",
        "/api/napari/colour-branch-labels", "/api/napari/colour-labels",
        "/api/napari/configure-autosave", "/api/napari/event",
        "/api/napari/gpu", "/api/napari/open",
        "/api/napari/overlay-legend", "/api/napari/refresh-labels",
        "/api/napari/restart", "/api/napari/screenshot",
        "/api/napari/selection-scope", "/api/napari/set-z-view", "/api/napari/set-3d-level",
        "/api/napari/show-labels",
        "/api/napari/show-populations", "/api/napari/show-tracks",
        "/api/napari/start-selection", "/api/napari/stop-selection",
        "/api/napari/view-state",
        "/api/movies/delete", "/api/movies/meta",
        "/api/notebooks/build-sysimage",
        "/api/notebooks/create", "/api/notebooks/delete",
        "/api/notebooks/describe", "/api/notebooks/duplicate",
        "/api/notebooks/launch", "/api/notebooks/prune",
        "/api/notebooks/restart", "/api/notebooks/restore",
        "/api/notebooks/revise", "/api/notebooks/shutdown",
        "/api/notebooks/snapshot", "/api/notebooks/write",
        "/api/optical-flow/delete", "/api/optical-flow/inspect",
        "/api/optical-flow/rename",
        "/api/observer/clear", "/api/observer/feedback",
        "/api/observer/labarchives/set",
        "/api/observer/register", "/api/plot_data",
        "/api/pools/set", "/api/preview/run",
        "/api/preview/start", "/api/preview/stop",
        "/api/projects/animations", "/api/projects/boards",
        "/api/projects/canvases", "/api/projects/create",
        "/api/projects/delete", "/api/projects/list",
        "/api/projects/load", "/api/projects/rename",
        "/api/qc/cohort/check", "/api/repl",
        "/api/repl/config", "/api/sets/create",
        "/api/sets/delete", "/api/setup/init",
        "/api/storage/compressor/set", "/api/storage/layout/set", "/api/storage/reclaim",
        "/api/tasks/custom-modules/reload",
        "/api/update/apply",
    ]
    UNSAFE = [
        "/api/app/restart", "/api/app/shutdown",
        "/api/import/register-legacy", "/api/import/scan-legacy",
        "/api/napari/apply-movie-config", "/api/napari/apply-view-state",
        "/api/napari/restart", "/api/napari/stop-selection",
        "/api/notebooks/build-sysimage", "/api/notebooks/launch",
        "/api/notebooks/restart", "/api/notebooks/shutdown",
        "/api/preview/run", "/api/preview/start",
        "/api/preview/stop", "/api/storage/reclaim",
        "/api/update/apply",
    ]
    # counts pinned below: 67 GET, 92 POST, 17 not live-called

    # Served in handle_stream BEFORE handle_http (binary/Range responses), not part of the tables.
    STREAM_ROUTES = ["/api/board-assets", "/api/movies/file"]

    # Would genuinely restart/shut down/spawn a worker if called with an empty body. Their PRESENCE is
    # still pinned by the inventory half; only the live call is skipped.
    unsafe = Set(UNSAFE)

    function dispatched(method, path)
        try
            st, body = handle_http(HTTP.Request(method, path), UInt8[])
            !(st == 404 && occursin("Not found: $path", String(body)))
        catch
            true    # reached a handler and it threw — routing still happened
        end
    end

    missed, checked = String[], 0
    for (m, routes) in (("GET", GET_ROUTES), ("POST", POST_ROUTES)), p in routes
        p in unsafe && continue
        checked += 1
        dispatched(m, p) || push!(missed, "$m $p")
    end
    @test isempty(missed)
    isempty(missed) || @info "routes that no longer dispatch" missed

    # Anti-vacuity: a loop over nothing passes trivially.
    @test checked >= 130
    @test length(GET_ROUTES) == 74 && length(POST_ROUTES) == 104

    # A path nobody registered must still 404, else "dispatched" means nothing.
    @test !dispatched("GET",  "/api/definitely-not-a-route")
    @test !dispatched("POST", "/api/definitely-not-a-route")
    # …and an unrouted METHOD falls through to 405, so method association is real.
    @test first(handle_http(HTTP.Request("DELETE", "/api/health"), UInt8[])) == 405

    # INVENTORY — every /api literal in server.jl is accounted for, and vice versa. Structure-agnostic
    # on purpose: it keeps working whatever shape the router takes next.
    src = read(joinpath(@__DIR__, "..", "src", "server.jl"), String)
    literals = Set(strip(m.match, '"') for m in eachmatch(r"\"/api/[^\"]+\"", src))
    @test literals == Set(vcat(GET_ROUTES, POST_ROUTES, STREAM_ROUTES))
end

@testset "API: movie output size (blank means the canvas size)" begin
    # `_movie_size_params` is the ONE reader of a requested movie size for all three surfaces (single
    # record, keyframe animation, batch). "Blank = the napari canvas size" has to be defined once: a
    # movie has always come out at canvas size, so an absent field is the default, not an error — and
    # `nothing` is what reaches `record_timelapse!`, which omits the size and lets napari use the canvas.
    # The pixel-level validation (clamp, even axes) lives in Python's movie_io.coerce_movie_size.
    p(json) = _movie_size_params(JSON3.read(json))

    @test p("""{"sizeX":1920,"sizeY":1080}""") == (1920, 1080)
    @test p("{}") == (nothing, nothing)                     # absent → canvas size
    @test p("""{"sizeX":1920}""") == (1920, nothing)         # one axis: the caller decides what half means
    # a blank field arrives as "" from a number input the user cleared — not a parse error, a default
    @test p("""{"sizeX":"","sizeY":""}""") == (nothing, nothing)
    @test p("""{"sizeX":"1920","sizeY":"1080"}""") == (1920, 1080)   # strings parse
    # zero/negative/junk are all "unset" rather than 500s — the size is advisory, never fatal
    @test p("""{"sizeX":0,"sizeY":0}""") == (nothing, nothing)
    @test p("""{"sizeX":-4,"sizeY":-4}""") == (nothing, nothing)
    @test p("""{"sizeX":"wide","sizeY":"tall"}""") == (nothing, nothing)
    @test p("""{"sizeX":null,"sizeY":null}""") == (nothing, nothing)
end

@testset "API: movie filename suffix (two movies of one image)" begin
    # A movie is named after the IMAGE, so recording the AF-corrected version and then the raw import
    # writes the same path twice — the second silently replaces the first. `_movie_suffix` is the
    # filename addition that keeps them apart; the frontend prefills it with the shown version, but it
    # is free text, so it must be sanitised HERE rather than trusted.
    @test _movie_suffix("corrected") == "_corrected"
    @test _movie_suffix("") == ""
    @test _movie_suffix(nothing) == ""
    @test _movie_suffix("   ") == ""
    @test _movie_suffix(" raw vs af ") == "_raw_vs_af"        # spaces are not filename material
    @test _movie_suffix("../../etc/passwd") == "_etc_passwd"   # no separators, no leading dots
    @test _movie_suffix("__x__") == "_x"                       # no doubled/trailing separators
    @test _movie_suffix("_") == ""                             # nothing left after stripping
    @test length(_movie_suffix("a"^200)) == MOVIE_SUFFIX_MAX + 1   # + the leading '_'

    # …and it lands BEFORE the extension in both naming schemes, or the file stops being an .mp4 to
    # every listing that filters on one.
    @test endswith(_movie_basename(Dict(), "abc", String[]; suffix = "_corrected"), "abc_corrected.mp4")
    @test endswith(_movie_basename(Dict("a" => "wt"), "abc", ["a"]; suffix = "_raw"), "wt_abc_raw.mp4")
end

@testset "API: zarr v2 and v3 read identically" begin
    # Two committed stores holding the SAME real pixels, written by bioformats2raw 0.12.1 as NGFF 0.4
    # (zarr v2) and NGFF 0.5 (zarr v3, SHARDED). See test-data/README.md + docs/todo/ZARR_V3_PLAN.md.
    #
    # Why real stores rather than hand-written metadata: NGFF 0.5 nests every attribute under `ome`, and
    # a reader that misses that does not error — axes come back EMPTY and `axis_dims` silently guesses
    # the order by rank, while scale comes back missing and becomes "1 um, 1 second per frame"
    # downstream. Both failures look like success. The fixture's calibration is deliberately NOT 1.0 so
    # a correct read is distinguishable from the fallback.
    v2 = api_fixture("ZARRFMT", "0", "ZV2img", "ccidImage.ome.zarr")
    v3 = api_fixture("ZARRFMT", "0", "ZV3img", "ccidImage.ome.zarr")
    if !(api_have_fixture(v2) && api_have_fixture(v3))
        @test_skip "zarr format fixtures missing"
    else
        a2, ax2 = open_level0(v2)
        a3, ax3 = open_level0(v3)

        # axes must be READ, not guessed. `String[]` here is the silent-failure signature.
        @test ax2 == ["t", "c", "z", "y", "x"]
        @test ax3 == ax2
        @test !isempty(ax3)
        @test axis_dims(ax3, ndims(a3)) == axis_dims(ax2, ndims(a2))

        @test image_geometry(v2) == image_geometry(v3)
        @test image_geometry(v2) == (sizeX = 64, sizeY = 64, sizeZ = 3, sizeT = 3)

        # identical pixels across formats — this is also what proves v3 needs no byte-order branch:
        # v3 keeps `endian` in the `bytes` codec INSIDE the pipeline Zarr.jl executes, unlike v2 where
        # the dtype string is metadata Zarr.jl parses for the eltype and then ignores.
        b2 = read_native(a2, :, :, :, :, :)
        b3 = read_native(a3, :, :, :, :, :)
        @test size(b2) == size(b3)
        @test b2 == b3
        @test maximum(b2) > 3000            # real intensity data, not a zeroed/garbled read

        # store_compression reports the format, and chunk-vs-shard the right way round. The v3 fixture
        # is sharded with shard != chunk ON PURPOSE — with equal values this assertion cannot fail.
        # the NGFF spec version each store declares — a different question from the zarr format, and
        # both are shown side by side in the metadata modal
        @test ngff_version(v2) == "0.4"
        @test ngff_version(v3) == "0.5"

        c2 = store_compression(v2); c3 = store_compression(v3)
        @test c2.zarrFormat == 2 && isnothing(c2.shard)
        @test c3.zarrFormat == 3
        @test c3.chunks == [1, 1, 1, 32, 32]      # inner chunk (from the sharding codec)
        @test c3.shard  == [1, 1, 1, 64, 64]      # outer grid = one file on disk
        @test c3.chunks != c3.shard

        # The chunk-key separator: "/" nests keys into a directory tree, "." keeps them flat. It is
        # most of a store's filesystem footprint (measured on a real 1.7 GB import: 20,933 directories
        # nested vs 4 flat) and all of its cost on a network share, so the modal states it. The DEFAULT
        # differs per format — "." for v2, "/" for v3 — so an absent key must not be read as one value.
        @test c2.separator in (".", "/")
        @test c3.separator in (".", "/")
        @test c2.separator == "/"      # bioformats2raw nests by default, in BOTH formats
        @test c3.separator == "/"
        # same codec asked for on both, so the describer must agree across formats (int shuffle in v2
        # metadata, NAME in v3 — normalised in one place)
        @test c2.codec == c3.codec == "zstd"
        @test c2.shuffle && c3.shuffle
        @test c2.label == c3.label

        # the preview renderer works on both (no props file → percentile auto-contrast)
        for p in (v2, v3)
            png = render_preview_frame(p, joinpath(p, "absent.json"), 1)
            @test length(png) > 100
            @test png[2:4] == UInt8['P', 'N', 'G']
        end
    end
end

@testset "API: store layout defaults" begin
    # DEFAULTS the import form pre-fills, not a switch over what happens next: format and separator are
    # fixed per image at import (no converter) and derived stores inherit. ZARR_V3_PLAN D10.
    st, body = api_store_layout_get(HTTP.Request("GET", "/api/storage/layout"))
    @test st == 200
    d = JSON3.read(body)
    @test d.default == "flat"                       # measured: same read time, ~14% less on disk
    @test d.current in [String(c.name) for c in d.choices]
    @test !isempty(String(d.measuredOn))

    # The rows are the three VIABLE combinations, NOT the cross product. Flat keys + NGFF 0.5 cannot be
    # written (bioformats2raw silently emits zarr v2 for that pair), so it must not be offered at all —
    # an unreachable state beats a warned one.
    @test length(d.choices) == 3
    @test !any(String(c.chunkSeparator) == "flat" && String(c.ngffVersion) == "0.5" for c in d.choices)
    # every row carries its measured numbers, since that is the whole reason this is a table
    for c in d.choices
        for k in (:label, :keys, :dirs, :size, :read, :detail)
            @test !isempty(String(getproperty(c, k)))
        end
    end

    # bad input is rejected rather than silently persisted — this writes custom.toml
    @test _post(api_store_layout_set, Dict("name" => "nope"))[1] == 400
    @test _post(api_store_layout_set, Dict("name" => ""))[1] == 400
end
