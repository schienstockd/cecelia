# Admin + install/update + app-lifecycle API testsets — extracted from api/test/runtests.jl.
#
# 13 testsets covering the server-admin / install / update / lifecycle surface: diagnostics,
# pool-limit set guards, task thread budget (reports where the number came from), maintenance
# patches, system envs (probe + install guards), running-version (prefers dev marker over
# stale VERSION), _find_pixi (falls back past PATH), update scope, update version ordering
# (rcN sorts numerically), update apply guard rails, setup wizard, app lifecycle, and the
# incremental console log ring. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl. The extracted file loads at
# the top level of runtests.jl, so helpers defined earlier (_post) are still in scope
# (lexical include).

@testset "API: diagnostics" begin
    st, body = api_diagnostics(HTTP.Request("GET", "/api/diagnostics"))
    @test st == 200
    d = JSON3.read(body)
    @test d.threads >= 1
    @test !isempty(String(d.julia))
    @test haskey(d, :replAvailable) && haskey(d, :loopback) && haskey(d, :replEnabled)
    # service ports surfaced for the System panel
    @test d.port > 0 && d.previewPort == 7656 && d.notebooksPort == 7660
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

@testset "API: task thread budget — reports where the number came from" begin
    # The `derived` flag is the point of the route: an effective 16 means something different when it
    # came from the core count than when someone chose it, because the derived value follows the box.
    # A readout that cannot tell them apart shows a setting nobody set.
    st, body = api_task_threads_get(HTTP.Request("GET", "/api/tasks/threads"))
    @test st == 200
    r = JSON3.read(body)
    @test r.workers >= 1 && r.workers <= r.max
    @test r.default >= 1
    @test r.derived isa Bool
    @test r.cores >= 1

    # A non-numeric value is a 400, not a silent fall back to the default — the caller asked for
    # something specific and got neither it nor a complaint.
    st2, _ = _post(api_task_threads_set, Dict("workers" => "lots"))
    @test st2 == 400
    # The success path is covered in app/test, where the config dir is redirected to a temp so the
    # real custom.toml is untouched.
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

@testset "API: system envs — probe + install guards" begin
    # Probe reports the current platform and the catalog. On this Linux CI box, cellpose-v3 is not
    # supported and not installed.
    st, body = api_system_envs(nothing)
    @test st == 200
    doc = JSON3.read(body)
    @test haskey(doc, :platform)
    @test haskey(doc, :envs)
    @test haskey(doc.envs, Symbol("cellpose-v3"))
    v3 = doc.envs[Symbol("cellpose-v3")]
    @test haskey(v3, :installed) && haskey(v3, :supported) && haskey(v3, :approxSizeMb)

    # The install POST must refuse an unknown env with 400 — never silently accept.
    st, body = api_system_envs_install(Vector{UInt8}(JSON3.write(Dict("env" => "no-such-env"))))
    @test st == 400
    @test haskey(JSON3.read(body), :error)

    # The install POST must refuse a supported-elsewhere env on a non-Mac host with 400 (this test
    # runs on linux-64 / win-64 in CI; on osx-arm64 CI it would begin the job instead — but we're
    # asserting the guard, not the happy path). Skip on macOS.
    if !Sys.isapple()
        st, body = api_system_envs_install(Vector{UInt8}(JSON3.write(Dict("env" => "cellpose-v3"))))
        @test st == 400
        @test occursin("not supported", String(JSON3.read(body)[:error]))
    end

    # Invalid JSON body must 400 too, never crash the handler.
    st, body = api_system_envs_install(Vector{UInt8}("not json"))
    @test st == 400
end

@testset "API: running version prefers dev marker over stale VERSION" begin
    # A dev-channel apply moves the branch archive's payload over the install root, but that archive
    # contains no VERSION file (only release.yml writes one). So on a stable→dev flip, ROOT/VERSION
    # keeps the previous release's tag — preferring it would report the stale semver while the
    # dev-channel update check correctly reports "dev @ main <sha>", and the two surfaces would
    # disagree (that was the reported bug: one panel showed the release tag, another the dev sha).
    # `_running_version` checks `.cecelia-version` first and returns "dev" whenever it declares a dev
    # build, collapsing the mismatch to one answer.
    withenv("CECELIA_VERSION" => nothing) do
        mktempdir() do root
            @test _running_version(root) == "dev"                          # no markers → dev
            write(joinpath(root, "VERSION"), "v0.2.1")
            @test _running_version(root) == "v0.2.1"                       # stable install
            write(joinpath(root, ".cecelia-version"), "dev @ main 1a2b3c4")
            @test _running_version(root) == "dev"                          # dev marker beats stale VERSION
            write(joinpath(root, ".cecelia-version"), "v0.2.1")            # non-dev marker → VERSION wins
            @test _running_version(root) == "v0.2.1"
        end
    end
    # Env override still wins over both files.
    withenv("CECELIA_VERSION" => "v9.9.9") do
        mktempdir() do root
            write(joinpath(root, "VERSION"), "v0.2.1")
            write(joinpath(root, ".cecelia-version"), "dev @ main 1a2b3c4")
            @test _running_version(root) == "v9.9.9"
        end
    end
end

@testset "API: _find_pixi falls back past PATH" begin
    # `Sys.which` alone was too strict: the desktop shortcut wrapper exports pixi on PATH, but a
    # `.desktop` launch with a minimal inherited env, or `pixi run app` from a shell where pixi is
    # not on PATH, arrives at the update apply with a stripped PATH and the pre-fix code errored
    # with "the Cecelia install looks broken". `_find_pixi` mirrors install.sh's install locations.
    exe = Sys.iswindows() ? "pixi.exe" : "pixi"
    withenv("PIXI_HOME" => nothing) do
        mktempdir() do root
            # Nothing on disk and (assume) nothing on PATH from this shell → empty. If the CI runner
            # happens to have pixi on PATH we can't test the empty case here, so just require the
            # non-empty result to be a file that exists.
            r = _find_pixi(root)
            @test isempty(r) || isfile(r)

            # System-scope layout: `<root>/pixi/bin/pixi` — the install.sh location.
            sys_bin = joinpath(root, "pixi", "bin")
            mkpath(sys_bin)
            fake = joinpath(sys_bin, exe)
            write(fake, "")
            # If PATH happened to have a real pixi, that still wins — but the disk fallback must at
            # least resolve to a real file (either PATH or our fake).
            @test isfile(_find_pixi(root))
        end
    end
    # PIXI_HOME env var: honoured over the ~/.pixi default.
    mktempdir() do home
        bin = joinpath(home, "bin")
        mkpath(bin)
        fake = joinpath(bin, exe)
        write(fake, "")
        withenv("PIXI_HOME" => home) do
            # A random root that has no `pixi/bin` — PIXI_HOME must resolve. Falls through to PATH
            # only if PATH itself has a real pixi, so the assertion is "some real file".
            @test isfile(_find_pixi(mktempdir()))
        end
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

    # Dev channel: `version` is a full 40-char sha, not a tag. Same guard-rail shape — anchored,
    # interpolated into an archive URL, and the same scope/install rules apply.
    okdev(sha) = _apply_precheck(sha; scope = "user", installed = true, channel = "dev")
    @test okdev("0123456789abcdef0123456789abcdef01234567") === nothing
    @test okdev("f"^40) === nothing
    # Rejected: too short (short sha), too long, uppercase (GitHub returns lowercase, and the
    # comparison in `_installed_dev_sha` lowercases both sides — but for the URL, be strict).
    for bad in ["deadbeef", "0"^39, "0"^41, "0123456789ABCDEF0123456789abcdef01234567",
                "v0.1.0", "main", "0"^40 * "/..", "0"^40 * " x", "../etc/passwd"]
        r = _apply_precheck(bad; scope = "user", installed = true, channel = "dev")
        @test r !== nothing && r[1] == 400
    end
    # Scope + install rules apply to dev too.
    @test _apply_precheck("0"^40; scope = "system", installed = true, channel = "dev")[1] == 403
    @test _apply_precheck("0"^40; scope = "dev",    installed = false, channel = "dev")[1] == 400
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
    st2, body2 = api_logs_recent(HTTP.Request("GET", "/api/logs/recent"))
    @test st2 == 200
    d2 = JSON3.read(body2)
    @test haskey(d2, :logs)
    # `ringId` is what tells a client its `seq` cursor still refers to THIS ring. Without it, a client
    # reconnecting to a RESTARTED backend would treat the fresh ring's first N records as ones it had
    # already seen and drop them — the restart would silently eat its own startup lines.
    @test !isempty(String(d2.ringId))
    @test d2.seq isa Integer
end

@testset "API: the console log ring is incremental" begin
    # `?since=<seq>` is the gap repair: a WS log frame is DROPPABLE (broadcast_ws skips a full client's
    # queue rather than block a worker), and before the ring carried a sequence nothing could even
    # notice a line had gone missing. Exercised through the real handler, against the real ring.
    before = Cecelia.log_ring_seq(_log_ring)
    _log_sink(Cecelia.log_record("warn", "ring probe"; source = Cecelia.LOG_SOURCE_PREVIEW))

    _, all_body = api_logs_recent(HTTP.Request("GET", "/api/logs/recent"))
    probe = last(JSON3.read(all_body).logs)
    @test String(probe.message) == "ring probe"
    @test String(probe.source)  == Cecelia.LOG_SOURCE_PREVIEW     # the facet survives the round trip
    @test probe.seq == before + 1
    @test !isempty(String(probe.ts))                             # stamped by the SINK, not the caller

    # since = the record's own seq → nothing new; since = one less → exactly that record
    _, none_body = api_logs_recent(HTTP.Request("GET", "/api/logs/recent?since=$(before + 1)"))
    @test isempty(JSON3.read(none_body).logs)
    _, gap_body = api_logs_recent(HTTP.Request("GET", "/api/logs/recent?since=$before"))
    @test length(JSON3.read(gap_body).logs) == 1

    # a garbage cursor backfills everything rather than 500-ing
    st, _ = api_logs_recent(HTTP.Request("GET", "/api/logs/recent?since=abc"))
    @test st == 200
end
