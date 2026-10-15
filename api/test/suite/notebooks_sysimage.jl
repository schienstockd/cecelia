# Notebooks sysimage status testset — extracted from api/test/runtests.jl.
#
# One small testset pinning /api/notebooks/status and its staleness classifier: the
# response carries a `sysimage` field in a fixed enumeration, and _classify_sysimage pins
# the update-safety logic against Julia version + Manifest hash (tested without touching
# disk). No path expressions to rewrite. Extracted so runtests.jl contains only include
# lines + section-header comments — same shape as app/test/suite/*.jl.

@testset "API: notebooks sysimage status" begin
    # status always carries a `sysimage` field, one of the valid states (machine-independent: deps.so
    # may or may not exist here). Pins the response contract the frontend's first-run build reads.
    d = JSON3.read(api_notebooks_status(HTTP.Request("GET", "/api/notebooks/status"))[2])
    @test haskey(d, :sysimage)
    @test String(d.sysimage) in ("ready", "building", "error", "absent", "stale", "shared")

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

# A non-owner of a shared (system-scope) install cannot write pluto/ inside it. Simulated by seeding
# the per-dir writability memo for THIS install's pluto/, restored afterwards.
function _with_pluto_readonly(f)
    root = _pluto_root()
    had = haskey(_pluto_writable_cache, root); old = get(_pluto_writable_cache, root, true)
    _pluto_writable_cache[root] = false
    try f() finally had ? (_pluto_writable_cache[root] = old) : delete!(_pluto_writable_cache, root) end
end

@testset "API: notebooks on a shared install, non-owner (fanout-962561e3, fanout-557c5a75)" begin
    # The probe itself: a dir this account cannot write reads as not writable (root bypasses modes).
    ro = mktempdir(); chmod(ro, 0o555)
    try
        if _dir_writable(ro)
            @test_skip "running as root — mode bits do not bind"
        else
            @test !_pluto_writable(ro)
        end
    finally
        chmod(ro, 0o755)
    end

    # fanout-962561e3: the build is refused up front with a 403 that says why — it never spawns a
    # build that fails ~10 min later with a generic "Retry".
    _with_pluto_readonly() do
        code, body = api_notebooks_build_sysimage(UInt8[])
        @test code == 403
        @test occursin("Shared installation", String(JSON3.read(body).error))
        @test _nb_build_proc[] === nothing
    end
    # …and status says so before anyone clicks, rather than offering a build that cannot work.
    @test _classify_sysimage(false, nothing, false, false, "1.11", "abc"; writable = false) == "shared"
    @test _classify_sysimage(false, nothing, false, true,  "1.11", "abc"; writable = false) == "shared"
    @test _classify_sysimage(true, "{\"julia\":\"1.10\",\"manifest\":\"abc\"}", false, false,
                             "1.11", "abc"; writable = false) == "shared"                  # stale, can't rebuild
    @test _classify_sysimage(true, "{\"julia\":\"1.11\",\"manifest\":\"abc\"}", false, false,
                             "1.11", "abc"; writable = false) == "ready"                   # owner's image serves all

    # fanout-557c5a75: the launch publishes the session secret per-user, not into the install — for
    # an install we own too: the secret is how a backend tells its Pluto from another Cecelia's, so
    # two config dirs on one checkout must not share the file. Built for real: the launch command
    # hands launch.jl exactly the file the API reads its secret from.
    for readonly in (true, false)
        cmd = readonly ? _with_pluto_readonly(() -> _pluto_launch_cmd(mktempdir())) : _pluto_launch_cmd(mktempdir())
        env = Dict(split(kv, "="; limit = 2) for kv in cmd.env)
        @test env["CECELIA_PLUTO_SECRET_FILE"] == pluto_secret_path() == joinpath(config_dir(), "pluto", ".plutosecret")
        @test env["CECELIA_PLUTO_PORT"] == string(service_port(:notebooks))
        @test isdir(dirname(pluto_secret_path()))
    end
    # launch.jl writes where the API reads: it honours the path the launch hands it.
    @test occursin("CECELIA_PLUTO_SECRET_FILE", read(joinpath(_pluto_root(), "launch.jl"), String))
end

@testset "API: a standalone `pixi run notebooks` binds this user's slot port" begin
    # The backend pins CECELIA_PLUTO_PORT; standalone, a bare 7660 is slot 0's — another user's on a
    # shared machine, and our backend probes its own slot's port. Same rule as `pixi run runner`.
    src = read(joinpath(_pluto_root(), "launch.jl"), String)
    @test occursin("const PLUTO_PORT = service_port(:notebooks; slot = current_port_slot())", src)
    @test !occursin("\"7660\"", src)
    # …and publishes its secret where this config dir's backend reads it.
    @test occursin("get(ENV, \"CECELIA_PLUTO_SECRET_FILE\", pluto_secret_path())", src)
end

@testset "API: notebooks never adopt another Cecelia's Pluto" begin
    # The sticky port slot does not check :7660+, so another config dir's orphaned Pluto can answer on
    # our notebooks port. Ownership = it accepts OUR session secret; a stand-in Pluto here answers 403
    # unless the request carries its own secret, exactly as Pluto's auth middleware does.
    theirs = "their-secret-" * string(rand(UInt64); base = 16)
    paths  = String[]
    srv = HTTP.listen!("127.0.0.1", 0) do stream
        uri = HTTP.URI(stream.message.target)
        push!(paths, uri.path)
        ok = get(HTTP.queryparams(uri), "secret", "") == theirs
        HTTP.setstatus(stream, ok ? 200 : 403)
        HTTP.startwrite(stream)
        Cecelia.write_http_body!(stream, ok ? "OK!" : "Not yet authenticated")
    end
    port = Int(HTTP.port(srv))
    url  = "http://localhost:$port/"
    uid  = "nbforeign" * string(rand(UInt32); base = 16)
    mkpath(joinpath(projects_dir(), uid))
    try
        @test _notebook_server_probe(; url, secret = theirs) === NB_OURS
        @test _notebook_server_probe(; url, secret = "ours") === NB_FOREIGN
        @test _notebook_server_probe(; url, secret = "") === NB_FOREIGN      # no secret file yet
        # The cheap authenticated route, never the index page the status poll would otherwise fetch.
        @test !isempty(paths) && all(==("/auth-check"), paths)
        # Our port, our secret file (which cannot hold `theirs`): refused rather than adopted, and the
        # routes say so, flagged `foreign` so the Notebooks page offers the repair.
        withenv("CECELIA_PLUTO_PORT" => string(port)) do
            @test !_notebook_server_alive()
            @test_throws r"another Cecelia" _ensure_notebook_server!(mktempdir())
            d = JSON3.read(api_notebooks_status(HTTP.Request("GET", "/api/notebooks/status"))[2])
            @test d.running === false && d.foreign === true
            @test occursin("another Cecelia", String(d.error))
            code, body = _post(api_notebooks_launch, Dict("projectUid" => uid))
            @test code == 409 && JSON3.read(body).foreign === true
            # A notebook FILE is guarded against ANY Pluto: two config dirs can share a projects dir,
            # so another Cecelia's may have this very file open.
            nbdir = _project_notebooks_dir(uid); mkpath(nbdir); write(joinpath(nbdir, "x.jl"), "")
            code, _ = _post(api_notebooks_delete, Dict("projectUid" => uid, "file" => "x.jl"))
            @test code == 409 && isfile(joinpath(nbdir, "x.jl"))
            # Shut down without the user's explicit `force` leaves it running (the forced stop kills by
            # port — exercised against an out-of-process stand-in in suite/shutdown.jl).
            code, body = api_notebooks_shutdown(UInt8[])
            @test code == 409 && occursin("another Cecelia", String(JSON3.read(body).message))
            # Quit/Restart's port-level backstop leaves it alone too (a regression kills THIS process).
            @test_logs (:warn, r"belongs to another Cecelia") (@test !_free_notebooks_port!())
            @test _notebook_server_probe(; url, secret = theirs) === NB_OURS
        end
    finally
        close(srv)
        rm(joinpath(projects_dir(), uid); recursive = true, force = true)
    end
    @test _notebook_server_probe(; url, secret = theirs) === NB_NONE       # nothing listening
end
