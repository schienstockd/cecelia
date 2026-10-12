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

    # fanout-557c5a75: the launch publishes the session secret per-user, not into the install.
    _with_pluto_readonly() do
        p = _secret_path()
        @test !startswith(p, _pluto_root())
        @test startswith(p, config_dir())
    end
    # launch.jl writes where the API reads: it honours the path the launch hands it.
    @test occursin("CECELIA_PLUTO_SECRET_FILE", read(joinpath(_pluto_root(), "launch.jl"), String))
end
