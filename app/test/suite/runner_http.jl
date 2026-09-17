# ── Runner + HTTP body testsets ───────────────────────────────────────
# Four sections covering the runner + HTTP-body plumbing: view profiles (curated
# sidebar), runner_serve stands down when the port is taken, _runner_owns_port
# recognises a port WE just bound, and an empty response body is written through
# write_http_body!. Extracted from suite.jl to keep it small enough to merge without EOF
# conflicts on every append. The extracted file loads inside this file's aggregating
# testset scope, so any helpers defined earlier in suite.jl are still in scope (lexical
# include).
#
# One `joinpath(_app_src, "runner", "server.jl")` scan (in the empty-response
# body testset) is rerouted through pathof(Cecelia) via `_app_src` so it resolves
# identically whether the file sits at app/test/ or app/test/suite/.

_app_src = joinpath(dirname(dirname(dirname(pathof(Cecelia)))), "app", "src")

@testset "view profiles (curated sidebar)" begin
    # A profile is a named, ORDERED subset of sidebar routes, dropped in as a file under
    # <config_dir>/profiles/. The reader validates SHAPE ONLY — it deliberately does not know the route
    # table (that lives in frontend/src/main.ts), so an item naming a route that no longer exists is a
    # frontend concern, not an error here. See docs/todo/VIEW_PROFILES_PLAN.md.
    @test view_profiles_dir() == joinpath(config_dir(), "profiles")

    # ── parse: what a valid profile is ────────────────────────────────────────
    p = parse_view_profile("focused", Dict("label" => "Gating + behaviour",
                                           "items" => ["/gate", "/behaviour"]))
    @test p.id == "focused"
    @test p.label == "Gating + behaviour"
    @test p.items == ["/gate", "/behaviour"]

    # the label falls back to the id, so a file needs only `items`
    @test parse_view_profile("myview", Dict("items" => ["/gate"])).label == "myview"
    # order is the profile's, and duplicates keep their FIRST position
    @test parse_view_profile("x", Dict("items" => ["/track", "/gate", "/track"])).items ==
          ["/track", "/gate"]

    # ── parse: every rejection is a message, never a crash ────────────────────
    @test_throws ArgumentError parse_view_profile("x", Dict("label" => "no items"))
    # empty is an ERROR, not an empty profile: rendering a blank sidebar from a typo looks broken
    @test_throws ArgumentError parse_view_profile("x", Dict("items" => String[]))
    @test_throws ArgumentError parse_view_profile("x", Dict("items" => ["gate"]))   # not a route path
    @test_throws ArgumentError parse_view_profile("x", Dict("items" => [42]))
    @test_throws ArgumentError parse_view_profile("x", "not an object")

    # ── ids come from a user-typed label, never a filename ────────────────────
    @test view_profile_id("Gating + behaviour") == "gating_behaviour"
    @test_throws ArgumentError view_profile_id("+++")
    # and a hostile id cannot escape the profiles dir
    @test !occursin("..", view_profile_id("../../etc/passwd"))

    # ── round-trip through disk: write → read ─────────────────────────────────
    dir = view_profiles_dir()
    saved = write_view_profile("Gating + behaviour", ["/gate", "/track", "/behaviour"])
    @test saved.id == "gating_behaviour"
    @test isfile(joinpath(dir, "gating_behaviour.json"))

    got = read_view_profiles()
    @test got.dir == dir
    mine = only(filter(p -> p.id == "gating_behaviour", got.profiles))
    @test mine.label == "Gating + behaviour"
    @test mine.items == ["/gate", "/track", "/behaviour"]
    @test isempty(got.errors)

    # renaming the LABEL must keep the id, or an active selection breaks under the user
    renamed = write_view_profile("Gating only", ["/gate"]; id = "gating_behaviour")
    @test renamed.id == "gating_behaviour"
    @test renamed.label == "Gating only"
    @test only(filter(p -> p.id == "gating_behaviour", read_view_profiles().profiles)).items == ["/gate"]

    # ── a broken file is REPORTED, never fatal, and never hides the good ones ──
    write(joinpath(dir, "broken.json"), "{ not json")
    write(joinpath(dir, "empty.json"), "{\"items\": []}")
    bad = read_view_profiles()
    @test any(p -> p.id == "gating_behaviour", bad.profiles)     # the valid one still loads
    @test Set(e.file for e in bad.errors) == Set(["broken.json", "empty.json"])

    # a non-.json file in the dir is not a profile and not an error
    write(joinpath(dir, "notes.txt"), "ignore me")
    @test Set(e.file for e in read_view_profiles().errors) == Set(["broken.json", "empty.json"])

    # ── delete ────────────────────────────────────────────────────────────────
    @test delete_view_profile!("gating_behaviour")
    @test !delete_view_profile!("gating_behaviour")             # already gone is not an error
    @test !any(p -> p.id == "gating_behaviour", read_view_profiles().profiles)

    # tidy up so a later testset reading the config dir sees no leftovers
    for f in ("broken.json", "empty.json", "notes.txt")
        rm(joinpath(dir, f); force = true)
    end
end

# ── The task runner losing its port is a normal outcome, not a crash ──────────
#
# The runner is built to OUTLIVE the API server, so on a fresh start there is often already one
# running — and two checkouts sharing a `CECELIA_DEV_DIR` share the port outright. Two things used to
# go wrong when a second one lost that race, and this pins both:
#
#   1. it died with a `TaskFailedException` stack trace, which reads as a broken app; and
#   2. WORSE — it wrote `runner.json` with its own pid BEFORE binding, clobbering the incumbent's
#      record, then its `atexit` hook deleted the file on the way out. So a collision left the
#      SURVIVING runner with no state file, which is exactly the "a stray runner is folklore" case
#      that file exists to prevent.
#
# Uses an EPHEMERAL port held by a plain socket, never the real 7657 — a test must not touch a runner
# the developer has running.
#
# It caught a THIRD way to get this wrong, on macOS CI only: `HTTP.listen!` builds a `Server` and
# spawns a task that does the bind, and the failure path notifies its ready `Event` BEFORE it rethrows —
# so `listen!` returned normally, the state file was claimed, and the EADDRINUSE surfaced at `wait` as
# a `TaskFailedException`. Both original symptoms, back. Ownership is therefore proven by asking
# `/ping` for the responder's PID (`_runner_owns_port`), which no scheduler ordering can fake — the
# raw socket this test holds never speaks HTTP, so nothing answers and the runner stands down.
@testset "runner_serve stands down when the port is taken" begin
    using Sockets: listen as sock_listen, getsockname, localhost

    held = sock_listen(localhost, 0)              # port 0 → the OS picks a free one
    port = Int(getsockname(held)[2])

    state = joinpath(Cecelia.config_dir(), "runner.json")
    mkpath(dirname(state))
    # stand in for the incumbent's record, with a pid that is NOT ours
    marker = Dict{String,Any}("pid" => 999_999, "port" => port, "commit" => "incumbent")
    Cecelia.write_json_atomic(state, marker)

    try
        # Returns rather than throwing. The old code raised TaskFailedException out of HTTP.listen.
        r = @test_logs (:warn,) match_mode=:any Cecelia.runner_serve(; port = port)
        @test r === nothing

        # …and the incumbent's record is untouched: neither overwritten with our pid nor deleted.
        @test isfile(state)
        @test JSON3.read(read(state, String), Dict{String,Any})["pid"] == 999_999
        @test JSON3.read(read(state, String), Dict{String,Any})["commit"] == "incumbent"
    finally
        close(held)
        rm(state; force = true)
    end
end

# ── The TWIN of the test above, and the one whose absence let a regression ship ───────────────────
#
# The guard was only ever pinned from the "port is taken" side, so `_runner_owns_port` answering false
# for a port we had *just bound ourselves* looked like a pass. It shipped, and then every fresh
# `pixi run dev` printed "another process took it" with nothing listening on 7657 at all — the runner
# exited on start and every task fell back to in-process.
#
# The cause was a budget that could not be met, not a wrong idea: a freshly returned `HTTP.listen!`
# server does not serve its first in-process request for ~1.2 s (the accept path is still compiling),
# and the session's first `HTTP.get` compiles for about as long — so a 2 s per-attempt timeout inside a
# 3 s deadline bought ONE attempt, which could not succeed. This test fails outright on those numbers.
#
# Deliberately exercises `_runner_owns_port` against a real `HTTP.listen!` rather than `runner_serve`:
# a full runner installs `_runner_idle_watchdog!`, which calls `exit(0)`, and a test process that may
# exit from under the suite is not a test.
@testset "_runner_owns_port recognises a port WE just bound" begin
    using Sockets: listen as sock_listen, getsockname, localhost

    probe = sock_listen(localhost, 0)             # port 0 → the OS names a free one
    port  = Int(getsockname(probe)[2])
    close(probe)                                   # …then hand it back, so we can bind it ourselves

    server = Cecelia.HTTP.listen!(Cecelia._runner_stream, "127.0.0.1", port)
    try
        @test Cecelia._runner_owns_port(port)
        # and it is OUR pid it recognised, not merely "something answered"
        reply = Cecelia.runner_ping(Cecelia.RunnerHandle(; port = port))
        @test reply !== nothing
        @test string(get(reply, "pid", "")) == string(getpid())
    finally
        try; close(server); catch; end
    end
end

# ── An empty response body must not corrupt the connection ────────────────────
#
# `write_http_body!` (utils.jl, where the mechanism is explained) exists for one wire-level reason:
# our responses carry no Content-Length, so HTTP.jl frames them CHUNKED and frames every `write` as
# its own chunk — so a zero-length write emits the TERMINATING `0\r\n\r\n` and `closewrite` emits a
# second one. This asserts the BYTES, over two requests on ONE keep-alive connection, because the
# response that broke was never the empty one: the extra terminator sits in the connection and the
# NEXT response is parsed starting at it.
#
# The unguarded `write` is kept as the negative control — a guard is only worth having if the test
# fails without it. Ephemeral port, and a raw socket that speaks HTTP by hand (an HTTP client would
# hide the framing, which is the whole subject).
@testset "an empty response body is written through write_http_body!" begin
    using Sockets: connect as sock_connect
    HTTP = Cecelia.HTTP

    # READING THE WIRE, without guessing at packetisation — both halves of this were measured, and
    # both got it wrong first:
    #   * `bytesavailable(sock)` is NOT a usable poll. Julia stops the libuv read loop when its
    #     buffer empties, so it reports 0 with bytes still pending — that version saw 2 terminators
    #     of 4 on half its runs. So ONE reader task blocks in `readavailable` for the socket's whole
    #     life and appends everything; `close` is what ends it.
    #   * The stopping condition must be the DATA, not a fixed sleep. The extra terminator is written
    #     separately from the head, so "both responses are in" can be true with those 4 bytes still in
    #     flight — that is how the first version of this test flaked on ubuntu CI only (2 of 4) while
    #     passing on macOS, Windows and locally. A generous deadline also absorbs the first call's
    #     compilation, which a fixed window did not.
    settle = function (acc, quiet, cap)         # wait until the accumulated bytes stop growing
        n = length(acc[]); t0 = time(); t_last = time()
        while time() - t_last < quiet && time() - t0 < cap
            sleep(0.02)
            if length(acc[]) != n; n = length(acc[]); t_last = time(); end
        end
    end

    wire = function (guarded::Bool)
        handler = function (stream)
            read(stream)
            HTTP.setstatus(stream, 200)
            HTTP.setheader(stream, "Content-Type" => "application/octet-stream")
            HTTP.startwrite(stream)
            guarded ? write_http_body!(stream, UInt8[]) : write(stream, UInt8[])
        end
        server = HTTP.listen!(handler, "127.0.0.1", 0)          # port 0 → never a real one
        try
            sock = sock_connect("127.0.0.1", HTTP.port(server))
            acc  = Ref("")
            reader = @async try
                while !eof(sock); acc[] *= String(copy(readavailable(sock))); end
            catch; end
            try
                for i in 1:2                    # request 2 is the one the stray bytes broke
                    write(sock, "GET /$i HTTP/1.1\r\nHost: x\r\n\r\n")
                    t0 = time()
                    while count("HTTP/1.1 200", acc[]) < i && time() - t0 < 20; sleep(0.02); end
                end
                settle(acc, 0.5, 5.0)
            finally
                close(sock)
            end
            wait(reader)
            acc[]
        finally
            close(server)
        end
    end

    guarded = wire(true)
    @test count("HTTP/1.1 200", guarded) == 2
    @test count("0\r\n\r\n", guarded) == 2        # exactly ONE terminating chunk per response
    @test endswith(guarded, "0\r\n\r\n")          # …and no bytes left over for the next response

    # the bug, on the wire: `…0\r\n\r\n0\r\n\r\nHTTP/1.1 …`. Vite's proxy died on exactly these bytes
    # (`HPE_INVALID_CONSTANT … rawPacket <30 0d 0a 0d 0a 30 0d 0a 0d 0a>`), failing the OTHER, good
    # requests that shared the connection with one legitimately-empty gating plot.
    bare = wire(false)
    @test count("0\r\n\r\n", bare) == 4
    @test occursin("0\r\n\r\n0\r\n\r\n", bare)

    # so no handler hands a body to `write` directly — the runner's reply path included
    src = read(joinpath(_app_src, "runner", "server.jl"), String)
    @test occursin("write_http_body!(stream, body)", src)
    @test !occursin("write(stream, ", src)
end
