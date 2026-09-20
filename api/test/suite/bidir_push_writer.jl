# BIDIR Part 5 push writer + Kiwi target endpoints testsets — extracted from api/test/runtests.jl.
#
# Six testsets covering BIDIR_PUSH_PLAN PR #2/#3 + Kiwi follow-ups:
#  - `API: format_capture_message builds the locked-format one-liner` (chat grep target).
#  - `API: push_capture_notification without pairing ⇒ :not_paired, silent`.
#  - `API: push_capture_notification round-trips to a local Unix socket`.
#  - `API: push writer clears stale target on connect failure ⇒ :fallback`.
#  - `API: POST /api/push/target/clear removes the pairing record` (Kiwi PR #3).
#  - `API: POST /api/push/target/probe` (Kiwi follow-up).
#
# No path expressions to rewrite. Extracted so runtests.jl contains only include lines +
# section-header comments — same shape as app/test/suite/*.jl.

# ── BIDIR Part 5 push writer (BIDIR_PUSH_PLAN PR #2) ────────────────────────
@testset "API: format_capture_message builds the locked-format one-liner" begin
    # The exact wording is load-bearing — a chat-side grep for `"[cecelia] shared capture"`
    # must find every incident, past and future. Change the wording only if you're changing
    # the plan's Decision 5.
    m1 = format_capture_message("zolIMa", "cap-20260919T140000-abcdef",
        Dict{String,Any}("surface" => "viewer_frame", "imageUid" => "1SqevM",
                         "t" => 3, "z" => 7))
    @test m1 == "[cecelia] shared capture cap-20260919T140000-abcdef from project zolIMa " *
                "(viewer_frame, image 1SqevM, t=3, z=7). Read it with " *
                "get_capture(\"zolIMa\", \"cap-20260919T140000-abcdef\")."

    # A UI-surface capture with no image / t / z reads cleanly — parts that are empty are
    # dropped rather than showing "()" or "image ".
    m2 = format_capture_message("p", "cap-x", Dict{String,Any}("surface" => "ui"))
    @test m2 == "[cecelia] shared capture cap-x from project p (ui). Read it with " *
                "get_capture(\"p\", \"cap-x\")."

    # Zero-address capture (address stripped or absent) still names project + id.
    m3 = format_capture_message("p", "cap-y", nothing)
    @test occursin("[cecelia] shared capture cap-y from project p", m3)
    @test occursin("get_capture(\"p\", \"cap-y\")", m3)
end

@testset "API: push_capture_notification without pairing ⇒ :not_paired, silent" begin
    # No push_target.json on disk ⇒ we skip the socket work and return :not_paired. This is
    # the common "user hasn't paired a session yet" case; the frontend renders it the same as
    # :fallback (both trigger the clipboard/toast path).
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-push-unpaired")
        outcome, msg = push_capture_notification(proj.uid, "cap-x",
            Dict{String,Any}("surface" => "viewer_frame"))
        @test outcome === :not_paired
        @test msg === nothing
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: push_capture_notification round-trips to a local Unix socket" begin
    # The full wire test: bind a Unix socket in the test process, listen, then have the writer
    # connect + send both lines. Assert the receiver saw the exact bytes documented above.
    # Skips on Windows — the named-pipe branch would need a different listener setup and this
    # PR doesn't have a Windows box to verify against.
    Sys.iswindows() && (@test_skip "push writer round-trip (Windows named pipes)"; return)

    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    sock_path = joinpath(tmp, "probe.sock")
    server = Sockets.listen(sock_path)
    received = String[]
    receiver = @async begin
        try
            s = Sockets.accept(server)
            while !eof(s)
                line = readline(s; keep = false)
                isempty(line) || push!(received, line)
            end
            close(s)
        catch  # server closed during test teardown
        end
    end
    try
        proj = create_project!(name = "api-push-uds")
        # Write a pairing record ourselves — mirrors what /api/push/target POST would do.
        target_dir = joinpath(tmp, proj.uid, "settings")
        mkpath(target_dir)
        write_json_atomic(joinpath(target_dir, "push_target.json"), Dict{String,Any}(
            "socketPath" => sock_path, "token" => "test-token-abc",
            "sessionLabel" => "probe", "pairedAt" => "2026-09-19T14:00:00",
            "pairedFromPid" => "0",
        ))
        outcome, sent_content = push_capture_notification(proj.uid, "cap-y",
            Dict{String,Any}("surface" => "viewer_frame", "imageUid" => "IMG1",
                             "t" => 2, "z" => 5))
        @test outcome === :sent
        @test sent_content !== nothing
        @test occursin("cap-y", sent_content::String)
        # Give the receiver a moment to drain both lines from the socket buffer.
        for _ in 1:20
            length(received) >= 2 && break
            sleep(0.05)
        end
        @test length(received) == 2
        auth = JSON3.read(received[1], Dict{String,Any})
        msg  = JSON3.read(received[2], Dict{String,Any})
        @test auth["type"]  == "auth"
        @test auth["token"] == "test-token-abc"
        @test msg["type"]   == "user"
        @test msg["message"]["role"]    == "user"
        @test msg["message"]["content"] == sent_content
    finally
        try; close(server); catch; end
        # The @async receiver exits on eof/close; give it a moment.
        try; wait(receiver); catch; end
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: push writer clears stale target on connect failure ⇒ :fallback" begin
    # A pairing record pointing at a dead socket path ⇒ the writer catches the connect
    # exception, removes the record, and returns :fallback. Next auto-pair (from any MCP
    # tool call) rewrites it; until then GET /api/push/target reads "not paired".
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-push-stale")
        target_dir = joinpath(tmp, proj.uid, "settings")
        mkpath(target_dir)
        stale_path = joinpath(tmp, "definitely-not-a-socket.sock")
        target_json = joinpath(target_dir, "push_target.json")
        write_json_atomic(target_json, Dict{String,Any}(
            "socketPath" => stale_path, "token" => "t",
        ))
        outcome, _ = push_capture_notification(proj.uid, "cap-z", nothing)
        @test outcome === :fallback
        # Stale record cleared silently — no leftover file to re-attempt next time.
        @test !isfile(target_json)
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: POST /api/push/target/clear removes the pairing record (Kiwi PR #3)" begin
    # Manual unpair — the Kiwi cockpit's "Clear pairing" button. Deletes the file if present;
    # succeeds silently when it wasn't there (idempotent).
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-push-clear")
        target_dir = joinpath(tmp, proj.uid, "settings")
        mkpath(target_dir)
        target_json = joinpath(target_dir, "push_target.json")
        write_json_atomic(target_json, Dict{String,Any}(
            "socketPath" => "/tmp/nowhere.sock", "token" => "t",
        ))
        @test isfile(target_json)

        # Clear it via the HTTP handler. Rebuild the body each call — `_parse_body` moves the
        # bytes into a String, so a reused Vector{UInt8} reads empty on the second dispatch.
        make_body() = Vector{UInt8}(JSON3.write(Dict("projectUid" => proj.uid)))
        status, resp = api_push_target_clear(make_body())
        @test status == 200
        parsed = JSON3.read(resp, Dict{String,Any})
        @test parsed["ok"] == true
        @test parsed["cleared"] == true
        @test !isfile(target_json)

        # Second clear on an already-gone file — still 200, cleared: false.
        status2, resp2 = api_push_target_clear(make_body())
        @test status2 == 200
        @test JSON3.read(resp2, Dict{String,Any})["cleared"] == false

        # Bad inputs — 400 on empty projectUid, 404 on unknown project.
        st400, _ = api_push_target_clear(Vector{UInt8}(JSON3.write(Dict("projectUid" => ""))))
        @test st400 == 400
        st404, _ = api_push_target_clear(Vector{UInt8}(JSON3.write(Dict("projectUid" => "does-not-exist"))))
        @test st404 == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end

@testset "API: POST /api/push/target/probe (Kiwi follow-up)" begin
    # Liveness probe. Three shapes:
    #   1. Unpaired ⇒ {paired:false, alive:false} with no side effect.
    #   2. Paired at a live listener ⇒ {paired:true, alive:true}.
    #   3. Paired at a dead socket ⇒ {paired:false, alive:false, reason:...} + record cleared.
    conf = cecelia_conf(); dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    tmp = mktempdir(); dirs["projects"] = tmp
    try
        proj = create_project!(name = "api-push-probe")
        target_dir = joinpath(tmp, proj.uid, "settings")
        mkpath(target_dir)
        target_json = joinpath(target_dir, "push_target.json")
        make_body() = Vector{UInt8}(JSON3.write(Dict("projectUid" => proj.uid)))

        # (1) unpaired — no file yet.
        st1, r1 = api_push_target_probe(make_body())
        @test st1 == 200
        p1 = JSON3.read(r1, Dict{String,Any})
        @test p1["paired"] == false && p1["alive"] == false

        # (2) alive — bind a local UDS in a background task, probe it. Skipped on native
        # Windows: `Sockets.listen(<file path>)` requires a `\\.\pipe\...` name there rather
        # than a plain filesystem path, so a temp-dir socket file is a portable server we can't
        # spin up. The probe under test itself IS Windows-safe (Julia's `Sockets.connect`
        # dispatches on the transport transparently — see push_writer.jl); the dead-socket
        # branch below still exercises the code path on every platform.
        if !Sys.iswindows()
            live_path = joinpath(tmp, "live.sock")
            server = Sockets.listen(live_path)
            accept_task = @async try Sockets.accept(server) catch _ end
            try
                write_json_atomic(target_json, Dict{String,Any}(
                    "socketPath" => live_path, "token" => "t",
                ))
                st2, r2 = api_push_target_probe(make_body())
                @test st2 == 200
                p2 = JSON3.read(r2, Dict{String,Any})
                @test p2["paired"] == true && p2["alive"] == true
                @test isfile(target_json)   # alive ⇒ record preserved
            finally
                close(server)
                try wait(accept_task) catch _ end
            end
        end

        # (3) dead — point at a socket path nobody is listening on.
        dead_path = joinpath(tmp, "definitely-not-a-socket.sock")
        write_json_atomic(target_json, Dict{String,Any}(
            "socketPath" => dead_path, "token" => "t",
        ))
        st3, r3 = api_push_target_probe(make_body())
        @test st3 == 200
        p3 = JSON3.read(r3, Dict{String,Any})
        @test p3["paired"] == false && p3["alive"] == false
        @test haskey(p3, "reason")
        @test !isfile(target_json)   # dead ⇒ record cleared server-side

        # Bad inputs mirror the clear route.
        st400, _ = api_push_target_probe(Vector{UInt8}(JSON3.write(Dict("projectUid" => ""))))
        @test st400 == 400
        st404, _ = api_push_target_probe(Vector{UInt8}(JSON3.write(Dict("projectUid" => "does-not-exist"))))
        @test st404 == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        rm(tmp; recursive = true, force = true)
    end
end
