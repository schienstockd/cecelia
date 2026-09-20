# Runner + debug console + packages testsets — extracted from api/test/runtests.jl.
#
# Five testsets covering the server-internal control surface: runner relaunch semantics
# (unavailable vs refused, the runner_alive re-check inside the relaunch lock, one-shot
# announcement of runner death), packages diagnostics (Julia via Pkg.dependencies +
# Python via `pixi list`), debug-console gating (loopback + toggle), debug-console eval
# (value / captured stdout / error / empty-code), and repl config toggle.
#
# Path expressions rewritten to use the shared API_TEST_DIR constant defined in runtests.jl.
# Extracted so runtests.jl contains only include lines + section-header comments — same
# shape as app/test/suite/*.jl. Helpers defined earlier (_post, _repl) stay in scope
# (lexical include).

# ── A missing runner is repaired, not silently worked around ──────────────────
# The runner dying used to degrade in total silence: every later run went in-process, dying with the
# next restart — the one thing enabling the runner prevents — and the only tell was a 20-second-polled
# label on the Run panel. Six segmentations were lost to exactly this.
@testset "API: an unavailable runner is relaunched, not quietly bypassed" begin
    src = read(joinpath(API_TEST_DIR, "..", "src", "runner_api.jl"), String)
    sock = read(joinpath(API_TEST_DIR, "..", "src", "sockets.jl"), String)

    # THREE states, not two. A refusal (a live runner saying no) and "nothing answered" call for
    # opposite responses, and they are indistinguishable from a `try` — the same split, for the same
    # reason, as `_submit_chain_to_runner`.
    @test occursin("function _submit_to_runner(req)::Symbol", src)
    for st in (":accepted", ":refused", ":unavailable")
        @test occursin(st, src)
    end
    # With the runner disabled (the state under test), a submit is `:unavailable` — never `:refused`,
    # which would send the dispatch down the "a live runner said no" path and skip the relaunch.
    @test _submit_to_runner(TaskRequest(; task_id = "t1", fun_name = "segment.cellpose",
                                          project_uid = "p", image_uid = "i")) === :unavailable

    # …and only `:unavailable` relaunches. Relaunching on `:refused` would fight a runner that is
    # alive and has already answered.
    @test occursin("st === :unavailable && _runner_enabled()", sock)
    @test occursin("_ensure_runner!()", sock)

    # The relaunch is a ~45 s cold start and `handle_message` runs INLINE on the WS receive loop, so it
    # must be spawned — blocking there stalls that client's whole stream, pings included, once per
    # image on a set. Asserted on the ordering, since the bug is a missing `Threads.@spawn`.
    let disp = sock[findfirst("st === :unavailable && _runner_enabled()", sock)[1]:end]
        i_spawn  = findfirst("Threads.@spawn", disp)
        i_ensure = findfirst("_ensure_runner!()", disp)
        @test i_spawn !== nothing && i_ensure !== nothing
        @test i_spawn[1] < i_ensure[1]        # spawned FIRST, then the slow work inside it
    end

    # Concurrent submits must not each pay a cold start — pressing Run on a set fires one per image.
    @test occursin("_RUNNER_RELAUNCH_LOCK", src)
    @test occursin("lock(_RUNNER_RELAUNCH_LOCK)", src)
    # …and it re-checks liveness INSIDE the lock, or every queued submit relaunches in turn.
    let ens = src[findfirst("function _ensure_runner!()", src)[1]:end]
        ens = ens[1:findfirst("\nend", ens)[1]]
        @test length(collect(eachmatch(r"runner_alive\(_RUNNER\)", ens))) >= 2
        # A REPLACEMENT runner restarts its log `seq` at 0, so the read cursor has to go back with it —
        # exactly as `/api/runner/restart` does. Left stale, the new runner's whole startup is skipped,
        # and on this path that startup is the only place the reason for the death could show up.
        @test occursin("_RUNNER_LOG_SEQ[] = 0", ens)
        # …before the launch, not after: the runner starts talking as soon as it is up.
        @test findfirst("_RUNNER_LOG_SEQ[] = 0", ens)[1] < findfirst("runner_launch!", ens)[1]
    end

    # The death itself is announced. The subscriber loop swallowed the drop and retried forever, so
    # nothing anywhere said the runner had gone — not a log line, not a frame. It is a `@warn`, which
    # the server's BroadcastLogger tees to the browser as `server:log`.
    client = read(joinpath(API_TEST_DIR, "..", "..", "app", "src", "runner", "client.jl"), String)
    @test occursin("Task runner connection lost", client)
    # …ONCE, on a connected→gone transition. This loop also spins while a cold runner precompiles, and
    # warning per retry would bury the one event that matters under its own noise.
    @test occursin("connected = false", client) && occursin("if connected", client)
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
