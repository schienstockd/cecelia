# ── The detached task runner — client half ────────────────────────────────────
#
# The API server's side of the conversation with the runner process (`server.jl`). Sink-agnostic like
# the rest of the package: this talks to the runner and hands frames to a callback; wiring those frames
# to WebSocket clients is `api/src/`'s job. See docs/todo/TASK_RUNNER_PLAN.md.
#
# Mirrors `preview.jl`'s lifecycle deliberately — launch-then-ping, adopt-if-already-listening, refuse
# on a protocol mismatch — because "a resident process we talk to" already has one way to be done here.
#
# The ONE way it differs, and it is the entire feature: this child is expected to OUTLIVE us. It is
# launched detached (its own process group), so an API-server restart leaves it running with its work
# intact. That is why `_stop_children_for_exit` must not treat it like napari or the preview worker —
# see Decision 3.

mutable struct RunnerHandle
    port::Int
    proc::Union{Base.Process,Nothing}   # nothing when ADOPTED (we didn't start it) or not launched
    adopted::Bool
end

# Both halves read the SAME override, or they disagree about where the runner is: `api/runner.jl`
# binds `CECELIA_RUNNER_PORT`, so a client defaulting to the constant would ping a port nothing is on
# and silently fall back to in-process forever. It also makes a second, isolated backend+runner pair
# possible alongside a real one — which is how the restart-mid-run path gets exercised without
# stopping somebody's actual session.
runner_port_default()::Int = parse(Int, get(ENV, "CECELIA_RUNNER_PORT", string(RUNNER_PORT)))

RunnerHandle(; port::Int = runner_port_default()) = RunnerHandle(port, nothing, false)

_runner_url(h::RunnerHandle, path::AbstractString) = "http://127.0.0.1:$(h.port)$path"

# Short timeouts throughout: every call here is control traffic against a loopback process, so a slow
# reply means "not there" rather than "be patient". Execution is announced on the event stream, never
# awaited on a request.
function _runner_get(h::RunnerHandle, path::AbstractString; timeout::Real = 5)::Dict{String,Any}
    r = HTTP.get(_runner_url(h, path); request_timeout = timeout, retry = false, status_exception = true)
    JSON3.read(String(r.body), Dict{String,Any})
end

function _runner_get_array(h::RunnerHandle, path::AbstractString; timeout::Real = 5)::Vector{Any}
    r = HTTP.get(_runner_url(h, path); request_timeout = timeout, retry = false, status_exception = true)
    JSON3.read(String(r.body), Vector{Any})
end

function _runner_post(h::RunnerHandle, path::AbstractString, body::Dict; timeout::Real = 10)::Dict{String,Any}
    r = HTTP.post(_runner_url(h, path), ["Content-Type" => "application/json"], JSON3.write(body);
                  request_timeout = timeout, retry = false, status_exception = true)
    JSON3.read(String(r.body), Dict{String,Any})
end

"""
    runner_ping(h) -> Dict | nothing

Identity of whatever is listening on the port, or `nothing` if nothing answers. Note the distinction
this preserves: a reply with the WRONG protocol is still a reply — the caller decides whether to use
it — because reporting "nothing there" for a process that is very much there produces a relaunch loop
against a port that can never be bound.
"""
function runner_ping(h::RunnerHandle; timeout::Real = 2)::Union{Dict{String,Any},Nothing}
    try; _runner_get(h, "/ping"; timeout); catch; nothing; end
end

runner_alive(h::RunnerHandle)::Bool = runner_ping(h) !== nothing

"""
    runner_launch!(h; wait_seconds = 120) -> RunnerHandle

Ensure a usable runner is listening, and record whether it is ours or one we adopted.

**Adoption is the normal path, not the crash path** — unlike the preview worker. This process is meant
to outlive an API-server restart, so on nearly every start-up there is already one running with our
work in it. Killing and relaunching would destroy exactly what the runner exists to protect.

A protocol mismatch is therefore reported, not repaired: the running process may be mid-segmentation,
and only the user can decide that finishing it matters less than running current code (Settings →
System → Restart when idle, Decision 5). Throwing here would also be wrong — the app must start.

The wait is generous because a cold runner pays Julia load + Cecelia precompilation, which is slower
than the preview worker's ~18 s of Python imports and is the reason it is started WITH the app rather
than lazily on the first task.
"""
function runner_launch!(h::RunnerHandle; wait_seconds::Real = 120)::RunnerHandle
    existing = runner_ping(h)
    if existing !== nothing
        h.proc, h.adopted = nothing, true
        proto = Int(get(existing, "protocol", 0))
        proto == RUNNER_PROTOCOL ?
            @info("Task runner adopted", port = h.port, pid = get(existing, "pid", "?"),
                  commit = get(existing, "commit", "")) :
            @warn("Task runner on the port speaks a different protocol — it is running older code. " *
                  "Restart it from Settings → System when its work is done.",
                  port = h.port, its_protocol = proto, ours = RUNNER_PROTOCOL)
        return h
    end

    script = joinpath(dirname(dirname(dirname(@__DIR__))), "api", "runner.jl")
    isfile(script) || (@warn "Task runner script not found — not launching" script; return h)
    julia = Base.julia_cmd().exec[1]
    # `detach = true` gives it its OWN process group. That is the point: a Ctrl-C or a signal aimed at
    # the API server's group must not reach a process whose whole job is to outlive it. The deliberate
    # teardown routes reach it by port instead (`pixi run stop`, dev.jl's CHILD_PORTS, Quit).
    # Pass the port explicitly rather than relying on the child inheriting our env: the handle may have
    # been built with an explicit port, and a child that binds a different one than we then ping is a
    # launch that "succeeds" and never answers.
    #
    # ── Its output, and why the runner has TWO carriers ────────────────────────
    # stdio is INHERITED explicitly. It has to be said out loud, because the default for a
    # non-blocking `run` is not "inherit", it is DEVNULL (`spawn_opts_swallow`) — which is what this
    # line used to do, so everything the runner printed was discarded. Not merely absent from the
    # console: absent from the terminal too, for the one process most likely to be holding a running
    # segmentation.
    #
    # Inheriting is safe for a process built to outlive us, and this is the distinction that matters:
    # the fd belongs to the TERMINAL (or wherever the launcher's stdout points), not to this process,
    # so it stays valid after we exit — verified, a detached child's writes still land three seconds
    # after the parent is gone. What would be wrong is `spawn_logged`, which hands the child a pipe
    # THIS process reads: that becomes a broken pipe on exactly the restart the runner exists to
    # survive. Hence the split, and it is not a dev/prod switch — both halves are right everywhere:
    #
    #   raw stdout/stderr  → the calling terminal.  `println`, an unhandled `@spawn` task error, the
    #                        precompile chatter, and a segfault's dump — which the C runtime writes
    #                        from a DYING process, so a pipe read by that same process is the worst
    #                        possible destination for the one output you most need.
    #   `@info/@warn/@error` → `runner:log` on its event stream → the app console, with source/detail/
    #                        seq, gap-filled from the runner's own ring after a reconnect.
    h.proc = run(pipeline(Cmd(addenv(`$julia --project -t auto runner.jl`,
                                     "CECELIA_RUNNER_PORT" => string(h.port));
                              dir = dirname(script), detach = true);
                          stdout = stdout, stderr = stderr); wait = false)
    h.adopted = false

    deadline = time() + wait_seconds
    while time() < deadline
        reply = runner_ping(h)
        if reply !== nothing && Int(get(reply, "protocol", 0)) == RUNNER_PROTOCOL
            # WHOSE runner answered? Not necessarily ours. A runner that was already starting up when
            # we looked answers neither ping nor state file for the tens of seconds it spends in Julia
            # load + precompilation, so we may have launched a second one into a race we then lost —
            # and the reply is the incumbent's. Reporting "started" for a process we did not start sent
            # the reader hunting for a bug in the pid that died, which is what happened in practice.
            # The pid on the wire is the truth; compare it and say which of the two this was.
            their_pid = get(reply, "pid", nothing)
            ours      = try; Libc.getpid(h.proc); catch; nothing; end
            mine      = their_pid !== nothing && ours !== nothing && Int(their_pid) == Int(ours)
            h.adopted = !mine
            mine ?
                @info("Task runner started", port = h.port, pid = something(their_pid, "?")) :
                @info("Task runner adopted — one was already starting on this port, so ours stood down",
                      port = h.port, pid = something(their_pid, "?"),
                      ours = something(ours, "?"))
            return h
        end
        # `!process_running` no longer implies the port was taken: a runner that finds an incumbent now
        # exits cleanly and deliberately (see `runner_serve`). Loop once more so an incumbent that is
        # up gets adopted on the next pass rather than reported as a failure.
        if !process_running(h.proc)
            reply = runner_ping(h)
            if reply !== nothing
                h.adopted = true
                @info("Task runner adopted — ours stood down for the one already on this port",
                      port = h.port, pid = get(reply, "pid", "?"))
            else
                @warn "Task runner exited without binding, and nothing is answering" port = h.port
            end
            return h
        end
        sleep(0.5)
    end
    @warn "Task runner did not answer within $(wait_seconds)s" port = h.port
    h
end

"""
    runner_stop!(h)

Stop the runner. Deliberate teardown only — Quit and `pixi run stop`, never a restart (Decision 3).
Kills by port rather than by handle because the common case is a runner we ADOPTED and have no handle
for; `_kill_listeners_on_port` kills the tree, so a task's Python subprocess goes with it.
"""
function runner_stop!(h::RunnerHandle)
    try; _kill_listeners_on_port(h.port); catch e; @warn "Stopping the task runner failed" exception = e; end
    h.proc, h.adopted = nothing, false
    nothing
end

# ── Control ───────────────────────────────────────────────────────────────────

runner_submit(h::RunnerHandle, req::TaskRequest) = _runner_post(h, "/submit", task_request_dict(req))
runner_cancel(h::RunnerHandle, task_id::AbstractString) =
    _runner_post(h, "/cancel", Dict{String,Any}("taskId" => string(task_id)))
# Bare arrays now (the runner speaks the API server's rail API) — `_runner_get_array` rather than
# `_runner_get`, which is typed for an object.
runner_tasks(h::RunnerHandle)  = _runner_get_array(h, "/api/tasks")
# Chains. `runner_submit_chain` returns the runner's reply so a 409 (the run id is already executing
# there) is distinguishable from a transport failure — those need different answers, and treating a
# refusal as "unreachable" would run the same chain twice.
runner_submit_chain(h::RunnerHandle, req::ChainRequest) =
    _runner_post(h, "/submit-chain", chain_request_dict(req))
runner_cancel_chain(h::RunnerHandle, run_id::AbstractString) =
    _runner_post(h, "/cancel-chain", Dict{String,Any}("runId" => string(run_id)))
runner_chain_runs(h::RunnerHandle) = get(_runner_get(h, "/chains"), "runs", Any[])
runner_pools(h::RunnerHandle)  = _runner_get_array(h, "/api/pools")
runner_recent(h::RunnerHandle; since::AbstractString = "") =
    _runner_get_array(h, "/api/tasks/recent?since=$(HTTP.escapeuri(since))")
# The runner's own console records after `since` (a `seq` from its `LogRing`). The gap-fill half of
# `runner:log` — see `_emit_server_log` in runner/server.jl for why a ring is needed at all.
runner_logs(h::RunnerHandle; since::Integer = 0) =
    get(_runner_get(h, "/api/logs/recent?since=$(Int(since))"), "logs", Any[])
runner_set_pool_limit(h::RunnerHandle, name::AbstractString, limit::Integer) =
    _runner_post(h, "/pools/set", Dict{String,Any}("name" => string(name), "limit" => Int(limit)))

# ── Event stream ──────────────────────────────────────────────────────────────

"""
    runner_subscribe!(h, on_frame; on_reconnect = () -> nothing) -> Task

Consume the runner's event stream, calling `on_frame(::Dict{String,Any})` for each frame. Runs until
the returned task is interrupted; reconnects on its own, because the runner outliving us is normal and
so is us outliving a dropped connection.

`on_reconnect` fires after every successful (re)connect — including the first. That is the hook for
reconciliation, and it is not optional: frames are droppable, and a task that STARTED and FINISHED
while nobody was listening left no trace on this stream at all. The caller must fetch `runner_tasks`
(what is in flight) and `runner_recent` (how everything else ended) there, or a restart mid-run leaves
a row pinned at `running` forever — the same failure the outcome bank exists to prevent, one process
boundary further out.

Errors in `on_frame` are logged and swallowed: a malformed frame must not kill the subscription.
"""
function runner_subscribe!(h::RunnerHandle, on_frame::Function; on_reconnect::Function = () -> nothing)::Task
    Threads.@spawn begin
        backoff = 1.0
        # Was there a connection to lose? Only a connected→gone transition is worth saying out loud:
        # this loop also spins while a COLD runner precompiles (normal, expected, ~45 s of failures),
        # and warning on every retry would bury the one event that matters in its own noise.
        connected = false
        while true
            try
                HTTP.WebSockets.open("ws://127.0.0.1:$(h.port)/events";
                                     maxframesize = WS_MAX_FRAME_SIZE) do ws
                    backoff = 1.0
                    connected && @info "Task runner reconnected"
                    connected = true
                    try; Base.invokelatest(on_reconnect); catch e
                        @warn "Runner reconnect hook failed" exception = (e, catch_backtrace())
                    end
                    for raw in ws
                        try
                            Base.invokelatest(on_frame, JSON3.read(String(raw), Dict{String,Any}))
                        catch e
                            @warn "Runner frame handler failed" exception = (e, catch_backtrace())
                        end
                    end
                end
            catch e
                e isa InterruptException && rethrow()
            end
            # The runner dying used to be entirely silent here — the loop just retried forever while
            # the backend quietly ran everything in-process, so an hour of work could be attached to a
            # process the user was about to restart with nothing having said so. The backend's logger
            # broadcasts this to the browser console (`server:log`), so it is a real notification.
            if connected
                @warn "Task runner connection lost — reconnecting. Tasks started meanwhile run in \
                       this server and will not survive a restart."
                connected = false
            end
            sleep(backoff)
            backoff = min(backoff * 2, 15.0)     # a stopped runner must not become a hot loop
        end
    end
end
