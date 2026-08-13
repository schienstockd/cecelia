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

RunnerHandle(; port::Int = RUNNER_PORT) = RunnerHandle(port, nothing, false)

_runner_url(h::RunnerHandle, path::AbstractString) = "http://127.0.0.1:$(h.port)$path"

# Short timeouts throughout: every call here is control traffic against a loopback process, so a slow
# reply means "not there" rather than "be patient". Execution is announced on the event stream, never
# awaited on a request.
function _runner_get(h::RunnerHandle, path::AbstractString; timeout::Real = 5)::Dict{String,Any}
    r = HTTP.get(_runner_url(h, path); request_timeout = timeout, retry = false, status_exception = true)
    JSON3.read(String(r.body), Dict{String,Any})
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
function runner_ping(h::RunnerHandle)::Union{Dict{String,Any},Nothing}
    try; _runner_get(h, "/ping"; timeout = 2); catch; nothing; end
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
    h.proc = run(Cmd(`$julia --project -t auto runner.jl`; dir = dirname(script), detach = true);
                 wait = false)
    h.adopted = false

    deadline = time() + wait_seconds
    while time() < deadline
        reply = runner_ping(h)
        if reply !== nothing
            Int(get(reply, "protocol", 0)) == RUNNER_PROTOCOL &&
                (@info("Task runner started", port = h.port, pid = get(reply, "pid", "?")); return h)
        end
        if !process_running(h.proc)
            @warn "Task runner exited immediately — is port $(h.port) already taken?"
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
runner_tasks(h::RunnerHandle)  = get(_runner_get(h, "/tasks"), "tasks", Any[])
runner_pools(h::RunnerHandle)  = get(_runner_get(h, "/pools"), "pools", Any[])
runner_recent(h::RunnerHandle; since::AbstractString = "") =
    get(_runner_get(h, "/tasks/recent?since=$(HTTP.escapeuri(since))"), "tasks", Any[])
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
        while true
            try
                HTTP.WebSockets.open("ws://127.0.0.1:$(h.port)/events";
                                     maxframesize = WS_MAX_FRAME_SIZE) do ws
                    backoff = 1.0
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
            sleep(backoff)
            backoff = min(backoff * 2, 15.0)     # a stopped runner must not become a hot loop
        end
    end
end
