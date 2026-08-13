# ── The API server's half of the detached task runner ─────────────────────────
#
# The runner (app/src/runner/) owns the pools and executes tasks in its OWN process, so restarting
# this server does not kill work in flight. This file is the seam: launch/adopt it, relay its event
# stream to browsers, and reconcile after a reconnect. See docs/todo/TASK_RUNNER_PLAN.md.
#
# **This server becomes a RELAY, not an origin.** The runner emits the same frame shapes this server
# already broadcasts (`task:log` / `task:progress` / `task:status` / `task:result`), so they go into
# the existing `ws_*` sinks unchanged — no new client-facing protocol, and `ws.ts`, the task store and
# the task console are untouched.

const _RUNNER      = Cecelia.RunnerHandle()
const _RUNNER_SUB  = Ref{Union{Task,Nothing}}(nothing)

# Off by default while the runner is being built out: with it off, tasks execute in-process exactly as
# before, so a bug here cannot cost anyone a run. Flip with CECELIA_RUNNER=1 (`pixi run dev-runner`).
# This is a BUILD-OUT switch, not a permanent setting — Decision 1 says there is one scheduler, and two
# places tasks can run means `gpu = 1` is silently `gpu = 2`. It comes out when Phase 1 is verified.
_runner_enabled() = lowercase(strip(get(ENV, "CECELIA_RUNNER", ""))) in ("1", "true", "yes", "on")

# ── Frame relay ───────────────────────────────────────────────────────────────

# One runner frame → this server's own sinks. Unknown types are ignored rather than logged: a newer
# runner emitting a frame this server has no handler for must not produce a warning per log line.
function _relay_runner_frame(f::Dict{String,Any})
    id = String(get(f, "taskId", ""))
    isempty(id) && return
    t = String(get(f, "type", ""))
    if t == "task:log"
        ws_log(nothing, id, String(get(f, "line", "")))
    elseif t == "task:progress"
        ws_progress(nothing, id, Float64(get(f, "progress", 0.0)))
    elseif t == "task:result"
        ws_result(nothing, id, String(get(f, "imageUid", "")), get(f, "meta", nothing))
    elseif t == "task:status"
        # The runner's timestamps are passed through, not re-derived — see `ws_status`.
        ws_status(nothing, id, String(get(f, "status", "")), String(get(f, "imageUid", ""));
                  image_uids  = String[String(u) for u in get(f, "imageUids", String[])],
                  fun         = String(get(f, "fun", "")),
                  started_at  = String(get(f, "startedAt", "")),
                  finished_at = String(get(f, "finishedAt", "")))
    end
end

"""
Re-sync with the runner after every (re)connect — including the first.

Not optional, and not merely an optimisation. The event stream is droppable and, more to the point,
a task that STARTED and FINISHED while this server was restarting left no trace on it at all. Without
this, the common case the runner exists for — restart the backend mid-run — leaves the browser's row
pinned at `running` forever, which is exactly the failure it was built to prevent.

Two questions, two sources, mirroring what the *frontend* already does against `/api/tasks` and
`/api/tasks/recent`: what is in flight (`runner_tasks`) and how everything else ended
(`runner_recent`). In-flight rows are re-announced as `running` so a fresh tab adopts them; terminal
rows replay through the same sink as a live frame, with the runner's own timestamps, so a replayed
outcome and a live one cannot disagree.
"""
function _reconcile_with_runner()
    try
        for t in Cecelia.runner_tasks(_RUNNER)
            String(get(t, "status", "")) == "running" || continue
            ws_status(nothing, String(get(t, "id", "")), "running", String(get(t, "image_uid", ""));
                      fun = String(get(t, "fun_name", "")), pool = String(get(t, "pool_name", "")),
                      started_at = String(get(t, "started_at", "")))
        end
    catch e
        @warn "Runner reconcile: could not read in-flight tasks" exception = e
    end
    try
        for r in Cecelia.runner_recent(_RUNNER)
            ws_status(nothing, String(get(r, "id", "")), String(get(r, "status", "")),
                      String(get(r, "image_uid", ""));
                      image_uids  = String[String(u) for u in get(r, "image_uids", String[])],
                      fun         = String(get(r, "fun_name", "")),
                      pool        = String(get(r, "pool_name", "")),
                      started_at  = String(get(r, "started_at", "")),
                      finished_at = String(get(r, "finished_at", "")))
        end
    catch e
        @warn "Runner reconcile: could not read recent outcomes" exception = e
    end
    nothing
end

"""
Start (or adopt) the runner and subscribe to it. Called once at server start-up.

Launching happens on its own task: a COLD runner pays Julia load + Cecelia precompilation, and the app
must not be unavailable for the length of that. A task launched before the runner answers still works
— `_submit_to_runner` falls back to in-process execution rather than failing.
"""
function _start_runner!()
    _runner_enabled() || return
    Threads.@spawn begin
        try
            Cecelia.runner_launch!(_RUNNER)
            _RUNNER_SUB[] = Cecelia.runner_subscribe!(_RUNNER, _relay_runner_frame;
                                                      on_reconnect = _reconcile_with_runner)
        catch e
            @error "Task runner failed to start — tasks will run in-process" exception = (e, catch_backtrace())
        end
    end
    nothing
end

"""
    _submit_to_runner(req) -> Bool

Hand a task to the runner. `false` means "not handled here" — the caller runs it in-process instead.

**Falling back rather than failing is deliberate.** A runner that is still precompiling, has been
stopped from the shell, or died is a normal state of the world; the user pressing Run should get their
task executed either way. The cost is that the run is then attached to this process and dies with it —
which is the status quo, not a regression.
"""
function _submit_to_runner(req)::Bool
    _runner_enabled() || return false
    try
        ok = get(Cecelia.runner_submit(_RUNNER, req), "ok", false)
        ok === true && return true
        @warn "Runner refused the task — running it in-process" task_id = req.task_id
        false
    catch e
        @warn "Runner unreachable — running the task in-process" task_id = req.task_id exception = e
        false
    end
end

# Cancel has to reach BOTH: the task may be in this process (fallback path, chains, background jobs) or
# in the runner. Both are no-ops when the id is unknown, so asking twice is free and asking only one is
# a cancel button that silently does nothing.
function _cancel_on_runner(task_id::AbstractString)
    _runner_enabled() || return
    try; Cecelia.runner_cancel(_RUNNER, task_id); catch; end
    nothing
end

# ── Status for the System panel ───────────────────────────────────────────────

"""
`GET /api/runner/status` → what the Settings → System row shows.

`commit` is the point, not decoration: this process deliberately survives your edits, so it keeps
running the code it started with. `stale` compares that to the backend's own commit — the only way to
tell "my fix isn't working" from "my fix isn't loaded" (Decision 5).
"""
function api_runner_status(::HTTP.Request)
    _runner_enabled() || return 200, JSON3.write((; enabled = false, running = false))
    id = Cecelia.runner_ping(_RUNNER)
    isnothing(id) && return 200, JSON3.write((;
        enabled = true, running = false, port = Cecelia.RUNNER_PORT))
    commit = String(get(id, "commit", ""))
    200, JSON3.write((;
        enabled  = true,
        running  = true,
        port     = Cecelia.RUNNER_PORT,
        pid      = get(id, "pid", 0),
        adopted  = _RUNNER.adopted,          # true = it outlived something; we did not start it
        commit   = commit,
        stale    = !isempty(commit) && !isempty(_GIT_COMMIT) && commit != _GIT_COMMIT,
        protocol = get(id, "protocol", 0),
        protocolMismatch = Int(get(id, "protocol", 0)) != Cecelia.RUNNER_PROTOCOL,
        uptimeSeconds = get(id, "uptimeSeconds", 0),
        busy     = !isempty(try; Cecelia.runner_tasks(_RUNNER); catch; []; end)))
end

# POST /api/runner/restart — drain-and-relaunch. REFUSES while work is in flight: the whole point of
# the runner is that it holds work the app does not, so a restart that discards it silently is the one
# thing this control must never do. `force` is the user saying they know.
function api_runner_restart(body_bytes::Vector{UInt8})
    _runner_enabled() || return 409, JSON3.write((; error = "The task runner is not enabled."))
    body  = try; JSON3.read(String(body_bytes), Dict{String,Any}); catch; Dict{String,Any}(); end
    force = get(body, "force", false) === true
    busy  = try; Cecelia.runner_tasks(_RUNNER); catch; []; end
    if !isempty(busy) && !force
        return 409, JSON3.write((; error = "$(length(busy)) task(s) still running on the runner.",
                                   busy = length(busy)))
    end
    Cecelia.runner_stop!(_RUNNER)
    sleep(0.5)
    Threads.@spawn try; Cecelia.runner_launch!(_RUNNER); catch e
        @error "Task runner failed to relaunch" exception = (e, catch_backtrace())
    end
    200, JSON3.write((; ok = true, message = "Restarting the task runner"))
end
