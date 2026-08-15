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
#: How far we have read the runner's OWN log ring (its `seq`, not ours). The cursor is what turns a
#: reconnect into a gap-fill: a runner that kept working through a backend restart said things nobody
#: was listening to, and this is how those lines still arrive. Reset to 0 when the runner is relaunched
#: (a fresh runner starts its counter over, so a stale cursor would skip its whole startup).
const _RUNNER_LOG_SEQ = Ref(0)

# A user SETTING (`[runner].enabled`, Settings → System), overridable by CECELIA_RUNNER for dev/CI —
# see `runner_enabled` in config.jl for why it is not just the env var. Default off while the design
# has known gaps (no spool; chains and background jobs still in-process).
#
# Read through this alias rather than calling `Cecelia.runner_enabled()` directly at each site: it is
# consulted on the request path (submit, cancel, /api/tasks, /api/pools), and having one name here is
# what makes "where does this decision get made" answerable.
_runner_enabled() = Cecelia.runner_enabled()

# ── Frame relay ───────────────────────────────────────────────────────────────

# One runner frame → this server's own sinks. Unknown types are ignored rather than logged: a newer
# runner emitting a frame this server has no handler for must not produce a warning per log line.
function _relay_runner_frame(f::Dict{String,Any})
    t  = String(get(f, "type", ""))
    id = String(get(f, "taskId", ""))
    # The runner's own log records: NOT keyed by a task at all, so this has to come before the task-id
    # guard below. They go into THIS server's ring (which restamps `seq` — the browser follows one
    # counter, not two) and out as an ordinary `server:log` with `source = "runner"`, so the console
    # treats a runner line exactly like a napari or backend line.
    if t == "runner:log"
        _relay_runner_log(f)
        return
    end
    # A chain frame is keyed by run/node, not by a task id — a skipped or set-scope node legitimately
    # has none, so the task-frame guard must not drop it.
    (isempty(id) && !startswith(t, "chain:")) && return
    if t == "task:log"
        ws_log(nothing, id, String(get(f, "line", "")))
    elseif t == "task:progress"
        ws_progress(nothing, id, Float64(get(f, "progress", 0.0)))
    elseif t == "task:result"
        ws_result(nothing, id, String(get(f, "imageUid", "")), get(f, "meta", nothing))
    elseif startswith(t, "chain:")
        # Chain frames are already in their final client shape (`subscribe_chain_frames!` built them in
        # the runner, from the same builder this server uses in-process), so they are broadcast
        # VERBATIM. The one thing that must still happen here is the BANK: `/api/tasks/recent` is served
        # from this process, so an outcome banked only in the runner is unrecoverable for a browser that
        # missed the live frame. Banked with the RUNNER's timestamps, never re-derived.
        if t in ("chain:node:done", "chain:node:failed")
            record_task_outcome!(String(get(f, "taskId", "")),
                                 t == "chain:node:done" ? "done" : String(get(f, "status", "failed"));
                                 image_uid   = String(get(f, "imageUid", "")),
                                 fun         = String(get(f, "fn", "")),
                                 started_at  = String(get(f, "startedAt", "")),
                                 finished_at = String(get(f, "finishedAt", "")))
        end
        broadcast_ws(f)
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
One runner log record → this server's console ring.

`seq` is DROPPED rather than passed through: the runner counts in its own sequence and this server
restamps, so the browser follows exactly one counter and its gap detection stays meaningful. The
runner's value is kept as the read cursor instead. `ts` survives, so a line is ordered by when the
runner said it, not by when we heard it — which is the whole point for lines that arrive in a
reconnect backfill, minutes late and all at once.
"""
function _relay_runner_log(f::AbstractDict)
    _RUNNER_LOG_SEQ[] = max(_RUNNER_LOG_SEQ[], Int(get(f, "seq", 0)))
    rec = Dict{String,Any}(String(k) => v for (k, v) in f if String(k) ∉ ("type", "seq"))
    get!(rec, "source", Cecelia.LOG_SOURCE_RUNNER)
    _log_sink(rec)
    nothing
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
    # …and the third question, added with the log rail: what did it SAY while we were away. Same shape
    # as the two above — a cursor, a bounded ring, replay through the live sink — because a runner that
    # crashed a task during a backend restart explained itself to nobody otherwise.
    try
        for rec in Cecelia.runner_logs(_RUNNER; since = _RUNNER_LOG_SEQ[])
            _relay_runner_log(rec)
        end
    catch e
        @warn "Runner reconcile: could not read recent logs" exception = e
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
    _runner_enabled() || return 200, JSON3.write((;
        enabled = false, running = false, port = _RUNNER.port,
        settable = !haskey(ENV, "CECELIA_RUNNER")))
    id = Cecelia.runner_ping(_RUNNER)
    isnothing(id) && return 200, JSON3.write((;
        enabled = true, running = false, port = _RUNNER.port,
        settable = !haskey(ENV, "CECELIA_RUNNER")))
    commit = String(get(id, "commit", ""))
    200, JSON3.write((;
        enabled  = true,
        settable = !haskey(ENV, "CECELIA_RUNNER"),   # false = the env var is forcing it; the toggle would lie
        running  = true,
        port     = _RUNNER.port,
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
    # The replacement runner starts its log `seq` at 0, so a cursor left at the old one's high-water
    # mark would skip its entire startup — including whatever made the restart necessary.
    _RUNNER_LOG_SEQ[] = 0
    Threads.@spawn try; Cecelia.runner_launch!(_RUNNER); catch e
        @error "Task runner failed to relaunch" exception = (e, catch_backtrace())
    end
    200, JSON3.write((; ok = true, message = "Restarting the task runner"))
end

# POST /api/runner/enabled {enabled} — the Settings toggle. Persists `[runner].enabled`.
#
# Turning it ON starts the runner immediately; turning it OFF does NOT kill one that is busy. Those
# are deliberately asymmetric: "off" means new tasks run in-process, and killing a runner mid-cellpose
# because someone flipped a switch would destroy exactly what it exists to protect. The row keeps
# showing it until it drains, which is the truth.
function api_runner_set_enabled(body_bytes::Vector{UInt8})
    body = try; JSON3.read(String(body_bytes), Dict{String,Any}); catch
        return 400, JSON3.write((; error = "invalid JSON body")); end
    haskey(ENV, "CECELIA_RUNNER") && return 409, JSON3.write((;
        error = "CECELIA_RUNNER is set for this session — it overrides the setting."))
    want = get(body, "enabled", false) === true
    now  = Cecelia.set_runner_enabled!(want)
    now && _start_runner!()      # idempotent: adopts an already-running one rather than relaunching
    200, JSON3.write((; enabled = now,
                        message = now ? "Tasks will run in the task runner." :
                                        "New tasks will run in the backend."))
end

# ── Chains on the runner ──────────────────────────────────────────────────────

"""
    _submit_chain_to_runner(req) -> Symbol

`:accepted` (running there), `:refused` (the runner says that run id is already executing on it), or
`:unavailable` (no runner — the caller runs it here).

The three are kept apart deliberately. A refusal and a transport failure look the same from a `try`,
and treating a refusal as "unavailable" would start a SECOND execution of a run that is already going —
both writing the same `run.json`. That is the corruption the runner's claim exists to prevent, so the
client must not undo it by falling back.
"""
function _submit_chain_to_runner(req)::Symbol
    _runner_enabled() || return :unavailable
    try
        get(Cecelia.runner_submit_chain(_RUNNER, req), "ok", false) === true ? :accepted : :refused
    catch e
        # An HTTP 409 raises here (status_exception), and it is the one error that is NOT "no runner".
        occursin("409", sprint(showerror, e)) && return :refused
        @warn "Runner unreachable — running the chain in-process" exception = e
        :unavailable
    end
end

_cancel_chain_on_runner(run_id::AbstractString) =
    (_runner_enabled() && try; Cecelia.runner_cancel_chain(_RUNNER, run_id); catch; end; nothing)
