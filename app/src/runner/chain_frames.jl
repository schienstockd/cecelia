# ── Chain events → wire frames, in one place ──────────────────────────────────
#
# The four `chain:node:*` transitions a client sees — plus a node's `task:progress` telemetry — built
# once and emitted through a callback so the SAME
# builder serves both processes: the API server broadcasting to browsers (the in-process path), and the
# detached runner emitting to the API server (which relays them on). See
# docs/todo/TASK_RUNNER_PLAN.md.
#
# This was inline in `api/src/server.jl`. It is extracted rather than copied because of what it does
# besides shaping a dict: **it banks the terminal outcome** (`record_task_outcome!`), and it is one of
# only two carriers that do. A second copy in the runner would mean either two banks disagreeing about
# when a node ran, or — far likelier — one of them quietly not banking at all, which is how a chain
# node becomes permanently unrecoverable for a client that missed the live frame.

"""
    chain_event_task_id(p) -> String

The scheduler task a chain node ran as — the correlation handle a consumer uses to attribute the
node's real outcome. Read defensively, and a named function rather than a closure because the
degradation IS the contract: two real payloads have no usable id (a node with no task yet — skipped
before submission, or an INCREMENTAL node, which still bypasses `run_task` — carries `nothing`; a
hand-fired REPL or test event omits the field entirely). Either must become `""`. A frame builder that
throws here takes down chain telemetry for every connected client.
"""
chain_event_task_id(p)::String = something(get(p, :task_id, ""), "")

"""
    subscribe_chain_frames!(emit) -> Vector{Pair{String,Function}}

Subscribe to the chain event bus and call `emit(::Dict{String,Any})` with a ready-to-send frame for
every node transition. Returns the `(event => handler)` pairs so a caller can unsubscribe.

**Banking happens here, exactly once per terminal event.** A chain run emits no `task:status` frames at
all (`handle_chain_run` passes no `on_status_change` — by design, or every chain node would get a second
row in the Task Manager), so `ws_status` never sees a chain node and the bank in *that* sink cannot
cover them. This is the other carrier.

The banked row is also where the frame's timestamps come from. It is written FIRST, and writing it is
what drops the task's start from the in-flight timing map — so re-deriving `startedAt` afterwards would
publish `""` for every finished node. One derivation, two carriers, no disagreement about when a task
ran.
"""
function subscribe_chain_frames!(emit::Function)::Vector{Pair{String,Function}}
    ev_task_id = chain_event_task_id
    ev_times(row) = isnothing(row) ? ("", "") : (row.started_at, row.finished_at)

    base(p) = Dict{String,Any}(
        "runId"      => p.run_id,
        "chainName"  => p.chain_name,
        "projectUid" => p.project_uid,
        "imageUid"   => p.image_uid,
        "nodeId"     => p.node_id,
        "fn"         => p.fn,
        "taskId"     => ev_task_id(p))

    # Per-node PROGRESS, shaped as a `task:progress` frame so it lands on the row the task snapshot
    # already publishes for this node. No `chain:node:progress` type: a progress tick is not a node
    # transition, it is telemetry for a task that already exists, and the console/GUI already know how
    # to read `task:progress` by `taskId`. Emitting it here rather than wiring `on_progress` at each
    # call site is what stops the drift — this builder is the ONE place both processes share.
    #
    # A node with no task id is dropped rather than emitted with `""`: an empty id would create (or
    # worse, update) a blank row, which is the failure `task:log` handling already guards against.
    progress = function(p)
        tid = ev_task_id(p)
        isempty(tid) && return
        total = Int(get(p, :total, 0))
        n     = Int(get(p, :n, 0))
        emit(Dict{String,Any}(
            "type"     => "task:progress",
            "taskId"   => tid,
            "progress" => clamp(total > 0 ? n / total : 0.0, 0.0, 1.0)))
    end

    queued = function(p)
        f = base(p); f["type"] = "chain:node:queued"; f["params"] = p.params; emit(f)
    end
    running = function(p)
        tid = ev_task_id(p)
        f = base(p); f["type"] = "chain:node:running"; f["params"] = p.params
        # A set-scope node bypasses `run_task` and so has no `TaskRecord` — `note_task_started!` is
        # first-write-wins, so this can only ever fill in a start nothing more precise recorded.
        f["startedAt"] = isempty(tid) ? "" : iso_utc(note_task_started!(tid))
        emit(f)
    end
    done = function(p)
        started, finished = ev_times(record_task_outcome!(ev_task_id(p), "done";
                                                          image_uid = p.image_uid, fun = p.fn))
        f = base(p); f["type"] = "chain:node:done"; f["params"] = p.params; f["result"] = p.result
        f["startedAt"] = started; f["finishedAt"] = finished
        emit(f)
    end
    failed = function(p)
        # `status` here may be "skipped" — a node that never ran, and not a terminal TASK status, so
        # `record_task_outcome!` ignores it and the frame simply goes out without timing.
        started, finished = ev_times(record_task_outcome!(ev_task_id(p), p.status;
                                                          image_uid = p.image_uid, fun = p.fn))
        f = base(p); f["type"] = "chain:node:failed"; f["status"] = p.status
        f["startedAt"] = started; f["finishedAt"] = finished
        emit(f)
    end

    pairs = ["node:queued" => queued, "node:running" => running,
             "node:done" => done, "node:failed" => failed,
             "node:progress" => progress]
    for (ev, h) in pairs
        subscribe_chain_events!(ev, h)
    end
    pairs
end
