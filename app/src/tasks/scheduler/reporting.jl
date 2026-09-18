# Read-only reporting for the API + chain cancellation registry. Joins the pool registry
# (`_POOLS`, from pools.jl) and task registry (`_TASKS`, from task_record.jl) OUTSIDE their
# individual locks — `pool_status`'s comment pins the invariant: never nest `_TASKS_LOCK`
# inside `_POOLS_LOCK`. `_json_writable`/`_publishable_params` is the whitelist that decides
# whether a submitted task's params publish as JSON or reads back as `nothing` (withholds
# Re-run rather than corrupting it — see the comment inline for the whole-snapshot rationale).
# `cancel_chain_run!` + `is_chain_cancelled` are the chain executor's between-node cancel gate.

# ── Chain cancellation registry ───────────────────────────────────────────────
# Keyed by run_id. Checked by run_chain's is_cancelled closure before each node.
const _CANCELLED_CHAINS      = Set{String}()
const _CANCELLED_CHAINS_LOCK = ReentrantLock()

"""Return all initialised resource pools as `(; name, limit)` named tuples."""
function list_pools()
    _POOLS_INIT[] || _pools_init!()
    lock(_POOLS_LOCK) do
        [(; name=p.name, limit=p.limit) for p in values(_POOLS)]
    end
end

"""
Snapshot of tasks currently known to the scheduler (queued or running) as named tuples.
Deregistered on completion, so this is a live view of in-flight work only — nothing terminal.
`status` is stringified for JSON. Mirrors `list_pools()`; read-only reporting, no control.

For how a task that has LEFT this snapshot ended, see `recent_tasks()` (`tasks/task_outcomes.jl`) —
this one cannot answer that, and inferring an outcome from a row's absence is how the task console
came to report every finished task as "outcome unseen".

`project_uid` says which project `image_uid` belongs to — one server serves every project under
`projects_dir()`, so an image uid on its own doesn't. Resolved at submit time; `""` for a task
registered without an image (there is none today).

`live_outputs` carries the stores the task is streaming into right now (usually empty — see
`live_outputs` in task.jl). This snapshot is therefore also the answer to "what can I watch while it
runs?", which is how the napari viewer offers a preview of a segmentation before `ccid.json` knows
about its output.

`queued_at`/`started_at` are ISO-8601 UTC (`TASK_TS_FORMAT`), `started_at` empty until a pool slot
admits the task. They make the snapshot answer "how long has this been going?" — which a client
otherwise has to guess from when it first saw the row, so a console or tab that attached mid-run could
only ever report a lower bound.

`params` are the ones the run was submitted with, so a client that did not launch the task can still
offer Re-run rather than withholding it (`utils/runningTasks.ts`). Same reasoning as the timestamps:
the alternative is a client guessing, and a guessed param set is a silently different run. `nothing`
(JSON `null`) when they can't be published — see `_publishable_params`.
"""
function list_tasks()
    lock(_TASKS_LOCK) do
        [(; id=rec.id, fun_name=rec.fun_name, pool_name=rec.pool_name,
           image_uid=rec.image_uid, project_uid=rec.project_uid, chain_run_id=rec.chain_run_id,
           chain_node_id=rec.chain_node_id,
           status=string(rec.status), queued_at=iso_utc(rec.queued_at),
           started_at=iso_utc(rec.started_at), live_outputs=rec.live_outputs,
           params=_publishable_params(rec.params))
         for rec in values(_TASKS)]
    end
end

# `nothing` unless EVERY value survives JSON — the whole snapshot is written in one `JSON3.write`
# (`/api/tasks`), so one unserialisable value would throw and take the endpoint down for every row:
# no adoption in the browser, no task-console reconcile, and a quit/export busy-check that reads idle.
# Params normally arrive parsed from JSON and are always fine; a REPL-dispatched task (`run_task` is
# documented as REPL-driveable) can put anything in the dict.
#
# All-or-nothing, deliberately: dropping just the offending key would publish a param set that LOOKS
# complete, and a client would then offer Re-run on it — a silently different run, which is exactly what
# publishing params is here to prevent. `nothing` reads as "unknown" and withholds the button, while an
# empty dict keeps meaning "this task takes no params".
# A WHITELIST of the JSON-native shapes, not a `try JSON3.write` probe — deliberately, because the probe
# does not fail where it needs to. JSON3 throws on a `Function`, but serialises a plain struct into an
# object (`Fake(1,"x")` → `{"a":1,"b":"x"}`), so a probe would PUBLISH that and a client would re-run on
# it. Anything whose JSON form isn't the value it came from must read as unknown, not as a param.
# The cost is the other direction, and the safe one: a serialisable type nobody whitelisted (a `Date`)
# withholds Re-run rather than corrupting it. Tuples are in because Julia code writes them naturally —
# a REPL-dispatched run is the only way a non-JSON value gets in here at all.
#
# (A PREDICATE — not to be confused with `_json_safe` in api/src/plotting_api.jl, which CONVERTS a
# payload by nulling non-finite floats. Different job, so a different name.)
_json_writable(v)::Bool =
    v isa AbstractString || v isa Symbol || v isa Real || v isa Bool || isnothing(v) ||
    (v isa NamedTuple && all(_json_writable, values(v))) ||           # → a JSON object, like a dict
    ((v isa AbstractVector || v isa Tuple) && all(_json_writable, v)) ||
    (v isa AbstractDict && all(p -> (p.first isa AbstractString || p.first isa Symbol) &&
                                    _json_writable(p.second), v))

function _publishable_params(params::Dict{String,Any})
    all(p -> _json_writable(p.second), params) ? params : nothing
end

"""
Per-pool live status for the UI: `limit` (configured slot budget), `running` (slots currently in
use = `in_flight`), and `queued` (submitted-but-not-yet-started jobs assigned to this pool). Joins
the pool registry with the task registry. The two snapshots are taken under their OWN locks and
merged outside both — never nest `_TASKS_LOCK` inside `_POOLS_LOCK`. Read-only reporting.
"""
function pool_status()
    _POOLS_INIT[] || _pools_init!()
    # pool budget + in-flight slots (authoritative running count) — under the pools lock
    pools = lock(_POOLS_LOCK) do
        [(; name=p.name, limit=p.limit, running=p.in_flight) for p in values(_POOLS)]
    end
    # per-pool queued count from the task registry — under its own lock
    queued = Dict{String,Int}()
    lock(_TASKS_LOCK) do
        for rec in values(_TASKS)
            rec.status === TASK_QUEUED || continue
            queued[rec.pool_name] = get(queued, rec.pool_name, 0) + 1
        end
    end
    [(; p.name, p.limit, p.running, queued=get(queued, p.name, 0)) for p in pools]
end

function cancel_chain_run!(run_id::String)
    # 1) Flag the run so the executor skips not-yet-started nodes (checked between nodes).
    lock(_CANCELLED_CHAINS_LOCK) do; push!(_CANCELLED_CHAINS, run_id); end
    # 2) Kill any of this run's tasks that are running RIGHT NOW — the between-node
    #    flag never fires while a node is mid-execution (e.g. a cellpose subprocess).
    #    Collect IDs under the lock, then cancel outside it (cancel_task! re-locks).
    ids = lock(_TASKS_LOCK) do
        [id for (id, rec) in _TASKS if rec.chain_run_id == run_id]
    end
    for id in ids
        cancel_task!(id)
    end
end

function is_chain_cancelled(run_id::String)::Bool
    lock(_CANCELLED_CHAINS_LOCK) do; run_id ∈ _CANCELLED_CHAINS; end
end
