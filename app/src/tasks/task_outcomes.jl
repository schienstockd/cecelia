using Dates

# ── The task rail's terminal-frame replay log ───────────────────────────────────
#
# A unit of work on the WS task rail announces how it ENDED exactly once, in one frame — and that frame
# is droppable by design (per-client drop-on-full queue in `api/src/server.jl`; a half-open socket loses
# the rest outright). Nothing survived it: a scheduler task is deregistered the instant it finishes, so
# `list_tasks()` can only ever say what is in flight, and a background job was never in that registry at
# all. A client that missed the frame had no way to ever learn the outcome — the task console counted
# every such run as "finished, outcome unseen" and nine images that all succeeded read `0 done · 17
# ended`; the browser left the task pinned at `running` and skipped everything that hangs off completion.
#
# So terminal frames are kept: a bounded, replayable log read back over `GET /api/tasks/recent`. A
# consumer that missed the live frame reconstructs it from here — same shape, same handlers, no second
# code path (`api/task_console.jl`, `frontend/src/utils/taskReconcile.ts`).
#
# **Written at the rail's status SINKS, never by the producers.** A terminal outcome reaches a client
# through exactly two carriers, and both bank it:
#
#   `ws_status`                        (api/src/sockets.jl) → `task:status`
#   `node:done` / `node:failed` subs   (api/src/server.jl)  → `chain:node:*`
#
# Sinks rather than producers is what makes coverage automatic: scheduler tasks, background jobs
# (export/import/data patches, `pool="job"`) and batch movies (`pool="viewer"`) all reach `ws_status`, so
# a producer added later needs no extra thought. Banking in the scheduler's `_deregister_task!` instead
# (where this started) covers only *that* producer and leaves a dropped project-export frame stranding
# its row forever. And BOTH carriers are needed: a chain run passes no `on_status_change`, so a chain
# node never reaches `ws_status` at all — banking only there left every chain node unrecoverable.
# Two carriers, two banks, no more.
#
# (One consequence, and the right one: a REPL `run_task` banks nothing — it reaches no sink. There is no
# client to recover and no server to serve the route; this is a reporting aid for the rail, not task
# state. A Julia-side caller asking "how did that run end?" wants the return value, or the on-disk run
# log — not this.)
#
# NOT run history: fixed size, in memory, gone on restart. Durable per-image history is
# `append_run_log!` → `GET /api/tasks/history`, on disk and permanent.

const _OUTCOME_CAP    = 500
const _OUTCOMES       = Vector{NamedTuple}()      # oldest → newest, capped
const _OUTCOMES_LOCK  = ReentrantLock()

"""Statuses that END a unit of work on the task rail. Mirrors the scheduler's terminal set."""
const TASK_TERMINAL_STATUSES = ("done", "failed", "cancelled")

"""
    record_task_outcome!(task_id, status; image_uid, image_uids, fun, pool)

Bank a terminal task-rail frame so a client that missed it can replay it. **A no-op for a non-terminal
status**, so the one call site can hand over every status frame without repeating the terminal test.

One row per task id: a repeat lands on top of the existing entry rather than beside it. Both cases are
real — a cancel is announced twice (immediately from `on_status_change`, then again as the final status),
and `task:restart` reuses the task id for a genuinely new run whose outcome must supersede the old one.
"""
function record_task_outcome!(task_id::AbstractString, status;
                              image_uid::AbstractString = "",
                              image_uids::AbstractVector = String[],
                              fun::AbstractString = "",
                              pool::AbstractString = "")
    st = string(status)
    (st in TASK_TERMINAL_STATUSES && !isempty(task_id)) || return nothing
    row = (; id          = String(task_id),
             status      = st,
             fun_name    = String(fun),
             pool_name   = String(pool),
             image_uid   = String(image_uid),
             # every image the unit touched — a set-scope task's full member list, which only ever
             # existed on this frame. A replayed frame without it under-invalidates: only the
             # representative image's plots refresh, not every member's.
             image_uids  = String[String(u) for u in image_uids],
             finished_at = Dates.format(Dates.now(UTC), "yyyy-mm-ddTHH:MM:SS.sssZ"))
    lock(_OUTCOMES_LOCK) do
        i = findfirst(r -> r.id == row.id, _OUTCOMES)
        isnothing(i) || deleteat!(_OUTCOMES, i)       # re-append so the log stays in time order
        push!(_OUTCOMES, row)
        length(_OUTCOMES) > _OUTCOME_CAP && deleteat!(_OUTCOMES, 1:(length(_OUTCOMES) - _OUTCOME_CAP))
    end
    nothing
end

"""
    recent_tasks(; since = "")

The banked terminal frames, oldest → newest, as `(; id, status, fun_name, pool_name, image_uid,
image_uids, finished_at)`. `finished_at` is ISO-8601 UTC. Read-only reporting, like `list_tasks()`.

Pass a previous poll's newest `finished_at` as `since` to get just that poll's tail. The bound is
INCLUSIVE, so a poll always re-reads its own newest entry: two units finishing in the same millisecond
would otherwise let a poll landing between them drop the second forever. Consumers de-duplicate by task
id, which they must anyway — the same outcome also arrives over WS.
"""
function recent_tasks(; since::AbstractString = "")
    rows = lock(_OUTCOMES_LOCK) do; copy(_OUTCOMES); end
    isempty(since) ? rows : filter(r -> r.finished_at >= since, rows)
end
