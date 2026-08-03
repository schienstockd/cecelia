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
The one wire format for a task-rail timestamp: ISO-8601 **UTC**, millisecond precision. Every
timestamp the rail publishes (`GET /api/tasks`, `GET /api/tasks/recent`, `task:status`,
`chain:node:*`) is written with this, so a consumer has one thing to parse. `nothing` → `""`, which
means "not known", never epoch zero.
"""
const TASK_TS_FORMAT = dateformat"yyyy-mm-ddTHH:MM:SS.sssZ"
iso_utc(dt::DateTime)  = Dates.format(dt, TASK_TS_FORMAT)
iso_utc(::Nothing)     = ""

# ── When a unit of work started RUNNING ───────────────────────────────────────
#
# The other fact a client needs and cannot derive, kept here for the same reason as the terminal log: the
# scheduler's `TaskRecord` holds it exactly while the task is alive, but the record is deregistered the
# instant the task finishes — and the consumers that need a DURATION mostly ask after that. The chain
# bridge fires `node:done` only once `run_task` has returned (and deregistered); a dropped terminal frame
# is recovered from `recent_tasks` seconds or minutes later. Without a start that outlives the record,
# every client has to time tasks off when it first happened to SEE them, which is why the task console
# reported `≥1h13m` for a task it connected to mid-run and why the GUI's elapsed restarted at zero on a
# page reload.
#
# **Written at the rail's sinks and by the scheduler, first note wins.** The scheduler knows the instant
# precisely (`_set_status!(rec, :running)`), so it stamps; a producer with no `TaskRecord` at all —
# background jobs, batch movies, whose only announcement is a `running` frame at `ws_status` — is stamped
# by the sink. Same mechanism, two writers, no second source of truth: `task_started_at` is the only
# reader and it does not care which one got there first.
#
# An entry is dropped when the task's terminal outcome is banked (the outcome row carries the start from
# then on), so the map holds in-flight work only. `_STARTED_CAP` is the backstop for a producer that
# never announces an outcome — the same leak class as `_TASKS`, bounded rather than trusted.
const _STARTED_CAP   = 2_000
const _STARTED       = Dict{String,DateTime}()
const _STARTED_LOCK  = ReentrantLock()

"""
    note_task_started!(task_id, at = Dates.now(UTC)) -> DateTime

Record when a unit of work began running and return the start now **on record** — the first note wins, so
a repeat announcement (a second `running` frame, a status re-broadcast) reads back the original rather
than resetting the clock. A no-op for an empty id.

Callers should use the returned value rather than their own `at`, so every carrier publishes one number.
"""
function note_task_started!(task_id::AbstractString, at::DateTime = Dates.now(UTC))::DateTime
    isempty(task_id) && return at
    lock(_STARTED_LOCK) do
        id = String(task_id)
        haskey(_STARTED, id) && return _STARTED[id]
        # Backstop only: a producer that never announces a terminal status would otherwise accumulate
        # here forever. Evict the oldest starts, not the newest — a long-running task is exactly the one
        # whose elapsed matters most.
        if length(_STARTED) >= _STARTED_CAP
            for (k, _) in sort(collect(_STARTED), by = last)[1:(length(_STARTED) - _STARTED_CAP + 1)]
                delete!(_STARTED, k)
            end
        end
        _STARTED[id] = at
        at
    end
end

"""
    task_started_at(task_id) -> Union{DateTime,Nothing}

When the task began running, or `nothing` if it hasn't started, was never noted, or has already been
banked as finished (the outcome row carries its `started_at` from that point on — see `recent_tasks`).
"""
function task_started_at(task_id::AbstractString)
    isempty(task_id) && return nothing
    lock(_STARTED_LOCK) do; get(_STARTED, String(task_id), nothing); end
end

"""Forget a task's start — called when a fresh run REUSES the id (`task:restart`), so the new run is
timed from its own beginning instead of inheriting the previous one's."""
function forget_task_start!(task_id::AbstractString)
    isempty(task_id) || lock(_STARTED_LOCK) do; delete!(_STARTED, String(task_id)); end
    nothing
end

"""
    record_task_outcome!(task_id, status; image_uid, image_uids, fun, pool) -> row | nothing

Bank a terminal task-rail frame so a client that missed it can replay it. **A no-op for a non-terminal
status**, so the one call site can hand over every status frame without repeating the terminal test.

Returns the banked row (`nothing` for a non-terminal status) so the caller can publish the SAME
timestamps it stored — the live frame and the replayable row must not derive them separately, or a task
finishes at two different times depending on which carrier reached the client.

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
             # when it began running ("" if it never did, or nobody noted it) — the row is where the start
             # OUTLIVES the scheduler record, so a client that missed the live frame still gets a real
             # duration instead of timing from when it noticed the task was gone.
             started_at  = iso_utc(task_started_at(task_id)),
             # every image the unit touched — a set-scope task's full member list, which only ever
             # existed on this frame. A replayed frame without it under-invalidates: only the
             # representative image's plots refresh, not every member's.
             image_uids  = String[String(u) for u in image_uids],
             finished_at = iso_utc(Dates.now(UTC)))
    lock(_OUTCOMES_LOCK) do
        i = findfirst(r -> r.id == row.id, _OUTCOMES)
        isnothing(i) || deleteat!(_OUTCOMES, i)       # re-append so the log stays in time order
        push!(_OUTCOMES, row)
        length(_OUTCOMES) > _OUTCOME_CAP && deleteat!(_OUTCOMES, 1:(length(_OUTCOMES) - _OUTCOME_CAP))
    end
    # The row now carries the start, so the in-flight map doesn't need to. Dropped AFTER it's read into
    # the row, and last so a throw above can't lose the timing while the outcome is still unbanked.
    forget_task_start!(task_id)
    row
end

"""
    recent_tasks(; since = "")

The banked terminal frames, oldest → newest, as `(; id, status, fun_name, pool_name, image_uid,
started_at, image_uids, finished_at)`. Both timestamps are ISO-8601 UTC (`TASK_TS_FORMAT`); `started_at`
is `""` when the unit never ran or nobody noted its start, so a consumer must treat an empty string as
"unknown" and fall back to its own clock rather than showing a duration of decades. Read-only reporting,
like `list_tasks()`.

Pass a previous poll's newest `finished_at` as `since` to get just that poll's tail. The bound is
INCLUSIVE, so a poll always re-reads its own newest entry: two units finishing in the same millisecond
would otherwise let a poll landing between them drop the second forever. Consumers de-duplicate by task
id, which they must anyway — the same outcome also arrives over WS.
"""
function recent_tasks(; since::AbstractString = "")
    rows = lock(_OUTCOMES_LOCK) do; copy(_OUTCOMES); end
    isempty(since) ? rows : filter(r -> r.finished_at >= since, rows)
end
