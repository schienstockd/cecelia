# Cecelia task console — a read-only, GUI-less live view of scheduler tasks.
#
# Connects to the running API server's WebSocket (/ws) for the live
# task:* / chain:* event stream, and polls GET /api/tasks for the in-flight
# snapshot (which fills in fun_name / image / pool that the WS stream alone
# doesn't carry), GET /api/tasks/recent for how the tasks that left it ENDED
# (the counters come from there — WS is lossy, see refresh_recent!), plus
# GET /api/pools for live per-pool occupancy (limit vs running + queued).
# REPORTING ONLY — it never sends task:run / task:cancel /
# chain:* control messages, so it is safe to run alongside the GUI.
#
# Elapsed time comes from the SERVER — `started_at`/`queued_at` on the snapshot, `startedAt`/`finishedAt`
# on the frames — so a task that was already running when this console connected still shows its true
# elapsed. The console's own clock is only the fallback for a server too old to send them (or a producer
# whose start nobody noted), and a locally-clocked run whose start we didn't witness renders `≥`: a
# floor, not a reading.
#
#   pixi run console                 # live dashboard (default; needs a TTY)
#   pixi run console -- --stream     # append-only event log (pipe/tee friendly)
#
# Or directly:  julia --project=api api/task_console.jl [--stream]
#
# Honours CECELIA_HOST / CECELIA_PORT (same defaults as the server: 127.0.0.1:8080).

using HTTP, JSON3, Dates, Printf

const HOST      = get(ENV, "CECELIA_HOST", "127.0.0.1")
const PORT      = get(ENV, "CECELIA_PORT", "8080")
const HTTP_BASE = "http://$HOST:$PORT"
const WS_URL    = "ws://$HOST:$PORT/ws"

# Append-only mode when asked, or automatically when stdout isn't a terminal
# (so `pixi run console | tee run.log` produces a clean, un-escaped log).
const STREAM_MODE = ("--stream" in ARGS) || !(stdout isa Base.TTY)

# ── ANSI ──────────────────────────────────────────────────────────────────────
const RESET = "\e[0m"; const BOLD = "\e[1m"; const DIM = "\e[2m"
const RED = "\e[31m"; const GREEN = "\e[32m"; const YELLOW = "\e[33m"
const BLUE = "\e[34m"; const MAGENTA = "\e[35m"; const CYAN = "\e[36m"; const GREY = "\e[90m"

col(c, s) = STREAM_MODE ? s : string(c, s, RESET)

function status_colour(s::AbstractString)
    s == "running"   ? CYAN    :
    s == "queued"    ? YELLOW  :
    s == "done"      ? GREEN   :
    s == "failed"    ? RED     :
    s == "cancelled" ? MAGENTA : GREY
end

const TERMINAL = ("done", "failed", "cancelled")   # everything else is active (queued / running)

# ── State ───────────────────────────────────────────────────────────────────────
mutable struct TaskView
    id::String
    fun_name::String
    image_uid::String
    # Which project the image belongs to (`project_uid` on the /api/tasks snapshot). The console
    # watches the WHOLE server, which serves every project under `projects_dir()`, so an image uid on
    # its own doesn't say whose row this is. Snapshot-only: a WS-only producer (job, batch movie)
    # names no project, and those rows render it blank rather than guessing.
    project_uid::String
    pool_name::String
    chain_run_id::String
    status::String
    progress::Float64      # -1 = unknown / not reported yet
    last_log::String
    updated::DateTime
    # ── Elapsed clock (see `_set_phase!`) ─────────────────────────────────────────
    # `since` is when the row entered its CURRENT status (UTC, like the rail), so the clock reads as queue
    # wait while queued and as run time while running. Normally the SERVER's own timestamp — the scheduler
    # stamps `queued_at`/`started_at` on the record and publishes them on the snapshot and the status
    # frames — in which case `exact` is true. `exact` false means the console had to time the row itself
    # (an older server, or a producer whose start nobody noted) *and* did not witness the transition, so
    # the elapsed is a floor and renders `≥` rather than posing as a measurement — the same rule as
    # counting "ended" instead of guessing done/failed. `phase_set` distinguishes "status asserted" from
    # the default `"queued"` a fresh row carries before any frame has said anything about it.
    since::DateTime
    exact::Bool
    phase_set::Bool
    # Reconciliation state (see refresh_snapshot!). `in_snapshot` marks a task the /api/tasks
    # snapshot has listed at least once — i.e. a real SCHEDULER task, so its absence from a later
    # snapshot is meaningful. WS-only producers (jobs, batch movies) never appear in the snapshot
    # and must never be pruned by it — they identify themselves with fun + pool on their status
    # frames, which is also what tells them apart from an unattributed row (`_unattributed`, which
    # IS prunable). `misses` counts consecutive snapshots it went missing from.
    in_snapshot::Bool
    misses::Int
end

const TASKS      = Dict{String,TaskView}()   # taskId => view — ACTIVE tasks only (finished ones drop out)
const EVENTS     = String[]                   # ring buffer of rendered ACTIVITY lines (status/chain/result)
const LOGS       = String[]                   # ring buffer of task LOG lines — shown in their own pane
const LOCK       = ReentrantLock()
const MAX_EVENTS = 500
const MAX_LOGS   = 500
# Finished tasks are collapsed to a COUNT, not kept as rows — the console answers "what's running now
# and how many are done", not "show all 50". TALLY holds cumulative terminal outcomes; SEEN_TERM stops
# a task being re-counted / re-added (by a late WS event or the snapshot poll) once it has finished.
# "ended" = finished, outcome unseen: the task left the scheduler snapshot, its terminal task:status
# frame never arrived (WS telemetry is lossy by design — see broadcast_ws) AND the server's outcome
# log couldn't name it either (`refresh_recent!`). Counted separately rather than guessed as
# done/failed, so the console never claims an outcome it didn't see. It should now be RARE — before
# the outcome poll existed, one dropped frame per task made it the only number that ever moved.
const TALLY      = Dict{String,Int}("done" => 0, "failed" => 0, "cancelled" => 0, "ended" => 0)
const SEEN_TERM  = Set{String}()

# Live resource-pool occupancy (polled from GET /api/pools): per pool, its configured concurrency
# `limit`, how many slots are `running` now, and how many tasks are `queued` for it. Rendered as a
# small panel so you can see at a glance how much of each pool (cpu/gpu/io/network) is in use.
mutable struct PoolView
    name::String
    limit::Int
    running::Int
    queued::Int
end
const POOLS      = PoolView[]
const POOL_ORDER = Dict("cpu" => 1, "gpu" => 2, "io" => 3, "network" => 4)   # canonical display order
_poolsort(p) = (get(POOL_ORDER, p.name, 99), p.name)

now_hms() = Dates.format(Dates.now(), "HH:MM:SS")
# Display only the id TAIL — task ids are full UUIDs; 6 chars is plenty to eyeball-correlate a handful
# of concurrent tasks (and matches the backend's 6-char gen_uid object ids). The real id is untouched.
short(id::AbstractString, n::Int=6) = length(id) <= n ? String(id) : String(last(id, n))
trunc_s(s::AbstractString, n::Int) = length(s) <= n ? String(s) : String(first(s, max(0, n - 1))) * "…"

# Elapsed as a compact string: 42s · 4m 12s · 1h 30m. Same spelling as the GUI's
# `formatTaskDuration` (`frontend/src/utils/taskElapsed.ts`) so one duration doesn't read two ways
# depending on where you look at it — never wider than 8 chars including the marker.
# `exact=false` prefixes `≥`: the console had to time this itself and only started watching part-way in,
# so the number is a floor, not a reading.
function dur_str(ms::Real; exact::Bool = true)
    s = max(0, round(Int, ms / 1000))
    body = s < 60   ? string(s, "s") :
           s < 3600 ? string(s ÷ 60, "m ", lpad(s % 60, 2, '0'), "s") :
                      string(s ÷ 3600, "h ", lpad((s % 3600) ÷ 60, 2, '0'), "m")
    (exact ? "" : "≥") * body
end
# Everything is compared in UTC, because the rail's timestamps are UTC.
dur_since(t::TaskView) = dur_str(Dates.value(Dates.now(UTC) - t.since); exact = t.exact)

# The rail's timestamp format. A copy of `TASK_TS_FORMAT` (`app/src/tasks/task_outcomes.jl`) — this
# script deliberately depends on nothing but HTTP/JSON3/Dates, so it can't import the constant; that file
# is where the format is decided.
const TS_FORMAT = dateformat"yyyy-mm-ddTHH:MM:SS.sssZ"

# Parse a timestamp off the wire. `""` means the server does not know (an older server, or a task that
# never started) — answered as `nothing` so the caller falls back to the console's own clock. A malformed
# value is treated the same way rather than taking the reader down over a field it can live without.
function _ts(v)::Union{DateTime, Nothing}
    s = v isa AbstractString ? String(v) : ""
    isempty(s) && return nothing
    try; DateTime(s, TS_FORMAT); catch; nothing; end
end

# Get-or-create a task view, so a WS event for a not-yet-snapshotted task still shows up.
function _task!(id::AbstractString)
    get!(TASKS, String(id)) do
        TaskView(String(id), "", "", "", "", "", "queued", -1.0, "", Dates.now(),
                 Dates.now(UTC), false, false, false, 0)
    end
end

# The ONE place a row's status changes — because the elapsed clock restarts with it.
#
# `at` is the server's own timestamp for this transition (`started_at`/`queued_at` on the snapshot,
# `startedAt` on a live frame). When present it simply wins, changed status or not: it is the real
# instant, so a row the console had been timing itself is UPGRADED to exact the first time the rail
# supplies one. Only when there is none does the console clock the row, and then only on a real *change* —
# the snapshot re-asserts the same status every 2s, and resetting per poll would peg every clock at "0s".
# `witnessed` then says whether that local clock is a measurement (a live WS frame — the transition is
# happening now) or a floor (the HTTP snapshot, which says a task IS running, not that it just started).
function _set_phase!(t::TaskView, status::AbstractString; witnessed::Bool = false, at = nothing)
    isempty(status) && return t
    changed = !t.phase_set || status != t.status
    if !isnothing(at)
        t.since = at
        t.exact = true
    elseif changed
        t.since = Dates.now(UTC)
        t.exact = witnessed
    end
    t.phase_set = true
    t.status    = status
    t
end

function push_event!(kind::AbstractString, detail::AbstractString; colour=nothing)
    c = colour === nothing ? GREY : colour
    line = string(col(GREY, now_hms()), "  ", col(c, rpad(kind, 12)), "  ", detail)
    push!(EVENTS, line)
    length(EVENTS) > MAX_EVENTS && deleteat!(EVENTS, 1:(length(EVENTS) - MAX_EVENTS))
    STREAM_MODE && println(line)
end

# Task log lines go in their OWN buffer, rendered in a separate confined pane (they're high-volume and
# would otherwise drown the activity stream). Prefixed with the short task id for context.
function push_log!(id::AbstractString, line::AbstractString)
    entry = string(col(GREY, now_hms()), "  ", col(DIM, short(id)), "  ", line)
    push!(LOGS, entry)
    length(LOGS) > MAX_LOGS && deleteat!(LOGS, 1:(length(LOGS) - MAX_LOGS))
    STREAM_MODE && println(entry)
end

# A task's first terminal sighting: bump its outcome tally and drop the row (kept only as a count).
#
# One exception to "first sighting wins": a task retired as `ended` (finished, outcome unseen) whose
# real outcome shows up LATER — over HTTP from `GET /api/tasks/recent`, or from a terminal frame that
# arrived after the snapshot had already dropped the row. Moving the count is strictly better than
# keeping a number we know to be wrong, and it can't double-count: the id leaves ENDED_IDS as it's
# corrected, and every other repeat sighting still returns early.
const ENDED_IDS = Set{String}()   # ids currently counted as "ended" — correctable, unlike a real outcome

# How long the task ran, for the line announcing its outcome — the only place a finished task's elapsed
# can be reported, since it is collapsed to a count and its row is dropped.
#
# Two sources, server first: `started`/`finished` are the frame's (or outcome row's) own timestamps and
# give the exact duration — including for a task this console never held a row for, which is the whole
# point of the outcome poll. Without them it falls back to the row's own clock, which must therefore be
# read BEFORE `_note_terminal!` drops the row. Empty when neither can answer, and when the task was never
# running: a task cancelled from the queue never ran, and a queue wait is not a run time.
function _ran_for(id::AbstractString; started = nothing, finished = nothing)
    s     = _ts(started)
    exact = true
    if isnothing(s)
        t = get(TASKS, String(id), nothing)
        (t === nothing || t.status != "running") && return ""
        s, exact = t.since, t.exact
    end
    f = something(_ts(finished), Dates.now(UTC))
    string(" ", col(DIM, "in $(dur_str(Dates.value(f - s); exact = exact))"))
end

# Returns what it did, so a caller can report an outcome the live stream never delivered:
#   :counted   — first terminal sighting, tallied
#   :corrected — was tallied "ended", now moved to its real outcome
#   :ignored   — a repeat sighting of an outcome already counted
function _note_terminal!(id::AbstractString, status::AbstractString)::Symbol
    outcome = :counted
    if id in SEEN_TERM
        (id in ENDED_IDS && status != "ended" && haskey(TALLY, status)) || return :ignored
        TALLY["ended"] -= 1                       # …and fall through to count the real outcome
        delete!(ENDED_IDS, String(id))
        outcome = :corrected
    else
        push!(SEEN_TERM, String(id))
        status == "ended" && push!(ENDED_IDS, String(id))
    end
    haskey(TALLY, status) && (TALLY[status] += 1)
    delete!(TASKS, id)
    outcome
end

# Mark a task finished-and-counted WITHOUT tallying it — for outcomes that predate this console
# session (see `refresh_recent!`'s prime pass). Keeps them out of the counters while still
# suppressing a later re-count.
_seen_only!(id::AbstractString) = push!(SEEN_TERM, String(id))

# ── HTTP snapshot (fills in fun_name / image / pool for in-flight tasks) ─────────
# The snapshot is AUTHORITATIVE and complete: /api/tasks returns the scheduler's whole in-flight set
# under its lock, and a task is deregistered the moment it finishes. So reconciliation runs BOTH ways —
# rows are added/updated from the snapshot AND retired when they vanish from it. The retire half is what
# makes WS telemetry genuinely lossy-safe: frames are dropped by design for a slow client (per-client
# drop-on-full queue in server.jl) and lost outright on a half-open socket, and without this a missed
# terminal frame stranded the row as "running" forever — the scheduler idle, the console still listing it.
#
# Only tasks the snapshot has ALREADY listed (`in_snapshot`) are eligible: jobs and batch movies are
# WS-only producers that never appear there, and pruning them would delete every row they own.
# Two consecutive misses are required so a task registered between the poll and its first WS frame
# is never retired on a one-poll race.
const SNAPSHOT_MISSES_TO_RETIRE = 2

# …with ONE addition: an UNATTRIBUTED row is eligible too. Every `task:status` frame in the codebase
# carries `fun` (and non-scheduler producers also carry `pool`), so a row with neither has only ever
# seen `task:log` / `task:progress` frames — it has no identity, and nothing to display but its id.
# Left ineligible it is immortal: absent from the scheduler (so no snapshot can set `in_snapshot`) yet
# never retired, it sits in the table as "queued / waiting" forever while every pool reads idle.
# It cannot be a live job or batch movie — those announce themselves with fun + pool — and it cannot
# be a live scheduler task, because that would be IN the snapshot and so never miss a poll.
# Dropped silently rather than tallied: we never saw what it was, so "ended" would be a claim about a
# task we can't even name. Nor is it added to SEEN_TERM — if frames keep coming the row simply comes
# back and is dropped again, which is self-correcting; SEEN_TERM would suppress it permanently.
_unattributed(t::TaskView) = isempty(t.fun_name) && isempty(t.pool_name)

# Pure half — takes already-parsed rows, touches no socket, so `api/test/runtests.jl` can drive it
# with synthetic snapshots (that's the only automated coverage this script can have: it's run by path,
# never imported, so the entrypoint at the bottom is guarded to keep `include`ing it side-effect-free).
function _reconcile_snapshot!(rows)
    lock(LOCK) do
        present = Set{String}()
        for row in rows
            id = String(row.id)
            push!(present, id)
            status = String(get(row, :status, ""))
            if status in TERMINAL                       # finished before we saw it live → just count it
                _note_terminal!(id, status)             # (a repeat sighting is ignored in there)
                continue
            end
            id in SEEN_TERM && continue                 # already finished + counted — don't resurrect
            t = _task!(id)
            t.fun_name     = String(get(row, :fun_name, t.fun_name))
            t.image_uid    = String(get(row, :image_uid, t.image_uid))
            t.project_uid  = String(get(row, :project_uid, t.project_uid))
            t.pool_name    = String(get(row, :pool_name, t.pool_name))
            t.chain_run_id = String(get(row, :chain_run_id, t.chain_run_id))
            # The snapshot carries the scheduler's own timestamps, so elapsed is exact even for a task
            # that was already running when this console connected — which is the case the console used
            # to have to report as a floor. `queued_at` does the same for the queue wait. Falls back to
            # "clock it locally" against a server too old to send them.
            _set_phase!(t, status;
                        at = _ts(get(row, status == "running" ? :started_at : :queued_at, "")))
            t.in_snapshot  = true
            t.misses       = 0
        end
        # retire what the scheduler no longer knows about: a task it once listed is tallied as "ended"
        # (finished, outcome unseen); an unattributed row is dropped without a tally (see above).
        for (id, t) in collect(TASKS)
            ((t.in_snapshot || _unattributed(t)) && !(id in present)) || continue
            t.misses += 1
            t.misses >= SNAPSHOT_MISSES_TO_RETIRE || continue
            if t.in_snapshot
                ran = _ran_for(id)                 # before the row is dropped
                _note_terminal!(id, "ended")
                push_event!("status", string(col(BOLD, short(id)), " ", col(GREY, "ended"), ran,
                            col(DIM, " (outcome unseen — dropped frame)")); colour = GREY)
            else
                delete!(TASKS, id)
                push_event!("status", string(col(BOLD, short(id)), " ", col(GREY, "dropped"),
                            col(DIM, " (unattributed — logs only, never in the scheduler)"));
                            colour = GREY)
            end
        end
    end
    nothing
end

function refresh_snapshot!()
    try
        r = HTTP.get("$HTTP_BASE/api/tasks"; connect_timeout=2, readtimeout=3, retry=false)
        _reconcile_snapshot!(JSON3.read(String(r.body)))
        return true
    catch
        return false   # server not up yet / transient — the caller shows connection state
    end
end

# ── Outcome poll (GET /api/tasks/recent → how the tasks that left the snapshot ended) ─────────────
# The done/failed/cancelled TALLY must not depend on the WS stream: the terminal `task:status` frame
# is dropped for a slow client by design (per-client drop-on-full queue in server.jl) and lost outright
# on a half-open socket — and it is the ONE frame per task that carries the outcome, so losing it made
# a whole successful batch read "0 done · N ended". This poll is the lossy-safe channel for it: the
# server keeps a bounded log of terminal frames (`recent_tasks`), so a missed frame costs at most
# a couple of seconds of latency, not the count. Banked from `ws_status` — the rail's one status sink — so
# it covers background jobs and batch movies too, not just scheduler tasks.
#
# WS still drives the live feel (an outcome shows the instant it happens); this is the backstop that
# makes the numbers true, and `_note_terminal!` de-duplicates whichever arrives second.
const RECENT_SINCE = Ref("")   # newest finished_at seen — polls ask only for the tail after it

# Pure half — same split as `_reconcile_snapshot!`, so the tests can drive it with synthetic rows.
#
# `prime` = the first pass after connecting: the ring holds tasks that finished BEFORE this console
# existed. Counting those would credit the session with work it never watched, so they're recorded as
# seen-and-counted without a tally — the counters stay "since you started looking".
function _apply_recent!(rows; prime::Bool = false)
    lock(LOCK) do
        for row in rows
            id = String(get(row, :id, "")); isempty(id) && continue
            ts = String(get(row, :finished_at, ""))
            ts > RECENT_SINCE[] && (RECENT_SINCE[] = ts)
            prime && (_seen_only!(id); continue)
            status = String(get(row, :status, ""))
            # The banked row carries both ends, so a recovered outcome reports the SAME duration the live
            # frame would have — even for a task this console never had a row for.
            ran = _ran_for(id; started = get(row, :started_at, ""), finished = ts)
            # Report only what the live stream never delivered — a frame that already arrived is
            # ignored here and was announced when it landed.
            if _note_terminal!(id, status) != :ignored
                push_event!("status", string(col(BOLD, short(id)), " ",
                            col(status_colour(status), status), ran,
                            col(DIM, " (outcome poll)"));
                            colour = status_colour(status))
            end
        end
    end
    nothing
end

function refresh_recent!(; prime::Bool = false)
    try
        since = HTTP.escapeuri(RECENT_SINCE[])
        r = HTTP.get("$HTTP_BASE/api/tasks/recent?since=$since";
                     connect_timeout=2, readtimeout=3, retry=false, status_exception=false)
        # An older server has no such route — degrade to WS-only counting rather than erroring.
        r.status == 200 || return false
        _apply_recent!(JSON3.read(String(r.body)); prime = prime)
        return true
    catch
        return false
    end
end

# ── Pool occupancy snapshot (GET /api/pools → limit + running + queued per pool) ──
function refresh_pools!()
    try
        r = HTTP.get("$HTTP_BASE/api/pools"; connect_timeout=2, readtimeout=3, retry=false)
        rows = JSON3.read(String(r.body))
        lock(LOCK) do
            empty!(POOLS)
            for row in rows
                push!(POOLS, PoolView(String(get(row, :name, "")), Int(get(row, :limit, 0)),
                                      Int(get(row, :running, 0)), Int(get(row, :queued, 0))))
            end
        end
        return true
    catch
        return false
    end
end

# ── WS message handling ─────────────────────────────────────────────────────────
function handle_ws(raw::AbstractString)
    msg  = JSON3.read(raw)
    type = String(get(msg, :type, ""))
    lock(LOCK) do
        if type == "task:status"
            id = String(get(msg, :taskId, "")); isempty(id) && return
            status = String(get(msg, :status, ""))
            # Already finished and counted? Then this is a repeat sighting — with ONE exception, which
            # is why the check can't just `return` here as it used to: a task the snapshot retired as
            # "ended" whose real terminal frame turns up afterwards. Dropping that frame is what made
            # the ended→real-outcome correction in `_note_terminal!` unreachable from this path, so a
            # late `done` was discarded and the run stayed counted as "outcome unseen" forever.
            if id in SEEN_TERM
                status in TERMINAL && _note_terminal!(id, status) === :corrected &&
                    push_event!("status", string(col(BOLD, short(id)), " ",
                                col(status_colour(status), status), col(DIM, " (late frame)"));
                                colour = status_colour(status))
                return
            end
            # Non-scheduler producers (batch movies, jobs) carry fun/pool on the event itself — they
            # never hit the /api/tasks snapshot, so without this their rows show a blank function AND a
            # blank pool ("floating in space"). Prefer the event's values; fall back to the snapshot's.
            ev_fun  = String(get(msg, :fun,  ""))
            ev_pool = String(get(msg, :pool, ""))
            fn = !isempty(ev_fun) ? ev_fun : (haskey(TASKS, id) ? TASKS[id].fun_name : "")
            ev_started  = get(msg, :startedAt,  "")       # the rail's own timestamps (empty on an
            ev_finished = get(msg, :finishedAt, "")       # older server → the local clock is used)
            push_event!("status", string(col(BOLD, short(id)), " ",
                        col(status_colour(status), status),
                        # still the running row at this point, so the local fallback can read it
                        status in TERMINAL ? _ran_for(id; started = ev_started, finished = ev_finished) : "",
                        isempty(fn) ? "" : col(DIM, " ($fn)"));
                        colour = status_colour(status))
            if status in TERMINAL
                _note_terminal!(id, status)            # collapse to a count, drop the row
            else
                t = _task!(id)
                _set_phase!(t, status; witnessed = true, at = _ts(ev_started))
                isempty(ev_fun)  || (t.fun_name = ev_fun)     # label WS-only ops (else blank FUNCTION)
                isempty(ev_pool) || (t.pool_name = ev_pool)   # …and their POOL (viewer / job)
                uid = String(get(msg, :imageUid, "")); isempty(uid) || (t.image_uid = uid)
                t.updated = Dates.now()
            end

        elseif type == "task:progress"
            id = String(get(msg, :taskId, "")); (isempty(id) || id in SEEN_TERM) && return
            t = _task!(id)
            t.progress = clamp(Float64(get(msg, :progress, 0.0)), 0.0, 1.0)
            t.updated  = Dates.now()

        elseif type == "task:log"
            id = String(get(msg, :taskId, "")); isempty(id) && return
            line = String(get(msg, :line, ""))
            # Always SHOW the line — but never let it (re)create a row for a task that has already
            # finished. Post-mortem log frames are normal: cancel_task! kills the subprocess and the
            # terminal frame goes out at once, then the process reader flushes whatever was still in
            # the pipe. A log frame carries no fun / pool / status, so `_task!` minted a blank row
            # stuck at the default "queued" — and nothing could ever retire it: the scheduler had
            # already deregistered the task, so it never came back in a snapshot to set `in_snapshot`.
            # That was the zombie queued row (six of them after seven cancels, every pool idle).
            id in SEEN_TERM || let t = _task!(id)
                t.last_log = line
                t.updated  = Dates.now()
            end
            push_log!(id, line)

        elseif type == "task:result"
            id = String(get(msg, :taskId, ""))
            push_event!("result", string(col(BOLD, short(id)), " result ready"); colour = GREEN)

        elseif startswith(type, "chain:node:")
            node   = replace(type, "chain:node:" => "")
            fn     = String(get(msg, :fn, ""))
            run_id = String(get(msg, :runId, ""))
            img    = String(get(msg, :imageUid, ""))
            # A chain run emits NO task:status frames (handle_chain_run passes no on_status_change), so
            # a chain node's row would otherwise only ever leave the table via the snapshot-retire path —
            # i.e. always "ended / outcome unseen", never done or failed. `taskId` on the chain event is
            # the correlation handle: attribute the node's real outcome to its task row here. Terminal
            # events only; `:queued`/`:running` already come from the snapshot poll. Resolved BEFORE the
            # line is pushed so the node's run time can go on it, while its row still exists.
            tid = let t = get(msg, :taskId, nothing); t isa AbstractString ? String(t) : "" end
            push_event!("chain:$node",
                        string(col(BOLD, fn),
                               node in ("done", "failed") && !isempty(tid) ?
                                   _ran_for(tid; started  = get(msg, :startedAt, ""),
                                                 finished = get(msg, :finishedAt, "")) : "",
                               col(DIM, "  img=$(short(img)) run=$(short(run_id))"));
                        colour = status_colour(node == "done" ? "done" :
                                               node == "failed" ? "failed" :
                                               node == "running" ? "running" : "queued"))
            if !isempty(tid) && node in ("done", "failed")
                # node:failed carries which of failed/skipped/cancelled it was
                _note_terminal!(tid, node == "done" ? "done" : String(get(msg, :status, "failed")))
            end

        elseif startswith(type, "chain:run:") || type == "chain:log"
            detail = type == "chain:log" ? String(get(msg, :line, "")) :
                     string(col(BOLD, String(get(msg, :chain, ""))),
                            haskey(msg, :error) ? col(RED, "  $(String(msg.error))") : "")
            push_event!(type, detail;
                        colour = endswith(type, "failed") ? RED :
                                 endswith(type, "done")   ? GREEN : BLUE)
        end
        # ping/pong and anything else are ignored.
    end
    STREAM_MODE || render_throttled()
end

# ── Dashboard render (in-place redraw) ───────────────────────────────────────────
function progress_bar(p::Float64, width::Int=14)
    p < 0 && return col(DIM, rpad("—", width + 5))
    filled = round(Int, p * width)
    bar = string("[", col(GREEN, repeat("█", filled)), repeat("·", width - filled), "]")
    string(bar, " ", lpad(string(round(Int, p * 100)), 3), "%")
end

function render()
    io = IOBuffer()
    rows, cols = try displaysize(stdout) catch; (40, 120) end

    w = min(cols, 100)

    # Only ACTIVE tasks are rows — running first, then queued. Finished ones live in TALLY as counts.
    tasks = sort(collect(values(TASKS)), by = t -> (t.status == "running" ? 0 : 1, t.fun_name, t.id))
    n_run = count(t -> t.status == "running", tasks)
    n_q   = count(t -> t.status == "queued", tasks)

    # ── ONE height budget so nothing ever clips: fixed chrome (title/counts/blank, table header,
    # dividers, footer) ≈ 6 lines (+1 with a logs pane), reserved generously; the rest splits between
    # the task table (priority), a small activity peek and the logs pane. EVERY section obeys this — no
    # per-pane minimum can push content past the window (the earlier bug: min-6 logs + min-3 activity
    # overran a short terminal and shoved the header off the top).
    haveLogs  = !isempty(LOGS)
    havePools = !isempty(POOLS)
    content   = max(4, rows - 9 - (haveLogs ? 1 : 0) - (havePools ? 1 : 0))
    logCap    = haveLogs ? clamp(content ÷ 3, 1, 6) : 0
    evtCap    = clamp(content ÷ 4, 1, 4)
    tableRoom = max(2, content - logCap - evtCap)
    truncated = length(tasks) > tableRoom
    nShown    = truncated ? max(1, tableRoom - 1) : length(tasks)   # reserve a line for "…and N more"

    # header — live counts + cumulative finished tallies (so you see "how many done" without 50 rows)
    print(io, "\e[H\e[2J")
    print(io, col(BOLD, "Cecelia task console"), col(DIM, "  $HTTP_BASE"),
          "   ", col(GREY, Dates.format(Dates.now(), "yyyy-mm-dd HH:MM:SS")), "\n")
    print(io, col(CYAN, "$n_run running"), col(DIM, " · "), col(YELLOW, "$n_q queued"),
          col(DIM, " · "), col(GREEN, "$(TALLY["done"]) done"),
          col(DIM, " · "), col(RED, "$(TALLY["failed"]) failed"),
          TALLY["cancelled"] > 0 ? string(col(DIM, " · "), col(MAGENTA, "$(TALLY["cancelled"]) cancelled")) : "",
          TALLY["ended"] > 0 ? string(col(DIM, " · "), col(GREY, "$(TALLY["ended"]) ended")) : "",
          "\n")

    # pools panel — configured concurrency limit vs slots in use now (+ any queued) for each pool.
    # Running count coloured (cyan when busy, dim when idle); queued shown in yellow when non-zero.
    if havePools
        parts = map(sort(POOLS, by = _poolsort)) do p
            string(col(BOLD, p.name), " ",
                   col(p.running > 0 ? CYAN : DIM, "$(p.running)/$(p.limit)"),
                   p.queued > 0 ? col(YELLOW, " +$(p.queued)q") : "")
        end
        print(io, col(DIM, "pools  "), join(parts, col(DIM, "   ")), "\n")
    end
    print(io, "\n")

    # active task table
    if isempty(tasks)
        print(io, col(DIM, "  no active tasks — waiting for work\n"))
    else
        # Widths add up to 99 with the progress bar, so the row still fits a 100-column terminal —
        # PROJECT was paid for by narrowing IMAGE and POOL, both of which never fill 10 (a uid is 6
        # chars, the longest pool name is 7).
        print(io, col(DIM, string(rpad("TASK", 9), rpad("FUNCTION", 25), rpad("PROJECT", 8),
                                   rpad("IMAGE", 8), rpad("POOL", 8), rpad("STATUS", 11),
                                   rpad("ELAPSED", 9), "PROGRESS")), "\n")
        for t in tasks[1:nShown]
            chain = isempty(t.chain_run_id) ? "" : " ⛓"
            # ELAPSED is run time on a running row and queue wait on a queued one — the clock restarts
            # with the status (`_set_phase!`), so the column always reads "how long in this state".
            print(io,
                rpad(short(t.id), 9),
                rpad(trunc_s(isempty(t.fun_name) ? "…" : t.fun_name * chain, 24), 25),
                rpad(short(t.project_uid), 8),
                rpad(short(t.image_uid), 8),
                rpad(trunc_s(t.pool_name, 7), 8),
                col(status_colour(t.status), rpad(t.status, 11)),
                t.status == "running" ? rpad(dur_since(t), 9) : col(DIM, rpad(dur_since(t), 9)),
                t.status == "running" ? progress_bar(t.progress) : col(DIM, "waiting"),
                "\n")
        end
        truncated && print(io, col(DIM, "  …and $(length(tasks) - nShown) more active\n"))
    end

    # activity peek, then the confined logs pane — both bounded by the budget above
    print(io, col(DIM, "── activity " * "─"^max(0, w - 12)), "\n")
    for line in (length(EVENTS) > evtCap ? EVENTS[(end - evtCap + 1):end] : EVENTS)
        print(io, trunc_s(line, cols + 40), "\n")   # +40 slack for ANSI escape bytes
    end
    if logCap > 0
        print(io, col(DIM, "── logs " * "─"^max(0, w - 8)), "\n")
        for line in (length(LOGS) > logCap ? LOGS[(end - logCap + 1):end] : LOGS)
            print(io, trunc_s(line, cols + 40), "\n")
        end
    end
    print(io, col(DIM, "(reporting only — Ctrl-C to quit)"))

    print(String(take!(io)))
    flush(stdout)
    _last_render[] = Dates.now()
end

# A full-screen repaint per WS frame made the receive loop the bottleneck at task-log volume — and a
# slow reader is exactly what makes the server drop THIS client's frames (per-client drop-on-full queue
# in server.jl), so the console helped cause the loss it then couldn't recover from. Coalesce repaints;
# the 2s snapshot loop always repaints, so a skipped frame is at most 2s behind.
const RENDER_MIN_INTERVAL = Millisecond(100)
const _last_render        = Ref(Dates.now() - Second(10))

function render_throttled()
    Dates.now() - _last_render[] < RENDER_MIN_INTERVAL && return
    render()
end

function show_waiting(reason::AbstractString)
    if STREAM_MODE
        push_event!("console", col(YELLOW, reason))
    else
        print("\e[H\e[2J")
        println(col(BOLD, "Cecelia task console"), col(DIM, "  $HTTP_BASE"))
        println()
        println(col(YELLOW, reason))
        flush(stdout)
    end
end

# ── Main loop (connect, stream, reconnect) ───────────────────────────────────────
function run_console()
    STREAM_MODE ? push_event!("console", "connecting to $WS_URL") :
                  show_waiting("Connecting to $WS_URL …")
    while true
        connected = Ref(true)
        try
            HTTP.WebSockets.open(WS_URL; connect_timeout=3) do ws
                # A (re)connect on localhost means the server (re)started — its task ids and in-flight
                # set are gone, so drop our stale view and re-seed from the fresh snapshot. Otherwise
                # tasks from the previous server session would linger forever (we only ever add rows).
                lock(LOCK) do
                    empty!(TASKS); empty!(EVENTS); empty!(LOGS); empty!(SEEN_TERM); empty!(POOLS)
                    empty!(ENDED_IDS); RECENT_SINCE[] = ""
                    for k in keys(TALLY); TALLY[k] = 0; end
                end
                # seed the snapshot, then keep it fresh + keep the socket alive. The prime pass fast-
                # forwards past outcomes that predate this connection without counting them.
                refresh_recent!(; prime = true)
                refresh_snapshot!(); STREAM_MODE || refresh_pools!()
                STREAM_MODE || render()
                @async while connected[]
                    # outcomes BEFORE the snapshot: learn how a task ended in the same poll that
                    # notices it left the in-flight set, so it is never retired as "outcome unseen"
                    # while the server can still say what happened.
                    try sleep(2); refresh_recent!(); refresh_snapshot!(); STREAM_MODE || refresh_pools!(); STREAM_MODE || render() catch end
                end
                @async while connected[]
                    try
                        sleep(20); HTTP.WebSockets.send(ws, "{\"type\":\"ping\"}")
                    catch
                        # A failed keepalive means the socket is dead or half-open. Swallowing it left
                        # the reader blocked in `for msg in ws` forever — HTTP polling kept refreshing
                        # rows while no WS frame ever arrived again, so nothing ever reached a terminal
                        # status. Tear it down instead: the outer loop reconnects, clears and reseeds.
                        connected[] = false
                        try; close(ws); catch; end
                    end
                end
                for msg in ws
                    handle_ws(msg isa AbstractString ? msg : String(msg))
                end
            end
        catch e
            e isa InterruptException && rethrow()
        finally
            connected[] = false
        end
        show_waiting("Disconnected from $WS_URL — is the server running? Retrying in 2s …")
        sleep(2)
    end
end

# Entrypoint — only when run as a script (`pixi run console`). Guarded so the test suite can
# `include` this file to drive `_reconcile_snapshot!` without opening a socket or a dashboard.
if abspath(PROGRAM_FILE) == @__FILE__
    try
        run_console()
    catch e
        e isa InterruptException || rethrow()
        println("\n", col(DIM, "task console stopped."))
    end
end
