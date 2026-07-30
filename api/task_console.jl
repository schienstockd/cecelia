# Cecelia task console — a read-only, GUI-less live view of scheduler tasks.
#
# Connects to the running API server's WebSocket (/ws) for the live
# task:* / chain:* event stream, and polls GET /api/tasks for the in-flight
# snapshot (which fills in fun_name / image / pool that the WS stream alone
# doesn't carry) plus GET /api/pools for live per-pool occupancy (limit vs
# running + queued). REPORTING ONLY — it never sends task:run / task:cancel /
# chain:* control messages, so it is safe to run alongside the GUI.
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
    pool_name::String
    chain_run_id::String
    status::String
    progress::Float64      # -1 = unknown / not reported yet
    last_log::String
    updated::DateTime
    # Reconciliation state (see refresh_snapshot!). `in_snapshot` marks a task the /api/tasks
    # snapshot has listed at least once — i.e. a real SCHEDULER task, so its absence from a later
    # snapshot is meaningful. WS-only producers (jobs, batch movies) never appear in the snapshot
    # and must never be pruned by it. `misses` counts consecutive snapshots it went missing from.
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
# "ended" = finished, outcome unseen: the task left the scheduler snapshot but its terminal
# task:status frame never arrived (WS telemetry is lossy by design — see broadcast_ws). Counted
# separately rather than guessed as done/failed, so the console never claims an outcome it didn't see.
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

# Get-or-create a task view, so a WS event for a not-yet-snapshotted task still shows up.
function _task!(id::AbstractString)
    get!(TASKS, String(id)) do
        TaskView(String(id), "", "", "", "", "queued", -1.0, "", Dates.now(), false, 0)
    end
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
function _note_terminal!(id::AbstractString, status::AbstractString)
    id in SEEN_TERM && return
    push!(SEEN_TERM, id)
    haskey(TALLY, status) && (TALLY[status] += 1)
    delete!(TASKS, id)
end

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

function refresh_snapshot!()
    try
        r = HTTP.get("$HTTP_BASE/api/tasks"; connect_timeout=2, readtimeout=3, retry=false)
        rows = JSON3.read(String(r.body))
        lock(LOCK) do
            present = Set{String}()
            for row in rows
                id = String(row.id)
                push!(present, id)
                id in SEEN_TERM && continue                 # already finished + counted — don't resurrect
                status = String(get(row, :status, ""))
                if status in TERMINAL                       # finished before we saw it live → just count it
                    _note_terminal!(id, status)
                    continue
                end
                t = _task!(id)
                t.fun_name     = String(get(row, :fun_name, t.fun_name))
                t.image_uid    = String(get(row, :image_uid, t.image_uid))
                t.pool_name    = String(get(row, :pool_name, t.pool_name))
                t.chain_run_id = String(get(row, :chain_run_id, t.chain_run_id))
                isempty(status) || (t.status = status)
                t.in_snapshot  = true
                t.misses       = 0
            end
            # retire what the scheduler no longer knows about (outcome unseen — tallied as "ended")
            for (id, t) in collect(TASKS)
                (t.in_snapshot && !(id in present)) || continue
                t.misses += 1
                t.misses >= SNAPSHOT_MISSES_TO_RETIRE || continue
                _note_terminal!(id, "ended")
                push_event!("status", string(col(BOLD, short(id)), " ", col(GREY, "ended"),
                            col(DIM, " (outcome unseen — dropped frame)")); colour = GREY)
            end
        end
        return true
    catch
        return false   # server not up yet / transient — the caller shows connection state
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
            id = String(get(msg, :taskId, "")); (isempty(id) || id in SEEN_TERM) && return
            status = String(get(msg, :status, ""))
            # Non-scheduler producers (batch movies, jobs) carry fun/pool on the event itself — they
            # never hit the /api/tasks snapshot, so without this their rows show a blank function AND a
            # blank pool ("floating in space"). Prefer the event's values; fall back to the snapshot's.
            ev_fun  = String(get(msg, :fun,  ""))
            ev_pool = String(get(msg, :pool, ""))
            fn = !isempty(ev_fun) ? ev_fun : (haskey(TASKS, id) ? TASKS[id].fun_name : "")
            push_event!("status", string(col(BOLD, short(id)), " ",
                        col(status_colour(status), status),
                        isempty(fn) ? "" : col(DIM, " ($fn)"));
                        colour = status_colour(status))
            if status in TERMINAL
                _note_terminal!(id, status)            # collapse to a count, drop the row
            else
                t = _task!(id)
                isempty(status)  || (t.status = status)
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
            t = _task!(id); t.last_log = line; t.updated = Dates.now()
            push_log!(id, line)

        elseif type == "task:result"
            id = String(get(msg, :taskId, ""))
            push_event!("result", string(col(BOLD, short(id)), " result ready"); colour = GREEN)

        elseif startswith(type, "chain:node:")
            node   = replace(type, "chain:node:" => "")
            fn     = String(get(msg, :fn, ""))
            run_id = String(get(msg, :runId, ""))
            img    = String(get(msg, :imageUid, ""))
            push_event!("chain:$node",
                        string(col(BOLD, fn), col(DIM, "  img=$(short(img)) run=$(short(run_id))"));
                        colour = status_colour(node == "done" ? "done" :
                                               node == "failed" ? "failed" :
                                               node == "running" ? "running" : "queued"))

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
        print(io, col(DIM, string(rpad("TASK", 9), rpad("FUNCTION", 26), rpad("IMAGE", 10),
                                   rpad("POOL", 10), rpad("STATUS", 11), "PROGRESS")), "\n")
        for t in tasks[1:nShown]
            chain = isempty(t.chain_run_id) ? "" : " ⛓"
            print(io,
                rpad(short(t.id), 9),
                rpad(trunc_s(isempty(t.fun_name) ? "…" : t.fun_name * chain, 25), 26),
                rpad(short(t.image_uid), 10),
                rpad(trunc_s(t.pool_name, 9), 10),
                col(status_colour(t.status), rpad(t.status, 11)),
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
                    for k in keys(TALLY); TALLY[k] = 0; end
                end
                # seed the snapshot, then keep it fresh + keep the socket alive
                refresh_snapshot!(); STREAM_MODE || refresh_pools!()
                STREAM_MODE || render()
                @async while connected[]
                    try sleep(2); refresh_snapshot!(); STREAM_MODE || refresh_pools!(); STREAM_MODE || render() catch end
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

try
    run_console()
catch e
    e isa InterruptException || rethrow()
    println("\n", col(DIM, "task console stopped."))
end
