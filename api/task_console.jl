# Cecelia task console — a read-only, GUI-less live view of scheduler tasks.
#
# Connects to the running API server's WebSocket (/ws) for the live
# task:* / chain:* event stream, and polls GET /api/tasks for the in-flight
# snapshot (which fills in fun_name / image / pool that the WS stream alone
# doesn't carry). REPORTING ONLY — it never sends task:run / task:cancel /
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

const ACTIVE   = ("queued", "running")
const TERMINAL = ("done", "failed", "cancelled")

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
end

const TASKS      = Dict{String,TaskView}()   # taskId => view
const EVENTS     = String[]                   # ring buffer of rendered event lines
const LOCK       = ReentrantLock()
const MAX_EVENTS = 500
const KEEP_DONE  = 12                         # cap on lingering finished rows

now_hms() = Dates.format(Dates.now(), "HH:MM:SS")
short(id::AbstractString, n::Int=8) = length(id) <= n ? String(id) : String(last(id, n))
trunc_s(s::AbstractString, n::Int) = length(s) <= n ? String(s) : String(first(s, max(0, n - 1))) * "…"

# Get-or-create a task view, so a WS event for a not-yet-snapshotted task still shows up.
function _task!(id::AbstractString)
    get!(TASKS, String(id)) do
        TaskView(String(id), "", "", "", "", "queued", -1.0, "", Dates.now())
    end
end

function push_event!(kind::AbstractString, detail::AbstractString; colour=nothing)
    c = colour === nothing ? GREY : colour
    line = string(col(GREY, now_hms()), "  ", col(c, rpad(kind, 12)), "  ", detail)
    push!(EVENTS, line)
    length(EVENTS) > MAX_EVENTS && deleteat!(EVENTS, 1:(length(EVENTS) - MAX_EVENTS))
    STREAM_MODE && println(line)
end

# Drop the oldest finished rows so the table can't grow without bound.
function _prune_done!()
    done_ids = [t.id for t in values(TASKS) if t.status in TERMINAL]
    if length(done_ids) > KEEP_DONE
        sort!(done_ids, by = id -> TASKS[id].updated)
        for id in done_ids[1:(length(done_ids) - KEEP_DONE)]
            delete!(TASKS, id)
        end
    end
end

# ── HTTP snapshot (fills in fun_name / image / pool for in-flight tasks) ─────────
function refresh_snapshot!()
    try
        r = HTTP.get("$HTTP_BASE/api/tasks"; connect_timeout=2, readtimeout=3, retry=false)
        rows = JSON3.read(String(r.body))
        lock(LOCK) do
            for row in rows
                t = _task!(String(row.id))
                t.fun_name     = String(get(row, :fun_name, t.fun_name))
                t.image_uid    = String(get(row, :image_uid, t.image_uid))
                t.pool_name    = String(get(row, :pool_name, t.pool_name))
                t.chain_run_id = String(get(row, :chain_run_id, t.chain_run_id))
                # Only trust the snapshot's status for tasks it still knows about; the WS
                # stream owns terminal transitions (finished tasks deregister server-side).
                t.status in TERMINAL || (t.status = String(get(row, :status, t.status)))
            end
        end
        return true
    catch
        return false   # server not up yet / transient — the caller shows connection state
    end
end

# ── WS message handling ─────────────────────────────────────────────────────────
function handle_ws(raw::AbstractString)
    msg  = JSON3.read(raw)
    type = String(get(msg, :type, ""))
    lock(LOCK) do
        if type == "task:status"
            id = String(get(msg, :taskId, "")); isempty(id) && return
            t = _task!(id)
            t.status  = String(get(msg, :status, t.status))
            uid = String(get(msg, :imageUid, "")); isempty(uid) || (t.image_uid = uid)
            t.updated = Dates.now()
            push_event!("status", string(col(BOLD, short(id)), " ",
                        col(status_colour(t.status), t.status),
                        isempty(t.fun_name) ? "" : col(DIM, " ($(t.fun_name))"));
                        colour = status_colour(t.status))
            t.status in TERMINAL && _prune_done!()

        elseif type == "task:progress"
            id = String(get(msg, :taskId, "")); isempty(id) && return
            t = _task!(id)
            t.progress = clamp(Float64(get(msg, :progress, 0.0)), 0.0, 1.0)
            t.updated  = Dates.now()

        elseif type == "task:log"
            id = String(get(msg, :taskId, "")); isempty(id) && return
            line = String(get(msg, :line, ""))
            t = _task!(id); t.last_log = line; t.updated = Dates.now()
            push_event!("log", string(col(DIM, short(id)), " ", line))

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
    STREAM_MODE || render()
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

    tasks = collect(values(TASKS))
    order = Dict("running" => 0, "queued" => 1, "done" => 2, "failed" => 3, "cancelled" => 4)
    sort!(tasks, by = t -> (get(order, t.status, 9), t.fun_name, t.id))
    n_run = count(t -> t.status == "running", tasks)
    n_q   = count(t -> t.status == "queued", tasks)

    # header
    print(io, "\e[H\e[2J")
    print(io, col(BOLD, "Cecelia task console"), col(DIM, "  $HTTP_BASE"),
          "   ", col(GREY, Dates.format(Dates.now(), "yyyy-mm-dd HH:MM:SS")), "\n")
    print(io, col(CYAN, "$n_run running"), col(DIM, " · "), col(YELLOW, "$n_q queued"),
          col(DIM, " · $(length(tasks)) shown"), "\n\n")

    # task table
    if isempty(tasks)
        print(io, col(DIM, "  no tasks in flight\n"))
    else
        hdr = string(rpad("TASK", 9), rpad("FUNCTION", 26), rpad("IMAGE", 10),
                     rpad("POOL", 10), rpad("STATUS", 11), "PROGRESS")
        print(io, col(DIM, hdr), "\n")
        for t in tasks
            chain = isempty(t.chain_run_id) ? "" : " ⛓"
            print(io,
                rpad(short(t.id), 9),
                rpad(trunc_s(isempty(t.fun_name) ? "…" : t.fun_name * chain, 25), 26),
                rpad(short(t.image_uid), 10),
                rpad(trunc_s(t.pool_name, 9), 10),
                col(status_colour(t.status), rpad(t.status, 11)),
                t.status == "running" ? progress_bar(t.progress) :
                    col(DIM, t.status in TERMINAL ? "done" : ""),
                "\n")
        end
    end

    # event log — fill the remaining terminal height
    print(io, "\n", col(DIM, "─"^min(cols, 100)), "\n")
    used  = length(tasks) + 8
    avail = max(4, rows - used - 1)
    tail  = length(EVENTS) > avail ? EVENTS[(end - avail + 1):end] : EVENTS
    for line in tail
        print(io, trunc_s(line, cols + 40), "\n")   # +40 slack for ANSI escape bytes
    end
    print(io, col(DIM, "\n(reporting only — Ctrl-C to quit)"))

    print(String(take!(io)))
    flush(stdout)
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
                # seed the snapshot, then keep it fresh + keep the socket alive
                refresh_snapshot!()
                STREAM_MODE || render()
                @async while connected[]
                    try sleep(2); refresh_snapshot!(); STREAM_MODE || render() catch end
                end
                @async while connected[]
                    try sleep(20); HTTP.WebSockets.send(ws, "{\"type\":\"ping\"}") catch end
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
