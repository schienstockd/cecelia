# ── The log rail: one record shape, one tee, one child-process pump ────────────
#
# Everything the app can say about itself ends up in the bottom console, and before this file there
# were four unrelated ways of getting it there — three of which lost the message entirely.
#
# The rule this file exists to enforce: **a component reports by LOGGING, and the sink decides where
# that goes.** The package never knows about WebSockets (docs/ARCHITECTURE.md → layer boundary), so
# it emits ordinary `@info`/`@warn`/`@error`; `TeeLogger` is what a *server* installs to also forward
# each record to its own transport. The API server broadcasts it as `server:log`; the detached task
# runner emits it as `runner:log` over its event stream and the API server relays that into the same
# ring. One record shape, three sinks, no second formatter.
#
# ## Why a record and not a string
#
# The console has a collapsed row (`message`) and an expandable body (`detail`). A Julia log record
# carries its payload in KWARGS, and the previous tee flattened those with `"$k = $v"` — which turned
# the single most valuable line in the app, `@error … exception = (e, catch_backtrace())`, into 857
# characters of raw `Ptr{Nothing}`. A backtrace is not a value to interpolate, it is a thing to
# FORMAT (`showerror(io, e, bt)`), and the formatted form belongs in `detail`, not in the row.
#
# ## Why child processes are pumped here
#
# `run(cmd; wait = false)` does NOT inherit stdio — Julia swallows it to devnull (`spawn_opts_swallow`).
# Long-lived children were spawned exactly that way (the preview worker :7656, the task runner :7657),
# so ~20 `print(..., flush=True)` diagnostics and the worker's `traceback.print_exc()` went nowhere at
# all — not to the console, and not to the terminal either.
# That is the "messages get lost" case, and it is why `spawn_logged` is the only sanctioned way to
# start a long-lived child: it pipes both streams into the logger, so the child's output travels the
# same rail as everything else and lands in the console under its own `source`.

import Logging
import Dates

# Log sources. The `source` field is a FACET in the console (one chip per source), so the set is
# closed and shared with the frontend — a new source needs a chip, not a free-form string. Keep in
# step with `LOG_SOURCES` in `frontend/src/stores/log.ts`; asserted by the
# "log sources agree across languages" testset.
const LOG_SOURCE_BACKEND   = "backend"
const LOG_SOURCE_PREVIEW   = "preview"
const LOG_SOURCE_RUNNER    = "runner"
const LOG_SOURCE_NOTEBOOKS = "notebooks"

#: The sources a CHILD PROCESS writes under — the noisy ones. The console hides these by default
#: (their chips are off until you ask) because a warm cellpose preview or a notebooks server can be
#: chatty, while their warnings and errors still surface. See docs/UI.md → *The console*.
const CHILD_LOG_SOURCES = (LOG_SOURCE_PREVIEW, LOG_SOURCE_RUNNER, LOG_SOURCE_NOTEBOOKS)

const LOG_SOURCES = (LOG_SOURCE_BACKEND, CHILD_LOG_SOURCES...)

#: A formatted backtrace is unbounded and the record rides a WS frame into a browser store. Cap it —
#: the top of a stacktrace is the part anybody reads, and 8 KB is ~80 frames.
const LOG_DETAIL_CAP = 8000

# ── The record ────────────────────────────────────────────────────────────────

"""
    log_record(level, message; kwargs...) -> Dict{String,Any}

Build the canonical console record: `level`, `message`, `source`, and (optionally) `detail`.

Three kwargs are interpreted rather than stringified:

| kwarg | becomes |
|---|---|
| `exception` | a FORMATTED error + stacktrace in `detail`, and its first line appended to `message` |
| `detail`    | `detail` verbatim (how `spawn_logged` carries a Python traceback block) |
| `source`    | the `source` facet (defaults to `"backend"`) |

Everything else is flattened `k = v` into `detail`, which is where the old tee put it *in the row*.

The exception's first line is promoted into `message` on purpose. Half the call sites read
`@warn "show_labels failed" exception = e` — a row saying only "show_labels failed" tells you nothing
you did not already know, and the one word that matters (`BoundsError`, `KeyError`) was hidden behind
a click. If the message already contains it, it is not repeated.
"""
function log_record(level::AbstractString, message; kwargs...)::Dict{String,Any}
    msg    = string(message)
    source = LOG_SOURCE_BACKEND
    detail = String[]
    exc_head = ""

    for (k, v) in kwargs
        if k === :source
            source = string(v)
        elseif k === :exception
            formatted = _format_exception(v)
            exc_head  = first(split(formatted, '\n'; limit = 2))
            push!(detail, formatted)
        elseif k === :detail
            push!(detail, string(v))
        else
            push!(detail, "$k = $v")
        end
    end

    # promote the exception's first line into the row, unless the message already says it
    if !isempty(exc_head) && !occursin(exc_head, msg)
        msg = isempty(msg) ? exc_head : "$msg — $exc_head"
    end

    rec = Dict{String,Any}("level" => level, "message" => msg, "source" => source)
    body = join(detail, "\n")
    isempty(body) || (rec["detail"] = _cap(body, LOG_DETAIL_CAP))
    rec
end

_cap(s::AbstractString, n::Int)::String =
    length(s) <= n ? String(s) : String(first(s, n)) * "\n… truncated ($(length(s)) chars)"

"""
    _format_exception(v) -> String

Render an `exception` kwarg the way the terminal would. Julia's logging convention is that the value
is either the exception or an `(exception, backtrace)` tuple, and `showerror(io, e, bt)` is the
function that turns the second form into the "Stacktrace:" block you would have read in the terminal.
Never throws: a value that is neither shape falls back to `string(v)`, because a logging failure that
escapes takes out the thing being logged about.
"""
function _format_exception(v)::String
    io = IOBuffer()
    try
        if v isa Tuple && length(v) == 2
            showerror(io, v[1], v[2])
        else
            showerror(io, v)
        end
    catch
        take!(io)                     # drop whatever was half-written
        print(io, string(v))
    end
    String(take!(io))
end

# ── The tee ───────────────────────────────────────────────────────────────────

"""
    TeeLogger(inner, sink)

An `AbstractLogger` that forwards every record to `inner` (the real console logger — the terminal
keeps working exactly as before) AND hands `sink` the `log_record` for it.

Installed by a SERVER, never by the package itself: `Cecelia.jl` must stay usable headless from the
REPL, so nothing here decides that a record should reach a browser. `sink` errors are swallowed —
a logger that can throw turns every logged warning into a second, worse failure.
"""
struct TeeLogger{L<:Logging.AbstractLogger,F} <: Logging.AbstractLogger
    inner::L
    sink::F
end

Logging.min_enabled_level(l::TeeLogger)   = Logging.min_enabled_level(l.inner)
Logging.shouldlog(l::TeeLogger, args...)  = Logging.shouldlog(l.inner, args...)
Logging.catch_exceptions(l::TeeLogger)    = Logging.catch_exceptions(l.inner)

function Logging.handle_message(l::TeeLogger, level, message, _module, group, id, file, line; kwargs...)
    Logging.handle_message(l.inner, level, message, _module, group, id, file, line; kwargs...)
    try
        l.sink(log_record(log_level_name(level), message; kwargs...))
    catch
        # a logging failure must never escape the logger
    end
    nothing
end

"""Julia log level → the console's three levels. `@debug` never reaches here (filtered by `shouldlog`)."""
log_level_name(level)::String =
    level >= Logging.Error ? "error" : level >= Logging.Warn ? "warn" : "info"

"""
    install_log_tee!(sink)

Wrap the current global logger in a `TeeLogger`. Idempotent-ish by construction: calling it twice
would nest two tees and double every record, so a server calls it once, in its start path.
"""
install_log_tee!(sink) = Logging.global_logger(TeeLogger(Logging.global_logger(), sink))

"""An ISO-8601 UTC stamp for a record. The SINK stamps, not the producer — see `spawn_logged`."""
log_timestamp()::String = Dates.format(Dates.now(Dates.UTC), "yyyy-mm-ddTHH:MM:SS.sssZ")

# ── The ring ──────────────────────────────────────────────────────────────────

"""
    LogRing(cap = 500)

A bounded, thread-safe history of console records, stamped with a monotonic `seq` and a `ts`.

**The `seq` is the point.** WS telemetry is lossy by design here — `broadcast_ws` DROPS a frame for a
client whose queue is full rather than block a worker thread — and task frames get away with that
because there is a reconciliation path (`GET /api/tasks/recent`). Log lines had none: a dropped line
was simply gone, and nothing anywhere could tell you it had happened. With a `seq` on every record a
client that receives `n+2` after `n` KNOWS it missed one and asks for the gap
(`GET /api/logs/recent?since=n`), so "sometimes messages get lost" becomes a self-healing case rather
than a silent one.

Both servers keep one: the API server's is what the browser reads, and the runner's is what lets the
API server backfill everything the runner said while nobody was subscribed (it outlives backends, so
that window is normal, not exceptional).
"""
mutable struct LogRing
    cap::Int
    seq::Int
    #: Identifies THIS ring instance. A restarted server begins counting at 1 again, so a client
    #: holding `seq = 412` from the previous process would treat the new ring's first 412 records as
    #: ones it already had and drop them — a restart would silently eat its own startup. The id is
    #: what lets the client tell "more of the same ring" from "a different ring", and it is why the
    #: cursor is safe to keep across a reconnect at all.
    id::String
    entries::Vector{Dict{String,Any}}
    lk::ReentrantLock
end
LogRing(cap::Int = 500) = LogRing(cap, 0, gen_uid(8), Dict{String,Any}[], ReentrantLock())

"""Append a record, returning the STAMPED copy to broadcast (so the wire and the ring agree)."""
function log_ring_push!(r::LogRing, rec::Dict{String,Any})::Dict{String,Any}
    lock(r.lk) do
        r.seq += 1
        stamped = copy(rec)
        stamped["seq"] = r.seq
        # A relayed record (the runner's) already carries the stamp from where it happened — keep it,
        # or the console would order the runner's lines by when the backend heard them.
        haskey(stamped, "ts") || (stamped["ts"] = log_timestamp())
        push!(r.entries, stamped)
        length(r.entries) > r.cap && popfirst!(r.entries)
        stamped
    end
end

"""Every record after `since` (a `seq`). `since = 0` is the whole ring — a cold console's backfill."""
log_ring_since(r::LogRing, since::Integer)::Vector{Dict{String,Any}} =
    lock(r.lk) do
        [e for e in r.entries if get(e, "seq", 0) > since]
    end

"""The highest `seq` issued so far — what a client reports back as `since`."""
log_ring_seq(r::LogRing)::Int = lock(r.lk) do; r.seq; end

"""This ring instance's id. A client that sees a new one throws its cursor away — see the struct."""
log_ring_id(r::LogRing)::String = r.id

# ── Child process → log rail ──────────────────────────────────────────────────

#: A Python traceback is ONE event spread over many lines, and the console filters per line. Split
#: across records, the header lands as `error` and every frame under it as `info` — which the default
#: view hides, so the traceback appears as a bare `Traceback (most recent call last):` with nothing
#: under it. `ChildLineSink` reassembles the block into a single record instead.
const _PY_TRACEBACK_HEAD = r"^\s*Traceback \(most recent call last\):\s*$"
#: The line that ENDS a traceback: `ValueError: bad thing`, `cecelia.utils.zarr_utils.StoreError: …`.
const _PY_EXC_LINE       = r"^[A-Za-z_][A-Za-z0-9_.]*(Error|Exception|Exit|Interrupt|Warning)\b"

"""
    ChildLineSink(source)

Line-oriented reader state for one child process. Feed it lines with [`child_line_records!`]; it
returns the records to emit (0, 1 or 2 per line) and holds the partial traceback between calls.
"""
mutable struct ChildLineSink
    source::String
    buf::Vector{String}
end
ChildLineSink(source::AbstractString) = ChildLineSink(String(source), String[])

"""
    child_line_level(line) -> "info" | "warn" | "error"

Classify one child stdout/stderr line. Deliberately conservative — the cost of guessing `error` is a
red row, the cost of guessing `info` is a hidden message, and the child sources are hidden by default
so the second is the expensive one. Anything that names itself an error or carries our own `[ERROR]`
tag is an error; the rest is info.
"""
function child_line_level(line::AbstractString)::String
    s = strip(line)
    # WARN is checked FIRST, because `_PY_EXC_LINE` deliberately matches `…Warning:` too (it is a valid
    # traceback terminator under `-W error`) and would otherwise call every `UserWarning: deprecated`
    # an error — cellpose and scikit-image emit those by the dozen, and a console that paints them red
    # is one nobody reads the red in.
    (startswith(s, "[WARN]") || startswith(s, "WARNING") || occursin(r"^\S*Warning\b", s)) && return "warn"
    (startswith(s, "[ERROR]") || startswith(s, "ERROR") || occursin(_PY_EXC_LINE, s)) && return "error"
    "info"
end

"""
    child_line_records!(sink, line) -> Vector{Dict{String,Any}}

Feed one line; get back the records it completes. Pure apart from `sink`'s own buffer, so the
traceback reassembly is unit-tested directly (see the "child log lines" testset).
"""
function child_line_records!(sink::ChildLineSink, line::AbstractString)::Vector{Dict{String,Any}}
    out = Dict{String,Any}[]
    if !isempty(sink.buf)
        push!(sink.buf, String(line))
        # the exception line closes the block; a non-indented, non-exception line means the traceback
        # ended without one (interleaved output) — flush what we have rather than swallow it
        if occursin(_PY_EXC_LINE, strip(line))
            push!(out, _flush_traceback!(sink))
        elseif !startswith(line, " ") && !isempty(strip(line))
            pop!(sink.buf)
            push!(out, _flush_traceback!(sink))
            append!(out, child_line_records!(sink, line))
        end
        return out
    end
    if occursin(_PY_TRACEBACK_HEAD, line)
        push!(sink.buf, String(line))
        return out
    end
    isempty(strip(line)) && return out            # blank filler between prints is not an event
    push!(out, log_record(child_line_level(line), line; source = sink.source))
    out
end

"""Emit whatever is left in the buffer — called when the child's stream ends mid-traceback."""
function child_line_flush!(sink::ChildLineSink)::Vector{Dict{String,Any}}
    isempty(sink.buf) ? Dict{String,Any}[] : [_flush_traceback!(sink)]
end

function _flush_traceback!(sink::ChildLineSink)::Dict{String,Any}
    lines = copy(sink.buf)
    block = join(lines, "\n")
    empty!(sink.buf)
    # the row shows the exception line (the last one) — "Traceback (most recent call last):" as a
    # collapsed row says only that something failed, which the red already said
    at  = findlast(l -> occursin(_PY_EXC_LINE, strip(l)), lines)
    msg = at === nothing ? "Traceback (no exception line)" : strip(lines[at])
    log_record("error", msg; source = sink.source, detail = block)
end

"""
    spawn_logged(source, cmd) -> Base.Process

**The only sanctioned way to start a long-lived child process.** Spawns `cmd` with both its stdout
and stderr piped into the Julia logger, one record per line, tagged `source = source` — so the child's
output reaches the terminal AND the app console like everything else.

Use this instead of `run(cmd; wait = false)`, which swallows both streams to devnull (see the header
of this file: that silently discarded every line the preview worker ever printed). It is the
process-level twin of `run_py`'s stream handling for task runners.

**Not for a process that must outlive us.** The pipe is owned by THIS process, so a child holding it
after we exit writes into a broken pipe. The detached task runner is deliberately excluded for that
reason — it reports over its own event stream instead (`runner:log`, see `runner/server.jl`), which
survives the backend restart it exists to survive.

The pump task runs until EOF. That matters: a child whose reader stopped would BLOCK on write once the
64 KB pipe filled, so the loop is kept free of anything that can throw.
"""
function spawn_logged(source::AbstractString, cmd::Base.AbstractCmd)::Base.Process
    out  = Pipe()
    proc = run(pipeline(cmd; stdout = out, stderr = out); wait = false)
    close(out.in)
    Threads.@spawn _pump_child!(String(source), out)
    proc
end

function _pump_child!(source::String, out::Pipe)
    sink = ChildLineSink(source)
    try
        for line in eachline(out)
            for rec in child_line_records!(sink, line)
                _emit_child_record(rec)
            end
        end
    catch
        # the child died or the pipe broke — nothing to report that its exit code will not say
    finally
        for rec in child_line_flush!(sink)
            try; _emit_child_record(rec); catch; end
        end
    end
end

# One place turns a child record back into a Julia log call, so it travels the SAME tee as everything
# else rather than needing its own transport. `source` rides as a kwarg; `log_record` reads it back out
# on the far side.
#
# `_module`/`_file`/`_line` are nulled deliberately. Julia's console logger appends `@ <module>
# <file>:<line>` to every warning and error, and left to the default that would read
# `@ Cecelia log_stream.jl:390` on every line the preview worker ever prints — pointing at the pump
# that forwarded it rather than at anything to do with the failure, which is worse than no pointer at all.
function _emit_child_record(rec::Dict{String,Any})
    lvl = get(rec, "level", "info")
    msg = get(rec, "message", "")
    src = get(rec, "source", LOG_SOURCE_BACKEND)
    det = get(rec, "detail", nothing)
    if lvl == "error"
        det === nothing ? (@error msg source=src _module=nothing _file=nothing _line=nothing) :
                          (@error msg source=src detail=det _module=nothing _file=nothing _line=nothing)
    elseif lvl == "warn"
        det === nothing ? (@warn msg source=src _module=nothing _file=nothing _line=nothing) :
                          (@warn msg source=src detail=det _module=nothing _file=nothing _line=nothing)
    else
        det === nothing ? (@info msg source=src) : (@info msg source=src detail=det)
    end
end
