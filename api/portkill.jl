# ── portkill.jl — free a TCP port by killing whatever LISTENS on it ─────────────────────────────
#
# ONE implementation, reached two ways:
#   • `include`d by `dev.jl`, the standalone supervisor — it has no Cecelia loaded, so it cannot call
#     `Cecelia._kill_listeners_on_port` (app/src/jobs.jl), which is the same job for in-process callers;
#   • run as a script by the `pixi run stop*` tasks: `julia portkill.jl 8080 5173 …`.
# Base + Sockets only — no packages, no `--project` needed — so `pixi run stop` still works when a manifest is
# broken or the env is half-installed, which is exactly when you reach for it. That also let the six
# `stop*` tasks drop their three per-OS shell variants (18 strings) for one call each.
#
# ── Why the escalation to SIGKILL is not optional ───────────────────────────────────────────────
# **SIGTERM alone does not stop a Julia server.** On Julia 1.12 a signal-driven exit has to bring
# every thread to a safepoint, and a thread inside a non-yielding region — LLVM codegen, a tight
# loop, a blocking `ccall` — never reaches one. The process prints *every* thread's backtrace
# (hundreds of lines, ending in `Allocations: …; GC: …`, which reads exactly like a crash) and then
# stays alive indefinitely. Measured on 1.12.6, `-t auto`, SIGTERM:
#
#   idle julia                    → dies in ~0.4 s
#   one busy worker thread        → still running after 8 s (had to be SIGKILLed)
#   worker thread compiling       → still running after 8 s (had to be SIGKILLed)
#
# The backend spawns every HTTP handler onto the thread pool, so "a worker thread is mid-compile" is
# the normal state, not an edge case. The old `pixi run stop` sent a single `kill` and then echoed
# "stopped backend(8080)/…" unconditionally — which is how a wedged backend kept :8080 while the
# terminal said it was gone, and why the next `pixi run dev` could not bind.
#
# A wedged process **keeps its listening socket** (verified), so re-finding it after the grace period
# and SIGKILLing it is what actually frees the port. That is the whole point of this file.
#
# ── Discovery must match LISTENING sockets only ─────────────────────────────────────────────────
# Never an ESTABLISHED connection *to* the port — the browser's open tab and its Vite HMR websocket
# are connected to :5173, and killing those reaps Firefox/Chrome. `ss -l`, `lsof -sTCP:LISTEN` and
# `netstat`'s LISTENING state all enforce that.
#
# Linux uses `ss` rather than `lsof`: lsof walks every process's /proc/<pid>/fd table and hangs
# indefinitely on any process stuck in D (uninterruptible) sleep — e.g. a task wedged on a slow
# mount — which used to stall `pixi run stop` outright. macOS has no `ss`, and the D-state hang is
# Linux-specific, so it keeps lsof.

module PortKill

export free_port

# Grace between SIGTERM and SIGKILL. Long enough for a healthy process to exit on its own; short
# enough that stopping the app stays interactive. A wedged one never exits on SIGTERM at all, so this
# is only ever the wait before the kill that works.
const KILL_GRACE = 1.5

# Capture a command's stdout, but NEVER let it hang the caller: if it runs longer than `secs`, kill it
# and return "". `lsof` can wedge INDEFINITELY on some boxes (blocking while it scans a stuck mount);
# that froze the GUI-shutdown teardown in dev.jl (readchomp never returned, so the terminal only came
# back on Ctrl-C). Pure-Julia watchdog — macOS has no coreutils `timeout`.
function _capture(cmd::Cmd; secs::Real = 4)::String
    p = try
        open(pipeline(ignorestatus(cmd), stderr = devnull), "r")
    catch
        return ""
    end
    wd = Timer(_ -> (process_running(p) && (try; kill(p); catch; end)), secs)
    out = try; read(p, String); catch; ""; end
    close(wd)
    return out
end

# Extract listener PIDs from `ss -tlnpH` output (the `users:(("name",pid=NNN,fd=..))` field), deduped
# — a listener shows once for IPv4 and once for IPv6. Pure, so the test suite can pin it; the rest of
# this file shells out and kills, which it cannot. (Mirrors `Cecelia._listener_pids_from_ss`; the two
# cannot share code across the package boundary, so `api/test/runtests.jl` pins them to agree.)
pids_from_ss(raw::AbstractString)::Vector{String} =
    unique(String(m.captures[1]) for m in eachmatch(r"pid=(\d+)", raw))

# PIDs of the processes LISTENING on `port`, as strings (they go straight back to `kill`). Empty when
# nothing listens — not an error.
function listener_pids(port::Integer)::Vector{String}
    if Sys.iswindows()
        out = String[]
        for ln in split(_capture(`cmd /c netstat -ano -p tcp`), '\n'; keepempty = false)
            (occursin("LISTENING", ln) && occursin(":$port ", ln)) || continue
            push!(out, String(last(split(strip(ln)))))
        end
        return unique(out)
    elseif Sys.islinux()
        return pids_from_ss(_capture(`ss -tlnpH $("sport = :$port")`))
    else   # macOS
        return unique(String.(split(_capture(`lsof -ti tcp:$port -sTCP:LISTEN`))))
    end
end

_run_quiet(cmd::Cmd) = try
    run(pipeline(ignorestatus(cmd); stdout = devnull, stderr = devnull))
catch
end

"""
    free_port(port; grace = KILL_GRACE) -> Bool

Kill whatever is LISTENING on `port`, escalating SIGTERM → SIGKILL, and report whether the port ended
up free. Best-effort and idempotent: nothing listening is a success, not an error.

Returns `false` only if something is STILL listening after the SIGKILL — a genuine "could not stop
it" that the caller should surface rather than print "stopped" over.
"""
function free_port(port::Integer; grace::Real = KILL_GRACE)::Bool
    pids = listener_pids(port)
    isempty(pids) && return true

    if Sys.iswindows()
        # `/F /T` is already a forced kill of the whole tree — no escalation to stage.
        for p in pids
            _run_quiet(`taskkill /PID $p /F /T`)
        end
    else
        _run_quiet(`kill $pids`)               # SIGTERM: let a healthy process exit on its own…
        deadline = time() + grace
        while time() < deadline
            isempty(listener_pids(port)) && return true
            sleep(0.1)
        end
        left = listener_pids(port)
        isempty(left) || _run_quiet(`kill -9 $left`)   # …and SIGKILL the one that cannot.
    end

    # Confirm, rather than assume. This is the check the old one-shot `kill` never made.
    for _ in 1:20
        isempty(listener_pids(port)) && return true
        sleep(0.1)
    end
    return isempty(listener_pids(port))
end

# ── Whose child is this? ──────────────────────────────────────────────────────────────────────────
# Kill-by-port trusts the port, and the child ports are ones a slot does NOT protect: the sticky slot
# only checks API + frontend (app/src/ports.jl). So another config dir's runner, Pluto or preview
# worker (same OS user — say the installed app, while you run `pixi run dev` against CECELIA_DEV_DIR)
# can be sitting on our slot's ports. In-process, `runner_stop!` (`runner_is_foreign`),
# `_free_notebooks_port!` and `_free_preview_port!` (api/src/) refuse to stop one; the dev
# supervisor's teardown and `pixi run stop*` cannot load Cecelia, so they ask the same questions here.

import Sockets

# GET `path` on 127.0.0.1:`port` and return the raw response ("" when nothing answers). Sockets only —
# this file must not need HTTP.jl. HTTP/1.0 so the reply is never chunked; the watchdog closes the
# socket if the far end never finishes, so a wedged listener cannot hang a teardown.
function _http_get_raw(port::Integer, path::AbstractString; headers::AbstractString = "",
                       secs::Real = 2)::String
    sock = try
        Sockets.connect(Sockets.localhost, port)
    catch
        return ""
    end
    wd = Timer(_ -> close(sock), secs)
    out = try
        write(sock, "GET $path HTTP/1.0\r\nHost: 127.0.0.1:$port\r\nConnection: close\r\n$headers\r\n")
        String(read(sock))
    catch
        ""
    end
    close(wd)
    close(sock)
    return out
end

# The `configDir` field of a runner `/ping` reply, JSON-unescaped (a Windows path arrives as
# `C:\\Users\\…`), or `nothing` when absent. Pure, so the suite pins it.
function ping_config_dir(raw::AbstractString)::Union{String,Nothing}
    m = match(r"\"configDir\"\s*:\s*\"((?:[^\"\\]|\\.)*)\"", raw)
    m === nothing && return nothing
    replace(m.captures[1], r"\\(u[0-9a-fA-F]{4}|.)" => function (esc)
        c = esc[2:end]
        startswith(c, "u") ? string(Char(parse(UInt16, c[2:end]; base = 16))) :
        c == "n" ? "\n" : c == "t" ? "\t" : c == "r" ? "\r" : c == "b" ? "\b" : c == "f" ? "\f" : c
    end)
end

"""
    foreign_runner_dir(port, cfg) -> Union{String,Nothing}

The config dir of the task runner on `port` when it is NOT `cfg` — i.e. another Cecelia's runner,
which a stop must leave alone — else `nothing`. Nothing answering, a reply without `configDir` (a
runner too old to report one) and a non-runner all read as "not foreign", exactly as
`runner_is_foreign` reads them, so the stop proceeds as it always did. Never throws.
"""
function foreign_runner_dir(port::Integer, cfg::AbstractString)::Union{String,Nothing}
    d = try
        ping_config_dir(_http_get_raw(port, "/ping"))
    catch
        nothing
    end
    (d !== nothing && d != cfg) ? d : nothing
end

# HTTP status code of a raw response, or `nothing` when there is none. Pure, so the suite pins it.
http_status(raw::AbstractString)::Union{Int,Nothing} =
    (m = match(r"^HTTP/\d(?:\.\d)? (\d{3})", raw); m === nothing ? nothing : parse(Int, m.captures[1]))

"""
    pluto_is_foreign(port, secret) -> Bool

Does the Pluto on `port` refuse our `secret`? Pluto answers `/auth-check` with 403 to a secret that is
not its own, so a server that rejects ours belongs to another Cecelia. Nothing answering, or a server
with the secret disabled (accepts anything), reads as "not foreign". Never throws.
"""
function pluto_is_foreign(port::Integer, secret::AbstractString)::Bool
    try
        http_status(_http_get_raw(port, "/auth-check?secret=" * _escape_query(secret))) == 403
    catch
        false
    end
end

"""
    preview_is_foreign(port, token) -> Bool

Does the preview worker on `port` refuse our API `token`? It answers a handshake without the token of
the backend that launched it with 401 (and ours with 426, a GET that is not a WebSocket upgrade), so a
401 means another Cecelia's. No token of ours proves nothing — every worker refuses that — and reads as
"not foreign", as does nothing answering. Never throws.
"""
function preview_is_foreign(port::Integer, token::Union{AbstractString,Nothing})::Bool
    token === nothing && return false
    try
        http_status(_http_get_raw(port, "/"; headers = "Authorization: Bearer $token\r\n")) == 401
    catch
        false
    end
end

_escape_query(s::AbstractString) =
    join(isletter(c) && isascii(c) || isdigit(c) || c in "-_.~" ? string(c) :
         join("%" * uppercase(string(b; base = 16, pad = 2)) for b in codeunits(string(c))) for c in s)

"""
    foreign_owner(svc, port, cfg; token, secret) -> Union{String,Nothing}

Why a stop must leave `svc`'s listener on `port` alone — another Cecelia owns it — else `nothing`.
`:runner` asks `/ping` for its config dir (`foreign_runner_dir`); `:notebooks` checks whether Pluto
accepts our session `secret` (`pluto_is_foreign`; callers pass `read_pluto_secret()`, which this
Base-only module cannot reach); `:preview` whether the worker accepts our API `token`
(`preview_is_foreign`). Backend and frontend are ours by the slot itself.
"""
function foreign_owner(svc::Symbol, port::Integer, cfg::AbstractString;
                       token::Union{AbstractString,Nothing} = nothing,
                       secret::AbstractString = "")::Union{String,Nothing}
    if svc === :runner
        d = foreign_runner_dir(port, cfg)
        return d === nothing ? nothing : "config dir $d"
    elseif svc === :notebooks
        return pluto_is_foreign(port, secret) ? "it rejects this install's session secret" : nothing
    elseif svc === :preview
        return preview_is_foreign(port, token) ? "it rejects this install's API token" : nothing
    end
    nothing
end

end # module

using .PortKill: free_port, listener_pids, foreign_runner_dir, foreign_owner

# ── Script entry point ────────────────────────────────────────────────────────────────────────────
#   julia portkill.jl <service> [<service> …]         — backend/frontend/preview/runner/notebooks
#   julia portkill.jl <label> <port> [<label> <port> …]
# The `pixi run stop*` tasks use the first form: a service resolves to THIS user's port slot
# (`CECELIA_PORT_SLOT`, else the sticky slot the last launch saved — app/src/ports.jl), so a stop never
# aims at another user's Cecelia on the same machine. Prints one line per component and reports what
# actually happened — a port that could not be freed says so instead of being echoed over as "stopped".
if abspath(PROGRAM_FILE) == @__FILE__
    include(joinpath(@__DIR__, "..", "app", "src", "config_dir.jl"))
    include(joinpath(@__DIR__, "..", "app", "src", "ports.jl"))
    include(joinpath(@__DIR__, "..", "app", "src", "api_token.jl"))   # Base + Random: the preview check
    args = ARGS
    targets = Pair{String,Int}[]
    if !isempty(args) && all(a -> Symbol(a) in keys(SERVICE_PORT_BASE), args)
        slot = current_port_slot()
        append!(targets, [a => service_port(Symbol(a); slot) for a in args])
    else
        (isempty(args) || isodd(length(args))) &&
            (println(stderr, "usage: julia portkill.jl <service> [...] | <label> <port> [...]"); exit(2))
        for i in 1:2:length(args)
            port = something(tryparse(Int, args[i + 1]), 0)
            port == 0 && (println(stderr, "portkill: not a port: $(args[i + 1])"); exit(2))
            push!(targets, args[i] => port)
        end
    end
    stuck = String[]
    done  = String[]
    for (label, port) in targets
        # Never another config dir's child — see `foreign_owner`.
        if label in ("runner", "notebooks", "preview") &&
           (why = foreign_owner(Symbol(label), port, config_dir(); token = read_api_token(),
                                                             secret = read_pluto_secret())) !== nothing
            println("left $label($port) running — it belongs to another Cecelia ($why)")
            continue
        end
        (free_port(port) ? done : stuck) |> v -> push!(v, "$label($port)")
    end
    isempty(done)  || println("stopped ", join(done, "/"))
    isempty(stuck) || println(stderr, "STILL RUNNING: ", join(stuck, "/"),
                              " — could not free (survived SIGKILL?); check with `ss -tlnp`")
    exit(isempty(stuck) ? 0 : 1)
end
