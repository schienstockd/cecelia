# ── Diagnostics + (opt-in) debug console ─────────────────────────────────────────
#
# `GET /api/diagnostics` — read-only server health (threads, versions, memory, paths). Always on;
# powers the Settings-page Diagnostics panel (and answers "is the multithreaded server actually on?").
#
# `POST /api/repl` — evaluate Julia in the SERVER process. This is arbitrary code execution, so it is
# gated by TWO independent conditions, BOTH required:
#   1. `CECELIA_REPL=1` — the operator explicitly opts in at launch (default off).
#   2. the server is bound to LOOPBACK (`_BOUND_HOST` is 127.0.0.1/::1) — so the OS itself refuses
#      non-local connections. This is the network control: unlike a Host header it can't be spoofed,
#      because a network client cannot even open a socket to a loopback-bound server.
# So a `0.0.0.0`-bound (network-reachable) server NEVER serves the REPL, even with the flag on — the
# operator must relaunch with `CECELIA_HOST=127.0.0.1`. Server code is `include`d into `Main`, so eval
# runs with `Cecelia`, `projects_dir`, etc. in scope — the point of a debug console.

import Pkg   # for the in-process Julia package inventory (api_packages)

const _REPL_LOCK = ReentrantLock()   # serialise evals (redirect_stdout is process-global)
# `_repl_on` is a RUNTIME toggle (the Settings switch flips it via POST /api/repl/config); it seeds from
# CECELIA_REPL at startup so the env var still auto-enables. It is NOT a security boundary — eval is
# hard-gated by the loopback bind below regardless — it just controls whether the console is offered.
_repl_env_default() = lowercase(strip(get(ENV, "CECELIA_REPL", ""))) in ("1", "true", "yes", "on")
const _repl_on = Ref{Bool}(_repl_env_default())
_host_is_loopback() = _BOUND_HOST[] in ("127.0.0.1", "::1", "localhost", "[::1]")
# Usable only when the toggle is on AND the server is loopback-bound. The loopback bind is the hard
# network control (a 0.0.0.0 server never serves the REPL, toggle or not); the UI reads `replAvailable`.
_repl_available() = _repl_on[] && _host_is_loopback()

# POST /api/repl/config {enabled} — flip the runtime toggle. Not a security boundary (eval still needs a
# loopback bind), so it's safe to accept from the UI; returns the resulting state.
function api_repl_config(body_bytes::Vector{UInt8})
    body = try; JSON3.read(String(body_bytes), Dict{String,Any}); catch; return 400, JSON3.write((; error="invalid JSON body")); end
    _repl_on[] = Bool(get(body, "enabled", false))
    200, JSON3.write((; replEnabled = _repl_on[], loopback = _host_is_loopback(), replAvailable = _repl_available()))
end

# Installed-build provenance: the installer writes `.cecelia-version` at the install root (the repo
# root, two levels up from api/src) — a tag for stable, "dev @ <branch> <sha>" for the dev channel.
# Absent in a plain source/dev checkout → report that instead. See docs/SHIPPING.md → install channels.
function _installed_version()
    f = joinpath(dirname(dirname(@__DIR__)), ".cecelia-version")
    isfile(f) || return "dev (source checkout)"
    v = strip(read(f, String))
    isempty(v) ? "unknown" : v
end

# GET /api/diagnostics/packages — the installed-package inventory, split by ecosystem because the two
# stacks are provisioned SEPARATELY and neither tool sees the other: the Python analysis env (conda +
# pypi) lives in Pixi, while Julia is on juliaup + Manifest (docs/SHIPPING.md → "Julia and Node are not
# in Pixi"). So `pixi list` alone misses every Julia package. We query both:
#   • Julia  — in-process via Pkg.dependencies() (the server IS Julia; free, no subprocess)
#   • Python — `pixi list --json` at the install root (conda + pypi = the complete engine env)
# Lazy — its own endpoint, not part of /api/diagnostics — because the pixi subprocess is slowish and
# shouldn't run on every Settings load.
function _julia_packages()
    pkgs = [(; name = i.name, version = i.version === nothing ? "" : string(i.version))
            for (_, i) in Pkg.dependencies() if i.name !== nothing]
    sort(pkgs; by = p -> lowercase(p.name))
end

function _pixi_packages()
    root = dirname(dirname(@__DIR__))   # install/repo root — where pixi.toml lives (server cwd is api/)
    out  = read(Cmd(`pixi list --json`; dir = root), String)
    arr  = JSON3.read(out)
    pkgs = [(; name     = string(p.name),
               version  = string(p.version),
               kind     = haskey(p, :kind) ? string(p.kind) : "",
               explicit = haskey(p, :is_explicit) && p.is_explicit === true) for p in arr]
    sort(pkgs; by = p -> lowercase(p.name))
end

function api_packages(::HTTP.Request)
    jl = try
        _julia_packages()
    catch e
        @warn "Julia package list failed" exception = e
        NamedTuple[]
    end
    py, pyerr = try
        _pixi_packages(), nothing
    catch e
        NamedTuple[], "Could not run `pixi list` (is pixi on PATH?): " * sprint(showerror, e)
    end
    200, JSON3.write((; julia = jl, python = py, pythonError = pyerr))
end

function api_diagnostics(::HTTP.Request)
    gb(x) = round(x / 2^30; digits = 2)
    200, JSON3.write((;
        threads     = Threads.nthreads(),
        julia       = string(VERSION),
        version     = _installed_version(),
        projectsDir = projects_dir(),
        memFreeGB   = gb(Sys.free_memory()),
        memTotalGB  = gb(Sys.total_memory()),
        gcLiveMB    = round(Base.gc_live_bytes() / 2^20; digits = 1),
        host        = HOST,
        port        = PORT,
        loopback    = _host_is_loopback(),
        replEnabled = _repl_on[],           # runtime toggle state
        replAvailable = _repl_available(),  # toggle on AND loopback-bound → console usable
        dev         = _is_dev(),            # dev server (pixi run dev sets CECELIA_DEV); prod/app.py never does
        napariPort    = Cecelia.NAPARI_PORT,  # child-service ports, surfaced so the panel shows which
        notebooksPort = NOTEBOOKS_PORT,       # ports Cecelia occupies (backend `port` is above)
    ))
end

# Dev vs prod: the `dev` pixi task sets CECELIA_DEV=1; the packaged app (app.py / `pixi run app`) and
# `pixi run prod` never do — so absence means prod, auto-detected with no installer involvement. Gates
# dev-only controls (e.g. the planned backend restart) in Settings → System.
_is_dev() = get(ENV, "CECELIA_DEV", "") ∉ ("", "0", "false")

# Render a value the way the REPL would (text/plain), length-capped so a huge object can't flood the UI.
function _repl_show(x)::String
    io = IOBuffer()
    try
        show(IOContext(io, :limit => true, :displaysize => (60, 120)), MIME"text/plain"(), x)
    catch e
        print(io, "<unshowable: ", sprint(showerror, e), ">")
    end
    s = String(take!(io))
    length(s) > 20_000 ? s[1:20_000] * "\n… [truncated]" : s
end

function api_repl(body_bytes::Vector{UInt8})
    _repl_on[] || return 403, JSON3.write((;
        error = "Debug console is disabled. Turn it on in Settings."))
    _host_is_loopback() || return 403, JSON3.write((;
        error = "Debug console requires a loopback bind (the server is on $(_BOUND_HOST[])). " *
                "Relaunch with CECELIA_HOST=127.0.0.1 to use it."))
    body = try
        JSON3.read(String(body_bytes), Dict{String,Any})
    catch
        return 400, JSON3.write((; error = "invalid JSON body"))
    end
    code = strip(string(get(body, "code", "")))
    isempty(code) && return 400, JSON3.write((; error = "empty code"))
    value = nothing; errmsg = nothing; output = ""
    # capture stdout+stderr so `println`/`@info` show in the console. `redirect_stdout` swaps the
    # PROCESS-global stream, so evals are serialised under a lock (one at a time) to avoid two evals
    # clobbering each other; an async reader drains the pipe so large output can't deadlock on a full
    # pipe buffer.
    #
    # KNOWN LIMITATION (accepted, not fixed — see docs/API.md): the lock only serialises evals against
    # each other. Under `-t auto`, any OTHER handler / pool task running concurrently with an eval has
    # its stdout/stderr (`println`/`@info`/`@warn`) captured into this pipe too — so during an eval,
    # unrelated server-log lines may appear in the console output or go missing from the server log. A
    # real fix needs per-task output capture rather than a process-global redirect, which isn't worth
    # the machinery for a loopback-only, opt-in debug console. The UI shows a note to this effect.
    lock(_REPL_LOCK) do
        old_o, old_e = stdout, stderr
        rd, wr = redirect_stdout()
        redirect_stderr(wr)
        reader = @async read(rd, String)
        try
            value = Core.eval(Main, Meta.parseall(String(code)))
        catch e
            errmsg = sprint(showerror, e, catch_backtrace())
        finally
            redirect_stdout(old_o); redirect_stderr(old_e); close(wr)
        end
        output = fetch(reader)
    end
    200, JSON3.write((;
        ok     = errmsg === nothing,
        value  = value === nothing ? "" : _repl_show(value),
        output = output,
        error  = errmsg,
    ))
end
