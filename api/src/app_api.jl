# ── App lifecycle: global shutdown (+ restart, Phase 3) ─────────────────────────
# Drives the Settings "System" panel's global controls. Per-component start/stop/restart reuse the
# existing notebooks_api endpoints; only the whole-app actions live here.

# Stop the child processes THIS server owns, best-effort, before the process exits. napari had no
# atexit hook (unlike the notebook server), so it must be closed explicitly here or the bridge is
# orphaned on :7655. Called by the global shutdown (and, later, restart).
#
# EVERY resident child this server can launch belongs here — the list is the contract, and it is
# checked by a test (`api/test/runtests.jl`) rather than by whoever adds the next one. The preview
# worker was missed when it was added and outlived Quit on :7656, holding the VRAM of a warm cellpose
# model that nothing could then reach; the whole reason this function exists is that zombie children
# were a recurring problem.
#
# Killing the preview worker here does NOT cost the warm-worker optimisation it was built for. A Revise
# reload never reaches this function (the process does not exit), which is the case adoption exists to
# serve; only an explicit Quit or Restart does, and after either the process handle is gone anyway.
# `stop_runner` is the ONE asymmetry between Quit and Restart, and it is the whole point of the task
# runner: Restart must leave it running (that is how a backend restart stops costing a segmentation),
# while Quit — the user saying they are done — takes it with everything else. Every other resident
# child is stopped by both. See docs/todo/TASK_RUNNER_PLAN.md → Decision 3.
#
# NOTE for whoever adds the next child: the runtests assertion below counts `_kill_listeners_on_port`
# calls against the ports `api_diagnostics` reports. The runner is reported there too but is stopped
# CONDITIONALLY, so it is asserted separately rather than by that count.
function _stop_children_for_exit(; stop_runner::Bool = true)
    # In-flight TASK subprocesses. Nothing else kills these: the `exit` below just reparents them, so a
    # Quit during a cellpose run left the Python child alive — finishing, writing its zarr, and never
    # registered, because the Julia post-step (`register_label_files!`, QC, run log) died with us.
    # `cancel_task!` marks the record and kills the process TREE. Only reaches tasks in THIS process;
    # ones on the detached runner are not in `_TASKS` at all and go when the runner does, below.
    #
    # NOT gated on `stop_runner`. It was, and that was wrong in the one direction that costs work: a
    # Restart or a worktree switch exits this process either way, so an IN-PROCESS task (the fallback
    # whenever the runner is down or disabled) dies regardless — the gate only decided whether it died
    # *tidily*. Skipping the cancel orphaned its Python child to keep burning GPU into a `.partial`
    # nobody would ever promote, and left its record with no terminal status, so the run vanished with
    # no cancelled entry and no error. The runner's own tasks are untouched by this loop, which is why
    # the gate was never what protected them — `stop_runner` below is.
    try
        for t in list_tasks(); cancel_task!(String(t.id)); end
    catch e
        @warn "Shutdown: cancelling in-flight tasks failed" exception = e
    end
    try
        _shutdown_notebook_server!()        # stops a Pluto server we spawned (no-op otherwise)
    catch e
        @warn "Shutdown: stopping notebooks failed" exception = e
    end
    try
        _stop_preview_worker!()             # the resident preview worker (no-op when never launched)
    catch e
        @warn "Shutdown: stopping the preview worker failed" exception = e
    end
    # belt-and-suspenders: also free the child ports by force, covering a child we only ADOPTED or
    # that outlived a crash (no process handle to close!) — so shutdown/restart never leaves a zombie
    # on :7656 / :7660. Mirrors `pixi run stop`. No-op when the graceful stop already freed it.
    try; Cecelia._kill_listeners_on_port(Cecelia.PREVIEW_PORT); catch; end
    try; Cecelia._kill_listeners_on_port(NOTEBOOKS_PORT);       catch; end
    # …and the task runner, but ONLY on the routes that mean "stop everything". A restart leaving it
    # alive is not an oversight to be tidied up later — it is the feature.
    stop_runner && try; Cecelia.runner_stop!(_RUNNER); catch; end
end

# ── Leaving the process ─────────────────────────────────────────────────────────
# **Never call `exit()` here — use `_exit_now`.** `Base.exit` runs `jl_atexit_hook`, which tears down
# the JIT and the thread pool while the rest of the process is still live. Every HTTP handler runs on
# the thread pool (see `handle_stream`), so at shutdown there is routinely a worker mid-compile, and
# that teardown then either SEGFAULTS or hangs. Measured on Julia 1.12.6 with worker threads
# compiling: `exit(0)` faulted in LLVM in 3 of 5 runs; `_exit` was clean in 8 of 8.
#
# The segfault was not cosmetic — it is why Quit did not quit. `dev.jl` classifies a fault signal as a
# CRASH (`_crash_death`) and relaunches the backend, so an in-app Quit that faulted on the way out came
# straight back up. And the exit CODE is the only channel that carries intent (0 = quit,
# RESTART_EXIT_CODE = restart); a process that dies of SIGSEGV delivers a signal instead, so the
# supervisor could not tell "the user asked to stop" from "it broke".
#
# POSIX `_exit` skips all of it: no atexit hooks, no thread rendezvous, no JIT teardown — the kernel
# just reaps the process, with the exact status we asked for.
#
# **Skipping atexit is deliberate, not collateral.** `start` (server.jl) registers a
# `_stop_children_for_exit` atexit hook, because that is the ONLY thing that runs on a Ctrl-C or a
# SIGTERM — Julia's `jl_exit` runs hooks, and a caught `InterruptException` is not available to a
# multithreaded server (see the comment on `start`). Every caller here has already run the teardown
# ITSELF, with the arguments this route wants — so bypassing the hook is what keeps it from running a
# second time and, on a Restart, from stopping the task runner that `stop_runner = false` just spared.
# Each route must therefore keep calling `_stop_children_for_exit` explicitly; `api/test` asserts it.
#
# Buffered IO is NOT flushed by `_exit`, so flush first — otherwise the final log line is lost.
function _exit_now(code::Integer)
    for io in (stdout, stderr)
        try; flush(io); catch; end
    end
    try
        ccall(:_exit, Cvoid, (Cint,), code)
    catch e     # no `_exit` in this libc (should not happen on any supported OS) — take the risky path
        @warn "Immediate exit unavailable; falling back to exit()" exception = e
        exit(code)
    end
end

# POST /api/app/shutdown  → { ok, message }   — the global "Quit everything".
# Stops children, answers 200, then exits the process from a detached task so the HTTP response
# flushes first. In dev this ends `pixi run dev`; in the packaged app the server exit ends app.py.
function api_app_shutdown(body_bytes::Vector{UInt8})
    @info "Shutdown requested via /api/app/shutdown"
    _stop_children_for_exit()
    @async begin
        sleep(0.3)      # give handle_stream time to write the response before the process dies
        _exit_now(0)
    end
    200, JSON3.write((; ok = true, message = "Shutting down Cecelia"))
end

# Backend restart works ONLY under a supervisor that relaunches the server when it exits with
# RESTART_EXIT_CODE — the `dev.jl` loop in dev, or `app.py` in prod. Both set CECELIA_SUPERVISED so we
# never "exit to nowhere" on a bare launch (`julia src/server.jl` directly). This replaces the earlier
# detached-relauncher approach, which couldn't reattach a new server to a foreground terminal and
# depended on `pixi` being on PATH. The UI offers restart dev-only (button gated on `diag.dev`).
const RESTART_EXIT_CODE = 42
_can_restart()::Bool = haskey(ENV, "CECELIA_SUPERVISED")

# POST /api/app/restart  → { ok } | 409  — restart the backend itself.
# Stop children, then exit with the sentinel; the supervisor relaunches in place (same terminal / app
# window) — no detaching, no pixi-on-PATH dependency.
function api_app_restart(body_bytes::Vector{UInt8})
    _can_restart() || return 409, JSON3.write((;
        error = "Restart unavailable — the server isn't running under a supervisor."))
    @info "Restart requested via /api/app/restart"
    _stop_children_for_exit(; stop_runner = false)   # a restart must NOT cost a running task
    @async begin
        sleep(0.4)      # flush the HTTP response first, then exit with the restart sentinel
        _exit_now(RESTART_EXIT_CODE)
    end
    200, JSON3.write((; ok = true, message = "Restarting Cecelia"))
end

# ── Dev worktree switch (Settings → System) ─────────────────────────────────────
# A dev convenience: relaunch the BACKEND from another git worktree without dropping to the console.
# The supervisor (`dev.jl`) does the actual relaunch — this endpoint just records the target and exits
# with the restart sentinel (same mechanism as restart). DEV + supervised only. NOTE: this switches the
# server on :8080 only; a frontend-only branch still needs its own Vite (see docs/DEV.md branch preview).
_git_toplevel(dir::AbstractString) = Cecelia.git_probe("rev-parse", "--show-toplevel"; dir = dir)

# GET /api/app/worktrees → { worktrees: [{path, branch, current, primary}], current, canSwitch }
function api_app_worktrees(::HTTP.Request)
    here = _git_toplevel(pwd())
    out = Any[]
    # "" covers both "git said no" and "there is no .git here" — an installed app is the latter, and
    # it is not an error state, just nothing to switch to. See Cecelia.git_probe.
    text = Cecelia.git_probe("worktree", "list", "--porcelain")
    isempty(text) && return 200, JSON3.write((; worktrees = Any[], current = here, canSwitch = false))
    try
        for block in split(text, "\n\n")
            isempty(strip(block)) && continue
            path = ""; branch = "(detached)"
            for l in split(block, "\n")
                startswith(l, "worktree ") && (path = String(l[10:end]))
                startswith(l, "branch ")   && (branch = replace(String(l[8:end]), "refs/heads/" => ""))
            end
            isempty(path) && continue
            # git lists the primary (main) worktree FIRST — flag it so the UI can identify the main
            # checkout even when it's on a feature branch (the branch label alone can't, since no
            # worktree need be on `main`). `primary = isempty(out)` → true only for the first entry.
            push!(out, (; path, branch, current = path == here, primary = isempty(out)))
        end
    catch e
        return 200, JSON3.write((; worktrees = Any[], current = here, canSwitch = false,
                                   error = "git worktree list failed: $(sprint(showerror, e))"))
    end
    200, JSON3.write((; worktrees = out, current = here, canSwitch = _can_restart()))
end

# POST /api/app/switch-worktree { path } → { ok } | 4xx  — relaunch the backend from `path`'s api/ dir.
function api_app_switch_worktree(body_bytes::Vector{UInt8})
    _can_restart() || return 409, JSON3.write((;
        error = "Worktree switch unavailable — the server isn't running under a supervisor."))
    sf = get(ENV, "CECELIA_SWITCH_FILE", "")
    isempty(sf) && return 409, JSON3.write((; error = "Supervisor didn't provide a switch channel."))
    body = JSON3.read(body_bytes, Dict{String,Any})
    target = String(get(body, "path", ""))
    isempty(target) && return 400, JSON3.write((; error = "path required"))
    here = _git_toplevel(pwd())
    known = Set{String}()
    for l in split(Cecelia.git_probe("worktree", "list", "--porcelain"), "\n")
        startswith(l, "worktree ") && push!(known, String(l[10:end]))
    end
    (target in known) || return 400, JSON3.write((; error = "Not a known worktree: $target"))
    target == here && return 200, JSON3.write((; ok = true, message = "Already on this worktree"))
    apidir = joinpath(target, "api")
    isdir(apidir) || return 400, JSON3.write((; error = "No api/ directory in $target"))
    write(sf, apidir)                    # the supervisor relaunches the child here on the next loop
    @info "Worktree switch requested via /api/app/switch-worktree" target apidir
    # Same as restart: the runner survives. It is now definitely running the OTHER worktree's code —
    # `/api/runner/status` reports its commit so that is visible rather than silent (Decision 5).
    _stop_children_for_exit(; stop_runner = false)
    @async begin
        sleep(0.4)
        _exit_now(RESTART_EXIT_CODE)
    end
    200, JSON3.write((; ok = true, message = "Switching to $(basename(target))"))
end
