# ── App lifecycle: global shutdown (+ restart, Phase 3) ─────────────────────────
# Drives the Settings "System" panel's global controls. Per-component start/stop/restart reuse the
# existing napari_api / notebooks_api endpoints; only the whole-app actions live here.

# Stop the child processes THIS server owns, best-effort, before the process exits. napari has no
# atexit hook (unlike the notebook server), so it must be closed explicitly here or the bridge is
# orphaned on :7655. Called by the global shutdown (and, later, restart).
function _stop_children_for_exit()
    try
        v = _viewer()
        v !== nothing && close!(v)          # kills the napari bridge process
    catch e
        @warn "Shutdown: closing napari failed" exception = e
    end
    try
        _shutdown_notebook_server!()        # stops a Pluto server we spawned (no-op otherwise)
    catch e
        @warn "Shutdown: stopping notebooks failed" exception = e
    end
    # belt-and-suspenders: also free the child ports by force, covering a bridge we only ADOPTED or
    # that outlived a crash (no process handle to close!) — so shutdown/restart never leaves a zombie
    # on :7655 / :7660. Mirrors `pixi run stop`. No-op when the graceful stop already freed the port.
    try; Cecelia._kill_listeners_on_port(Cecelia.NAPARI_PORT); catch; end
    try; Cecelia._kill_listeners_on_port(NOTEBOOKS_PORT);      catch; end
end

# POST /api/app/shutdown  → { ok, message }   — the global "Quit everything".
# Stops children, answers 200, then exits the process from a detached task so the HTTP response
# flushes first. In dev this ends `pixi run dev`; in the packaged app the server exit ends app.py.
function api_app_shutdown(body_bytes::Vector{UInt8})
    @info "Shutdown requested via /api/app/shutdown"
    _stop_children_for_exit()
    @async begin
        sleep(0.3)      # give handle_stream time to write the response before the process dies
        exit(0)
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
    _stop_children_for_exit()
    @async begin
        sleep(0.4)      # flush the HTTP response first, then exit with the restart sentinel
        exit(RESTART_EXIT_CODE)
    end
    200, JSON3.write((; ok = true, message = "Restarting Cecelia"))
end

# ── Dev worktree switch (Settings → System) ─────────────────────────────────────
# A dev convenience: relaunch the BACKEND from another git worktree without dropping to the console.
# The supervisor (`dev.jl`) does the actual relaunch — this endpoint just records the target and exits
# with the restart sentinel (same mechanism as restart). DEV + supervised only. NOTE: this switches the
# server on :8080 only; a frontend-only branch still needs its own Vite (see docs/DEV.md branch preview).
_git_toplevel(dir::AbstractString) =
    try; strip(readchomp(`git -C $dir rev-parse --show-toplevel`)); catch; ""; end

# GET /api/app/worktrees → { worktrees: [{path, branch, current}], current, canSwitch }
function api_app_worktrees(::HTTP.Request)
    here = _git_toplevel(pwd())
    out = Any[]
    try
        text = readchomp(`git -C $(pwd()) worktree list --porcelain`)
        for block in split(text, "\n\n")
            isempty(strip(block)) && continue
            path = ""; branch = "(detached)"
            for l in split(block, "\n")
                startswith(l, "worktree ") && (path = String(l[10:end]))
                startswith(l, "branch ")   && (branch = replace(String(l[8:end]), "refs/heads/" => ""))
            end
            isempty(path) && continue
            push!(out, (; path, branch, current = path == here))
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
    try
        for l in eachline(`git -C $(pwd()) worktree list --porcelain`)
            startswith(l, "worktree ") && push!(known, String(l[10:end]))
        end
    catch; end
    (target in known) || return 400, JSON3.write((; error = "Not a known worktree: $target"))
    target == here && return 200, JSON3.write((; ok = true, message = "Already on this worktree"))
    apidir = joinpath(target, "api")
    isdir(apidir) || return 400, JSON3.write((; error = "No api/ directory in $target"))
    write(sf, apidir)                    # the supervisor relaunches the child here on the next loop
    @info "Worktree switch requested via /api/app/switch-worktree" target apidir
    _stop_children_for_exit()
    @async begin
        sleep(0.4)
        exit(RESTART_EXIT_CODE)
    end
    200, JSON3.write((; ok = true, message = "Switching to $(basename(target))"))
end
