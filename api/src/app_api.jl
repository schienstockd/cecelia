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
