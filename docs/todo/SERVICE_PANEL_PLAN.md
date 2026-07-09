# Service control panel (Settings) — plan

**Status:** in-progress — branch `feat/settings-service-panel`. Phases 1–2 verified live. Phase 3
(backend restart) reworked from a detached relauncher (failed the live test) to a **supervisor +
`exit(42)`** model (`api/dev.jl` in dev, `app.py` loop in prod) — pending a re-test of the restart
button. Committed incrementally on the branch; final squash/PR at the end.

## Goal

A small **control panel in the Settings page** that (a) shows the live status of each runtime
component (running / starting / stopped) for transparency — "what is actually running" — and (b)
gives per-component **start / stop / restart**, plus one global **Quit everything** button and an
**Open console** button (live backend/pixi console in a separate window).

Trigger: a stale **napari bridge**. The bridge is a long-lived child that survives server restarts
and isn't Revise-tracked, so edits to `napari_bridge.py` (and PR #78's `configure_autosave`) don't
take effect until it's killed + relaunched — today that means a second terminal (`pixi run
stop-napari` + reopen). The old R app had a shutdown button; the pineapple app had nothing.

Components managed: **Application** (Julia backend :8080), **Napari bridge** (:7655), **Notebooks /
Pluto** (:7660). Vite frontend is **excluded** (prod: backend serves the built UI; dev: it's serving
the page you're on).

## Decisions (locked)

1. **Reuse existing endpoints for napari/notebooks.** napari `POST /api/napari/{restart,close}` +
   `GET /api/napari/status {alive,starting}`; notebooks `POST /api/notebooks/{restart,shutdown,launch}`
   + `GET /api/notebooks/status {running,starting,…}`. `launch`/`restart` need `projectUid`.
2. **Global Quit / app shutdown is a new endpoint** `POST /api/app/shutdown`: best-effort stop
   children (napari `close!`, `api_notebooks_shutdown`), respond `200`, then `@async` short delay +
   `exit(0)` so the response flushes. Napari has **no** `atexit` (notebooks does) → it MUST be killed
   explicitly. Dev: ends `pixi run dev`; packaged: server exit ends `app.py`.
3. **Backend restart needs a supervisor** (revised after live test — a detached relauncher does NOT
   work: it can't reattach a new server to a foreground terminal, and depended on `pixi` being on
   PATH). `POST /api/app/restart` stops children then `exit(42)` (`RESTART_EXIT_CODE`); a **supervisor**
   relaunches in place — `api/dev.jl` loops the Revise server in dev (the `dev` pixi task runs
   `julia dev.jl`), and `app.py`'s loop does it in prod. Both set `CECELIA_SUPERVISED`; restart is
   `409` without it (`_can_restart`), and the UI offers it dev-only (`diag.dev`).
4. **ONE console implementation — no second task-tracking path.** The windowed console reuses the
   existing `log` store (`frontend/src/stores/log.ts`) + `ErrorConsole.vue`; extract the log rendering
   into a shared component if needed and mount it in BOTH the docked bar and the window. The window is
   a second *mount point*, not a second console.
5. **Console window = console-only mode of the same SPA** (no vue-router in this app). Button →
   `window.open(origin + '#console', 'cecelia-console', …)`; on boot, `location.hash === '#console'`
   renders the shared console full-window with its own WS connection.
6. **Make it a real "pixi console":** add an `AbstractLogger` tee in `server.jl` that `broadcast_ws`es
   backend `@info/@warn/@error` as `{type:"server:log",level,message,ts}` → pushed into the same `log`
   store. Bounded server-side ring buffer + `GET /api/logs/recent` so a freshly-opened console
   backfills the last N lines (mirrors how the task console reconciles from `GET /api/tasks`).
7. **Status polling** is ephemeral UI state → plain `ref` + `setInterval(~4s)` cleared on unmount
   (NOT persisted view state); refresh immediately after each action.
8. **Dev vs prod is auto-detected**, no installer changes. The `dev` pixi task sets `CECELIA_DEV=1`;
   the packaged app (`pixi run app` → `app.py` → `julia src/server.jl`) and `pixi run prod` never do,
   so absence = prod. Exposed via `GET /api/diagnostics` → `dev`. End users (prod) see the whole
   System panel — status, Quit, napari/notebooks controls, Open console — EXCEPT the backend
   **Restart**, which is dev-only (gated on `diag.dev`). A small "dev" badge marks a dev server.

## Phases

- **Phase 1 (high value, low risk):** System section + status polling (all three) + napari & notebooks
  Start/Stop/Restart (existing endpoints) + global **Quit** (`/api/app/shutdown`). Delivers the
  transparency and the napari-reload win.
- **Phase 2 (console window):** `server:log` tee + ring buffer + `GET /api/logs/recent`, shared console
  component, `#console` window mode, "Open console" button.
- **Phase 3 (fragile, cross-platform):** Application **Restart** — `/api/app/restart` + detached
  relauncher + `CECELIA_RELAUNCH_CMD` in the pixi tasks (+ win-64 variants).

## Files

- `frontend/src/modules/SettingsModule.vue` — "System" section (grid rows: name·status·port·actions,
  polling, Quit confirm, Open console, read-only Frontend/GUI row, ports). Status→verb logic →
  `frontend/src/utils/serviceStatus.ts` (+ `.test.ts`).
- `frontend/src/stores/appControl.ts` *(new)* — shared app-level actions (`quit`, `restartBackend`,
  `dev`, `busy`) used by BOTH the Settings panel and the sidebar footer (one implementation).
- `frontend/src/components/AppSidebar.vue` — bottom-left footer: Quit (all) + Restart backend (dev only).
- `frontend/src/components/ErrorConsole.vue` — `fill` prop → the one reusable console (docked + window).
- `frontend/src/App.vue` — `#console` full-window `bare` mode.
- `frontend/src/stores/ws.ts` — handle `server:log` → `log` store.
- `api/src/app_api.jl` *(new)* — `api_app_shutdown`, `api_app_restart` (exit-42 sentinel), `_can_restart`,
  `_stop_children_for_exit` (graceful + kill-by-port fallback).
- `api/dev.jl` *(new)* — dev supervisor loop (relaunch on exit 42). `app.py` — same loop for prod.
- `app/src/tasks/scheduler.jl` — `_kill_listeners_on_port` (next to `_kill_tree`) → zombie-free Quit.
- `api/src/server.jl` — includes + routes (`/api/app/{shutdown,restart}`, `GET /api/logs/recent`) +
  `server:log` logger tee + ring buffer. `api/src/repl_api.jl` — `dev`/ports in `/api/diagnostics`.
- `pixi.toml` — `dev` task runs `dev.jl` with `CECELIA_DEV` + `CECELIA_SUPERVISED`.
- Docs: `docs/UI.md`, `docs/API.md`, `docs/TODO.md`. Tests: `serviceStatus.test.ts`,
  `api/test/runtests.jl` (app lifecycle + diagnostics ports).

## Non-goals

- **Browser-close → quit. DECIDED AGAINST (2026-07-09).** Considered auto-shutdown when the last WS
  client disconnects (a ~15s grace window survives refresh — the robust alternative to a footgun
  `beforeunload`). Dom chose **explicit Quit only**: closing the browser just disconnects; the backend
  keeps running. To stop the app: the Quit button, or closing `app.py`'s window in prod. Don't
  re-raise without a new ask.
- Frontend (Vite) *controls* — but its port IS shown (read-only "Frontend (GUI)" row) so the full
  picture of occupied ports (8080/5173/7655/7660) is visible; Quit/Restart free the child ports by
  force (`_kill_listeners_on_port`) so no zombies survive even an adopted/crashed bridge.

## Verification

Open Settings → pills reflect reality; kill the bridge → Napari flips to Stopped within ~4s → Start →
Running. Edit `napari_bridge.py` → Restart → reopen image → new code live (timelapse selection
respects the frame; no `configure_autosave` warning). Notebooks Start/Stop/Restart cycle. Open console
→ separate window streams live logs incl. server `@info/@warn`; docked bar + window are the same
component. Quit → server exits. Restart (Phase 3, via `pixi run dev`) → relauncher brings it back;
disabled when the env var is absent. `pixi run test-frontend` + `test-api` green; `vue-tsc` clean.
