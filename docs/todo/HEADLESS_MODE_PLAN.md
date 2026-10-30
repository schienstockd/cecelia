# Headless mode — Cecelia with Claude as the front end, no GUI

**Status:** parked. Scoped 2026-10-29; P1 is built (d855631f, `mcp/cecelia_mcp/no_gui.py`) and the rest is not started. Prompted by a request from the data science
platform: can they run Cecelia on their own machine without the GUI, with Claude in the terminal as the
way in?

## The ask

A data scientist on their **own local machine** starts Cecelia from a terminal and does the whole
analysis by talking to Claude: create a project, import images, run segmentation, tracking and
behaviour, gate, and read the results. No browser. The output is a normal Cecelia project, so a GUI user
can open it later and see the same thing.

## What already exists

The GUI is only a client. Everything below already runs without it:

| Need | Already there |
|---|---|
| Server without a browser | `app.py` starts `api/src/server.jl`; the browser open is gated by `first` / `CECELIA_LAUNCHER_NO_BROWSER` (`app.py` `main`) |
| Read, design chains, notebooks and boards | `cecelia-observer` (`mcp/cecelia_mcp/server.py`) |
| Start and wait on work, gate | `cecelia-autonomous` (`mcp/cecelia_mcp/autonomous_server.py`): `run_task`, `run_chain`, `wait_for_*`, `gate_histogram` → `add_gate` → `gate_stats` |
| Look at a result | `gate_plot` / `gate_cells_view` render on the server and come back as Image blocks |
| Watch tasks live | `pixi run console` (`api/task_console.jl`), a read-only live task view: it covers the GUI's task manager in a second terminal |
| Proof that it works end to end | `scripts/agent_eval/run_app.py` already runs `claude -p` with only these two servers against a live backend |
| Backend routes for project setup | `/api/projects/create`, `/api/sets/create`, `/api/images/register` (`api/src/server.jl`); the import itself is the `importImages.omezarr` task |

## Gaps

1. **No way to get data in over MCP.** The agent-eval runs copy images in with a script
   (`scripts/agent_eval/app_project.py`). Nothing exposes project, set or image creation as tools.
2. **`cecelia-autonomous` is built as an eval sandbox.** It is locked to one throwaway project
   (`CECELIA_MCP_PROJECT`, required), optionally forces an output prefix (`CECELIA_MCP_PREFIX`), and
   its docs say it is never wired into an interactive session.
3. **No launcher.** Nothing starts the server headless, writes the MCP config and opens `claude`.
4. **The observer carries GUI-only tools.** These are `mark_tracks` / `mark_cells` / `mark_plot` /
   `mark_tile` / `mark_freeform` / `point_at_ui`, `get_recent_captures` / `get_capture` /
   `get_capture_landscape_tiles` and `poll_observations` / `set_observer_active`. Without a GUI they
   point at nothing.

## Locked decisions

- **D1 — command name: `pixi run headless`.** `pixi run console` is taken by the task console
  (`pixi.toml`, cited in `docs/RUNNER.md`, `DEV.md`, `API.md`) and stays as it is. The headless
  launcher's startup message points at it for live task progress.
- **D2 — the lock: the named project plus any project created in the session.**
  `pixi run headless <project>` locks to that project. A project made with `create_project` during
  the session joins the allowed set. Anything else is refused. No output prefix. **With no argument
  the launcher shows a numbered list of projects** (most recently opened first, the same order as
  the observer's `list_projects`) with a "new project" entry at the top. Picking new starts with an
  empty allowed set, so Claude's first `create_project` fills it. The pick happens in the launcher,
  not in the Claude chat, because the lock goes into the MCP server's env before `claude` starts. Reason: unattended,
  the lock and prefix are the only safety net. Interactive, the user approves each tool call, so the
  lock only has to stop Claude from wandering into the user's *other* projects.
- **D3 — the eval sandbox stays as it is.** An unset `CECELIA_MCP_PROJECT` still refuses everything,
  and `run_app.py` keeps its single-project lock and prefix. Headless mode is a separate switch,
  `CECELIA_MCP_NO_GUI=1`, not a loosening of the existing one. Not `…_MODE=headless`: inside the code,
  "headless" already means the app's own `claude -p` turns (`observer_mcp_config(; headless)`), and
  this mode is an interactive session.
- **D6 — Julia builds the session, the launcher only runs it.** The MCP config (`observer_mcp_spec`
  plus a sibling spec for `cecelia-autonomous`), the caller hook (`observer_hook_settings`), the
  `claude` binary (`agent_bin_path`) and the active profile's env (`_claude_env_pairs`) are all
  already canonical in `app/src/ai/agent_runner.jl`. They work on a shipped install, carry the API
  token file, and handle Windows. The server writes the files the way it already writes the
  observer's (`_write_observer_mcp_config` / `_write_observer_hook_settings` in
  `api/src/observer_api.jl`, atomic, into `ensure_config_dir()`), as a no-GUI config next to those
  two, and a route returns their paths plus the `claude` command. The launcher only starts it. `scripts/agent_eval/run_app.py`'s Python `mcp_config` is dev-checkout-only and is not
  reused.
- **D7 — reuse a running server.** If Cecelia is already up for this user (the GUI is open), the
  launcher attaches to it instead of starting a second one, and leaves it running on exit.
- **D4 — interactive `claude`, not `-p`.** The user is at the keyboard, and the normal permission
  prompts apply. No `--dangerously-skip-permissions`.
- **D5 — out of scope:** segmentation correction, the live viewer, movies. These stay GUI-only. The
  headless guidance tells Claude to say so and to name the GUI page instead of trying a workaround.

## Phases

**P1 — data in.** In `autonomous.py`, allow `POST /api/projects/create`, `/api/sets/create` and
`/api/images/register`, and add the session allowed-set (D2). In `autonomous_server.py`, add the
`create_project`, `create_set` and `register_images` tools. The import itself goes through the
existing `run_task("importImages.omezarr", …)`. Name the new tools in `guidance.py` (`GuidanceTest`).
Tests: allow-list, lock across create, prefix off.

**P2 — headless surface.** Under `CECELIA_MCP_NO_GUI`, following the single-reader env switch in
`discovery.py` (`discovery_enabled()`), hide the observer's GUI-only tools (gap 4).
Add a headless block to `guidance.py`: what can't be done (D5), and that `pixi run console` shows task
progress.

**P3 — launcher.** `pixi run headless [<project>]` starts the server with no browser (reusing
`app.py`'s path rather than a second launcher), waits on `/api/health`, shows the project list when
no project was named (D2, read from `/api/projects`), asks the server for the session (D6) with
the lock and the switch filled in, and starts `claude` with
the open project as context (the same shape as `run_app.py`'s `CONTEXT`). On exit it shuts down a
server it started itself (D7). Tests: the Julia session builder (pure), the launcher's pick logic.

**P4 — install path.** The data scientist likely runs a shipped install, not a dev checkout. Decide
how `headless` is reached there: the install scripts' entry point, or the Cecelia Claude plugin
(#1826).

## Rough cost

About five touchpoints: `autonomous.py`, `autonomous_server.py`, `guidance.py` plus the observer
tool filter, a launcher (`app.py` flag plus a `pixi.toml` task), and tests. P1–P3 is a few days. P4
depends on the shipping route.

## Open questions

- **P4:** the install-script entry point or the plugin?
- **Prerequisite:** the `claude` CLI must be installed and logged in. Resolved by D6: the server
  resolves it with `agent_bin_path`, and the launcher stops with an install hint when it is missing.
- **Python users:** a data scientist may later want a scriptable API too (`docs/REPL.md` is Julia).
  That's not in this plan; record it if they ask.
