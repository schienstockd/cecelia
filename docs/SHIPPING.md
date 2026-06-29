# Shipping & Distribution

How Cecelia is packaged, installed, and updated for end users.

This is both a description of the current setup and a roadmap toward the target one. Parts of the
target (the graphical `constructor` installer, the desktop shortcut, in-app update) are not built
yet; those are called out as such. The dependency layer (Pixi) and the same-origin server are in
place. When you change install-related code (`pixi.toml`, `app.py`, the static-serving in
`api/src/server.jl`, the installer config), keep this document in sync.

---

## Target user experience

A biologist should be able to:
1. Download an installer for their platform
2. Run it — no terminal, no package managers, no manual dependency steps
3. See a Cecelia icon on their desktop
4. Click it to open Cecelia
5. Get a prompt when a new version is available and update it

This is the standard desktop-application experience. Every layer of the distribution stack exists
to achieve it — without adding a heavy GUI runtime (no Electron, no Tauri/Rust, no bundled browser).

---

## Stack

```
Desktop shortcut  (created by the installer via menuinst)
  └── app.py launcher  (Python, runs in the env)
        ├── starts → Julia API server (Cecelia.jl + HTTP/WebSocket) on :8080
        │               └── serves the built Vue frontend AND the /api + /ws endpoints (same origin)
        │               └── spawns Python analysis subprocesses (Cellpose, btrack, napari…) from this env
        └── opens → the user's default browser at http://localhost:8080

Pixi / conda  (install-time dependency manager)
  └── one env: Julia runtime + Cecelia.jl + the full Python analysis stack + the built frontend

constructor  (build-time: turns the conda env into a native graphical installer + desktop shortcut)
```

The window is just the **user's default browser** pointed at `localhost:8080`. There is no bundled
WebView and no second language: the only "shell" is `app.py`, a small Python launcher that lives in
the same env as everything else. The native **installer + desktop icon** come from conda
`constructor` (the tool behind the Anaconda/Miniforge installers) with `menuinst` shortcuts.

**Why this shape.** The genuinely hard part of distribution is provisioning the multi-GB
Julia + Python(+CUDA) environment on the user's machine; Pixi/conda solves that reproducibly. The
window is cheap, so we don't pay for a bundled browser or a Rust/JS shell to get one — we reuse the
system browser and serve the frontend from the server we already run.

---

## Runtime flow (every launch)

```
User clicks the desktop icon
  → menuinst shortcut runs `app.py` (in the env)
  → app.py spawns the Julia server (`julia --project src/server.jl`, production mode)
  → Julia loads Cecelia.jl, starts HTTP + WebSocket on :8080, serves frontend/dist + /api + /ws
  → app.py polls http://localhost:8080/api/health until {ok:true}
  → app.py opens the default browser at http://localhost:8080
  → Cecelia is running
  → User closes the launcher window → app.py SIGTERMs the Julia server → clean shutdown
```

The same-origin design (server serves the frontend) is what removes all the friction: the frontend
calls `/api/...` and `ws://<host>/ws` relatively, so it works identically whether served by Vite in
dev (:5173, proxying to :8080) or by the Julia server in production (:8080). No CORS, no API-base
configuration, no second process for the user.

---

## Components

### Pixi / conda environment
The engine env (Julia + Python analysis stack + frontend build deps). Managed by `pixi.toml` /
`pixi.lock` in dev; the installer ships the resolved env. See **Engine Python environment** below.

### `app.py` launcher
The single entrypoint behind `pixi run app` and the desktop shortcut. Starts the server, waits for
`/api/health`, opens the browser, and shuts the server down when closed. Pure Python, runs in the
env. ~70 lines, no logic beyond launch/health/open/teardown.

### Julia server serves the frontend
`api/src/server.jl` serves `frontend/dist` for any non-`/api` GET (with SPA fallback to
`index.html` for client routes), alongside `/api/*` and `/ws`. Build the frontend with
`pixi run build` first. In dev you still use the Vite server; if `dist/` is absent the static
handler no-ops and requests fall through to the API router.

### constructor installer + menuinst shortcut  *(to build)*
`constructor` builds a per-platform graphical installer (`.pkg` / `.exe` / `.sh`) from conda
packages, with a progress UI and a `menuinst` desktop/Start-Menu shortcut that runs `app.py`. See
**Building installers**.

---

## Building installers  *(per-platform, not yet built)*

`constructor` consumes a `construct.yaml` and must run **on each target OS** to produce that OS's
installer (so this belongs in a CI matrix, like any installer). Key considerations:

- **conda vs PyPI.** `constructor` builds from conda channels, but our stack is mostly PyPI (torch
  cu124, napari, cellpose, btrack, scanpy via pip). Options: (a) move those to conda-forge in the
  installer build, or (b) use **`pixi-pack`** (pack the resolved pixi env — conda + PyPI — into a
  portable bundle) and wrap it with a thin installer + menuinst shortcut. `pixi-pack` is the
  pixi-native path and avoids re-expressing the env in pure conda; evaluate it before committing to
  raw `constructor`.
- **What ships:** the resolved env, the Cecelia Julia source + an instantiated/precompiled depot,
  the built `frontend/dist`, and `app.py`. The shortcut command is `app.py` (via the env's python).
- **GPU variant** chosen at install (CUDA on Linux/Win, MPS/CPU on macOS), mirroring `pixi.toml`'s
  platform-gated torch.
- **First-run size:** the env is multi-GB; the installer's progress UI makes the download/expand
  feel normal.

---

## Updates

| Path | Mechanism |
|---|---|
| Console | `pixi run update` → `pixi update` (refreshes deps within `pixi.toml` constraints, rewrites `pixi.lock`) |
| In-app (planned) | A Vue "Update" control → `/api/update`; checks GitHub Releases for a newer version, then updates and prompts to restart |

The **in-app update needs the release infrastructure to exist first** (a versioned release stream to
check against — see *Release repo* below). Until then, `pixi run update` is the supported path and
the console fallback for technical users. `/api/update` + the Vue control are scaffolded against
that future release stream, not a placeholder that silently bumps dependencies.

**Release repo (planned cleanup phase):** updates and the installer will point at
**`https://github.com/schienstockd/cecelia`** as the canonical home of the *new* (Julia + Vue +
Python) package. The existing R/Shiny version currently at that name moves to
**`https://github.com/schienstockd/cecelia-legacy`**. This repo swap is a dedicated cleanup phase and
must precede wiring in-app update to a releases endpoint. GitHub Releases is the single source of
truth for version state; each release ships the platform installers as assets.

---

## Platform targets

| Platform | Installer format | Window | Status |
|---|---|---|---|
| macOS | `.pkg` (constructor) | default browser | primary target |
| Windows | `.exe` (constructor) | default browser | secondary target |
| Linux | `.sh` (constructor) | default browser | tertiary target |

**Multi-user / lab machines:** for a shared install, point the env at a shared location (e.g. set a
system-wide `PIXI_HOME` / conda prefix like `C:\ProgramData\cecelia` on Windows) so the multi-GB env
is shared across accounts rather than reinstalled per-user — the same concern as a shared conda/Docker
install. The menuinst shortcut can be installed per-user or system-wide.

---

## Engine Python environment (Pixi) — current state & non-obvious notes

Source of truth for how the Python env is wired *today*. Read before touching `pixi.toml`,
`python_bin_path()`, `app/src/napari.jl`, or `app.py`.

### It is the engine's env, not napari's
The Python env was historically `napari/.venv`, but it is shared by the whole engine — cellpose,
measure, btrack, the label-props writer, **and** the napari bridge. It now lives at the repo-root
**`.pixi/`** (managed by Pixi), not under `napari/`. `napari/` keeps only `napari_bridge.py`.

### The "run via `pixi run`" rule (no hardcoded env paths)
`pixi run <task>` prepends `.pixi/envs/default/bin` to `PATH`, so `python_bin_path()` stays at its
config default `"python3"` (no `custom.toml` override) and resolves to the Pixi env python inside an
activated run. `app/src/napari.jl` no longer hardcodes a venv path; `NAPARI_PYTHON` routes through
`python_bin_path()`. **Always launch via `pixi run`** (`dev`/`prod`/`app`/`napari`) — launching
`julia` directly outside the env makes subprocesses fall back to system `python3` and fail.

### Julia and Node are not in Pixi (deliberate, for dev)
Pixi scopes to the **Python** env. Julia stays on **juliaup + Manifest**, Node on **fnm**; `pixi run`
inherits PATH so both resolve. For the *shipped* installer, Julia (and the prebuilt frontend) are
baked into the conda env at build time — see **Building installers**.

### cellpose is pinned to v3 — do not bump
`cellpose == 3.1.1.2`. **v4 dropped denoise support, and denoising cannot be excised from v3.**
Bumping to 4.x breaks the denoise path. (INSTALL.md's old `4.2.1.1` table entry was wrong.)

### PyTorch is platform-gated
The cu124 wheel index carries only Linux/Windows wheels, so `torch`/`torchvision` are declared in
`[target.linux-64.*]` / `[target.win-64.*]` with that index and in `[target.osx-arm64.*]` from the
default index (MPS/CPU). A single global cu124 index would make macOS unsolvable.

### coastal is dropped for now
`coastal` (live-cell seg/tracking) was an editable install from a non-git sibling path
(`~/cc-workspace/coastal`), so a committed lockfile can't fetch it. Omitted to keep the env
reproducible; re-add later as an editable path-dep (dev) or a git/PyPI dep (shipping).

### GPU / RAPIDS is parked (CUDA-only)
Leiden clustering ships CPU-only via `leidenalg` (cross-platform). The GPU backend (RAPIDS) is
CUDA-only and lives as a commented `[feature.gpu]` stub in `pixi.toml`; when un-parked it becomes a
separate env so non-CUDA platforms fall back to `leidenalg`. GPU is auto-detected at runtime, never a
task param (`CLAUDE.md`). See `~/cc-workspace/cecelia/CLUSTERING_PLAN.md`.

---

## Roadmap

**Phase 1 — done.** Pixi env (committed `pixi.toml`/`pixi.lock`, relocated out of `napari/`, verified
to build + import); `pixi run` tasks replace the old shell scripts; the Julia server serves the built
frontend at same-origin `:8080` (verified); the `app.py` launcher + `pixi run app` exist.

**Phase 2 — installer.** `construct.yaml` (or `pixi-pack` wrapper) producing per-platform graphical
installers + a menuinst desktop shortcut that runs `app.py`. Built in a CI matrix. GPU variant chosen
at install. *(Note: building installers requires running on each target OS with the system toolchain;
it cannot be done from a single dev box.)*

**Phase 3 — in-app update + release stream.** Repo migration (`schienstockd/cecelia` ←
new package, `…/cecelia-legacy` ← old R version); versioned GitHub Releases; wire the Vue "Update"
control + `/api/update` to check releases, update, and prompt restart.

**Possible future — Julia package registry.** Registering Cecelia.jl in the General Registry would
allow `] add Cecelia` for REPL-only users. Not a priority. See `FUTURE.md`.

---

## Developer build

Setup steps (install Pixi/Julia/Node, `pixi install`, instantiate Julia, `npm install`) live in
`docs/INSTALL.md`. Day-to-day commands:

```bash
pixi run dev              # Julia API server, Revise hot-reload (development) → :8080
pixi run prod             # Julia API server, plain include (production)
pixi run frontend         # Vite dev server (hot reload) → :5173
pixi run build            # frontend → static assets (frontend/dist), required before `prod`/`app`
pixi run app              # one-click: start prod server, wait for /api/health, open the browser
pixi run napari           # napari bridge standalone (normally the backend launches it)
pixi run update           # update the env to latest within pixi.toml constraints
pixi run stop             # stop backend/frontend/napari by port
```

In dev you typically run `pixi run dev` + `pixi run frontend` (Vite, hot reload). To exercise the
shipped same-origin path, `pixi run build` then `pixi run prod` (or `pixi run app`) and use `:8080`.
Running through `pixi run` is what guarantees the Julia server's Python subprocesses use the Pixi env.
