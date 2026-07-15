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
install.sh / install.ps1  (bootstrap installer — one command)
  ├── installs Pixi + Juliaup if missing
  ├── downloads the release bundle (app + prebuilt frontend) → ~/.local/share/cecelia
  ├── pixi install  +  julia --project=api instantiate   (provision the env on this machine)
  └── creates a desktop launcher → runs `pixi run app`

Desktop launcher → app.py  (Python, in the env)
  ├── starts → Julia API server (Cecelia.jl + HTTP/WebSocket) on :8080
  │               └── serves the built Vue frontend AND /api + /ws (same origin)
  │               └── spawns Python analysis subprocesses (Cellpose, btrack, napari…) from the env
  └── opens → the user's default browser at http://localhost:8080
```

The window is just the **user's default browser** pointed at `localhost:8080` — no bundled WebView,
no second language. The "installer" is a small **bootstrap script** (`install.sh` / `install.ps1`)
that provisions the env with Pixi + Juliaup; the "shell" is `app.py`, a tiny Python launcher in that
same env.

**Why this shape.** The hard part of distribution is provisioning the multi-GB Julia + Python(+CUDA)
environment on the user's machine; Pixi (env) + Juliaup (Julia) solve that reproducibly, and `app.py`
+ the server serving its own frontend gives the window for free. We don't compile native per-OS
installers: the release **bundle is OS-independent** (the prebuilt frontend is static; Julia/Python
are provisioned per-OS at install), so only the install *script* differs per OS. A fully-graphical
double-click installer (e.g. conda `constructor`) is a possible later polish — its cost is per-OS
conda packaging work; the bootstrap script is the v1. The one tradeoff vs a graphical installer: a
single terminal command at install time (like Miniforge/conda itself).

---

## Runtime flow (every launch)

```
User clicks the desktop icon
  → the desktop launcher runs `app.py` (in the env)
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

### First-run configuration (setup wizard)

An installed app has no `custom.toml`, so on first launch the backend reports `setupRequired` (via
`/api/diagnostics`) and the frontend redirects to a `/setup` wizard that asks only for a projects
directory, then writes it to the user's `custom.toml`. **Config is always per-user and lives at a
fixed location — `~/.cecelia/custom.toml`** (`%USERPROFILE%\.cecelia\custom.toml` on Windows) —
resolved by `Cecelia.config_dir()`, which never depends on install scope (so a future system-wide
install needs no config-path changes). In a dev checkout, `CECELIA_DEV_DIR`/`.env` overrides that to
the dev dir. The wizard removes the old terminal-editing barrier for non-technical users. Full design
+ the deferred system-wide install: `docs/todo/ONBOARDING_PLAN.md`.

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

### Installer scripts + release bundle
`install.sh` (Linux/macOS) and `install.ps1` (Windows) **are** the installer: they bootstrap Pixi +
Juliaup, download the release bundle, provision the env, and create the desktop launcher. The bundle
(`cecelia.tar.gz`) and the scripts are published as **GitHub Release assets** by
`.github/workflows/release.yml` on each `v*` tag. See **Building & releasing** below.

---

## Building & releasing

`.github/workflows/release.yml` runs on a `v*` tag — one `ubuntu-latest` job, because the bundle is
OS-independent:
1. `npm ci && npm run build` → prebuilt `frontend/dist`.
2. `tar` a portable bundle `cecelia.tar.gz` (api, app, **python** (the `cecelia` helper package),
   app.py, pixi.toml, pixi.lock, napari/napari_bridge.py, frontend/dist, install scripts, README,
   docs). It excludes `.pixi`/`node_modules`/`.CondaPkg` — those are provisioned/regenerated on the
   user's machine. **Note:** `python/` must ship — `run_py` resolves task scripts under
   `python/cecelia/` and the editable `cecelia` dep points there; omitting it breaks every Python
   task on the installed app. The bundle is **~6 MB** (frontend/dist dominates).
3. Publish a GitHub Release with `cecelia.tar.gz` + `install.sh` + `install.ps1` as assets.

**bioformats2raw is NOT in the bundle.** The image-import binary (its `lib/` of Bio-Formats JARs +
native codecs) is ~190 MB — it would have dwarfed the ~6 MB of actual app. Instead `install.sh` /
`install.ps1` fetch the latest bioformats2raw release into `<install>/bioformats2raw/` at install
time (skipped if one is already on PATH), where `bioformats2raw_bin()` (config.jl) resolves it. Java
comes from the Pixi env, so only the JARs are downloaded. This also means the **`dev` channel now
gets bioformats2raw too** — the branch tarball never contained it (it's not in git), so before this
the dev channel had no working image import.

Users bootstrap the installer from `raw.githubusercontent.com/…/main/install.{sh,ps1}` — **not**
`releases/latest/download/…`. GitHub's `releases/latest` endpoint only ever resolves to a
*non-prerelease* release, so while we're on release candidates (all `v*-rcN` marked prerelease) it
404s; the raw path on `main` is always available. `install.{sh,ps1}` then resolve the newest
published release themselves via the GitHub API (`/repos/…/releases`, newest first — *includes*
prereleases) and download that tag's `cecelia.tar.gz`. `CECELIA_VERSION=v0.1.0-rcN` pins a specific
tag. Once a stable (non-prerelease) release is cut, `releases/latest` also starts working, but the
API-resolve path keeps working for RC-only states — so no installer change is needed at that point.
They then install Pixi + Juliaup if missing, run `pixi install` (which resolves the GPU variant
per-OS from `pixi.toml`'s platform-gated torch) + `julia --project=api instantiate`, and create the
launcher.
**First-run size:** the env is multi-GB and downloads at install time. `install.ps1` is authored but
**not yet verified on Windows hardware** — the first real test is a Windows machine / runner.

### Install channels (stable vs dev)

`CECELIA_CHANNEL` selects *what* the installer fetches; the provisioning (Pixi + Juliaup +
`julia instantiate`) is identical either way.

| `CECELIA_CHANNEL` | Source | Frontend | To update |
|---|---|---|---|
| `stable` (default) | newest tagged Release's `cecelia.tar.gz` | prebuilt (shipped in the bundle) | re-run installer / `pixi run update` / in-app Update |
| `dev` | `main` branch tarball (`archive/refs/heads/<branch>.tar.gz`) | **built locally** (`npm install && npm run build`) | re-run installer (re-downloads current `main`) |

> The dev-channel local build uses `npm install`, **not** `npm ci`: the rolldown native binding vite 8
> bundles with is an *optional* dep, and npm can silently skip it on Windows/macOS (a non-fatal optional
> download/cache miss, npm/cli#4828), leaving the build with "Cannot find native binding". `npm install`
> re-resolves optional deps for the host and is far less prone to it. `release.yml` keeps `npm ci`
> because it only ever runs on `ubuntu-latest`, where the current-platform binding installs reliably.

**Why a dev channel.** So testers can track HEAD without a tag being cut every couple of days. GitHub
serves any branch as a tarball at `archive/refs/heads/<branch>.tar.gz` — no release, no asset upload —
so the dev path just points the *same* installer at that URL. `CECELIA_BRANCH` overrides the branch.

**The one extra requirement: Node.** A release bundle ships a prebuilt `frontend/dist`; a branch
archive is source only, so the dev channel builds the frontend on the machine and therefore needs
Node.js (npm) on PATH — the installer errors clearly if it's absent. (Node stays on fnm in dev and is
baked into the release bundle for stable, so this is the one case an end user needs it — see *Julia and
Node are not in Pixi*.) The multi-GB Pixi env is cached across re-runs, so a dev "update" only
re-downloads the few-MB source and rebuilds the frontend (seconds), not the environment.

**Provenance.** Both channels write `<install>/.cecelia-version` — the tag for stable, `dev @ <branch>
<sha>` for dev (the SHA resolved via the commits API) — so a bug report can name the exact state. The
server reads it back via `GET /api/diagnostics` (`_installed_version()`; a plain source checkout with no
such file reports `dev (source checkout)`) and it shows as the **Version** row in Settings → Diagnostics.

The branch archive wraps everything in one `<repo>-<branch>/` dir, so the dev extraction uses
`tar --strip-components=1`; the flat release bundle must not (it would hoist `api/`'s contents to the
root). This is the only structural difference between the two paths.

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

### cvxopt is pinned from conda-forge (not PyPI)
`cvxopt` is a transitive dependency of `btrack`. PyPI ships **no macOS-arm64 wheel** for it, so a
PyPI-only solve falls back to the sdist and tries to compile the UMFPACK extension — which needs
SuiteSparse headers (`umfpack.h`) that aren't on a stock Mac, so `pixi install` fails during first
run (reported by a tester on macOS-arm64). It is therefore declared in `[dependencies]`
(`cvxopt = ">=1.3.1"`, conda-forge, prebuilt on every platform); pixi maps the conda package onto
btrack's PyPI requirement so it is never built from source. This pulls the SuiteSparse stack
(`libumfpack`, `suitesparse`, …) into the lock as conda packages.

### napari-animation is PyPI, not conda-forge
The in-viewer movie recorder (`docs/NAPARI.md` → *Animation recorder*) is **napari-animation**, declared
in `[pypi-dependencies]`. The conda-forge build resolves numpy to ≥2.1, which violates the
`cellpose==3.1.1.2` pin (`numpy<2.1`) and makes `pixi add`/`install` unsolvable. The PyPI resolve is
constrained alongside cellpose, so numpy stays at 2.0.x and the recorder's own deps (imageio-ffmpeg for
export, superqt, npe2) install cleanly. This is the mirror of the cvxopt case above: there conda-forge
*avoids* a source build, here PyPI *avoids* a pin conflict — the right channel is per-package.

### GPU / RAPIDS is parked (CUDA-only)
Leiden clustering ships CPU-only via `leidenalg` (cross-platform). The GPU backend (RAPIDS) is
CUDA-only and lives as a commented `[feature.gpu]` stub in `pixi.toml`; when un-parked it becomes a
separate env so non-CUDA platforms fall back to `leidenalg`. GPU is auto-detected at runtime, never a
task param (`CLAUDE.md`). See `docs/todo/CLUSTERING_PLAN.md`.

### Notebook Playground sysimage (`pluto/deps.so`)
The Notebooks feature (`docs/NOTEBOOKS.md`) runs Pluto in its own Julia env (`pluto/`, separate from
`app`/`api` — the API server must not carry the Makie plot stack). Makie's time-to-first-plot is
~20 s cold; a **PackageCompiler sysimage kills it** (measured 32 s → 7.6 s cold-start). Two builds,
both → `pluto/deps.so` (git-ignored, ~1.4 GB, ~10 min):
- **Dev** — `pixi run notebooks-sysimage` — deps only (Makie/CairoMakie/AoG/DataFrames/HDF5/HTTP/CSV),
  deliberately **excluding Cecelia** so Revise still hot-reloads it.
- **Release** — `pixi run notebooks-sysimage-full` — also bakes in `Cecelia` + `CeceliaNb` (code is
  frozen) for near-instant first plot AND first `pop_df`.

**End users never run either task.** An **Enable fast plots** button on the Notebooks page builds
`deps.so` in a background process (a banner explains the one-time slow-first-plot), so the
build-on-demand path works with zero packaging effort — it's the baseline. A sysimage can't be
committed or shipped as one universal
artifact: it's native code tied to the exact **platform/arch + Julia + package versions**. So each
build stamps `deps.so.stamp` (`{julia, hash(Manifest.toml)}`, `pluto/sysimage_stamp.jl`); after an
update the image is detected **stale** and rebuilt automatically, and `launch.jl` refuses to hand a
stale image to workers (a Julia-version mismatch would break them). *Optional* release optimisation
(TODO #00070): build the `-full` image per platform in CI and ship it in the bundle so even the first
open is instant — it falls through to the on-first-run build wherever no prebuilt image is present.
Because the image is stamped, a shipped one that predates the user's Julia/deps still self-heals.

The Pluto server keeps its **secret token ON** (Pluto's secure default): it's a browser-reachable
code-execution surface, so the secret guards against other local sites/processes driving it (CSRF/RCE).
`launch.jl` writes the session secret to `pluto/.plutosecret` (git-ignored) and the API threads it into
the URLs. Do not bind it to a public host. Launched/stopped like napari: `pixi run notebooks` (port
7660) / `pixi run stop-notebooks`, and the app launches it via `POST /api/notebooks/launch`.

---

## Roadmap

**Phase 1 — done.** Pixi env (committed `pixi.toml`/`pixi.lock`, relocated out of `napari/`, verified
to build + import); `pixi run` tasks replace the old shell scripts; the Julia server serves the built
frontend at same-origin `:8080` (verified); the `app.py` launcher + `pixi run app` exist.

**Phase 2 — installer + releases — done.** `install.sh` / `install.ps1` (bootstrap Pixi + Juliaup,
download the bundle, provision, add a desktop launcher) and `release.yml` (build bundle → GitHub
Release on `v*` tag). Linux verified; macOS/Windows scripts authored, to be confirmed on those OSes.

**Phase 3 — in-app update + release polish.** Repo migration (`schienstockd/cecelia` ← new package,
`…/cecelia-legacy` ← old R version); wire a Vue "Update" control + `/api/update` to check GitHub
Releases, update, and prompt restart (console `pixi run update` already exists). Optional: a
fully-graphical double-click installer (conda `constructor` / `pixi-pack`) if the one-time terminal
command proves a barrier for users.

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
