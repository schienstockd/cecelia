# Cecelia Feijoa — Installation Guide

Supports: Linux, macOS, Windows. Designed to run natively on all three — no Docker.

Tested on: Ubuntu 24.04, NVIDIA RTX 2000 Ada (8 GB VRAM), driver 580 / CUDA 13.0.

---

## Prerequisites

- NVIDIA GPU with driver ≥ 525 (Linux/Windows) **or** Apple Silicon / Intel Mac
- Python 3.11 or 3.12 — system install is fine, uv manages the venv
- git
- curl (Linux/macOS) or PowerShell (Windows)

Check your driver version and max supported CUDA:
```bash
nvidia-smi          # Linux / Windows
```

---

## 1. Install Pixi  (dependency manager)

Pixi manages the whole Python analysis env from a single committed lockfile
(`pixi.toml` + `pixi.lock`) — one `pixi install` resolves and installs everything, no
manual pip/venv steps. (Pixi uses uv internally for PyPI resolution, so you keep uv's
speed.) It installs into `~/.pixi` and does not conflict with system Python.

**Linux / macOS:**
```bash
curl -fsSL https://pixi.sh/install.sh | bash
source ~/.bashrc   # or open a new terminal
```

**Windows (PowerShell):**
```powershell
powershell -ExecutionPolicy ByPass -c "irm -useb https://pixi.sh/install.ps1 | iex"
```

Verify:
```
pixi --version
# pixi 0.71.x or newer
```

---

## 2. Install Julia  (via juliaup)

juliaup is Julia's official version manager. It handles Julia upgrades cleanly
and works identically on all platforms.

**Linux / macOS:**
```bash
curl -fsSL https://install.julialang.org | sh -s -- --yes
source ~/.bashrc
```

**Windows (PowerShell):**
```powershell
winget install julia -s msstore
```
Or download the juliaup installer from https://julialang.org/downloads/

Verify:
```
julia --version
# julia version 1.12.x or newer
```

---

## 3. Install Node.js  (via fnm)

fnm (Fast Node Manager) is cross-platform — the same tool and CLI on Linux,
macOS, and Windows. Do not use nvm (Linux/macOS only).

**Linux / macOS:**
```bash
curl -fsSL https://fnm.vercel.app/install | bash
source ~/.bashrc
fnm install --lts
```

**Windows (PowerShell):**
```powershell
winget install Schniz.fnm
# then open a new terminal:
fnm install --lts
```

Verify:
```
node --version   # v24.x.x
npm --version    # 11.x.x
```

---

## 4. Python environment  (analysis stack)

One command resolves and installs the entire Python env from the committed lockfile —
Cellpose, napari, btrack, scanpy, anndata, zarr, PyTorch and the rest. The right PyTorch
build is selected per platform automatically (CUDA cu124 on Linux/Windows, MPS/CPU on
macOS); no `--index-url` juggling.

```bash
# from the cecelia-feijoa project root:
pixi install
```

The env lives at the repo-root `.pixi/` — it is the engine's env (shared by all tasks and
the napari bridge), not napari-specific.

Verify the env and GPU:
```bash
pixi run python -c "import torch, napari, cellpose; print('cuda', torch.cuda.is_available())"
# Linux/Windows + NVIDIA: cuda True   |   macOS: use torch.backends.mps.is_available()
```

> Always run the stack through `pixi run` (`pixi run dev`, `pixi run napari`, …) so the
> Julia server's Python subprocesses use this env. The design rationale — the cellpose-v4
> floor, the `coastal` git dependency, GPU/RAPIDS being parked, the run-via-`pixi run`
> model — lives in `docs/SHIPPING.md`, not here.

> **First cellpose run downloads weights.** Cellpose 4 fetches `cpsam_v2` (~1.2 GB) from
> HuggingFace into `~/.cellpose/models` on first use. Set `CELLPOSE_LOCAL_MODELS_PATH` to point
> elsewhere, or to a pre-seeded directory on a machine with no internet access.

### Don't move or rename the checkout after installing

The Pixi env is **not relocatable** — `pixi install` bakes the absolute prefix into generated
files. Renaming or moving the checkout leaves them pointing at the old path, and nothing errors
loudly: console scripts in `.pixi/envs/default/bin/` get an unusable shebang, and
`etc/fonts/fonts.conf` points at font directories and a font cache that no longer exist (which
surfaces as napari rendering its UI in the wrong font).

If you move the checkout, rebuild the env from the committed lockfile:

```bash
pixi run stop          # the backend and napari run from this env
rm -rf .pixi
pixi install
```

Safe and reproducible — `pixi.lock` is committed, and nothing user-owned lives in `.pixi`
(projects, models and `bioformats2raw` all sit outside it). Verify:

```bash
grep -rIl <old-directory-name> .pixi/envs/default | wc -l   # expect 0
pixi run test-py
```

The same applies to a **git worktree**: it is a different path, so it needs its own
`pixi install` (see `docs/DEV.md`).

---

## 5. Julia packages

From the project root:
```bash
pixi run julia-instantiate
# equivalently: julia --project=app -e 'using Pkg; Pkg.instantiate()'
```

Reads `app/Project.toml` and installs all Julia dependencies. Julia itself is provided by
juliaup (step 2), not Pixi.

---

## 6. Frontend dependencies

```bash
cd frontend
npm install
```

---

## First launch — projects directory

You do **not** hand-edit `custom.toml`. On first launch, if no config exists, Cecelia opens a
one-screen setup wizard in the browser that asks where to store your projects and writes it for you.
Config is per-user at `~/.cecelia/custom.toml` (`%USERPROFILE%\.cecelia\custom.toml` on Windows). In a
dev checkout, `cecelia-feijoa/.env` (`CECELIA_DEV_DIR`) overrides that to your dev dir. The *why*
and the config-resolution rules live in `docs/SHIPPING.md` and `docs/todo/ONBOARDING_PLAN.md`.

## Shared / lab machines (system-wide install)

For one shared install serving every account, pass `CECELIA_INSTALL_SCOPE=system` (needs root /
Administrator). It installs to `/opt/cecelia` · `/Applications/cecelia` · `%ProgramFiles%\cecelia`
with a shared runtime; config + projects stay per-user (`~/.cecelia`), and updates are admin-only.
See `docs/SHIPPING.md` → *Install scope* for the full model and its verification status.

## Custom install location

`CECELIA_HOME=<dir>` overrides the default install directory in either scope — the whole runtime
(Pixi, Juliaup, the multi-GB env) lands inside it. Leading `~` / `~/…` (Unix) and `~` / `~/…` / `~\…`
(Windows PowerShell) are expanded by the installer, so quoted-tilde values still work.

```sh
# Linux / macOS — user scope, custom directory
curl -LsSf https://raw.githubusercontent.com/schienstockd/cecelia/main/install.sh | CECELIA_HOME=~/apps/cecelia sh

# Windows — PowerShell
$env:CECELIA_HOME='D:\apps\cecelia'
irm https://raw.githubusercontent.com/schienstockd/cecelia/main/install.ps1 | iex
```

The desktop entry (`cecelia.desktop`), `/Applications/Cecelia.command` and the Start Menu shortcut
all embed the resolved `INSTALL_DIR`, so they keep working when `CECELIA_HOME` is set — no manual
`cd` needed. Config and projects still live in `~/.cecelia`; only the install moves.

---

## Platform notes

### Windows path separators
Julia uses `joinpath()` throughout — never hardcoded `/` — so paths work on
Windows without changes. If you hit a path issue, report it as a bug.

### Windows subprocess signals
Task cancellation sends SIGTERM to Python subprocesses. Python on Windows handles
SIGTERM via its default handler (process exits cleanly). If a task does not cancel
within 5 seconds, Julia escalates to forceful termination.

### macOS GPU
MPS (Metal Performance Shaders) is the Apple Silicon GPU backend for PyTorch.
Cellpose 4.x supports MPS. Use `torch.backends.mps.is_available()` to check.

### Napari on headless / SSH
Napari requires a display **and a working OpenGL context**. `QT_QPA_PLATFORM=offscreen` gives the
first but not the second — Qt's offscreen platform plugin provides no GL context, so napari cannot
render at all (measured: `offscreen`, `minimal` and `vnc` all fail to create one). `eglfs` is also
not an option with the PyQt5 wheel we ship: it carries no EGL device integration plugins
(`EGL device integration plugin keys: ()`), so it falls back to an integration that requires
`/dev/fb0`.

What works is a real X server with the `xcb` platform — `Xvfb`, or `Xorg` in a headless config:
```bash
export QT_QPA_PLATFORM=xcb
export DISPLAY=:99            # e.g. Xvfb :99 -screen 0 1920x1080x24
```
Note that `Xvfb` alone renders in software (`llvmpipe`), which is too slow for 3D volumes; hardware
GL needs VirtualGL or a GPU-backed X server. On a hybrid-graphics Linux box, also set the discrete-GPU
env (see `app/src/napari.jl`) or the renderer silently falls back to `llvmpipe`.

Cecelia is designed for local desktop use and is not tested headless.

### Running on a remote server (SSH tunnel)

Cecelia can run on a headless Linux VM (e.g. Google Cloud Compute Engine) and be reached from a
laptop over an SSH tunnel. The install path is the same as a local Linux install; the differences are
sizing, one shell wrapper, and the fact that **the port must never be opened directly**.

**Security up front.** The HTTP/WS server binds `127.0.0.1` by default and has **no authentication**;
CORS is `*` and every route — projects, tasks, image data — is reachable to whoever gets a socket.
Anything you expose on a public firewall rule is world-readable and world-writable. SSH-tunnel it.
Do not set `CECELIA_HOST=0.0.0.0` and do not open TCP 8080 in the cloud firewall.

**VM sizing.**
- **Boot disk ≥ 50 GB SSD.** The pixi env (Julia + Python + torch/cu124) is a few GB, bioformats2raw
  adds ~190 MB, first-run cellpose weights add ~1.2 GB. GCE's 10 GB default fails partway through
  install.
- **`linux-64` architecture.** `pixi.toml`'s `platforms` list is `linux-64, win-64, osx-arm64` — no
  `linux-aarch64`, so ARM VMs (GCE `t2a-*`) will not solve the env. Use `n1-*`, `n2-*`, `e2-*`, `c3-*`.
- **NVIDIA driver ≥ 525** for the cu124 wheels. L4/T4/V100/A100 all work. Without a GPU cellpose
  silently falls back to CPU (see `torch.cuda.is_available()`), which is 50–100× slower — not an
  error, just slow. Verify with `nvidia-smi` before you trust segmentation timings.
- Start at ≥ 8 vCPU. First-run precompile on a 2-vCPU VM takes long enough to look hung.

**Environment adjustments.** After `install.sh` finishes:
- Re-source `~/.bashrc` (or open a new shell) — juliaup adds `~/.local/bin` to `PATH`, and `pixi`
  installs its own shim there. Without this, `pixi run app` won't find `julia`.
- `~/.cecelia/` is the default config + projects root. If the boot disk is small, mount a persistent
  disk and set `projects_dir` in `~/.cecelia/custom.toml` (or in the first-run wizard, which you
  reach through the tunnel).
- Leave `CECELIA_HOST` unset — the loopback default is what makes the tunnel model safe.

**Run under a persistent shell.** `pixi run app` is a foreground process supervising Julia; an SSH
drop kills it. Use `tmux` (or a systemd unit if you set one up):
```bash
tmux new -s cecelia
pixi run app        # Ctrl-b d to detach; `tmux attach -t cecelia` to come back
```
The startup will call `webbrowser.open()` and `xdg-open` will fail silently — that is expected on a
headless VM; you'll open the browser on the laptop side.

**Tunnel from the laptop:**
```bash
ssh -L 8080:localhost:8080 <vm>
# then, in a laptop browser:
open http://localhost:8080
```

**Napari-driven flows.** Anything that opens napari on the VM is out of scope for this setup — see
*Napari on headless / SSH* above. The browser viewer (`docs/todo/WEB_VIEWER_PLAN.md`) is the intended
remote path; treat a headless VM run today as a pipeline-only test (import → segment → cluster →
analysis board / notebooks).

---

## Summary of installed versions  (initial Linux setup)

| Tool | Version |
|---|---|
| pixi | 0.71.1 |
| Julia | 1.12.6 |
| Node.js | 24.16.0 |
| Python (pixi env) | 3.12 |
| torch | 2.6.0+cu124 |
| napari | 0.7.1 |
| cellpose | 4.2.1.1 |
| zarr | 3.2.1 |
| anndata | 0.12.17 |
| scanpy | 1.12.1 |
| leidenalg | 0.12.0 |
| websockets | 16.0 |
