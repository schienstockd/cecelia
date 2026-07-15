# Cecelia Pineapple — Installation Guide

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
# from the cecelia-pineapple project root:
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
> Julia server's Python subprocesses use this env. The design rationale — the cellpose-v3
> pin, the dropped `coastal` dependency, GPU/RAPIDS being parked, the run-via-`pixi run`
> model — lives in `docs/SHIPPING.md`, not here.

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
dev checkout, `cecelia-pineapple/.env` (`CECELIA_DEV_DIR`) overrides that to your dev dir. The *why*
and the config-resolution rules live in `docs/SHIPPING.md` and `docs/todo/ONBOARDING_PLAN.md`.

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
Napari requires a display. On a headless machine set:
```bash
export QT_QPA_PLATFORM=offscreen
```
Note: offscreen disables interactive viewing. Cecelia is designed for local desktop
use and is not tested headless.

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
| cellpose | 3.1.1.2 |
| zarr | 3.2.1 |
| anndata | 0.12.17 |
| scanpy | 1.12.1 |
| leidenalg | 0.12.0 |
| websockets | 16.0 |
