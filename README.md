# <img src="frontend/public/feijoa.svg" alt="" width="30" align="top"> Cecelia Feijoa

[![CI](https://github.com/schienstockd/cecelia/actions/workflows/ci.yml/badge.svg)](https://github.com/schienstockd/cecelia/actions/workflows/ci.yml)
[![Release](https://github.com/schienstockd/cecelia/actions/workflows/release.yml/badge.svg)](https://github.com/schienstockd/cecelia/actions/workflows/release.yml)
[![License: GPL v3+](https://img.shields.io/badge/License-GPLv3+-blue.svg)](LICENSE)

A Julia package with a graphical interface for cellular image cytometry — import, segmentation,
tracking, gating, behavioural analysis, and clustering of multiplexed and live-cell microscopy
data. It is a ground-up reimplementation of the original R/Shiny
[cecelia](https://github.com/schienstockd/cecelia-legacy) in a Julia + Python + Vue stack.

> ⚠️ **This software was written almost entirely by an AI** ([Claude Code](https://claude.com/claude-code)),
> under Dominik's direction. All scientific validation was done by the human author, and
> it has **not yet been independently tested** by other users — treat early releases accordingly.
> Full methodology and sources are at the [end of this README](#how-this-software-was-built).

- **Developer setup:** [`docs/INSTALL.md`](docs/INSTALL.md) · **Architecture:** [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) · **FAQ (why it's built this way):** [`FAQ.md`](FAQ.md)

---

## Install & run

**Install**, then **run** — Cecelia asks where to keep your projects on first launch, so there's
nothing to configure by hand. Image import works out of the box too — Java ships in the
environment and the installer fetches **bioformats2raw** for you.

The installer sets up [Pixi](https://pixi.sh) + [Julia](https://julialang.org) if they're missing,
downloads the latest release, and provisions the environment (a few GB on first run; later launches
are fast). By default it installs **just for you** (no admin rights, into your account). For a shared
lab machine, see [System-wide install](#system-wide-install-shared-machines) below.

### Linux

Install — in a terminal:
```sh
curl -LsSf https://raw.githubusercontent.com/schienstockd/cecelia/main/install.sh | sh
```
Run — launch **Cecelia** from your applications menu (or `cd ~/.local/share/cecelia && pixi run app`).

### macOS

Install — in Terminal:
```sh
curl -LsSf https://raw.githubusercontent.com/schienstockd/cecelia/main/install.sh | sh
```
Run — open **Cecelia** from `~/Applications` (or `cd ~/.local/share/cecelia && pixi run app`).

### Windows

Install — in PowerShell (no admin rights needed):
```powershell
irm https://raw.githubusercontent.com/schienstockd/cecelia/main/install.ps1 | iex
```
Run — launch **Cecelia** from the Start Menu.

### First launch — pick your projects folder

Cecelia opens in your browser at <http://localhost:8080>. The **first** time, a one-screen setup
wizard asks where to store your projects — type or accept a folder (it's created if it doesn't exist)
and you're done. Image import is ready to use.

Your choice is saved to **`~/.cecelia/custom.toml`** (`%USERPROFILE%\.cecelia\custom.toml` on
Windows) — a per-user file you never have to edit by hand. To move the folder later, delete that file
and relaunch to get the wizard again, or edit `dirs.projects` in it directly.

> **Advanced:** to use your *own* bioformats2raw instead of the bundled one, add
> `bioformats2raw = "/path/to/bioformats2raw"` under `[dirs]` in `~/.cecelia/custom.toml`. Every
> setting is listed in the bundled `app/config.toml`.

### System-wide install (shared machines)

For a shared or lab workstation, install **once as an administrator** and every account uses the same
copy. Pixi, Julia and the environment are provisioned inside the install directory so all users share
one runtime — but each user still gets **their own** projects folder and settings (config always
stays per-user in `~/.cecelia`, so every account runs its own setup wizard on first launch).

Set `CECELIA_INSTALL_SCOPE=system` and run elevated:

| OS | Command | Installs to |
|----|---------|-------------|
| Linux | `curl -LsSf https://raw.githubusercontent.com/schienstockd/cecelia/main/install.sh \| CECELIA_INSTALL_SCOPE=system sudo -E sh` | `/opt/cecelia` |
| macOS | *(same command as Linux)* | `/Applications/cecelia` |
| Windows | in an **elevated** PowerShell: `$env:CECELIA_INSTALL_SCOPE='system'; irm https://raw.githubusercontent.com/schienstockd/cecelia/main/install.ps1 \| iex` | `%ProgramFiles%\cecelia` |

Every account gets a launcher (application menu / Start Menu). Because the files are admin-owned,
**updates are admin-only**: the in-app Update button defers to an administrator, who re-runs the same
command elevated. (The default per-user install can self-update in-app.)

---

## Updating

Re-run the install command for your OS, run `pixi run update` from the install directory, or use the
in-app **Update** button when a new release is available. (In-app update applies to a per-user
install; a system-wide install is updated by re-running the installer as an administrator.)

---

## Monitoring tasks (terminal console)

Long jobs — segmentation, tracking, and whole **chain runs** — are handled by a background scheduler.
Alongside the in-app view, you can watch them from a terminal with a live, **read-only** dashboard:
which tasks are running, their progress, how long each has been going (the real elapsed, even for a task
that was already running when you opened the console), how many are queued, and how many have finished.
It only
*reads* the running Cecelia (it never starts or cancels anything), so it's safe to leave open next to
the app. It also shows a **pools** line — how many concurrent slots each resource pool (cpu/gpu/io/
network) allows and how many are in use right now — so you can see what's saturated at a glance.

With Cecelia running, from the install directory:

```sh
# Linux / macOS
cd ~/.local/share/cecelia && pixi run console
```
```powershell
# Windows
cd $env:LOCALAPPDATA\cecelia ; pixi run console
```

Add `-- --stream` for an append-only log you can pipe to a file (`pixi run console -- --stream | tee run.log`).
Press `Ctrl-C` to close it — your tasks keep running.

---

## Bleeding-edge builds (dev channel)

To run the **current GitHub state** without waiting for a tagged release, set `CECELIA_CHANNEL=dev`.
The installer then downloads the latest `main` instead of a release and builds the frontend locally,
so **[Node.js](https://nodejs.org) (npm) ≥ 20 must be installed**. Re-run the same command to update
to the newest `main`. The installed commit is recorded in `.cecelia-version` for bug reports.

```sh
# Linux / macOS
curl -LsSf https://raw.githubusercontent.com/schienstockd/cecelia/main/install.sh | CECELIA_CHANNEL=dev sh
```
```powershell
# Windows
$env:CECELIA_CHANNEL='dev'; irm https://raw.githubusercontent.com/schienstockd/cecelia/main/install.ps1 | iex
```

Everything else (projects folder, running) is identical to a stable install. Dev builds track HEAD,
so expect the occasional rough edge — for routine use, prefer the default stable channel above.

---

## Adding your own analysis step

Cecelia is extensible without touching the package or rebuilding anything. Drop two files into your
config directory — a JSON describing the form and a Julia file saying what happens on Run — and your
task appears on the page you named, alongside the built-in ones.

Package the same files as a **plugin** and you get a page of your own: the task on one side, plots of
its results below. Plugins install from a URL in **Settings → Plugins**, so they can be shared,
versioned and reinstalled.

[`docs/CUSTOM_MODULES.md`](docs/CUSTOM_MODULES.md) is the guide, and starts with which of the two you
want. Two runnable examples ship in the repo and are loaded by CI on every commit, so copying one is
the route that stays correct:
[`docs/examples/custom-modules/`](docs/examples/custom-modules/) and
[`docs/examples/plugins/`](docs/examples/plugins/).

> Neither is sandboxed — a custom module is arbitrary code with full access to your machine, exactly
> like an R package. Only run what you wrote or trust.

---

## Developing

Running from source with hot-reload (`pixi run dev` + `pixi run frontend`) is covered in
[`docs/INSTALL.md`](docs/INSTALL.md).

---

## How this software was built

This software was developed almost entirely with [Claude Code](https://claude.com/claude-code)
(Anthropic), using the Claude Opus and Claude Sonnet models, under the Garvan Institute of Medical
Research enterprise license. The field hasn't settled on how to develop, disclose, credit,
publish, or validate AI-assisted scientific software — this section is what we did and why,
described precisely enough to be judged. Longer version, with the field context that shaped these
choices, in [`docs/PROVENANCE.md`](docs/PROVENANCE.md).

### Claude's role

Claude wrote essentially all of the code — both the port of the original R/Shiny `cecelia` into
this stack and the newer subsystems that have no direct predecessor (the WebGPU browser viewer,
the offline renderer, the analysis board, the notebook playground, the chain executor). It worked
interactively in the terminal: given a goal and, for the ported parts, the original design, it
explored the old codebase, proposed how to translate it, wrote and revised code against review,
and ran the test suite headlessly.

What it couldn't do is the part that matters most for trust: it had no access to a microscope, the
running GUI, or real imaging output beyond small test fixtures. It couldn't watch a movie play
back, judge whether a segmentation captured the right cells, or confirm that a gate picked the
population it was supposed to. On engineering decisions it was consulted like a colleague with
opinions worth listening to; on scientific decisions it was the implementer, not the judge.

### The human role — direction, and validation on real data

Dominik set every goal and every design decision, and provided the immunology and
intravital-microscopy judgment the analysis has to be correct for. The load-bearing part of that
role — the thing an earlier "reviewed all output" line wasn't specific enough about — is what
"validated" actually means.

The automated test suite checks *code correctness* — that a function does what it's supposed to,
that a pipeline runs to completion, that the data round-trips. It doesn't check *scientific
correctness*: whether a segmentation captured the cells that mattered. Those are separate
questions, and it's easy to conflate them.

The failure mode this discipline exists to catch is that **a fit or optimisation number can look
fine while measuring fit to the wrong thing**. Two cases from Cecelia's own development:

- **Segmentation.** Early on, the segmentation model was being tuned by an accuracy number computed
  without ground truth. On those numbers, temporal smoothing looked unhelpful, and the AI proposed
  dropping it. On real intravital output, the temporal-smoothed model was in fact what captured
  the cells Dominik was analysing — the metric had been optimising something other than the
  biological signal. The final choice was made on what the images looked like, not on the number.
  Confetti data as a proper ground-truth benchmark is a wanted-but-not-yet piece.
- **coastal** (a sibling project that reuses Cecelia's helpers). Same shape in a different
  setting: automated optimisation numbers looked acceptable while the segmentation was missing the
  signal Dominik cared about. Visual inspection caught it.

Different subsystems were validated in different ways:

- The **ported** parts — celltrackR track measures, the btrack pipeline, drift correction — were
  validated by matching the R version's output. This is the same pattern several public AI-assisted
  rewrites use (Seqera's RustQC, Fulcrum's fgumi, Rob Patro's sshash-rs). A reference exists, so
  parity is the check.
- The **logicle transform** in gating is cross-checked against a reference implementation
  (FlowUtils), with golden values asserted in the test suite.
- The **original** parts — segmentation on real intravital data, the WebGPU viewer, the offline
  renderer, gating population plausibility, autofluorescence correction — have no reference to
  diff against and were validated by looking at real images. The WebGPU viewer specifically was
  cross-checked against the offline renderer, and a real disagreement between the two was found
  and resolved rather than either being trusted on its own.
- The **chain executor** has been exercised on the common analysis arrangements Dominik runs, but
  not on every combination a user might build. Real bugs are expected to surface once other people
  run combinations he hasn't. That's flagged rather than smoothed over.

For most of this, real intravital data was the validation, not synthetic fixtures. Synthetic
fixtures were useful for correctness tests during development; their outcomes diverged from real
data often enough that they weren't a substitute for looking.

### What framework scale changed — the discipline that had to be added

The publicly disclosed AI-assisted open-source scientific projects in 2025-2026 are, so far as we
could find, all single-tool: a QC pipeline (RustQC), a UMI collapser (fgumi), an indexer
(sshash-rs), a STAR fork for a specific data type (MorPhiC/STAR-Flex). Their validation check —
output equivalence against the original — is a clean fit for that shape. Cecelia is a framework,
not one tool, and two things bit us that a single-tool port wouldn't have hit.

**Drift.** With no persistent memory across sessions, an AI will happily build the same helper
twice — a second zarr reader, a second shutdown button, a second image-access path — because it
didn't know the first one existed. The second variant is not a style choice; it's the bug. The
frontend was the worst of it. By the time we measured, there were 116 icon-only buttons across the
app carrying 60 distinct class names — really only two shapes and four size tiers. Collapsing
those into one primitive with a test that now fails on any new hand-rolled icon-button was a
39-file, ~700-line change ([PR #353](https://github.com/schienstockd/cecelia/pull/353)), which is
a fair illustration of why this discipline is better landed early than after 60 spellings of two
shapes have accumulated. The CSS-convention tests specifically were the most painful to add after
the fact.

What now keeps this from compounding is making "what already exists" cheap to find and expensive
to ignore. [`docs/inventory/*.md`](docs/) catalogs the shared components. `CLAUDE.md`'s first
section after the doc index is a mandatory discovery step: before writing new code, grep the
inventory and the codebase for the thing you're about to build. The rule "one canonical helper
per job; the second way is the bug" is stated at the top and enforced by convention tests where
possible — zarr access, atomic writes, `run_py` for spawning Python, and the icon-button
primitive itself. None of this was designed up front; each rule went in the day drift caused a
real duplication.

**Open-ended vs closed-loop work.** Closed-loop tasks like the celltrackR port went smoothly:
the target was "match these numbers", the AI could work against it, the finish line was visible.
Open-ended exploration — where the coastal sibling project spent a lot of time, and where much of
Cecelia's original UI and viewer work sat — was harder. Many dead ends. Without a clear objective
to converge on, the AI would produce something plausible that turned out to be the wrong shape
once tried on real data, and the loop of "propose → try → discard" ran a lot longer than it does
for a port. This is a genuine current limit of the tooling, not a critique. It's why real-data
visual inspection is not optional for the open-ended parts, and why we accepted many more
iterations there than on the ported parts.

**No UML, no package diagrams — the architecture came from rejigging.** We didn't do the classic
up-front software-design pass. We ported, ran into consistency problems, and then wrote down the
invariants that turned out to matter — the package/API/GUI separation, `Cecelia.jl` as a
standalone Julia package that runs from the REPL with no interface attached, `run_py` as the one
Python launcher, per-area `CLAUDE.md` files for local rules. Everything durable about the
architecture is in [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) and the `CLAUDE.md` files, and
every one of those rules exists because something broke without it. That isn't the cleanest way to
design a framework — but it's what actually worked with an AI collaborator, and it's the honest
account.

### Attribution, and publication — open questions

Who "the author" of AI-assisted software is is not a settled question. Claude wrote the code.
Dominik directed it, reviewed as much as was practical, and made every scientific and design
decision. Neither of them is the sole author in the sense the word meant a few years ago. Every
public disclosure the field has landed on so far agrees on one thing: AI isn't a listed author.
On everything else — how loudly to say "AI wrote this", what to call the human's role, whether
such a tool is publishable at all, and where the maintenance responsibility sits long-term —
there isn't a convention yet. This section doesn't try to invent one.

Publication is also undecided. If it happens, the most likely paths are a *Nature Methods*-shaped
paper for the parts that are new (the browser WebGPU viewer, the offline renderer, the analysis
board) and a *Nature Protocols*-shaped paper for the full pipeline, with the 2025 *Nature
Communications* paper on the original R/Shiny `cecelia` carrying the underlying science. Neither
is a commitment.

### Sources

- The original **`cecelia`** R/Shiny package by Dominik and colleagues — the behavioural
  specification this project ports. Published in *Nature Communications* (2025),
  [doi:10.1038/s41467-025-57193-y](https://doi.org/10.1038/s41467-025-57193-y); source (R version):
  [github.com/schienstockd/cecelia-legacy](https://github.com/schienstockd/cecelia-legacy).
- The scientific tools this pipeline orchestrates, each retaining its own license and citation:
  **Cellpose** (segmentation), **btrack** (Bayesian cell tracking), **napari** (image viewing),
  **scanpy** / **anndata** (single-cell data + clustering), **scikit-image**, **PyTorch**.
- The **celltrackR** R package (Wortel & Textor) — its track-measurement algorithms are ported in
  `app/src/tasks/tracking/track_measures.jl`. Cited work, not just a dependency: Wortel et al.
  (2021), *Cell Reports Methods*, [doi:10.1016/j.crmeth.2021.100006](https://doi.org/10.1016/j.crmeth.2021.100006).
- The **Julia**, **Python**, and **Vue** (with PrimeVue and Observable Plot) open-source ecosystems.

---

## Stack at a glance

| Layer | Tech | Responsibility |
|-------|------|----------------|
| Frontend | Vue 3 + Pinia + PrimeVue, Observable Plot, regl-scatterplot | UI only — no analysis logic |
| API | Julia (HTTP + WebSocket server) | Thin transport over the package |
| Package | **Cecelia.jl** | Data model, tasks, gating, statistics — headless-runnable |
| Compute | Pixi-managed env (napari, Cellpose, btrack, scanpy, PyTorch) | Image I/O and ML |

See [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) for the layer boundaries and the REPL-runnable
contract.

---

## License

Cecelia Feijoa is licensed under **GPL-3.0-or-later** — see [`LICENSE`](LICENSE). This is
inherited from the original `cecelia` R package (`GPL (>= 3)`) that this project ports.

Third-party software it derives from, bundles, or depends on — including **celltrackR** (GPL-2.0),
whose track-measure algorithms are reimplemented in `app/src/tasks/tracking/track_measures.jl` — is
acknowledged in [`THIRD_PARTY.md`](THIRD_PARTY.md).
