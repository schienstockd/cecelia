# Cecelia Pineapple

A Julia package with a graphical interface for cellular image cytometry — import, segmentation,
tracking, gating, behavioural analysis, and clustering of multiplexed and live-cell microscopy
data. It is a ground-up reimplementation of the original R/Shiny [cecelia](#sources) in a
Julia + Python + Vue stack.

> ⚠️ **This software was written almost entirely by an AI** ([Claude Code](https://claude.com/claude-code)),
> under Dominik Schienstock's direction. All scientific validation was done by the human author, and
> it has **not yet been independently tested** by other users — treat early releases accordingly.
> Full methodology and sources are at the [end of this README](#how-this-software-was-built).

- **Developer setup:** [`docs/INSTALL.md`](docs/INSTALL.md) · **Architecture:** [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md)

---

## Quickstart

One command installs [Pixi](https://pixi.sh) + [Julia](https://julialang.org) if they're missing,
downloads the latest release, provisions the environment (a few GB on first run), and adds a
**Cecelia** launcher to your applications menu.

**Linux / macOS**
```sh
curl -LsSf https://github.com/schienstockd/cecelia/releases/latest/download/install.sh | sh
```

**Windows** (PowerShell)
```powershell
irm https://github.com/schienstockd/cecelia/releases/latest/download/install.ps1 | iex
```

**Run.** Click the **Cecelia** launcher (or run `pixi run app` from the install directory) — it
starts the server and opens Cecelia in your browser at <http://localhost:8080>. From there: create a
project, import images, then run tasks (segment → track → gate → cluster / behaviour) and inspect
the results in the linked plots and the napari viewer.

**Update.** Re-run the install command above, or run `pixi run update` from the install directory.

Running from source for development (hot-reload) is covered in [`docs/INSTALL.md`](docs/INSTALL.md).

---

## How this software was built

**This software was developed almost entirely with [Claude Code](https://claude.com/claude-code)
(Anthropic), using the Claude Opus and Claude Sonnet models, under the Garvan Institute of Medical
Research enterprise license.**

Building a research software package largely with an AI assistant is, by now, not in itself novel.
What this disclaimer is for is **transparency about the specific route we took and the process we
followed** — so that users, reviewers, and collaborators can judge the work with that context, and
so the approach is reproducible.

### Claude's role (the AI)

Claude wrote essentially all of the source code in this repository. Working interactively in the
terminal, it:

- **ported** the original R/Shiny `cecelia` into this Julia/Python/Vue stack. The architecture and
  data model are largely *carried over* from the original — the object model, the population table,
  the gating and population model are Dominik's existing design, not new inventions. The work was
  translating them faithfully into Julia idioms (see [`docs/`](docs/) for the per-subsystem design
  notes);
- worked out the new-stack-specific structure under Dominik's direction (the package / API / GUI
  layer split, the task/scheduler system, the Vue frontend, the Python/napari bridge) and
  implemented all of it;
- wrote the automated test suite and the documentation in [`docs/`](docs/).

Claude operated as the **implementer/engineer**: given a goal and the original design, it explored
the old codebase, proposed how to port it, wrote and revised code against review, and ran the test
suite headlessly. It **could not** see the running GUI, a microscope, or real imaging output — so it
never had the final word on whether a result was scientifically or visually correct.

Concretely, what Claude **could not** do is the part that matters most for trust: it had no access
to microscopy data during development beyond the small fixture files in the test suite. It could not
observe live-cell tracking, validate segmentation quality on real images, or confirm that a
population gate was biologically meaningful. All scientific validation — that the software produces
correct results on real data — was performed by Dominik.

### The human role

Dominik is the **author and scientific owner** of this software and of the original `cecelia`. He:

- set every goal and constraint, and made all architecture and design decisions (the language
  boundary, the package / API / GUI separation, the gating and population model, what to port,
  what to drop, what to defer);
- contributed the domain expertise — immunology and intravital / live-cell microscopy — that the
  analysis must be correct for;
- **reviewed all output and validated scientific and visual correctness**, which the AI could not
  verify from its environment. This division of labour was deliberate: the AI implements, the human
  is the architect and the scientific authority.

### Sources

- The original **`cecelia`** R/Shiny package by Dominik Schienstock and colleagues — the
  behavioural specification this project ports. Published in *Nature Communications* (2025),
  [doi:10.1038/s41467-025-57193-y](https://doi.org/10.1038/s41467-025-57193-y); source (R version):
  [github.com/schienstockd/cecelia-legacy](https://github.com/schienstockd/cecelia-legacy).
- The scientific tools this pipeline orchestrates, each retaining its own license and citation:
  **Cellpose** (segmentation), **btrack** (Bayesian cell tracking), **napari** (image viewing),
  **scanpy** / **anndata** (single-cell data + clustering), **scikit-image**, **PyTorch**.
- The **celltrackR** R package (Wortel & Textor) — its track-measurement algorithms are ported in
  `app/src/tasks/tracking/track_measures.jl`. Cited work, not just a dependency: Wortel et al.
  (2021), *Cell Reports Methods*, [doi:10.1016/j.crmeth.2021.100006](https://doi.org/10.1016/j.crmeth.2021.100006).
- The **Julia**, **Python**, and **Vue** (with PrimeVue and Observable Plot) open-source ecosystems.

### Our approach — the route we took

- **Human-as-architect, AI-as-implementer**, fully interactive via Claude Code in the terminal.
- **Port-driven**: the old R behaviour was the spec. Every departure (e.g. dropping the tracking
  "filter crutch") was a deliberate, documented decision — not drift.
- **Invariants written down first and enforced**: a headless-testable package boundary, no analysis
  logic in the API or UI, no fourth language. See [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md) and
  [`CLAUDE.md`](CLAUDE.md).
- **Every change ships with tests and docs**, and the package is verifiable from the Julia REPL
  without the GUI.
- Because the AI cannot observe the live GUI or real microscopy data, **visual and scientific
  verification was always performed by the human** — by design.

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
