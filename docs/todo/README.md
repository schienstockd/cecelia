# Parked plans

This directory holds **parked plans**: standalone design documents for a feature that is too big to
capture as a `docs/TODO.md` item, but not yet built (or built in phases). Each is a `*_PLAN.md` with
the real design work done up front — decisions, architecture, a phased build sequence — so the
thinking survives a context break and anyone (human or agent) can pick it up cold.

## What belongs here vs. the other trackers

| Doc | Holds | Shape |
|-----|-------|-------|
| `docs/TODO.md` | the backlog | numbered items, one line → one paragraph each |
| `docs/todo/*_PLAN.md` | **parked plans** | full design doc: decisions + phases + architecture |
| `docs/FUTURE.md` | deferred *known-better* alternatives (ecosystem/scale not ready) | what/why-deferred/when-to-revisit |
| `docs/ROADMAP.md` / `docs/MILESTONES.md` | forward phase goals / shipped ledger | high-level |
| `docs/<AREA>.md` | how a **built** subsystem works | permanent reference |

## When to create a parked plan

Create a `*_PLAN.md` here when **any** of these is true:

- The feature needs **multiple locked decisions** and a **phased build sequence** before (or while)
  building — more than a TODO paragraph can hold.
- A topic is being **paused** ("I need a break from this") but the design must be preserved.
- **Code needs a stable pointer** to the rationale — e.g. `# see docs/todo/X_PLAN.md (Decision 5)`.

If it fits in a paragraph and needs no design, it's a `docs/TODO.md` item, not a parked plan.

## Conventions

- **Name**: `<FEATURE>_PLAN.md`, SCREAMING_SNAKE (`CLUSTERING_PLAN.md`, `ANALYSIS_CANVAS_PLAN.md`).
- **Top matter**: a one-line `Status:` (planning / paused / in-progress + branch) and a `## Goal`.
- **Locked decisions**: an explicit dated `## Decisions` (or "Locked decisions") section, numbered so
  code and other docs can cite them (`Decision 5`).
- **Phases**: an independently-shippable build sequence with checkpoints.
- **References**: code/docs that point at the plan use the repo-relative path
  (`docs/todo/<FILE>.md`), not an absolute/`~` path, so the pointer survives a checkout anywhere.
- **Promotion**: once the feature ships, move the durable "how it works" content into a permanent
  `docs/<AREA>.md` and either delete the plan or leave it as history (note which at the top).

## Current parked plans

- `CLUSTERING_PLAN.md` — Leiden clustering (cells + tracks), GPU/RAPIDS parked. Cited from
  `pixi.toml`, `clustering_utils.py`, `clustPops`/`clustTracks` `cluster.jl`, `docs/SHIPPING.md`.
- `ANALYSIS_CANVAS_PLAN.md` — multipage tabbed analysis board + gating-strategy plot + PDF export
  (branch `feat/multipage-analysis-canvas`).
- `SERVICE_PANEL_PLAN.md` — Settings control panel: live status + start/stop/restart for backend /
  napari / notebooks, global Quit, and a separate-window "pixi console" (reuses the existing log
  console). Branch `feat/settings-service-panel`; phased (panel → console → backend restart).
- `PY_PACKAGING_PLAN.md` — make `app/py` an installable package (rename `py` → `cecelia`,
  `pyproject.toml` + extras, editable pixi install) so external consumers (`coastal`) can
  `import cecelia.utils.*` without a `sys.path` hack. Touches `app/src/py_runner.jl` (1 line),
  19 Python imports, `pixi.toml`.
