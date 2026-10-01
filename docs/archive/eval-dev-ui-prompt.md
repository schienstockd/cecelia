> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/todo/CLAUDE_MD_EVAL_PLAN.md` and its `CLAUDE_MD_EVAL_*` siblings.
>
> **Outcome:** consolidated into `docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md` (2026-10-01), which
> corrects this brief against the shipped eval — read that instead.

# Task: Dev section in the Cecelia app for the eval feedback loop

## Context

A weekly supervisor-run eval (see `eval-supervisor-prompt.md`) produces run records in `docs/ai-assist/eval-runs/` and a structured `YYYY-MM-DD.json` per run. The owner must review findings, label spot-check samples, and decide on proposed prompt/setup changes. Doing this by editing markdown files will get skipped, so this task builds a Dev section in the Cecelia app to do it in a GUI.

Prerequisite: PRs (a)-(e) from the supervisor prompt exist, or at least the `run.json` schema and the review-events JSONL contract. If they don't, stop and say so. Do not invent the schema here; reuse it.

Read first: root `CLAUDE.md`, `frontend/CLAUDE.md`, `docs/ui/COPY.md`, `docs/inventory/*` (frontend components, routes, data access), the existing recital console (`effectiveness/console.py`) and `effectiveness/rollup.py`. Reuse existing components, API patterns and inventory conventions. Check the inventory before writing any new shared component or route.

## Hard requirements

1. **Dev-only.** Cecelia is a published tool for end users. The Dev section is hidden unless a dev flag is on (env var or config; follow any existing convention). No Dev routes, nav entries or bundle weight in normal use where avoidable. Add a test that the section is absent with the flag off.
2. **Read structured data.** Read `run.json` files and the review JSONL. Never parse the markdown records.
3. **Decisions go to the local review JSONL only** (append-only, path from config, outside the repo). Event types: `spot_check_label`, `finding_status`, `proposal_decision`. Each event has a timestamp, target id, value and optional note. No editing or deleting events: corrections are new events.
4. **No repo mutation from the UI.** It does not commit, push, merge, or edit files in the repo. It may show links to the PR/branch for the run.
5. **Backend follows existing architecture** (Julia backend + Vue frontend unless the repo says otherwise). Keep the API small: list runs, get run, list/append review events.
6. **Copy** follows `docs/ui/COPY.md`.

## Phase 1 (this task)

### Latest run overview
- Score (raw and after scorer fixes), pinned SHA, prompt-set version, cost, CLI/model version
- Delta vs previous run: findings opened/resolved, score change, with a visible note when prompt-set or CLI version changed
- Per-prompt results table (pass/fail, class: scorer_bug / infra / genuine), filterable, with link/path to the trace

### Review queue (the main screen)
Keyboard-driven, built for about 5 minutes per week. Shows only items awaiting a human decision:
- **Spot check:** the sampled findings. Show the finding, its evidence and diagnosis. Buttons: `real` / `false` / `unclear`, optional note. Do not show any supervisor-suggested label.
- **Findings triage:** open findings. Set status: open / fixed / wontfix, optional note.
- **Proposals:** prompt-set add/change/remove and setup-change proposals with their evidence and hypothesis line. Decide: accept / reject / defer, optional note. Accept only records the decision; merging the PR stays outside the app.
- Progress indicator ("4 of 7 reviewed"), undo for the last decision (implemented as a correcting event), and an empty state.

## Phase 2 (do NOT build now; keep the data model compatible)
- Trends: score, false-positive rate, findings recurring vs watch, cost, setup size over runs
- Proposal history with hypothesis vs outcome
- Findings browser across runs

## Testing and done when

- Dev flag off: no Dev nav or routes.
- Dev flag on, fixture `run.json` + empty JSONL: overview renders, review queue lists the expected items.
- Recording decisions appends valid events and they survive reload; the queue no longer shows decided items.
- A malformed or missing `run.json`, and a schema version mismatch, show a clear error instead of crashing.
- Inventory docs and convention checks updated; existing tests pass.
- Short summary of design decisions and anything ambiguous.

## Constraints

- One small PR for the backend endpoints, one for the frontend. Do not bundle.
- No new heavy dependencies without justification.
- No charts in phase 1 unless an existing chart component is reused.
