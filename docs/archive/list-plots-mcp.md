> **ARCHIVED — original build brief.** Not authoritative; kept as a record of what was asked. Shipped as BIDIR PR #8 — see [`docs/todo/BIDIR_CONTEXT_PLAN.md`](../todo/BIDIR_CONTEXT_PLAN.md) Decision 28 for the locked design and the "PR #8" entry in the PR sequence. Sketch source: [`docs/archive/opus-audit-pr-1165.md`](opus-audit-pr-1165.md).

# Design + ship: `list_plots` MCP registry (BIDIR PR #8)

## Context

PR #1165 (merged) rolled `mark_plot` out across ~10 plot families in Cecelia. `plot_id`
= `persistKey` — every mounted `InteractivePanel` / cluster panel / summary panel
already forwards its persistKey as `plot-id`. But today Claude has NO way to
discover a plot's id — a human has to hand it over.

`BIDIR_CONTEXT_PLAN.md` → Decision 6 (capture-address citation) covers **post-hoc**
referencing via `plotSpec` on a shared capture. It does NOT cover **live discovery**:
"which plots are on the user's screen right now, and what are their ids?" That's the
gap this ships.

**Reference:** [`docs/todo/BIDIR_CONTEXT_PLAN.md`](../../cc-workspace/cecelia/cecelia-multipanel-plan/docs/todo/BIDIR_CONTEXT_PLAN.md)
— read Decision 6, Decision 11 (existing highlight bags), Decision 18 (marker
lifecycle), and the Verdict / Open items sections.

**Audit source:** `docs/archive/opus-audit-pr-1165.md` — Part 2 has the recommendation
(Option A: `list_plots` MCP registry) with a concrete sketch. Treat it as the
starting point; verify + refine.

## Locked constraints (do not violate)

- **MCP-only at both ends.** Cecelia runs on cloud VMs where Claude Code has no
  source access. Every id Claude uses at runtime must come through an MCP tool —
  never inferred from reading Vue source.
- **Additive-write-only.** New routes; no mutation of existing capture / mark /
  bag shapes.
- **Session-only, no persistence.** Registry lives in Julia memory + Pinia ref.
  WS disconnect drops that connection's entries. Page reload does NOT resurrect
  stale entries.
- **`persistKey` is the identity.** Don't overwrite it, don't derive a parallel
  id from route + family + position — that collides with the canvas's persistKey
  scheme and breaks Decision 31/34 restore-from-envelope.
- **Parallel to `usePanelExport`, not a fresh invention.** The canvas already has
  `stores/canvasPanelExports.ts` — a per-panel PNG-exporter registry keyed by
  persistKey. Study its shape; the plot registry has the same lifecycle profile.

## Do

**Part 1 — verify the sketch against current code.**

Audit Option A from `docs/archive/opus-audit-pr-1165.md` Part 2:
- Does `InteractivePanel` still forward `persistKey` as `plot-id` unconditionally?
  (`frontend/src/components/canvas/InteractivePanel.vue`)
- Does `SummaryPanel` still expose `getFrame` + `axisRect` via `defineExpose`?
  (`frontend/src/components/canvas/SummaryPanel.vue`)
- Is `usePanelExport(persistKey, cb)` (or equivalent — grep to confirm the exact
  name) still the shape to mirror? What does `stores/canvasPanelExports.ts` look
  like?

If the sketch has drifted, adjust — come back with WHAT changed and WHY the sketch
needs to shift, not a silent rewrite.

**Part 2 — build it.**

1. **Frontend composable** — `frontend/src/composables/usePlotRegistry.ts`, mirroring
   `usePanelExport`:
   ```
   usePlotRegistry(() => persistKey, () => ({ family, title, route, cellKeys?, bboxScreen? }))
   ```
   On mount: POST `/api/viewer/plots/register`. On unmount: POST
   `/api/viewer/plots/deregister`. On reactive meta change: re-register.
2. **Wire callers** — one line per host:
   - `InteractivePanel.vue` (family from `INTERACTIVE_VIEWS[view].label` or the
     canvas panel spec)
   - `SummaryPanel.vue` (family per spec type — `summary` / `heatmap` /
     `hmm-states` / `hmm-transitions`)
   - Cluster panels: `ClusterHeatmapPanel`, `ClusterHmmStatesPanel`,
     `ClusterHmmTransitionsPanel`
   - `GateMontage`, `GatePairsPanel`, `GatingStrategyView`, `UmapView`,
     `ImageStripView`, `CardsPanelInner`
3. **Server** — `api/src/plots_registry_api.jl`:
   - In-memory bag keyed by `(projectUid, plotId)`.
   - Track the WS connection that registered each entry; on disconnect, drop that
     connection's entries.
   - Routes: `POST /register`, `POST /deregister`, `GET /?projectUid=…`.
   - Optional: broadcast `viewer:plots:changed` on change (MCP polls on demand
     anyway — decide once built).
4. **MCP** — `mcp/cecelia_mcp/server.py::list_plots(project_uid: str) -> list[dict]`.
   Add to `ALLOWED_ROUTES`. Update `guidance.py`: one line — "Call `list_plots`
   before `mark_plot` to resolve 'the UMAP' → a `plot_id`."
5. **Tests** — `pixi run test-api` (Julia registry), `pixi run test-frontend`
   (composable logic if extracted to `.ts`), `pixi run test-py` (MCP tool +
   client). Register/deregister on WS disconnect is the critical case — mock a
   client drop, assert entries are gone.

**Part 3 — plan doc amendment.**

Append to `docs/todo/BIDIR_CONTEXT_PLAN.md`:
- **New Decision 37** in *Locked decisions*: "Live plot registry, MCP-only.
  Populated by `usePlotRegistry(persistKey, meta)` on panel mount/unmount, exposed
  as `list_plots(project_uid)`. Session-only, no persistence; same-persistKey
  supersedes; deregister on WS disconnect. Cloud-VM safe. persistKey is identity;
  title + route disambiguate."
- **New PR entry** in *PR sequence*: "PR #8 — Live plot registry + `list_plots`
  MCP. Follows #4c-shipped point-out consumers (which already key on plot_id);
  precedes any workflow where Claude calls `mark_plot` without a human handing
  over an id."

## Disambiguation

`persistKey` is unique by construction. When the user has two panels of the same
family (e.g. two UMAPs), they carry distinct persistKeys AND distinct titles (the
canvas gives each panel a label). Claude matches by title + route. Fallback:
include `bboxScreen: {x, y, w, h}` from `getBoundingClientRect()` at register
time; refresh on resize with `usePlotResize` discipline (no self-loop — mandated
by `continuousControls.test.ts` ratchet).

## Constraints while working

- Work in a fresh worktree — `git worktree add ../cecelia-list-plots main` at
  `/home/dominik/cc-workspace/cecelia/`. Copy `.env` from the primary checkout so
  `CECELIA_DEV_DIR` resolves.
- Never start/kill servers (Dominik owns 8080/5173/7655).
- Never write shared dev config.
- State reservations before every commit.
- Commit attribution: `Co-Authored-By: Claude Opus 4.7 <noreply@anthropic.com>`
- PR body ends with `🤖 Generated with [Claude Code](https://claude.com/claude-code)`.
- Untestable rendering changes need Dominik's eyes BEFORE shipping. This is mostly
  plumbing, but if you add ANY visible affordance, hold for eyeball.

## Output

A PR on a branch off latest `origin/main`, with:
- Composable + wiring
- Julia routes + tests
- MCP tool + guidance line
- Plan doc amendment
- PR body listing reservations (register cadence, WS-disconnect edge cases,
  multi-tab handling)
