# Linked brushing across plot families — plan

**Status:** planning (2026-09-23) · branch `linked-brushing-mvp`. Written to be picked up cold
by another session. Supersedes P5 of [`RUBBER_DUCK_FIT_PLAN.md`](RUBBER_DUCK_FIT_PLAN.md), which
misframed this as a Claude feature — it's a **user-facing linked-brushing surface** that Claude
piggy-backs on, same as `mark_tracks` piggy-backs on the shipped `TrackHighlight` bag.

## Goal

The user selects data on one plot; every other plot on the page (and the viewer, and the
correction cockpit) reacts. Same mechanic every Shiny / Dash / Vega-Lite / Plotly crossfilter app
converges on. Zero LLMs in the loop for the base capability — Claude reuses the same bag through
`mark_*` MCP tools, which is not a separate code path.

## What already ships

`stores/viewer.ts` `TrackHighlight` / `PickHighlight` are already **fan-out consumers** across
10 subscribing plot families (BIDIR PR #1165, 2026-09-22): a write to either bag propagates a
track-id / label-id selection to every subscribing family — viewer, TrackScheme, cell cards,
UMAP, gate scatter, and the summary panels that carry per-id annotations. So the *consumer* side
of linked brushing is basically shipped.

**What's missing is a user-driven PRODUCER on the plot side.** Today the two writers are:
- **User** — TrackSchemeView "Show" button (only), gate-scatter picking (only).
- **Claude** — `mark_tracks` / `mark_cells` MCP tools.

Every SummaryPanel chart type (`boxplot` / `violin` / `strip` / `histogram` / `bar` / `heatmap` /
`frequency` / `stacked` / `percent` / `count`) is **server-aggregated** — the frontend does not
hold per-cell IDs for those glyphs. `PlotSeries.points: number[]` on strip/violin is *values only,
no ids*. `RawRow` (with `track_id`) exists but isn't a user-selectable chart type. This shapes
the design.

## The producer fork — locked to Option A for MVP

Three shapes, one gets built now:

- **A. Category-source (MVP).** Click a categorical glyph — an HMM-state bar, a histogram bin,
  a heatmap cell — → tiny server round-trip resolves `(measure, category, filter)` → `track_ids` →
  drops into the shared bag. Every subscribing family already reacts.
  - Widest applicability (works on every categorical / binned chart type).
  - No `PlotSeries` data-model change.
  - One new server endpoint (`/api/tracks/by_category`).
  - Weak on continuous distributions ("select this speed range" isn't naturally a category —
    that's Option C).

- **B. Point-source (parked).** Extend `PlotSeries.points` to `{value, id}[]` — click / lasso a
  dot on strip / violin / boxplot jitter → track_ids directly, no round-trip.
  - Zero backend round-trip after initial fetch.
  - Small server change (add ids to `points` output on `chartType: "points"`).
  - Narrow — only works on charts that render per-point glyphs.

- **C. Predicate-source (parked).** Brush a range on any chart → emits
  `{measure, min, max, categoryEq?}` → *other panels re-aggregate under that filter* (Plotly
  crossfilter / Shiny reactive filter idiom).
  - Most powerful — cross-panel filter.
  - Biggest — shared "active filter" store, every fetch carries the filter, refetch coordination.
  - Doesn't feed shipped `TrackHighlight` consumers directly (a filter isn't an id list) —
    viewer / TrackScheme need a resolve-to-ids step anyway.

**MVP page** is the behaviour page. The HMM-state frequency / stacked chart is the natural first
producer — an HMM state IS the category the user reaches for, and every track on the page has
one.

## Locked decisions

Numbered so code and other docs can cite them (`Decision 5`).

1. **One shared bag, scope-tagged.** New store `frontend/src/stores/linkedSelection.ts` with
   `{scope: 'tracks'|'cells', ids: number[], source: string, sourcePlotId?: string}`. Scope is
   the id namespace, not the biology. Rename-not-remove of the shipped `TrackHighlight` /
   `PickHighlight` bags is a **follow-up** — MVP writes to the new bag AND to the existing bags
   so the shipped 10-family subscription works unchanged.

2. **Two composable shapes, one file.** `frontend/src/composables/useLinkedSelection.ts` —
   `useLinkedSelectionSource(plotId, scope)` for producers, `useLinkedSelectionSubscriber(scope)`
   for consumers. Consumers never touch the store; producers never touch subscribers. Test the
   store + composable in isolation (frontend suite is pure-logic only per `frontend/CLAUDE.md`).

3. **MVP producer = Option A.** Category-source on the HMM-state frequency chart on the
   behaviour page. Server-side resolution via a new endpoint.

4. **New endpoint `POST /api/tracks/by_category`.** Body:
   `{projectUid, imageUids[], valueName, pop, measure, category}`. Reply: `{trackIds: [int]}`.
   Reuses existing `label_props` / `pop_df` readers per `app/CLAUDE.md`. Julia testset for the
   round-trip.

5. **Visual convention: dim non-selected to opacity 0.15, selected at 1.0.** One convention across
   all subscribing families. Idle state (empty bag) = no dimming; opacity 1.0 across the board.
   Where a family already paints "selected" (viewer track overlays, cell-card halos), keep the
   existing paint and dim the *non-selected* siblings — additive, no visual replacement.

6. **Clear affordance is page-level, one per page.** A small "Clear selection" chip appears on
   the behaviour page **only when the bag is non-empty**. Escape key clears from anywhere on
   the page. No per-panel clear button — the shared bag means there's one thing to clear.

7. **Claude reuses the same bag, no new MCP tool for MVP.** `mark_tracks` and `mark_cells`
   already write to `TrackHighlight` / `PickHighlight`. During MVP, the linkedSelection store
   is a NEW writer; the shipped bags stay as-is. Migration = Follow-up 1 below.

8. **Selection clears on page navigation.** Selection is per-page, not global. `onBeforeUnmount`
   in the page module clears the store. Cross-page persistence is out of scope.

9. **Selection does NOT survive a full reload.** Ephemeral, session-only. Same discipline as
   `TrackHighlight` / `PickHighlight` — no localStorage bridge.

10. **MVP does NOT change `PlotSeries` / `RawRow`.** Options B and C both need data-model
    changes; Option A doesn't. If B or C is later greenlit, that's a separate design pass.

## Phases

Independently mergeable, ordered by dependency.

### P1 — Shared store + composable (~1 PR, frontend only)

- `frontend/src/stores/linkedSelection.ts` — the bag + setter + clearer + `isEmpty` computed.
  Per-project keyed so a project swap clears state. File-level docstring in the shape of
  `stores/captureReshow.ts`.
- `frontend/src/composables/useLinkedSelection.ts` — `useLinkedSelectionSource` +
  `useLinkedSelectionSubscriber`. Consumers get `{isSelected(id), anyActive, activeIds}`.
- Tests: `linkedSelection.test.ts` for the store (set / clear / scope isolation / project swap),
  `useLinkedSelection.test.ts` for the composable.
- **No visible change** — nothing subscribes yet.

### P2 — Backend endpoint (~1 PR, Julia)

- `api/src/tracks_by_category_api.jl` (new) — `POST /api/tracks/by_category`. Body validation,
  reuses `label_props` / `pop_df` for the resolution. Response `{trackIds: [int]}`.
- Julia testset: valid category → non-empty ids; unknown category → empty; unknown pop → 404;
  missing body fields → 400.
- Registered in `api/src/server.jl` and `api/src/routes.jl` (or wherever routes live for this
  shape — grep first).

### P3 — Category brush on the HMM-state frequency chart (~1 PR, frontend)

- Add click-to-select on the HMM-state bar/frequency chart in `SummaryPanel.vue`. The exact hook
  depends on which chart is rendering — Observable Plot's `pointerX` / bar-click event.
- On click, call `/api/tracks/by_category` and write result into the linkedSelection store via
  `useLinkedSelectionSource`.
- Optional: shift-click to add to selection.

### P4 — Subscription + visual convention on sibling panels (~1 PR, frontend)

- Every SummaryPanel on the behaviour page subscribes via `useLinkedSelectionSubscriber('tracks')`.
- Non-selected series/rows dim to opacity 0.15; selected at 1.0. Additive with existing
  highlight paint.
- Page-level "Clear selection" chip in the module layout header row — visible only when
  `!isEmpty`.
- Escape key clears from anywhere on the page.

### P5 (parked) — Family sweep + MCP migration + producer variants

Not in the MVP PR. Once the shape is validated:
- Extend subscription to the remaining plot families (UMAP, gate scatter, cell cards, etc.).
- Migrate `mark_tracks` / `mark_cells` to write into `linkedSelection` instead of the per-shape
  `TrackHighlight` / `PickHighlight` bags. Deprecate the old bags with a compatibility shim
  during the transition. **Guidance rewrite in `mcp/cecelia_mcp/guidance.py` — the mark_*
  paragraph names the old bags today.**
- Producer variants: point-source (Option B) if a family with per-point ids lands; predicate-
  source (Option C) if we need continuous-range filtering.

## Cross-cutting

- **Additive**: linkedSelection is a new store; nothing shipped is renamed or removed. The 10-
  family subscription in `stores/viewer.ts` keeps working. Migration is Follow-up 1.
- **No MCP change in MVP.** Claude's `mark_tracks` still writes `TrackHighlight`; user brushes
  write `linkedSelection`. After the migration (Follow-up 1) they're the same bag.
- **No new UI primitive.** The "Clear selection" chip uses the existing `.cc-btn` family per
  `docs/ui/PRIMITIVES.md`.

## References

- Fork it supersedes: [`RUBBER_DUCK_FIT_PLAN.md`](RUBBER_DUCK_FIT_PLAN.md) → P5
- Shipped consumer fan-out: BIDIR PR #1165 (Decision 19), `frontend/src/stores/viewer.ts`
  (`TrackHighlight`, `PickHighlight`, subscribers list in the ws.ts `viewer:mark` handler)
- Cross-cutting Cecelia rules: [`CLAUDE.md`](../../CLAUDE.md),
  [`docs/MAINTAINABILITY.md`](../MAINTAINABILITY.md)
