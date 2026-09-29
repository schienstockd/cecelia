# Population → plot sync — audit + parked plan

Status: **DONE (2026-09-29)** · Branch: `pop-sync`. All punch-list items P1–P5 shipped.

## Origin

Chat, 2026-09-29 — reviewing cluster tracks with a colleague, Dominik hit:

1. "The toggle to wait for population selection to update plots seems broken and doesn't
   seem to be wired into all plots on the module pages."
2. "On the cluster tracks module page, plots don't update when I change the names of
   populations, or delete and add populations."
3. "We probably need a reload button for some plots depending on the selection — I think
   there is already one wired, can't remember where."

Two read-only forks audited the frontend. Findings below are what shipped, not a bug list
we've triaged — but they line up cleanly enough to punch-list.

## What we found

### 1. "Wait for population selection" toggle is not broken — it's under-mounted

- `manualApply` + `stagedSel` live in `frontend/src/composables/useSummaryData.ts:70`,
  persisted in the canvas `shared` bag via `useViewState`.
- UI: `frontend/src/components/canvas/CanvasSidePanel.vue:186-191` (toggle button),
  `:151` (Apply chip). Routed through `PopulationManager.vue`/`SeriesPicker.vue`.
- Only affects **global-scope** pop selection — local-scope panel selections commit per
  click (comment at `useSummaryData.ts:68` — deliberate).
- Consumer hosts:

  | Host                                            | Subscribed? |
  |-------------------------------------------------|-------------|
  | `SummaryCanvas` (Phenotype/Segment/Behaviour/Spatial/Custom pages) | Yes (global scope) |
  | `LayoutCanvas` (`/analysis` board)              | Yes (global scope) |
  | `ClusterPlots` (cluster tracks/cells/regions)   | **No — never wired** |
  | `GatingPlots` (Gating/Tracking modules)         | **No — never wired** |
  | `TrackPathsView` / interactive views            | No (own selection path) |
  | Local-scope panels inside SummaryCanvas         | No (by design) |

So "seems broken on the cluster tracks module page" reads correctly as: **the toggle
does not exist on that page.** Not a regression — a coverage gap.

### 2. Cluster-tracks rename/delete/add drops the highlight

Reactivity chain for **rename**:

1. `PopulationManager.vue:103 commitRename` → `gating.ts:377 renamePop` → POST
   `/api/gating/pop/rename`.
2. Server `app/src/gating/popmanager/mutations.jl:92 rename_pop!` returns a tree with a
   **new path** (`newpath` at `:101`).
3. `gating.ts:237 setTree(data.tree)` + WS `applyBroadcast:388` → `g.flat` recomputes
   with the new path.
4. **`ClusterPlots.vue:136`** watcher on `g.flat.map(p => p.path).join('\n')` fires and
   **prunes `highlighted.value` to only paths present in the new `g.flat`** — the old
   path is gone, the new path is not yet in `highlighted` → highlight silently lost.
5. `shownPopsFor(activeHL)` (`useClusterContext.ts:94`) returns one fewer pop → all
   cluster panels' `props.shownPops` watcher (`ClusterHeatmapPanel.vue:118`,
   `ClusterHmmStatesPanel.vue:157`, `ClusterHmmTransitionsPanel.vue:172`) fires on
   `JSON.stringify([path, clusterIds])` → refetch → panel renders **without** the pop.

Delete: same pruner, correctly drops the highlight. Add: nothing auto-highlights the
new pop, so it doesn't appear in cluster panels until the user clicks its eye. To a
user, both read as "the plot didn't update."

`useSummaryData.ts:202-206` has the **same rename-drops-highlight failure mode** — same
class of bug on the SummaryCanvas side, gated only by `if (!segPops.value.length)
return`.

### 3. Reload buttons — the scaffolding is already in place, just not fanned out

- **Auto-refresh chokepoint**: `frontend/src/composables/useDataRefresh.ts` — every plot
  host passes `(imageUids, onRefresh)`; the composable watches
  `project.dataVersionFor(imageUids)` (bumped in `ws.ts` on `task:status=='done'`) and
  calls `onRefresh` iff `settings.autoRefreshOnTask` (toggle at `SettingsModule.vue:737`).
- **Coverage is ratcheted** by `frontend/src/composables/dataRefreshCoverage.test.ts` —
  enforces a MUST_REFRESH list and forbids reads of `autoRefreshOnTask` outside the
  composable.
- **Per-plot manual reload buttons** exist on self-hosted views only:
  `TrackPathsView.vue:452`, `TrackSchemeView.vue:993`, `FlowProbabilityView.vue:220`,
  `FlowMetricsView.vue:221`, `TrackDiagnosticsView.vue:321`, `FlowTrainingView.vue:454`.
  Each fires that view's own `load()`.
- **Canvas-wide reload token** (`reloadToken`) already exists in
  `useSummaryData.ts:178,182` → `SummaryPanel.vue:66,570` → `SummaryCanvas.vue:511` /
  `LayoutCanvas.vue:754`. There is *no button* wired to it today; a bump happens via
  the auto-refresh path only.
- Cluster and gate panels **don't** accept an external reload token — they'd need one
  prop wired in, mirroring `SummaryPanel.vue`'s pattern.

So: the prior art is `reloadToken` + `useDataRefresh`. Nothing new needed at the
composable layer; the work is host-level wiring + a canvas-scope button.

## What shipped

The whole punch list landed together on 2026-09-29 as one PR. Highlights:

- **`frontend/src/utils/popRenameRemap.ts`** (new) — pure `makePopPathRemap(oldItems,
  newItems)` + `remapPopKeys(keys, remap)`. Unit tests in `.test.ts` (9 cases: rename,
  delete, add, first-mount, synthetic-no-uid, reparent, no-op).
- **`frontend/src/composables/usePopSelectionMode.ts`** (new) — the shared
  `manualApply`/`stagedSel` staging surface (`hasStaged`, `stagedChangeCount`,
  `applyStaged`, `discardStaged`, `remapStaged`), used by summary + cluster + gate.
- **`PopNode` / `FlatPop`** now carry `uid`; `flatten()` in `stores/gating.ts` passes it
  through. Server side already emitted `uid` on every popmap node
  (`app/src/gating/popmanager/persistence.jl:14`) — no server change needed.
- **`CanvasSidePanel.vue`** gained a `reloadable` prop + `reload` emit + a refresh icon
  next to the manual-apply toggle. `PopulationManager.vue` and `SeriesPicker.vue`
  forward both.
- **Rename-preserve pruners** in `ClusterPlots.vue`, `GatingPlots.vue`, and
  `useSummaryData.ts` (with a shared `localSelRemap` re-used by SummaryCanvas +
  LayoutCanvas for their LOCAL per-panel prunes).
- **Reload buttons** wired on Summary/Layout (uses `useSummaryData.reload()`),
  Cluster (own `reloadToken`, threaded via `clusterPanelProps`), and Gating (own
  `reloadTokenLocal`, threaded as `:reload-token` on GatePlotPanel + GatePairsPanel).
- Cluster panels (`ClusterHeatmapPanel`, `ClusterHmmStatesPanel`,
  `ClusterHmmTransitionsPanel`), `GatePlotPanel`, and `GatePairsPanel` accept a
  `reloadToken` prop and include it in their fetch watch keys.
- `validSelKeys` computed removed from `useSummaryData` (no consumers after the
  rename-preserve pruner replaced the last two path-intersect callers).

Full test suite: `pixi run test-frontend` — 3760 / 3760 pass. `vue-tsc -b` clean.

## Punch list (original — kept for cross-reference to code review)

Ordered by value / cost. Each item is standalone; you call which we take first.

### P1 — Rename-preserve the highlight (smallest, highest value)

- **File**: `frontend/src/modules/cluster/ClusterPlots.vue:136` (and mirror the same
  fix at `useSummaryData.ts:202-206`).
- **Change**: when `g.flat` changes, don't just prune to intersection — carry old→new
  paths across a rename.
- **Fix shape (draft — not decided)**:
  - **Preferred**: server side emits `renamed: {oldPath, newPath}` on
    `/api/gating/pop/rename` response and on the WS `gating:popmap` broadcast; both
    watchers apply that map before pruning. Small server change,
    `app/src/gating/popmanager/mutations.jl:92`.
  - **Fallback**: diff pre/post `flat` snapshots keyed by a stable pop id — but
    `PopTree` doesn't currently carry one, so this is more work than the server change.
- **Also**: delete already works; on add, the "click the eye to reveal" gate stays —
  Dominik confirmed 2026-09-29.
- **Open**: is there a stable pop id we could diff by, or must the server change land
  first? (Fork suspected there isn't — verify before coding.)

### P2 — Fan the "wait for population selection" toggle into cluster/gate hosts

- **Lift** `manualApply`/`stagedSel` out of `useSummaryData.ts:70` into a shared
  composable (candidate location: `useCanvasSharedState.ts` or a new
  `usePopSelectionMode.ts`).
- **Wire** into `ClusterPlots.vue` (both `highlighted` and each panel's `state.hl`) and
  `GatingPlots.vue`. Local-scope panels stay per-click (unchanged; documented at
  `useSummaryData.ts:68`).
- **The staged path must also honour P1**: a rename during a staged edit must not wipe
  the pending pop.
- **Sanity**: count touchpoints before starting. Fork estimate: ~3 hosts + shared
  composable + tests. Re-audit at start.

### P3 — Canvas-scope reload button on cluster + gate canvases

- Existing prior art: `reloadToken` in `useSummaryData.ts:178,182` → SummaryPanel.
- **Wire** the same shape into `ClusterPlots.vue` and `GatingPlots.vue`; add a
  `reloadToken` prop to `ClusterHeatmapPanel` / `ClusterHmmStatesPanel` /
  `ClusterHmmTransitionsPanel` (their fetch already runs on prop change — the watcher
  extension is one JSON.stringify key).
- **Button** goes on the canvas side panel next to the existing manual-apply toggle
  (probably `CanvasSidePanel.vue` — but cluster/gate don't use CanvasSidePanel today,
  so a new host-appropriate spot is needed).
- **Explicitly ask** whether the button should always show, or only when
  `autoRefreshOnTask` is off (my instinct: always show — the manual reload button is
  cheap insurance whenever the reactivity chain misfires, which is exactly the class
  of bug P1 fixes).

### P4 — Extend the data-refresh coverage ratchet

- After P3, `dataRefreshCoverage.test.ts` MUST_REFRESH list needs the new hosts added
  so future churn doesn't drop them. This lands with P3, not separately.

### P5 — CellCardsView

- Fork didn't audit `CellCardsView.vue`'s shownPops-watcher in detail. Likely same
  rename-drop bug as ClusterPlots. Verify + apply P1 fix once P1's shape is decided.

## Non-goals

- Not touching the auto-refresh-on-task chokepoint (`useDataRefresh`) — it works.
- Not touching per-view `load()` buttons on self-fetching views — they work.
- Not changing local-scope panel selection semantics (per-click commit, documented).
- Not adding a new global "population changed" event bus — the WS `gating:popmap`
  broadcast is the source of truth and we're layering on top of it.

## Anchor

- Two audits ran on branch `pop-sync` on 2026-09-29 (subagent forks; transcripts in
  the session scratchpad, not committed).
- Related invariants: `docs/ARCHITECTURE.md` (population manager), `docs/UI.md`
  (existing toggle copy).
- Cross-reference: `feedback_napari_reload_optin` — same "two toggles, don't unify"
  principle applies here between (a) `manualApply` for population-selection staging
  and (b) canvas reload button — they are separate signals, don't collapse into one
  control.

## Open questions to close before P1

- Does the WS `gating:popmap` broadcast currently carry rename info, or would the
  server change need to add `renamed: {oldPath, newPath}`?
- Does `PopTree` carry a stable id we could diff by client-side? (Fork's guess: no.)
