# Canvas side-panel managers: one role contract, registry-declared rails

**Status:** BUILT (2026-08-08), branch `work/manager-rail` — Phases 1-6 done, typecheck + 987 frontend tests green, **not yet looked at in a browser**. Follow-on from
[`OPTICAL_FLOW_MODULE_PLAN.md`](OPTICAL_FLOW_MODULE_PLAN.md) (which built the vault) and
[`ANALYSIS_CANVAS_PLAN.md`](ANALYSIS_CANVAS_PLAN.md) (which built the board rail).

## Goal

A plot declares **which manager it needs**; the host renders it. Today the Analysis board hardcodes
`activeIsCluster ? PopulationManager : SeriesPicker`, so a plot that needs the **model vault** cannot
say so — and `flowProbability` is consequently dead on the board.

Dominik: *"the plot has to say what manager it needs … i hope that pop manager and model vault share
the same interface that this is easy to switch out."*

They nearly do. This plan makes that contract explicit, teaches the board to resolve it from the
registry, and cleans up the CSS vocabulary the half-finished generalisation left behind.

## What the audit found (2026-08-08)

All three flow views are correctly registry-flagged `analysisBoard: true` and **do** appear in the
board's picker — that half works and is test-pinned. The manager half does not exist:

| Plot | On the board today |
|---|---|
| `flowMetrics` | Fine — needs no model by design. |
| `flowProbability` | **Dead.** `LayoutCanvas.ctxFor()` returns no `model` for a non-cluster slot, so the view renders "Select a model in the vault." forever — and the board has no vault. |
| `flowTraining` | Works, but only via a **second, bespoke model picker** it carries as a fallback (`hostPicks = props.model !== undefined`). Exactly the "two pickers for one thing" its own header argues against. |

The rail branch is also inert for `gatingStrategy` and `filmstrip` (both self-contained): they get the
population `SeriesPicker`, which does nothing for them. So this is a **general** gap the flow plots
merely exposed.

**Why no shared base emerged.** The chrome *is* shared — `CanvasSidePanel` (was
`PopulationPanelShell`), plus `SelectionTable` and `useInlineEdit`, each extracted once a second or
third instance made the shape undeniable. What was never extracted is the **role contract**. Nothing
extracted it because **no host had ever held two managers behind one variable** — every one of the six
call sites names its manager statically. The board's rail is the first polymorphic consumer. The two
defects above are the shape of that missing interface, not of missing effort.

**Correction, made while building (2026-08-08).** The contract is *smaller* than first stated. This
plan originally claimed all three managers already satisfy `{ selected, scope, docked }`. They do not:
`SeriesPicker` holds `selected: string[]` and emits `toggle(valueName, pop, popType)`;
`PopulationManager` holds `selected: string` (the displayed parent) **plus** a separate
`highlighted: string[]` and a `toggleHighlight`; `FlowModelVault` holds `selected?: string` and emits
`update:selected`. Only **`scope` + `docked` + `update:scope`** are genuinely common. Hoisting one
`selected` would have misdescribed two of the three to make the table look tidy, so
`CanvasManagerChrome` covers the chrome only and each manager keeps its own selection — which is
enough, because the rail swaps on "can be docked with a scope footer", not on the selection shape.

## Decisions (2026-08-08)

1. **The registry declares the rail, not the host.** `InteractiveView` (`interactiveViews.ts`) and
   `ClusterPanelDef` (`clusterPanels.ts`) each gain a `rail` field. The board derives the rail from
   the active slot; it names no manager per view key. This is the same rule — and the same recurrence
   guard — that `interactiveViews.test.ts` already enforces for the *picker*, which shipped a dead
   `analysisBoard` flag once because a host filtered a hardcoded key list.

2. **Four rail kinds.** `'pops'` (default → `SeriesPicker`), `'clusterPops'` (→ `PopulationManager`,
   read-only), `'flowModels'` (→ `FlowModelVault`), `'none'`.

3. **`'none'` keeps the rail, drops the list.** The rail carries two independent things: the
   selection list *and* the shared styling block (`PlotOptions` via `vis`) + scope footer.
   `GatingStrategyView` consumes `vis.fontSize`, so hiding its rail outright would be a regression.
   `'none'` renders the panel with the styling block and no list — `SeriesPicker` already has
   `selectionUnused` for precisely this case, so there is an existing idiom to reuse rather than a
   new empty-state to invent.

4. **The plot components do not change.** The board holds the pick in `entry.shared` (alongside
   `clustHl`/`clustSuffix`) and merges it into the slot's context — `{ model }` for a `flowModels`
   slot — so the view receives it through the standard bag exactly as it does on the flow page. If a
   view needs editing to work under the new rail, the abstraction is wrong.

5. **Module pages stay static.** Only the board is polymorphic. The five module-page call sites
   (`GatingPlots`, `ClusterPlots`, `SummaryCanvas`, `FlowPlots`) each render one manager and should
   keep doing so — making them registry-driven would be indirection with no second case.

6. **CSS prefix = owning component; no shared row vocabulary.** Scoped styles mean slotted rows carry
   the *consumer's* scope id, so `CanvasSidePanel`'s rules can never reach a consumer's rows. The
   shared `pm-` prefix is therefore a **vocabulary** artefact, not a CSS dependency — renaming is
   mechanical and safe. Assignments:

   | Component | Prefix | Status |
   |---|---|---|
   | `CanvasSidePanel` | `csp-`, root `.canvas-side-panel` | **rename** (was `pm-` / `.pop-manager`) |
   | `SeriesPicker` | `pick-` | **rename** (was `pm-`; `sp-` is taken by `SummaryPanel`) |
   | `PopulationManager` | `pm-` | correct as-is — it *is* the population manager |
   | `FlowModelVault` | `vault-` | already clean |

7. **Don't rename the components.** `PopulationManager` / `SeriesPicker` / `FlowModelVault` name three
   genuinely different jobs (mutating tree over the gating store / read-only grouped series selector /
   comparison table over a REST listing). A common name would hide that. Only the *contract* is
   shared, and Phase 1 declares it as a type.

## Phases

### Phase 1 — make the role contract real
- `FlowModelVault`: add `docked?: boolean`, forward to `CanvasSidePanel`. This is the only thing
  standing between it and the board rail (docked mode already ignores `width` and the drag).
- Declare `CanvasManagerChrome` / `CanvasManagerChromeEmits` in `components/canvas/canvasManager.ts`
  (see the correction above for why the selection is not in it).
- Fix the stale comments at `CanvasSidePanel.vue:42-43,101` — they say the vault passes no `scope`;
  it does (`FlowModelVault.vue:131`), and the vault's own header explains why.

**Checkpoint:** vault renders docked in a fixed-width rail with no layout change on the flow page.

### Phase 2 — plots declare their rail
- Add `rail` to `InteractiveView` and `ClusterPanelDef` (Decision 2).
- Assign: `flowTraining`/`flowProbability` → `flowModels`; `flowMetrics` → `none`; `umap` + all
  `CLUSTER_PANELS` → `clusterPops`; `gatingStrategy`/`filmstrip` → `none`; default `'pops'`.

**Checkpoint:** registry only — no behaviour change yet.

### Phase 3 — the board resolves the rail
- Replace the `activeIsCluster ? … : …` branch (`LayoutCanvas.vue:596-604`) with a registry lookup on
  the active slot.
- Hold the model pick in `entry.shared.flowModel`, reusing the existing global/local scope machinery
  verbatim; local scope writes `slot.state.model`, as `FlowPlots` does.
- `ctxFor` merges `{ model }` for a `flowModels` slot.

**Checkpoint:** `flowProbability` renders on the board for the first time; `flowMetrics` and
`gatingStrategy` keep their styling block and lose the inert pop list.

### Phase 4 — delete the workaround
- `FlowTrainingView` drops its `hostPicks` fallback picker (`:71,213`). Both of its hosts now supply a
  model, so it is dead code.

### Phase 5 — CSS vocabulary cleanup (Decision 6)
- Rename `CanvasSidePanel`'s 9 own names (`.pop-manager`, `.pm-{header,title,count,body,icon,opts,footer,seg}`)
  → `.canvas-side-panel` / `csp-*`; drop the "keeps the `pm-` prefix" note from its header.
- Rename `SeriesPicker`'s 19 `pm-*` occurrences → `pick-*`.
- Per row-icon rule, **check before renaming**. Three `.pm-icon:hover { color: var(--cc-text) }` rules
  turned out byte-identical to `.cc-btn-bare:hover` (`CanvasSidePanel`, `SeriesPicker`,
  `PopulationManager`) and were **deleted, not renamed**; only the state variants (`.lit`, `.danger`)
  are the components' own.
- Stragglers: `ConfirmDeleteButton.vue:10` cites `pm-icon` in a comment; `ProjectPanel.vue:364` uses
  storage key `cc-pm-io-open` among `pp-io-*` siblings (→ `cc-pp-io-open`; it is `default-open="false"`,
  so a reset just re-closes a closed section).
- `SummaryCanvas.showManager` vs `FlowPlots.showVault` — same toggle, two names. Unify on
  `showManager` (both default to shown, so the persisted-key reset is invisible).
- **Out of scope:** `PopulationManager`'s own 126 `pm-*` occurrences (correct), and the historical
  "was `PopulationPanelShell`" notes in comments/docs (they explain *why*, and are not half-renames).

### Phase 6 — pin and document
- Extend `interactiveViews.test.ts`: every board-flagged view declares a rail kind the board can
  render (the Decision 1 recurrence guard).
- Promote the durable parts: `docs/UI.md` (side-panel section + the prefix table), `docs/ANALYSIS.md`
  (board rail), `INVENTORY.md`.

## Risks

- The rail follows the **active slot**, so on a mixed board the pop selection disappears while a flow
  slot is active. Already true for cluster slots, so it is consistent — but it is a real ergonomic
  wrinkle and is *not* addressed here.
- `flowProbability` on the board is currently dead, so Phase 3 is verified against nothing — it is
  being made to work for the first time, not preserved.
- Phase 5 is mechanical but touches ~40 class occurrences across two components; scoped CSS makes it
  safe in principle, and the failure mode is visual, so it needs a look in the browser rather than a
  test.

## References

- [`OPTICAL_FLOW_MODULE_PLAN.md`](OPTICAL_FLOW_MODULE_PLAN.md) — the vault and why it is a
  `CanvasSidePanel` rather than a `FloatingPanel`.
- [`ANALYSIS_CANVAS_PLAN.md`](ANALYSIS_CANVAS_PLAN.md) — the board, its slots and the docked rail.
- `docs/UI.md` → *Interactive plots*, *Canvas side panels* — the surfaces and the shared chrome.
- `docs/inventory/FRONTEND.md` → *Dialog/panel shells* — why canvas-scoped managers never use `FloatingPanel`.

## Outcome (2026-08-08)

Built as planned, with one design correction (above) and one unplanned deletion: `inBoardGroup` in
`interactiveViews.ts` lost its only production caller once `isClusterSlot` became rail-derived, so it
went rather than linger as a dead export — its test now asserts the same thing through `boardViews`,
which is the live API.

Verified: `npm run typecheck` clean, 987/987 frontend tests pass. **Not yet opened in a browser** —
Phase 5 is a visual change and Phase 3 makes `flowProbability` work on the board for the first time,
so neither has a before-state to regress against.
