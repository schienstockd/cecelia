// The ROLE contract for a canvas side-panel MANAGER — the thing in the rail that picks what the
// plots show. Three components fill it: `PopulationManager` (mutating gating tree), `SeriesPicker`
// (read-only cross-segmentation series list) and `FlowModelVault` (trained-model table). They share
// the CHROME already (`CanvasSidePanel`), but what a HOST can ask of one was never written down —
// every call site named its manager statically, so the common shape stayed implicit.
//
// The Analysis board is the first polymorphic host, and the cost of the missing contract showed up
// there twice: `FlowModelVault` never grew a `docked` prop (nothing docked it) and `FlowTrainingView`
// grew its own model picker (nothing supplied one). Hence this file.
// See docs/todo/CANVAS_MANAGER_RAIL_PLAN.md (Decisions 1-4).

/**
 * What EVERY manager accepts — and deliberately no more.
 *
 * The SELECTION is **not** in here, though it looks like it should be. The three disagree on both
 * arity and emit shape: `SeriesPicker` holds `selected: string[]` and emits
 * `toggle(valueName, pop, popType)`; `PopulationManager` holds `selected: string` (the displayed
 * parent) plus a separate `highlighted: string[]`; `FlowModelVault` holds `selected?: string` and
 * emits `update:selected`. Hoisting one `selected` here would misdescribe two of the three to make a
 * table look tidy. The host binds each manager's own selection, as it already does today.
 *
 * What this DOES buy is the part the rail actually swaps on: any manager can be rendered in a docked
 * box with a global/local footer, without the host knowing which one it is.
 */
export interface CanvasManagerChrome {
  /** global = the pick applies to every plot; local = to the active plot only */
  scope?: 'global' | 'local'
  /** render in-flow in a fixed rail (the board) instead of as a draggable box (module canvases) */
  docked?: boolean
}

export interface CanvasManagerChromeEmits {
  'update:scope': ['global' | 'local']
}

/**
 * Which manager a PLOT needs. Declared on the plot's registry entry (`InteractiveView.rail`,
 * `ClusterPanelDef.rail`) so the host resolves it from the registry and names no view key — the same
 * rule `interactiveViews.test.ts` already enforces for the "+ Plot" picker, which shipped a dead
 * `analysisBoard` flag once because a host filtered a hardcoded list.
 *
 * `'none'` still renders the rail: it carries the shared styling block (`PlotOptions` via `vis`) and
 * the scope footer as well as the list, and a self-contained plot may well use the styling
 * (`GatingStrategyView` reads `vis.fontSize`). It renders WITHOUT the list — see `SeriesPicker`'s
 * `selectionUnused`, which is that state and already existed.
 */
export type RailKind = 'pops' | 'clusterPops' | 'flowModels' | 'none'

/** A plot that declares nothing gets the summary population picker — today's behaviour. */
export const DEFAULT_RAIL: RailKind = 'pops'
