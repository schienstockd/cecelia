// The population-type registry, frontend side. The source of truth is Julia's `POP_TYPE_REGISTRY`
// (app/src/gating/popmanager/registry.jl); `popTypeRegistry.json` is that table written out by
// `write_pop_type_registry_json()`, and a Julia test fails when the two differ. Read it from here —
// a hand-written list of pop-type literals fails `popTypeRegistry.test.ts` (POP-TYPES-OK: <reason>
// on the line marks a deliberate subset). See docs/POPULATION.md → *Population types*.
//
// A static import rather than a fetch: the predicates below run inside computeds and render paths
// that cannot wait on a request, and an empty registry before boot would answer "not a cluster type"
// for every pop on first paint.

import registry from './popTypeRegistry.json'

export interface PopTypeInfo {
  name: string
  label: string                         // user-facing name in plot pickers
  suffix: string | null                 // own gating sidecar suffix; null = no own file
  mapType: string | null                // the stored map its pops live in (live → flow)
  gating: boolean                       // hand-drawn gates (undo/redo, copy-to-images)
  cluster: boolean                      // a filter on a per-run cluster column
  clusterPrefix: string                 // that column's family prefix, else ''
  grain: 'cell' | 'track' | 'branch'    // rows its pops are evaluated over
  category: 'gated' | 'clustered' | 'region'
  trackFamily: boolean                  // a population of tracks on the board's track plots
}

export const POP_TYPE_REGISTRY = registry as PopTypeInfo[]
const BY_NAME = new Map(POP_TYPE_REGISTRY.map(i => [i.name, i]))

export const popTypeInfo = (popType?: string | null): PopTypeInfo | undefined => BY_NAME.get(popType ?? '')
const names = (pred: (i: PopTypeInfo) => boolean): string[] => POP_TYPE_REGISTRY.filter(pred).map(i => i.name)

/** Every pop type with its own gating sidecar. */
export const STORED_POP_TYPES = names(i => i.suffix !== null)
/** The board's populations of tracks, in picker order (the first is the default). */
export const TRACK_FAMILY_POP_TYPES = names(i => i.trackFamily)
/** The stored, cell-grained types — what the viewer draws as centroid point overlays. */
export const STORED_CELL_POP_TYPES = names(i => i.suffix !== null && i.grain === 'cell')
// The predicates over these rows keep their existing homes: `isTrackPopType` (utils/measureGroups),
// `isClusterPopType` / `isGatingPopType` / `clusterMeasurePrefix` (utils/clusterMeasure),
// `popTypeLabel` (plots/popTypes).
