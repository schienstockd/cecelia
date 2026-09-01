import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, computed, watch } from 'vue'
import { useLogStore } from './log'
import { useProjectMetaStore } from './projectMeta'
import { useProjectStore } from './project'
import { clusterMeasure } from '../utils/clusterMeasure'
import { centroidLabel, defaultTransformForCol } from '../utils/gatingAxes'
import { popPath } from '../utils/popName'

// Derived populations (e.g. `_tracked`, future clustering pops) own a reserved namespace:
// leaf names beginning with `_`. Hand-drawn gates may not use that prefix — mirrors the backend
// `is_reserved_pop_name` / `DERIVED_POP_PREFIX` (app/src/gating/population_manager.jl), which
// also enforces it server-side (a reserved name → 400). Used for inline validation hints.
export const DERIVED_POP_PREFIX = '_'
export const isReservedPopName = (name: string) => name.trim().startsWith(DERIVED_POP_PREFIX)

// ── Types mirroring the gating JSON tree (docs/API.md, docs/POPULATION.md) ──────
export interface TransformSpec {
  kind: 'linear' | 'log' | 'asinh' | 'logicle'
  T?: number; W?: number; M?: number; A?: number; cofactor?: number; floor?: number
}
export interface GateSpec {
  kind: 'rectangle' | 'polygon'
  x_channel: string; y_channel: string
  x_transform: TransformSpec; y_transform: TransformSpec
  x_min?: number; x_max?: number; y_min?: number; y_max?: number
  vertices?: [number, number][]
}
// one AND-ed condition of a compound filter (Decision 15)
export interface FilterCondition { measure: string; fun: string; values: unknown }
// a filter spec: the single measure/fun/values (back-compat) plus optional AND-ed `conditions`
export interface FilterSpec { measure: string; fun: string; values: unknown; default_all: boolean; conditions?: FilterCondition[] }

// a boolean spec (Decision 16): the included terms combined with `op`, minus every excluded term.
// `pops: []` + `not: [x]` is the plain "everything here except x" case.
export interface BooleanSpec { op: 'and' | 'or'; pops: string[]; not: string[] }

export interface PopNode {
  name: string; colour: string; show: boolean
  gate?: GateSpec
  filter?: FilterSpec
  boolean?: BooleanSpec
  is_track?: boolean
  transient?: boolean              // ephemeral (napari cell selection) — not persisted
  membership_sig?: string          // explicit-label pops (napari selection): hash of label set
  children: PopNode[]
}
export interface PopTree { value_name: string; pop_type: string; populations: PopNode[] }

// flattened view for the manager (path-keyed, with depth)
export interface FlatPop {
  path: string; name: string; parent: string; colour: string; show: boolean
  depth: number; gate?: GateSpec; transient?: boolean
  filter?: FilterSpec  // cluster / region / user-defined filter pops
  boolean?: BooleanSpec  // a combination of OTHER populations (Decision 16)
}

function flatten(tree: PopTree): FlatPop[] {
  const out: FlatPop[] = []
  const walk = (nodes: PopNode[], parent: string, depth: number) => {
    for (const n of nodes) {
      const path = popPath(parent, n.name)
      out.push({ path, name: n.name, parent, colour: n.colour, show: n.show, depth,
                 gate: n.gate, transient: n.transient, filter: n.filter, boolean: n.boolean })
      walk(n.children ?? [], path, depth + 1)
    }
  }
  walk(tree.populations ?? [], 'root', 0)
  return out
}

// path → serialised gate spec, to diff trees and find membership-affecting changes
function gateSignatures(tree: PopTree): Map<string, string> {
  const m = new Map<string, string>()
  const walk = (nodes: PopNode[], parent: string) => {
    for (const n of nodes) {
      const path = popPath(parent, n.name)
      // include membership_sig so explicit-label pops (the napari selection) bump their version
      // when their cell set changes — they have no gate/filter to diff on.
      m.set(path, JSON.stringify(n.gate ?? null) + '|' + JSON.stringify(n.filter ?? null)
                  + '|' + JSON.stringify(n.boolean ?? null) + '|' + (n.membership_sig ?? ''))
      walk(n.children ?? [], path)
    }
  }
  walk(tree.populations ?? [], 'root')
  return m
}

export const useGatingStore = defineStore('gating', () => {
  const log = useLogStore()
  const meta = useProjectMetaStore()
  const projStore = useProjectStore()

  const imageUid  = ref<string | null>(null)
  const valueName = ref<string>('default')
  const popType   = ref<string>('flow')

  // Set-wide cluster pops: `imageUid` is the primary image (drives the displayed tree/stats), and
  // `mirrorUids` are the OTHER clustered images the same pop mutation is replayed to, so a cluster
  // pop (a filter on `clusters.{suffix}`, which is image-independent) lands identically on every
  // image in the run. Empty for ordinary single-image gating. Set by the cluster page.
  const mirrorUids = ref<string[]>([])

  // Cell-selection Z scope for the WebGPU viewer's rectangle picker: 'stack' = read the whole
  // z-stack (napari's original semantics; ignores the viewer's z-plane), 'slice' = read only ±N
  // planes around the viewer's live z. Written by `CellSelectionTools.vue`; read by
  // `ViewerWindow.vue`'s `pickRectAt` which passes `zLo`/`zHi` to `/api/viewer/pick-rect`.
  const pickZMode   = ref<'stack' | 'slice'>('stack')
  const pickZWindow = ref<number>(0)

  const tree      = ref<PopTree>({ value_name: 'default', pop_type: 'flow', populations: [] })
  const columns   = ref<string[]>([])           // gateable feature columns (raw var names)
  const obsColumns = ref<string[]>([])          // per-cell obs measures (regions.*/clusters.*/hmm.*/is.aggregate/speed…) — filter-pop measures
  const channels  = ref<string[]>([])           // intensity columns, e.g. mean_intensity_0 (ordered)
  const channelNames = ref<string[]>([])         // display names aligned to `channels`
  // spatial/temporal centroid axes (obsm) — gateable + visualisable scatter axes (centroid_x/_y/_z, centroid_t)
  const spatialColumns  = ref<string[]>([])
  const temporalColumns = ref<string[]>([])
  const valueNames = ref<string[]>([])
  // track gating only (popType==='track'): cell measures aggregatable into per-track properties,
  // and the aggregate suffixes — the client builds an axis `{measure}.{agg}` (server inverts it).
  const cellMeasures = ref<string[]>([])
  const trackAggregates = ref<string[]>([])
  const stats     = ref<Record<string, { count: number; pctParent: number }>>({})
  // per-population membership version — bumped when a pop's (or an ancestor's) gate changes.
  // Panels watch the version of their displayed pop to refresh points smoothly (no full reload).
  const popVersion = ref<Record<string, number>>({})

  // spatial + temporal centroid axes, offered together as a "Spatial / Time" group in the axis pickers
  const spatialAxes = computed(() => [...spatialColumns.value, ...temporalColumns.value])
  const isSpatialAxis = (col: string) => spatialAxes.value.includes(col)
  // THE default scale for a measure, wherever one is picked: raw coordinates (spatial/temporal axes +
  // any centroid column, matched by name so it holds even for data that doesn't list centroids in
  // spatial_cols) are positions and never logicle; a flow intensity is logicle. The rule itself is the
  // pure `utils/gatingAxes` `defaultTransformForCol` — the board's read-only gating view shares it
  // without a store — and this binds it to THIS segmentation's spatial axes and pop type. (It replaced
  // an `isLinearAxis` predicate that every picker then turned into the same transform by hand.)
  const defaultTransformFor = (col: string): 'linear' | 'logicle' =>
    defaultTransformForCol(col, { spatialAxes: spatialAxes.value, popType: popType.value })

  const flat = computed(() => flatten(tree.value))
  // transient pops (e.g. the napari cell selection) — auto-highlighted on the plots
  const transientPaths = computed(() => flat.value.filter(p => p.transient).map(p => p.path))
  const projectUid = () => meta.current?.uid ?? ''
  // the set the gated image belongs to — keys the per-set napari point size (see settings store)
  const napariSetUid = () => (imageUid.value ? projStore.setUidOfImage(imageUid.value) : null) ?? ''

  // resolve a raw column to its display label: intensity columns → channel name (R
  // change_channel_names), centroids → "X position"/"Time" (centroidLabel), everything else
  // (morphology, etc.) stays as-is.
  function colLabel(col: string): string {
    const i = channels.value.indexOf(col)
    if (i >= 0 && channelNames.value[i]) return channelNames.value[i]
    return centroidLabel(col)
  }

  // ── Undo / redo (hand-drawn gating only: flow + track) ────────────────────────
  // History lives on the SERVER — the whole population tree is one serialisable document, and the
  // server is the only place that sees every writer (this tab, another tab, napari). Keeping a
  // client stack would give each tab its own idea of "before". So these two are just the server's
  // answer, refreshed with the tree on every fetch and every broadcast.
  const canUndo = ref(false)
  const canRedo = ref(false)
  // absent fields (a response that predates this, or a pop type with no history) read as "no history"
  const _setHistory = (d: { canUndo?: boolean; canRedo?: boolean }) => {
    canUndo.value = d.canUndo === true
    canRedo.value = d.canRedo === true
  }

  function bump(p: string) {
    popVersion.value = { ...popVersion.value, [p]: (popVersion.value[p] ?? 0) + 1 }
  }

  // single entry point for tree updates: diff vs current, bump the membership version of every
  // pop whose gate/filter changed AND its descendants (parent∩child propagation), refresh stats.
  function setTree(next: PopTree) {
    const oldSig = gateSignatures(tree.value)
    tree.value = next
    const newSig = gateSignatures(next)
    const changed = new Set<string>()
    for (const [p, s] of newSig) if (oldSig.get(p) !== s) changed.add(p)
    for (const p of oldSig.keys()) if (!newSig.has(p)) changed.add(p)   // deleted
    if (changed.size) {
      const toBump = new Set<string>(changed)
      for (const p of newSig.keys())
        for (const c of changed) if (p === c || p.startsWith(c + '/')) { toBump.add(p); break }
      toBump.forEach(bump)
    }
    fetchStats()
  }

  function _params() {
    return `projectUid=${projectUid()}&imageUid=${imageUid.value}&valueName=${valueName.value}&popType=${popType.value}`
  }

  async function _post(path: string, body: Record<string, unknown>) {
    try {
      const res = await fetch(path, {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid: projectUid(), imageUid: imageUid.value,
                               valueName: valueName.value, popType: popType.value, ...body }),
      })
      const data = await res.json().catch(() => ({})) as
        { tree?: PopTree; error?: string; canUndo?: boolean; canRedo?: boolean }
      if (!res.ok) throw new Error(data.error ?? `HTTP ${res.status}`)
      // set-wide: replay the SAME mutation to the other clustered images (cluster pops only). The
      // body is image-independent (filter on clusters.{suffix}), so paths stay in sync. Await these
      // BEFORE applying the tree, so anything that reloads off the tree (e.g. the set-pooled heatmap)
      // sees every image's write already persisted, not a half-written set.
      if (mirrorUids.value.length) await Promise.all(mirrorUids.value.map(uid =>
        fetch(path, {
          method: 'POST', headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ projectUid: projectUid(), imageUid: uid,
                                 valueName: valueName.value, popType: popType.value, ...body }),
        }).catch(() => undefined)))
      if (data.tree) setTree(data.tree)
      _setHistory(data)
      // Any pop mutation (add/delete/rename/setGate/updatePop) changes what the viewer draws — the
      // /viewer-window popup has its own store, so it needs the localStorage ping to refetch its
      // overlays. Silent + cheap; a matching-imageUid viewer will loadOverlays() on receive.
      if (typeof localStorage !== 'undefined' && imageUid.value) {
        localStorage.setItem('cc.viewerOverlaysTick', `${imageUid.value}:${Date.now()}`)
      }
      return true
    } catch (e) {
      log.error(`Gating: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
      return false
    }
  }

  async function selectImage(uid: string, vn?: string, pt?: string) {
    imageUid.value = uid
    if (vn) valueName.value = vn
    if (pt) popType.value = pt           // 'flow' | 'track' | 'clust' | 'trackclust' | 'region' | 'branch'
    mirrorUids.value = []                // single-image by default; the cluster page re-sets it after
    await fetchChannels()
    await fetchPopmap()
  }

  async function fetchChannels() {
    if (!imageUid.value) return
    try {
      const res = await fetch(`/api/gating/channels?${_params()}`)
      if (!res.ok) throw new Error(`HTTP ${res.status}`)
      const d = await res.json() as { columns: string[]; channels?: string[]; channelNames?: string[]
        valueNames: string[]; valueName?: string; cellMeasures?: string[]; trackAggregates?: string[]
        obsColumns?: string[]; spatialColumns?: string[]; temporalColumns?: string[] }
      columns.value = d.columns ?? []
      obsColumns.value = d.obsColumns ?? []
      spatialColumns.value = d.spatialColumns ?? []
      temporalColumns.value = d.temporalColumns ?? []
      // track gating returns no intensity channels — `columns` are the (motility) track axes; flow
      // returns intensity channels + display names. cellMeasures/trackAggregates are track-only.
      channels.value = d.channels ?? []
      channelNames.value = d.channelNames ?? []
      cellMeasures.value = d.cellMeasures ?? []
      trackAggregates.value = d.trackAggregates ?? []
      valueNames.value = d.valueNames ?? []
      // Adopt the value_name the server actually resolved these columns for. The server falls
      // back an invalid request (e.g. "default") to the *active* segmentation — if the client
      // guessed a different one, channels (for X) and plotdata would be for different
      // segmentations and the plot would come up empty until something forced a re-fetch.
      if (d.valueName && valueNames.value.includes(d.valueName)) valueName.value = d.valueName
      else if (valueNames.value.length && !valueNames.value.includes(valueName.value))
        valueName.value = valueNames.value[0]
    } catch (e) {
      log.error(`Gating channels: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
    }
  }

  async function fetchPopmap() {
    if (!imageUid.value) return
    try {
      const res = await fetch(`/api/gating/popmap?${_params()}`)
      if (!res.ok) throw new Error(`HTTP ${res.status}`)
      const d = await res.json() as { tree: PopTree; canUndo?: boolean; canRedo?: boolean }
      setTree(d.tree)
      _setHistory(d)
    } catch (e) {
      log.error(`Gating popmap: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
    }
  }

  async function fetchStats() {
    const next: Record<string, { count: number; pctParent: number }> = {}
    await Promise.all(flat.value.map(async p => {
      try {
        const res = await fetch(`/api/gating/stats?${_params()}&pop=${encodeURIComponent(p.path)}`)
        if (!res.ok) return
        const s = await res.json() as { count: number; pctParent: number }
        next[p.path] = { count: s.count, pctParent: s.pctParent }
      } catch { /* ignore individual failures */ }
    }))
    stats.value = next
  }

  const addPop = (name: string, gate: GateSpec, parent: string, colour: string) =>
    _post('/api/gating/pop/add', { name, gate, parent, colour })
  // cluster pop: a filter on the run's `clusters.{suffix}` column (fun "in", values = cluster IDs).
  // Starts empty; the manager ticks IDs in via updatePop's filter patch. Set-wide via mirrorUids.
  const addClusterPop = (name: string, suffix: string, colour: string) =>
    _post('/api/gating/pop/add', { name, colour,
      filter: { measure: clusterMeasure(popType.value, suffix), fun: 'in', values: [], default_all: false } })
  // user-defined filter population (Decision 15): a compound AND-ed filter on any obs measures, under a
  // chosen parent, for the current popType. The backend mirrors conditions[1] onto the single fields.
  const addFilterPop = (name: string, parent: string, colour: string, conditions: FilterCondition[]) =>
    _post('/api/gating/pop/add', { name, parent, colour,
      filter: { conditions, measure: conditions[0]?.measure, fun: conditions[0]?.fun,
                values: conditions[0]?.values, default_all: false } })
  // rewrite a filter pop's conditions (edit) — mirror onto single fields as add does.
  const updateFilterPop = (path: string, conditions: FilterCondition[]) =>
    _post('/api/gating/pop/update', { path,
      filter: { conditions, measure: conditions[0]?.measure, fun: conditions[0]?.fun, values: conditions[0]?.values } })
  // boolean population (Decision 16): membership is a set operation over OTHER pops in this map —
  // "nuc-GFP+ OR mem-TOM+", or "both, but NOT CD169+". No gate and no column of its own; it links
  // gates that already exist. Sent whole on both add and edit (a term list has no partial patch).
  const addBooleanPop = (name: string, parent: string, colour: string, spec: BooleanSpec) =>
    _post('/api/gating/pop/add', { name, parent, colour, boolean: spec })
  const updateBooleanPop = (path: string, spec: BooleanSpec) =>
    _post('/api/gating/pop/update', { path, boolean: spec })
  const setGate    = (path: string, gate: GateSpec) => _post('/api/gating/pop/set-gate', { path, gate })
  // Step through the server's history. `_post` handles the response tree + flags like any other
  // mutation, so an undo lands on screen exactly the way the edit it reverses did. `mirrorUids` is
  // empty on the gating pages (it is the cluster pages' set-wide replay), so no mirroring here.
  const undo = () => _post('/api/gating/undo', {})
  const redo = () => _post('/api/gating/redo', {})
  const deletePop  = (path: string)                  => _post('/api/gating/pop/delete', { path })
  // prune the subtree UNDER a pop, keeping the pop — one request, so one undo step (not one per child)
  const deletePopChildren = (path: string)           => _post('/api/gating/pop/delete', { path, childrenOnly: true })
  // Re-parent a population, subtree and all. The gate is untouched; its MEMBERSHIP is not — a pop is
  // its own gate ∩ its parent's, so lifting `/qc/B` out to root re-derives it against all cells.
  const movePop    = (path: string, parent: string)  => _post('/api/gating/pop/move', { path, parent })
  const renamePop  = (path: string, newName: string) => _post('/api/gating/pop/rename', { path, newName })
  const updatePop  = (path: string,
                      patch: { colour?: string; show?: boolean; filter?: Record<string, unknown> }) =>
    _post('/api/gating/pop/update', { path, ...patch })

  // WS push: server broadcasts gating:popmap after any mutation (incl. from other clients / napari)
  // The guard matches the DOCUMENT the broadcast is about — image + segmentation + pop type. popType
  // was missing: an edit to the same image's cluster pops (another tab, the cluster page) broadcasts
  // under the same image/valueName, and its tree would have been applied straight onto the gating
  // page's list. The undo flags are per document too, so they would have been overwritten with the
  // other document's answer.
  function applyBroadcast(data: { imageUid?: string; valueName?: string; popType?: string
                                  tree?: PopTree; canUndo?: boolean; canRedo?: boolean }) {
    if (data.imageUid === imageUid.value && data.valueName === valueName.value &&
        (data.popType === undefined || data.popType === popType.value) && data.tree) {
      setTree(data.tree)
      _setHistory(data)
    }
  }

  // ── Linked brushing (WebGPU viewer picker) ────────────────────────────────────
  // Publish the pop manager's CURRENT selection so the WebGPU viewer can follow it. The overlays
  // route resolves valueName+popType server-side when the client sends nothing (which is what the
  // viewer used to do, defaulting to the ACTIVE segmentation + popType=flow) — but the pop manager
  // is the authoring surface. If the user is gating in `(coastalSm15, clust)`, that is what the
  // viewer should draw, not the active `default` `flow` (Dominik, 2026-08-26: "it should switch
  // depending on the pop manager not depending on the segmentation being shown on the image").
  //
  // Bag keyed by imageUid — one open pop-manager tab per image is the normal shape, and a viewer
  // on image B should not follow a selection change on image A. Written on every selectImage +
  // popType/valueName ref change; the popup window reads it on mount + on storage event.
  const _publishGatingCurrent = () => {
    if (typeof localStorage === 'undefined' || !imageUid.value) return
    const raw = localStorage.getItem('cc.gatingCurrent') ?? '{}'
    let bag: Record<string, { valueName: string; popType: string }> = {}
    try { bag = JSON.parse(raw) } catch { bag = {} }
    bag[imageUid.value] = { valueName: valueName.value, popType: popType.value }
    localStorage.setItem('cc.gatingCurrent', JSON.stringify(bag))
  }
  watch([imageUid, valueName, popType], _publishGatingCurrent, { immediate: true })

  // Publish the cell-selection Z scope so the popup viewer's `pickRectAt` can read it (a
  // localStorage bag is the cross-window channel — the popup has its own Pinia instance). Global,
  // not per-image: the pop manager naturally follows what the user is gating, and the scope is a
  // preference on the workflow.
  const _publishPickZScope = () => {
    if (typeof localStorage === 'undefined') return
    const window = Math.max(0, Math.floor(Number(pickZWindow.value) || 0))
    localStorage.setItem('cc.pickZScope', JSON.stringify({ mode: pickZMode.value, window }))
  }
  watch([pickZMode, pickZWindow], _publishPickZScope, { immediate: true })
  // A change in the pop manager's (imageUid, valueName, popType) with no other mutation still
  // means the viewer should redraw — the other ping-firing sites (`_post`, `refreshPops`,
  // `refreshOverlays`) only fire on pop mutations or explicit refresh, not on tab switches.
  // Without this the viewer keeps drawing yesterday's popType until the user gates something.
  watch([imageUid, valueName, popType], () => {
    if (typeof localStorage !== 'undefined' && imageUid.value) {
      localStorage.setItem('cc.viewerOverlaysTick', `${imageUid.value}:${Date.now()}`)
    }
  })

  // Ping the browser volume viewer via localStorage — /viewer-window is a popup with its own store
  // (P2), so a `pop.show` change here needs a channel to reach it. The tick's VALUE carries the
  // imageUid so a popup on image A doesn't refetch on a change to image B (broadcasts are cheap
  // but noisy). See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md P5.
  const _pingViewer = () => {
    if (typeof localStorage !== 'undefined' && imageUid.value) {
      // Publish the current (valueName, popType) BEFORE the tick, so a viewer that refetches on the
      // storage event reads the up-to-date selection rather than a stale one.
      _publishGatingCurrent()
      localStorage.setItem('cc.viewerOverlaysTick', `${imageUid.value}:${Date.now()}`)
    }
  }
  // Refresh POPULATIONS on the WebGPU viewer after a per-pop visibility change.
  // PopulationManager's per-pop checkbox is the primary surface, so a change here MUST reach the
  // popup window.
  const refreshPops = () => { _pingViewer(); return Promise.resolve(true) }
  // Unified refresh used by the manager's per-pop visibility toggle — the WebGPU viewer routes to
  // tracks or points itself based on `popType`, so this is just a ping.
  const refreshOverlays = () => { _pingViewer(); return Promise.resolve(true) }
  // Clear the transient cell-selection pop: the server drops it from the registry, re-broadcasts
  // the tree without it. Used by the × button in `CellSelectionTools`; the WebGPU picker writes
  // into the same registry (`_set_pick_selection!`), so this clear is symmetric.
  async function clearSelection(): Promise<boolean> {
    if (!imageUid.value) return false
    try {
      const res = await fetch('/api/viewer/pick-clear', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid: projectUid(), imageUid: imageUid.value,
                               valueName: valueName.value, popType: popType.value }),
      })
      if (!res.ok) {
        const d = await res.json().catch(() => ({})) as { error?: string }
        throw new Error(d.error ?? `HTTP ${res.status}`)
      }
      return true
    } catch (e) {
      log.error(`Clear selection: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
      return false
    }
  }

  return {
    imageUid, valueName, popType, mirrorUids, tree, columns, obsColumns, channels, channelNames, valueNames,
    spatialColumns, temporalColumns, spatialAxes, isSpatialAxis, defaultTransformFor,
    cellMeasures, trackAggregates, stats, popVersion, flat,
    transientPaths, pickZMode, pickZWindow,
    projectUid, napariSetUid, colLabel, selectImage, fetchChannels, fetchPopmap, fetchStats,
    addPop, addClusterPop, addFilterPop, updateFilterPop, addBooleanPop, updateBooleanPop, setGate, deletePop, deletePopChildren, movePop,
    renamePop, updatePop, applyBroadcast,
    canUndo, canRedo, undo, redo,
    refreshPops, refreshOverlays, clearSelection,
  }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useGatingStore, import.meta.hot))
