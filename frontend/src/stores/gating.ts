import { defineStore } from 'pinia'
import { ref, computed, watch } from 'vue'
import { useLogStore } from './log'
import { useProjectMetaStore } from './projectMeta'
import { useSettingsStore } from './settings'

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
export interface PopNode {
  name: string; colour: string; show: boolean
  gate?: GateSpec
  filter?: { measure: string; fun: string; values: unknown; default_all: boolean }
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
  filter?: { measure: string; fun: string; values: unknown; default_all: boolean }  // cluster pops
}

function flatten(tree: PopTree): FlatPop[] {
  const out: FlatPop[] = []
  const walk = (nodes: PopNode[], parent: string, depth: number) => {
    for (const n of nodes) {
      const path = parent === 'root' ? `/${n.name}` : `${parent}/${n.name}`
      out.push({ path, name: n.name, parent, colour: n.colour, show: n.show, depth,
                 gate: n.gate, transient: n.transient, filter: n.filter })
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
      const path = parent === 'root' ? `/${n.name}` : `${parent}/${n.name}`
      // include membership_sig so explicit-label pops (the napari selection) bump their version
      // when their cell set changes — they have no gate/filter to diff on.
      m.set(path, JSON.stringify(n.gate ?? null) + '|' + JSON.stringify(n.filter ?? null)
                  + '|' + (n.membership_sig ?? ''))
      walk(n.children ?? [], path)
    }
  }
  walk(tree.populations ?? [], 'root')
  return m
}

export const useGatingStore = defineStore('gating', () => {
  const log = useLogStore()
  const meta = useProjectMetaStore()
  const settings = useSettingsStore()

  const imageUid  = ref<string | null>(null)
  const valueName = ref<string>('default')
  const popType   = ref<string>('flow')

  // Set-wide cluster pops: `imageUid` is the primary image (drives the displayed tree/stats), and
  // `mirrorUids` are the OTHER clustered images the same pop mutation is replayed to, so a cluster
  // pop (a filter on `clusters.{suffix}`, which is image-independent) lands identically on every
  // image in the run. Empty for ordinary single-image gating. Set by the cluster page.
  const mirrorUids = ref<string[]>([])

  // napari cell-selection z scope: 'stack' = select across the whole z-stack (ignore z),
  // 'slice' = only cells within ±napariZWindow slices of the currently displayed z in napari.
  const napariZMode   = ref<'stack' | 'slice'>('stack')
  const napariZWindow = ref<number>(0)

  const tree      = ref<PopTree>({ value_name: 'default', pop_type: 'flow', populations: [] })
  const columns   = ref<string[]>([])           // gateable feature columns (raw var names)
  const channels  = ref<string[]>([])           // intensity columns, e.g. mean_intensity_0 (ordered)
  const channelNames = ref<string[]>([])         // display names aligned to `channels`
  const valueNames = ref<string[]>([])
  // track gating only (popType==='track'): cell measures aggregatable into per-track properties,
  // and the aggregate suffixes — the client builds an axis `{measure}.{agg}` (server inverts it).
  const cellMeasures = ref<string[]>([])
  const trackAggregates = ref<string[]>([])
  const stats     = ref<Record<string, { count: number; pctParent: number }>>({})
  // per-population membership version — bumped when a pop's (or an ancestor's) gate changes.
  // Panels watch the version of their displayed pop to refresh points smoothly (no full reload).
  const popVersion = ref<Record<string, number>>({})

  const flat = computed(() => flatten(tree.value))
  // transient pops (e.g. the napari cell selection) — auto-highlighted on the plots
  const transientPaths = computed(() => flat.value.filter(p => p.transient).map(p => p.path))
  const projectUid = () => meta.current?.uid ?? ''

  // resolve a raw column to its display label: intensity columns → channel name (R
  // change_channel_names), everything else (morphology, etc.) stays as-is.
  function colLabel(col: string): string {
    const i = channels.value.indexOf(col)
    return i >= 0 && channelNames.value[i] ? channelNames.value[i] : col
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
      const data = await res.json().catch(() => ({})) as { tree?: PopTree; error?: string }
      if (!res.ok) throw new Error(data.error ?? `HTTP ${res.status}`)
      if (data.tree) setTree(data.tree)
      // set-wide: replay the SAME mutation to the other clustered images (cluster pops only). The
      // body is image-independent (filter on clusters.{suffix}), so paths stay in sync; we don't
      // touch the tree from these (the primary already updated it). Fire-and-forget per image.
      if (mirrorUids.value.length) await Promise.all(mirrorUids.value.map(uid =>
        fetch(path, {
          method: 'POST', headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ projectUid: projectUid(), imageUid: uid,
                                 valueName: valueName.value, popType: popType.value, ...body }),
        }).catch(() => undefined)))
      return true
    } catch (e) {
      log.error(`Gating: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
      return false
    }
  }

  async function selectImage(uid: string, vn?: string, pt?: string) {
    imageUid.value = uid
    if (vn) valueName.value = vn
    if (pt) popType.value = pt           // 'flow' | 'track' | 'clust' | 'trackclust'
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
        valueNames: string[]; valueName?: string; cellMeasures?: string[]; trackAggregates?: string[] }
      columns.value = d.columns ?? []
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
      const d = await res.json() as { tree: PopTree }
      setTree(d.tree)
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
      filter: { measure: `clusters.${suffix}`, fun: 'in', values: [], default_all: false } })
  const setGate    = (path: string, gate: GateSpec) => _post('/api/gating/pop/set-gate', { path, gate })
  const deletePop  = (path: string)                  => _post('/api/gating/pop/delete', { path })
  const renamePop  = (path: string, newName: string) => _post('/api/gating/pop/rename', { path, newName })
  const updatePop  = (path: string,
                      patch: { colour?: string; show?: boolean; filter?: Record<string, unknown> }) =>
    _post('/api/gating/pop/update', { path, ...patch })

  // WS push: server broadcasts gating:popmap after any mutation (incl. from other clients / napari)
  function applyBroadcast(data: { imageUid?: string; valueName?: string; tree?: PopTree }) {
    if (data.imageUid === imageUid.value && data.valueName === valueName.value && data.tree)
      setTree(data.tree)
  }

  // ── Napari linked brushing ────────────────────────────────────────────────────
  // Fire-and-forget POSTs: Julia talks to the bridge over WS (returns {ok}, not a tree).
  // `silent` suppresses the error toast (used for the per-pop visibility auto-refresh, which
  // shouldn't nag when napari isn't open).
  async function _napari(path: string, silent = false, extra: Record<string, unknown> = {}): Promise<boolean> {
    if (!imageUid.value) return false
    try {
      const res = await fetch(path, {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid: projectUid(), imageUid: imageUid.value,
                               valueName: valueName.value, popType: popType.value,
                               pointsSize: settings.napariPointSize, ...extra }),
      })
      const d = await res.json().catch(() => ({})) as { error?: string }
      if (!res.ok) throw new Error(d.error ?? `HTTP ${res.status}`)
      return true
    } catch (e) {
      if (!silent) log.error(`Napari: ${e instanceof Error ? e.message : String(e)}`, { source: 'gating' })
      return false
    }
  }
  // re-push populations to napari after a per-pop visibility change (silent if napari is down)
  const refreshNapariPops = () => _napari('/api/napari/show-populations', true)
  // track gating: render the gated tracks as napari Tracks layers (one per pop). Loud (user action).
  const showTracks = () => _napari('/api/napari/show-tracks', false)
  // unified re-push used by the manager's per-pop visibility toggle — routes to the right overlay
  // for the current popType (track → Tracks layers, else → population Points), silent.
  const refreshNapari = () => popType.value === 'track'
    ? _napari('/api/napari/show-tracks', true) : _napari('/api/napari/show-populations', true)
  // add a Shapes layer in napari; drawing on it selects cells → highlighted here. The z scope
  // (whole stack vs ± slices around the live z) is captured now and applied when the polygon closes.
  // a blank/garbage dial value (v-model.number can yield NaN) → 0; never negative
  const zWin = () => Math.max(0, Math.floor(Number(napariZWindow.value) || 0))
  const startCellSelection = () => _napari('/api/napari/start-selection', false,
    { zMode: napariZMode.value, zWindow: zWin() })
  // push the z scope to the active selection and re-evaluate the drawn polygon live (silent — the
  // bridge no-ops when no selection is active, e.g. nothing drawn yet / napari closed)
  const updateSelectionScope = () => _napari('/api/napari/selection-scope', true,
    { zMode: napariZMode.value, zWindow: zWin() })
  // changing the toggle / window re-evaluates the existing selection immediately (and the new
  // value is also picked up by the next startCellSelection for fresh selections)
  watch([napariZMode, napariZWindow], () => { updateSelectionScope() })
  // clear the transient napari cell-selection pop: the server drops it from the registry,
  // re-broadcasts the tree without it, AND removes the "Cell selection" Shapes layer from napari.
  // The transient pop is never persisted, so there's nothing to "delete" — this is how the user
  // removes it (and its draw layer goes with it).
  const clearNapariSelection = () => _napari('/api/napari/stop-selection', true)

  return {
    imageUid, valueName, popType, mirrorUids, tree, columns, channels, channelNames, valueNames,
    cellMeasures, trackAggregates, stats, popVersion, flat,
    transientPaths, napariZMode, napariZWindow,
    projectUid, colLabel, selectImage, fetchChannels, fetchPopmap, fetchStats,
    addPop, addClusterPop, setGate, deletePop, renamePop, updatePop, applyBroadcast,
    refreshNapariPops, showTracks, refreshNapari, startCellSelection, clearNapariSelection, updateSelectionScope,
  }
})
