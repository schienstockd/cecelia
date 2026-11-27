import { ref, computed, watch, reactive, provide, inject, onActivated, onScopeDispose,
         type InjectionKey, type Reactive } from 'vue'
import { useLogStore } from './log'
import { errorMessage, getJson, postJson } from '../utils/api'
import { useWsStore } from './ws'
import { useProjectMetaStore } from './projectMeta'
import { useProjectStore } from './project'
import { useImageMetaRefresh } from '../composables/useImageMetaRefresh'
import { clusterMeasure } from '../utils/clusterMeasure'
import { centroidLabel, defaultTransformForCol } from '../utils/gatingAxes'
import { measureColumnLabel } from '../utils/trackAxis'
import { popPath } from '../utils/popName'
import { gateSignatures, changedPopPaths, booleanDeps, statsToFetch } from '../utils/popChanges'
import { OVERLAY_TICK_KEY, encodeOverlayTick, lookOf, widerScope, writeScope, type OverlayTickScope } from '../utils/viewer/overlayTick'

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
  // slice / quadrant are SPLIT gates — one divider, one population per region (plots/splitGeometry.ts);
  // range / bisector are 1D, on one channel's histogram: y_channel = x_channel (plots/histGeometry.ts)
  kind: 'rectangle' | 'polygon' | 'slice' | 'quadrant' | 'range' | 'bisector'
  x_channel: string; y_channel: string
  x_transform: TransformSpec; y_transform: TransformSpec
  x_min?: number; x_max?: number; y_min?: number; y_max?: number
  vertices?: [number, number][]              // polygon outline · slice dividing line
  side?: number                              // slice: 1 = left of the line's direction, -1 = right
  centre?: [number, number]; arms?: [number, number][]; quadrant?: number   // quadrant: 4 CCW arms, sector 1–4
  group?: string                             // split: the id the region populations share
  lo?: number; hi?: number                   // range: the interval
  cut?: number                               // bisector: the cut (side 1 = above it, -1 = at or below)
  rule?: GateRule | FollowRule               // bisector: cut at the parent's density valley instead ·
                                             // rectangle / polygon: move with the cloud it was drawn on
  rule_ok?: boolean                          // server outline: the rule found its valley / cloud on this parent
  rule_peak?: [number, number]               // server outline: the peak a following shape found here (display coords)
  lo_rule?: GateRule; hi_rule?: GateRule     // range: an end that follows its own valley
  lo_rule_ok?: boolean; hi_rule_ok?: boolean // server outline: that end's rule found a valley
}
// a bisector's rule: the deepest valley of the parent's density in [lo, hi] (gate coordinates)
export interface GateRule { kind: 'valley'; lo: number; hi: number; adjust?: number }
// a rectangle / polygon's rule: on each parent it moves by how far the cloud it was drawn on moved (the
// server anchors it where it is saved and never sends the anchor — plots/followRule.ts)
export interface FollowRule { kind: 'follow'; adjust?: number }
// one AND-ed condition of a compound filter (Decision 15)
export interface FilterCondition { measure: string; fun: string; values: unknown }
// a filter spec: the single measure/fun/values (back-compat) plus optional AND-ed `conditions`
export interface FilterSpec { measure: string; fun: string; values: unknown; default_all: boolean; conditions?: FilterCondition[] }

// a boolean spec (Decision 16): the included terms combined with `op`, minus every excluded term.
// `pops: []` + `not: [x]` is the plain "everything here except x" case.
export interface BooleanSpec { op: 'and' | 'or'; pops: string[]; not: string[] }

export interface PopNode {
  name: string; colour: string; show: boolean
  // Stable gating-map identity (`PopulationTree._fresh_pop_uid` on the server, always emitted by
  // `to_tree` in `app/src/gating/popmanager/persistence.jl`). Preserved across rename/reparent —
  // used by `utils/popRenameRemap` to keep path-keyed highlights alive when a rename rewrites the
  // pop's path. Optional on the wire type for legacy pop trees that predate the field.
  uid?: string
  gate?: GateSpec
  filter?: FilterSpec
  boolean?: BooleanSpec
  is_track?: boolean
  transient?: boolean              // ephemeral (viewer cell selection) — not persisted
  membership_sig?: string          // explicit-label pops (viewer selection): hash of label set
  createdBy?: unknown              // `author_stamp()` — present on a gate Claude proposed (utils/authorStamp)
  updatedBy?: unknown              // who last edited a proposal — a person's edit makes it theirs
  trackClassOf?: string            // "mostly <child>": the parent's cells whose track is classed as this child
  runAt?: string                   // cluster pops: the run its clusters were picked on (`clusterPopStale`)
  children: PopNode[]
}
export interface PopTree { value_name: string; pop_type: string; populations: PopNode[] }

// flattened view for the manager (path-keyed, with depth). `uid` is the pop's stable identity;
// empty string when the source tree carries none (a legacy sidecar that predates the field).
export interface FlatPop {
  path: string; name: string; uid: string; parent: string; colour: string; show: boolean
  depth: number; gate?: GateSpec; transient?: boolean
  filter?: FilterSpec  // cluster / region / user-defined filter pops
  boolean?: BooleanSpec  // a combination of OTHER populations (Decision 16)
  createdBy?: unknown    // who drew it, when Claude did (`claudeProposal`)
  updatedBy?: unknown    // who last edited it, on a proposal
  runAt?: string         // cluster pops: the run its clusters were picked on
  trackClassOf?: string  // "mostly <child>": that child's path (utils/trackedChild.ts)
}

function flatten(tree: PopTree): FlatPop[] {
  const out: FlatPop[] = []
  const walk = (nodes: PopNode[], parent: string, depth: number) => {
    for (const n of nodes) {
      const path = popPath(parent, n.name)
      out.push({ path, name: n.name, uid: n.uid ?? '', parent, colour: n.colour, show: n.show,
                 depth, gate: n.gate, transient: n.transient, filter: n.filter, boolean: n.boolean,
                 createdBy: n.createdBy, updatedBy: n.updatedBy, runAt: n.runAt, trackClassOf: n.trackClassOf })
      walk(n.children ?? [], path, depth + 1)
    }
  }
  walk(tree.populations ?? [], 'root', 0)
  return out
}

// ── The gating store: ONE PER CANVAS, owned by it ──────────────────────────────────────────────────
// Each canvas that works on a population tree — GatingPlots (the Gate + Track pages), ClusterPlots
// (the three cluster pages) and LayoutCanvas (the Analysis board's cluster slots) — creates its OWN
// store with `provideGatingStore(popType)` and passes it to its own composables; everything below it
// reads it with `useGatingStore()`. It used to be one app-wide Pinia store that every page rebound to
// its own (image, segmentation, pop type) — so a page came back to a store another page had moved,
// and every one of them carried guards against that ("the singleton flips under us"). Owning it:
//  - no page can move another's document, so returning to a page is instant (App.vue keeps pages
//    alive) and there is nothing to guard;
//  - the store lives and pauses with its page (composables/useKeepAlive.ts) — a hidden page's store
//    does not publish to the viewer or refetch;
//  - two canvases on the SAME document (a cluster page and the board) stay in step through the
//    server's `gating:pop-tree` push, which every store subscribes to for its own document.
// Enforced by `utils/gatingStoreOwnership.test.ts`.
// Exported for its unit test only; a component calls provideGatingStore.
export function createGatingStore(initialPopType: string) {
  const log = useLogStore()
  const ws = useWsStore()
  const meta = useProjectMetaStore()
  const projStore = useProjectStore()

  const imageUid  = ref<string | null>(null)
  const valueName = ref<string>('default')
  const popType   = ref<string>(initialPopType)

  // Set-wide cluster pops: `imageUid` is the primary image (drives the displayed tree/stats), and
  // `mirrorUids` are the OTHER clustered images the same pop mutation is replayed to, so a cluster
  // pop (a filter on `clusters.{suffix}`, which is image-independent) lands identically on every
  // image in the run. Empty for ordinary single-image gating. Set by the cluster page.
  const mirrorUids = ref<string[]>([])

  const tree      = ref<PopTree>({ value_name: 'default', pop_type: 'flow', populations: [] })
  const columns   = ref<string[]>([])           // gateable feature columns (raw var names)
  const obsColumns = ref<string[]>([])          // per-cell obs measures (regions.*/clusters.*/hmm.*/is.aggregate/speed…) — filter-pop measures
  const channels  = ref<string[]>([])           // intensity columns, e.g. mean_intensity_0 (ordered)
  const channelNames = ref<string[]>([])         // display names by CHANNEL index — resolve a column via colLabel
  // spatial/temporal centroid axes (obsm) — gateable + visualisable scatter axes (centroid_x/_y/_z, centroid_t)
  const spatialColumns  = ref<string[]>([])
  const temporalColumns = ref<string[]>([])
  const valueNames = ref<string[]>([])
  // track gating only (popType==='track'): cell measures aggregatable into per-track properties,
  // and the aggregate suffixes — the client builds an axis `{measure}.{agg}` (server inverts it).
  const cellMeasures = ref<string[]>([])
  const trackAggregates = ref<string[]>([])
  const stats     = ref<Record<string, { count: number; pctParent: number }>>({})
  // path → gate/filter columns this image's table lacks, for each pop that is EMPTY because of it
  // (its own column or an ancestor's — `missingColumns` beside every server `tree`, #1769). Absent ⇒ none.
  const missingColumns = ref<Record<string, string[]>>({})
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
  // transient pops (e.g. the viewer cell selection) — auto-highlighted on the plots
  const transientPaths = computed(() => flat.value.filter(p => p.transient).map(p => p.path))
  const projectUid = () => meta.current?.uid ?? ''
  // the set the gated image belongs to — keys the per-set viewer point size (see settings store)
  const viewerSetUid = () => (imageUid.value ? projStore.setUidOfImage(imageUid.value) : null) ?? ''

  // resolve a raw column to its display label: intensity columns → channel name by channel index
  // (channelLabel — `nuc_mean_intensity_2` → "nuc_<ch2>"), centroids → "X position"/"Time"
  // (centroidLabel), everything else (morphology, etc.) stays as-is.
  // (a per-track summary of a cell measure reads "<channel> median" — utils/trackAxis.ts)
  function colLabel(col: string): string {
    return measureColumnLabel(col, { channels: channels.value, channelNames: channelNames.value,
      cellMeasures: cellMeasures.value, trackAggregates: trackAggregates.value }) ?? centroidLabel(col)
  }

  // ── Undo / redo (hand-drawn gating only: flow + track) ────────────────────────
  // History lives on the SERVER — the whole population tree is one serialisable document, and the
  // server is the only place that sees every writer (this tab, another tab, viewer). Keeping a
  // client stack would give each tab its own idea of "before". So these two are just the server's
  // answer, refreshed with the tree on every fetch and every broadcast.
  const canUndo = ref(false)
  const canRedo = ref(false)
  // absent fields (a response that predates this, or a pop type with no history) read as "no history"
  const _setHistory = (d: { canUndo?: boolean; canRedo?: boolean }) => {
    canUndo.value = d.canUndo === true
    canRedo.value = d.canRedo === true
  }

  // ── P3b — gating labels-version pin (docs/todo/VN_VERSIONING_PLAN.md → P3b) ─────
  // Two server-reported fields drive the drift banner:
  //   authoredLabelsVersion       — the labels vN the map was last saved against (`null` on
  //                                 legacy pre-breadcrumb files and on maps whose value_name has
  //                                 no label_props yet).
  //   currentLatestLabelsVersion  — the image's current `_latest` labels vN (`null` when no
  //                                 label_props entry exists yet).
  // The banner renders when both are non-null AND they differ. `labelsVersionPin` is the session
  // choice — `null` = follow `_latest` (default), `"vN"` = pin every gating request to that vN.
  // Reset on `selectImage` so a per-image drift resolution doesn't leak across images.
  const authoredLabelsVersion = ref<string | null>(null)
  // uids of the flow pops whose tracking run made this segmentation's tracks (`pop_track_sources`):
  // deleting one warns that its tracks stop drawing as ribbons
  const trackSources = ref<string[]>([])
  const currentLatestLabelsVersion = ref<string | null>(null)
  const labelsVersionPin = ref<string | null>(null)
  const driftDetected = computed(() =>
    !!authoredLabelsVersion.value && !!currentLatestLabelsVersion.value &&
    authoredLabelsVersion.value !== currentLatestLabelsVersion.value)

  function bump(p: string) {
    popVersion.value = { ...popVersion.value, [p]: (popVersion.value[p] ?? 0) + 1 }
  }

  // single entry point for tree updates: diff vs current, bump the membership version of every
  // pop whose definition changed AND everything that follows it — descendants and boolean pops
  // combining it (`utils/popChanges.ts`) — then refresh the stats of exactly those pops.
  // `full` (a fresh read of the document: image switch, labels-version pin) refreshes every pop's.
  function setTree(next: PopTree, missing?: Record<string, string[]>, full = false) {
    const oldSig = gateSignatures(tree.value.populations ?? [])
    tree.value = next
    missingColumns.value = missing ?? {}
    const pops = next.populations ?? []
    const changed = changedPopPaths(oldSig, gateSignatures(pops), booleanDeps(pops))
    changed.forEach(bump)
    void fetchStats(full ? undefined : changed)
  }

  function _params() {
    const base = `projectUid=${projectUid()}&imageUid=${imageUid.value}&valueName=${valueName.value}&popType=${popType.value}`
    // P3b: append `labelsVersion` when a session pin is set; the backend threads it into every
    // `label_props(img; value_name=vn, version=…)` read (membership eval + plot data). Empty on
    // the common auto-latest path so the URL shape is byte-identical for existing tests.
    return labelsVersionPin.value ? `${base}&labelsVersion=${labelsVersionPin.value}` : base
  }

  async function _post(path: string, body: Record<string, unknown>) {
    // the document as it stood before this write — what the viewer's tick is scoped against. Not the
    // tree when the reply lands: the server broadcasts before it answers, so by then `applyBroadcast`
    // has usually set the new tree already and a diff against it finds nothing.
    const before = tree.value.populations ?? []
    try {
      // quiet: every failure here is logged below under `gating`, with the server's text
      const data = (await postJson<{ tree?: PopTree; missingColumns?: Record<string, string[]>
          canUndo?: boolean; canRedo?: boolean } | null>(path,
        // `labelsVersion`: the reply's `missingColumns` answers in this session's pinned frame
        { projectUid: projectUid(), imageUid: imageUid.value,
          valueName: valueName.value, popType: popType.value,
          ...(labelsVersionPin.value ? { labelsVersion: labelsVersionPin.value } : {}),
          ...body }, { quiet: true })) ?? {}
      // set-wide: replay the SAME mutation to the other clustered images (cluster pops only). The
      // body is image-independent (filter on clusters.{suffix}), so paths stay in sync. Await these
      // BEFORE applying the tree, so anything that reloads off the tree (e.g. the set-pooled heatmap)
      // sees every image's write already persisted, not a half-written set.
      // A refusal there (e.g. 409 `cluster_taken` on an image whose populations differ) is reported, not
      // swallowed: the run's images would otherwise disagree without a word.
      if (mirrorUids.value.length) await Promise.all(mirrorUids.value.map(uid =>
        postJson(path, { projectUid: projectUid(), imageUid: uid,
                         valueName: valueName.value, popType: popType.value, ...body }, { quiet: true })
          .then(() => undefined, e => {
            log.error(`Gating (image ${uid}): ${errorMessage(e)}`, { source: 'gating' })
          })))
      if (data.tree) setTree(data.tree, data.missingColumns)
      const pops = data.tree?.populations ?? []
      const changed = data.tree
        ? changedPopPaths(gateSignatures(before), gateSignatures(pops), booleanDeps(pops)) : undefined
      _setHistory(data)
      // Any pop mutation (add/delete/rename/setGate/updatePop) changes what the viewer draws — the
      // /viewer-window popup has its own store, so it needs the localStorage ping. Scoped by what
      // changed: a colour or show change ships its look and fetches nothing, a membership change
      // fetches this document's populations alone (`utils/viewer/overlayTick.ts`). Held for Apply in
      // manual-apply mode (`holdViewerPings`).
      _pingViewerAfterApply(writeScope(changed))
      return true
    } catch (e) {
      log.error(`Gating: ${errorMessage(e)}`, { source: 'gating' })
      return false
    }
  }

  async function selectImage(uid: string, vn?: string, pt?: string) {
    imageUid.value = uid
    if (vn) valueName.value = vn
    if (pt) popType.value = pt           // 'flow' | 'track' | 'clust' | 'trackclust' | 'region' | 'branch'
    mirrorUids.value = []                // single-image by default; the cluster page re-sets it after
    // P3b: a labels-version pin is session-and-image scoped — moving to a different image resets it
    // so a resolved-drift decision on image A doesn't silently follow onto image B (which has its
    // own authored vN and may have no drift).
    labelsVersionPin.value = null
    await fetchChannels()
    await fetchPopTree()
  }

  async function fetchChannels() {
    if (!imageUid.value) return
    try {
      // quiet: the failure is logged below under `gating`
      const d = await getJson<{ columns: string[]; channels?: string[]; channelNames?: string[]
        valueNames: string[]; valueName?: string; cellMeasures?: string[]; trackAggregates?: string[]
        obsColumns?: string[]; spatialColumns?: string[]; temporalColumns?: string[] }>(
        `/api/gating/channels?${_params()}`, { quiet: true })
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
      log.error(`Gating channels: ${errorMessage(e)}`, { source: 'gating' })
    }
  }

  async function fetchPopTree() {
    if (!imageUid.value) return
    try {
      // quiet: the failure is logged below under `gating`
      const d = await getJson<{ tree: PopTree; canUndo?: boolean; canRedo?: boolean;
        missingColumns?: Record<string, string[]>
        authoredLabelsVersion?: string | null; currentLatestLabelsVersion?: string | null
        trackSources?: string[] }>(
        `/api/gating/pop-tree?${_params()}`, { quiet: true })
      setTree(d.tree, d.missingColumns, true)
      _setHistory(d)
      // P3b: capture the breadcrumb pair. `undefined` (a pre-P3b backend) reads the same as `null`
      // → no drift banner, unchanged behaviour. Once populated, `driftDetected` drives the module.
      authoredLabelsVersion.value = d.authoredLabelsVersion ?? null
      currentLatestLabelsVersion.value = d.currentLatestLabelsVersion ?? null
      trackSources.value = d.trackSources ?? []
    } catch (e) {
      log.error(`Gating population tree: ${errorMessage(e)}`, { source: 'gating' })
    }
  }

  // P3b banner actions. `pinToAuthored` sets the session pin to the authored vN so every subsequent
  // gating request reads at that vN — membership + plot dots + density all shift onto the labels
  // the map was authored against. `clearPin` returns to `_latest`. Both trigger a pop tree refresh so
  // the tree + membership snap to the new evaluation frame in one tick.
  async function pinToAuthored() {
    if (!authoredLabelsVersion.value) return
    labelsVersionPin.value = authoredLabelsVersion.value
    // the axis pickers too: /channels answers per pin, so a `_latest`-only column isn't left on offer
    await fetchChannels(); await fetchPopTree(); await fetchStats()
  }
  async function clearLabelsVersionPin() {
    labelsVersionPin.value = null
    await fetchChannels(); await fetchPopTree(); await fetchStats()
  }

  /** Fetch the counts of `only` (every pop when absent); the others keep theirs — `statsToFetch`.
   *  The replies merge into the counts as they are when they land, not into a copy taken now: a second
   *  edit's refresh in flight must not put back a count the first one has since replaced. */
  async function fetchStats(only?: ReadonlySet<string>) {
    const paths = flat.value.map(p => p.path)
    const { fetch: want } = statsToFetch(paths, stats.value, only)
    const got: Record<string, { count: number; pctParent: number }> = {}
    await Promise.all(want.map(async path => {
      try {
        // quiet: one request per pop, so a backend restart would log a line per population
        const s = await getJson<{ count: number; pctParent: number }>(
          `/api/gating/stats?${_params()}`, { query: { pop: path }, quiet: true })
        got[path] = { count: s.count, pctParent: s.pctParent }
      } catch { /* ignore individual failures */ }
    }))
    // the pops that exist NOW keep their latest count (a full refresh drops the ones it did not get)
    const live = new Set(flat.value.map(p => p.path))
    const next: Record<string, { count: number; pctParent: number }> = {}
    if (only) for (const [p, v] of Object.entries(stats.value)) if (live.has(p)) next[p] = v
    for (const [p, v] of Object.entries(got)) if (live.has(p)) next[p] = v
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
  const newFilter = (conditions: FilterCondition[]) => ({ conditions, measure: conditions[0]?.measure,
    fun: conditions[0]?.fun, values: conditions[0]?.values, default_all: false })
  const addFilterPop = (name: string, parent: string, colour: string, conditions: FilterCondition[]) =>
    _post('/api/gating/pop/add', { name, parent, colour, filter: newFilter(conditions) })
  // the same filter child under several parents (bulk "Add tracked cells") — one request, one undo step,
  // all or none; each child takes the colour given with its parent
  const addFilterPopUnder = (name: string, under: { parent: string; colour: string }[], conditions: FilterCondition[]) =>
    _post('/api/gating/pop/add', { name, under, filter: newFilter(conditions) })
  // "mostly <child>" under `parent`, one per sub-population still without one — one request, one undo
  const addTrackClassPops = (parent: string) => _post('/api/gating/pop/add-track-classes', { parent })
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
  // every region of a split gate (slice / quadrant) at once — one request, one undo step
  const addSplit = (gate: GateSpec, parent: string, pops: { name: string; colour: string; region: number }[]) =>
    _post('/api/gating/pop/add-split', { gate, parent, pops })
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
  // A multi-selection (the manager's ctrl/shift-click) in ONE request — one save, one broadcast, one
  // undo step — not one request per population each rewriting the tree. A selected child of a selected
  // parent rides with the parent; a refused selection changes nothing (`move_pops!` / `del_pops!`).
  const movePops   = (paths: string[], parent: string) => _post('/api/gating/pop/move', { paths, parent })
  const deletePops = (paths: string[])                 => _post('/api/gating/pop/delete', { paths })
  const renamePop  = (path: string, newName: string) => _post('/api/gating/pop/rename', { path, newName })
  // the user has checked a stale cluster population's clusters against the run as it is now (the stale icon)
  // show/hide several populations in ONE write (the manager's "all" toggle) — one reply, one broadcast,
  // one viewer refetch, not one of each per population
  const setPopsShown = (paths: string[], show: boolean) => _post('/api/gating/pop/update', { paths, show })
  const confirmClusterRun = (path: string) => _post('/api/gating/pop/update', { path, runChecked: true })
  const updatePop  = (path: string,
                      patch: { colour?: string; show?: boolean; filter?: Record<string, unknown> }) =>
    _post('/api/gating/pop/update', { path, ...patch })

  // WS push: server broadcasts gating:pop-tree after any mutation (incl. from other clients / viewer)
  // The guard matches the DOCUMENT the broadcast is about — image + segmentation + pop type. popType
  // was missing: an edit to the same image's cluster pops (another tab, the cluster page) broadcasts
  // under the same image/valueName, and its tree would have been applied straight onto the gating
  // page's list. The undo flags are per document too, so they would have been overwritten with the
  // other document's answer.
  function applyBroadcast(data: { imageUid?: string; valueName?: string; popType?: string
                                  tree?: PopTree; missingColumns?: Record<string, string[]>
                                  canUndo?: boolean; canRedo?: boolean }) {
    if (data.imageUid === imageUid.value && data.valueName === valueName.value &&
        (data.popType === undefined || data.popType === popType.value) && data.tree) {
      // a broadcast answers `missingColumns` at `_latest`; a session pinned to another labels
      // version keeps its own (pinned) answer, refreshed by its own edits and fetchPopTree
      // every count, not just the changed pops': a broadcast also follows writes this store did not
      // make, and a task that rewrote the cell table (a clustering re-run) moves counts with no gate
      // change. Cheap: the server evaluates the map once per edit and answers the rest from its cache.
      setTree(data.tree, labelsVersionPin.value ? missingColumns.value : data.missingColumns, true)
      _setHistory(data)
    }
  }

  // ── Linked brushing (WebGPU viewer picker) ────────────────────────────────────
  // Publish the pop manager's CURRENT selection so the WebGPU viewer can follow it. The overlays
  // route resolves valueName+popType server-side when the client sends nothing (which is what the
  // viewer used to do, defaulting to the ACTIVE segmentation + popType=flow) — but the pop manager
  // is the authoring surface. If the user is gating in `(coastalSm15, clust)`, that is what the
  // viewer should draw, not the active `default` `flow` ("it should switch
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

  // A change in the pop manager's (imageUid, valueName, popType) with no other mutation still
  // means the viewer should redraw — the other ping-firing sites (`_post`, `refreshPops`,
  // `refreshOverlays`) only fire on pop mutations or explicit refresh, not on tab switches.
  // Without this the viewer keeps drawing yesterday's popType until the user gates something.
  watch([imageUid, valueName, popType], () => {
    if (typeof localStorage !== 'undefined' && imageUid.value) {
      localStorage.setItem(OVERLAY_TICK_KEY, encodeOverlayTick({ imageUid: imageUid.value, scope: 'all' }))
    }
  })

  // Live updates for THIS store's document (another page's store on the same document, another tab,
  // the viewer's picker, an MCP edit): the server pushes the new tree to every client after any
  // mutation. Subscribed here, not in each canvas, so every owner gets it and none can forget it.
  const onPopTreePush = (d: unknown) => applyBroadcast(d as Parameters<typeof applyBroadcast>[0])
  ws.on('gating:pop-tree', onPopTreePush)
  onScopeDispose(() => ws.off('gating:pop-tree', onPopTreePush))
  // WS reconnect: the backend may have restarted, and the transient viewer-selection pop lived only in
  // its memory (docs/POPULATION.md) — refetch so a stale selection stops greying the plots. The COUNT,
  // not `status`: while the page is hidden this watcher is paused and sees only the latest value.
  watch(() => ws.connects, (n) => {
    if (n > 1 && imageUid.value) void fetchChannels().then(fetchPopTree)
  })
  // Channel renamed (image table, metadata panel): `channelNames` is a once-per-select copy, so
  // refetch or the axis labels keep the old names until the image is reselected.
  useImageMetaRefresh(() => (imageUid.value ? [imageUid.value] : []), () => { void fetchChannels() })

  // Ping the browser volume viewer via localStorage — /viewer-window is a popup with its own store
  // (P2), so a `pop.show` change here needs a channel to reach it. The tick's VALUE carries the
  // imageUid so a popup on image A doesn't refetch on a change to image B (broadcasts are cheap
  // but noisy). See docs/todo/VIEWER_CONTROLS_SPLIT_PLAN.md P5.
  // The scope still to send: a held `pops` ping followed by a `look` one must still refetch
  // (`widerScope`), so pings accumulate here until one goes out.
  let _pendingScope: OverlayTickScope | null = null
  const _sendTick = (uid: string) => {
    const scope = _pendingScope ?? 'all'
    _pendingScope = null
    // Publish the current (valueName, popType) BEFORE the tick, so a viewer that refetches on the
    // storage event reads the up-to-date selection rather than a stale one. A tick for an image this
    // store has since left is unscoped: its document is no longer the one named here.
    const own = uid === imageUid.value
    if (own) _publishGatingCurrent()
    localStorage.setItem(OVERLAY_TICK_KEY, encodeOverlayTick(own
      ? { imageUid: uid, scope, valueName: valueName.value, popType: popType.value,
          // the look as it stands when the tick goes out — after Apply, the latest
          ...(scope === 'look' ? { look: lookOf(flat.value) } : {}) }
      : { imageUid: uid, scope: 'all' }))
  }
  const _pingViewer = (scope: OverlayTickScope) => {
    _pendingScope = widerScope(_pendingScope, scope)
    if (typeof localStorage !== 'undefined' && imageUid.value) _sendTick(imageUid.value)
  }
  // Manual apply holds the viewer reload a gate write triggers, so a hierarchy can be built on a
  // large image before the viewer draws any of it. The owning canvas hands in its Apply queue
  // (`usePopSelectionMode.deferUntilApply`); without one the ping goes out at once. An explicit
  // reload (`pingViewer`) does what a held one would, so it drops it.
  let _viewerHold: { defer: (run: () => void) => void; drop: () => void } | null = null
  const holdViewerPings = (h: typeof _viewerHold) => { _viewerHold = h }
  function _pingViewerAfterApply(scope: OverlayTickScope = 'all') {
    const uid = imageUid.value
    _pendingScope = widerScope(_pendingScope, scope)
    const ping = () => {
      if (typeof localStorage === 'undefined' || !uid) return
      _sendTick(uid)
    }
    if (_viewerHold) _viewerHold.defer(ping)
    else ping()
  }
  const pingViewer = (scope: OverlayTickScope = 'all') => { _viewerHold?.drop(); _pingViewer(scope) }
  // Refresh POPULATIONS on the WebGPU viewer after a per-pop visibility change (or the point size).
  // PopulationManager's per-pop checkbox is the primary surface, so a change here MUST reach the
  // popup window. Nothing about membership changed: the viewer re-reads its eyes and draws what it holds.
  const refreshPops = () => { pingViewer('look'); return Promise.resolve(true) }
  // Unified refresh used by the manager's per-pop visibility toggle — the WebGPU viewer routes to
  // tracks or points itself based on `popType`, so this is just a ping.
  const refreshOverlays = () => { pingViewer('look'); return Promise.resolve(true) }
  // Clear the transient cell-selection pop: the server drops it from the registry, re-broadcasts
  // the tree without it. Used by the × button in `CellSelectionTools`; the WebGPU picker writes
  // into the same registry (`_set_pick_selection!`), so this clear is symmetric.
  async function clearSelection(): Promise<boolean> {
    if (!imageUid.value) return false
    try {
      // quiet: the failure is logged below under `gating`
      await postJson('/api/viewer/pick-clear', { projectUid: projectUid(), imageUid: imageUid.value,
        valueName: valueName.value, popType: popType.value }, { quiet: true })
      return true
    } catch (e) {
      log.error(`Clear selection: ${errorMessage(e)}`, { source: 'gating' })
      return false
    }
  }

  return {
    imageUid, valueName, popType, mirrorUids, tree, columns, obsColumns, channels, channelNames, valueNames,
    spatialColumns, temporalColumns, spatialAxes, isSpatialAxis, defaultTransformFor,
    cellMeasures, trackAggregates, stats, missingColumns, popVersion, flat,
    transientPaths,
    projectUid, viewerSetUid, colLabel, selectImage, fetchChannels, fetchPopTree, fetchStats,
    addPop, addClusterPop, addFilterPop, addFilterPopUnder, addTrackClassPops, updateFilterPop, addBooleanPop, updateBooleanPop, setGate, addSplit, deletePop, deletePopChildren, movePop, movePops, deletePops,
    renamePop, updatePop, setPopsShown, confirmClusterRun, applyBroadcast,
    canUndo, canRedo, undo, redo,
    refreshPops, refreshOverlays, clearSelection, pingViewer, holdViewerPings,
    pingViewerAfterApply: _pingViewerAfterApply,
    // P3b — labels-version pin
    authoredLabelsVersion, currentLatestLabelsVersion, labelsVersionPin, driftDetected, trackSources,
    pinToAuthored, clearLabelsVersionPin,
  }
}

export type GatingStore = Reactive<ReturnType<typeof createGatingStore>>
const GATING_STORE: InjectionKey<GatingStore> = Symbol('gatingStore')

/**
 * Create THIS canvas's gating store and hand it to everything below it. Call once, in the canvas that
 * owns the population tree (see the block comment on `createGatingStore`), and use the returned store
 * directly there — `inject` cannot see a `provide` made by the same component.
 */
export function provideGatingStore(popType: string): GatingStore {
  const g = reactive(createGatingStore(popType))
  provide(GATING_STORE, g)
  // Back on screen: the viewer follows the VISIBLE pop manager — tell it which document that is
  // (the publish watchers above were paused while the page was hidden). Held like a gate write:
  // coming back to a half-built hierarchy must not draw it.
  onActivated(() => g.pingViewerAfterApply())
  return g
}

/** The gating store of the canvas this component sits in. */
export function useGatingStore(): GatingStore {
  const g = inject(GATING_STORE, null)
  if (!g) throw new Error('useGatingStore(): no gating store above this component — its canvas must call provideGatingStore()')
  return g
}
