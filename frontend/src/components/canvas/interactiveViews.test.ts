import { describe, it, expect } from 'vitest'
import { cohortParams } from '../../plots/trackGroups'
import { tkey, parseTkey } from '../../plots/series'
import { resolvePopType } from '../../plots/popTypes'
import { INTERACTIVE_VIEWS, boardViews, pageViews, railFor, popTypesFor, VIEW_ALIASES, migrateViewKey }
  from './interactiveViews'
import { CLUSTER_PANELS, clusterPanelRail } from '../../modules/cluster/clusterPanels'

// the two PICKER HOSTS, read as source: each must derive its options from the registry rather than
// naming view keys (see the recurrence guards below)
const SFC = import.meta.glob(
  ['/src/components/canvas/LayoutCanvas.vue', '/src/modules/gate/GatingPlots.vue'], {
    query: '?raw', import: 'default', eager: true }) as Record<string, string>

describe('interactive view surface flags', () => {
  it('every analysisBoard view lands in exactly one board optgroup', () => {
    const flagged = Object.entries(INTERACTIVE_VIEWS).filter(([, v]) => v.analysisBoard).map(([k]) => k)
    const grouped = (['interactive', 'clustering', 'image'] as const).flatMap(g => boardViews(g).map(v => v.key))
    expect([...grouped].sort()).toEqual([...flagged].sort())
    expect(new Set(grouped).size).toBe(grouped.length)     // no view in two groups
  })

  it('unflagged views are offered nowhere', () => {
    const anywhere = new Set([
      ...boardViews('interactive'), ...boardViews('clustering'), ...boardViews('image'),
      ...pageViews('clusterPage'), ...pageViews('opticalFlowPage'),
    ].map(v => v.key))
    for (const [key, v] of Object.entries(INTERACTIVE_VIEWS))
      expect(anywhere.has(key)).toBe(!!(v.analysisBoard || v.clusterPage || v.opticalFlowPage))
  })

  it('groups the views the board picker separates', () => {
    expect(boardViews('clustering').map(v => v.key)).toContain('umap')
    expect(boardViews('image').map(v => v.key)).toContain('filmstrip')
    expect(boardViews('interactive').map(v => v.key)).toContain('flowMetrics')
  })

  it('the flow views are offered on their module page AND the board', () => {
    expect(pageViews('opticalFlowPage').map(v => v.key))
      .toEqual(['flowMetrics', 'flowTraining', 'flowProbability'])
    for (const k of ['flowMetrics', 'flowTraining', 'flowProbability'])
      expect(boardViews('interactive').map(v => v.key)).toContain(k)
  })

  it('the cluster page offers only cluster-page views', () => {
    expect(pageViews('clusterPage').map(v => v.key)).toEqual(['umap'])
  })

  // The rail is the SECOND thing the board must derive from the registry rather than a key list. The
  // first (the picker) shipped broken once; this pins the same rule for the manager, so a new view
  // cannot land on the board with a rail the board has no branch for — which is exactly how
  // `flowProbability` ended up dead there, permanently asking for a vault the board never rendered.
  const RENDERABLE = ['pops', 'clusterPops', 'flowModels', 'none']
  it('every board-flagged plot declares a rail the board can render', () => {
    for (const [key, v] of Object.entries(INTERACTIVE_VIEWS))
      if (v.analysisBoard) expect(RENDERABLE, `view "${key}"`).toContain(railFor(key))
    for (const [key, d] of Object.entries(CLUSTER_PANELS))
      if (d.analysisBoard) expect(RENDERABLE, `cluster panel "${key}"`).toContain(clusterPanelRail(key))
  })

  it('a plot that needs a model asks for the vault, and the pre-model one does not', () => {
    expect(railFor('flowTraining')).toBe('flowModels')
    expect(railFor('flowProbability')).toBe('flowModels')
    // asked BEFORE a model exists — handing it one turns "what should I train on" into "what did I train"
    expect(railFor('flowMetrics')).toBe('none')
  })

  it('cluster plots all ask for the cluster pop manager', () => {
    expect(railFor('umap')).toBe('clusterPops')
    for (const key of Object.keys(CLUSTER_PANELS)) expect(clusterPanelRail(key)).toBe('clusterPops')
  })

  it('an undeclared plot falls back to the population picker', () => {
    expect(railFor('no-such-view')).toBe('pops')
  })

  // The recurrence guard. `flowMetrics` shipped with `analysisBoard: true` and never showed up, because
  // LayoutCanvas filtered a HARDCODED key list (`ANALYSIS_VIEWS`/`IMAGE_VIEWS`) that the flag could not
  // reach — the flag was a checkbox wired to nothing, and nothing failed. A board host must therefore
  // name no view key at all; it derives every group from the registry.
  it('LayoutCanvas names no view key — the board picker is registry-derived', () => {
    const src = SFC['/src/components/canvas/LayoutCanvas.vue']
    expect(src, 'LayoutCanvas.vue not found — fix the glob').toBeTruthy()
    for (const key of Object.keys(INTERACTIVE_VIEWS)) {
      expect(src, `LayoutCanvas hardcodes the view key "${key}" — build the picker from boardViews()`)
        .not.toMatch(new RegExp(`['"\`]${key}['"\`]`))
    }
  })

  // Same guard for the TRACK canvas. It shipped one `+ Xxx` button per track view — four hardcoded
  // keys — which both made the toolbar wider than the window and meant a new track view needed an edit
  // here to be reachable at all. It now builds the "+ Track…" picker from `pageViews('trackPage')`.
  it('GatingPlots names no view key — the track picker is registry-derived', () => {
    const src = SFC['/src/modules/gate/GatingPlots.vue']
    expect(src, 'GatingPlots.vue not found — fix the glob').toBeTruthy()
    for (const key of Object.keys(INTERACTIVE_VIEWS)) {
      expect(src, `GatingPlots hardcodes the view key "${key}" — build the picker from pageViews('trackPage')`)
        .not.toMatch(new RegExp(`['"\`]${key}['"\`]`))
    }
  })

  // A retired key must MAP, not vanish: `isInteractiveView` would return false and the Track canvas's
  // `v-else` would render a GATING PLOT carrying the old panel's state.
  it('every alias points at a view that exists, and none shadows a live key', () => {
    for (const [from, to] of Object.entries(VIEW_ALIASES)) {
      expect(INTERACTIVE_VIEWS[to], `alias ${from} → ${to} names no view`).toBeTruthy()
      expect(INTERACTIVE_VIEWS[from], `${from} is aliased AND registered`).toBeUndefined()
    }
  })

  it('migrates a saved worklist panel into the timeline that replaced it', () => {
    const state = { kind: 'trackCorrection' }
    expect(migrateViewKey(state)).toBe(true)
    expect(state.kind).toBe('trackScheme')
    const live = { kind: 'trackPaths' }
    expect(migrateViewKey(live)).toBe(false)
    expect(live.kind).toBe('trackPaths')
  })

  it('every track view is offered on the Track page', () => {
    const offered = pageViews('trackPage').map(v => v.key)
    expect(offered).toEqual(
      Object.entries(INTERACTIVE_VIEWS).filter(([, v]) => v.trackPage).map(([k]) => k))
    for (const k of ['trackPaths', 'trackDiagnostics', 'trackScheme'])
      expect(offered).toContain(k)
  })
})

// ── The rail's selection must actually REACH the plot it was picked for ──────────────────────────
//
// This is the shape of a bug that shipped: the Track canvas built its `series` by tagging every ticked
// population with the CANVAS's popType (`track`), while a track panel resolves its family from this
// registry (so `live`, the first one declared). `filterSeriesToPopType` then dropped all of them and the
// panels silently drew the whole segmentation — a picker on screen, clicked, reaching nothing.
//
// The fix is that the picker's rows carry their own family (`tkey`), so what follows is the round trip
// that must hold for EVERY family a track view declares.
describe('a picked population survives the trip to the request', () => {
  const TRACK_VIEWS = ['trackPaths', 'trackDiagnostics', 'trackScheme']

  it('every track view declares its families (without them the rail lists another plot’s)', () => {
    for (const key of TRACK_VIEWS) expect(popTypesFor(key).length).toBeGreaterThan(0)
  })

  it('a row ticked under any declared family arrives as `pops`', () => {
    for (const key of TRACK_VIEWS) {
      for (const fam of popTypesFor(key)) {
        // what the picker emits → what the canvas stores → what the panel asks under
        const target = parseTkey(tkey(fam.popType, 'memTom', '/T cells'))
        const resolved = resolvePopType({ dataSource: { popTypes: popTypesFor(key) } }, fam.popType)
        const p = cohortParams({ imageUids: ['i'], compareMode: 'image', series: [target],
                                 popType: resolved })
        expect([key, fam.popType, p.get('pops')])
          .toEqual([key, fam.popType, 'memTom/T cells'])
      }
    }
  })

  it('a series tagged with a family the panel is NOT showing is dropped — the old bug, pinned', () => {
    // 'track' populations asked for under the 'live' family: no error, no pops, whole segmentation
    const p = cohortParams({ imageUids: ['i'], compareMode: 'image', popType: 'live',
                             series: [{ popType: 'track', valueName: 'memTom', pop: '/T cells' }] })
    expect(p.get('pops')).toBeNull()
  })
})
