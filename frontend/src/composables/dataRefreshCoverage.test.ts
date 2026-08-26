import { describe, it, expect } from 'vitest'

// COVERAGE ratchet for the task-refresh framework (docs/todo/TASK_DATA_REFRESH_PLAN.md).
//
// `useDataRefresh` is the ONE chokepoint — it is what makes the global `autoRefreshOnTask` setting
// govern every plot at once. The failure mode is therefore never "the toggle is broken"; it is a
// surface that never adopted the primitive, which is invisible because everything else on the page
// refreshes around it.
//
// That is exactly what happened: the cluster/region pages' plots each called `useDataRefresh`, so their
// DATA refetched after a run — but `useClusterContext`, which owns the run (suffix) list, the tickable
// cluster/region IDs and the heatmap's feature rows, did not. Re-running clustering left the page on the
// old run list and the old feature rows, with the heatmap still asking for the previous run's columns:
// "I re-ran it and nothing changed."
//
// An EXACT list, not a count — a count silently permits swapping one gap for another. Each entry is a
// surface that owns a data lifecycle of its own (it fetches and holds state that a finished task can
// invalidate). A surface that merely receives data from a host is deliberately absent.
const MUST_REFRESH = [
  // shared data layers — these own the fetch for a whole canvas
  'composables/useSummaryData.ts',      // plot specs, population list, reloadToken for every panel
  'composables/useClusterContext.ts',   // run list, cluster/region IDs, heatmap feature rows + labels
  // self-fetching plots
  'components/plots/UmapView.vue',
  'components/plots/GatingStrategyView.vue',
  'modules/cluster/ClusterHeatmapPanel.vue',
  'modules/cluster/ClusterHmmStatesPanel.vue',
  'modules/cluster/ClusterHmmTransitionsPanel.vue',
  'modules/gate/GatePlotPanel.vue',
  'modules/gate/GatePairsPanel.vue',
  'components/plots/TrackPathsView.vue',
  'components/plots/TrackDiagnosticsView.vue',
  // the correction timeline: tracking, correction and re-measuring all rewrite exactly what it draws
  'components/plots/TrackSchemeView.vue',
  // the gating canvas owns its rail's POPULATION LIST — a run that creates or drops a population
  // changes what can be picked, not just what a panel draws
  'modules/gate/GatingPlots.vue',
]

// Fetches plot/population data but must NOT self-refresh — with the reason, so removing an entry is a
// deliberate act rather than a shrug.
const EXEMPT: Record<string, string> = {
  'components/canvas/SummaryPanel.vue':
    'receives `reloadToken` from its host (useSummaryData owns the refresh) — a second watcher would double-fetch',
  'components/canvas/SummaryCanvas.vue':
    'delegates its whole data lifecycle to useSummaryData',
  'components/canvas/SeriesPicker.vue':
    'renders the population list its host passes in; holds no fetched state',
  'stores/gating.ts':
    'live-updates from the `gating:popmap` broadcast, which is finer-grained than task completion',
  'components/ViewerPanel.vue':
    'viewer reload is a SEPARATE opt-in toggle (viewerAutoUpdate, default off) — expensive on large images',
  'composables/useNapariAutoShow.ts': 'napari overlay state, not a plot',
  'tasks/ParamRenderer.vue': 'a form; its options come from image metadata, not from task output',
  'modules/ChainModule.vue': 'whiteboard wiring, not a plot surface',
  'modules/batchmovies/BatchMoviesPanel.vue': 'movie config, not a plot surface',
}

const RAW = import.meta.glob('/src/**/*.{vue,ts}', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const sources = Object.entries(RAW).map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

const PLOT_DATA_FETCH = /\/api\/(plot_data|plots\/(populations|umap)|gating\/channels)/

describe('task-refresh coverage', () => {
  it('the glob resolved', () => {
    expect(sources.length).toBeGreaterThan(100)
  })

  it('every surface that owns a data lifecycle calls useDataRefresh', () => {
    const missing = MUST_REFRESH.filter(p => {
      const s = sources.find(x => x.path === p)
      return !s || !s.text.includes('useDataRefresh')
    })
    expect(missing).toEqual([])
  })

  it('no NEW plot-data fetcher appears without either refreshing or a stated exemption', () => {
    // catches the gap in the other direction: a surface added later that fetches plot data and neither
    // refreshes nor says why not.
    const unaccounted = sources
      .filter(s => PLOT_DATA_FETCH.test(s.text))
      .filter(s => !s.path.endsWith('.test.ts'))
      .filter(s => !s.text.includes('useDataRefresh'))
      .filter(s => !(s.path in EXEMPT))
      // pure helpers/types name the routes in comments or builders but fetch nothing themselves
      .filter(s => /fetch\(/.test(s.text))
      .map(s => s.path)
    expect(unaccounted).toEqual([])
  })

  it('the exemption list stays honest — every entry still exists and still fetches', () => {
    const stale = Object.keys(EXEMPT).filter(p => {
      const s = sources.find(x => x.path === p)
      return !s || !PLOT_DATA_FETCH.test(s.text)
    })
    expect(stale).toEqual([])
  })

  it('is gated by the global setting in ONE place, so the toggle governs everything', () => {
    const prim = sources.find(s => s.path === 'composables/useDataRefresh.ts')!
    expect(prim.text).toContain('autoRefreshOnTask')
    // Nobody else may READ the setting to decide whether to refresh — that would fork the toggle, and a
    // fork is how "the toggle isn't honoured everywhere" becomes true. Comments naming it are fine and
    // in fact desirable (they point at the chokepoint), so strip them before looking.
    const code = (t: string) => t.replace(/\/\*[\s\S]*?\*\//g, ' ').replace(/(^|[^:])\/\/[^\n]*/g, '$1')
    const forks = sources
      .filter(s => s.path !== 'composables/useDataRefresh.ts' && !s.path.endsWith('.test.ts'))
      .filter(s => s.path !== 'stores/settings.ts' && !s.path.startsWith('modules/SettingsModule'))
      .filter(s => code(s.text).includes('autoRefreshOnTask'))
      .map(s => s.path)
    expect(forks).toEqual([])
  })
})
