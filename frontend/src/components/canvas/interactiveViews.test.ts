import { describe, it, expect } from 'vitest'
import { INTERACTIVE_VIEWS, boardViews, pageViews, railFor } from './interactiveViews'
import { CLUSTER_PANELS, clusterPanelRail } from '../../modules/cluster/clusterPanels'

const SFC = import.meta.glob('/src/components/canvas/LayoutCanvas.vue', {
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
})
