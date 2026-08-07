import { describe, it, expect } from 'vitest'
import { INTERACTIVE_VIEWS, boardViews, pageViews, inBoardGroup } from './interactiveViews'

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

  it('groups the views the hosts special-case', () => {
    expect(inBoardGroup('umap', 'clustering')).toBe(true)     // board treats it as a cluster slot
    expect(inBoardGroup('filmstrip', 'image')).toBe(true)
    expect(inBoardGroup('flowModel', 'interactive')).toBe(true)
  })

  it('the flow model is offered on its module page AND the board', () => {
    expect(pageViews('opticalFlowPage').map(v => v.key)).toEqual(['flowModel'])
    expect(boardViews('interactive').map(v => v.key)).toContain('flowModel')
  })

  it('the cluster page offers only cluster-page views', () => {
    expect(pageViews('clusterPage').map(v => v.key)).toEqual(['umap'])
  })

  // The recurrence guard. `flowModel` shipped with `analysisBoard: true` and never showed up, because
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
