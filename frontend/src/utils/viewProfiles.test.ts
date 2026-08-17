import { describe, it, expect } from 'vitest'
import {
  applyProfile, availablePaths, guideRoutes, hiddenGuideRoutes, unknownPaths,
} from './viewProfiles'

const GROUPS = [
  { heading: 'Data', items: [{ to: '/manage-images' }, { to: '/metadata' }, { to: '/segment' }] },
  { heading: 'Populations', items: [{ to: '/gate' }, { to: '/track' }, { to: '/clust-cells' }] },
  { heading: 'Pipeline', items: [{ to: '/tasks' }] },
]

describe('availablePaths', () => {
  it('lists every path in sidebar order', () => {
    expect(availablePaths(GROUPS)).toEqual([
      '/manage-images', '/metadata', '/segment', '/gate', '/track', '/clust-cells', '/tasks',
    ])
  })
})

describe('applyProfile', () => {
  it('keeps only the listed items and drops emptied groups', () => {
    const out = applyProfile(GROUPS, ['/gate', '/track', '/tasks'])
    expect(out.map(g => g.heading)).toEqual(['Populations', 'Pipeline'])
    expect(out[0].items.map(i => i.to)).toEqual(['/gate', '/track'])
  })

  it('orders items within a group by the profile, not the sidebar', () => {
    const out = applyProfile(GROUPS, ['/track', '/gate'])
    expect(out[0].items.map(i => i.to)).toEqual(['/track', '/gate'])
  })

  it('keeps the built-in group order regardless of the profile order', () => {
    // "All" is about hiding clutter, not restructuring the app: a profile listing a Pipeline page
    // first must not float Pipeline above Data.
    const out = applyProfile(GROUPS, ['/tasks', '/gate', '/segment'])
    expect(out.map(g => g.heading)).toEqual(['Data', 'Populations', 'Pipeline'])
  })

  it('ignores paths that do not exist', () => {
    const out = applyProfile(GROUPS, ['/gate', '/nope'])
    expect(out.map(g => g.heading)).toEqual(['Populations'])
    expect(out[0].items.map(i => i.to)).toEqual(['/gate'])
  })

  it('passes everything through for the implicit All profile', () => {
    expect(applyProfile(GROUPS, null)).toEqual(GROUPS)
    expect(applyProfile(GROUPS, [])).toEqual(GROUPS)
  })

  it('does not mutate the input groups', () => {
    applyProfile(GROUPS, ['/track', '/gate'])
    expect(GROUPS[1].items.map(i => i.to)).toEqual(['/gate', '/track', '/clust-cells'])
  })
})

describe('unknownPaths', () => {
  it('reports listed paths the app no longer has', () => {
    expect(unknownPaths(['/gate', '/gone', '/also-gone'], availablePaths(GROUPS)))
      .toEqual(['/gone', '/also-gone'])
  })
  it('is empty for All and for a fully valid profile', () => {
    expect(unknownPaths(null, availablePaths(GROUPS))).toEqual([])
    expect(unknownPaths(['/gate'], availablePaths(GROUPS))).toEqual([])
  })
})

describe('guideRoutes', () => {
  it('collects distinct step routes in order', () => {
    expect(guideRoutes([{ route: '/gate' }, {}, { route: '/gate' }, { route: '/track' }]))
      .toEqual(['/gate', '/track'])
  })
  it('is empty for a guide tied to no page', () => {
    expect(guideRoutes([{}, {}])).toEqual([])
  })
})

describe('hiddenGuideRoutes', () => {
  const visible = availablePaths(applyProfile(GROUPS, ['/gate', '/track', '/tasks']))

  it('names the pages a guide visits that the profile hides', () => {
    expect(hiddenGuideRoutes([{ route: '/gate' }, { route: '/clust-cells' }], visible))
      .toEqual(['/clust-cells'])
  })
  it('is empty when every page the guide visits is shown', () => {
    expect(hiddenGuideRoutes([{ route: '/gate' }, { route: '/track' }], visible)).toEqual([])
  })
  it('never flags a guide that is not tied to a page', () => {
    expect(hiddenGuideRoutes([{}], visible)).toEqual([])
  })
  it('flags nothing when no profile is resolved', () => {
    expect(hiddenGuideRoutes([{ route: '/clust-cells' }], [])).toEqual([])
  })
})
