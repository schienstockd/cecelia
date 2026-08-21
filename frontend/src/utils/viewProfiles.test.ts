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
  const curatable = availablePaths(GROUPS)
  const visible = availablePaths(applyProfile(GROUPS, ['/gate', '/track', '/tasks']))

  it('names the pages a guide visits that the profile hides', () => {
    expect(hiddenGuideRoutes([{ route: '/gate' }, { route: '/clust-cells' }], visible, curatable))
      .toEqual(['/clust-cells'])
  })
  it('is empty when every page the guide visits is shown', () => {
    expect(hiddenGuideRoutes([{ route: '/gate' }, { route: '/track' }], visible, curatable))
      .toEqual([])
  })
  it('never flags a guide that is not tied to a page', () => {
    expect(hiddenGuideRoutes([{}], visible, curatable)).toEqual([])
  })
  it('flags nothing when no profile is resolved', () => {
    expect(hiddenGuideRoutes([{ route: '/clust-cells' }], [], curatable)).toEqual([])
  })
})

// The bug this pins: `/settings` lives in the sidebar FOOTER, not in `NAV_GROUPS`, so it is not a
// path a profile can list — and `availablePaths` therefore never contains it. Comparing a guide's
// routes against that list alone made the orientation tour read "1 missing · needs pages your view
// profile hides (/settings)" for everybody, on a first launch, with no profile chosen. Chrome routes
// (`/`, `/settings`, `/console`) are outside the catalogue by design; only a curatable page can be
// hidden.
describe('hiddenGuideRoutes and pages a profile cannot curate', () => {
  const curatable = availablePaths(GROUPS)

  it('never flags a route the menu does not offer', () => {
    const visible = availablePaths(applyProfile(GROUPS, ['/gate']))
    expect(hiddenGuideRoutes([{ route: '/settings' }], visible, curatable)).toEqual([])
  })

  it('does not flag a chrome route under the implicit All profile either', () => {
    const visible = availablePaths(applyProfile(GROUPS, null))
    expect(hiddenGuideRoutes([{ route: '/settings' }, { route: '/gate' }], visible, curatable))
      .toEqual([])
  })

  it('still flags a real page the profile drops', () => {
    const visible = availablePaths(applyProfile(GROUPS, ['/gate']))
    expect(hiddenGuideRoutes([{ route: '/settings' }, { route: '/track' }], visible, curatable))
      .toEqual(['/track'])
  })
})

// A step can name a page in TWO ways, and both matter to a profile: `route` (the page the step's
// control lives on) and a `nav:/movies` ANCHOR (the sidebar row for that page — the step's whole
// point is "click this menu item"). Reading `route` alone let two guides declare Movies nowhere:
// `record-a-movie` step 5 and `build-an-animation` step 8 both point at `nav:/movies` and neither
// guide has a `/movies` step, so a profile without Movies got "Ready" and then a centred "That
// control isn't on screen right now" card in place of the sidebar row.
describe('guideRoutes counts a nav: anchor as a page', () => {
  it('takes the page from the anchor as well as from route', () => {
    expect(guideRoutes([{ anchor: 'nav:/movies' }, { anchor: 'images.table', route: '/gate' }]))
      .toEqual(['/movies', '/gate'])
  })

  it('does not list the same page twice when a step declares it both ways', () => {
    expect(guideRoutes([{ anchor: 'nav:/gate' }, { anchor: 'gate.axes', route: '/gate' }]))
      .toEqual(['/gate'])
  })

  it('ignores a plain data-guide anchor', () => {
    expect(guideRoutes([{ anchor: 'gate.axes' }])).toEqual([])
  })

  it('so a hidden page is flagged even when only an anchor names it', () => {
    const curatable = availablePaths(GROUPS)
    const visible = availablePaths(applyProfile(GROUPS, ['/gate']))
    expect(hiddenGuideRoutes([{ anchor: 'nav:/track' }], visible, curatable)).toEqual(['/track'])
  })
})
