import { describe, it, expect } from 'vitest'
import { routePathFromHash, anchorSelector, rankAnchorCandidates, NAV_PREFIX } from './guideAnchor'

// The pure halves of guide anchoring. `routePathFromHash` is here because of a real bug: the guide
// runtime compares a step's declared `route` against the hash, and it must re-READ that value rather
// than only listen for `hashchange` — vue-router navigates a hash history with `history.pushState`,
// which fires no `hashchange`, so a listener-only version sat at the boot path (`/`) forever and every
// routed step reported "you are on another page". The listening half is browser behaviour a unit test
// can't reach; the parsing half is this.

describe('routePathFromHash', () => {
  it('strips the leading # to give a router path', () => {
    expect(routePathFromHash('#/manage-images')).toBe('/manage-images')
  })

  it('tolerates a hash with no #', () => {
    expect(routePathFromHash('/segment')).toBe('/segment')
  })

  it('drops a query string, which no step route carries', () => {
    expect(routePathFromHash('#/segment?foo=1')).toBe('/segment')
  })

  it('falls back to / rather than an empty string no step could match', () => {
    expect(routePathFromHash('')).toBe('/')
    expect(routePathFromHash('#')).toBe('/')
  })

  it('keeps a nested path intact', () => {
    expect(routePathFromHash('#/custom/my-category')).toBe('/custom/my-category')
  })
})

describe('anchorSelector', () => {
  it('targets a data-guide attribute for a plain id', () => {
    expect(anchorSelector('task.run')).toBe('[data-guide="task.run"]')
  })

  it('targets a nav item by its hash href, so the sidebar needs no attributes', () => {
    expect(anchorSelector(`${NAV_PREFIX}/clust-cells`)).toBe('a[href="#/clust-cells"]')
  })

  it('escapes a quote rather than producing an invalid selector', () => {
    expect(anchorSelector('a"b')).toBe('[data-guide="a\\"b"]')
  })
})

// Which of several matching elements to point at. The case that forced this: two floating gating plots,
// each with its own axis controls under the same anchor id — the resolver took the first in DOM order,
// so it ringed plot 1 while the user worked in plot 2, and the ring (which sits above the app) drew
// across the panel in front.
describe('rankAnchorCandidates', () => {
  const c = (reachable: boolean, inActive: boolean, occluded: boolean) => ({ reachable, inActive, occluded })

  it('returns -1 for nothing to choose from', () => {
    expect(rankAnchorCandidates([])).toBe(-1)
  })

  it('prefers a visible candidate over a hidden one', () => {
    expect(rankAnchorCandidates([c(false, true, false), c(true, false, false)])).toBe(1)
  })

  it('prefers the one in the ACTIVE panel — the plot the user is working in', () => {
    expect(rankAnchorCandidates([c(true, false, false), c(true, true, false)])).toBe(1)
  })

  it('prefers an unoccluded candidate when neither is active', () => {
    expect(rankAnchorCandidates([c(true, false, true), c(true, false, false)])).toBe(1)
  })

  it('keeps the active one even when something covers its midpoint', () => {
    // being active is the stronger signal: a tooltip over the control does not make it the wrong one
    expect(rankAnchorCandidates([c(true, false, false), c(true, true, true)])).toBe(1)
  })

  it('keeps DOM order on a tie, so a row list still points at the first row', () => {
    expect(rankAnchorCandidates([c(true, false, false), c(true, false, false)])).toBe(0)
  })
})
