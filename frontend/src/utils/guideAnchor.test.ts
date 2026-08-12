import { describe, it, expect } from 'vitest'
import { routePathFromHash, anchorSelector, NAV_PREFIX } from './guideAnchor'

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
