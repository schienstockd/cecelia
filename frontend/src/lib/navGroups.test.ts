import { describe, it, expect } from 'vitest'
import { NAV_GROUPS, allNavGroups, customNavGroup, navLabelFor } from './navGroups'

// The nav catalogue and the router's route table are two lists that must agree, and nothing but this
// enforced it: a page whose path is mistyped here renders a dead sidebar row, and a page added to the
// router but not here is unreachable from the menu. `main.ts` can't be imported (it boots the app), so
// the route table is read as source — the same trick uiCopy.test.ts / cssScenarios.test.ts use.
const MAIN = import.meta.glob('/src/main.ts', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>

function routerSource(): string {
  const src = Object.values(MAIN)[0]
  if (!src) throw new Error('could not read src/main.ts — did the glob path change?')
  return src
}

/** Every `path:` in the router's `routes` array, in declaration order. */
function routerPaths(): string[] {
  return [...routerSource().matchAll(/\{\s*path:\s*'([^']+)'/g)].map(m => m[1])
}

// Routes that deliberately have NO sidebar entry, each for a stated reason. Anything else in the
// router but not in the catalogue is a page nobody can reach from the menu — which is the bug this
// list exists to keep visible rather than tolerate silently.
const NOT_IN_MENU: Record<string, string> = {
  '/':                  'the neutral welcome page — the shell is the landmark, not a nav row',
  '/settings':          'lives in the sidebar FOOTER, and must never be hideable (a profile could strand the user)',
  '/console':           'bare route, opened in its own window',
  '/tasks-window':      'bare route, the Task Manager opened in its own window',
  '/viewer-window':     'bare route, the volume viewer opened in its own window from the viewer panel',
  '/setup':             'bare route, the first-launch wizard',
  '/custom/:category':  'parameterised — the Custom group is generated per category at runtime',
  '/optical-flow':      'legacy redirect to /model-training — pinned tabs and old guide links land here, then bounce',
}

describe('the nav catalogue', () => {
  it('reads the router source', () => {
    // If this fails the two assertions below are vacuous, so it is checked on its own.
    expect(routerPaths().length).toBeGreaterThan(15)
  })

  it('has no page the router cannot route', () => {
    const routed = new Set(routerPaths())
    const orphans = NAV_GROUPS.flatMap(g => g.items.map(i => i.to)).filter(p => !routed.has(p))
    expect(orphans).toEqual([])
  })

  it('offers every routed page, or says why not', () => {
    const inMenu = new Set(NAV_GROUPS.flatMap(g => g.items.map(i => i.to)))
    const missing = routerPaths().filter(p => !inMenu.has(p) && !(p in NOT_IN_MENU))
    expect(missing).toEqual([])
  })

  it('never lists Settings — a view profile must not be able to hide it', () => {
    expect(NAV_GROUPS.flatMap(g => g.items.map(i => i.to))).not.toContain('/settings')
  })

  it('gives every item a label, icon and tip', () => {
    // The sidebar renders all three; a missing tip also trips the UI-copy ratchet, but only once the
    // row is rendered — this catches it at the source.
    const bad = NAV_GROUPS.flatMap(g => g.items)
      .filter(i => !i.label || !i.icon || !i.tip)
      .map(i => i.to)
    expect(bad).toEqual([])
  })

  it('has unique paths across groups', () => {
    const all = NAV_GROUPS.flatMap(g => g.items.map(i => i.to))
    expect(all.length).toBe(new Set(all).size)
  })
})

describe('customNavGroup', () => {
  const cat = (name: string, builtin: boolean) => ({ name, builtin, funNames: [`${name}.doThing`] })

  it('offers only categories with no built-in page', () => {
    const g = customNavGroup([cat('myThing', false), cat('behaviour', true)])
    expect(g?.items.map(i => i.to)).toEqual(['/custom/myThing'])
  })

  it('is null when there is nothing to show, so the group disappears', () => {
    expect(customNavGroup([])).toBeNull()
    expect(customNavGroup([cat('behaviour', true)])).toBeNull()
  })

  it('prettifies the category name for the label', () => {
    expect(customNavGroup([cat('my_thing', false)])?.items[0].label).toBe('My thing')
    expect(customNavGroup([cat('myThing', false)])?.items[0].label).toBe('My Thing')
  })

  it('appends the custom group AFTER the built-in ones', () => {
    const groups = allNavGroups([cat('myThing', false)])
    expect(groups.length).toBe(NAV_GROUPS.length + 1)
    expect(groups[groups.length - 1].heading).toBe('Custom')
  })

  it('leaves the built-in groups alone when there are no custom modules', () => {
    expect(allNavGroups([])).toEqual(NAV_GROUPS)
  })
})

describe('navLabelFor', () => {
  it('names a page for a message that must not show a raw path', () => {
    expect(navLabelFor(NAV_GROUPS, '/clust-cells')).toBe('Cluster cells')
  })
  it('falls back to the path it was given', () => {
    expect(navLabelFor(NAV_GROUPS, '/nope')).toBe('/nope')
  })
})
