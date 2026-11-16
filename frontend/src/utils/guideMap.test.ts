import { describe, it, expect } from 'vitest'
import { NAV_GROUPS } from '../lib/navGroups'
import { MAP, GROUP_BLURB, CLAUDE_MAKES } from '../lib/guides/mapData'
import { guideById, RECIPES, isWanted, type WrittenRecipe } from '../lib/guides'
import { recipeRoutes, defaultOpenGroup, stepRoute } from './guideMap'
import { pageTier } from './maturity'

const ROUTES = NAV_GROUPS.flatMap(g => g.items.map(i => i.to))
const written = RECIPES.filter(r => !isWanted(r)) as WrittenRecipe[]

// The map's CI checks (WORKFLOW_RECIPES_PLAN → *The map*): a page with no entry, or an entry
// pointing at a page or guide that doesn't exist, fails here.
describe('the map covers the sidebar, exactly', () => {
  it('every sidebar page has an entry', () => {
    expect(ROUTES.filter(r => !MAP[r])).toEqual([])
  })
  it('every entry is a sidebar page', () => {
    expect(Object.keys(MAP).filter(r => !ROUTES.includes(r))).toEqual([])
  })
  it('every guide and pair exists', () => {
    for (const [route, e] of Object.entries(MAP)) {
      if (e.guide) expect(guideById(e.guide), `${route} → ${e.guide}`).toBeDefined()
      if (e.pairedWith) expect(ROUTES, `${route} ↔ ${e.pairedWith}`).toContain(e.pairedWith)
    }
  })
  it('every sidebar group has its one line', () => {
    for (const g of NAV_GROUPS) expect(GROUP_BLURB[g.heading], g.heading).toBeTruthy()
  })
  it('keeps the what-for to one short line', () => {
    for (const [route, e] of Object.entries(MAP)) {
      if (e.claude) expect(e.claude.length, `${route} claude`).toBeLessThanOrEqual(70)
      expect(e.what.length, route).toBeLessThanOrEqual(70)
      expect(e.what.endsWith('.'), route).toBe(false)
    }
  })
})

// The one list of MCP tools (mcp/cecelia_mcp/tools.json), read as text — the same `?raw` glob idiom as
// userDocs.test.ts. The observer registers exactly its `observer` names (mcp/tests/test_server.py).
const TOOLS_JSON = JSON.parse(Object.values(import.meta.glob('../../../mcp/cecelia_mcp/tools.json',
  { query: '?raw', import: 'default', eager: true }) as Record<string, string>)[0] ?? '{}')

describe('the ✦ follows what Claude can make', () => {
  const makers = Object.entries(TOOLS_JSON.observer as Record<string, string>)
    .filter(([, kind]) => kind === 'add').map(([name]) => name).sort()
  it('reads the observer’s create tools', () => {
    expect(makers).toContain('create_chain')
  })
  it('classifies every create tool, and only those', () => {
    expect(Object.keys(CLAUDE_MAKES).sort()).toEqual(makers)
  })
  it('badges exactly the pages a create tool lands on', () => {
    const landed = new Set(Object.values(CLAUDE_MAKES).filter((r): r is string => !!r))
    const badged = new Set(Object.entries(MAP).filter(([, e]) => e.claude).map(([r]) => r))
    expect(badged).toEqual(landed)
  })
})

describe('recipes light real pages', () => {
  it('every step of every recipe lands on a sidebar page', () => {
    const lost: string[] = []
    for (const r of written) for (const s of r.steps) if (!stepRoute(s)) lost.push(`${r.id}: ${JSON.stringify(s).slice(0, 60)}`)
    expect(lost).toEqual([])
  })
  it('the intravital recipe walks drift → segment → gate → track → behaviour, in order', () => {
    const r = written.find(x => x.id === 'intravital-timelapse')!
    const order = recipeRoutes(r)
    const at = (p: string) => order.indexOf(p)
    expect(at('/cleanup')).toBeLessThan(at('/segment'))
    expect(at('/segment')).toBeLessThan(at('/gate'))
    expect(at('/gate')).toBeLessThan(at('/track'))
    expect(at('/track')).toBeLessThan(at('/behaviour'))
  })
})

describe('every sidebar page', () => {
  it('carries a maturity tier (exempt counts)', () => {
    expect(ROUTES.filter(r => !pageTier(r))).toEqual([])
    expect(pageTier('/tasks')).toBe('exempt')
  })
  // the README prints each line as it stands, with no "Claude:" prefix, so each must name the mode itself
  it('says which way in makes what Claude makes there', () => {
    for (const [route, e] of Object.entries(MAP)) if (e.claude) expect(e.claude, route).toMatch(/Claude Code|Kiwi/)
    expect(MAP['/chain']?.claude).toBeTruthy()
  })
})

describe('defaultOpenGroup', () => {
  it('opens the guides for the page you are on, else Start', () => {
    expect(defaultOpenGroup('/segment')).toBe('Data')
    expect(defaultOpenGroup('/gate')).toBe('Populations')
    expect(defaultOpenGroup('/nowhere')).toBe('Start')
  })
})


describe('chipDot — one dot per tier, read through the legend', () => {
  it('gives every tiered page a dot, exempt none, and the legend lists each dot in use', async () => {
    const { chipDot, TIER_LEGEND } = await import('./guideMap')
    expect(chipDot('experimental')).toBe('cc-tier-experimental')
    expect(chipDot('beta')).toBe('cc-tier-beta')
    expect(chipDot('tested')).toBe('cc-tier-tested')
    expect(chipDot('comingSoon')).toBe('cc-tier-comingSoon')
    for (const t of ['exempt', 'stable', null, undefined] as const) expect(chipDot(t)).toBe('')
    // every dot a page can carry is explained in the legend
    const legend = new Set(TIER_LEGEND.map(l => l.dot))
    for (const t of ['experimental', 'beta', 'tested', 'comingSoon'] as const) expect(legend.has(chipDot(t))).toBe(true)
  })
})
