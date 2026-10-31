import { describe, it, expect } from 'vitest'
import { NAV_GROUPS } from '../lib/navGroups'
import { MAP, GROUP_BLURB, CLAUDE_MAKES } from '../lib/guides/mapData'
import { guideById, RECIPES, isWanted, type WrittenRecipe } from '../lib/guides'
import { buildMap, recipeRoutes, chipTip, defaultOpenGroup, stepRoute, mapCaption } from './guideMap'
import { availablePaths, applyProfile } from './viewProfiles'

const ROUTES = NAV_GROUPS.flatMap(g => g.items.map(i => i.to))
const visibleWith = (items: string[] | null) => availablePaths(applyProfile(NAV_GROUPS, items))
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

// The observer's source, read as text — the same `?raw` glob idiom as userDocs.test.ts. Its tools live
// in server.py and in the gating toolset it registers (gating_views.py, shared with the autonomous server).
const SERVER = Object.values(import.meta.glob(['../../../mcp/cecelia_mcp/server.py', '../../../mcp/cecelia_mcp/gating_views.py'],
  { query: '?raw', import: 'default', eager: true }) as Record<string, string>).join('\n')

describe('the ✦ follows what Claude can make', () => {
  const makers = [...SERVER.matchAll(/^ *(?:async )?def ((?:create|add|append)_\w+)\(project_uid/gm)].map(m => m[1]).sort()
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

describe('buildMap', () => {
  // No default path: where you start depends on the data and the question, so with no recipe
  // nothing is lit and nothing is dimmed.
  it('lights and dims nothing when no recipe is picked', () => {
    const chips = buildMap(null, visibleWith(null)).flatMap(g => g.chips)
    expect(chips.some(c => c.lit || c.dim)).toBe(false)
  })
  it('lights exactly a recipe’s pages when one is picked', () => {
    const r = written.find(x => x.id === 'multiplex-tissue')!
    const lit = new Set(buildMap(r, visibleWith(null)).flatMap(g => g.chips).filter(c => c.lit).map(c => c.route))
    expect(lit).toEqual(new Set(recipeRoutes(r)))
    expect(lit.has('/clust-cells')).toBe(true)
    expect(lit.has('/gate')).toBe(false)
    const gate = buildMap(r, visibleWith(null)).flatMap(g => g.chips).find(c => c.route === '/gate')!
    expect(gate.dim).toBe(true)
  })
  // An empty layout is "All", as everywhere else (applyProfile) — not "everything hidden".
  it('treats an empty layout as all pages', () => {
    expect(buildMap(null, visibleWith([])).flatMap(g => g.chips).some(c => c.hidden)).toBe(false)
  })
  it('dims pages the sidebar layout hides, and says so on hover', () => {
    const chips = buildMap(null, visibleWith(['/segment'])).flatMap(g => g.chips)
    expect(chips.find(c => c.route === '/segment')!.hidden).toBe(false)
    const gate = chips.find(c => c.route === '/gate')!
    expect(gate.hidden).toBe(true)
    expect(chipTip(gate)).toMatch(/hidden by your sidebar layout/)
  })
  it('says what bold means, or to pick a recipe', () => {
    expect(mapCaption(null)).toMatch(/Pick a recipe/)
    expect(mapCaption(written[0])).toContain(written[0].title)
  })
  it('says what Claude makes on a page only with the Claude toggle on', () => {
    const chain = buildMap(null, visibleWith(null)).flatMap(g => g.chips).find(c => c.route === '/chain')!
    expect(chain.claude).toBeTruthy()
    expect(chipTip(chain)).not.toContain(chain.claude!)
    expect(chipTip(chain, true)).toContain(chain.claude!)
    // the line names the mode itself — there is no "Claude:" prefix any more — so every one must
    for (const c of buildMap(null, visibleWith(null)).flatMap(g => g.chips).filter(c => c.claude)) {
      expect(c.claude, c.route).toMatch(/Claude Code|Kiwi/)
    }
  })
  it('names the pair and the guide on a chip', () => {
    const gate = buildMap(null, visibleWith(null)).flatMap(g => g.chips).find(c => c.route === '/gate')!
    expect(gate.pairedLabel).toBe('Phenotype')
    expect(gate.guideTitle).toBeTruthy()
    expect(chipTip(gate)).toMatch(/summarised on Phenotype/)
  })
})

describe('defaultOpenGroup', () => {
  it('opens the guides for the page you are on, else Start', () => {
    expect(defaultOpenGroup('/segment')).toBe('Data')
    expect(defaultOpenGroup('/gate')).toBe('Populations')
    expect(defaultOpenGroup('/nowhere')).toBe('Start')
  })
})
