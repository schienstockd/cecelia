import { describe, it, expect } from 'vitest'
import { renderUserDocs, sidebarSection, recipesSection } from './userDocs'
import { NAV_GROUPS } from '../lib/navGroups'

// The hand-written prose, read as text — the same `?raw` glob idiom as guides.test.ts.
const PARTS = import.meta.glob('../../../user-docs/parts/*.md', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>
const part = (name: string) => PARTS[`../../../user-docs/parts/${name}.md`] ?? ''

describe('user-docs/README.md', () => {
  it('names every sidebar page once', () => {
    const s = sidebarSection()
    for (const g of NAV_GROUPS) for (const i of g.items)
      expect(s.split('\n').filter(l => l.startsWith(`| ${i.label} |`)), i.label).toHaveLength(1)
  })

  it('lists every written recipe', () => {
    expect(recipesSection()).toMatch(/Intravital timelapse/)
    expect(recipesSection()).toMatch(/Multiplex tissue/)
  })

  // The committed README. Stale ⇒ `npx vitest run -u src/utils/userDocs.test.ts` and commit it. Edit the
  // prose in user-docs/parts/, the tables through navGroups.ts / guides/mapData.ts / guides/recipes.ts.
  it('matches the generated file', async () => {
    expect(part('top').length).toBeGreaterThan(0)
    expect(part('bottom').length).toBeGreaterThan(0)
    await expect(renderUserDocs(part('top'), part('bottom')))
      .toMatchFileSnapshot('../../../user-docs/README.md')
  })
})
