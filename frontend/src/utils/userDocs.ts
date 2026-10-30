// user-docs/README.md, generated: the hand-written prose (user-docs/parts/top.md, bottom.md) around
// two sections built from the same data the in-app map uses — the sidebar (navGroups.ts + the map's
// guides/mapData.ts) and the recipes (guides/recipes.ts). So the README and the map cannot disagree
// about a page, a pair or a guide. Written by `userDocs.test.ts` (`npx vitest run -u` refreshes it);
// a stale README fails that test in CI. WORKFLOW_RECIPES_PLAN → *The map*.
import { NAV_GROUPS, navLabelFor } from '../lib/navGroups'
import { MAP, GROUP_BLURB } from '../lib/guides/mapData'
import { guideById, RECIPES, isWanted, type WrittenRecipe } from '../lib/guides'

const cell = (s: string) => s.replace(/\|/g, '\\|')

export function sidebarSection(): string {
  const out = [
    '## The sidebar',
    '',
    'The groups follow the analysis: **Populations** pages *define* who the cells are; **Explore** pages',
    '*summarise* those populations across images. Every page except Tasks needs an open project. Each',
    'page that defines a population names the page that summarises it.',
  ]
  for (const g of NAV_GROUPS) {
    out.push('', `### ${g.heading}: ${(GROUP_BLURB[g.heading] ?? '').toLowerCase()}`, '',
      '| Page | What it\'s for | Summarised on | Guide | Claude |', '|---|---|---|---|---|')
    for (const i of g.items) {
      const e = MAP[i.to]
      const guide = e?.guide ? guideById(e.guide)?.title ?? '' : ''
      const pair = e?.pairedWith ? navLabelFor(NAV_GROUPS, e.pairedWith) : ''
      out.push(`| ${cell(i.label)} | ${cell(e?.what ?? i.tip)} | ${cell(pair)} | ${cell(guide)} | ${cell(e?.claude ?? '')} |`)
    }
  }
  return out.join('\n')
}

export function recipesSection(): string {
  const written = RECIPES.filter(r => !isWanted(r)) as WrittenRecipe[]
  const out = [
    '## Recipes',
    '',
    'Pick the one that matches your data at the top of **Guides**: its pages light up on the map and its',
    'steps are listed in order, each with why it is there. Steps marked optional depend on your images.',
    'Every recipe ends the same way: once the settings work on a few images, run them over the whole set',
    'as a chain.',
    '',
    '| Recipe | When this is you | Steps |',
    '|---|---|---|',
  ]
  for (const r of written) {
    const n = r.steps.length
    const opt = r.steps.filter(s => s.optional).length
    out.push(`| **${cell(r.title)}** | ${cell(r.whenThisIsYou)} | ${n}${opt ? ` (${opt} optional)` : ''} |`)
  }
  return out.join('\n')
}

/** The whole README from its two hand-written parts. */
export function renderUserDocs(top: string, bottom: string): string {
  return [top.trimEnd(), '', recipesSection(), '', sidebarSection(), '', bottom.trimStart()].join('\n')
}
