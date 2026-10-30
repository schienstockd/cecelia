// The map at the front of the Guides dialog, as data — pure, so it's tested here and `GuideMap.vue`
// only renders it. Joins three sources and restates none: the sidebar (`navGroups.ts`: pages, labels,
// icons, groups), the map (`guides/mapData.ts`: what for, pairs, guide) and the recipes
// (which pages a picked recipe lights). WORKFLOW_RECIPES_PLAN → *The map*.
import { NAV_GROUPS, navLabelFor } from '../lib/navGroups'
import { availablePaths, guideRoutes } from './viewProfiles'
import { MAP, GROUP_BLURB } from '../lib/guides/mapData'
import { GUIDES, guideById } from '../lib/guides'
import { isGuideStep, type WrittenRecipe, type RecipeStep } from '../lib/guides/recipes'
import type { GuideDef } from '../lib/guides/types'

export interface MapChip {
  route: string
  label: string
  icon: string
  what: string
  guideId: string | null
  guideTitle: string | null
  pairedLabel: string | null   // the page this one is paired with, by its sidebar name
  lit: boolean                 // on the picked recipe's path
  dim: boolean                 // a recipe is picked and this page isn't on it
  hidden: boolean              // the active sidebar layout hides it — dimmed, still reachable
  claude: string | null        // what Claude makes here (mapData.ts → claude)
}

export interface MapGroup {
  heading: string
  blurb: string
  chips: MapChip[]
}

const SIDEBAR = new Set(availablePaths(NAV_GROUPS))

/** The sidebar page a guide teaches: the map's own pairing first, else the first sidebar route its
 *  steps go to. Null for a guide about the app's chrome (the tour, profiles). */
export function guideRoute(g: GuideDef): string | null {
  const own = Object.entries(MAP).find(([, e]) => e.guide === g.id)
  if (own) return own[0]
  return guideRoutes(g.steps).find(p => SIDEBAR.has(p)) ?? null
}

/** The sidebar page a recipe step happens on. */
export function stepRoute(s: RecipeStep): string | null {
  if (!isGuideStep(s)) return s.page
  const g = guideById(s.guide)
  return g ? guideRoute(g) : null
}

/** The pages a recipe walks through, in order of first visit. */
export function recipeRoutes(r: WrittenRecipe): string[] {
  const out: string[] = []
  for (const s of r.steps) {
    const p = stepRoute(s)
    if (p && !out.includes(p)) out.push(p)
  }
  return out
}

/** The map's groups and chips. `visible` = the pages the active sidebar layout shows —
 *  `availablePaths(applyProfile(NAV_GROUPS, activeItems))`, the same set the guide prereqs use. */
export function buildMap(recipe: WrittenRecipe | null, visible: string[]): MapGroup[] {
  const lit = recipe ? new Set(recipeRoutes(recipe)) : null
  return NAV_GROUPS.map(g => ({
    heading: g.heading,
    blurb: GROUP_BLURB[g.heading] ?? '',
    chips: g.items.map(i => {
      const e = MAP[i.to]
      const guide = e?.guide ? guideById(e.guide) : undefined
      return {
        route: i.to,
        label: i.label,
        icon: i.icon,
        what: e?.what ?? i.tip,
        guideId: guide?.id ?? null,
        guideTitle: guide?.title ?? null,
        pairedLabel: e?.pairedWith ? navLabelFor(NAV_GROUPS, e.pairedWith) : null,
        lit: !!lit?.has(i.to),
        dim: !!lit && !lit.has(i.to),
        hidden: !visible.includes(i.to),
        claude: e?.claude ?? null,
      }
    }),
  }))
}

/** One hover line for a chip: what it's for, its pair, its guide — and, with the Claude toggle on,
 *  what Claude makes there. */
export function chipTip(c: MapChip, showClaude = false): string {
  const parts = [c.what]
  if (c.pairedLabel) parts.push(`summarised on ${c.pairedLabel}`)
  if (c.hidden) parts.push('hidden by your sidebar layout')
  if (showClaude && c.claude) parts.push(`Claude: ${c.claude}`)
  return parts.join(' · ')
}

/** Which guide group opens first: the one whose guides teach the page you're on, else Start. */
export function defaultOpenGroup(currentPath: string): string {
  const g = GUIDES.find(d => guideRoute(d) === currentPath)
  return g?.group ?? 'Start'
}

/** The line under the map: what bold means, or that a recipe is how pages light up. No recipe = no
 *  default path — where you start depends on your data and your question. */
export function mapCaption(recipe: WrittenRecipe | null): string {
  return recipe ? `Bold: the pages ${recipe.title} uses` : 'Pick a recipe to light its pages'
}
