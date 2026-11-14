// Joins between the sidebar (`navGroups.ts`), the page map (`guides/mapData.ts`: what for, pairs,
// guide) and the recipes: which page a guide or recipe step lands on, the tier dot and legend, and
// which guide group opens first. Pure and tested. The map is no longer drawn in the app (the sidebar
// is the map); `user-docs/README.md` renders the same data (`utils/userDocs.ts`).
import { NAV_GROUPS } from '../lib/navGroups'
import { availablePaths, guideRoutes } from './viewProfiles'
import { MAP } from '../lib/guides/mapData'
import { GUIDES, guideById } from '../lib/guides'
import { isGuideStep, type WrittenRecipe, type RecipeStep } from '../lib/guides/recipes'
import type { GuideDef, Tier } from '../lib/guides/types'
import { TIER_LABEL, TIER_DOT, chipDot } from '../lib/guides/tiers'

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

/** The page button's marker — `chipDot` lives beside the tier words (`lib/guides/tiers.ts`) so the
 *  badge can use it without the guide catalogue; re-exported here beside the legend. */
export { chipDot }

/** The tier legend (MaturityDialog): each tier in use, with its dot. Stable is reserved and never listed. */
export const TIER_LEGEND: { tier: Tier; dot: string; label: string }[] =
  (['experimental', 'beta', 'tested', 'comingSoon'] as const).map(t => ({ tier: t, dot: TIER_DOT[t]!, label: TIER_LABEL[t] }))

/** The marker's accessible name. */
export const tierLabel = (t: Tier | null | undefined): string => (t ? TIER_LABEL[t] : '')

/** Which guide group opens first: the one whose guides teach the page you're on, else Start. */
export function defaultOpenGroup(currentPath: string): string {
  const g = GUIDES.find(d => guideRoute(d) === currentPath)
  return g?.group ?? 'Start'
}
