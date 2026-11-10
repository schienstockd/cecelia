// Which recipe a set follows, and what a page's next step is under it (docs/todo/GUI_CLARITY_PLAN.md,
// Decisions 13 and 16). Pure, so `NextStep.vue` and `SetRecipePicker.vue` only render it.
//
// The recipe is stored per set (`set.recipe`, written by `POST /api/sets/recipe`). Without one, the
// set's axes SUGGEST a recipe, shown as a suggestion the user confirms, never applied silently. A
// recipe lights pages and names the next one; it never hides a page and never runs anything.
import { RECIPES, isWanted, recipeById, type WrittenRecipe } from '../lib/guides/recipes'
import { NAV_GROUPS, navLabelFor } from '../lib/navGroups'
import { stepRoute, recipeRoutes } from './guideMap'

/** The shape of a set this needs — a structural subset of `CciaSet`, so tests need no store. */
export interface RecipeSetLike {
  recipe?: string | null
  images: { sizeT?: number | null; sizeC?: number | null }[]
}

export type RecipeSource = 'set' | 'suggested'

export interface SetRecipe {
  recipe: WrittenRecipe
  source: RecipeSource
  reason: string | null      // why it was suggested; null when the user picked it
}

/** The written recipes, in catalogue order — what the picker offers. */
export const WRITTEN_RECIPES: WrittenRecipe[] = RECIPES.filter((r): r is WrittenRecipe => !isWanted(r))

/** A written recipe by id; null for an unknown id or a wanted (unwritten) one. */
export function writtenRecipe(id: string | null | undefined): WrittenRecipe | null {
  if (!id) return null
  const r = recipeById(id)
  return r && !isWanted(r) ? r : null
}

// Channel counts that point clearly at one still-tissue recipe. Between them, the set could be either,
// so nothing is suggested and the user picks.
const FEW_MARKERS_MAX = 6
const MULTIPLEX_MIN = 10

/** A recipe suggested from the set's axes, or null when the axes don't point at one.
 *  A time axis on most images → the intravital timelapse recipe; otherwise the channel count picks
 *  few-marker vs multiplex tissue. */
export function suggestRecipe(set: RecipeSetLike): SetRecipe | null {
  const imgs = set.images
  if (imgs.length === 0) return null
  const timed = imgs.filter(i => (i.sizeT ?? 1) > 1).length
  const pick = (id: string, reason: string): SetRecipe | null => {
    const recipe = writtenRecipe(id)
    return recipe ? { recipe, source: 'suggested', reason } : null
  }
  if (timed * 2 > imgs.length) return pick('intravital-timelapse', 'has a time axis')
  const cs = imgs.map(i => i.sizeC ?? 0).filter(c => c > 0)
  if (cs.length === 0) return null
  const c = Math.max(...cs)
  if (c >= MULTIPLEX_MIN) return pick('multiplex-tissue', `${c} channels`)
  if (c <= FEW_MARKERS_MAX) return pick('few-marker-tissue', `${c} channels, no time axis`)
  return null
}

/** The set's own recipe if it has a known one, else the suggestion, else null. */
export function resolveSetRecipe(set: RecipeSetLike | null | undefined): SetRecipe | null {
  if (!set) return null
  const own = writtenRecipe(set.recipe)
  if (own) return { recipe: own, source: 'set', reason: null }
  return suggestRecipe(set)
}

export interface NextStep {
  route: string
  label: string       // the page's sidebar name
  why: string         // the recipe's one-line reason for that step
}

/** The step after the current page in the recipe, skipping optional steps and steps on the same
 *  page. Null when the page isn't on the recipe or is its last page. */
export function nextStepFor(recipe: WrittenRecipe, currentRoute: string): NextStep | null {
  const steps = recipe.steps.map(s => ({ s, route: stepRoute(s) }))
  const here = steps.findIndex(x => x.route === currentRoute)
  if (here < 0) return null
  const next = steps.slice(here + 1).find(x => x.route && x.route !== currentRoute && !x.s.optional)
  if (!next?.route) return null
  return { route: next.route, label: navLabelFor(NAV_GROUPS, next.route), why: next.s.why }
}

/** The one line under "Next" saying where the step comes from. */
export function nextSourceLine(r: SetRecipe): string {
  return r.source === 'set'
    ? `From recipe · ${r.recipe.title}`
    : `Suggested · ${r.recipe.title}${r.reason ? ` (${r.reason})` : ''}`
}

/** The pages the set's CONFIRMED recipe walks through — the sidebar keeps them bright and dims the
 *  rest (still clickable; Decision 16). Null when the set has no recipe of its own: a suggestion
 *  does not restyle the sidebar until the user picks it. */
export function litRoutes(set: RecipeSetLike | null | undefined): Set<string> | null {
  const r = writtenRecipe(set?.recipe)
  return r ? new Set(recipeRoutes(r)) : null
}

/** Where a recipe starts: its first required step that lands on a page. */
export function firstStepOf(recipe: WrittenRecipe): NextStep | null {
  for (const s of recipe.steps) {
    const route = stepRoute(s)
    if (route && !s.optional) return { route, label: navLabelFor(NAV_GROUPS, route), why: s.why }
  }
  return null
}

