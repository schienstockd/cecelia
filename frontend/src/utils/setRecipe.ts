// Which recipe a set follows, and what a page's next step is under it (docs/todo/GUI_CLARITY_PLAN.md,
// Decisions 13 and 16). Pure, so `NextStep.vue` and `SetRecipePicker.vue` only render it.
//
// The recipe is stored per set (`set.recipe`, written by `POST /api/sets/recipe`). Without one, the
// set's axes SUGGEST a recipe, shown as a suggestion the user confirms, never applied silently. A
// recipe lights pages and names the next one; it never hides a page and never runs anything.
import { RECIPES, isWanted, recipeById, type WrittenRecipe, type RecipeStep } from '../lib/guides/recipes'
import { NAV_GROUPS, navLabelFor } from '../lib/navGroups'
import { stepRoute, recipeRoutes } from './guideMap'
import { taskLanding } from './kiwiTurn'
import { funsRunAcross, type RunLogEntry } from './runLog'

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

/** The pages a set's images have had a successful run on (`taskLanding`, the one fun → page rule). */
export function ranPagesOf(images: readonly { runLog?: RunLogEntry[] | null }[]): Set<string> {
  const out = new Set<string>()
  for (const fun of funsRunAcross(images.map(i => i.runLog))) {
    const p = taskLanding(fun)?.path
    if (p) out.add(p)
  }
  return out
}

/** One recipe step against a set: has its page run, and its place among the required steps. */
export interface StepProgress {
  step: RecipeStep
  route: string | null
  ran: boolean          // its page has a successful run on the set (`ranPagesOf`): ticked
  n: number | null      // 1-based among the REQUIRED steps; null for an optional one
}

export interface RecipeProgress {
  rows: StepProgress[]          // every step, recipe order
  next: StepProgress | null     // where to pick the set up (see `resumeStepOf`)
  required: number              // how many required steps, for "step 3 of 7"
}

/** A recipe walked against what has run on a set — the ONE rule for "where am I" that Welcome's
 *  Continue and the Guides dialog's next step both read, so they can never disagree.
 *  Next: the first required step whose page has not run yet, so a set whose images are already imported
 *  starts past Import. When every step has run, the last one (where the work was); with nothing run,
 *  the first. Optional steps are never next — their why-line is a condition only the user can judge. */
export function recipeProgress(recipe: WrittenRecipe, ranPages: ReadonlySet<string>): RecipeProgress {
  let n = 0
  const rows = recipe.steps.map((step): StepProgress => {
    const route = stepRoute(step)
    return { step, route, ran: !!route && ranPages.has(route), n: step.optional ? null : ++n }
  })
  const candidates = rows.filter(r => r.n !== null && r.route)
  const next = candidates.find(r => !r.ran) ?? candidates[candidates.length - 1] ?? null
  return { rows, next, required: n }
}

/** Where to pick a set up, as a page (Welcome's Continue): `recipeProgress`'s next step. */
export function resumeStepOf(recipe: WrittenRecipe, ranPages: ReadonlySet<string>): NextStep | null {
  const s = recipeProgress(recipe, ranPages).next
  if (!s?.route) return null
  return { route: s.route, label: navLabelFor(NAV_GROUPS, s.route), why: s.step.why }
}



/** What the Guides dialog opens on (docs/todo/WORKFLOW_RECIPES_PLAN.md → *The dialog's first view*):
 *  one choice and one button, everything else behind "Browse all guides".
 *    import   nothing imported anywhere in the project: the one step every recipe starts with, and the
 *             recipe question waits until the images can suggest an answer
 *    recipe   a recipe to walk: the active set's stored one, or (no set active) the one just selected
 *             in the dialog, which is not saved anywhere (`saved: false`)
 *    choose   "What are you working with?": the set's suggestion highlighted, never applied; selecting
 *             saves it as the set's recipe when a set is active (`saves`) */
export type GuideStartView =
  | { kind: 'import' }
  | { kind: 'recipe'; recipe: WrittenRecipe; saved: boolean }
  | { kind: 'choose'; suggestion: SetRecipe | null; saves: boolean }

export function guideStartView(o: {
  projectImages: number             // images across every set of the open project (0 with none open)
  set: RecipeSetLike | null         // the active set
  picked: string | null             // selected in the dialog while no set is active
  changing: boolean                 // the user pressed Change: show the choice even though there is a recipe
}): GuideStartView {
  if (o.projectImages === 0) return { kind: 'import' }
  if (o.set) {
    const own = writtenRecipe(o.set.recipe)
    if (own && !o.changing) return { kind: 'recipe', recipe: own, saved: true }
    return { kind: 'choose', suggestion: suggestRecipe(o.set), saves: true }
  }
  const picked = writtenRecipe(o.picked)
  if (picked && !o.changing) return { kind: 'recipe', recipe: picked, saved: false }
  return { kind: 'choose', suggestion: null, saves: false }
}

/** The recipe question's rows — Welcome's and the Guides dialog's, so the two read alike: every written
 *  recipe, the suggestion named on its own row (never pre-selected, GUI_CLARITY D16), and its
 *  recognition line. Columns: `RECIPE_CHOICE_COLUMNS`. */
export function recipeChoiceRows(suggestion: SetRecipe | null): { id: string; title: string; when: string }[] {
  return WRITTEN_RECIPES.map(r => ({
    id: r.id,
    title: r.id === suggestion?.recipe.id ? `${r.title} · suggested (${suggestion.reason})` : r.title,
    when: r.whenThisIsYou,
  }))
}

/** `recipeChoiceRows`' columns, in `SelectionTable`'s column shape. */
export const RECIPE_CHOICE_COLUMNS = [
  { key: 'title', label: 'Recipe' },
  { key: 'when', label: 'Is this you?', ellipsis: true },
]
