// The project's outward links, in one place.
//
// There were three hardcoded `github.com/schienstockd/cecelia/…` literals across the frontend before
// this file (the What's New "Report a problem" default, its "View on GitHub" footer link, and the
// releases page) and the header was about to add two more. A repo move is one rename here, not a
// grep — and the rename is coming: `docs/SHIPPING.md` → *Repo swap* still has the legacy repo living
// at the old name.
//
// Kept deliberately dumb — string constants only, no fetching. `stores/appControl.ts` owns anything
// that ASKS GitHub something (the update check reads the releases API); this module only says where
// things are.

export const CECELIA_REPO_URL = 'https://github.com/schienstockd/cecelia'

/** The issues LIST — "here is where problems get reported", for a browse-first entry point. */
export const CECELIA_ISSUES_URL = `${CECELIA_REPO_URL}/issues`
/** Straight to the form — for a "report this" action, where the user already knows what they hit. */
export const CECELIA_NEW_ISSUE_URL = `${CECELIA_REPO_URL}/issues/new`
export const CECELIA_RELEASES_URL = `${CECELIA_REPO_URL}/releases`

/**
 * A processing-recipe request, prefilled with the scenario the user clicked — the `Request` links in
 * `GuidesDialog` (docs/todo/WORKFLOW_RECIPES_PLAN.md D9).
 *
 * Points at the recipe FORM rather than a blank issue on purpose: the form asks the three things we
 * cannot guess — what they image, what they want out of it, and whether they can share an example
 * image — and a recipe written without those is the invented prose the guide system keeps getting
 * bitten by. Still just a string; nothing here posts anything.
 */
export function recipeRequestUrl(name: string): string {
  const q = new URLSearchParams({ template: 'recipe_request.yml', title: `Recipe: ${name}` })
  return `${CECELIA_REPO_URL}/issues/new?${q}`
}

// Zulip, not a GitHub Discussion: the lab already runs one, and a question people ask in chat is a
// question they would not have opened an issue for.
export const CECELIA_CHAT_URL = 'https://cecelia.zulipchat.com/'

/**
 * A "please support this file format for series picking" feature request, prefilled with the
 * extensions the user just hit. The picker only reads series from `.lif` today (readlif — pure
 * Python, no JVM); every other multi-series format falls through to a series-0 default. This turns
 * "we can't preview this" into a directed ask — the reason we bank the request is that the fastest
 * way to widen coverage is a sample file we can measure against (docs/PROVENANCE.md → *Real-data
 * visual validation*), so the prompt is what to attach.
 */
export function formatSupportRequestUrl(exts: string[]): string {
  const dedup = Array.from(new Set(exts.map(e => e.toLowerCase().replace(/^\./, ''))))
  const list  = dedup.join(', ')
  const q = new URLSearchParams({
    template: 'feature_request.yml',
    title:    `Series picker: support ${list}`,
  })
  return `${CECELIA_REPO_URL}/issues/new?${q}`
}
