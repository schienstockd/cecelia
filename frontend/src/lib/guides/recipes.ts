// Processing recipes — the answer to "what are you trying to do?" (docs/todo/WORKFLOW_RECIPES_PLAN.md).
//
// The guides are indexed on ONE axis: where in the pipeline am I (the picker's Start / Data /
// Populations / … groups, mirroring the sidebar). This file is the second axis — WHICH pipeline is
// mine. A recipe is a list of existing guides with a reason attached to each, and **the reasons are
// the product**: `segmentGuide` cannot say "use coastal instead" because it is the cellpose guide, so
// a recipe is the only place where "for this data, that tool, and here is why" gets said once instead
// of as a tip on every affected control.
//
// A recipe COMPOSES guides and adds no runtime (plan D1). Starting a step starts the ordinary guide,
// with the ordinary bubble; nothing here can click, select, navigate or run anything, and a step
// naming a guide that does not exist fails `guides.test.ts` rather than dead-ending a user.
//
// Two bodies (plan D9). A WRITTEN recipe has steps. A WANTED one is a name and a request link: we
// only know the forks for data we have measured, so the honest form of "large multiplex images" today
// is an ask for what they image and for an example image — not a generic path assembled from
// plausible-sounding steps. `docs/UI.md` → *Guides*: a guide's prose is an assertion about the app
// that no ratchet can check, and every content bug in this system so far has been an invented fact.
//
// Called "recipe", not "scenario", deliberately: `utils/cssScenarios.ts` and `docs/UI.md`'s "pick a
// scenario, then a size" already own that word for the CSS/copy utilities, and one grep should not
// return both concepts.
//
// NOTE for whoever edits this file: `app/test/suite.jl` globs every non-test `.ts` in this directory
// and asserts that the `funName` and `taskKey` literals in it pair up one-to-one against the Julia
// task registry. A recipe names GUIDES, never functions, so neither key belongs in here — keep task
// names in the guide definitions where the ratchet can check them.

export interface RecipeStep {
  guide: string                 // an existing GuideDef id — checked by guides.test.ts
  why: string                   // one line: why this step, in THIS recipe. The fork, not a summary.
  optional?: boolean            // "only if your movie drifts"
}

interface RecipeBase {
  id: string
  title: string
}

export interface WrittenRecipe extends RecipeBase {
  // The recognition test — "is this me?" — not a description of the steps.
  whenThisIsYou: string
  icon: string                  // a PrimeIcons class, as a GuideDef carries
  steps: RecipeStep[]
  wanted?: never
}

// A scenario we have NOT written, shown so the user finds their case named rather than absent, with a
// link that asks for what would let us write it. Deliberately just a title: the ask is stated once,
// above the group, instead of a sentence per row (plan D9).
export interface WantedRecipe extends RecipeBase {
  wanted: true
  steps?: never
}

export type RecipeDef = WrittenRecipe | WantedRecipe

export const RECIPES: RecipeDef[] = [
  {
    id: 'intravital-timelapse',
    title: 'Intravital timelapse',
    whenThisIsYou: 'Photon-limited movie of moving cells in tissue.',
    icon: 'pi-video',
    steps: [
      {
        guide: 'drift-correct',
        why: 'Only if the tissue drifts — it adds a version and never overwrites your import.',
        optional: true,
      },
      {
        guide: 'train-flow-model',
        why: 'Cellpose reads brightness; a flow model reads movement, which is what your cells give you.',
      },
      {
        guide: 'segment-by-motion',
        // The one number in here, and it is measured: docs/todo/SEG_QUALITY_PLAN.md, cellpose 4
        // matched to the tuned v3 config on this lab's own intravital movie — 0 of 65 objects passed
        // QC. The version archaeology (tuned cyto2 reached 13.4%, and is no longer selectable) is in
        // that plan; a recipe states the fork, not the history.
        why: 'Not cellpose: on a dim intravital movie, 0 of the 65 cells it found passed QC.',
      },
      {
        guide: 'track-cells',
        why: 'The composite, so tracks arrive measured — bare tracking leaves every later page empty.',
      },
      {
        guide: 'behaviour-states',
        why: 'What tracks are for: the states a cell moves through, not only where it went.',
      },
    ],
  },
  { id: 'large-multiplex', title: 'Large multiplex images', wanted: true },
  { id: 'cell-interactions', title: 'Cell interactions', wanted: true },
  { id: 'many-small-confocal', title: 'Many small confocal images', wanted: true },
]

export const isWanted = (r: RecipeDef): r is WantedRecipe => r.wanted === true

export function recipeById(id: string): RecipeDef | undefined {
  return RECIPES.find(r => r.id === id)
}
