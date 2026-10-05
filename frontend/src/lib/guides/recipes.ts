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
  icon: string                  // an icon class, as a GuideDef carries
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
    // The workflow of the Cecelia paper's behaviour analysis (Schienstock et al. 2025, Nat Commun
    // 16:1931, doi:10.1038/s41467-025-57193-y, Fig. 4c): cellpose → btrack → HMM states on speed and
    // angle → Leiden on whole-track measures + HMM states + transitions.
    id: 'intravital-timelapse',
    title: 'Intravital timelapse',
    whenThisIsYou: 'A time-lapse of cells moving in tissue, e.g. T cells in a lymph node.',
    icon: 'pi-video',
    steps: [
      {
        guide: 'drift-correct',
        why: 'Tissue drift would read as movement in every track — remove it before anything else.',
      },
      {
        guide: 'segment-an-image',
        why: 'Cellpose finds the cells in most movies — start here.',
      },
      // The fork for movies too dim for cellpose. The one number is measured
      // (docs/todo/SEG_QUALITY_PLAN.md): on this lab's own photon-limited movie, 0 of 65 cellpose
      // objects passed QC. It is the exception, not the default path.
      {
        guide: 'train-flow-model',
        why: 'Only if cellpose misses dim cells: a flow model reads movement instead of brightness.',
        optional: true,
      },
      {
        guide: 'segment-by-motion',
        why: 'With that model: on one dim movie, 0 of the 65 cells cellpose found passed QC.',
        optional: true,
      },
      {
        guide: 'gate-populations',
        why: 'QC before tracking: gate out debris and doublets on size × intensity, then track the gate.',
      },
      {
        guide: 'track-cells',
        why: 'The composite, so tracks arrive measured — bare tracking leaves every later page empty.',
      },
      {
        guide: 'behaviour-states',
        why: 'States per timepoint from speed and angle, plus the transitions between them in each track.',
      },
      {
        guide: 'cluster-tracks',
        why: 'Leiden on whole-track measures plus HMM states and transitions: finer than track averages.',
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
