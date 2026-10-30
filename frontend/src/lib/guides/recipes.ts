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

interface StepBase {
  why: string                   // one line: why this step, in THIS recipe. The fork, not a summary.
  optional?: boolean            // "only if your movie drifts"
  // What Claude adds at this step, shown only with the map's Claude toggle on (WORKFLOW_RECIPES_PLAN
  // D11). `where` is the way in that can do it: 'kiwi' works in Kiwi AND a terminal session (Kiwi only
  // reads); 'terminal' needs a terminal session. Only shipped tools — a step Claude adds nothing to
  // has no `assist`, not a line saying so.
  assist?: { where: 'kiwi' | 'terminal'; text: string }
}
// A step taught by a guide…
export interface RecipeGuideStep extends StepBase {
  guide: string                 // an existing GuideDef id — checked by guides.test.ts
  page?: never
}
// …or a specialised step with no guide (WORKFLOW_RECIPES_PLAN D10): it names the task and the sidebar page
// it lives on, and Open goes there. `page` must be a sidebar route — checked by guides.test.ts.
export interface RecipePageStep extends StepBase {
  page: string
  label: string                 // the task as the page names it, e.g. "Detect aggregates"
  guide?: never
}
export type RecipeStep = RecipeGuideStep | RecipePageStep
export const isGuideStep = (s: RecipeStep): s is RecipeGuideStep => typeof s.guide === 'string'

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

// The lab's own published workflows (WORKFLOW_RECIPES_PLAN D10): the Cecelia paper's Results
// (Schienstock et al. 2025, Nat Commun 16:1931, doi:10.1038/s41467-025-57193-y, Figs 2–5) and the
// R version's missions (cecelia.readthedocs.io/en/latest/missions.html). The data picks the recipe;
// the question asked of it (interactions, regions, behaviour, structures) is steps inside it. Every
// recipe ends the same way: settle the parameters on a few images, then run them over the set.
const CHAIN_STEP: RecipeGuideStep = {
  guide: 'run-a-chain',
  why: 'Settled on a few images? Wire the same steps into a chain and run it over the whole set.',
  assist: { where: 'terminal', text: 'Builds the chain from the steps you ran, for you to review and run' },
}
const SEGMENT_QC = { where: 'kiwi' as const, text: 'Share a frame with a bad mask; it points at suspect cells and names the knob' }
const READOUT = { where: 'kiwi' as const, text: 'Turns the numbers into sentences, each citing what it rests on' }
const BOARD = { where: 'terminal' as const, text: 'Adds a board comparing your groups; it never edits yours' }

export const RECIPES: RecipeDef[] = [
  {
    // Fig 2, mission "Mouse spleen confocal after LCMV infection".
    id: 'few-marker-tissue',
    title: 'Few-marker tissue',
    whenThisIsYou: 'A fixed section with a handful of markers, e.g. a confocal stack of spleen.',
    icon: 'pi-th-large',
    steps: [
      { guide: 'import-images', why: 'One image per acquisition; group them into sets by mouse or condition.' },
      { guide: 'fix-metadata', why: 'Pixel size sets every distance and volume; channel names are what you gate on.' },
      {
        guide: 'af-correct', optional: true,
        why: 'Only if autofluorescence or one channel\'s signal shows in another.',
      },
      {
        guide: 'segment-an-image',
        why: 'One model per population, grouped into one segmentation: mixed shapes and sizes need it.',
        assist: SEGMENT_QC,
      },
      {
        guide: 'gate-populations',
        why: 'Drop debris on volume × surface:volume; gate sparse populations first, the abundant last.',
        assist: { where: 'kiwi', text: 'Describes a gate and the cells in it; it cannot draw one' },
      },
      { guide: 'spatial-analysis', why: 'The neighbour graph first, then who touches whom.', assist: READOUT },
      {
        page: '/spatial', label: 'Detect aggregates', optional: true,
        why: 'Groups of one population packed together, e.g. T cells clustering around their DCs.',
      },
      { guide: 'build-plots', why: 'Compare populations and interactions across mice on one board.', assist: BOARD },
      CHAIN_STEP,
    ],
  },
  {
    // Fig 3a–b, missions "Human spleen IBEX naive" and "Human lymph node naive CODEX".
    id: 'multiplex-tissue',
    title: 'Multiplex tissue',
    whenThisIsYou: 'Many markers on one section, e.g. IBEX or CODEX of a lymph node or spleen.',
    icon: 'pi-objects-column',
    steps: [
      { guide: 'import-images', why: 'One image per section; a cohort of sections becomes a set.' },
      { guide: 'fix-metadata', why: 'With dozens of channels, the names are what clustering and heatmaps show.' },
      {
        guide: 'segment-an-image',
        why: 'Membrane and nuclear markers together, so cells of every shape are found.',
        assist: SEGMENT_QC,
      },
      {
        guide: 'cluster-cells',
        why: 'Too many markers to gate by hand: Leiden finds the populations, you name them.',
        assist: { where: 'kiwi', text: 'Describes a cluster from its marker profile; you name it' },
      },
      { guide: 'spatial-analysis', why: 'The neighbour graph is what tissue regions are clustered from.', assist: READOUT },
      {
        guide: 'cluster-regions',
        why: 'Neighbourhoods grouped into regions, e.g. B cell, T cell and macrophage zones.',
        assist: READOUT,
      },
      {
        page: '/segment', label: 'Branching (skeleton) analysis', optional: true,
        why: 'Only with a stromal marker: segment the stroma, then skeletonise it into a branch network.',
      },
      { guide: 'build-plots', why: 'Region and population frequencies across sections, on one board.', assist: BOARD },
      CHAIN_STEP,
    ],
  },
  {
    // The workflow of the Cecelia paper's behaviour analysis (Fig. 4c): cellpose → btrack → HMM states
    // on speed and angle → Leiden on whole-track measures + HMM states + transitions.
    id: 'intravital-timelapse',
    title: 'Intravital timelapse',
    whenThisIsYou: 'A time-lapse of cells moving in tissue, e.g. T cells in a lymph node.',
    icon: 'pi-video',
    steps: [
      {
        guide: 'drift-correct',
        why: 'Tissue drift would read as movement in every track — remove it before anything else.',
      },
      // After drift, not before: the AF task carries a drift-corrected store's valid box forward and
      // its background estimate skips the zero padding. The condition is the user's to judge — no
      // channel or threshold advice here.
      {
        guide: 'af-correct',
        why: 'Only if one channel\'s signal shows in another (bleedthrough, shared autofluorescence): a cell counts in both.',
        optional: true,
      },
      // Photon-limited data only: SUPPORT learns the noise floor of movies like this one.
      {
        guide: 'train-denoise-model', optional: true,
        why: 'Only for photon-limited movies: a model of your noise floor makes dim cells segmentable.',
      },
      {
        guide: 'segment-an-image',
        why: 'Cellpose finds the cells in most movies — start here.',
        assist: SEGMENT_QC,
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
        assist: { where: 'kiwi', text: 'Describes a gate and the cells in it; it cannot draw one' },
      },
      {
        guide: 'track-cells',
        why: 'The composite, so tracks arrive measured — bare tracking leaves every later page empty.',
        assist: { where: 'terminal', text: 'Marks the tracks that look wrong and jumps to the frame' },
      },
      {
        page: '/spatial', label: 'Detect aggregates', optional: true,
        why: 'Cells clustering together over time, e.g. T cells gathering where antigen is presented.',
      },
      {
        guide: 'behaviour-states',
        why: 'States per timepoint from speed and angle, plus the transitions between them in each track.',
        assist: READOUT,
      },
      {
        guide: 'cluster-tracks',
        why: 'Leiden on whole-track measures plus HMM states and transitions: finer than track averages.',
        assist: { where: 'kiwi', text: 'Describes a behaviour cluster from its track measures; you name it' },
      },
      CHAIN_STEP,
    ],
  },
  {
    // Fig 5a, mission "Mouse skin T cell interactions with lymphatics in DTH-inflammation". The distance
    // to the structure is a per-cell contact column that cluster tracks aggregates per track (sd, qUp,
    // qLow — the Fig 5a heatmap). WORKFLOW_RECIPES_PLAN D10: wired, not yet run end to end.
    id: 'live-structures',
    title: 'Live cells and structures',
    whenThisIsYou: 'A time-lapse where cells move along a structure, e.g. T cells and lymphatics in skin.',
    icon: 'pi-share-alt',
    steps: [
      { guide: 'drift-correct', why: 'Tissue drift would read as movement in every track — remove it first.' },
      { guide: 'af-correct', optional: true, why: 'Only if one channel\'s signal shows in another.' },
      {
        guide: 'segment-an-image',
        why: 'The cells and the structure as separate segmentations, each with its own model.',
        assist: SEGMENT_QC,
      },
      { guide: 'gate-populations', why: 'Gate out debris before tracking, then track the gate.' },
      { guide: 'track-cells', why: 'The composite, so tracks arrive measured.' },
      {
        page: '/spatial', label: 'Cell contacts (meshes)',
        why: 'Each cell\'s distance to the structure, per timepoint: the column behaviour clusters on.',
        assist: READOUT,
      },
      { guide: 'behaviour-states', why: 'Movement states per timepoint, from speed and angle.', assist: READOUT },
      {
        guide: 'cluster-tracks',
        why: 'Add the distance to the structure to the features: behaviour near it and away from it.',
        assist: { where: 'kiwi', text: 'Describes a behaviour cluster from its track measures; you name it' },
      },
      CHAIN_STEP,
    ],
  },
  {
    // Fig 5b, mission "Cancer cells in vitro flow tracks". Track flow fields are deferred
    // (SPATIAL_ANISOTROPY_PLAN D11), so the recipe stops at behaviour clusters.
    id: 'in-vitro-migration',
    title: 'In vitro migration',
    whenThisIsYou: 'Cells migrating in a dish or chamber, e.g. a cancer cell line under two treatments.',
    icon: 'pi-arrows-alt',
    steps: [
      { guide: 'import-images', why: 'One movie per well or position; a treatment becomes a set.' },
      { guide: 'fix-metadata', why: 'Pixel size and frame interval set every speed.' },
      { guide: 'segment-an-image', why: 'Cellpose on the nuclear stain finds the cells.', assist: SEGMENT_QC },
      { guide: 'gate-populations', why: 'Gate out debris and fragments before tracking, then track the gate.' },
      {
        guide: 'track-cells',
        why: 'The composite, so tracks arrive measured, e.g. straightness for directed motion.',
        assist: { where: 'terminal', text: 'Marks the tracks that look wrong and jumps to the frame' },
      },
      { guide: 'behaviour-states', why: 'Movement states per timepoint, from speed and angle.', assist: READOUT },
      {
        guide: 'cluster-tracks',
        why: 'Leiden on track measures, HMM states and transitions; compare treatments by cluster.',
        assist: { where: 'kiwi', text: 'Describes a behaviour cluster from its track measures; you name it' },
      },
      CHAIN_STEP,
    ],
  },
]

export const isWanted = (r: RecipeDef): r is WantedRecipe => r.wanted === true

export function recipeById(id: string): RecipeDef | undefined {
  return RECIPES.find(r => r.id === id)
}
