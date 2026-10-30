// The map of Cecelia — what the sidebar can't say: what each page is for, which page summarises the populations it defines, and which guide teaches it. ONLY that.
// Everything else about a page (label, icon, group, order, hover tip) is `navGroups.ts`'s, and what a
// guide needs is its own prereqs — so this file never restates them (WORKFLOW_RECIPES_PLAN → *The
// map*, "one source"). Three things read it: the map at the front of the Guides dialog (`GuideMap.vue`),
// user-docs/README.md (generated from it — `utils/userDocs.ts`), and the checks in
// `utils/guideMap.test.ts` (every sidebar page has an entry; every guide and pair exists).
//
// Keyed by sidebar route. A renamed page is a one-line change in navGroups.ts; nothing here moves.

export interface MapEntry {
  what: string          // what it's for, one line, no trailing period — the map's hover, the README's cell
  pairedWith?: string   // the page that summarises what this one defines (Gate → Phenotype)
  guide?: string        // the guide that teaches it, if one exists
  claude?: string       // what Claude MAKES on this page — the map's ✦ with the Claude toggle on. Only
                        // pages where it creates something you keep; Kiwi reads every page, so a
                        // badge for reading would mark them all. Mirrors the observer's create tools.
}

export const MAP: Record<string, MapEntry> = {
  // Data
  '/manage-images':  { what: 'Add, organise, copy, move and export images', guide: 'import-images' },
  '/preprocess':     { what: 'Crop, project or resample into a new image', guide: 'preprocess-images' },
  '/metadata':       { what: 'Pixel size, channel names, colours — fix before segmenting', guide: 'fix-metadata' },
  '/cleanup':        { what: 'Drift correction, bleedthrough and autofluorescence, denoise', guide: 'drift-correct' },
  '/model-training': { what: 'Train your own denoise or flow models', guide: 'train-flow-model' },
  '/segment':        { what: 'Find cells, with segmentation QC', guide: 'segment-an-image' },
  // Populations — define who the cells are
  '/gate':           { what: 'Draw gates on cell measures, like flow cytometry', pairedWith: '/phenotype', guide: 'gate-populations' },
  '/track':          { what: 'Link cells over time; gate tracks', pairedWith: '/behaviour', guide: 'track-cells' },
  '/clust-cells':    { what: 'Group cells by phenotype across a set', pairedWith: '/phenotype', guide: 'cluster-cells' },
  '/clust-tracks':   { what: 'Group tracks by behaviour', pairedWith: '/behaviour', guide: 'cluster-tracks' },
  '/regions':        { what: 'Find tissue neighbourhoods', pairedWith: '/spatial', guide: 'cluster-regions' },
  // Explore — summarise those populations
  '/phenotype':      { what: 'Counts and proportions per population, across images' },
  '/behaviour':      { what: 'Speed, HMM states and other track measures', guide: 'behaviour-states' },
  '/spatial':        { what: 'Neighbours, interactions, contacts — build the graph first', guide: 'spatial-analysis' },
  // Analysis — your own figures and notes
  '/analysis':       { what: 'Combine any plots across pages and images; export PDF or CSV', guide: 'build-plots',
                      claude: 'A terminal session builds a board of plots for you' },
  '/blackboard':     { what: 'Notes for an open question, versioned, tagged good or bad', guide: 'use-the-blackboard',
                      claude: 'Kiwi replies you save land here; a terminal session writes entries' },
  '/notebooks':      { what: 'Pluto (Julia) notebooks for what the board can’t do', guide: 'create-a-notebook',
                      claude: 'A terminal session writes a notebook for you' },
  '/animation':      { what: 'Keyframe a fly-through of one image into a movie', guide: 'build-an-animation' },
  '/batch-movies':   { what: 'One movie setup, one timelapse movie per image', guide: 'record-a-movie' },
  '/movies':         { what: 'Play the movies this project has rendered' },
  // Pipeline — run many images
  '/tasks':          { what: 'Everything running or finished; history of past runs' },
  '/chain':          { what: 'Wire the steps you settled on into a chain; run it over a set', guide: 'run-a-chain',
                      claude: 'A terminal session designs the chain; you run it' },
}

// One line under each sidebar group's heading — the README's, and the map's group hover.
export const GROUP_BLURB: Record<string, string> = {
  Data: 'Get images in and ready',
  Populations: 'Define who the cells are',
  Explore: 'Summarise those populations across images',
  Analysis: 'Your own figures and notes',
  Pipeline: 'Run many images',
}
