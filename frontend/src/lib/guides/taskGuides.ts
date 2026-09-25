// The guides that are "run a function on some images" — all built from `moduleTask.ts` (plan D8), so
// what lives here is only the prose that is genuinely per-function: what it is for, which parameters
// matter, and what to do with the output.
//
// Adding a guide for another module page belongs HERE as another `moduleTaskGuide({…})` call. Reach
// for a hand-written step list only when the page's shape is genuinely different (gating's canvas,
// the notebook server) — see `gatePopulations.ts` / `notebooks.ts` / `plots.ts`.
//
// Parameter bullets are checked against the task JSON specs, not invented: `driftEstimator`/
// `driftMaxLag` (cleanupImages/drift_correct.json), `models`/`cellDiameter` (segment/cellpose.json),
// `maxSearchRadius`/`minTimepoints` (tracking/bayesian_tracking.json) + `dims`
// (tracking/track_measures.json, merged in by the composite).
//
// Both segment and track teach the COMPOSITE (`…cellposeMeasure`, `…bayesian_track_measures`) rather
// than the bare task beside it in the dropdown. Labels without measures, or tracks without measures,
// leave every downstream page with nothing to read — which the guides' own endings promised.
//
// The clustering pair (`clusterCells`/`clusterTracks`) and the HMM guide (`behaviourStates`) also live
// here — same builder, same shape. They used to sit in `extraGuides.ts` as "the guides beyond the
// original seven"; that split was historical rather than architectural (see docs/todo/GUIDE_SYSTEM_AUDIT.md).

import { moduleTaskGuide } from './moduleTask'
import { PREREQ } from './prereqs'
import type { GuideStep } from './types'

// Where segmentation and tracking both end up: the mask/tracks are only trustworthy once you have
// LOOKED at them. So we point at the control that puts them on screen and say what to look for.
const viewerCheck = (what: string, toggleAnchor: string, lookFor: string[]): GuideStep[] => [
  {
    anchor: 'images.viewerBtn',
    placement: 'right',
    title: 'Open it in the viewer',
    text: 'The ↗ opens the image itself — the overlay is a separate switch.',
    when: c => c.viewerImageUid !== null,
  },
  {
    anchor: 'sidebar.viewerCta',
    placement: 'right',
    title: 'Open the Viewer panel',
    text: 'This panel drives the viewer — overlays, contrast, 3D, recording.',
    reveal: {
      needed: c => !c.viewerPanelOpen,
      anchor: 'sidebar.viewerCta',
      text: 'The Viewer panel is closed — open it here.',
      placement: 'right',
    },
    when: c => c.viewerPanelOpen,
  },
  {
    // The step users get stuck on: a finished run puts nothing on the image by itself. Each
    // segmentation has its own row here with a per-overlay switch, and until you flip it the viewer
    // shows the raw channels and it looks like the run did nothing.
    anchor: toggleAnchor,
    placement: 'left',
    title: `Switch the ${what} on`,
    text: `Nothing is drawn on the image until you toggle the ${what} for that segmentation.`,
    bullets: [
      'One row per segmentation, each with its own switches.',
      'The choice is remembered, so it comes back next time you open the image.',
    ],
    reveal: {
      needed: c => !c.viewerPanelOpen,
      anchor: 'sidebar.viewerCta',
      text: 'The Viewer panel is closed — open it to reach the overlay switches.',
      placement: 'right',
    },
  },
  {
    anchor: toggleAnchor,
    placement: 'left',
    title: `Now judge the ${what}`,
    text: 'With it on screen, this is what to look for.',
    bullets: lookFor,
  },
]

export const driftCorrectGuide = moduleTaskGuide({
  id: 'drift-correct',
  title: 'Drift correct a time series',
  group: 'Data',
  icon: 'pi-sparkles',
  summary: 'Register a drifting time series so cells stay put between frames.',
  route: '/cleanup',
  navLabel: 'Cleanup',
  taskKey: 'driftCorrect',
  funName: 'cleanupImages.driftCorrect',
  funLabel: 'Drift correction',
  selectionModule: 'cleanup',
  waitLabel: 'Drift correcting',
  prereqs: [PREREQ.projectOpen, PREREQ.imageImported, PREREQ.timeSeries],
  intro: 'Cleanup holds the corrections you apply before segmenting — drift, autofluorescence, noise.',
  // Deliberately the BARE task, unlike segment and track: the composite beside it in the dropdown
  // ("AF + drift correction") adds autofluorescence removal, a separate scientific decision rather
  // than the missing half of this one. Declared in the Julia ratchet so it stays a choice.
  funHint: [
    'Drift correction on its own — this is the whole operation, not half of one.',
    '"AF + drift correction" also removes autofluorescence, if you need that in the same pass.',
  ],
  selectHint: ['Only time series are worth correcting — a single frame cannot drift.'],
  params: [
    'Drift reference channel — pick a stable, bright structure, not a motile cell.',
    'Estimator — multi-lag compares non-adjacent frames, so one bad frame cannot offset the rest.',
    'Max frame gap — higher is more robust and slower.',
  ],
  after: [
    {
      anchor: 'images.table',
      route: '/cleanup',
      placement: 'top-start',
      title: 'It made a new version',
      text: 'Corrections never overwrite your import — they add a version and make it active.',
      bullets: [
        'Everything downstream reads the ACTIVE version.',
        'The row\'s info icon lists every version the image has.',
      ],
    },
    {
      anchor: 'images.qcDot',
      route: '/cleanup',
      placement: 'left',
      title: 'Did it work?',
      text: 'Cecelia measures the leftover drift itself and flags it here — hover for the findings.',
      bullets: [
        'A flag means the reference channel probably lost tracking.',
        'Re-run with a clearer, structural channel.',
      ],
    },
  ],
})

export const segmentGuide = moduleTaskGuide({
  id: 'segment-an-image',
  title: 'Segment an image',
  group: 'Data',
  icon: 'pi-th-large',
  summary: 'Turn fluorescence into per-cell labels — the entry point for everything else.',
  route: '/segment',
  navLabel: 'Segment',
  // The COMPOSITE (segment.cellpose + segment.measureLabels), not plain `segment.cellpose`. Labels on
  // their own carry no measures, so gating/tracking/clustering would have nothing to read and the QC
  // plots — declared on the measure step — would be empty. The guide's whole downstream story depends
  // on measuring, so it teaches the function that does both.
  taskKey: 'cellposeMeasure',
  funName: 'segment.cellposeMeasure',
  funLabel: 'Cellpose segment + measure',
  selectionModule: 'segment',
  waitLabel: 'Segmenting',
  withPreview: true,
  intro: 'Segmentation is the entry point: gating, tracking and clustering all read its output.',
  funHint: [
    'Plain "Cellpose segmentation" makes labels only — no measures to gate or cluster on.',
    'This one measures too, so everything downstream has something to read.',
    'Dim moving cells in tissue? Segment by motion instead — cellpose is for static signal.',
  ],
  params: [
    'Cell channels — the channels carrying the cell signal; they are merged by maximum.',
    'Cell diameter (µm) — the single setting that most decides whether this works.',
    'Nucleus channels — pair a "nuc" model with a "base" one for nucleus-anchored cells.',
    'Clear border cells — drops cells cut off by the image edge.',
  ],
  after: [
    {
      anchor: 'layout.plotsSection',
      route: '/segment',
      placement: 'top-start',
      title: 'Check the QC first',
      text: 'These plots flag the usual failures — too few cells, implausible sizes, edge artefacts.',
      bullets: [
        'One population per segmentation, so two runs plot side by side.',
        'A weird size distribution almost always means the diameter was off.',
      ],
    },
    ...viewerCheck('mask', 'viewer.toggleLabels', [
      'Are single cells one label, or is a clump merged into one?',
      'Are the outlines on the cells, or offset from them?',
    ]),
    {
      text: 'Labels are in, and measured — which is what makes them useful.',
      title: 'What you now have',
      bullets: [
        'Each cell has an id, a centroid and its regionprops + intensity measures.',
        'Gate on those next, or track them if this is a time series.',
      ],
    },
  ],
})

export const trackCellsGuide = moduleTaskGuide({
  id: 'track-cells',
  title: 'Track cells',
  group: 'Populations',
  icon: 'pi-share-alt',
  summary: 'Link segmented cells across frames into tracks, with speed and direction for free.',
  route: '/track',
  navLabel: 'Track',
  // The COMPOSITE (tracking.bayesian_tracking + tracking.track_measures), for the same reason segment
  // uses its composite: bare tracks carry no per-track measures, and speed/angle are what the HMM fits
  // and what track clustering and track gating read.
  taskKey: 'bayesianTrackMeasures',
  funName: 'tracking.bayesian_track_measures',
  funLabel: 'Bayesian track + measures',
  selectionModule: 'tracking',
  waitLabel: 'Tracking',
  funHint: [
    'Plain "Bayesian tracking" links cells but computes no per-track measures.',
    'This one measures too — speed, displacement, angle — which is what you gate and cluster on.',
  ],
  prereqs: [PREREQ.projectOpen, PREREQ.timeSeries, PREREQ.segmented],
  intro: 'Tracking links labels across frames — so segment every timepoint before you come here.',
  selectHint: [
    'Each image needs a segmentation covering all of its frames.',
    'It must be a MEASURED segmentation — the Segment guide\'s function does both.',
  ],
  params: [
    'Segmentation — which label set to track; a gated population narrows it.',
    'Max search radius (µm) — the furthest a cell may move between frames (~20 for T cells).',
    'Allowed gaps and minimum timepoints — how forgiving linking is, and what counts as a real track.',
    'Motion dimensions — 2D or 3D for the measures; it detects and recommends one.',
  ],
  after: [
    ...viewerCheck('tracks', 'viewer.toggleTracks', [
      'Do the trails follow single cells, or jump between neighbours?',
      'Jumping usually means the search radius is too generous.',
    ]),
    {
      anchor: 'layout.plotsSection',
      route: '/track',
      placement: 'top-start',
      title: 'Gate on track properties',
      text: 'This is the gating canvas in track mode — one point per track, not per cell.',
      bullets: [
        'Gate on speed, displacement and the other per-track aggregates.',
        'Select exactly one image to use it.',
      ],
    },
    {
      text: 'Tracks are in, and measured — speed, displacement and angle came with them.',
      title: 'What you now have',
      bullets: [
        'Every cell has a track_id and a position at each timepoint.',
        'Behaviour → HMM fits states to those measures; Cluster tracks groups on them.',
      ],
    },
  ],
})

// ── Optical flow: train, then segment by motion ───────────────────────────────
// Why these exist at all: cellpose 4 replaced the cyto*/nuclei zoo with one generalist model, and
// `docs/todo/SEG_QUALITY_PLAN.md` measured that model at 0.0% QC-pass on an intravital movie where
// tuned `cyto2` reached 13.4% — with `cyto2` no longer selectable. So the guides shipped teaching
// cellpose as THE way to segment, on an app whose answer for dim moving cells is now this pair.
// (docs/todo/CELLPOSE_V4_PLAN.md, docs/todo/WORKFLOW_RECIPES_PLAN.md P0.)
//
// Two guides, not one, because they are two runs on two pages with a real gap between them: a model
// is trained once per kind of movie and then reused across projects (the vault is in `config_dir`, not
// the project — `list_coastal_models` in config.jl).
//
// No `flowModelTrained` prereq, deliberately: every prereq in `prereqs.ts` is answerable from state
// the frontend already holds, and the vault list arrives with the served task spec instead. So the
// dependency is said in the copy — see WORKFLOW_RECIPES_PLAN.md D8 for the version that could gate on it.
export const trainFlowModelGuide = moduleTaskGuide({
  id: 'train-flow-model',
  title: 'Train a flow model',
  group: 'Data',
  icon: 'pi-sync',
  summary: 'Teach a model what moving cells look like in your own movies — the step before motion segmentation.',
  route: '/model-training',
  navLabel: 'Model training',
  taskKey: 'trainFlowModel',
  funName: 'opticalFlow.train',
  funLabel: 'Train flow model',
  // Backend module key stays `opticalFlow` — it is baked into the fun_name prefix and stored
  // ccid.json chain state. The page it lives on was renamed for Phase C but the taxonomy id was not.
  selectionModule: 'opticalFlow',
  waitLabel: 'Training',
  // A time series, not just an image: flow is computed between frames, so one frame trains nothing.
  prereqs: [PREREQ.projectOpen, PREREQ.imageImported, PREREQ.timeSeries],
  intro: 'Motion segmentation learns from movement, so it needs a model trained on movies like yours.',
  params: [
    'Model name — the vault entry this writes; you pick it again when you segment.',
    'Channels — the ones showing cell bodies move; the model reads motion, not markers.',
    'Max frames per movie — cap it for a first pass, then retrain on more.',
    'Epochs — 30 to start; the loss curve says whether it needed more.',
  ],
  after: [
    {
      anchor: 'layout.plotsSection',
      route: '/model-training',
      placement: 'top-start',
      title: 'Read the training curves',
      text: 'This is how you tell a model that learned from one that stalled.',
      bullets: [
        'A loss still falling at the last epoch means train again with more.',
        'Flow metrics show what the model was actually fed.',
      ],
    },
    {
      title: 'What you now have',
      text: 'A model in the vault, not in this project — the same one segments any movie of this kind.',
      bullets: ['Segment with it next, on the Segment page.'],
    },
  ],
})

// A denoise model is a peer of the flow model on the same page: same layout, same rail, different
// function. Trained once per set (channels pool into one model) and reused wherever that channel mix
// shows up. Deliberately no `driftCorrected` prereq — the picker only checks state the frontend
// already holds; the "run drift correction first" note lives in the copy.
export const trainDenoiseModelGuide = moduleTaskGuide({
  id: 'train-denoise-model',
  title: 'Train a denoise model',
  group: 'Data',
  icon: 'pi-sync',
  summary: 'Teach a SUPPORT model your noise floor so weaker frames become readable — the step before segmenting dim movies.',
  route: '/model-training',
  navLabel: 'Model training',
  taskKey: 'trainSupportDenoise',
  funName: 'opticalFlow.trainSupportDenoise',
  funLabel: 'Train denoise model (SUPPORT)',
  selectionModule: 'opticalFlow',
  waitLabel: 'Training',
  prereqs: [PREREQ.projectOpen, PREREQ.imageImported, PREREQ.timeSeries],
  intro: 'SUPPORT learns your movie\'s noise by comparing the same pixel across frames — so run drift correction first.',
  funHint: [
    'Run drift correction before this; SUPPORT needs the same pixel to stay put across frames.',
    'One model per set — pool the channels you want denoised into a single run.',
  ],
  params: [
    'Model name — the vault entry this writes; you pick it again when you segment or denoise.',
    'Channels — pool the ones with the same noise character; one model covers them all.',
    'Model size and temporal window — Medium and 61 frames are safe defaults; keep the window odd.',
    'Epochs — 20 to start; the loss curve says whether it needed more.',
  ],
  after: [
    {
      anchor: 'layout.plotsSection',
      route: '/model-training',
      placement: 'top-start',
      title: 'Read the training curves',
      text: 'Same read as any training run — a loss still falling means feed it more.',
      bullets: [
        'A stalled curve early on usually means the input was already clean, not that the model failed.',
      ],
    },
    {
      title: 'What you now have',
      text: 'A denoise model in the vault, not this project — it applies to any movie of this kind.',
      bullets: ['Denoise or segment with it next; the vault list holds it under the name you gave.'],
    },
  ],
})

export const segmentByMotionGuide = moduleTaskGuide({
  id: 'segment-by-motion',
  title: 'Segment a movie by motion',
  group: 'Data',
  icon: 'pi-th-large',
  summary: 'Find cells by how they move rather than how bright they are — for dim, moving cells in tissue.',
  route: '/segment',
  navLabel: 'Segment',
  // The COMPOSITE, same reason as cellpose above: bare labels carry no measures, so gating, tracking
  // and clustering would all have nothing to read.
  taskKey: 'coastalMeasure',
  funName: 'segment.coastalMeasure',
  funLabel: 'Optical flow segment + measure',
  selectionModule: 'segment',
  waitLabel: 'Segmenting',
  withPreview: true,
  prereqs: [PREREQ.projectOpen, PREREQ.imageImported, PREREQ.timeSeries],
  intro: 'This segments by motion, so it works where a cell is too dim to find in any single frame.',
  funHint: [
    'Needs a trained flow model — run "Train a flow model" first.',
    'Plain "Optical flow segmentation" makes labels only, with no measures to gate on.',
  ],
  params: [
    'Model — the one you trained; its manifest fixes the metrics and scales used.',
    'Cell channels — the same channels the model was trained on.',
    'Seed window (µm) — about one cell across; it decides what counts as one object.',
    'Foreground threshold — raise it if background is coming through as cells.',
  ],
  after: [
    {
      anchor: 'layout.plotsSection',
      route: '/segment',
      placement: 'top-start',
      title: 'Check the QC first',
      text: 'Same plots as any segmentation — too few cells, implausible sizes, edge artefacts.',
      bullets: ['Compare it against a cellpose run on the same image if you have one.'],
    },
    ...viewerCheck('mask', 'viewer.toggleLabels', [
      'Do the labels stay on the same cell as it moves?',
      'Is a moving cell one object, or does it break up between frames?',
    ]),
    {
      title: 'What you now have',
      text: 'Labels and measures on a movie cellpose could not read — track them next.',
      bullets: ['Tracking is the point of segmenting a movie; gating works on these too.'],
    },
  ],
})

// Clustering ends with numbered clusters, which are not yet populations. Turning them into named
// populations is a distinct move with its own UI — no gate to draw, you create a population and tick
// cluster IDs into it — and it is the step that makes the result usable downstream, so both cluster
// guides end here.
const clusterToPops = (route: string, what: string): GuideStep[] => [
  {
    anchor: 'cluster.popManager',
    route,
    placement: 'bottom-end',
    title: 'Clusters are numbers, not populations yet',
    text: 'Open the population manager — this is where numbered clusters become named groups.',
    clickAnchor: true,
  },
  {
    anchor: 'popmanager.addClusterPop',
    route,
    placement: 'left',
    title: 'Add a population',
    text: 'There is no gate to draw here — you create the population first, then fill it.',
    bullets: ['Name it for what it is: "patrolling", "CD4 T cell".'],
    // The chip row only exists once a population does, so its appearance is the signal.
    when: c => c.anchorExists('popmanager.clusterChips'),
  },
  {
    anchor: 'popmanager.clusterChips',
    route,
    placement: 'left',
    title: 'Tick clusters into it',
    text: `Each chip is one cluster — click to put it in this population.`,
    bullets: [
      'A cluster belongs to at most one population; ticking it elsewhere moves it.',
      'The heatmap is how you decide which clusters belong together.',
    ],
  },
  {
    anchor: 'popmanager.row',
    route,
    placement: 'left',
    title: 'Now it behaves like any population',
    text: `Your ${what} populations are usable everywhere a gated one is.`,
    bullets: [
      'Plot them on the analysis board, show them in the viewer, use them as an input.',
      'Populations are per clustering run — they follow that run\'s suffix.',
    ],
  },
]

// ── Cluster cells: a plain task run ──────────────────────────────────────────────────────────────
export const clusterCellsGuide = moduleTaskGuide({
  id: 'cluster-cells',
  title: 'Cluster cells into phenotypes',
  group: 'Populations',
  icon: 'pi-share-alt',
  summary: 'Group cells by their whole measure profile instead of gating two channels at a time.',
  funHint: ['Clustering CELLS — the track counterpart is its own page, and needs tracking first.'],
  route: '/clust-cells',
  navLabel: 'Cluster cells',
  taskKey: 'clusterCells',
  funName: 'clustPops.cluster',
  funLabel: 'Cluster cells',
  selectionModule: 'clustPops',
  waitLabel: 'Clustering',
  prereqs: [PREREQ.projectOpen, PREREQ.segmented],
  intro: 'Clustering finds cell types from all measures at once — the unsupervised counterpart to gating.',
  selectHint: ['Select every image you want clustered TOGETHER — the run pools across them.'],
  params: [
    'Populations — which cells to cluster; every selection is clustered jointly.',
    'Cluster on — the feature columns, usually the channel intensities.',
    'Resolution — the Leiden resolution; higher gives more, smaller clusters.',
    'Calculate UMAP — leave on, it is what the embedding plot draws.',
  ],
  after: [
    {
      anchor: 'layout.plotsSection',
      route: '/clust-cells',
      placement: 'top-start',
      title: 'UMAP plus heatmap',
      text: 'The UMAP shows how clusters separate; the heatmap says what each one actually expresses.',
      bullets: [
        'The heatmap is what turns "cluster 4" into "CD4 T cell".',
        'Read it before naming anything — it says what each cluster expresses.',
      ],
    },
    ...clusterToPops('/clust-cells', 'phenotype'),
  ],
})

// ── Cluster TRACKS: the same engine, a different table ────────────────────────────────────────────
// Clustering comes in two kinds and they are separate pages: cells (above, needs a segmentation) and
// tracks (here, needs TRACKING). Same Leiden/UMAP machinery, but the rows are tracks and the features
// are per-track aggregates, so a user who has only segmented cannot use this one.
export const clusterTracksGuide = moduleTaskGuide({
  id: 'cluster-tracks',
  title: 'Cluster tracks into behaviours',
  group: 'Populations',
  icon: 'pi-share-alt',
  summary: 'Group whole tracks by how they move, rather than grouping cells by what they express.',
  route: '/clust-tracks',
  navLabel: 'Cluster tracks',
  taskKey: 'clusterTracks',
  funName: 'clustTracks.cluster',
  funLabel: 'Cluster tracks',
  selectionModule: 'clustTracks',
  waitLabel: 'Clustering tracks',
  prereqs: [PREREQ.projectOpen, PREREQ.tracked],
  intro: 'One row per track instead of per cell — so this needs tracking, not just segmentation.',
  funHint: ['Cell measures are aggregated per track for you; you pick the base measures.'],
  selectHint: [
    'Select every image to cluster TOGETHER — the run pools across them.',
    'Needs measured tracks — the Track guide\'s function does both.',
  ],
  params: [
    'Track populations — which tracks to cluster; every selection is clustered jointly.',
    'Cluster on — base measures; cell measures are aggregated per track automatically.',
    'Minimum track length — drop tracks too short to characterise.',
    'Resolution — the Leiden resolution; higher gives more, smaller clusters.',
  ],
  after: [
    {
      anchor: 'layout.plotsSection',
      route: '/clust-tracks',
      placement: 'top-start',
      title: 'Clusters of movement',
      text: 'The UMAP separates behaviours; the heatmap says which measures define each one.',
      bullets: [
        'This answers "how many kinds of movement are in here", without naming them first.',
        'HMM states are the supervised alternative — fixed states, fitted per timepoint.',
      ],
    },
    ...clusterToPops('/clust-tracks', 'behaviour'),
  ],
})

// ── Behaviour states: a plain task run ───────────────────────────────────────────────────────────
export const behaviourStatesGuide = moduleTaskGuide({
  id: 'behaviour-states',
  title: 'Classify behaviour states',
  group: 'Explore',
  icon: 'pi-directions',
  summary: 'Fit an HMM to track movement so each cell gets an arrested / directed / meandering state.',
  route: '/behaviour',
  navLabel: 'Behaviour',
  taskKey: 'hmm',
  funName: 'behaviour.hmm',
  funLabel: 'HMM (states + transitions)',
  selectionModule: 'behaviourAnalysis',
  waitLabel: 'Fitting states',
  prereqs: [PREREQ.projectOpen, PREREQ.tracked],
  intro: 'A Gaussian HMM turns raw track movement into a small set of named behaviour states.',
  selectHint: [
    'Only tracked images qualify — the states are fitted to track measures.',
    'The tracking must have MEASURED — the Track guide\'s function does both.',
  ],
  params: [
    'Number of states — 3 is the usual starting point (arrested / meandering / directed).',
    'Which track measures to fit on — speed and angle are the standard pair.',
  ],
  after: [
    {
      anchor: 'layout.plotsSection',
      route: '/behaviour',
      placement: 'top-start',
      title: 'Read the states off the plots',
      text: 'The panel shows what each fitted state actually looks like, so you can name them.',
      bullets: [
        'A state is only meaningful once you have seen its speed profile.',
        'Fit landed oddly? Change the number of states and re-run.',
      ],
    },
    {
      anchor: 'sidebar.viewerCta',
      placement: 'right',
      title: 'Colour tracks by state',
      text: 'The Viewer panel can colour tracks by the new state column.',
      bullets: ['That is the quickest sanity check that the states mean something.'],
    },
  ],
})

// ── Preprocess (crop today) — the module page users find between import and cleanup ──────────────
// Crop is what ships today under `editImages`; MIP/bin/resample are peers in the same module and
// will show up in the same dropdown as they land. Deliberately not covered by fixMetadata: metadata
// edits are in-place, this one writes a NEW image version.
export const preprocessImagesGuide = moduleTaskGuide({
  id: 'preprocess-images',
  title: 'Preprocess an image',
  group: 'Data',
  icon: 'pi-crop',
  summary: 'Crop / project / resample raw images before you segment — writes a new version, keeps the original.',
  route: '/preprocess',
  navLabel: 'Preprocessing',
  taskKey: 'cropImage',
  funName: 'editImages.cropImage',
  funLabel: 'Crop image',
  selectionModule: 'preprocess',
  waitLabel: 'Cropping',
  withPreview: true,
  prereqs: [PREREQ.projectOpen, PREREQ.imageImported],
  intro: 'Preprocess sits between import and cleanup — trim the region before every downstream step reads the full one.',
  funHint: [
    'Crop is what this guide walks; MIP, bin, resample and dtype sit beside it in the same dropdown.',
    'Every preprocess writes a NEW image version — nothing overwrites your import.',
  ],
  params: [
    'Source image — which version of this image to crop; defaults to the active one.',
    'Crop area — draw a box on the preview; z/t unset = keep the whole axis.',
  ],
  after: [
    {
      anchor: 'images.table',
      route: '/preprocess',
      placement: 'top-start',
      title: 'It made a new version',
      text: 'The row picks up an extra version — the info icon lists every version this image has.',
      bullets: [
        'Everything downstream reads the ACTIVE version.',
        'Nothing is destroyed — flip back by making the original active.',
      ],
    },
  ],
})

// ── Cluster regions — spatial neighbourhoods, not cells. SET-SCOPE so IDs are cross-image ────────
// Same engine as `cluster-cells` (Leiden + UMAP + popmanager) but the rows are neighbourhoods.
// Ends with the shared `clusterToPops` tail — a numbered region becomes a named population.
export const clusterRegionsGuide = moduleTaskGuide({
  id: 'cluster-regions',
  title: 'Cluster spatial regions',
  group: 'Populations',
  icon: 'pi-map-marker',
  summary: 'Group spatial neighbourhoods — what surrounds each cell — into named regions you can plot and gate.',
  route: '/regions',
  navLabel: 'Cluster regions',
  taskKey: 'clusterRegions',
  funName: 'clustRegions.cluster',
  funLabel: 'Cluster regions',
  selectionModule: 'clustRegions',
  waitLabel: 'Clustering regions',
  prereqs: [PREREQ.projectOpen, PREREQ.segmented],
  intro: 'Region clustering groups NEIGHBOURHOODS ("what surrounds each cell"), not the cells themselves.',
  funHint: [
    'Needs a Neighbour graph — run Spatial → Neighbour graph first if you have not.',
    'SET-SCOPE — every selected image is clustered jointly, so region IDs are comparable across them.',
  ],
  selectHint: ['Select every image you want clustered TOGETHER — the composition vectors pool across them.'],
  params: [
    'Neighbour graph — the graph a Spatial run produced; pick one to cluster over.',
    'Population basis — the populations whose mix defines each neighbourhood\'s composition vector.',
    'Suffix — output name for this run; you pick it again when reading the regions downstream.',
  ],
  after: [
    {
      anchor: 'layout.plotsSection',
      route: '/regions',
      placement: 'top-start',
      title: 'Read the regions',
      text: 'UMAP shows how the neighbourhoods separate; the heatmap says what each region is composed of.',
      bullets: [
        'Bright rows on the heatmap = the populations that define that region.',
        'Rename a region once its composition is clear — "T-cell rich", "vessel edge".',
      ],
    },
    ...clusterToPops('/regions', 'region'),
  ],
})

// ── Spatial analysis — interaction matrix + neighbour stats. IMAGE-scope ─────────────────────────
// Multiple tasks live in this module (neighbour graph, aggregates, contacts, interaction matrix);
// this guide teaches the interaction matrix because that is the readout users open the page for.
// The prerequisite Neighbour graph is a separate run — called out in funHint rather than as a
// prereq, since a matching graph exists on disk or it doesn't (no local state to check).
export const spatialAnalysisGuide = moduleTaskGuide({
  id: 'spatial-analysis',
  title: 'Score spatial interactions',
  group: 'Explore',
  icon: 'pi-share-alt',
  summary: 'Who is near whom, and how often — the interaction matrix and per-image contact stats.',
  route: '/spatial',
  navLabel: 'Spatial',
  taskKey: 'neighbourStats',
  funName: 'spatialAnalysis.neighbourStats',
  funLabel: 'Interaction matrix',
  selectionModule: 'spatialAnalysis',
  waitLabel: 'Scoring interactions',
  prereqs: [PREREQ.projectOpen, PREREQ.segmented],
  intro: 'Reads relationships between populations on the image — who is near whom, and how often.',
  funHint: [
    'Neighbour graph runs first — this task LOADS it, does not build it.',
    'Interaction matrix, Aggregates and Contacts all live in this dropdown; this guide teaches the matrix.',
  ],
  params: [
    'Neighbour graph — the graph a Neighbour-graph run produced; pick the one you want scored.',
    'Population basis — the pairs to score interactions between; at least two.',
    'Name — output name for this run; a later plot picks it up by this name.',
  ],
  after: [
    {
      anchor: 'layout.plotsSection',
      route: '/spatial',
      placement: 'top-start',
      title: 'Read the matrix',
      text: 'Log-odds heatmap: bright cells co-locate more than chance; dark ones avoid.',
      bullets: [
        'The permutation test tells you if a pattern is a real signal or the same cell types rearranged.',
        'Contacts and aggregates plot the same underlying graph in different shapes — worth a look next.',
      ],
    },
  ],
})
