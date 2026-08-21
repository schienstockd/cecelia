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

import { moduleTaskGuide } from './moduleTask'
import { PREREQ } from './prereqs'
import type { GuideStep } from './types'

// Where segmentation and tracking both end up: the mask/tracks are only trustworthy once you have
// LOOKED at them, and looking happens in napari — a separate window this guide cannot point into
// (plan R1). So we point at the control that puts them on screen and say what to look for.
const napariCheck = (what: string, toggleAnchor: string, lookFor: string[]): GuideStep[] => [
  {
    anchor: 'images.viewerBtn',
    placement: 'right',
    title: 'Open it in napari',
    text: 'The eye opens the image itself — the overlay is a separate switch.',
    bullets: ['napari is its own window, so bring it to the front.'],
    when: c => c.napariImageUid !== null,
  },
  {
    anchor: 'sidebar.viewerCta',
    placement: 'right',
    title: 'Open the Viewer panel',
    text: 'This panel drives the napari window — overlays, contrast, 3D, recording.',
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
    // segmentation has its own row here with a per-overlay switch, and until you flip it napari shows
    // the raw channels and it looks like the run did nothing (Dominik, 2026-08-12).
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
  // on measuring, so it teaches the function that does both (Dominik, 2026-08-12).
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
    ...napariCheck('mask', 'viewer.toggleLabels', [
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
  // and what track clustering and track gating read (Dominik, 2026-08-12).
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
    ...napariCheck('tracks', 'viewer.toggleTracks', [
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
// dependency is said in the copy — see WORKFLOW_RECIPES_PLAN P3 for the version that could gate on it.
export const trainFlowModelGuide = moduleTaskGuide({
  id: 'train-flow-model',
  title: 'Train a flow model',
  group: 'Data',
  icon: 'pi-sync',
  summary: 'Teach a model what moving cells look like in your own movies — the step before motion segmentation.',
  route: '/optical-flow',
  navLabel: 'Optical flow',
  taskKey: 'trainFlowModel',
  funName: 'opticalFlow.train',
  funLabel: 'Train flow model',
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
      route: '/optical-flow',
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
    ...napariCheck('mask', 'viewer.toggleLabels', [
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
