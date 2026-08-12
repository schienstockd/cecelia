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
// `maxSearchRadius`/`minTimepoints` (tracking/bayesian_tracking.json).

import { moduleTaskGuide } from './moduleTask'
import { PREREQ } from './prereqs'
import type { GuideStep } from './types'

// Where segmentation and tracking both end up: the mask/tracks are only trustworthy once you have
// LOOKED at them, and looking happens in napari — a separate window this guide cannot point into
// (plan R1). So we point at the control that puts them on screen and say what to look for.
const napariCheck = (what: string, lookFor: string[]): GuideStep[] => [
  {
    anchor: 'images.viewerBtn',
    placement: 'right',
    title: `Look at the ${what}`,
    text: `The eye opens the image in napari with its ${what} on top.`,
    bullets: lookFor,
  },
  {
    anchor: 'sidebar.viewerCta',
    placement: 'right',
    title: 'Viewer panel',
    text: 'This panel drives the napari window — overlays, contrast, 3D, recording.',
    bullets: ['napari is its own window, so bring it to the front to see the result.'],
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
  taskKey: 'cellposeSegment',
  funName: 'segment.cellpose',
  funLabel: 'Cellpose segmentation',
  selectionModule: 'segment',
  waitLabel: 'Segmenting',
  intro: 'Segmentation is the entry point: gating, tracking and clustering all read its output.',
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
    ...napariCheck('mask', [
      'Are single cells one label, or is a clump merged into one?',
      'Are the outlines on the cells, or offset from them?',
    ]),
    {
      text: 'Labels are in — every measure came with them.',
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
  taskKey: 'bayesianTracking',
  funName: 'tracking.bayesian_tracking',
  funLabel: 'Bayesian tracking',
  selectionModule: 'tracking',
  waitLabel: 'Tracking',
  prereqs: [PREREQ.projectOpen, PREREQ.timeSeries, PREREQ.segmented],
  intro: 'Tracking links labels across frames — so segment every timepoint before you come here.',
  selectHint: ['Each image needs a segmentation covering all of its frames.'],
  params: [
    'Segmentation — which label set to track; a gated population narrows it.',
    'Max search radius (µm) — the furthest a cell may move between frames (~20 for T cells).',
    'Allowed gaps — frames a track may go unobserved and still be joined.',
    'Minimum timepoints — drops tracks too short to mean anything.',
  ],
  after: [
    ...napariCheck('tracks', [
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
      text: 'Tracks are in — behaviour states are the usual next step.',
      title: 'What you now have',
      bullets: [
        'Every cell has a track_id and a position at each timepoint.',
        'Behaviour → HMM classifies track movement into states.',
      ],
    },
  ],
})
