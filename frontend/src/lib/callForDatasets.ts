/**
 * Capabilities Cecelia can build, or check, once a real dataset lands — the registry behind the
 * "Call for Datasets" modal (docs/todo/CALL_FOR_DATASETS_PLAN.md).
 *
 * **Two kinds, one registry** (plan → Decision 9). A `build` ask is a capability not built yet,
 * waiting only for data to build it against. A `test` ask is a module that exists but has only been
 * checked on one lab's data; a dataset with a known answer would CHECK it (the maturity map's
 * "Needs data" rows link here, docs/todo/FEATURE_MATURITY_PLAN.md). A build ask leaves the registry
 * once built; a test ask once its data is in a committed test.
 *
 * **Why this exists.** Some engineering decisions are unblocked as soon as a validation dataset
 * arrives — full 6-DOF 3D rigid drift correction only makes sense on a movie where tissue actually
 * tilts through Z, and a large-static-3D-registration extension of `editImages.register` only makes
 * sense on a real multi-cycle 3D stack. Shipping those speculatively risks trading small tilts
 * against noise on every clean movie, or worse. The modal turns "we could build this if we had
 * data" into a one-click GitHub-issue routing prefilled with the two things we cannot guess
 * (what the imaging looks like, and a cloud link to an example).
 *
 * **Deliberately narrow — this is not a wish-list.** An entry belongs here when engineering has
 * DECIDED the capability, and the only missing piece is a dataset to build against. Ideas that
 * need more design go into `docs/TODO.md` or a parked plan.
 *
 * **Same shape as `PARAM_FIGURES` / `PARAM_ADVISORS`** — a plain array of objects, unit-tested,
 * with the modal only mounting what it is handed.
 */
import { CECELIA_NEW_ISSUE_URL, CECELIA_REPO_URL } from './links'

/** The expanded row's schematics — one inline SVG each in `DatasetAskFigure.vue`. */
export const ASK_FIGS = ['tilt', 'cycles', 'clusters', 'bleed', 'masks', 'pair', 'swap', 'tracks', 'switch'] as const
export type AskFig = typeof ASK_FIGS[number]

/** `build` — not built, waiting on data to build against. `test` — built, waiting on data to check it. */
export type AskKind = 'build' | 'test'

export interface CapabilityAsk {
  kind: AskKind
  /** Stable slug — cited from `?ask=<id>` deep links and from the GitHub issue's URL. */
  id: string
  /** The collapsed row's heading — a short phrase naming the capability (≤40 chars, tested). */
  title: string
  /**
   * The why, as one phrase — sits beside the title on the collapsed row, so it must fit there
   * (≤50 chars, tested). Also the `get_guide` summary.
   */
  oneLiner: string
  /**
   * The longer paragraph — NOT in the modal (docs/ui/COPY.md).
   * Claude reads it through `get_guide` (`guideText.ts → datasetCallText`); the design detail lives in
   * the plan at `planPath`.
   */
  blurb: string
  /**
   * The data we need, in one line — the expanded row's text (≤110 chars, one sentence, tested).
   * The full version is `sceneryHint`.
   */
  need: string
  /**
   * The data we need, in full — goes into the GitHub issue body and `get_guide`, where a submitter
   * reads it once. Named literally rather than a category, because "3D rigid drift" is not the same
   * shape of ask as "large static 3D registration" and a category would let the two collapse into
   * one prompt on the issue form.
   */
  sceneryHint: string
  /** Its small schematic in the expanded row (`DatasetAskFigure.vue`), or none where a picture adds
   *  nothing. */
  fig?: AskFig
  /** The label the modal writes into the prefilled GitHub-issue title. Kept explicit rather than
   *  derived from `title`, because a title reads "Full 3D rigid drift" and the issue wants
   *  something more like "Call for dataset: full 3D rigid drift correction" (a test ask:
   *  "Test data: …"). */
  issueTitle: string
  /** Labels the modal writes into the prefilled GitHub issue. Always includes
   *  `call-for-datasets`; additional labels are per-ask (e.g. `area:drift`). */
  labels: string[]
  /**
   * Pointer to the parked plan or docs section that DESIGNED this (for a test ask, the audit that
   * named the gap) — a card is not the last word,
   * a plan is. Repo-relative or an absolute URL.
   */
  planPath: string
}

/** The audit whose test-set table (§5) names every test ask's gap. */
const TEST_SET_NEEDS = 'docs/todo/HARDENING_RESULT_CORRECTNESS.md'

/** A test ask: the shared labels and plan pointer filled in. */
function testAsk(id: string, area: string,
              a: Pick<CapabilityAsk, 'title' | 'oneLiner' | 'blurb' | 'need' | 'sceneryHint' | 'issueTitle' | 'fig'>): CapabilityAsk {
  return { kind: 'test', id, ...a, labels: ['call-for-datasets', 'test-data', area], planPath: TEST_SET_NEEDS }
}

export const CALL_FOR_DATASETS: CapabilityAsk[] = [
  {
    kind: 'build',
    id: 'sitk-rigid-3d-full',
    title: '3D drift with tilting tissue',
    oneLiner: 'Drift where the sample tilts through Z',
    need: 'A 3D timelapse where a Z-plane at the start is a different plane later, not just shifted.',
    fig: 'tilt',
    blurb:
      "The `sitkRigid` estimator today handles in-plane rotation (the whole 3D volume rotates " +
      "around the optical axis) — the case a bumped microscope stage actually produces. Full " +
      "6-DOF rigid would let us catch a preparation where the coverslip loosened over the run, " +
      "or intravital tissue tilting from breathing / heartbeat. We haven't shipped it because " +
      "letting the fit try X/Y rotations on a clean movie trades small tilts against noise, and " +
      "a real dataset with genuine tilting is what tells us whether that trade is worth it.",
    sceneryHint:
      'A 3D confocal or intravital timelapse where a Z-slice at t=0 corresponds to a DIFFERENT ' +
      'plane at t=N (not just a shifted plane). If your movie only translates and rotates ' +
      "in-plane, `sitkRigid` already handles it.",
    issueTitle: 'Call for dataset: full 3D rigid drift correction',
    labels: ['call-for-datasets', 'area:drift'],
    planPath: 'docs/todo/DRIFT_RIGID_PLAN.md',
  },
  {
    kind: 'build',
    id: 'static-3d-registration',
    title: 'Large static 3D registration',
    oneLiner: 'Align 3D staining cycles or tiles',
    need: 'A multi-cycle 3D acquisition (CODEX, IBEX or similar), or 3D tiles that need stitching.',
    fig: 'cycles',
    blurb:
      "`editImages.register` aligns 2D staining cycles (whole-slide scans on their pyramid). A 3D extension would " +
      "let a multi-round staining protocol be registered across cycles in 3D, or align a set of " +
      "3D tiles into one volume. The path is straightforward (SimpleITK does this natively), but " +
      "we've never had a dataset to profile against — the cost-vs-quality trade of the metric / " +
      "pyramid choice needs to be measured on a real store.",
    sceneryHint:
      'A multi-cycle 3D acquisition (e.g. CODEX / MERFISH / IBEX) or a set of 3D tiles that need ' +
      'stitching.',
    issueTitle: 'Call for dataset: large static 3D registration',
    labels: ['call-for-datasets', 'area:registration'],
    planPath: 'docs/audit/simpleitk-opportunities.md',
  },
  {
    kind: 'build',
    id: 'cluster-grouping-highplex',
    title: 'Group many clusters with Claude',
    oneLiner: 'Claude proposes groups; you accept or rename',
    need: 'A 10+ marker image that clusters into 10+ groups, and your own grouping if you have one.',
    fig: 'clusters',
    blurb:
      "Clustering a high-plex panel often gives tens of clusters, and it is hard to see which belong " +
      "together. Claude can already read each cluster's marker profile and suggest groupings in chat; " +
      "the next step is letting it write a proposed population (marked ✦) with the marker evidence " +
      "beside it, for you to accept, rename or delete. We haven't built it because our own data " +
      "clusters into a handful of groups, and a dataset with many is what tells us whether the " +
      "proposals hold up.",
    sceneryHint:
      'A multiplex image (roughly 10+ markers) whose cell clustering gives 10+ clusters — and, if you ' +
      'have it, how you would group and name them yourself, so the proposals can be checked against it.',
    issueTitle: 'Call for dataset: grouping many clusters (high-plex)',
    labels: ['call-for-datasets', 'area:clustering'],
    planPath: 'docs/todo/CLUSTER_POP_PROPOSALS_PLAN.md',
  },

  // ── Test asks: the module exists; data with a known answer would check it. Each is one "Needs data"
  //    line on the maturity map (lib/guides/maturityRecords.ts, unguided.ts). The gap each closes is
  //    §5 of the audit below.
  testAsk('vendor-calibration', 'area:import', {
    title: 'Vendor files with known calibration',
    oneLiner: 'Pixel size and frame interval, other vendors',
    need: '3–4 tiny vendor files (LIF, CZI, ND2, IMS) with the pixel size and frame interval you set.',
    blurb:
      "Import has only been checked on one lab's 2-photon files, timed in seconds with pixels in µm. " +
      'Small files with a calibration you know would show whether other vendors and units read right.',
    sceneryHint:
      '3–4 tiny files (LIF, CZI, ND2, IMS or similar), a few frames each, with the pixel size and ' +
      'frame interval you set. At least one timed in ms or min, or with pixels in nm.',
    issueTitle: 'Test data: vendor files with known calibration',
  }),
  testAsk('single-stain-controls', 'area:correction', {
    title: 'Single-stained controls',
    oneLiner: 'Bleedthrough correction on a measured leak',
    need: 'Two channels on a single-labelled sample and on a double-labelled one, fluorophores named.',
    fig: 'bleed',
    blurb:
      'The correction is tested on a leak we inject ourselves. A real single-stain control from ' +
      'another microscope gives the leak a known size to recover.',
    sceneryHint:
      'A small 2-channel image of a sample carrying only ONE of the two labels, plus the same two ' +
      'channels on a double-labelled sample. Name the fluorophores and the filter set.',
    issueTitle: 'Test data: single-stained controls for bleedthrough correction',
  }),
  testAsk('annotated-3d-stack', 'area:segmentation', {
    title: '3D stack with hand-drawn masks',
    oneLiner: 'Score segmentation against hand outlines',
    need: 'A small 3D stack (e.g. 256×256×30) with every cell outlined by hand as a label image.',
    fig: 'masks',
    blurb:
      "Segmentation has been checked by eye on intravital movies, which have no ground truth. A stack " +
      'with every cell outlined lets a test count cells and score overlap.',
    sceneryHint:
      'A small fixed-tissue or cultured-cell 3D stack (e.g. 256×256×30) with each cell outlined by ' +
      'hand as a label image. Touching cells, a dim region and a few empty planes help.',
    issueTitle: 'Test data: 3D stack with hand-annotated masks',
  }),
  testAsk('paired-snr', 'area:denoise', {
    title: 'Paired noisy and clean images',
    oneLiner: 'What denoising removes, against a clean image',
    need: 'One field imaged twice without moving: fast or low-power, then averaged or slow.',
    fig: 'pair',
    blurb:
      "Denoised output has not been checked against anything. The same field imaged noisy and clean " +
      'is the reference that shows whether it keeps real structure.',
    sceneryHint:
      'One field acquired twice without the sample moving: fast or low-power (noisy), then averaged ' +
      'or slow (clean). 2D or 3D; a short timelapse is fine.',
    issueTitle: 'Test data: paired low- and high-SNR acquisitions',
  }),
  testAsk('swapped-channel-set', 'area:gating', {
    title: 'Channels in a different order',
    oneLiner: 'A copied gate must not land on the wrong marker',
    need: 'Two small images from one experiment whose channel order differs, markers named.',
    fig: 'swap',
    blurb:
      'Our sets are acquired with one channel order throughout, so a copy or a pooled set across ' +
      'a reordered channel has never been exercised on real data.',
    sceneryHint:
      'Two small images from one experiment whose channel order differs (or one channel is renamed), ' +
      'with the markers named in each.',
    issueTitle: 'Test data: image set with swapped channel order',
  }),
  testAsk('ground-truth-tracks', 'area:tracking', {
    title: 'A movie with ground-truth tracks',
    oneLiner: 'Score tracking against hand-made links',
    need: 'A cell timelapse with hand-linked tracks, ideally with a division (Cell Tracking Challenge style).',
    fig: 'tracks',
    blurb:
      'Tracking is checked by eye; no test scores its links. A movie whose tracks were linked by hand ' +
      'lets us measure how often a link is wrong.',
    sceneryHint:
      'A 2D or 3D cell timelapse with hand-linked tracks, ideally with a division and a cell that ' +
      'leaves and returns — a Cell Tracking Challenge-style set you have used works.',
    issueTitle: 'Test data: movie with ground-truth tracks',
  }),
  testAsk('behaviour-switch-tracks', 'area:behaviour', {
    title: 'Tracks with known behaviour switches',
    oneLiner: 'Behaviour states against switches you marked',
    need: 'Tracks, as a movie or a table, where you marked when cells changed behaviour.',
    fig: 'switch',
    blurb:
      'Behaviour states, motifs and track clusters run on real tracks, but nothing checks them against ' +
      'a known answer. Tracks whose switches are annotated are that answer.',
    sceneryHint:
      'A tracked movie, or the tracks as a table, where you marked when cells changed behaviour — ' +
      'e.g. arrested on contact, then migrated on.',
    issueTitle: 'Test data: tracks with known behaviour switches',
  }),
  testAsk('annotated-niches', 'area:spatial', {
    title: 'Tissue with annotated niches',
    oneLiner: 'Spatial regions against ones an expert drew',
    need: 'A 6+ marker tissue section with regions such as follicles or a tumour border outlined.',
    blurb:
      'Region clustering and interaction scores have only been checked on few-marker data. A ' +
      'multiplex section with drawn niches shows whether they find the regions that are there.',
    sceneryHint:
      'A multiplex tissue section (roughly 6+ markers) with regions such as follicles, T-cell zones or ' +
      'a tumour border outlined by an expert, as a mask or shapes.',
    issueTitle: 'Test data: tissue section with annotated niches',
  }),
]

/**
 * The prefilled GitHub-issue URL for one ask — mirrors `recipeRequestUrl` in `links.ts`. The body
 * template fronts the two questions we cannot guess (what the imaging looks like; a cloud link to
 * a sample) so an issue opened via the modal lands in a shape we can act on, without a triage
 * round trip.
 */
export function datasetAskUrl(ask: CapabilityAsk): string {
  const body =
    `## ${ask.kind === 'test' ? 'What it would test' : 'Capability'}\n\n${ask.title}\n\n` +
    `Plan: [${ask.planPath}](${planUrl(ask)})\n\n` +
    `## The data that would help\n\n${ask.sceneryHint}\n\n` +
    `## What does the imaging look like?\n\n` +
    `(Modality, ~size, dimensionality, per-timepoint content — one paragraph is enough)\n\n` +
    `## Cloud link to a sample\n\n` +
    `(Google Drive / Zenodo / an S3 bucket / anywhere a maintainer can pull it from. A subset is fine — enough to reproduce the scenario, not the whole cohort.)\n\n` +
    `## Anything else worth naming\n\n` +
    `(Ownership / redistribution constraints, related issues, prior discussions.)\n`
  const q = new URLSearchParams({
    title: ask.issueTitle,
    labels: ask.labels.join(','),
    body,
  })
  return `${CECELIA_NEW_ISSUE_URL}?${q}`
}

/** The plan on GitHub — the expanded row's small Plan link and the issue body's. A repo-relative
 *  `planPath` points at `main`; an absolute URL is used as is. */
export function planUrl(ask: CapabilityAsk): string {
  return /^https?:\/\//.test(ask.planPath) ? ask.planPath : `${CECELIA_REPO_URL}/blob/main/${ask.planPath}`
}

/** The asks of one kind, in registry order — the modal's two sections. */
export function asksOfKind(kind: AskKind): CapabilityAsk[] {
  return CALL_FOR_DATASETS.filter(a => a.kind === kind)
}

/** Look up one ask by id, or null. Used by the modal's deep-link scroll (`?ask=<id>`). */
export function findAsk(id: string | null | undefined): CapabilityAsk | null {
  if (!id) return null
  return CALL_FOR_DATASETS.find(a => a.id === id) ?? null
}
