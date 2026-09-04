/**
 * Capabilities Cecelia can build once a real dataset lands — the registry behind the "Call for
 * Datasets" modal (docs/todo/CALL_FOR_DATASETS_PLAN.md).
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
import { CECELIA_NEW_ISSUE_URL } from './links'

export interface CapabilityAsk {
  /** Stable slug — cited from `?ask=<id>` deep links and from the GitHub issue's URL. */
  id: string
  /** Card heading — a short verb phrase naming the capability. */
  title: string
  /**
   * One line naming what the capability would unlock. Renders under the title. Kept to one line
   * so a reader scanning the modal picks up the "what would this do for me" without expanding.
   */
  oneLiner: string
  /**
   * Longer paragraph. The half of the modal that persuades a reader with a matching dataset that
   * their data is what would move this. One short paragraph — no marketing copy.
   */
  blurb: string
  /**
   * What kind of dataset would let us build this. Named literally rather than a category, because
   * "3D rigid drift" is not the same shape of ask as "large static 3D registration" and a category
   * would let the two collapse into one prompt on the issue form.
   */
  sceneryHint: string
  /** The label the modal writes into the prefilled GitHub-issue title. Kept explicit rather than
   *  derived from `title`, because a title reads "Full 3D rigid drift" and the issue wants
   *  something more like "Call for dataset: full 3D rigid drift correction". */
  issueTitle: string
  /** Labels the modal writes into the prefilled GitHub issue. Always includes
   *  `call-for-datasets`; additional labels are per-ask (e.g. `area:drift`). */
  labels: string[]
  /**
   * Pointer to the parked plan or docs section that DESIGNED this — a card is not the last word,
   * a plan is. Repo-relative or an absolute URL.
   */
  planPath: string
}

export const CALL_FOR_DATASETS: CapabilityAsk[] = [
  {
    id: 'sitk-rigid-3d-full',
    title: 'Full 3D rigid drift (X + Y tilting)',
    oneLiner: 'Correct drift on a movie whose sample TILTS through Z, not just rotates in-plane.',
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
    id: 'static-3d-registration',
    title: 'Large static 3D registration',
    oneLiner: 'Align a multi-tile or multi-cycle 3D stack the way `editImages.register` aligns 2D.',
    blurb:
      "`editImages.register` uses sitkibex for 2D staining-cycle alignment. A 3D extension would " +
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
]

/**
 * The prefilled GitHub-issue URL for one ask — mirrors `recipeRequestUrl` in `links.ts`. The body
 * template fronts the two questions we cannot guess (what the imaging looks like; a cloud link to
 * a sample) so an issue opened via the modal lands in a shape we can act on, without a triage
 * round trip.
 */
export function datasetAskUrl(ask: CapabilityAsk): string {
  const body =
    `## Capability\n\n${ask.title}\n\n` +
    `Plan: [${ask.planPath}](${ask.planPath})\n\n` +
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

/** Look up one ask by id, or null. Used by the modal's deep-link scroll (`?ask=<id>`). */
export function findAsk(id: string | null | undefined): CapabilityAsk | null {
  if (!id) return null
  return CALL_FOR_DATASETS.find(a => a.id === id) ?? null
}
