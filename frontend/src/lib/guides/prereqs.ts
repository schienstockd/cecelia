// What a guide needs before it can usefully point at anything (plan D6).
//
// Guides run on the user's own data — there is no demo project — so the picker has to say up front
// what a guide assumes, check it, and offer the guide that gets you there. Every predicate here is
// answerable from state the frontend ALREADY holds (`CciaImage` + its `runLog`, both in the project
// store), so opening the picker costs no requests.
//
// The bar for adding one: it must be derivable locally and it must be worth blocking on. A prereq
// that needs a fetch belongs in the guide's prose instead — a picker that hits the network to grey
// out a row is a picker that hangs.

import type { Prereq } from './types'
import { isImported } from '../../utils/inclusion'

// A short label reads as the tail of "This guide needs …" — so no leading capital, no full stop.
export const PREREQ = {
  // Deliberately NO `fixGuide`: no guide can open a project for you, and pointing at the import guide
  // would be circular (it needs a project too — the ratchet in guides.test.ts caught exactly that).
  // "needs an open project" is self-explanatory, and the sidebar's project block is right there.
  projectOpen: {
    id: 'projectOpen',
    label: 'an open project',
    ok: c => c.hasProject,
  },

  setHasImages: {
    id: 'setHasImages',
    label: 'at least one image in the current set',
    ok: c => c.images.length > 0,
    fixGuide: 'import-images',
  },

  // "There is a row in the table" is not "there is something to work on": adding files registers a
  // row, and converting to OME-Zarr is a separate run.
  //
  // Uses the canonical `isImported` (does the image HAVE a converted file) rather than the hand-rolled
  // `status === 'done'` this shipped with. `status` is the transient conversion-job state and is not a
  // reliable record of the outcome — the image table's Status column shows the per-MODULE task status
  // and reads "—" for an image with none, and `isImported` is what the table itself uses to decide
  // whether the napari eye is enabled. A second definition of "imported" meant the picker declared this
  // missing for a project full of perfectly good images (Dominik, 2026-08-12).
  imageImported: {
    id: 'imageImported',
    label: 'an image that finished importing',
    ok: c => c.images.some(isImported),
    fixGuide: 'import-images',
  },

  // Drift correction and tracking are meaningless on a single frame.
  timeSeries: {
    id: 'timeSeries',
    label: 'a time series (more than one frame)',
    ok: c => c.images.some(i => (i.sizeT ?? 1) > 1),
  },

  // Segmentation is the entry point for everything downstream, and its output is what `labels` holds.
  segmented: {
    id: 'segmented',
    label: 'a segmented image',
    ok: c => c.images.some(i => Object.keys(i.labels ?? {}).length > 0),
    fixGuide: 'segment-an-image',
  },

  // Ask whether the TRACKS EXIST, not whether a tracking task was recorded. `trackValueNames` is the
  // `{vn}__tracks.h5ad` sidecars on disk (backend: img_track_value_names), which is exactly what the
  // track-grained consumers downstream read.
  //
  // This shipped as a run-log scan for `tracking.*` and was wrong on real data: a project migrated
  // from the R version — or tracked before the run log existed — has no `tracking.*` entry at all, so
  // the picker declared "needs a tracked image" over a project whose tracks were sitting on disk and
  // already clustered (Dominik, project 4kS67f). The run log records PROVENANCE; a prereq is asking
  // about STATE, and the two are not interchangeable for data that predates the log. Second time this
  // exact substitution bit — see `imageImported` above.
  tracked: {
    id: 'tracked',
    label: 'a tracked image',
    ok: c => c.images.some(i => (i.trackValueNames ?? []).length > 0),
    fixGuide: 'track-cells',
  },
} satisfies Record<string, Prereq>
