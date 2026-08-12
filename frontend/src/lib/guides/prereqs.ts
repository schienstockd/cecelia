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
import { funsRun } from '../../utils/runLog'

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

  // Import converts to OME-Zarr in the background; until that lands the image can't be read, so
  // "there is a row in the table" is not the same as "there is something to work on".
  imageImported: {
    id: 'imageImported',
    label: 'an image that finished importing',
    ok: c => c.images.some(i => i.status === 'done'),
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

  // Tracks are not a field on the image — the run log is the single source of truth for "what has
  // been done to this image" (utils/runLog.ts), so ask it rather than adding a status attribute.
  tracked: {
    id: 'tracked',
    label: 'a tracked image',
    ok: c => c.images.some(i => [...funsRun(i.runLog)].some(f => f.startsWith('tracking.'))),
    fixGuide: 'track-cells',
  },
} satisfies Record<string, Prereq>
