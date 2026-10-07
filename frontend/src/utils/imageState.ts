// What an image already HAS, answered from the `CciaImage` the project store holds — no request.
// Shared by the guide prerequisites (lib/guides/prereqs.ts, "does any image in the set…") and the
// task-discovery checks (utils/taskDiscovery.ts `TASK_CHECKS`, "which of the selected images…"), so the two never hold
// two definitions of "segmented". STATE, not provenance: each reads what is on disk / in ccid.json,
// never the run log — a migrated project has data with no run-log entry (see prereqs.ts `tracked`).
import type { CciaImage } from '../stores/project'
import { imageAxes, imageScales } from './taskGating'

// Axes and scales come from taskGating's `imageAxes` / `imageScales` — the twins of Julia's img_axes /
// img_scale_axes — so a check and the task gate can never disagree about the same image.

/** A T axis (more than one timepoint). */
export const isTimeSeries = (i: CciaImage) => imageAxes(i).has('T')

/** A Z axis (more than one plane). */
export const isZStack = (i: CciaImage) => imageAxes(i).has('Z')

/** At least one segmentation (`labels`). */
export const isSegmented = (i: Pick<CciaImage, 'labels'>) => Object.keys(i.labels ?? {}).length > 0

/** At least one `{vn}__tracks.h5ad` — written by Track measures, so this is "tracks with motility
 *  measures", not merely "linked". */
export const hasMeasuredTracks = (i: Pick<CciaImage, 'trackValueNames'>) => (i.trackValueNames ?? []).length > 0

/** A drift-corrected version (the `driftCorrected` value name Drift correction writes). */
export const isDriftCorrected = (i: Pick<CciaImage, 'filepaths'>) => !!i.filepaths?.driftCorrected

/** A spatial neighbour graph (spatialAnalysis.cellNeighbours). */
export const hasNeighbourGraph = (i: Pick<CciaImage, 'spatialGraphs'>) => Object.keys(i.spatialGraphs ?? {}).length > 0

/** A recorded XY pixel size (both X and Y). */
export const hasPixelSize = (i: CciaImage) => imageScales(i).has('XY')

/** A recorded Z step. */
export const hasZStep = (i: CciaImage) => imageScales(i).has('Z')
