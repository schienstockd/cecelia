// The viewer control panel's Movie section with "What: ROIs" (docs/todo/ROI_TRIAGE_PLAN.md Decision 12)
// — the pure half. Which ROIs a record or a figure takes (the ROI list's current set + status filter,
// capped), the `rois` block a `movie:record` request carries (the server reads each ROI's geometry
// from the set — `api/src/movie_rois.jl`), and an ROI's figure spec (its box as the square, its
// planes, frames from its window).

import { filterRois, type RoiRow, type RoiStatus } from './rois'
import { figureFrames, type FigureSpec } from './figureSpec'

export type RecordWhat = 'view' | 'rois'
export const RECORD_WHAT_DEFAULT: RecordWhat = 'view'
/** How many ROIs one record takes at most, by default — a sheet of more than ~16 tiles is unreadable. */
export const ROI_MAX_DEFAULT = 12
/** The least an ROI figure shows around its box, in image pixels — twin of the movie's
 *  `ROI_CONTEXT_MIN_PX` (api/src/movie_rois.jl); keep the two equal. */
export const ROI_CONTEXT_MIN_PX = 64
/** Frames either side of an ROI's window a movie shows by default, so the event has a before and after. */
export const ROI_PAD_FRAMES = 2
/** The most frames of context either side a record takes — enough to see what came before and after. */
export const ROI_PAD_MAX = 60

/** What one ROI record writes: a movie per ROI, those plus a contact sheet, or only the sheet. */
export type RoiOutput = 'movies' | 'both' | 'sheet'
export const ROI_OUTPUT_DEFAULT: RoiOutput = 'movies'

/** A typed pad, clamped to 0…ROI_PAD_MAX whole frames; blank or nonsense = the default. */
export function roiPadFrames(pad: number | null | undefined): number {
  return typeof pad === 'number' && Number.isFinite(pad) ? Math.min(ROI_PAD_MAX, Math.max(0, Math.round(pad))) : ROI_PAD_FRAMES
}

/**
 * The ROIs to record: the ones TICKED in the ROI list, or with none ticked, the first `max` it shows.
 * Either way only rows the status filter shows, in the list's order — a tick hidden by the filter
 * doesn't count, so the record is never of something you can't see.
 */
export function roiRecordSelection(rows: readonly RoiRow[], status: RoiStatus | 'all', max: number,
                                   checked: readonly string[] = []): RoiRow[] {
  const shown = filterRois(rows, status)
  if (checked.length) { const on = new Set(checked); return shown.filter(r => on.has(r.id)) }
  const n = Number.isFinite(max) && max > 0 ? Math.floor(max) : ROI_MAX_DEFAULT
  return shown.slice(0, n)
}

/** The key the ticked ROIs are kept under — per image and ROI set ('' = the image's default set). */
export const roiCheckedKey = (imageUid: string, suffix: string) => `${imageUid}/${suffix}`

/** The `rois` block of a `movie:record` request: the set, the ids in order, the sheet (and whether it is
 *  the only output — one ROI has no sheet, so it is recorded as its movie), and the frames of context. */
export function roiRecordBlock(suffix: string, rows: readonly RoiRow[], output: RoiOutput, pad?: number | null):
  { suffix: string, ids: string[], sheet: boolean, sheetOnly: boolean, padFrames: number } {
  const sheet = output !== 'movies' && rows.length > 1
  return { suffix, ids: rows.map(r => r.id), sheet, sheetOnly: sheet && output === 'sheet', padFrames: roiPadFrames(pad) }
}

/**
 * The figure of one ROI, from the figure the view would make (`figureSpecFromView` — its channels,
 * contrast and layout): a square about the box's centre with context around it, its planes as the z window,
 * and every `every`th frame of its time window widened by `pad` frames each side, inside the movie's
 * `nT` frames (null = auto, as Save as figure picks).
 */
export function roiFigureSpec(base: FigureSpec, roi: RoiRow, every: number | null | undefined,
                              pad: number | null = 0, nT = Infinity): FigureSpec {
  const b = roi.box
  const cy = Math.round((b.y0 + b.y1) / 2), cx = Math.round((b.x0 + b.x1) / 2)
  // context, as the movie has: twice the box's longer side, at least ROI_CONTEXT_MIN_PX
  const sizePx = Math.max(ROI_CONTEXT_MIN_PX, 2 * Math.max(b.y1 - b.y0 + 1, b.x1 - b.x0 + 1))
  return { ...base, center: [cy, cx], sizePx,
           z: Math.round((b.z0 + b.z1) / 2), zHalf: Math.max(0, Math.round((b.z1 - b.z0) / 2)),
           frames: figureFrames(Math.max(0, roi.t0 - (pad ?? 0)), Math.min(nT - 1, roi.t1 + (pad ?? 0)), every).frames }
}
