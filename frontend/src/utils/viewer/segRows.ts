/**
 * The segmentation rows of "On the image" (docs/todo/VIEWER_ON_THE_IMAGE_PLAN.md, S2): which
 * segmentations get a row, which are in use, the one-mask-at-a-time visibility bag, the track flip and
 * the warnings a row carries. Pure; the components own the store writes.
 */
import type { LabelsDimMismatch } from '../../stores/viewer'

export interface SegRow {
  valueName: string
  /** has mask PIXELS (`labels`) — the mask eye needs them; tracks need only a cell table */
  masked: boolean
  dimMismatch: boolean
  shortLevels: boolean
  /** a running task is writing this store now (`useLivePreview`) — the row offers the ⚡ preview */
  live: boolean
  /** registered on the image (mask or table); a live-only row has nothing else to toggle yet */
  registered: boolean
}

/** One row per segmentation: masks first, then points-only tables (tracks, no mask), then stores a
 *  running task is writing that are not registered yet. A re-run to an existing name is one row. */
export function segRows(labelNames: string[], trackNames: string[],
                        dims: LabelsDimMismatch | null | undefined, liveNames: string[] = []): SegRow[] {
  const masks = new Set(labelNames)
  const live = new Set(liveNames)
  const mismatched = new Set(dims?.mismatched ?? [])
  const registered = [...labelNames, ...trackNames.filter(vn => !masks.has(vn))]
  const known = new Set(registered)
  return [...registered, ...liveNames.filter(vn => !known.has(vn))].map(valueName => ({
    valueName, masked: masks.has(valueName),
    dimMismatch: mismatched.has(valueName),
    shortLevels: !!dims?.shortLevels?.[valueName],
    live: live.has(valueName), registered: known.has(valueName),
  }))
}

/** A row worth showing without being asked: its mask or its tracks are on, or a run is writing it. */
export const segRowActive = (r: SegRow, labelVis: Record<string, boolean>, trackVis: Record<string, boolean>) =>
  !!(r.live || labelVis[r.valueName] || trackVis[r.valueName])

/**
 * The label-visibility bag that shows only `valueName`'s mask (`null` = none): the viewer draws one
 * mask at a time. Every name gets an explicit boolean, so no other mask reads as still ticked — a
 * first open with no stored bag reads an unknown name as visible (`settings.getLabelVisibility`).
 */
export function onlyMask(names: Iterable<string>, valueName: string | null): Record<string, boolean> {
  const bag: Record<string, boolean> = {}
  for (const n of names) bag[n] = n === valueName
  if (valueName) bag[valueName] = true
  return bag
}

/** `onlyMask` after flipping `valueName`'s mask: on (and every other off), or none. */
export function flipMaskVisibility(names: Iterable<string>, current: Record<string, boolean>,
                                   valueName: string): Record<string, boolean> {
  return onlyMask(new Set([...names, ...Object.keys(current)]), current[valueName] ? null : valueName)
}

/** Why a mask that does not fit this image version is not drawn. */
export function mismatchTip(dims: LabelsDimMismatch | null | undefined, vn: string): string {
  const d = dims?.byVn?.[vn]
  if (!dims || !d) return "Mask doesn't match this image version — pick a matching version, or re-segment"
  return `Mask is ${d.nX}×${d.nY}, image version is ${dims.imageNX}×${dims.imageNY} — pick a matching version, or re-segment`
}

/** A mask with fewer zoom levels than the image: it draws only where it has a level. */
export function shortLevelsTip(dims: LabelsDimMismatch | null | undefined, vn: string): string {
  const d = dims?.shortLevels?.[vn]
  const n = d ? `${d.nLevels} of ${d.imageLevels}` : 'too few'
  return `Mask has ${n} zoom levels — zoom in to see it, or re-run the task that made it`
}

/** The track-visibility bag after flipping `valueName`'s tracks. */
export const flipTracks = (current: Record<string, boolean>, valueName: string): Record<string, boolean> =>
  ({ ...current, [valueName]: !current[valueName] })
