// Task-applicability gate — the frontend twin of Cecelia.task_applies / img_axes / img_scale_axes
// (Julia).
//
// Task JSON declares `requires.axes` and `requires.scale`; every consumer that offers a task to the
// user (module-page task picker, chain-module task picker) MUST ask this util before showing/enabling
// the task, so a task that needs a T axis never appears runnable for a static image, and one that
// measures in microns never appears runnable for an image with no pixel size. The backend gate raises
// TaskApplicabilityError anyway — the frontend gate just removes the surprise.
//
// The two halves fail differently and that shows in the copy. A missing AXIS is a fact about the
// image and nothing can be done about it; a missing SCALE is metadata the user can fill in, so its
// message points at where.
//
// Composite tasks: the server merges sub-task `requires.axes` into the composite's own field
// (api/src/routes.jl → api_task_definitions), so this util treats every def flat — no recursion.

import type { TaskDef } from '../tasks/types'
import type { CciaImage } from '../stores/project'

export type Axis = 'T' | 'Z' | 'C' | 'Y' | 'X'

/** The set of non-trivial axes an image carries. Mirrors Cecelia.img_axes (Julia). */
export function imageAxes(img: CciaImage): Set<Axis> {
  const axes = new Set<Axis>(['X', 'Y'])
  if ((img.sizeT ?? 1) > 1) axes.add('T')
  if ((img.sizeZ ?? 1) > 1) axes.add('Z')
  if ((img.sizeC ?? 1) > 1) axes.add('C')
  // Pre-SizeT imports still carry timeIncrement when the source was a timelapse.
  if (!axes.has('T') && img.timeIncrement != null && img.timeIncrement !== 0) axes.add('T')
  return axes
}

export type ScaleAxis = 'XY' | 'Z' | 'T'

/**
 * Which physical scales the image RECORDS. Mirrors Cecelia.img_scale_axes (Julia): `XY` needs both
 * pixel sizes, `Z` needs the z spacing, `T` needs the frame interval — each present and > 0, because
 * a zero is not a measurement.
 */
export function imageScales(img: CciaImage): Set<ScaleAxis> {
  const out = new Set<ScaleAxis>()
  const pos = (v: number | null | undefined) => v != null && v > 0
  if (pos(img.physicalSizeX) && pos(img.physicalSizeY)) out.add('XY')
  if (pos(img.physicalSizeZ)) out.add('Z')
  if (pos(img.timeIncrement)) out.add('T')
  return out
}

/** Scales the task requires, normalised. Empty = the task computes in pixels and does not care. */
export function taskRequiresScale(def: TaskDef): Set<ScaleAxis> {
  const raw = def.requires?.scale
  if (!raw?.length) return new Set()
  return new Set(raw.map(s => String(s).toUpperCase() as ScaleAxis).filter(Boolean))
}

/**
 * Required scales this image does not record — empty when it can run.
 *
 * Intersected with the image's own axes, same rule as the Julia `task_missing_scale`: a static image
 * needs no frame interval and a single plane needs no z spacing, so a task declares the scales its
 * MATHS uses and this decides which of them apply here.
 */
export function missingScale(def: TaskDef, img: CciaImage): Set<ScaleAxis> {
  const need = taskRequiresScale(def)
  if (need.size === 0) return new Set()
  const axes = imageAxes(img)
  const have = imageScales(img)
  const out = new Set<ScaleAxis>()
  for (const s of need) {
    if (s === 'Z' && !axes.has('Z')) continue
    if (s === 'T' && !axes.has('T')) continue
    if (!have.has(s)) out.add(s)
  }
  return out
}

/** What a missing scale is CALLED, in the words the metadata editor uses. */
export const SCALE_LABEL: Record<ScaleAxis, string> = {
  XY: 'pixel size', Z: 'z spacing', T: 'time interval',
}

/**
 * True iff the image is missing a scale that ANY task needing one would ask of it — i.e. the image
 * cannot be measured in physical units at all. Independent of any single task, which is what makes it
 * a property of the IMAGE (the blocked tag in the image table) rather than of a run.
 *
 * Derived, never stored: it stops being true the moment the metadata editor is used, and there is
 * nothing to clear. See `utils/inclusion.isBlocked`, which is where callers should go.
 */
export function imageMissingScale(img: CciaImage): Set<ScaleAxis> {
  const axes = imageAxes(img)
  const have = imageScales(img)
  const out = new Set<ScaleAxis>()
  if (!have.has('XY')) out.add('XY')
  if (axes.has('T') && !have.has('T')) out.add('T')
  return out
}

/** Axes the task requires, normalised to uppercase Axis codes. Empty = applies to any image. */
export function taskRequiresAxes(def: TaskDef): Set<Axis> {
  const raw = def.requires?.axes
  if (!raw?.length) return new Set()
  return new Set(raw.map(a => String(a).toUpperCase() as Axis).filter(Boolean))
}

/** True iff every axis the task requires is present on this image. */
export function taskApplies(def: TaskDef, img: CciaImage): boolean {
  const have = imageAxes(img)
  for (const ax of taskRequiresAxes(def)) if (!have.has(ax)) return false
  return missingScale(def, img).size === 0
}

/** True iff the task applies to EVERY image in the selection. Empty selection → true. */
export function taskAppliesToAll(def: TaskDef, imgs: CciaImage[]): boolean {
  if (imgs.length === 0) return true
  if (taskRequiresAxes(def).size === 0 && taskRequiresScale(def).size === 0) return true
  return imgs.every(img => taskApplies(def, img))
}

/**
 * One-line reason the task doesn't apply — for the greyed picker row's tooltip / helper. Empty
 * string when the task applies. `imgs` = the current picker selection; the message names the
 * missing axis rather than every image, so a mixed selection still reads cleanly.
 */
export function taskGatingReason(def: TaskDef, imgs: CciaImage[]): string {
  if (imgs.length === 0) return ''
  const need = taskRequiresAxes(def)
  const missingBy = new Set<Axis>()
  for (const img of imgs) {
    const have = imageAxes(img)
    for (const ax of need) if (!have.has(ax)) missingBy.add(ax)
  }
  // The axis is reported first when both are missing: it is the one the user cannot act on, so it is
  // the one that decides whether this task is ever going to run on this image.
  if (missingBy.size) return `Requires ${[...missingBy].sort().join(', ')}`

  const noScale = new Set<ScaleAxis>()
  for (const img of imgs) for (const s of missingScale(def, img)) noScale.add(s)
  if (noScale.size === 0) return ''
  const what = [...noScale].sort().map(s => SCALE_LABEL[s]).join(' and ')
  return `Measures in microns — set the ${what} in the image metadata`
}
