// Task-applicability gate — the frontend twin of Cecelia.task_applies / img_axes (Julia).
//
// Task JSON declares `requires.axes`; every consumer that offers a task to the user (module-page
// task picker, chain-module task picker) MUST ask this util before showing/enabling the task, so
// a task that needs a T axis never appears runnable for a static image. The backend gate raises
// TaskApplicabilityError anyway — the frontend gate just removes the surprise.
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

/** Axes the task requires, normalised to uppercase Axis codes. Empty = applies to any image. */
export function taskRequiresAxes(def: TaskDef): Set<Axis> {
  const raw = def.requires?.axes
  if (!raw?.length) return new Set()
  return new Set(raw.map(a => String(a).toUpperCase() as Axis).filter(Boolean))
}

/** True iff every axis the task requires is present on this image. */
export function taskApplies(def: TaskDef, img: CciaImage): boolean {
  const need = taskRequiresAxes(def)
  if (need.size === 0) return true
  const have = imageAxes(img)
  for (const ax of need) if (!have.has(ax)) return false
  return true
}

/** True iff the task applies to EVERY image in the selection. Empty selection → true. */
export function taskAppliesToAll(def: TaskDef, imgs: CciaImage[]): boolean {
  if (imgs.length === 0) return true
  const need = taskRequiresAxes(def)
  if (need.size === 0) return true
  return imgs.every(img => taskApplies(def, img))
}

/**
 * One-line reason the task doesn't apply — for the greyed picker row's tooltip / helper. Empty
 * string when the task applies. `imgs` = the current picker selection; the message names the
 * missing axis rather than every image, so a mixed selection still reads cleanly.
 */
export function taskGatingReason(def: TaskDef, imgs: CciaImage[]): string {
  const need = taskRequiresAxes(def)
  if (need.size === 0 || imgs.length === 0) return ''
  const missingBy = new Set<Axis>()
  for (const img of imgs) {
    const have = imageAxes(img)
    for (const ax of need) if (!have.has(ax)) missingBy.add(ax)
  }
  if (missingBy.size === 0) return ''
  const list = [...missingBy].sort().join(', ')
  return `Requires ${list}`
}
