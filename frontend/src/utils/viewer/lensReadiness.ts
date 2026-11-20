// Why the lens chip shows no regions / track clusters (docs/UI.md → *Channel lens*). The chip counts the
// layers the viewer DRAWS (`lensChipCounts`), not what was run on the image — so "no regions" is
// usually "the Regions layer is off". A layer that adds nothing to the count gets one dim line saying
// which: NOT DRAWN (the pops exist, the layer is off — Show turns it on) or NOT RUN (no pops of that
// type on the image — the task that makes them, opened, never started). Only layers that apply to the
// image: track clusters need a time-lapse.
import { POP_TYPE_REGISTRY, type PopTypeInfo } from '../../plots/popTypeRegistry'

/** One line on the chip: a layer that adds nothing, and why. */
export interface LensReadiness {
  popType: string
  /** the layer as the viewer names it — "Regions", "Track clusters" */
  label: string
  state: 'notDrawn' | 'notRun'
  /** the task that makes it (`clustRegions.cluster`, …) */
  fun: string
  /** what the line offers: "Show", or the task's label */
  action: string
}

/** The layers the chip groups (its regions and track-cluster groups), from the registry: each with the
 *  task that makes it and whether it needs a time-lapse. */
const isRegionType = (i: PopTypeInfo) => i.category === 'region'
const isTrackType = (i: PopTypeInfo) => i.grain === 'track'
export const LENS_READINESS_LAYERS = POP_TYPE_REGISTRY
  .filter(i => i.cluster && (isRegionType(i) || isTrackType(i)))
  .map(i => ({ popType: i.name, label: i.label, timed: i.grain === 'track',
               fun: i.category === 'region' ? 'clustRegions.cluster' : 'clustTracks.cluster' }))

/** The chip's readiness lines. `present` = the pop types this image has pops of (`/api/viewer/pop-types`;
 *  `null` = not answered yet → no lines, rather than a wrong "not run"); `visible` = the viewer's layer
 *  switches (`spec.popVisible`). A drawn layer gets no line — it is counted, even when nothing of it is
 *  inside the lens. */
export function lensReadiness(o: { present: readonly string[] | null; visible: Record<string, boolean>; timecourse: boolean
                                  /** a task's label (`taskDefs.labelFor`) */
                                  taskLabel?: (fun: string) => string }): LensReadiness[] {
  const task = o.taskLabel ?? ((f: string) => f)
  if (!o.present) return []
  const out: LensReadiness[] = []
  for (const l of LENS_READINESS_LAYERS) {
    if (l.timed && !o.timecourse) continue
    if (!o.present.includes(l.popType))
      out.push({ popType: l.popType, label: l.label, state: 'notRun', fun: l.fun, action: task(l.fun) })
    else if (!o.visible[l.popType])
      out.push({ popType: l.popType, label: l.label, state: 'notDrawn', fun: l.fun, action: 'Show' })
  }
  return out
}

/** The line's words: "Regions not shown" / "No regions". */
export function lensReadinessText(r: LensReadiness): string {
  return r.state === 'notDrawn' ? `${r.label} not shown` : `No ${r.label.toLowerCase()}`
}
