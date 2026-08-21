import { describe, it, expect } from 'vitest'
import {
  imageAxes, imageScales, imageMissingScale, taskApplies, taskAppliesToAll, taskGatingReason,
  taskRequiresAxes, taskRequiresScale,
} from './taskGating'
import type { CciaImage } from '../stores/project'
import type { TaskDef } from '../tasks/types'

// The frontend twin of Cecelia.task_applies. Two halves with different failure characters: a missing
// AXIS is a fact about the image, a missing SCALE is metadata the user can fill in.
describe('axis gating', () => {
  const IMG = (over: Partial<CciaImage> = {}): CciaImage =>
    ({ uid: 'u', name: 'n', status: 'done', ...over } as CciaImage)

  it('reads the non-trivial axes, with the pre-SizeT TimeIncrement fallback', () => {
    expect([...imageAxes(IMG())].sort()).toEqual(['X', 'Y'])
    expect(imageAxes(IMG({ sizeT: 5 })).has('T')).toBe(true)
    expect(imageAxes(IMG({ sizeZ: 5 })).has('Z')).toBe(true)
    expect(imageAxes(IMG({ timeIncrement: 30 })).has('T')).toBe(true)   // no sizeT, still a timelapse
  })

  it('requires every declared axis, and applies to all or nothing across a selection', () => {
    const def = { requires: { axes: ['T'] } } as TaskDef
    expect(taskRequiresAxes(def).has('T')).toBe(true)
    expect(taskApplies(def, IMG({ sizeT: 5 }))).toBe(true)
    expect(taskApplies(def, IMG())).toBe(false)
    expect(taskAppliesToAll(def, [IMG({ sizeT: 5 }), IMG()])).toBe(false)
    expect(taskAppliesToAll({} as TaskDef, [IMG()])).toBe(true)
    expect(taskRequiresScale({} as TaskDef).size).toBe(0)
  })
})


// ── The scale half of the gate ───────────────────────────────────────────────
// `requires.scale` exists because the failure is SILENT: img_physical_sizes falls back to 1.0 for a
// missing axis, which is indistinguishable from a genuine 1 µm/px, so a µm-measuring task on an
// uncalibrated image reports pixels as microns and nothing complains.
describe('scale gating', () => {
  const IMG = (over: Partial<CciaImage> = {}): CciaImage =>
    ({ uid: 'u', name: 'n', status: 'done', ...over } as CciaImage)
  const DEF = (scale?: string[], axes?: string[]): TaskDef =>
    ({ requires: { ...(axes ? { axes } : {}), ...(scale ? { scale } : {}) } } as TaskDef)

  const CAL = { physicalSizeX: 0.5, physicalSizeY: 0.5 }
  const LIVE = { sizeT: 10, timeIncrement: 30 }

  it('records a scale only when both pixel sizes are present and positive', () => {
    expect([...imageScales(IMG(CAL))]).toEqual(['XY'])
    expect([...imageScales(IMG({ physicalSizeX: 0.5 }))]).toEqual([])          // Y missing
    expect([...imageScales(IMG({ physicalSizeX: 0, physicalSizeY: 0.5 }))]).toEqual([])  // 0 is not a measurement
    expect([...imageScales(IMG({ timeIncrement: 30 }))]).toEqual(['T'])
  })

  it('a task with no scale requirement applies to anything', () => {
    expect(taskApplies(DEF(), IMG())).toBe(true)
  })

  it('blocks a µm-measuring task on an uncalibrated image', () => {
    expect(taskApplies(DEF(['xy']), IMG())).toBe(false)
    expect(taskApplies(DEF(['xy']), IMG(CAL))).toBe(true)
  })

  // The subtlety: a declaration says "these, for whichever axes this image HAS". Otherwise every
  // task would have to enumerate the 2D/3D/static/timelapse combinations itself.
  it('does not ask a static image for a frame interval', () => {
    expect(taskApplies(DEF(['xy', 't']), IMG(CAL))).toBe(true)
    expect(taskApplies(DEF(['xy', 't']), IMG({ ...CAL, ...LIVE }))).toBe(true)
    expect(taskApplies(DEF(['xy', 't']), IMG({ ...CAL, sizeT: 10 }))).toBe(false)   // live, no interval
  })

  it('does not ask a single plane for a z spacing', () => {
    expect(taskApplies(DEF(['xy', 'z']), IMG(CAL))).toBe(true)
    expect(taskApplies(DEF(['xy', 'z']), IMG({ ...CAL, sizeZ: 8 }))).toBe(false)
    expect(taskApplies(DEF(['xy', 'z']), IMG({ ...CAL, sizeZ: 8, physicalSizeZ: 2 }))).toBe(true)
  })

  // A missing axis is a fact about the image; a missing scale is metadata the user can fill in. So
  // the axis is reported first when both are missing — it decides whether this will EVER run.
  it('reports the axis before the scale, and the scale as an action', () => {
    expect(taskGatingReason(DEF(['xy'], ['T']), [IMG()])).toBe('Requires T')
    // A timelapse with NEITHER recorded, so both halves are named. (`LIVE` carries an interval —
    // using it here would only report the pixel size, which is what this assertion is not about.)
    const r = taskGatingReason(DEF(['xy', 't']), [IMG({ sizeT: 10 })])
    expect(r).toContain('pixel size')
    expect(r).toContain('time interval')
    expect(r).toContain('metadata')
    expect(taskGatingReason(DEF(['xy']), [IMG(CAL)])).toBe('')
  })

  it('an image is blocked when it cannot be measured at all, whatever the task', () => {
    expect([...imageMissingScale(IMG())]).toEqual(['XY'])
    expect([...imageMissingScale(IMG(CAL))]).toEqual([])
    expect([...imageMissingScale(IMG({ ...CAL, sizeT: 10 }))]).toEqual(['T'])
    expect([...imageMissingScale(IMG({ ...CAL, ...LIVE }))]).toEqual([])
  })
})
