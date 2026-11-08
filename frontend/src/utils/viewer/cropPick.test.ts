import { describe, it, expect } from 'vitest'
import { cropTaskBox, cropPickFromValue } from './cropPick'

const box = { x0: 10, y0: 20, x1: 110, y1: 220 }

describe('cropTaskBox', () => {
  it('keeps a whole axis as -1', () => {
    expect(cropTaskBox(box, [0, 9], [0, 4], 10, 5))
      .toEqual({ x0: 10, x1: 110, y0: 20, y1: 220, z0: -1, z1: -1, t0: -1, t1: -1 })
    expect(cropTaskBox(box, null, null, 10, 5).z0).toBe(-1)
  })
  it('turns inclusive ranges into half-open bounds', () => {
    expect(cropTaskBox(box, [2, 5], [1, 1], 10, 5)).toMatchObject({ z0: 2, z1: 6, t0: 1, t1: 2 })
  })
  it('clamps and orders a range', () => {
    expect(cropTaskBox(box, [7, -3], null, 10, 1)).toMatchObject({ z0: 0, z1: 8, t0: -1 })
  })
  it('a single-plane image never trims z', () => {
    expect(cropTaskBox(box, [0, 0], null, 1, 1).z0).toBe(-1)
  })
})

describe('cropPickFromValue', () => {
  it('restores the chosen box and ranges', () => {
    const p = cropPickFromValue('r', 'img', 'default', { x0: 1, x1: 5, y0: 2, y1: 6, z0: 3, z1: 7, t0: -1, t1: -1 }, false)
    expect(p).toMatchObject({ box: { x0: 1, x1: 5, y0: 2, y1: 6 }, z: [3, 6], t: null, status: 'drawing' })
  })
  it('starts empty without a value', () => {
    expect(cropPickFromValue('r', 'img', 'default', null, true)).toMatchObject({ box: null, z: null, t: null, square: true })
  })
  it('round-trips through cropTaskBox', () => {
    const v = { x0: 1, x1: 5, y0: 2, y1: 6, z0: 3, z1: 7, t0: 0, t1: 2 }
    const p = cropPickFromValue('r', 'img', 'default', v, false)
    expect(cropTaskBox(p.box!, p.z, p.t, 10, 5)).toEqual(v)
  })
})
