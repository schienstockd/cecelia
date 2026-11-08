// RESULT-REVIEW: result-26fdcd54
// A remembered Square=on over a restored NON-square box sent a non-square crop on Use, and a 1 px
// move then silently grew the box to a square (200×50 → 200×200).
import { it, expect } from 'vitest'
import { moveBox } from './previewBox'
import { cropPickFromValue, cropTaskBox } from './cropPick'

const value = { x0: 100, x1: 300, y0: 100, y1: 150, z0: -1, z1: -1, t0: -1, t1: -1 }

it('a restored non-square box opens with Square off, and Use sends exactly that box', () => {
  const p = cropPickFromValue('r', 'img', 'default', value, true)
  expect(p.square).toBe(false)
  expect(cropTaskBox(p.box!, p.z, p.t, 1, 1)).toEqual(value)
})

it('a restored square box keeps the remembered Square', () => {
  const p = cropPickFromValue('r', 'img', 'default', { ...value, y1: 300 }, true)
  expect(p.square).toBe(true)
})

it('moving a box never changes its size, Square or not', () => {
  const p = cropPickFromValue('r', 'img', 'default', value, true)
  const moved = moveBox(p.box!, 1, 0, { imageW: 4000, imageH: 3000, maxSide: Infinity, square: true })
  expect(moved).toEqual({ x0: 101, x1: 301, y0: 100, y1: 150 })
})
