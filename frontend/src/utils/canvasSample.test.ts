import { describe, it, expect } from 'vitest'
import { measureRgba } from './canvasSample'

const rgba = (px: number[][]) => new Uint8Array(px.flatMap(p => [p[0], p[1], p[2], 255]))

describe('measureRgba', () => {
  it('reads pure black as nothing lit', () => {
    // The whole point: this number is what separates "the draw never landed on the canvas" from
    // "the canvas never reached the screen", so a black frame has to read as exactly zero.
    const s = measureRgba(rgba([[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]]), 2)
    expect(s).toMatchObject({ max: 0, mean: 0, lit: 0 })
  })

  it('counts a pixel as lit on ANY channel, not on luminance', () => {
    // A single saturated channel is what a one-channel MIP looks like; weighting by luminance would
    // report a blue-only image as nearly dark.
    const s = measureRgba(rgba([[0, 0, 255], [0, 0, 0], [0, 0, 0], [0, 0, 0]]), 2)
    expect(s.lit).toBe(0.25)
    expect(s.max).toBe(1)
  })

  it('averages over all three channels, not over the lit ones', () => {
    const s = measureRgba(rgba([[255, 255, 255], [0, 0, 0]]), 1)
    expect(s.mean).toBeCloseTo(0.5, 6)
    expect(s.lit).toBe(0.5)
  })

  it('ignores alpha — an opaque canvas carries none of the answer there', () => {
    const px = new Uint8Array([255, 0, 0, 0])      // alpha 0, red 255
    expect(measureRgba(px, 1).max).toBe(1)
  })
})
