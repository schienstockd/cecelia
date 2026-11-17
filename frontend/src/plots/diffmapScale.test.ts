import { describe, it, expect } from 'vitest'
import { asinhSpread } from './diffmapScale'

describe('asinhSpread', () => {
  it('maps each axis to asinh((v - median) / IQR)', () => {
    // x = 0..4: median 2, IQR 3-1 = 2  →  asinh((x-2)/2);  y constant except one: IQR 0 → range fallback
    const xy = new Float32Array([0, 0, 1, 0, 2, 0, 3, 0, 4, 10])
    const out = asinhSpread(xy)
    for (let i = 0; i < 5; i++) expect(out[2 * i]).toBeCloseTo(Math.asinh((i - 2) / 2), 6)
    expect(out[9]).toBeCloseTo(Math.asinh(10 / 10), 6)
    expect(out[1]).toBe(0)
  })
  it('is monotone and compresses a long arm relative to the core', () => {
    const xy = new Float32Array([0, 0, 0.001, 0, 0.002, 0, 0.003, 0, 0.05, 0])
    const out = asinhSpread(xy)
    for (let i = 1; i < 5; i++) expect(out[2 * i]).toBeGreaterThan(out[2 * (i - 1)])
    // linear: the core spans 0.003/0.05 = 6% of the range; spread, a much larger share
    expect((out[6] - out[0]) / (out[8] - out[0])).toBeGreaterThan(0.25)   // asinh: (asinh(.5)+asinh(1))/(asinh(24)+asinh(1)) ≈ 0.29
  })
  it('handles an empty array', () => { expect(asinhSpread(new Float32Array(0)).length).toBe(0) })
})
