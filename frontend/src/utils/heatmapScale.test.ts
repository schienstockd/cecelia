import { describe, it, expect } from 'vitest'
import { rescaleRows01 } from './heatmapScale'

describe('rescaleRows01', () => {
  it('rescales each row independently to [0,1]', () => {
    const cells = [
      { y: 'a', value: 10 }, { y: 'a', value: 20 }, { y: 'a', value: 30 },  // → 0, .5, 1
      { y: 'b', value: 5 },  { y: 'b', value: 7 },  { y: 'b', value: 9 },   // → 0, .5, 1
    ]
    expect(rescaleRows01(cells)).toEqual([0, 0.5, 1, 0, 0.5, 1])
  })

  it('maps a flat row (max == min) to 0.5', () => {
    expect(rescaleRows01([{ y: 'a', value: 3 }, { y: 'a', value: 3 }])).toEqual([0.5, 0.5])
  })

  it('handles negative values', () => {
    expect(rescaleRows01([{ y: 'a', value: -2 }, { y: 'a', value: 0 }, { y: 'a', value: 2 }]))
      .toEqual([0, 0.5, 1])
  })

  it('is invariant under a positive affine per-row transform (z-score ≡ raw)', () => {
    // z-score is z = (x - mean) / sd, a positive affine map per row, so rescaling z-scores must
    // give the same [0,1] as rescaling the raw means — the property the heatmap relies on.
    const raw = [{ y: 'a', value: 4 }, { y: 'a', value: 8 }, { y: 'a', value: 10 }]
    const mean = 22 / 3, sd = 2.494438   // arbitrary a>0, b — any positive affine works
    const zscored = raw.map(c => ({ y: c.y, value: (c.value - mean) / sd }))
    const rawNorm = rescaleRows01(raw)
    const zNorm = rescaleRows01(zscored)
    rawNorm.forEach((v, i) => expect(zNorm[i]).toBeCloseTo(v, 10))
  })

  it('returns an empty array for no cells', () => {
    expect(rescaleRows01([])).toEqual([])
  })
})
