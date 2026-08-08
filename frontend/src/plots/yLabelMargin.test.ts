import { describe, it, expect } from 'vitest'
import { yLabelMargin } from './plot'

// Outside a browser `textWidth` falls back to a char-count estimate (length * fontPx * 0.55), which is
// deterministic — so these assert the SHAPE of the rule (scales with the longest label, clamped at both
// ends), not exact pixel values that would depend on font metrics.
describe('yLabelMargin', () => {
  it('never goes below the floor, however short the labels', () => {
    expect(yLabelMargin(['1'], 11)).toBe(40)
    expect(yLabelMargin([], 11)).toBe(40)
  })

  // The bug: flipped charts reserved a fixed 104px, so short series labels left a wide empty gap on
  // the left of every rotated plot.
  it('is much tighter than the old fixed 104 for short series labels', () => {
    expect(yLabelMargin(['T · 1', 'B · 2', 'T · 3'], 11)).toBeLessThan(104)
  })

  // …and the other direction, which is why the heatmap shares this: a fixed 120 CLIPPED long feature
  // names like "live.track.meanTurningAngle".
  it('grows for long labels instead of clipping them', () => {
    const long = yLabelMargin(['live.track.meanTurningAngle'], 11)
    expect(long).toBeGreaterThan(yLabelMargin(['T · 1'], 11))
    expect(long).toBeGreaterThan(120)
  })

  it('is driven by the LONGEST label, not the first or the count', () => {
    expect(yLabelMargin(['a', 'live.track.meanTurningAngle', 'b'], 11)).toBe(
      yLabelMargin(['live.track.meanTurningAngle'], 11))
  })

  it('clamps a pathological label so it cannot eat the plot', () => {
    expect(yLabelMargin(['x'.repeat(500)], 11)).toBe(240)
  })

  it('scales with font size', () => {
    expect(yLabelMargin(['live.track.meanTurningAngle'], 16))
      .toBeGreaterThan(yLabelMargin(['live.track.meanTurningAngle'], 8))
  })

  it('stringifies non-string labels rather than throwing', () => {
    expect(yLabelMargin([1, 2, null], 11)).toBe(40)
  })
})
