/**
 * The geometry behind the per-pass parameter strip.
 *
 * What is worth pinning is not that a circle has a radius; it is that the picture cannot LIE. A strip
 * that draws two different values the same size, or draws the passes in the wrong order, is worse than
 * no strip: it answers "do these passes differ" confidently and wrongly, and the cost of believing it
 * is a 500-second run that produces slivers.
 */
import { describe, it, expect } from 'vitest'
import { paramVisColumns, uniformWarning, caption, pxCaption, MAX_R } from './paramVis'
import type { ParamDef, ParamValues } from './types'

const GROUP: ParamDef = {
  key: 'models', label: 'Segmentation models', type: 'group', repeatable: true,
  params: [
    { key: 'model', label: 'Model', type: 'select' },
    { key: 'seedSize', label: 'Seed window', type: 'float', vis: 'diameter' },
    { key: 'affinityThreshold', label: 'Growing threshold', type: 'float', vis: 'fraction' },
    { key: 'advanced', label: 'Advanced', type: 'section', params: [
      { key: 'seedBlurSigma', label: 'Seed blur', type: 'float', vis: 'blur' },
      { key: 'minComponentSize', label: 'Min fragment', type: 'float', vis: 'area' },
      { key: 'mergeMaxDistance', label: 'Merge distance', type: 'float', vis: 'distance' },
    ] },
  ],
} as ParamDef

// The reference two-pass config, in the form's microns at ~0.33 µm/px.
const TWO_PASS: Record<string, ParamValues> = {
  '0': { model: 'flow.pt', seedSize: 10.61, affinityThreshold: 0.2, seedBlurSigma: 0,
         minComponentSize: 1.1, mergeMaxDistance: 0.5 },
  '1': { model: 'flow.pt', seedSize: 2.65, affinityThreshold: 0.8, seedBlurSigma: 0,
         minComponentSize: 1.1, mergeMaxDistance: 0.5 },
}
const PX = 0.331456303681194

const row = (v: ReturnType<typeof paramVisColumns>, key: string) =>
  v.rows.find(r => r.key === key)!

describe('paramVisColumns', () => {
  it('draws only the params the spec gives a role', () => {
    const v = paramVisColumns(GROUP, TWO_PASS, ['0', '1'])
    expect(v.rows.map(r => r.key).sort()).toEqual(
      ['affinityThreshold', 'mergeMaxDistance', 'minComponentSize', 'seedBlurSigma', 'seedSize'])
    expect(v.rows.find(r => r.key === 'model')).toBeUndefined()
  })

  it('reaches params inside a section, which are stored flat', () => {
    // Nested collection would silently drop half the picture — every Advanced param.
    expect(row(paramVisColumns(GROUP, TWO_PASS, ['0', '1']), 'seedBlurSigma')).toBeTruthy()
  })

  it('columns follow RUN order, not object key order', () => {
    // Which pass is FIRST is the whole meaning: it claims pixels and the other fills the remainder.
    const v = paramVisColumns(GROUP, TWO_PASS, ['1', '0'])
    expect(v.columns).toEqual(['1', '0'])
    expect(row(v, 'seedSize').cells[0].value).toBe(2.65)
  })

  it('drops an entry key that no longer exists', () => {
    expect(paramVisColumns(GROUP, TWO_PASS, ['0', '9']).columns).toEqual(['0'])
  })

  it('one column is still readable — the scale is shared across rows of a dimension', () => {
    // THE bug the first version shipped with: scaling each row against only its own columns makes a
    // single-column group draw EVERY shape at full radius, because each row's one value is trivially
    // its own maximum. A 10.61 µm seed window and a 1.1 µm² size floor rendered as identical circles.
    const one = paramVisColumns(GROUP, { '0': TWO_PASS['0'] }, ['0'])
    const seed = row(one, 'seedSize').cells[0].r
    const blur = row(one, 'seedBlurSigma').cells[0].r      // 0 -> off
    const dist = row(one, 'mergeMaxDistance').cells[0].r   // 0.5 µm against a 10.61 µm seed
    expect(seed).toBe(MAX_R)
    expect(dist).toBeLessThan(seed)
    // 0.5/10.61 of the full radius is ~1 unit, under the 2-unit floor that keeps a small value
    // VISIBLE. Clamped, not vanished — and still plainly smaller than the seed window.
    expect(dist).toBe(2)
    expect(blur).toBe(0)
  })

  it('proportion is exact wherever it clears the visibility floor', () => {
    const one = paramVisColumns(GROUP, { '0': { seedSize: 10, mergeMaxDistance: 4 } }, ['0'])
    const seed = row(one, 'seedSize').cells[0].r
    expect(row(one, 'mergeMaxDistance').cells[0].r / seed).toBeCloseTo(0.4, 5)
  })

  it('an area keeps its OWN scale — µm² is not comparable with µm', () => {
    const one = paramVisColumns(GROUP, { '0': TWO_PASS['0'] }, ['0'])
    // the only area row, so it is its own peak and draws full — not scaled against the seed window
    expect(row(one, 'minComponentSize').cells[0].r).toBe(MAX_R)
  })

  it('the larger value gets the larger radius, at full scale', () => {
    const seeds = row(paramVisColumns(GROUP, TWO_PASS, ['0', '1']), 'seedSize')
    expect(seeds.cells[0].r).toBe(MAX_R)
    expect(seeds.cells[1].r).toBeLessThan(seeds.cells[0].r)
    // and in proportion, because a picture out of proportion is a wrong answer
    expect(seeds.cells[1].r / seeds.cells[0].r).toBeCloseTo(2.65 / 10.61, 5)
  })

  it('an AREA is drawn by its square root, not its value', () => {
    // Radius is what the eye compares. Using the value as the radius exaggerates by the square: a
    // 4 µm² floor beside a 1 µm² one would look 4x bigger instead of 2x.
    const vals = { '0': { minComponentSize: 4 }, '1': { minComponentSize: 1 } }
    const r = row(paramVisColumns(GROUP, vals, ['0', '1']), 'minComponentSize')
    expect(r.cells[1].r / r.cells[0].r).toBeCloseTo(0.5, 5)
  })

  it('a threshold sits on its own 0-1 track, not scaled against the other column', () => {
    // 0.2 and 0.8 must land at 20% and 80% of the track. Normalising them against each other would
    // put the larger at the far end whatever it was, so 0.5 vs 0.5 and 0.2 vs 0.8 would look alike.
    const t = row(paramVisColumns(GROUP, TWO_PASS, ['0', '1']), 'affinityThreshold')
    expect(t.cells[0].at).toBeCloseTo(0.2, 6)
    expect(t.cells[1].at).toBeCloseTo(0.8, 6)
    expect(t.cells[0].r).toBe(0)
  })

  it('a threshold outside 0-1 is clamped rather than drawn off the track', () => {
    const t = row(paramVisColumns(GROUP, { '0': { affinityThreshold: 1.7 } }, ['0']),
                  'affinityThreshold')
    expect(t.cells[0].at).toBe(1)
  })

  it('a row that is zero everywhere does not produce NaN radii', () => {
    // "blur off on both passes" is a real, correct state — and the row still has to render.
    const b = row(paramVisColumns(GROUP, TWO_PASS, ['0', '1']), 'seedBlurSigma')
    expect(b.cells.every(c => Number.isFinite(c.r))).toBe(true)
    expect(b.cells.map(c => c.text)).toEqual(['off', 'off'])
  })

  it('a non-zero value never draws as nothing', () => {
    // A tiny value beside a huge one still has to be visible, or the picture says "off" when it is on.
    const v = { '0': { seedSize: 100 }, '1': { seedSize: 0.01 } }
    expect(row(paramVisColumns(GROUP, v, ['0', '1']), 'seedSize').cells[1].r).toBeGreaterThan(0)
  })

  it('a param no column has a number for is dropped, not drawn at zero', () => {
    // A blank shape reads as "this is off", which is a claim about the parameter.
    const v = { '0': { seedSize: 4 }, '1': { seedSize: 2 } }
    expect(paramVisColumns(GROUP, v, ['0', '1']).rows.map(r => r.key)).toEqual(['seedSize'])
  })

  it('a non-numeric value is treated as absent, not coerced to zero', () => {
    const v = { '0': { seedSize: 'wide' as unknown as number }, '1': { seedSize: 2 } }
    const r = row(paramVisColumns(GROUP, v, ['0', '1']), 'seedSize')
    expect(r.cells[0].value).toBe(0)
    expect(r.cells[1].r).toBe(MAX_R)
  })

  it('spatial rows come before thresholds', () => {
    const v = paramVisColumns(GROUP, TWO_PASS, ['0', '1'])
    const firstFraction = v.rows.findIndex(r => r.role === 'fraction')
    const lastSpatial = v.rows.map(r => r.role !== 'fraction').lastIndexOf(true)
    expect(lastSpatial).toBeLessThan(firstFraction)
  })

  it('marks the rows that are identical across passes', () => {
    const v = paramVisColumns(GROUP, TWO_PASS, ['0', '1'])
    expect(v.uniformKeys.sort()).toEqual(['mergeMaxDistance', 'minComponentSize', 'seedBlurSigma'])
    expect(row(v, 'seedSize').uniform).toBe(false)
  })

  it('a single pass is never "uniform" — there is nothing to be uniform with', () => {
    const v = paramVisColumns(GROUP, { '0': TWO_PASS['0'] }, ['0'])
    expect(v.uniformKeys).toEqual([])
  })
})

describe('caption', () => {
  it('is in the FORM\'s units, matching the row label and the control being edited', () => {
    // A row labelled "Seed window (µm)" whose caption reads "32 px" contradicts both the label and
    // the slider the user is dragging. That was the first thing Dominik said about it.
    expect(caption('diameter', 10.61)).toBe('10.61')
    expect(caption('area', 1.1)).toBe('1.1')
  })

  it('zero reads as off, not as 0', () => {
    expect(caption('blur', 0)).toBe('off')
  })

  it('a threshold is unitless', () => {
    expect(caption('fraction', 0.2)).toBe('0.2')
  })
})

describe('pxCaption', () => {
  it('is the engine-facing number, as a second line', () => {
    expect(pxCaption('diameter', 10.61, PX)).toBe('32 px')
    expect(pxCaption('diameter', 2.65, PX)).toBe('8 px')
  })

  it('an area converts with BOTH axes', () => {
    // Assuming a length conversion here would be quietly off by the pixel size — 3x on this data.
    expect(pxCaption('area', 1.1, PX)).toBe('10 px²')
  })

  it('is absent when there is no pixel size to convert with', () => {
    expect(pxCaption('diameter', 10.61, null)).toBe('')
  })

  it('is absent for a threshold, which has no length', () => {
    expect(pxCaption('fraction', 0.2, PX)).toBe('')
  })

  it('is absent for a value that is off — "0 px" is not a fact about it', () => {
    expect(pxCaption('blur', 0, PX)).toBe('')
  })
})

describe('uniformWarning', () => {
  it('says nothing for the reference two-pass config', () => {
    // The rows that ARE shared here (blur off, merge distance, size floor) are shared on purpose.
    expect(uniformWarning(paramVisColumns(GROUP, TWO_PASS, ['0', '1']))).toBe('')
  })

  it('fires when the passes share what decides how far they grow', () => {
    // The bug: both passes at the same seed size and growing threshold, which is what a second entry
    // used to be born as.
    const same = { '0': { seedSize: 4, affinityThreshold: 0.5 },
                   '1': { seedSize: 4, affinityThreshold: 0.5 } }
    const w = uniformWarning(paramVisColumns(GROUP, same, ['0', '1']))
    expect(w).toContain('identical')
    expect(w).toContain('repeat the first')
  })

  it('names the one setting when only one is shared', () => {
    const same = { '0': { seedSize: 4, affinityThreshold: 0.2 },
                   '1': { seedSize: 4, affinityThreshold: 0.8 } }
    expect(uniformWarning(paramVisColumns(GROUP, same, ['0', '1']))).toContain('seed window')
  })

  it('ignores rows that say nothing about how far a pass grows', () => {
    // Two passes sharing a merge distance is ordinary and must not be warned about, or the warning
    // becomes noise and gets ignored on the run where it matters.
    const v = { '0': { seedSize: 10, affinityThreshold: 0.2, mergeMaxDistance: 0.5 },
                '1': { seedSize: 2, affinityThreshold: 0.8, mergeMaxDistance: 0.5 } }
    expect(uniformWarning(paramVisColumns(GROUP, v, ['0', '1']))).toBe('')
  })

  it('says nothing about a single pass', () => {
    expect(uniformWarning(paramVisColumns(GROUP, { '0': TWO_PASS['0'] }, ['0']))).toBe('')
  })

  it('does not end in a period — it is a UI line', () => {
    const same = { '0': { seedSize: 4 }, '1': { seedSize: 4 } }
    expect(uniformWarning(paramVisColumns(GROUP, same, ['0', '1']))).not.toMatch(/\.$/)
  })
})
