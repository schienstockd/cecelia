import { describe, it, expect } from 'vitest'
import {
  overrideTooltip, overrideNote, transformOverride, xRotationOverride, needsXRotation, effectiveOf,
  sameOverrides,
} from './autoOverride'

// One concept: a setting the app could not honour and substituted. It existed twice, ad hoc — the two
// gating panels each compared preferred-vs-used themselves, ambered with their own class, and worded the
// explanation differently (GatePlotPanel's transform select said only "Axis transform", so the amber
// announced that SOMETHING happened without saying what). Auto-rotating x labels is the third case, and
// the reason to generalise rather than add a third variant.

describe('transformOverride', () => {
  it('reports the substitution the server made', () => {
    expect(transformOverride('logicle', 'linear')).toEqual({
      setting: 'Transform', from: 'logicle', to: 'linear',
      why: "this measure's range can't use it",
    })
  })

  it('is null when the preference was honoured — nothing to announce', () => {
    expect(transformOverride('logicle', 'logicle')).toBeNull()
    // …and while either side is still unknown (before the first plotmeta response)
    expect(transformOverride('', 'linear')).toBeNull()
    expect(transformOverride('logicle', '')).toBeNull()
  })
})

describe('xRotationOverride', () => {
  it('announces a rotation the user did not ask for', () => {
    const o = xRotationOverride(true, false)!
    expect(o.setting).toBe('X labels')
    expect(o.to).toBe('rotated')
    expect(o.why).toBe('they would overlap')
  })

  it('says nothing when the user asked for it, or when nothing rotated', () => {
    expect(xRotationOverride(true, true)).toBeNull()    // their own setting — not an override
    expect(xRotationOverride(false, false)).toBeNull()
    expect(xRotationOverride(false, true)).toBeNull()
  })
})

describe('overrideTooltip', () => {
  it('always explains WHY — that is the part the ad-hoc versions dropped', () => {
    expect(overrideTooltip(transformOverride('logicle', 'linear'), 'Axis transform'))
      .toBe("Transform: using linear instead of logicle — this measure's range can't use it")
  })

  it('falls back to the plain label so a call site needs no conditional', () => {
    expect(overrideTooltip(null, 'Axis transform')).toBe('Axis transform')
  })
})

describe('overrideNote', () => {
  it('names each adjusted setting, and is empty when there are none', () => {
    expect(overrideNote([])).toBe('')
    expect(overrideNote([xRotationOverride(true, false)!])).toBe('Adjusted: X labels → rotated')
    expect(overrideNote([xRotationOverride(true, false)!, transformOverride('logicle', 'linear')!]))
      .toBe('Adjusted: X labels → rotated, Transform → linear')
  })
})

describe('needsXRotation', () => {
  // a fixed 7px-per-character metric stands in for the canvas measurement
  const m = (s: string) => s.length * 7

  it('rotates when the widest label cannot fit its band', () => {
    // 6 categories over 400px ⇒ ~57px each; "B · Meandering" is 98px
    expect(needsXRotation(['B · Scanning', 'B · Meandering', 'B · Directed',
                           'T · Scanning', 'T · Meandering', 'T · Directed'], 400, m)).toBe(true)
  })

  it('leaves short labels alone', () => {
    expect(needsXRotation(['B', 'T'], 400, m)).toBe(false)
  })

  it('is driven by the WIDEST label, not the average', () => {
    // three tiny labels and one long one still collide
    expect(needsXRotation(['a', 'b', 'c', 'a very long category label'], 400, m)).toBe(true)
  })

  it('scales with the available width — the same labels fit a wider panel', () => {
    const labels = ['Scanning', 'Meandering', 'Directed']
    expect(needsXRotation(labels, 260, m)).toBe(true)
    expect(needsXRotation(labels, 900, m)).toBe(false)
  })

  it('does nothing before the width is known — one un-rotated frame beats a wrong guess', () => {
    expect(needsXRotation(['a very long category label', 'another one'], 0, m)).toBe(false)
  })

  it('needs at least two labels to have an overlap', () => {
    expect(needsXRotation(['a very long single category'], 100, m)).toBe(false)
    expect(needsXRotation([], 400, m)).toBe(false)
  })

  it('rotates when the margins leave no band at all', () => {
    expect(needsXRotation(['a', 'b'], 40, m)).toBe(true)
  })
})

describe('effectiveOf', () => {
  // The half that's easy to miss: an ambered control still SHOWING the value that wasn't used reads as
  // "your setting is being ignored". The gating transform selects always displayed the used transform
  // while writing the preference; the rotate toggle stayed at `off` beside a rotated plot until it did
  // the same.
  it('shows the substitution while it holds', () => {
    const o = xRotationOverride(true, false)
    expect(effectiveOf(o, false, true)).toBe(true)     // preference off, but the plot IS rotated
    expect(effectiveOf(o, true, true)).toBe(true)
  })

  it('shows the preference when nothing was overridden', () => {
    expect(effectiveOf(null, false, true)).toBe(false)
    expect(effectiveOf(null, true, true)).toBe(true)
  })

  it('works for any value type, not just booleans (the gating selects)', () => {
    expect(effectiveOf(transformOverride('logicle', 'linear'), 'logicle', 'linear')).toBe('linear')
    expect(effectiveOf(null, 'logicle', 'linear')).toBe('logicle')
  })
})

// The emit gate. A re-render that substituted exactly what the last one did must stay silent: the host
// keeps the set, the board keeps the host's readout, and writing that on every render re-rendered the
// panel — "Maximum recursive updates exceeded" on any board whose slots carry no vis of their own.
describe('sameOverrides', () => {
  const rotated = xRotationOverride(true, false)!

  it('two empty sets are the same — the case that caused the loop', () => {
    expect(sameOverrides([], [])).toBe(true)
  })

  it('compares by CONTENT, not identity — a rebuilt equal set is still silent', () => {
    expect(sameOverrides([rotated], [xRotationOverride(true, false)!])).toBe(true)
  })

  it('a substitution appearing or lifting is real news', () => {
    expect(sameOverrides([rotated], [])).toBe(false)
    expect(sameOverrides([], [rotated])).toBe(false)
  })

  it('same setting, different substitution', () => {
    expect(sameOverrides(
      [transformOverride('logicle', 'linear')!],
      [transformOverride('biexp', 'linear')!],
    )).toBe(false)
  })
})
