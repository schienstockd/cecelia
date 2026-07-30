import { describe, it, expect } from 'vitest'
import { statsBandFraction } from './plot'

// The stats annotations are positioned in DATA coordinates above the data, but the measure-axis domain
// used to be derived from the data alone (+5%). A compact letter therefore landed exactly on the frame
// and its pixel offset pushed the glyph outside the plot, where it was clipped — "squished to the edge
// of the box". `statsBandFraction` is what the domain (and the pixel margin) now reserve room from.
const LETTERS = {
  comparisons: {
    groups: ['a', 'b', 'c'], letters: ['a', 'b', 'ab'],
    comparisonPairs: [
      { a: 'a', b: 'b', pAdj: 0.01, significance: '*' },
      { a: 'a', b: 'c', pAdj: 0.4, significance: 'ns' },
      { a: 'b', b: 'c', pAdj: 0.02, significance: '*' },
    ],
  },
} as never

const ON = { statsEnabled: true }

describe('statsBandFraction', () => {
  it('is 0 when stats are off — no annotation, no reserved space', () => {
    expect(statsBandFraction(LETTERS, { statsEnabled: false, statsUseLetters: true })).toBe(0)
  })

  it('is 0 when the response carries no comparisons', () => {
    expect(statsBandFraction({}, { ...ON, statsUseLetters: true })).toBe(0)
  })

  it('reserves ONE row for a compact letter display, however many groups', () => {
    const band = statsBandFraction(LETTERS, { ...ON, statsUseLetters: true })
    expect(band).toBeGreaterThan(0)
    // letters are a single row at the headroom — they do NOT stack like brackets
    const brackets = statsBandFraction(LETTERS, { ...ON, statsUseLetters: false })
    expect(band).toBeLessThan(brackets)
  })

  it('reserves one row PER SHOWN PAIR for the bracket stack', () => {
    const shown2 = statsBandFraction(LETTERS, { ...ON, statsUseLetters: false, statsShowNs: false })
    const shown3 = statsBandFraction(LETTERS, { ...ON, statsUseLetters: false, statsShowNs: true })
    expect(shown3).toBeGreaterThan(shown2)      // the ns pair adds a row when shown
  })

  it('falls back to brackets when letters were requested but none were computed', () => {
    const noLetters = { comparisons: { ...(LETTERS as never as { comparisons: object }).comparisons, letters: [] } } as never
    expect(statsBandFraction(noLetters, { ...ON, statsUseLetters: true }))
      .toBe(statsBandFraction(noLetters, { ...ON, statsUseLetters: false }))
  })

  it('expands a 2-group omnibus with no explicit pairs into one row', () => {
    const omnibus = { comparisons: { groups: ['a', 'b'], pValue: 0.01, significance: '*' } } as never
    expect(statsBandFraction(omnibus, { ...ON, statsUseLetters: false })).toBeGreaterThan(0)
  })

  it('is 0 when every pair is ns and ns is hidden — nothing is drawn', () => {
    const allNs = {
      comparisons: { groups: ['a', 'b'], comparisonPairs: [{ a: 'a', b: 'b', pAdj: 0.9, significance: 'ns' }] },
    } as never
    expect(statsBandFraction(allNs, { ...ON, statsUseLetters: false, statsShowNs: false })).toBe(0)
    expect(statsBandFraction(allNs, { ...ON, statsUseLetters: false, statsShowNs: true })).toBeGreaterThan(0)
  })
})
