import { describe, it, expect } from 'vitest'
import { tkey, parseTkey, seriesMemo } from './series'

describe('tkey / parseTkey', () => {
  it('round-trips a population on a segmentation', () => {
    expect(parseTkey(tkey('live', 'T', '/qc/_tracked')))
      .toEqual({ popType: 'live', valueName: 'T', pop: '/qc/_tracked' })
  })

  it('splits on the FIRST separator of each kind — pop paths keep their slashes', () => {
    expect(parseTkey('track::B/qc/_tracked'))
      .toEqual({ popType: 'track', valueName: 'B', pop: '/qc/_tracked' })
  })

  it('defaults a key with no pop_type to live, and a whole-segmentation key to an empty pop', () => {
    expect(parseTkey('T')).toEqual({ popType: 'live', valueName: 'T', pop: '' })
  })
})

// The canvases build each panel's series list DURING RENDER, so a plain `.map(parseTkey)` handed every
// panel a new array of new objects on every canvas render — a prop that says the same thing it said
// before, and a re-render for it. Same family as DEFAULT_VIS.
describe('seriesMemo', () => {
  it('keeps identity while the keys are unchanged', () => {
    const memo = seriesMemo<number>()
    const a = memo(0, ['live::T/qc/_tracked'])
    expect(memo(0, ['live::T/qc/_tracked'])).toBe(a)     // rebuilt list, same identity
  })

  it('rebuilds when the selection changes — and again when it changes back', () => {
    const memo = seriesMemo<number>()
    const a = memo(0, ['live::T/qc/_tracked'])
    const b = memo(0, ['live::T/qc/_tracked', 'live::B/qc/_tracked'])
    expect(b).not.toBe(a)
    expect(b).toHaveLength(2)
    expect(memo(0, ['live::T/qc/_tracked'])).not.toBe(a)  // cache holds one entry per panel
  })

  it('order is part of the selection, not an implementation detail', () => {
    const memo = seriesMemo<number>()
    const a = memo(0, ['live::T/qc/_tracked', 'live::B/qc/_tracked'])
    expect(memo(0, ['live::B/qc/_tracked', 'live::T/qc/_tracked'])).not.toBe(a)
  })

  it('panels do not evict each other', () => {
    const memo = seriesMemo<number>()
    const a = memo(0, ['live::T/qc/_tracked'])
    memo(1, ['live::B/qc/_tracked'])
    expect(memo(0, ['live::T/qc/_tracked'])).toBe(a)
  })

  it('an empty selection is memoised too — the every-render case for a fresh slot', () => {
    const memo = seriesMemo<number>()
    const a = memo(0, [])
    expect(memo(0, [])).toBe(a)
    expect(a).toEqual([])
  })
})
