import { describe, it, expect } from 'vitest'
import { tkey, parseTkey, seriesMemo, pickedOrderChips, mergePickOrder, sortCats, attrGroupsOf, orderedAttrGroups } from './series'

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

describe('pickedOrderChips — the plot-order strip', () => {
  const groups = [{ valueName: 'cpSAM', populations: [
    { path: '/T', name: 'T cells', colour: '#ff0000', popType: 'flow', uid: 'a' },
    { path: '/T/CD8', name: 'CD8', colour: '#00ff00', popType: 'flow', uid: 'b' }] },
    { valueName: 'flowTom', populations: [{ path: '/T', name: 'T', colour: '#0000ff', popType: 'flow', uid: 'c' }] }]
  it('keeps the pick order and names + colours the chips', () => {
    const chips = pickedOrderChips(['flow::cpSAM/T/CD8', 'flow::cpSAM/T'], groups)
    expect(chips.map(c => c.value)).toEqual(['flow::cpSAM/T/CD8', 'flow::cpSAM/T'])
    expect(chips.map(c => c.label)).toEqual(['CD8', 'T cells'])
    expect(chips[0].swatch).toBe('#00ff00')
  })
  it('names two picked pops that share a name by their parents (every tracked pop is _tracked)', () => {
    const tg = [{ valueName: 'T', populations: [
      { path: '/qc/OTI/_tracked', name: '_tracked', colour: '#111111', popType: 'live', uid: 'c' },
      { path: '/qc/non-OTI/gBT/_tracked', name: '_tracked', colour: '#222222', popType: 'live', uid: 'd' },
      { path: '/qc/non-OTI/P14', name: 'P14', colour: '#333333', popType: 'live', uid: 'e' }] }]
    expect(pickedOrderChips(['live::T/qc/OTI/_tracked', 'live::T/qc/non-OTI/gBT/_tracked', 'live::T/qc/non-OTI/P14'], tg)
      .map(c => c.label)).toEqual(['OTI/_tracked', 'gBT/_tracked', 'P14'])
  })
  // a mixed board keeps every slot's picks in one selection — the active plot's picker lists one family
  it('shows only picks the picker lists — not another family, nor a pop gone from the images', () => {
    expect(pickedOrderChips(['flow::cpSAM/T', 'hmm::cpSAM/Scanning', 'flow::cpSAM/gone/B', 'flow::cpSAM/T/CD8'], groups)
      .map(c => c.value)).toEqual(['flow::cpSAM/T', 'flow::cpSAM/T/CD8'])
  })
  it('names the segmentation only when the SHOWN picks span more than one', () => {
    expect(pickedOrderChips(['flow::cpSAM/T', 'flow::flowTom/T'], groups).map(c => c.label))
      .toEqual(['cpSAM T cells', 'flowTom T'])
    expect(pickedOrderChips(['flow::cpSAM/T', 'hmm::flowTom/Scanning'], groups).map(c => c.label))
      .toEqual(['T cells'])
  })
})

describe('mergePickOrder — a strip reorder keeps the picks it does not show', () => {
  const sel = ['a', 'x', 'b', 'y', 'c']          // x, y: another slot's family
  it('fills the shown slots in the new order', () => {
    expect(mergePickOrder(sel, ['c', 'a', 'b'], ['a', 'b', 'c'])).toEqual(['c', 'x', 'a', 'y', 'b'])
  })
  it('a chip clicked off drops only that pick', () => {
    expect(mergePickOrder(sel, ['a', 'c'], ['a', 'b', 'c'])).toEqual(['a', 'x', 'c', 'y'])
  })
})

describe('attribute groups — the attribute-order strip', () => {
  it('sorts like the server: numeric when all numbers, else lexical', () => {
    expect(sortCats(['10', '2', '1'])).toEqual(['1', '2', '10'])
    expect(sortCats(['10', '2', 'WT'])).toEqual(['10', '2', 'WT'])
  })
  it('joins each image\'s values with "." dropping empties; an image with none is its own uID group', () => {
    const imgs = [{ uid: 'a', attr: { m: '1', t: 'WT' } }, { uid: 'b', attr: { m: '2' } }, { uid: 'c', attr: {} },
                  { uid: 'd', attr: { m: '1', t: 'WT' } }]
    expect(attrGroupsOf(imgs, ['m', 't'])).toEqual(['1.WT', '2', 'c'])
    expect(attrGroupsOf(imgs, [])).toEqual([])
  })
  it('the user order first (stale values ignored), the rest category-sorted after', () => {
    expect(orderedAttrGroups(['1', '2', '3', '4'], ['3', 'gone', '1'])).toEqual(['3', '1', '2', '4'])
    expect(orderedAttrGroups(['10', '2', '1'], undefined)).toEqual(['1', '2', '10'])
  })
})
