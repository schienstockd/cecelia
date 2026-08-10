import { describe, it, expect } from 'vitest'
import { attrKeysOf, attrValueMap, attrChipOptions, emptyAttrFilter, attrFilterActive,
         attrFilterDrafted, applyAttrFilter, matchesAttrFilter, pruneAttrFilter,
         type AttrBearing, type AttrFilterState } from './attrFilter'

const rows: AttrBearing[] = [
  { attr: { Treatment: 'control',  Timepoint: '4h' } },
  { attr: { Treatment: 'anti-PD1', Timepoint: '4h' } },
  { attr: { Treatment: 'control',  Timepoint: '24h' } },
  { attr: { Treatment: '' } },              // never annotated
  { },                                      // no attr bag at all
]

describe('attrKeysOf', () => {
  it('is the union across rows, sorted', () => {
    expect(attrKeysOf(rows)).toEqual(['Timepoint', 'Treatment'])
  })
  it('is empty for rows with no attributes', () => {
    expect(attrKeysOf([{}, { attr: {} }])).toEqual([])
  })
})

describe('attrValueMap', () => {
  it('collects the distinct values in use per key, sorted', () => {
    expect(attrValueMap(rows)).toEqual({
      Treatment: ['', 'anti-PD1', 'control'],
      Timepoint: ['24h', '4h'],
    })
  })
})

describe('attrChipOptions', () => {
  it('labels a blank value, so "never annotated" is a pickable chip and not an empty pill', () => {
    // ChipSelect hides the label span when label === '' (that is how icon-only chips work), so the
    // value is kept — the filter matches on it — and the label is given something to show
    expect(attrChipOptions('Treatment', ['', 'control'])).toEqual([
      { value: '', label: '—', tip: 'No Treatment set' },
      { value: 'control', label: 'control', tip: 'control' },
    ])
  })
})

describe('the draft/applied split', () => {
  it('picking chips does not narrow the list — Apply does', () => {
    const s: AttrFilterState = { ...emptyAttrFilter(), draft: { Treatment: ['control'] } }
    expect(attrFilterDrafted(s)).toBe(true)
    expect(attrFilterActive(s)).toBe(false)
    expect(rows.filter(r => matchesAttrFilter(r.attr, s))).toHaveLength(rows.length)
    const applied = applyAttrFilter(s)
    expect(attrFilterActive(applied)).toBe(true)
  })

  it('drops the keys nothing was picked for, so an emptied row stops narrowing', () => {
    const s: AttrFilterState = { ...emptyAttrFilter(), draft: { Treatment: ['control'], Timepoint: [] } }
    expect(applyAttrFilter(s).applied).toEqual({ Treatment: ['control'] })
  })
})

describe('matchesAttrFilter', () => {
  const applied = (draft: Record<string, string[]>, invert = false): AttrFilterState =>
    ({ ...applyAttrFilter({ ...emptyAttrFilter(), draft }), invert })

  it('passes everything when nothing is applied', () => {
    expect(rows.every(r => matchesAttrFilter(r.attr, emptyAttrFilter()))).toBe(true)
  })

  it('is ANY within a key', () => {
    const s = applied({ Timepoint: ['4h', '24h'] })
    expect(rows.filter(r => matchesAttrFilter(r.attr, s))).toHaveLength(3)
  })

  it('is ALL across keys — a row is "control AND 4h"', () => {
    const s = applied({ Treatment: ['control'], Timepoint: ['4h'] })
    expect(rows.filter(r => matchesAttrFilter(r.attr, s))).toEqual([rows[0]])
  })

  it('matches a missing or blank attribute as the empty string', () => {
    const s = applied({ Treatment: [''] })
    // the never-annotated row AND the one with no attr bag at all
    expect(rows.filter(r => matchesAttrFilter(r.attr, s))).toEqual([rows[3], rows[4]])
  })

  it('inverts the whole verdict, not each clause', () => {
    // NOT (control at 4h) — the other four, including the ones with neither attribute set
    const s = applied({ Treatment: ['control'], Timepoint: ['4h'] }, true)
    expect(rows.filter(r => matchesAttrFilter(r.attr, s))).toHaveLength(4)
  })

  it('invert on an empty filter still shows everything — nothing to invert', () => {
    expect(rows.every(r => matchesAttrFilter(r.attr, { ...emptyAttrFilter(), invert: true }))).toBe(true)
  })
})

describe('pruneAttrFilter', () => {
  it('drops values that no longer exist, so a persisted filter cannot empty a different project', () => {
    // otherwise `Treatment: MERTK` carried into a project without it hides every row, and an empty
    // list is indistinguishable from "there is nothing here"
    const s: AttrFilterState = { draft: { Treatment: ['control', 'gone'] },
                                 applied: { Treatment: ['gone'] }, invert: false }
    const out = pruneAttrFilter(s, rows)
    expect(out.draft).toEqual({ Treatment: ['control'] })
    expect(out.applied).toEqual({})          // nothing left → no filter at all
  })

  it('drops a key whose values are all gone rather than leaving it matching nothing', () => {
    expect(pruneAttrFilter({ ...emptyAttrFilter(), applied: { Nope: ['x'] } }, rows).applied).toEqual({})
  })

  it('returns the SAME object when nothing changed, so a watcher does not write on every read', () => {
    const s: AttrFilterState = { ...emptyAttrFilter(), applied: { Treatment: ['control'] } }
    expect(pruneAttrFilter(s, rows)).toBe(s)
  })
})
