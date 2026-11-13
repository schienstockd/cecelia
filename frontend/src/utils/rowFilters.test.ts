import { describe, it, expect } from 'vitest'
import {
  ROW_FILTERS, rowFilterKey, anyRowFilterActive, hiddenByRowFilters, nextRowFilterValue,
  parseRowFilterValue, serializeRowFilterValue, rowFilterButton, activeMode,
  type FilterableImage,
} from './rowFilters'

const img = (uid: string, o: Partial<FilterableImage> = {}): FilterableImage => ({
  uid, filepaths: { default: 'x.ome.zarr' }, ...o,
})

const IMAGES: FilterableImage[] = [
  img('a'),                                    // plain: included, imported, unstarred
  img('b', { rating: 'good' }),
  img('c', { included: false }),               // excluded
  img('d', { filepaths: {} }),                 // not yet imported
  img('e', { rating: 'good', included: false }), // starred AND excluded
]
const SCORED = ROW_FILTERS.find(f => f.id === 'unstarred')!

const active = (...ids: string[]) => Object.fromEntries(ids.map(i => [i, true]))
const keep = (a: Record<string, boolean>) => IMAGES.filter(i => !hiddenByRowFilters(i, a)).map(i => i.uid)

describe('rowFilters', () => {
  it('hides nothing when no filter is on', () => {
    expect(anyRowFilterActive({})).toBe(false)
    expect(keep({})).toEqual(['a', 'b', 'c', 'd', 'e'])
  })

  it('hides excluded rows', () => {
    expect(keep(active('excluded'))).toEqual(['a', 'b', 'd'])
  })

  it('hides not-yet-imported rows', () => {
    expect(keep(active('unimported'))).toEqual(['a', 'b', 'c', 'e'])
  })

  it('keeps only starred rows — the star is multi-select, not one per set', () => {
    expect(keep(active('unstarred'))).toEqual(['b', 'e'])
  })

  it('Scored keeps the good images in its first mode and the use-with-care ones in its second', () => {
    const mixed = [img('g', { rating: 'good' }), img('c', { rating: 'caution' }), img('n')]
    const keepMixed = (v: string) => mixed.filter(i => !hiddenByRowFilters(i, { unstarred: v })).map(i => i.uid)
    expect(keepMixed('good')).toEqual(['g'])
    expect(keepMixed('caution')).toEqual(['c'])
  })

  it('Scored steps off → good → use with care → off; on/off filters just flip', () => {
    const seq = [false as boolean | string]
    for (let i = 0; i < 3; i++) seq.push(nextRowFilterValue(SCORED, seq[seq.length - 1]))
    expect(seq).toEqual([false, 'good', 'caution', false])
    const excluded = ROW_FILTERS.find(f => f.id === 'excluded')!
    expect(nextRowFilterValue(excluded, false)).toBe(true)
    expect(nextRowFilterValue(excluded, true)).toBe(false)
  })

  it('a Starred filter left on before modes existed reads back as the good mode', () => {
    expect(parseRowFilterValue(SCORED, '1')).toBe('good')
    expect(parseRowFilterValue(SCORED, 'caution')).toBe('caution')
    for (const raw of ['0', null, 'bogus']) expect(parseRowFilterValue(SCORED, raw)).toBe(false)
    expect(activeMode(SCORED, true)?.id).toBe('good')
    const excluded = ROW_FILTERS.find(f => f.id === 'excluded')!
    expect(parseRowFilterValue(excluded, '1')).toBe(true)
    expect(parseRowFilterValue(excluded, 'good')).toBe(false)
    for (const v of [true, false, 'good', 'caution'] as const)
      expect(parseRowFilterValue(SCORED, serializeRowFilterValue(v))).toBe(v === true ? 'good' : v)
  })

  it('the Scored button shows the mode it is in — icon, count, and what the next click does', () => {
    const mixed = [img('g', { rating: 'good' }), img('c', { rating: 'caution' }), img('d', { rating: 'caution' }), img('n')]
    const off = rowFilterButton(SCORED, false, mixed)
    expect([off.on, off.icon, off.count]).toEqual([false, 'pi-star', 3])
    expect(off.tip).toContain('1 good')
    const good = rowFilterButton(SCORED, 'good', mixed)
    expect([good.on, good.icon, good.count]).toEqual([true, 'pi-star-fill', 1])
    expect(good.tip).toContain('2 use-with-care')
    const care = rowFilterButton(SCORED, 'caution', mixed)
    expect([care.on, care.icon, care.count, care.tip]).toEqual([true, 'pi-star-half-fill', 2, 'Show all images'])
  })

  it('ANDs active filters — starred AND included drops the excluded star', () => {
    expect(keep(active('unstarred', 'excluded'))).toEqual(['b'])
  })

  it('counts what each filter acts on', () => {
    const by = (id: string) => ROW_FILTERS.find(f => f.id === id)!
    expect(by('excluded').count(IMAGES)).toBe(2)     // c, e
    expect(by('unimported').count(IMAGES)).toBe(4)   // all but d
    expect(by('unstarred').count(IMAGES)).toBe(2)    // b, e
  })

  it('hides a filter button when it has nothing to act on', () => {
    const clean = [img('a'), img('b')]
    const buttons = ROW_FILTERS.filter(f => !f.host)
    expect(buttons.filter(f => f.visible(clean)).map(f => f.id)).toEqual([])
    expect(buttons.filter(f => f.visible(IMAGES)).map(f => f.id))
      .toEqual(['excluded', 'unimported', 'unstarred'])
  })

  it('keeps the pre-refactor localStorage keys so existing toggle state survives', () => {
    expect(rowFilterKey('excluded', 'manageImages')).toBe('cc-hide-excluded:manageImages')
    expect(rowFilterKey('unimported', 'manageImages')).toBe('cc-hide-unimported:manageImages')
    expect(rowFilterKey('excluded', undefined)).toBe('cc-hide-excluded:default')
  })

  it('gives every filter a distinct id, both icons, and a two-state tooltip', () => {
    expect(new Set(ROW_FILTERS.map(f => f.id)).size).toBe(ROW_FILTERS.length)
    for (const f of ROW_FILTERS) {
      if (!f.host) expect(f.iconOn).not.toBe(f.iconOff)   // a hosted filter's button draws its own state
      expect(f.tip(true, IMAGES)).not.toBe(f.tip(false, IMAGES))
      expect(f.tip(true, IMAGES).length).toBeGreaterThan(0)
    }
  })
})

describe('the QC-flagged filter (the Checks cluster QC button)', () => {
  const warn = { 'x/default': { funName: 'x', findings: [{ level: 'warn' as const, code: 'x.y', short: 's', long: 'l' }] } }
  const info = { 'x/default': { funName: 'x', findings: [{ level: 'info' as const, code: 'x.y', short: 's', long: 'l' }] } }
  const cal  = { 'importImages.omezarr/default': { findings: [{ level: 'warn' as const, code: 'metadata.pixel_size_unknown', short: 's', long: 'l' }] } }
  const rows = [
    { uid: 'w', qc: warn }, { uid: 'i', qc: info }, { uid: 'c', qc: cal },
    { uid: 'n' }, { uid: 'xw', included: false, qc: warn },
  ]
  const qc = ROW_FILTERS.find(f => f.id === 'qcFlagged')!

  it('is drawn by the Checks cluster, not as a generic filter button', () => {
    expect(qc.host).toBe('checks')
  })

  it('keeps rows with a warn finding, calibration included; info alone is not a flag', () => {
    const kept = rows.filter(r => !hiddenByRowFilters(r, { qcFlagged: true })).map(r => r.uid)
    expect(kept).toEqual(['w', 'c', 'xw'])
  })

  it('counts only included images', () => {
    expect(qc.count(rows)).toBe(2)
  })
})

