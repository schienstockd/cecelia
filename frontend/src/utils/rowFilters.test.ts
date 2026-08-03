import { describe, it, expect } from 'vitest'
import {
  ROW_FILTERS, rowFilterKey, anyRowFilterActive, hiddenByRowFilters,
  type FilterableImage,
} from './rowFilters'

const img = (uid: string, o: Partial<FilterableImage> = {}): FilterableImage => ({
  uid, filepaths: { default: 'x.ome.zarr' }, ...o,
})

const IMAGES: FilterableImage[] = [
  img('a'),                                    // plain: included, imported, unstarred
  img('b', { starred: true }),
  img('c', { included: false }),               // excluded
  img('d', { filepaths: {} }),                 // not yet imported
  img('e', { starred: true, included: false }), // starred AND excluded
]

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
    expect(ROW_FILTERS.filter(f => f.visible(clean)).map(f => f.id)).toEqual([])
    expect(ROW_FILTERS.filter(f => f.visible(IMAGES)).map(f => f.id))
      .toEqual(['excluded', 'unimported', 'unstarred'])
  })

  it('keeps the pre-refactor localStorage keys so existing toggle state survives', () => {
    expect(rowFilterKey('excluded', 'import')).toBe('cc-hide-excluded:import')
    expect(rowFilterKey('unimported', 'import')).toBe('cc-hide-unimported:import')
    expect(rowFilterKey('excluded', undefined)).toBe('cc-hide-excluded:default')
  })

  it('gives every filter a distinct id, both icons, and a two-state tooltip', () => {
    expect(new Set(ROW_FILTERS.map(f => f.id)).size).toBe(ROW_FILTERS.length)
    for (const f of ROW_FILTERS) {
      expect(f.iconOn).not.toBe(f.iconOff)
      expect(f.tip(true, IMAGES)).not.toBe(f.tip(false, IMAGES))
      expect(f.tip(true, IMAGES).length).toBeGreaterThan(0)
    }
  })
})
