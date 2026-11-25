import { describe, expect, it } from 'vitest'
import {
  applyOrder, applyShown, arrangeColumns, DEFAULT_PREFS, groupsOf, orderedGroups, parseColumnPrefs,
  serializeColumnPrefs, setGroupShown, type ColumnPrefs,
} from './imageTableColumns'

const col = (key: string, sticky = false) => ({ key, sticky })
// the image table's default arrangement, with two channels and one attribute
const COLS = [col('name', true), col('thumb'), col('ch'), col('ch:1'), col('ch:2'), col('attr:dose'), col('scale')]
const DEFAULTS = groupsOf(COLS)
const prefs = (p: Partial<ColumnPrefs> = {}): ColumnPrefs => ({ ...DEFAULT_PREFS, ...p })
const keys = (cs: { key: string }[]) => cs.map(c => c.key)

describe('image table columns — storage', () => {
  it('round-trips, and reads junk as the defaults', () => {
    const p = prefs({ order: ['scale', 'name'], hidden: ['thumb'], freezeFirst: false })
    expect(parseColumnPrefs(serializeColumnPrefs(p))).toEqual(p)
    expect(parseColumnPrefs(null)).toEqual(DEFAULT_PREFS)
    expect(parseColumnPrefs('not json')).toEqual(DEFAULT_PREFS)
    expect(parseColumnPrefs('{"order":[1,"z"],"hidden":"x","freezeFirst":"no"}'))
      .toEqual(prefs({ order: ['z'] }))
  })

  it('never stores the name column as hidden', () => {
    expect(parseColumnPrefs('{"hidden":["name","thumb"]}').hidden).toEqual(['thumb'])
  })
})

describe('image table columns — grouping and order', () => {
  it('folds the per-channel columns into one group', () => {
    expect(DEFAULTS).toEqual(['name', 'thumb', 'ch', 'channels', 'attr:dose', 'scale'])
  })

  it('keeps an absent group in the stored order but does not show it', () => {
    const p = prefs({ order: ['scale', 'z', 'name'] })
    expect(orderedGroups(['name', 'scale'], p)).toEqual(['scale', 'name'])
  })
})

describe('image table columns — arrangeColumns', () => {
  it('is the default arrangement with no prefs', () => {
    expect(arrangeColumns(COLS, DEFAULT_PREFS)).toEqual(COLS)
  })

  it('hides a group, channels as a block', () => {
    expect(keys(arrangeColumns(COLS, prefs({ hidden: ['channels', 'thumb'] }))))
      .toEqual(['name', 'ch', 'attr:dose', 'scale'])
  })

  it('reorders, moving the channel columns together', () => {
    const order = ['scale', 'channels', 'name', 'thumb', 'ch', 'attr:dose']
    expect(keys(arrangeColumns(COLS, prefs({ order }))))
      .toEqual(['scale', 'ch:1', 'ch:2', 'name', 'thumb', 'ch', 'attr:dose'])
  })

  it('freezes whichever column is first, and nothing when off', () => {
    const moved = arrangeColumns(COLS, prefs({ order: ['scale', ...DEFAULTS.filter(g => g !== 'scale')] }))
    expect(moved.filter(c => c.sticky).map(c => c.key)).toEqual(['scale'])
    expect(arrangeColumns(COLS, prefs({ freezeFirst: false })).some(c => c.sticky)).toBe(false)
  })

  it('will not hide the name column even if told to', () => {
    expect(keys(arrangeColumns(COLS, prefs({ hidden: ['name'] })))[0]).toBe('name')
  })
})

describe('image table columns — the picker answer', () => {
  it('hides what is unticked without moving anything', () => {
    const next = applyShown(DEFAULTS, DEFAULT_PREFS, ['name', 'ch', 'channels', 'attr:dose', 'scale'])
    expect(next.hidden).toEqual(['thumb'])
    expect(next.order).toEqual([])
    // ticked again, it is back in its own place, not at the end
    expect(keys(arrangeColumns(COLS, applyShown(DEFAULTS, next, DEFAULTS)))).toEqual(keys(COLS))
  })

  it('will not hide the name column when its chip is clicked off', () => {
    expect(applyShown(DEFAULTS, DEFAULT_PREFS, ['thumb', 'scale']).hidden).not.toContain('name')
  })

  it('reorders hidden and shown groups alike, keeping the hidden ones hidden', () => {
    const hid = prefs({ hidden: ['thumb'] })
    const next = applyOrder(DEFAULTS, hid, ['scale', 'thumb', 'name', 'ch', 'channels', 'attr:dose'])
    expect(next.hidden).toEqual(['thumb'])
    expect(keys(arrangeColumns(COLS, next))).toEqual(['scale', 'name', 'ch', 'ch:1', 'ch:2', 'attr:dose'])
  })

  it('leaves an absent group in its slot and in its hidden state', () => {
    const p = prefs({ order: ['name', 'z', 'scale'], hidden: ['z'] })
    const next = applyOrder(['name', 'scale'], p, ['scale', 'name'])
    expect(next.order).toEqual(['scale', 'z', 'name'])
    expect(applyShown(['name', 'scale'], next, ['name', 'scale']).hidden).toEqual(['z'])
    // and comes back between them once the set has a Z
    expect(orderedGroups(['name', 'z', 'scale'], setGroupShown(next, 'z', true))).toEqual(['scale', 'z', 'name'])
  })

  it('does not let a single toggle hide the name column', () => {
    expect(setGroupShown(DEFAULT_PREFS, 'name', false)).toBe(DEFAULT_PREFS)
    expect(setGroupShown(DEFAULT_PREFS, 'channels', false).hidden).toEqual(['channels'])
  })
})
