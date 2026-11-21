import { describe, expect, it } from 'vitest'
import {
  applyRules, baseName, designReservations, emptyDesign, gridPivot, runOf, sameNameGroups, suggestDesign,
  type DesignGrid, type DesignImage,
} from './experimentDesign'

// The names and original folders of an intravital set as its files were registered (16 movies, two
// label panels in the existing attribute `Test`, imaged on four days; mouse numbers restart each day;
// one name left unchanged across three positions). Only the folder prefix is replaced.
const RAW: [string, string, string][] = [
  ['20211125', 'M1-2-B6-naive-gBT-uGFP-OTI-CTV-P14-ubTomato-z300_0001', 'A'],
  ['20211125', 'M2-1-B6-naive-gBT-uGFP-OTI-CTV-P14-ubTomato-z280_0004', 'A'],
  ['20211125', 'M2-2-B6-naive-gBT-uGFP-OTI-CTV-P14-ubTomato-z280', 'A'],
  ['20211125', 'M3-1-B6-naive-gBT-uGFP-OTI-CTV-P14-ubTomato-z230_0002', 'A'],
  ['20211125', 'M3-2-B6-naive-gBT-uGFP-OTI-CTV-P14-ubTomato-z230', 'A'],
  ['20211128', 'M1-2-B6-naive-gBT-uGFP-OTI-CTV-P14-ubTomato-z250_0002', 'A'],
  ['20211128', 'M1-3-B6-naive-gBT-uGFP-OTI-CTV-P14-ubTomato-z250_0001', 'A'],
  ['20211128', 'M2-1-B6-naive-gBT-uGFP-OTI-CTV-P14-ubTomato-z230_0002', 'A'],
  ['20211128', 'M2-2-B6-naive-gBT-uGFP-OTI-CTV-P14-ubTomato-z230_0001', 'A'],
  ['20211201', 'M1-1-B6-naive-gBT-CTV-OTI-uGFP-P14-CTDR-z220_0004', 'B'],
  ['20211201', 'M1-2-B6-naive-gBT-CTV-OTI-uGFP-P14-CTDR-z220_0005', 'B'],
  ['20211201', 'M2-1-B6-naive-gBT-CTV-OTI-uGFP-P14-CTDR-z180_0001', 'B'],
  ['20211201', 'M2-1-B6-naive-gBT-CTV-OTI-uGFP-P14-CTDR-z180_0003', 'B'],
  ['20211201', 'M2-1-B6-naive-gBT-CTV-OTI-uGFP-P14-CTDR-z180_0005', 'B'],
  ['20211202', 'M1-1-B6-naive-gBT-CTV-OTI-uGFP-P14-CTDR-z200_0001', 'B'],
  ['20211202', 'M1-4-B6-naive-gBT-CTV-OTI-uGFP-P14-CTDR-z200', 'B'],
]
const IMGS: DesignImage[] = RAW.map(([day, name, panel], i) => ({
  uid: `u${i}`, name, oriPath: `/data/TCELL_TYPES/${day}/${name}.oir`, attr: { Test: panel },
}))

describe('suggestDesign', () => {
  it('reads date from the folder, mouse from the name, the group from an existing attribute', () => {
    const s = suggestDesign(IMGS)
    expect(s.rules.map(r => r.attr)).toEqual(['Date', 'Mouse'])
    expect(s.group).toEqual(['Test'])
    // mouse numbers restart each day, so a mouse is only named within its date
    expect(s.parents).toEqual(['Date', 'Mouse'])
    expect(s.unit).toEqual([])
    const v = applyRules(IMGS, s.rules)
    expect(v.u0).toEqual({ Date: '20211125', Mouse: '1' })
    expect(v.u13).toEqual({ Date: '20211201', Mouse: '2' })
  })

  it('keeps the subject alone when its numbers never repeat across folders', () => {
    const imgs = [['d1', 'M1-1'], ['d1', 'M2-1'], ['d2', 'M3-1'], ['d2', 'M4-1']]
      .map(([d, n], i) => ({ uid: `x${i}`, name: n, oriPath: `/x/${d}/${n}.czi` }))
    const s = suggestDesign(imgs)
    expect(s.rules.map(r => r.attr)).toEqual(['Folder', 'Mouse'])   // "d1" is not a date
    expect(s.parents).toEqual(['Mouse'])
    expect(s.group).toEqual([])                                      // nothing to compare yet
  })

  it('suggests nothing it cannot read: no path, mixed name shapes, attributes already there', () => {
    expect(suggestDesign([{ uid: 'a', name: 'M1-1' }, { uid: 'b', name: 'core_A3' }]).rules).toEqual([])
    const named = IMGS.map(i => ({ ...i, attr: { ...i.attr, Date: 'x', Mouse: 'y' } }))
    expect(suggestDesign(named).rules).toEqual([])
    expect(suggestDesign([])).toEqual({ rules: [], group: [], unit: [], parents: [] })
  })

  it('names the subject by its letter', () => {
    const imgs = ['D1_a', 'D2_a', 'D1_b'].map((n, i) => ({ uid: `d${i}`, name: n }))
    expect(suggestDesign(imgs).rules[0]).toMatchObject({ attr: 'Donor', source: 'name' })
  })
})

describe('run counter and repeated names', () => {
  it('splits the run counter off a name', () => {
    expect(runOf('M2-1-x-z180_0003')).toBe('_0003')
    expect(runOf('M2-1-x-z180_0003.oir')).toBe('_0003')
    expect(runOf('M2-2-x-z280')).toBe('')
    expect(baseName('M2-1-x-z180_0003.oir')).toBe('M2-1-x-z180')
  })

  it('finds one name used for several files in one folder only', () => {
    const g = sameNameGroups(IMGS)
    expect(g).toHaveLength(1)
    expect(g[0].name).toBe('M2-1-B6-naive-gBT-CTV-OTI-uGFP-P14-CTDR-z180')
    expect(g[0].runs).toEqual(['_0001', '_0003', '_0005'])
    // the same name in two folders (two days) is two animals, not a repeat
    expect(sameNameGroups([
      { uid: 'a', name: 'M1-1', oriPath: '/x/d1/M1-1.oir' }, { uid: 'b', name: 'M1-1', oriPath: '/x/d2/M1-1.oir' },
    ])).toEqual([])
  })
})

describe('designReservations', () => {
  const grid = (over: Partial<DesignGrid>): DesignGrid => ({
    cells: [], groups: [], unassigned: [], missingAttrs: [], confounded: [], paired: [],
    planned: { parents: null, units: null }, ...over,
  })
  const design = { ...emptyDesign(), group: ['Panel'], parents: ['Date', 'Mouse'] }
  const g2 = (a: number, b: number) => [
    { group: ['A'], nParents: a, nUnits: 9, nImages: 9 }, { group: ['B'], nParents: b, nUnits: 7, nImages: 7 }]

  it('says what the layout cannot support', () => {
    const r = designReservations(grid({ groups: g2(5, 3), confounded: ['Date'] }), design, sameNameGroups(IMGS))
    expect(r.map(x => x.short)).toEqual([
      'Panel is confounded with Date',
      'Few mice: 5 vs 3',
      '3 files named M2-1-B6-naive-gBT-CTV-OTI-uGFP-P14-CTDR-z180 (_0001, _0003, _0005)',
    ])
    expect(r.every(x => x.level === 'warn')).toBe(true)
  })

  it('names a paired level as information, not a problem', () => {
    const r = designReservations(grid({ groups: g2(6, 6), paired: ['Mouse'] }), design)
    expect(r).toEqual([{ level: 'info', short: 'Paired on Mouse',
                         long: 'Use a paired test, or a two-way ANOVA with Mouse as a factor.' }])
  })

  it('is quiet when the grid holds up', () => {
    expect(designReservations(grid({ groups: g2(6, 5) }), design)).toEqual([])
  })

  it('one group, unassigned images and missing attributes', () => {
    const r = designReservations(grid({
      groups: [{ group: ['A'], nParents: 6, nUnits: 6, nImages: 6 }],
      unassigned: [{ uid: 'a', name: 'beads', missing: ['Panel'] }], missingAttrs: ['Donor'],
    }), design)
    expect(r.map(x => [x.level, x.short])).toEqual([
      ['warn', 'One panel only'], ['info', 'No image has Donor yet'], ['warn', '1 unassigned'],
    ])
  })
})

describe('gridPivot', () => {
  it('a row per group, a column per date, mice with their unit counts', () => {
    const cell = (g: string, d: string, m: string, n: number) =>
      ({ group: [g], parent: [d, m], units: Array.from({ length: n }, (_, i) => ({ unit: [`${m}-${i}`], images: [`${m}${i}`] })) })
    const grid: DesignGrid = {
      cells: [cell('A', '20211125', '1', 1), cell('A', '20211125', '2', 2), cell('B', '20211201', '1', 2)],
      groups: [{ group: ['A'], nParents: 2, nUnits: 3, nImages: 3 }, { group: ['B'], nParents: 1, nUnits: 2, nImages: 2 }],
      unassigned: [], missingAttrs: [], confounded: [], paired: [], planned: { parents: null, units: null },
    }
    const p = gridPivot(grid, { ...emptyDesign(), group: ['Panel'], parents: ['Date', 'Mouse'] })
    expect(p.cols).toEqual(['20211125', '20211201'])
    expect(p.rows[0]).toEqual({ group: 'A', cells: [[{ label: '1', n: 1 }, { label: '2', n: 2 }], []], summary: '2 mice · 3 images' })
    expect(p.rows[1].summary).toBe('1 mouse · 2 images')
    // one parent: one column
    const one = gridPivot(grid, { ...emptyDesign(), group: ['Panel'], parents: ['Mouse'], unit: ['Movie'] })
    expect(one.cols).toEqual([''])
    expect(one.rows[0].cells[0].map(c => c.label)).toEqual(['20211125 1', '20211125 2'])
    expect(one.rows[0].summary).toBe('2 mice · 3 movies')
  })
})
