import { describe, expect, it } from 'vitest'
import {
  applyRules, assignRole, attrRole, designSketch, setAttrRole, baseName, dateLabel, designChange, designCompareApply, designExpectPhrase, designMark, questionChanged, withQuestion, designPlotTip, designReplicateAttrs, followsDesign, replicateNoun, roleOf, designDraft, designReservations, designSummary, emptyDesign, gridPivot, runOf, sameNameGroups, suggestDesign,
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
    expect(s.rules.map(r => r.attr)).toEqual(['Date', 'Mouse', 'Position'])
    expect(s.group).toEqual(['Test'])
    // mouse numbers restart each day, so a mouse is only named within its date
    expect(s.parents).toEqual(['Date', 'Mouse'])
    expect(s.unit).toEqual(['Position'])                 // M1-2 = mouse 1, position 2
    const v = applyRules(IMGS, s.rules)
    expect(v.u0).toEqual({ Date: '20211125', Mouse: '1', Position: '2' })
    expect(v.u13).toEqual({ Date: '20211201', Mouse: '2', Position: '1' })
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
    expect(suggestDesign(named).rules.map(r => r.attr)).toEqual(['Position'])   // only what is missing
    expect(suggestDesign([])).toEqual({ rules: [], group: [], unit: [], parents: [] })
  })

  it('takes the roles from attributes a set already carries', () => {
    // the same set after a first confirm: Panel, Date, Mouse and Position are attributes, no rules needed
    const done = IMGS.map(i => {
      const [, day, mouse, pos] = i.oriPath!.match(/\/(\d{8})\/M(\d+)-(\d+)/)!
      return { ...i, oriPath: null, attr: { Panel: i.attr!.Test, Date: day, Mouse: mouse, Position: pos } }
    })
    const s = suggestDesign(done)
    expect(s.rules).toEqual([])
    expect(s).toMatchObject({ group: ['Panel'], parents: ['Date', 'Mouse'], unit: ['Position'] })
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
  const cell = (g: string, d: string, m: string, n: number) =>
    ({ group: [g], parent: [d, m], units: Array.from({ length: n }, (_, i) => ({ unit: [`${i}`], images: [`${m}${i}`] })) })
  const grid: DesignGrid = {
    cells: [cell('A', '20211125', '1', 1), cell('A', '20211125', '2', 2), cell('B', '20211201', '1', 2)],
    groups: [{ group: ['A'], nParents: 2, nUnits: 3, nImages: 3 }, { group: ['B'], nParents: 1, nUnits: 2, nImages: 2 }],
    unassigned: [], missingAttrs: [], confounded: [], paired: [], planned: { parents: null, units: null },
  }

  it('a row per group, a column per date, each mouse with its positions', () => {
    const p = gridPivot(grid, { ...emptyDesign(), group: ['Panel'], parents: ['Date', 'Mouse'], unit: ['Position'] })
    expect(p.caption).toBe('Which mice each panel has on each date')
    expect(p.colAttr).toBe('Date')
    expect(p.cols).toEqual(['20211125', '20211201'])
    expect(p.colLabels).toEqual(['25 Nov 2021', '1 Dec 2021'])
    expect(p.rows[0]).toEqual({
      group: 'A',
      cells: [[{ label: 'Mouse 1', count: '1 position' }, { label: 'Mouse 2', count: '2 positions' }], []],
      summary: '2 mice · 3 positions',
    })
    expect(p.rows[1].summary).toBe('1 mouse · 2 positions')
  })

  it('one parent: one column; no unit: images', () => {
    const one = gridPivot(grid, { ...emptyDesign(), group: ['Panel'], parents: ['Mouse'] })
    expect(one.cols).toEqual([''])
    expect(one.colAttr).toBe('')
    expect(one.caption).toBe('Which mice each panel has')
    expect(one.rows[0].cells[0].map(c => c.label)).toEqual(['Mouse 20211125', 'Mouse 20211125'])
    expect(one.rows[0].summary).toBe('2 mice · 3 images')
  })

  it('reads dates, and leaves what is not one alone', () => {
    expect(dateLabel('2021-12-02')).toBe('2 Dec 2021')
    expect(dateLabel('211125')).toBe('211125')        // YYMMDD or DDMMYY: cannot tell
    expect(dateLabel('20211399')).toBe('20211399')
    expect(dateLabel('d1')).toBe('d1')
  })
})

describe('the Metadata design section and the dialog draft', () => {
  const sug = { group: ['Test'], unit: [], parents: ['Date', 'Mouse'] }
  const designed = { ...emptyDesign(), group: ['Panel'], parents: ['Mouse'], status: 'confirmed' as const }
  const explo = { ...emptyDesign(), question: 'first look', mode: 'exploratory' as const, status: 'confirmed' as const }

  it('summarises each state', () => {
    expect(designSummary(null)).toEqual({ state: 'none', tip: 'Add the metadata, then set up the design' })
    expect(designSummary(explo).state).toBe('exploratory')
    expect(designSummary(designed)).toEqual({ state: 'confirmed', tip: 'Design: Panel in Mouse' })
    expect(designSummary({ ...designed, status: 'inferred' }).tip).toBe('Design: Panel in Mouse (not confirmed)')
  })

  it('opens a new design on the suggestion, an existing one on itself', () => {
    expect(designDraft(null, sug)).toMatchObject({ mode: 'designed', group: ['Test'], parents: ['Date', 'Mouse'] })
    const d = designDraft(designed, sug)
    expect(d).toEqual(designed)
    d.group.push('x'); expect(designed.group).toEqual(['Panel'])         // a copy, not the stored arrays
  })

  it('promotes an exploratory set: designed, its question kept, roles from the suggestion when it has none', () => {
    expect(designDraft(explo, sug)).toMatchObject({ question: 'first look', mode: 'designed', group: ['Test'], parents: ['Date', 'Mouse'] })
    expect(designDraft({ ...explo, group: ['Dose'] }, sug).group).toEqual(['Dose'])
  })

  it('notes the change in the history', () => {
    expect(designChange(null, designed)).toBe('confirmed')
    expect(designChange(null, explo)).toBe('exploratory')
    expect(designChange(explo, designed)).toBe('promoted to designed')
    expect(designChange(designed, explo)).toBe('made exploratory')
    expect(designChange(designed, { ...designed, group: ['Dose'] })).toBe('edited')
  })
})

describe('the three questions', () => {
  const d0 = { ...emptyDesign(), group: ['Panel'], parents: ['Date', 'Mouse'], unit: [] as string[] }
  it('reads and writes the replicate, what names it, and what sits inside', () => {
    expect(roleOf(d0, 'replicate')).toEqual(['Mouse'])
    expect(roleOf(d0, 'namedWithin')).toEqual(['Date'])
    const d = assignRole(d0, 'inside', ['Position'])
    expect(d).toMatchObject({ group: ['Panel'], parents: ['Date', 'Mouse'], unit: ['Position'] })
  })
  it('takes an attribute from its old role when it is picked for another', () => {
    // the mix-up in the first version: Mouse picked as the inside level while it was the replicate
    const d = assignRole(d0, 'inside', ['Mouse'])
    expect(d.parents).toEqual([])            // no replicate left, so nothing to name it within
    expect(d.unit).toEqual(['Mouse'])
    expect(assignRole(d0, 'replicate', ['Date']).parents).toEqual(['Date'])   // Date leaves "named within"
  })
  it('names the replicate within its qualifiers, outermost first', () => {
    const d = assignRole({ ...emptyDesign(), parents: ['Mouse'] }, 'namedWithin', ['Date'])
    expect(d.parents).toEqual(['Date', 'Mouse'])
    expect(assignRole(emptyDesign(), 'namedWithin', ['Date']).parents).toEqual([])
  })
})

describe('plots follow the design', () => {
  const d = { ...emptyDesign(), group: ['Panel'], parents: ['Date', 'Mouse'], status: 'confirmed' as const }
  const attrs = ['Date', 'Mouse', 'Panel']
  const fresh = { compareMode: 'per_image', compareAttr: '', compareAttr2: '' }

  it('groups an untouched canvas by the compared attribute, once', () => {
    expect(designCompareApply(fresh, d, attrs))
      .toEqual({ compareMode: 'by_attr', compareAttr: 'Panel', compareAttr2: '', designApplied: true })
    expect(designCompareApply({ ...fresh, designApplied: true }, d, attrs)).toBeNull()   // the user went back to per image
  })

  it('keeps a compare the user chose, and marks it', () => {
    expect(designCompareApply({ compareMode: 'by_attr', compareAttr: 'Date', compareAttr2: '' }, d, attrs))
      .toEqual({ designApplied: true })
  })

  it('waits while the design or its attribute is missing', () => {
    expect(designCompareApply(fresh, null, attrs)).toBeNull()
    expect(designCompareApply(fresh, { ...d, mode: 'exploratory' }, attrs)).toBeNull()
    expect(designCompareApply(fresh, d, ['Date', 'Mouse'])).toBeNull()               // no image has Panel yet
  })

  it('names the replicate from the last parent', () => {
    expect(designReplicateAttrs(d)).toEqual(['Date', 'Mouse'])
    expect(designReplicateAttrs({ ...d, parents: [] })).toEqual([])
    expect(replicateNoun(['Date', 'Mouse'])).toEqual({ one: 'mouse', many: 'mice' })
    expect(designPlotTip(d)).toBe('From the design: Panel, n = mice')
  })

  it('shows the bullseye whenever the design applies, amber when the row differs', () => {
    expect(designMark(d, 'by_attr', ['Panel'], attrs)?.state).toBe('follows')
    expect(designMark(d, 'by_attr', ['Date'], attrs))
      .toEqual({ state: 'differs', tip: 'The design compares Panel — click to use it' })
    expect(designMark(d, 'per_image', [], attrs)?.state).toBe('differs')
    expect(designMark(d, 'by_attr', ['Date'], ['Date', 'Mouse'])).toBeNull()       // no image has Panel
    expect(designMark(null, 'by_attr', ['Date'], attrs)).toBeNull()
  })

  it('marks the compare row only when it shows the design group', () => {
    expect(followsDesign(d, 'by_attr', ['Panel'])).toBe(true)
    expect(followsDesign(d, 'by_attr', ['Panel', 'Date'])).toBe(false)
    expect(followsDesign(d, 'per_image', [])).toBe(false)
  })
})

describe('the acquisition plan asks the design question', () => {
  it('words the expectation like the backend', () => {
    expect(designExpectPhrase({ expect: 'same', expectIn: '' })).toBe('the same')
    expect(designExpectPhrase({ expect: 'higher', expectIn: 'A' })).toBe('higher in A')
    expect(designExpectPhrase({ expect: 'lower', expectIn: '' })).toBe('')      // no group named yet
    expect(designExpectPhrase({ expect: 'unsure', expectIn: 'A' })).toBe('')
    expect(designExpectPhrase(null)).toBe('')
  })
  it('writes the question into the design, keeping its roles', () => {
    const d = { ...emptyDesign(), group: ['Panel'], parents: ['Mouse'], status: 'confirmed' as const }
    const q = { question: ' Faster in A? ', expect: 'higher' as const, expectIn: 'A' }
    expect(withQuestion(d, q)).toEqual({ ...d, question: 'Faster in A?', expect: 'higher', expectIn: 'A' })
    expect(withQuestion(null, { ...q, expect: 'same' })).toEqual({ ...emptyDesign(), question: 'Faster in A?', expect: 'same', expectIn: '' })
    expect(questionChanged(d, { question: '', expect: 'unsure', expectIn: '' })).toBe(false)
    expect(questionChanged(null, { question: '', expect: 'unsure', expectIn: 'A' })).toBe(false)  // a stray group, not an expectation
    expect(questionChanged(d, q)).toBe(true)
  })
})

describe('step 2: one role per attribute, and the sketch', () => {
  const d0 = { ...emptyDesign(), group: ['Panel'], parents: ['Date', 'Mouse'], unit: ['Position'] }
  it('reads and sets one role per attribute', () => {
    expect(['Panel', 'Mouse', 'Date', 'Position', 'Run'].map(a => attrRole(d0, a)))
      .toEqual(['group', 'replicate', 'namedWithin', 'inside', ''])
    // a new n replaces the old one, which leaves the design
    const d1 = setAttrRole(d0, 'Position', 'replicate')
    expect(d1.parents).toEqual(['Date', 'Position']); expect(d1.unit).toEqual([])
    expect(attrRole(d1, 'Mouse')).toBe('')
    // out of the design; moving an attribute takes it from its old role
    expect(setAttrRole(d0, 'Panel', '').group).toEqual([])
    const d2 = setAttrRole(d0, 'Date', 'group')
    expect(d2.group).toEqual(['Panel', 'Date']); expect(d2.parents).toEqual(['Mouse'])
  })
  it('draws each group with its replicates and what sits inside them', () => {
    const cell = (g: string, p: string[], n: number) =>
      ({ group: [g], parent: p, units: Array.from({ length: n }, (_, i) => ({ unit: [String(i + 1)], images: [`${g}${p}${i}`] })) })
    const grid = { cells: [cell('A', ['25', '1'], 2), cell('A', ['25', '2'], 1), cell('B', ['28', '1'], 3)],
                   groups: [], unassigned: [], missingAttrs: [], confounded: [], paired: [], planned: { parents: null, units: null } }
    const s = designSketch(grid, d0)!
    expect(s.groups.map(g => [g.label, g.reps.map(r => r.inside)])).toEqual([['A', [2, 1]], ['B', [3]]])
    expect(s.caption).toBe('Panel: A vs B · n = 2 vs 1 mice')
    expect(s.insideNoun).toBe('positions')
    // no replicate: each image is the n
    const s2 = designSketch(grid, { ...d0, parents: [] })!
    expect(s2.groups[0]!.reps).toHaveLength(3); expect(s2.repNoun.many).toBe('images')
    expect(designSketch(grid, d0, 1)!.groups[0]!.more).toBe(1)
  })
})
