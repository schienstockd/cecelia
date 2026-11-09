import { describe, expect, it } from 'vitest'
import { badgeOf, cleanPlan, emptyPlan, findingsFor, formatDepth, listWords, planProblems, planSentence, rowsByDesign } from './acqCheck'
import type { AcqCheckResult, AcqPlan, AcqRow } from './acqCheckApi'

const designs = [{ id: 'd1', key: 'gBT-CTV-OTI-uGFP-P14-CTDR' }, { id: 'd2', key: 'gBT-uGFP-OTI-CTV-P14-ubTomato' }]
const plan = (): AcqPlan => ({
  ...emptyPlan(designs),
  objects: [{ name: 'P14', kind: 'cells', channels: { d1: 0, d2: 1 } },
            { name: 'OT-I', kind: 'cells', channels: { d1: 2, d2: 3 } },
            { name: 'gBT', kind: 'cells', channels: { d1: 3, d2: 2 } }],
  measure: ['movement'], compare: ['P14', 'OT-I', 'gBT'], expectation: 'naive — expected the same',
})
const row = (uid: string, design: string): AcqRow => ({
  uid, name: uid, design, lasers: '', brightZ: null, probed: true, depthPct: null, objects: {},
  ratio: null, dimmest: null, sharedR: null, driftUm: null, registers: null,
})
const result = (over: Partial<AcqCheckResult> = {}): AcqCheckResult => ({
  entryId: '', round: 0, plan: null, suggestedDesigns: designs, findings: [], rows: [], ran: [], skipped: [],
  nImages: 0, nMeasured: 0, nOpen: 0, ...over,
})

describe('planSentence', () => {
  it('reads the plan back as one sentence', () => {
    expect(planSentence(plan())).toBe('Compare movement of P14, OT-I and gBT (naive — expected the same)')
  })
  it('one object is measured, not compared; no plan says nothing', () => {
    expect(planSentence({ ...plan(), objects: [plan().objects[0]], compare: [], expectation: '' })).toBe('Measure movement of P14')
    expect(planSentence(null)).toBe('')
  })
  it('lists words', () => { expect(listWords(['a'])).toBe('a'); expect(listWords(['a', 'b'])).toBe('a and b') })
})

describe('planProblems / cleanPlan', () => {
  it('a full plan saves', () => { expect(planProblems(plan())).toEqual([]) })
  it('needs an object, unique names, and a channel in every design', () => {
    expect(planProblems(emptyPlan(designs))).toContain('Add at least one object')
    const p = plan(); p.objects[1].name = 'P14'
    expect(planProblems(p)).toContain('Two objects share a name')
    const q = plan(); delete q.objects[2].channels.d2
    expect(planProblems(q)[0]).toMatch(/^gBT: pick a channel for gBT-uGFP/)
  })
  it('drops unnamed rows and compares only objects that exist', () => {
    const p = plan(); p.objects.push({ name: '  ', channels: {} }); p.compare.push('ghost')
    const c = cleanPlan(p)
    expect(c.objects).toHaveLength(3)
    expect(c.compare).toEqual(['P14', 'OT-I', 'gBT'])
    expect(c.text).toBe(planSentence(plan()))
  })
})

describe('rowsByDesign / findingsFor', () => {
  it('groups rows in the designs order', () => {
    const r = result({ rows: [row('b', 'd2'), row('a', 'd1'), row('c', 'd2')] })
    const g = rowsByDesign(r)
    expect(g.map(x => x.design)).toEqual(['d1', 'd2'])
    expect(g[1].rows.map(x => x.uid)).toEqual(['b', 'c'])
    expect(g[0].label).toBe('gBT-CTV-OTI-uGFP-P14-CTDR')
  })
  it('puts problems first within a scope', () => {
    const f = (level: 'warn' | 'info' | 'ok', scope = 'set') => ({ level, code: level, short: level, long: '', scope }) as never
    const r = result({ findings: [f('ok'), f('warn', 'img'), f('info'), f('warn')] })
    expect(findingsFor(r, 'set').map(x => x.level)).toEqual(['warn', 'info', 'ok'])
    expect(findingsFor(r, 'img')).toHaveLength(1)
  })
})

describe('formatDepth / badgeOf', () => {
  it('writes the depth range top to bottom with real minus signs', () => {
    expect(formatDepth([-17.2, -3.4])).toBe('−3 to −17 %')
    expect(formatDepth([0, 9])).toBe('+9 to 0 %')
    expect(formatDepth([-5, -5])).toBe('−5 %')
    expect(formatDepth(null)).toBe('—')
  })
  it('the badge counts open warnings and asks for a plan when there is none', () => {
    expect(badgeOf(result({ nOpen: 3 })).tip).toMatch(/set a plan/)
    expect(badgeOf(result({ nOpen: 4, plan: plan() }))).toEqual({ n: 4, tip: '4 to look at' })
    expect(badgeOf(null).n).toBe(0)
  })
})
