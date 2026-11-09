import { describe, expect, it } from 'vitest'
import { badgeOf, measuringCount, brightZText, cleanPlan, emptyPlan, findingImageUids, planByName, findingLines, findingSeverity, findingsFor, listWords, planProblems, planSentence, reviewBlocked, rowsByDesign, verdictHeading } from './acqCheck'
import { parseAcqResult, type AcqCheckResult, type AcqFinding, type AcqPlan, type AcqRow } from './acqCheckApi'
import type { TaskStatus } from '../stores/tasks'

const designs = [{ id: 'd1', key: 'P14-CTDR · empty-570 · OTI-uGFP · gBT-CTV' }, { id: 'd2', key: 'empty-642 · P14-ubTomato · gBT-uGFP · OTI-CTV' }]
const plan = (): AcqPlan => ({
  ...emptyPlan(designs),
  objects: [{ name: 'P14', kind: 'cells', channels: { d1: 'P14-CTDR', d2: 'P14-ubTomato' } },
            { name: 'OT-I', kind: 'cells', channels: { d1: 'OTI-uGFP', d2: 'OTI-CTV' } },
            { name: 'gBT', kind: 'cells', channels: { d1: 'gBT-CTV', d2: 'gBT-uGFP' } }],
  measure: ['movement'], compare: ['P14', 'OT-I', 'gBT'], expectation: 'naive — expected the same',
})
const row = (uid: string, design: string): AcqRow => ({
  uid, name: uid, design, lasers: '', brightZ: null, brightZRamp: null, probed: true, objects: {},
  ratio: null, dimmest: null, sharedR: null, driftUm: null, registers: null,
})
const result = (over: Partial<AcqCheckResult> = {}): AcqCheckResult => ({
  entryId: '', round: 0, stage: 'test', plan: null, suggestedDesigns: designs, findings: [], rows: [], ran: [], skipped: [],
  nImages: 0, nMeasured: 0, nOpen: 0, review: null, categoryLabels: {}, ...over,
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
    expect(planProblems(q)[0]).toMatch(/^gBT: pick a channel for empty-642/)
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
    expect(g[0].label).toBe('P14-CTDR · empty-570 · OTI-uGFP · gBT-CTV')
  })
  it('puts problems first within a scope', () => {
    const f = (level: 'warn' | 'info' | 'ok', scope = 'set') => ({ level, code: level, short: level, long: '', scope }) as never
    const r = result({ findings: [f('ok'), f('warn', 'img'), f('info'), f('warn')] })
    expect(findingsFor(r, 'set').map(x => x.level)).toEqual(['warn', 'info', 'ok'])
    expect(findingsFor(r, 'img')).toHaveLength(1)
  })
})

describe('brightZText / findingImageUids / measuringCount', () => {
  it('says whether a saved ramp went unused', () => {
    expect(brightZText({ brightZ: true, brightZRamp: '12→15 % over 35 µm' })).toBe('on')
    expect(brightZText({ brightZ: false, brightZRamp: '12→15 % over 35 µm' })).toBe('off · ramp saved')
    expect(brightZText({ brightZ: false, brightZRamp: null })).toBe('off')
    expect(brightZText({ brightZ: null, brightZRamp: null })).toBe('—')
  })
  it('reads the movies off a finding', () => {
    expect(findingImageUids({ detail: { imageUids: ['a', 3, 'b'] } } as never)).toEqual(['a', 'b'])
    expect(findingImageUids(undefined)).toEqual([])
  })
  it('counts this batch’s live tasks only — other funs, other images and finished ones don’t', () => {
    const t = (funName: string, imageUid: string, status: TaskStatus) => ({ funName, imageUid, status })
    const tasks = [t('m', 'a', 'running'), t('m', 'b', 'queued'), t('m', 'c', 'done'), t('x', 'a', 'running'), t('m', 'z', 'running')]
    expect(measuringCount(tasks, 'm', ['a', 'b', 'c'])).toBe(2)
    expect(measuringCount(tasks, 'm', ['c'])).toBe(0)
  })
})

describe('badgeOf', () => {
  it('the badge counts open warnings and asks for a plan when there is none', () => {
    expect(badgeOf(result({ nOpen: 3 })).tip).toMatch(/set a plan/)
    expect(badgeOf(result({ nOpen: 4, plan: plan() }))).toEqual({ n: 4, tip: '4 to look at' })
    expect(badgeOf(null).n).toBe(0)
  })
})

describe('findingSeverity / findingLines', () => {
  const f = (level: 'warn' | 'info' | 'ok', thisData?: string, where?: string, whereLabel?: string) =>
    ({ level, code: 'x', short: 's', long: 'Do this.', scope: 'set', thisData, where, whereLabel }) as never
  it('info carries no severity, so the note claims no verdict', () => {
    expect(findingSeverity(f('info'))).toBeUndefined()
    expect(findingSeverity(f('warn'))).toBe('warn')
  })
  it('lines are labelled by what they hold: the movies per value, what to do, what it means for the analysis', () => {
    expect(findingLines(f('warn'))).toEqual([{ label: 'Next', text: 'Do this.' }])
    expect(findingLines(f('warn', 'Check it.')).map(l => l.label)).toEqual(['Next', 'Analysis'])
    // final data: the action is the analysis one
    expect(findingLines(f('warn'), 'final')).toEqual([{ label: 'Analysis', text: 'Do this.' }])
    // movie lists by image uid unless names are asked for; a list of designs has no uid form
    const m = { ...(f('warn', undefined, '10/10 %: M1-2-long-name') as AcqFinding), whereIds: '10/10 %: aB3dE9' }
    expect(findingLines(m, 'test', false)[0].text).toBe('10/10 %: aB3dE9')
    expect(findingLines(m)[0].text).toBe('10/10 %: M1-2-long-name')
    expect(findingLines(f('info', undefined, 'A: x', 'Designs'), 'test', false)[0].text).toBe('A: x')
    expect(verdictHeading('final')).toBe('Before analysing')
    expect(verdictHeading('test')).toBe('Before acquiring the rest')
    const w = findingLines(f('warn', undefined, '10/10 %: a, b · 8/15 %: c'))
    expect(w[0]).toEqual({ label: 'Movies', text: '10/10 %: a, b · 8/15 %: c' })
    expect(findingLines(f('info', undefined, 'x', 'Designs'))[0].label).toBe('Designs')
  })
})

describe('planByName', () => {
  it('turns an index into the design channel name, leaves names alone', () => {
    const ds = [{ id: 'd1', key: 'k', channels: [{ index: 0, name: 'P14-CTDR', filter: null }] }]
    const p = { ...emptyPlan(ds), objects: [{ name: 'P14', channels: { d1: 0 } }, { name: 'X', channels: { d1: 'OTI' } }] }
    const r = planByName(p, ds)
    expect(r.objects[0].channels.d1).toBe('P14-CTDR')
    expect(r.objects[1].channels.d1).toBe('OTI')
  })
  it('says why the review can’t run: AI off (and by whom), no plan, already running', () => {
    const on = { enabled: true, setBy: null } as const
    expect(reviewBlocked({ ai: on, hasPlan: true, running: false })).toBe('')
    expect(reviewBlocked({ ai: on, hasPlan: false, running: false })).toBe('Save a plan first')
    expect(reviewBlocked({ ai: on, hasPlan: true, running: true })).toBe('Reviewing…')
    expect(reviewBlocked({ ai: { enabled: false, setBy: 'profile' }, hasPlan: true, running: false })).toBe('AI features are off')
    expect(reviewBlocked({ ai: { enabled: false, setBy: 'workstation' }, hasPlan: true, running: false }))
      .toBe('AI features are off — turned off for this workstation')
  })
  it('reads the review and its category labels off a check reply', () => {
    expect(parseAcqResult(null)).toBeNull()
    expect(parseAcqResult({ error: 'x' })).toBeNull()
    const none = parseAcqResult({ entryId: '', findings: [], rows: [] })!
    expect(none.review).toBeNull()
    expect(none.categoryLabels).toEqual({})
    const r = parseAcqResult({ entryId: 'bb-1', categoryLabels: { drift: 'Drift', other: 'Other' },
      review: { status: 'done', model: 'sonnet', claims: [
        { kind: 'observation', category: 'drift', text: 'M1 shifts', action: 'Stabilise', sectionId: 's01' },
        { kind: 'interpretation', text: 'Tracking works on m2.' }] } })!
    expect(r.review?.items.map(x => [x.category, x.action, x.sectionId])).toEqual([['drift', 'Stabilise', 's01']])
    expect(r.review?.supports).toBe('Tracking works on m2.')
    expect(r.categoryLabels.drift).toBe('Drift')
  })
})
