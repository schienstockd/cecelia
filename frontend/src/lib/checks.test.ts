import { describe, it, expect } from 'vitest'
import { checkLook, acqCheckView, qcCheckView, cohortCheckView, cohortFindingsOf, designCheckView, type CohortState } from './checks'
import { emptyDesign, type DesignGrid } from '../utils/experimentDesign'
import { SEVERITY } from './severity'
import type { AcqCheckResult } from '../utils/acqCheckApi'

describe('checkLook', () => {
  it('draws a verdict with the severity icon AND colour, never colour alone', () => {
    for (const s of ['ok', 'warn', 'fail'] as const) {
      expect(checkLook(s)).toEqual({ icon: SEVERITY[s].icon, color: SEVERITY[s].color })
    }
  })
  it('draws the non-verdicts uncoloured', () => {
    expect(checkLook('none')).toEqual({ icon: 'pi-minus' })
    expect(checkLook('busy').color).toBeUndefined()
  })
})

describe('acqCheckView', () => {
  const res = (o: Partial<AcqCheckResult>) => ({ nOpen: 0, plan: null, ...o }) as AcqCheckResult
  it('none before anything is known, and with no plan and nothing open', () => {
    expect(acqCheckView(null).state).toBe('none')
    expect(acqCheckView(res({})).state).toBe('none')
  })
  it('warn with the open count', () => {
    expect(acqCheckView(res({ nOpen: 3 }))).toMatchObject({ state: 'warn', count: 3 })
  })
  it('ok once a plan was checked clean', () => {
    expect(acqCheckView(res({ plan: {} as AcqCheckResult['plan'] })).state).toBe('ok')
  })
})

describe('qcCheckView', () => {
  const warn = { 'x/default': { findings: [{ level: 'warn' as const, code: 'x', short: 's', long: 'l' }] } }
  const clean = { 'x/default': { findings: [] } }
  it('none before any QC ran', () => {
    expect(qcCheckView([{ uid: 'a' }], false)).toMatchObject({ state: 'none', count: 0 })
  })
  it('ok when QC ran and flagged nothing', () => {
    expect(qcCheckView([{ uid: 'a', qc: clean }], false).state).toBe('ok')
  })
  it('warn with the count of included flagged images; an excluded one is not counted', () => {
    const v = qcCheckView([{ uid: 'a', qc: warn }, { uid: 'b', qc: warn, included: false }], false)
    expect(v).toMatchObject({ state: 'warn', count: 1 })
  })
  it('the tip says what a click does in each state', () => {
    expect(qcCheckView([{ uid: 'a', qc: warn }], false).tip).toMatch(/Show only/)
    expect(qcCheckView([{ uid: 'a', qc: warn }], true).tip).toBe('Show all images')
  })
})

describe('cohortCheckView', () => {
  const st = (o: Partial<CohortState>): CohortState =>
    ({ checked: true, checkedAt: 1, flaggedUids: [], nIncluded: 6, funs: [], ...o })
  it('none when never checked', () => {
    expect(cohortCheckView(null).state).toBe('none')
    expect(cohortCheckView(st({ checked: false })).state).toBe('none')
  })
  it('warn with the flagged count, ok when clean, busy while checking', () => {
    expect(cohortCheckView(st({ flaggedUids: ['a', 'b'] }))).toMatchObject({ state: 'warn', count: 2 })
    expect(cohortCheckView(st({})).state).toBe('ok')
    expect(cohortCheckView(st({}), true).state).toBe('busy')
  })
})

describe('cohortFindingsOf', () => {
  const f = (code: string, level: 'info' | 'warn' = 'warn') => ({ level, code, short: code, long: '' })
  const img = { uid: 'a', qc: {
    'cohort.segment.cellpose/default': { funName: 'cohort.segment.cellpose', findings: [f('cohort.nCells')] },
    'gating.flow/default': { funName: 'gating.flow', findings: [f('gating.x'), f('gating.i', 'info')] },
    'segment.cellpose/default': { funName: 'segment.cellpose', findings: [f('seg.own')] },
  } }
  it('reads the cohort docs for these funs only — not the task own QC, not info', () => {
    expect(cohortFindingsOf(img, ['segment.cellpose']).map(x => x.code)).toEqual(['cohort.nCells'])
    expect(cohortFindingsOf(img, ['gating.flow']).map(x => x.code)).toEqual(['gating.x'])
  })
})

describe('designCheckView', () => {
  const confirmed = { ...emptyDesign(), status: 'confirmed' as const, group: ['Panel'], parents: ['Date', 'Mouse'] }
  const grid = (unassigned: DesignGrid['unassigned'] = [], missingAttrs: string[] = []): DesignGrid =>
    ({ cells: [], groups: [], unassigned, missingAttrs, confounded: ['Date'], paired: [], planned: { parents: null, units: null } })
  it('says nothing before it loads or on a set with no images', () => {
    expect(designCheckView(null, null, false, 5).state).toBe('none')
    expect(designCheckView(null, null, true, 0).state).toBe('none')
  })
  it('warns with no design, or one not confirmed', () => {
    expect(designCheckView(null, null, true, 5).state).toBe('warn')
    expect(designCheckView({ ...confirmed, status: 'inferred' }, grid(), true, 5)).toEqual({ state: 'warn', count: 0, tip: 'Design not confirmed' })
  })
  it('an exploratory set has nothing to check', () => {
    expect(designCheckView({ ...confirmed, mode: 'exploratory' }, grid(), true, 5).state).toBe('none')
  })
  it('counts images added since that sit outside the design', () => {
    const v = designCheckView(confirmed, grid([{ uid: 'a', name: 'x', missing: ['Mouse'] }, { uid: 'b', name: 'y', missing: ['Panel'] }]), true, 7)
    expect(v).toEqual({ state: 'warn', count: 2, tip: '2 images outside the design' })
  })
  it('names a design attribute no image has', () => {
    expect(designCheckView(confirmed, grid([], ['Mouse']), true, 5)).toEqual({ state: 'warn', count: 1, tip: 'No image has Mouse' })
  })
  it('a confounded group accepted at confirm is not re-raised', () => {
    expect(designCheckView(confirmed, grid(), true, 5).state).toBe('ok')
  })
})
