import { describe, it, expect } from 'vitest'
import { summariseCohortResult, type CohortDoc } from './cohortCheck'
import { cohortFunsFor, COHORT_STAGES } from './cohortStages'

describe('summariseCohortResult', () => {
  it('all clear → ok, message names the cohort size', () => {
    const docs: CohortDoc[] = [{ funName: 'segment.cellpose', nIncluded: 10, metrics: { nCells: { outliers: {} } } }]
    const r = summariseCohortResult(docs)
    expect(r.severity).toBe('ok')
    expect(r.flagged).toBe(0)
    expect(r.message).toBe('All 10 images within range')
  })

  it('outliers → warn; distinct images counted once across metrics + funs', () => {
    const docs: CohortDoc[] = [
      { funName: 'segment.cellpose', nIncluded: 10, metrics: {
        nCells: { outliers: { KDIeEm: { value: 100 } } } } },
      { funName: 'segment.measureLabels', nIncluded: 10, metrics: {
        nCells: { outliers: { KDIeEm: { value: 98 } } } } },   // same image, second fun → still 1
    ]
    const r = summariseCohortResult(docs)
    expect(r.severity).toBe('warn')
    expect(r.flagged).toBe(1)
    expect(r.message).toBe('1 image flagged — see lab log')
  })

  it('two distinct flagged images → plural', () => {
    const r = summariseCohortResult([{ nIncluded: 5, metrics: {
      nClusters: { outliers: { a: {} } }, largestClusterFrac: { outliers: { b: {} } } } }])
    expect(r.flagged).toBe(2)
    expect(r.message).toBe('2 images flagged — see lab log')
  })

  it('empty / missing fields are safe', () => {
    expect(summariseCohortResult([]).severity).toBe('ok')
    expect(summariseCohortResult([{}]).flagged).toBe(0)
  })
})

describe('cohortStages', () => {
  it('maps cohort-bearing modules to their fun_names; others empty', () => {
    expect(cohortFunsFor('segment')).toContain('segment.cellpose')
    expect(cohortFunsFor('clustPops')).toEqual(['clustPops.cluster'])
    expect(cohortFunsFor('gate')).toEqual([])          // no cohort metrics
    expect(cohortFunsFor(undefined)).toEqual([])
  })
  it('every mapped fun is dotted category.task', () => {
    for (const funs of Object.values(COHORT_STAGES))
      for (const f of funs) expect(f).toMatch(/^[a-zA-Z]+\.[a-zA-Z_]+$/)
  })
})
