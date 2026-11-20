// RESULT-REVIEW: result-5d4c5930
// A highlighted cluster population that doesn't select its values ("Motile" = clusters.movement neq 3)
// must be drawn with the clusters it HOLDS — every one but 3 — not with none (legend n = 0, all of its
// cells grey as "other") and not with 3. Counts are the cluster map legend's arithmetic: the pop's n is
// the summed size of its clusters, "other" the rest.
import { describe, it, expect } from 'vitest'
import { clusterPopIds } from './clusterMeasure'

describe('clusterPopIds over the run universe (cluster map legend)', () => {
  const sizes: Record<number, number> = { 1: 100, 2: 50, 3: 10 }
  const universe = [1, 2, 3]
  const legend = (ids: number[]) => {
    const n = ids.reduce((a, id) => a + (sizes[id] ?? 0), 0)
    return { n, other: 160 - n }
  }
  it('a neq 3 pop holds 1 and 2: n = 150, only cluster 3 is other', () => {
    const ids = clusterPopIds({ fun: 'neq', values: 3 }, universe)
    expect(ids).toEqual([1, 2])
    expect(legend(ids)).toEqual({ n: 150, other: 10 })
  })
  it('gt / lte select by value', () => {
    expect(clusterPopIds({ fun: 'gt', values: 1 }, universe)).toEqual([2, 3])
    expect(clusterPopIds({ fun: 'lte', values: 2 }, universe)).toEqual([1, 2])
  })
  it('an in pop is unchanged by the universe', () => {
    expect(clusterPopIds({ fun: 'in', values: [3] }, universe)).toEqual([3])
  })
})
