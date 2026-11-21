// RESULT-REVIEW: result-ea238aac
// A cluster pop stored as `neq [3]` (a LIST, the shape a raw pop/add sends) is drawn with the cells the
// gating engine actually puts in it. `_filter_mask` compares each id with the stored value as is
// (`x != [3]`), which is true for every id — so the population holds ALL clusters, 3 included, and the
// cluster map must not paint cluster 3 grey as "other" (sizes {1:100, 2:50, 3:10} → n = 160, other 0).
import { describe, it, expect } from 'vitest'
import { clusterPopIds } from './clusterMeasure'

describe('clusterPopIds agrees with the engine on a list value', () => {
  const universe = [1, 2, 3]
  it('neq [3] holds every cluster, as x != [3] is true for every id', () => {
    expect(clusterPopIds({ fun: 'neq', values: [3] }, universe)).toEqual([1, 2, 3])
  })
  it('an ordering against a list owns nothing (the engine cannot order an id against a list)', () => {
    expect(clusterPopIds({ fun: 'gt', values: [1] }, universe)).toEqual([])
  })
})
