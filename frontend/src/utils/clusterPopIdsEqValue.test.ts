// RESULT-REVIEW: result-354cc783
// An `eq` cluster population holds what the gating engine's `x == vals` selects, the stored value compared
// as is: `eq 3` holds cluster 3; `eq [3]` (a list) equals no id, so it holds none. The cluster map must
// draw those, not the reverse (n = 0 for `eq 3`, cluster 3's cells for `eq [3]`).
import { describe, it, expect } from 'vitest'
import { clusterPopIds } from './clusterMeasure'

describe('clusterPopIds on an eq population agrees with the engine', () => {
  const universe = [1, 2, 3]
  it('eq 3 holds cluster 3', () => {
    expect(clusterPopIds({ fun: 'eq', values: 3 }, universe)).toEqual([3])
    expect(clusterPopIds({ fun: 'eq', values: 3 })).toEqual([3])
  })
  it('eq [3] holds no cluster: a list is never equal to an id', () => {
    expect(clusterPopIds({ fun: 'eq', values: [3] }, universe)).toEqual([])
    expect(clusterPopIds({ fun: 'eq', values: [3] })).toEqual([])
  })
})
