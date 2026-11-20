// RESULT-REVIEW: result-3651babf
// A `neq` cluster population whose value was stored as a STRING ("3" — the API keeps filter_values as
// sent) holds every cluster: the engine's `x != "3"` is true for each numeric id. The cluster map must not
// draw cluster 3 grey as "other".
import { describe, it, expect } from 'vitest'
import { clusterPopIds } from './clusterMeasure'

describe('clusterPopIds never coerces a stored string', () => {
  const universe = [1, 2, 3]
  it('neq "3" holds every cluster', () => {
    expect(clusterPopIds({ fun: 'neq', values: '3' }, universe)).toEqual([1, 2, 3])
  })
  it('an ordering or eq against a string holds none', () => {
    expect(clusterPopIds({ fun: 'gt', values: '1' }, universe)).toEqual([])
    expect(clusterPopIds({ fun: 'eq', values: '3' }, universe)).toEqual([])
  })
})
