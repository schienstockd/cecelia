import { describe, expect, it } from 'vitest'
import { MAX_DRAWN_CLUSTERS, tooManyClustersText, tooManyClustersToDraw } from './clusterLimit'

describe('clusterLimit', () => {
  it('draws up to the limit and refuses past it', () => {
    expect(tooManyClustersToDraw(MAX_DRAWN_CLUSTERS)).toBe(false)
    expect(tooManyClustersToDraw(MAX_DRAWN_CLUSTERS + 1)).toBe(true)
  })
  it('names the count', () => {
    expect(tooManyClustersText(9700)).toBe(`${(9700).toLocaleString()} clusters — too many to label`)
  })
})
