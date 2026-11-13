import { describe, it, expect } from 'vitest'
import { colourByColumns } from './colourByColumns'

describe('colourByColumns', () => {
  it('cell obs columns first, then track ones, once each', () => {
    expect(colourByColumns({ obsColumns: ['hmm', 'clusters.a'], trackColourColumns: ['clusters.a', 'clusters.t'] }))
      .toEqual(['hmm', 'clusters.a', 'clusters.t'])
    expect(colourByColumns({})).toEqual([])
  })
})
