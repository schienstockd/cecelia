// RESULT-REVIEW: result-2986c8bd
// A spatial task's `graphSuffix` is a valueNameSelection over NEIGHBOUR GRAPHS, not a segmentation:
// a bare path in its pop list must not be read as `<graph name>/…` (the backend puts it on the image's
// active segmentation), or the first chip click rewrites it onto a segmentation that does not exist.
import { it, expect } from 'vitest'
import { popChipValues } from './paramValues'
import type { ParamDef } from './types'

const neighbourStats = [
  { key: 'graphSuffix', type: 'valueNameSelection', field: 'spatialGraphs' },
  { key: 'basisPops', type: 'popSelection', multiple: true, acrossSegmentations: true },
] as ParamDef[]

it('does not read a neighbour graph as the segmentation', () => {
  const v = ['Tcells/qc/A', '/qc/OTI']
  expect(popChipValues(neighbourStats, { graphSuffix: 'cellsGraph', basisPops: v }, v)).toEqual(v)
})
