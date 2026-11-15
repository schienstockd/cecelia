// RESULT-REVIEW: result-dc75eabd
// An across-segmentation pop picker with no Segmentation param (HMM's `pops`) shows a saved list as
// stored: a bare path's segmentation is the image's ACTIVE one on the backend, which the form cannot
// know — prefixing it with another entry's segmentation would rewrite the run onto the wrong cells.
import { it, expect } from 'vitest'
import { popChipValues } from './paramValues'
import type { ParamDef } from './types'

const hmm = [{ key: 'pops', type: 'popSelection', multiple: true, acrossSegmentations: true }] as ParamDef[]
const tracking = [{ key: 'valueName', type: 'valueNameSelection', field: 'labelPropsNames' },
                  { key: 'popsToTrack', type: 'popSelection', multiple: true, acrossSegmentations: true }] as ParamDef[]

it('leaves a mixed list unchanged when there is no Segmentation param', () => {
  const v = ['Bcells/_tracked', '/qc/OTI/_tracked']
  expect(popChipValues(hmm, { pops: v }, v)).toEqual(v)
})

it('reads bare paths on the Segmentation param when there is one', () => {
  expect(popChipValues(tracking, { valueName: 'Tcells' }, ['/qc/OTI', 'Tcells/qc/gBT']))
    .toEqual(['Tcells/qc/OTI', 'Tcells/qc/gBT'])
  expect(popChipValues(tracking, { valueName: 'Tcells' }, 'NONE')).toEqual([])
  expect(popChipValues(tracking, { valueName: 'Tcells' }, '/qc/OTI')).toEqual(['Tcells/qc/OTI'])
})

it('keeps an old single-picker string visible without a Segmentation param', () => {
  expect(popChipValues(hmm, {}, '/qc/OTI')).toEqual(['/qc/OTI'])
  expect(popChipValues(hmm, {}, 'NONE')).toEqual([])
})
