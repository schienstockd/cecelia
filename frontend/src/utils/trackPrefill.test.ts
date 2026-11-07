// RESULT-REVIEW: result-ca686e89
// A chain edge segmentation → tracking must prefill tracking's `valueName` with the segmentation's
// output. Tracking's picker reads `labelPropsNames` (it needs only the measurement table), while a
// segmentation output lands in `labels`; matching the fields raw dropped the prefill, and the node
// silently tracked the spec default set instead.
import { describe, it, expect } from 'vitest'
import { taskOutput, consumerField, consumerAccepts, normaliseField } from './taskOutput'
import type { TaskDef } from '../tasks/types'

const segDef: TaskDef = {
  fun_name: 'segment.cellpose', task: 'cellpose', label: 'Cellpose', category: 'segment', env: [],
  params: [{ key: 'outputValueName', type: 'valueNameInput', namespace: 'labels', default: 'default' } as any],
}

describe('segmentation → tracking edge prefill', () => {
  it('a labelPropsNames consumer takes a labels output', () => {
    const out = taskOutput(segDef, { outputValueName: 'tcells' })!
    const field = consumerField(out.namespace)!
    expect(out.name).toBe('tcells')
    expect(consumerAccepts('labelPropsNames', field)).toBe(true)
  })

  it('a labelPropsNames consumer is not offered an image version', () => {
    expect(normaliseField('labelPropsNames')).toBe('labelPropsNames')
    expect(consumerAccepts('labelPropsNames', 'filepaths')).toBe(false)
  })

  it('a mask consumer still does not take a table-only output', () => {
    expect(consumerAccepts('labels', 'labelPropsNames')).toBe(false)
    expect(consumerAccepts('labels', 'labels')).toBe(true)
  })
})
