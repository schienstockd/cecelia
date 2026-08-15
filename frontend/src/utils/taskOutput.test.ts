import { describe, it, expect } from 'vitest'
import { taskOutput, consumerField, normaliseField, isValueNameNamespace } from './taskOutput'
import { previewValueName } from './taskPreview'
import type { TaskDef } from '../tasks/types'

const def = (over: Partial<TaskDef>): TaskDef => ({
  fun_name: 'x.y', task: 'y', label: 'Y', category: 'x', env: [], params: [], ...over,
})

describe('taskOutput', () => {
  it('reads a FIXED output declared on the def', () => {
    const d = def({ outputValueName: 'smoothed', outputField: 'filepath' })
    expect(taskOutput(d, {})).toEqual({ name: 'smoothed', namespace: 'filepaths' })
  })

  it('a fixed output beats a param — the task always writes there', () => {
    const d = def({
      outputValueName: 'smoothed',
      params: [{ key: 'outputValueName', label: 'n', type: 'text', namespace: 'labels' }],
    })
    expect(taskOutput(d, { outputValueName: 'ignored' })?.name).toBe('smoothed')
  })

  it('reads a user-set param via its declared namespace, whatever the key is called', () => {
    const d = def({
      params: [{ key: 'valueNameSuffix', label: 'n', type: 'valueNameInput', namespace: 'clusters' }],
    })
    expect(taskOutput(d, { valueNameSuffix: 'immune' }))
      .toEqual({ name: 'immune', namespace: 'clusters' })
  })

  it('falls back to the param default when nothing is entered yet', () => {
    const d = def({
      params: [{ key: 'graphSuffix', label: 'n', type: 'valueNameInput',
                 namespace: 'spatialGraphs', default: 'default' }],
    })
    expect(taskOutput(d, {})).toEqual({ name: 'default', namespace: 'spatialGraphs' })
  })

  it('finds a param nested inside a group/section', () => {
    const d = def({
      params: [{ key: 'sec', label: 'S', type: 'section', params: [
        { key: 'statsSuffix', label: 'n', type: 'valueNameInput', namespace: 'stats' },
      ] }],
    })
    expect(taskOutput(d, { statsSuffix: 'contacts' })?.namespace).toBe('stats')
  })

  it('still resolves a pre-registry `outputValueName` param as labels', () => {
    const d = def({ params: [{ key: 'outputValueName', label: 'n', type: 'text' }] })
    expect(taskOutput(d, { outputValueName: 'Tcell' }))
      .toEqual({ name: 'Tcell', namespace: 'labels' })
  })

  it('is null when the task declares no output of its own', () => {
    expect(taskOutput(def({ params: [{ key: 'valueName', label: 'v', type: 'valueNameSelection' }] }), {}))
      .toBeNull()
    expect(taskOutput(undefined, { outputValueName: 'x' })).toBeNull()
  })

  it('ignores an empty string — a cleared field is not an output name', () => {
    const d = def({ params: [{ key: 'outputValueName', label: 'n', type: 'text' }] })
    expect(taskOutput(d, { outputValueName: '' })).toBeNull()
  })

  it('rejects an unknown namespace rather than trusting the spec', () => {
    expect(isValueNameNamespace('labels')).toBe(true)
    expect(isValueNameNamespace('nonsense')).toBe(false)
    const d = def({
      params: [{ key: 'k', label: 'n', type: 'valueNameInput',
                 namespace: 'nonsense' as never, default: 'v' }],
    })
    expect(taskOutput(d, {})).toBeNull()
  })
})

describe('consumerField', () => {
  it('maps the namespaces a consumer param can read', () => {
    expect(consumerField('labels')).toBe('labels')
    expect(consumerField('filepaths')).toBe('filepaths')
    expect(consumerField('spatialGraphs')).toBe('spatialGraphs')
  })

  it('maps the suffix namespaces to the payload fields they are listed under', () => {
    // deliberately named nothing like the namespace — the payload field is where the frontend READS
    // names from, which is a different vocabulary to what a task WRITES into
    expect(consumerField('stats')).toBe('statsSuffixes')
    expect(consumerField('clusters')).toBe('clusterSuffixes')
    expect(consumerField('regions')).toBe('regionSuffixes')
  })

  it('reports null for a namespace no IMAGE field can name', () => {
    // `models` is the global vault, so its suggestions arrive as injected spec options instead;
    // the rest name nothing a task writes through a valueNameInput today. The old `normField`
    // collapsed all of these to 'filepath', which would prefill an image-version picker with a
    // cluster suffix.
    for (const ns of ['models', 'tracks', 'branches', 'obsCols'] as const)
      expect(consumerField(ns)).toBeNull()
  })
})

describe('normaliseField — the singular/plural trap', () => {
  // Two spellings are already on disk and differ by one letter: consumer params say
  // `field: "filepaths"`, a fixed output says `outputField: "filepath"`. Compared raw they never
  // match, and the chain propagation silently does nothing.
  it('collapses both spellings of the image-version field', () => {
    expect(normaliseField('filepaths')).toBe('filepaths')
    expect(normaliseField('filepath')).toBe('filepaths')
  })

  it('passes the suffix fields through', () => {
    for (const f of ['statsSuffixes', 'clusterSuffixes', 'regionSuffixes'] as const)
      expect(normaliseField(f)).toBe(f)
  })

  it('treats an absent field as the image-version field, like paramValues does', () => {
    expect(normaliseField(undefined)).toBe('filepaths')
    expect(normaliseField(null)).toBe('filepaths')
  })

  it('passes the other two through', () => {
    expect(normaliseField('labels')).toBe('labels')
    expect(normaliseField('spatialGraphs')).toBe('spatialGraphs')
  })

  it('round-trips against consumerField, so a producer and a consumer can be compared', () => {
    for (const ns of ['labels', 'filepaths', 'spatialGraphs',
                      'stats', 'clusters', 'regions'] as const) {
      const f = consumerField(ns)
      expect(f).not.toBeNull()
      expect(normaliseField(f)).toBe(f)
    }
  })
})

describe('previewValueName still answers its own question', () => {
  // The preview layer stem is NOT "the task's output" — it falls back to the INPUT version for a task
  // with no output of its own, and must always be a string. Delegating the shared half must not
  // change either property. (Ported from taskPreview.test.ts, which pins the same cases.)
  const cellpose = def({ params: [{ key: 'outputValueName', label: 'n', type: 'text' }] })
  const afCorrect = def({ params: [{ key: 'valueName', label: 'v', type: 'valueNameSelection' }] })

  it('prefers the output name when the task has one', () => {
    expect(previewValueName(cellpose, { valueName: 'corrected', outputValueName: 'Tcell' }))
      .toBe('Tcell')
  })

  it('falls back to the input version for a task with no output name', () => {
    expect(previewValueName(afCorrect, { valueName: 'corrected' })).toBe('corrected')
  })

  it('falls back to "default" rather than returning empty', () => {
    expect(previewValueName(afCorrect, {})).toBe('default')
    expect(previewValueName(cellpose, { outputValueName: '' })).toBe('default')
  })
})
