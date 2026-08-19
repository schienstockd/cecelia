import { describe, it, expect } from 'vitest'
import { withChainProducedModels, modelFilename, MODELS_FIELD } from './chainModelOptions'
import type { TaskDef, ParamDef } from '../tasks/types'

const modelSelect = (options: { label: string; value: string }[] = []): ParamDef =>
  ({ key: 'model', label: 'Model', type: 'select', field: MODELS_FIELD, options } as ParamDef)

const coastalDef = (options?: { label: string; value: string }[]): TaskDef => ({
  fun_name: 'segment.coastal', label: 'Coastal', category: 'Segment',
  params: [
    { key: 'valueName', label: 'Images', type: 'valueNameSelection' },
    { key: 'models', label: 'Models', type: 'group', params: [
      modelSelect(options),
      { key: 'cellChannels', label: 'Channels', type: 'channelSelection' },
    ] },
  ],
} as unknown as TaskDef)

const findModel = (def: TaskDef): ParamDef =>
  (def.params.find(p => p.key === 'models')!.params!.find(p => p.key === 'model'))!

describe('modelFilename', () => {
  it('adds .pt to a stem, matching Julia flow_model_filename', () => {
    expect(modelFilename('flow.cytoFg')).toBe('flow.cytoFg.pt')
  })
  it('is idempotent, so a value that is already a filename survives', () => {
    expect(modelFilename('flow.cytoFg.pt')).toBe('flow.cytoFg.pt')
  })
})

describe('withChainProducedModels', () => {
  it('offers a model an upstream node will train, which the vault cannot know about', () => {
    const def = withChainProducedModels(coastalDef(), ['flow.cytoFg'])
    expect(findModel(def).options).toEqual([
      { label: 'flow.cytoFg (trained in this chain)', value: 'flow.cytoFg.pt' },
    ])
  })

  it('appends after the vault entries rather than replacing them', () => {
    const vault = [{ label: 'None', value: '' }, { label: 'flow.cyto', value: 'flow.cyto.pt' }]
    const def = withChainProducedModels(coastalDef(vault), ['flow.cytoFg'])
    expect(findModel(def).options!.map(o => o.value))
      .toEqual(['', 'flow.cyto.pt', 'flow.cytoFg.pt'])
  })

  it('does not duplicate a name already in the vault — retraining onto it is a legitimate overwrite', () => {
    const vault = [{ label: 'flow.cyto', value: 'flow.cyto.pt' }]
    const def = withChainProducedModels(coastalDef(vault), ['flow.cyto'])
    expect(findModel(def).options!.map(o => o.value)).toEqual(['flow.cyto.pt'])
  })

  it('returns the SAME object when there is nothing to add, so a computed does not churn', () => {
    const def = coastalDef([{ label: 'flow.cyto', value: 'flow.cyto.pt' }])
    expect(withChainProducedModels(def, [])).toBe(def)
    expect(withChainProducedModels(def, ['flow.cyto'])).toBe(def)
  })

  it('leaves selects that do not consume the models field alone', () => {
    const def = {
      fun_name: 'x', label: 'x', category: 'x',
      params: [{ key: 'mode', label: 'Mode', type: 'select', options: [{ label: 'a', value: 'a' }] }],
    } as unknown as TaskDef
    expect(withChainProducedModels(def, ['flow.cytoFg'])).toBe(def)
  })
})
