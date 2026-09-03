import { describe, it, expect } from 'vitest'
import { paramsFromManifest, unmappedFields, flowManifestParams,
         type FlowModelEntry } from './flowModelParams'
import type { FlowManifest } from './flowManifest'
import type { ParamDef } from '../tasks/types'

// the real shape, from `flow.cyto.json` in the dev vault
const M: FlowManifest = {
  temporalScales: [1, 2, 4, 8],
  cumulativeWindow: 5,
  cropSize: 256,
  zSpacing: 2,
  zPlanes: 10,
  epochs: 100,
  trainRatio: 0.8,
  maxFrames: 60,
  normalise: 99.99,
  embeddingDim: 16,
  foregroundWeight: 1.0,
  temporalWeight: 2.0,
  channelName: 'mem-TOM',
  sourceValueName: 'smoothed',
  droppedMetrics: ['divergence', 'flow_structure_alignment', 'vorticity'],
  metricKeys: ['acceleration', 'mag_1', 'mag_2', 'mag_4', 'mag_8', 'strain'],
} as FlowManifest

const OFFERED = ['acceleration', 'cell_boundary_likelihood', 'cumulative_mag', 'direction_stability',
                 'divergence', 'edge_strength', 'flow_structure_alignment', 'normal_flow', 'strain',
                 'tangential_flow', 'vorticity']

describe('paramsFromManifest', () => {
  it('copies the params recorded under the same key', () => {
    const p = paramsFromManifest(M, OFFERED)
    expect(p.cropSize).toBe(256)
    expect(p.epochs).toBe(100)
    expect(p.zSpacing).toBe(2)
    expect(p.normalise).toBe(99.99)
    // an Advanced-section param, emitted flat — buildParamValues reads either
    expect(p.embeddingDim).toBe(16)
    expect(p.temporalWeight).toBe(2)
  })

  it('stringifies the temporal scales, because chipSelect validates strings', () => {
    expect(paramsFromManifest(M, OFFERED).temporalScales).toEqual(['1', '2', '4', '8'])
  })

  it('rebuilds the metric set from the EXCLUSIONS, not from metricKeys', () => {
    // metricKeys holds the derived mag_<scale> planes, which are not options — using it would put
    // values in the chip list that the spec rejects
    const p = paramsFromManifest(M, OFFERED)
    expect(p.flowMetrics).not.toContain('mag_1')
    expect(p.flowMetrics).not.toContain('divergence')
    expect(p.flowMetrics).toContain('acceleration')
    expect(p.flowMetrics).toContain('strain')
    expect((p.flowMetrics as string[]).length).toBe(OFFERED.length - 3)
  })

  it('picks up a metric added to the form since the model was trained', () => {
    // the exclusion list is what was said NO to; anything new defaults to in
    const p = paramsFromManifest(M, [...OFFERED, 'brand_new_metric'])
    expect(p.flowMetrics).toContain('brand_new_metric')
  })

  it('turns the joined channel name back into the names the picker holds', () => {
    expect(paramsFromManifest(M, OFFERED).trainChannels).toEqual(['mem-TOM'])
    expect(paramsFromManifest({ ...M, channelName: 'mem-TOM+GFP' }, OFFERED).trainChannels)
      .toEqual(['mem-TOM', 'GFP'])
  })

  it('uses the channel NAME, never the recorded indices — those mean nothing on another image', () => {
    const p = paramsFromManifest({ ...M, trainChannels: [2] } as FlowManifest, OFFERED)
    expect(p.trainChannels).toEqual(['mem-TOM'])
  })

  it('restores which version of the image was read', () => {
    expect(paramsFromManifest(M, OFFERED).valueName).toBe('smoothed')
  })

  it('carries the intensity weight — a model trained at 1.0 must not come back as the default', () => {
    // This is the bug an audit caught before it shipped: while `intensityWeight` was off the form,
    // the mapper dropped it, so "use memTom's settings" produced a form that trained at a different
    // weight with no warning. memTom is the only model on disk where it diverges.
    expect(paramsFromManifest({ ...M, intensityWeight: 1.0 } as FlowManifest, OFFERED).intensityWeight)
      .toBe(1.0)
    expect(paramsFromManifest({ ...M, intensityWeight: 0 } as FlowManifest, OFFERED).intensityWeight)
      .toBe(0)
  })

  it('never fills in the model name or overwrite — that would target the model being copied', () => {
    const p = paramsFromManifest(M, OFFERED)
    expect(p.modelName).toBeUndefined()
    expect(p.overwrite).toBeUndefined()
  })

  it('omits what a pre-field manifest does not have, rather than inventing it', () => {
    const p = paramsFromManifest({ epochs: 30 } as FlowManifest, OFFERED)
    expect(p).toEqual({ epochs: 30 })
  })

  it('is empty for no manifest at all', () => {
    expect(paramsFromManifest(null)).toEqual({})
    expect(paramsFromManifest(undefined)).toEqual({})
  })

  it('leaves the metric chips alone when the form offered no options to reconcile against', () => {
    expect(paramsFromManifest(M, []).flowMetrics).toBeUndefined()
  })
  // ── the scale mode ───────────────────────────────────────────────────────────────────────────
  // The mode, and NOT the spans. The spans are not a form field: they are `temporalScales x the
  // reference interval`, so restoring the lags has already restored them. Handing back a second,
  // independently-stored copy is how the two would come to disagree.
  it('restores the mode, and reads the spans back through the lags', () => {
    const p = paramsFromManifest(
      { ...M, temporalScaleUnit: 's', temporalScaleSeconds: [5, 10, 20, 40],
        temporalReferenceInterval: 5, cumulativeWindowSeconds: 25 } as FlowManifest, OFFERED)
    expect(p.temporalScaleMode).toBe('seconds')
    // 5, 10, 20, 40 s at the model's own 5 s/frame reference IS lags 1, 2, 4, 8.
    expect(p.temporalScales).toEqual(['1', '2', '4', '8'])
    expect(p.temporalScaleSeconds).toBeUndefined()
    expect(p.cumulativeWindowSeconds).toBeUndefined()
  })

  it('sets the mode explicitly for a frame-lag model, so a stale form does not carry over', () => {
    const p = paramsFromManifest({ ...M, temporalScaleUnit: 'frames' } as FlowManifest, OFFERED)
    expect(p.temporalScaleMode).toBe('frames')
    expect(p.temporalScaleSeconds).toBeUndefined()
  })

  // Every model trained before the mode existed. It has no opinion, so the form keeps its default.
  it('says nothing about the mode for a model that predates it', () => {
    const p = paramsFromManifest(M, OFFERED)
    expect('temporalScaleMode' in p).toBe(false)
    expect(p.temporalScales).toEqual(['1', '2', '4', '8'])
  })

})

describe('unmappedFields', () => {
  it('is empty when the manifest carries everything', () => {
    expect(unmappedFields(M)).toEqual([])
  })

  it('names what an older manifest cannot supply, so the UI can say so', () => {
    expect(unmappedFields({ epochs: 30 } as FlowManifest))
      .toEqual(['channels', 'image version', 'metrics'])
  })
})

// The two-layer picker fallback: when /api/tasks/funparams answers matched=false, the modelName
// SuggestInput reaches for the vault manifest instead — same source the vault UI uses, so both
// paths agree on the same input. See TaskRunner.onParamCommit.
describe('flowManifestParams', () => {
  const DEF: ParamDef[] = [
    { key: 'flowMetrics', type: 'chipSelect', multiple: true,
      options: OFFERED.map(v => ({ value: v, label: v })) } as unknown as ParamDef,
  ]
  const models: FlowModelEntry[] = [
    { name: 'flow.small.pt', stem: 'flow.small', hasManifest: true, manifest: M },
    { name: 'flow.other.pt', stem: 'flow.other', hasManifest: false, manifest: {} as FlowManifest },
  ]

  it('resolves the model by stem (what the picker holds), copying the manifest params', () => {
    const p = flowManifestParams(models, 'flow.small', DEF)
    expect(p?.epochs).toBe(100)
    expect(p?.temporalScales).toEqual(['1', '2', '4', '8'])
  })

  it('accepts the .pt filename too, so a caller that carries either shape lands here', () => {
    expect(flowManifestParams(models, 'flow.small.pt', DEF)?.epochs).toBe(100)
  })

  it('is null for a name that is not in the vault', () => {
    expect(flowManifestParams(models, 'nope', DEF)).toBeNull()
  })

  it('is null for a vault entry with no manifest (an orphan .pt)', () => {
    expect(flowManifestParams(models, 'flow.other', DEF)).toBeNull()
  })

  it('is null when the vault is unreachable', () => {
    expect(flowManifestParams(null, 'flow.small', DEF)).toBeNull()
  })

  it('reads the metric options from THIS spec, so a spec that grows an option still selects it', () => {
    const wider: ParamDef[] = [{ key: 'flowMetrics', type: 'chipSelect', multiple: true,
      options: [...OFFERED, 'new_metric'].map(v => ({ value: v, label: v })) } as unknown as ParamDef]
    expect(flowManifestParams(models, 'flow.small', wider)?.flowMetrics).toContain('new_metric')
  })
})
