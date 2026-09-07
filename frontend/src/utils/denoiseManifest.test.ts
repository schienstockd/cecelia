// The denoise details modal (`DenoiseModelDetails.vue`) uses `denoiseModelDetailGroups` to render
// a manifest as labelled groups — parallel to `FlowModelDetails` / `modelDetailGroups`. If a manifest
// key from the trainer (`train_support_denoise_run.py`) is not rendered, the modal is silently
// missing a field, so the shape is worth pinning.
import { describe, it, expect } from 'vitest'
import { denoiseModelDetailGroups, denoiseTrainingSeries, type DenoiseManifest }
  from './denoiseManifest'

const flatFields = (m: DenoiseManifest | null | undefined) =>
  denoiseModelDetailGroups(m).flatMap(g => g.fields.map(f => f.label))

describe('denoiseModelDetailGroups', () => {
  it('returns [] for a missing manifest — the modal shows the "no manifest" message instead', () => {
    expect(denoiseModelDetailGroups(null)).toEqual([])
    expect(denoiseModelDetailGroups(undefined)).toEqual([])
  })

  it('renders the arch keys the runner reads back at inference (coastal.support.build_model)', () => {
    const m: DenoiseManifest = {
      kind: 'denoise-support',
      channels: ['CH1', 'CH2'],
      arch: {
        inputFrames: 61, patchXY: 128,
        midChannels: [32, 64, 128, 256], depth: 4, blindConvChannels: 64,
        oneByOneChannels: [32, 16], lastLayerChannels: [64, 32, 16], bsSize: [3, 3], bp: false,
      },
      training: {
        imageUids: ['aaa', 'bbb'], valueName: 'driftCorrected', channelIndices: [0, 1],
        epochs: 20, batchSize: 2, learningRate: 5e-4, midZOnly: true, framesPerImage: [100, 120],
        epochLosses: [0.65, 0.62, 0.60], finalLoss: 0.60, firstLoss: 0.65, lossDrop: 1.083,
      },
    }
    const labels = flatFields(m)
    for (const k of ['Temporal window', 'Patch size', 'UNet mid channels', 'UNet depth',
                     'Blind-spot channels', '1×1 channels', 'Last-layer channels',
                     'Blind-spot kernel', 'Bit-plane']) {
      expect(labels).toContain(k)
    }
    for (const k of ['Images', 'Input version', 'Channel indices', 'Epochs', 'Batch size',
                     'Learning rate', 'Middle Z only', 'Frames per image',
                     'Final loss', 'Loss drop', 'Loss samples']) {
      expect(labels).toContain(k)
    }
  })

  it('drops absent fields (an old manifest missing a later key does not render a row of dashes)', () => {
    const m: DenoiseManifest = { kind: 'denoise-support', channels: ['CH1'] }
    const labels = flatFields(m)
    // With no `arch` and no `training` the Architecture and Training groups are omitted altogether.
    expect(labels).toEqual(['Kind', 'Channels'])
  })

  it('any manifest key not in the KNOWN set falls through to "Other" (forward-compatible)', () => {
    // A future trainer might add `provenance: {...}` — the modal should still show it rather than
    // pretending it isn't there.
    const m = { kind: 'denoise-support', channels: ['CH1'], provenance: 'v1.2' } as
      unknown as DenoiseManifest
    const groups = denoiseModelDetailGroups(m)
    const other = groups.find(g => g.label === 'Other')
    expect(other?.fields.map(f => f.label)).toContain('provenance')
  })
})

describe('denoiseTrainingSeries', () => {
  it('is empty when the manifest has no training block', () => {
    expect(denoiseTrainingSeries(null)).toEqual([])
    expect(denoiseTrainingSeries({ kind: 'denoise-support' })).toEqual([])
  })

  it('pooled: one series from training.epochLosses, term "loss"', () => {
    const m: DenoiseManifest = { kind: 'denoise-support',
      training: { epochLosses: [0.65, 0.62, 0.60] } }
    const s = denoiseTrainingSeries(m)
    expect(s).toHaveLength(1)
    expect(s[0].term).toBe('loss')
    expect(s[0].values).toEqual([0.65, 0.62, 0.60])
  })

  it('perChannel bundle: one series per channel keyed by channel name', () => {
    // Chip row above the plot auto-shows for >1 series (FlowTrainingView.vue), so this shape is
    // what turns into three toggles for supp.MERTK's nuc-GFP / mem-TOM / CD169-Kat.
    const m: DenoiseManifest = { kind: 'denoise-support',
      training: { perChannelLosses: {
        'nuc-GFP':   [0.9, 0.7, 0.5],
        'mem-TOM':   [0.8, 0.6, 0.4],
        'CD169-Kat': [1.1, 1.0, 0.9] } } }
    const s = denoiseTrainingSeries(m)
    expect(s.map(x => x.term).sort()).toEqual(['CD169-Kat', 'mem-TOM', 'nuc-GFP'])
    const kat = s.find(x => x.term === 'CD169-Kat')
    expect(kat?.values).toEqual([1.1, 1.0, 0.9])
    expect(kat?.weight).toBe(1)
    expect(kat?.floored).toBe(false)
  })

  it('perChannel bundle: an empty per-channel curve is filtered out, not drawn as an empty line', () => {
    const m: DenoiseManifest = { kind: 'denoise-support',
      training: { perChannelLosses: { good: [0.5, 0.4], empty: [] } } }
    const s = denoiseTrainingSeries(m)
    expect(s.map(x => x.term)).toEqual(['good'])
  })

  it('perChannel takes precedence when BOTH fields are present (bundle manifest is the source)', () => {
    // The trainer never writes both, but defending against a hand-edited manifest is cheap; a
    // bundle claiming to be perChannel must not silently render the pooled fallback.
    const m: DenoiseManifest = { kind: 'denoise-support',
      training: { epochLosses: [1, 1, 1],
                  perChannelLosses: { A: [0.5, 0.4] } } }
    const s = denoiseTrainingSeries(m)
    expect(s.map(x => x.term)).toEqual(['A'])
  })

  it('perChannel with all channels empty falls back to the pooled curve (defensive)', () => {
    const m: DenoiseManifest = { kind: 'denoise-support',
      training: { epochLosses: [0.5, 0.4],
                  perChannelLosses: { A: [], B: [] } } }
    const s = denoiseTrainingSeries(m)
    expect(s.map(x => x.term)).toEqual(['loss'])
  })
})
