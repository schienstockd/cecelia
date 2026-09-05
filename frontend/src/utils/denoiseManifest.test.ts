// The denoise details modal (`DenoiseModelDetails.vue`) uses `denoiseModelDetailGroups` to render
// a manifest as labelled groups — parallel to `FlowModelDetails` / `modelDetailGroups`. If a manifest
// key from the trainer (`train_support_denoise_run.py`) is not rendered, the modal is silently
// missing a field, so the shape is worth pinning.
import { describe, it, expect } from 'vitest'
import { denoiseModelDetailGroups, type DenoiseManifest } from './denoiseManifest'

const flatFields = (m: DenoiseManifest | null | undefined) =>
  denoiseModelDetailGroups(m).flatMap(g => g.fields.map(f => f.label))

describe('denoiseModelDetailGroups', () => {
  it('returns [] for a missing manifest — the modal shows the "no manifest" message instead', () => {
    expect(denoiseModelDetailGroups(null)).toEqual([])
    expect(denoiseModelDetailGroups(undefined)).toEqual([])
  })

  it('renders the arch keys the runner reads back at inference (denoise_run._build_model)', () => {
    const m: DenoiseManifest = {
      kind: 'denoise-support',
      channelName: 'CH1',
      arch: {
        inputFrames: 61, patchXY: 128,
        midChannels: [32, 64, 128, 256], depth: 4, blindConvChannels: 64,
        oneByOneChannels: [32, 16], lastLayerChannels: [64, 32, 16], bsSize: [3, 3], bp: false,
      },
      training: {
        imageUids: ['aaa', 'bbb'], valueName: 'driftCorrected', channel: 0,
        epochs: 20, batchSize: 2, learningRate: 5e-4, midZOnly: true, framesPerImage: [100, 120],
      },
    }
    const labels = flatFields(m)
    for (const k of ['Temporal window', 'Patch size', 'UNet mid channels', 'UNet depth',
                     'Blind-spot channels', '1×1 channels', 'Last-layer channels',
                     'Blind-spot kernel', 'Bit-plane']) {
      expect(labels).toContain(k)
    }
    for (const k of ['Images', 'Input version', 'Channel index', 'Epochs', 'Batch size',
                     'Learning rate', 'Middle Z only', 'Frames per image']) {
      expect(labels).toContain(k)
    }
  })

  it('drops absent fields (an old manifest missing a later key does not render a row of dashes)', () => {
    const m: DenoiseManifest = { kind: 'denoise-support', channelName: 'CH1' }
    const labels = flatFields(m)
    // With no `arch` and no `training` the Architecture and Training groups are omitted altogether.
    expect(labels).toEqual(['Kind', 'Channel'])
  })

  it('any manifest key not in the KNOWN set falls through to "Other" (forward-compatible)', () => {
    // A future trainer might add `provenance: {...}` — the modal should still show it rather than
    // pretending it isn't there.
    const m = { kind: 'denoise-support', channelName: 'CH1', provenance: 'v1.2' } as
      unknown as DenoiseManifest
    const groups = denoiseModelDetailGroups(m)
    const other = groups.find(g => g.label === 'Other')
    expect(other?.fields.map(f => f.label)).toContain('provenance')
  })
})
