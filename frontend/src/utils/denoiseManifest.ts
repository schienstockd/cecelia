// Denoise-model manifest, the read-only mirror of what `train_support_denoise_run.py` writes to
// `<name>.json` beside `<name>.pt` in `<config_dir>/models/denoiseModels/`. The runtime contract
// lives in `app/src/tasks/cleanupImages/denoise_run.py::_build_model` (arch keys) and in
// `train_support_denoise_run.py` (training block); this file only names the same keys so the details
// modal can render them.

import type { DetailField, DetailGroup } from './flowManifest'

/**
 * The denoise runner reads `arch.*` to reconstruct the SUPPORT network at inference time — the
 * modal shows every one of these so a user can tell whether a saved model still matches the frame
 * count and patch size they are about to run against.
 */
export interface DenoiseArch {
  inputFrames?: number
  patchXY?: number
  midChannels?: number[]
  depth?: number
  blindConvChannels?: number
  oneByOneChannels?: number[]
  lastLayerChannels?: number[]
  bsSize?: number[]
  bp?: boolean
}

/** What went into a training run — parallel to FlowManifest's own training block. */
export interface DenoiseTraining {
  imageUids?: string[]
  valueName?: string
  channel?: number
  epochs?: number
  batchSize?: number
  learningRate?: number
  midZOnly?: boolean
  framesPerImage?: number[]
}

export interface DenoiseManifest {
  kind: 'denoise-support'
  channelName?: string
  arch?: DenoiseArch
  training?: DenoiseTraining
}

const field = (label: string, value: unknown, mono = false): DetailField | null => {
  if (value === undefined || value === null) return null
  const text = Array.isArray(value) ? value.join(', ') : String(value)
  return text === '' ? null : { label, value: text, mono }
}

const filter = (fs: (DetailField | null)[]): DetailField[] => fs.filter((x): x is DetailField => !!x)

/**
 * Detail groups for the `<i>` modal. Same shape and same rendering shell as `FlowModelDetails` uses,
 * so a manifest that grew a new field is not silently invisible — anything unmapped falls through to
 * **Other** below.
 */
export function denoiseModelDetailGroups(m: DenoiseManifest | null | undefined): DetailGroup[] {
  if (!m) return []
  const arch = m.arch ?? {}
  const tr   = m.training ?? {}

  const known = new Set([
    'kind', 'channelName', 'arch', 'training',
  ])
  const other = Object.entries(m).filter(([k]) => !known.has(k))
    .map(([k, v]) => field(k, v, true))

  return [
    { label: 'Model', fields: filter([
        field('Kind', m.kind),
        field('Channel', m.channelName),
    ])},
    { label: 'Architecture', fields: filter([
        field('Temporal window',   arch.inputFrames),
        field('Patch size',        arch.patchXY),
        field('UNet mid channels', arch.midChannels, true),
        field('UNet depth',        arch.depth),
        field('Blind-spot channels', arch.blindConvChannels),
        field('1×1 channels',      arch.oneByOneChannels, true),
        field('Last-layer channels', arch.lastLayerChannels, true),
        field('Blind-spot kernel',   arch.bsSize, true),
        field('Bit-plane',         arch.bp),
    ])},
    { label: 'Training', fields: filter([
        field('Images',       tr.imageUids, true),
        field('Input version', tr.valueName),
        field('Channel index', tr.channel),
        field('Epochs',       tr.epochs),
        field('Batch size',   tr.batchSize),
        field('Learning rate', tr.learningRate),
        field('Middle Z only', tr.midZOnly),
        field('Frames per image', tr.framesPerImage, true),
    ])},
    ...(other.length ? [{ label: 'Other', fields: filter(other) }] : []),
  ].filter(g => g.fields.length)
}
