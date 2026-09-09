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
  channelIndices?: number[]
  epochs?: number
  batchSize?: number
  learningRate?: number
  midZOnly?: boolean
  framesPerImage?: number[]
  // Loss curve travels with the model (banked here + in the QC sidecar). The Training convergence
  // plot reads these — a model imported from another project has no run log to fall back on.
  epochLosses?: number[]
  // PerChannel bundles (SUPPORT_PERCHANNEL_PLAN.md D3) — one curve per trained channel, keyed by
  // channel NAME. The Training convergence plot detects this and draws one series per key; the
  // chip row auto-shows when there is more than one series, so each channel becomes a toggle.
  // Pooled bundles carry only `epochLosses` and leave this undefined.
  perChannelLosses?: Record<string, number[]>
  // Sub-epoch loss trace — one point per gradient step, thinned by the runner to at most ~5000
  // points regardless of dataset size. `stepIndices[i]` is the 1-based gradient step for
  // `stepLosses[i]`; not every step is logged (adaptive stride), so the arrays are dense but the
  // step indices are not consecutive. The Training convergence plot's Detail view reads these to
  // render a log(step) view — SUPPORT typically converges within the first ~100 gradient steps,
  // and per-epoch means bury that so a converged run looks like a flat line.
  // Pooled bundles carry `stepLosses`/`stepIndices`; perChannel bundles carry per-channel dicts.
  stepLosses?: number[]
  stepIndices?: number[]
  perChannelStepLosses?: Record<string, number[]>
  perChannelStepIndices?: Record<string, number[]>
  // Early-stop signals. `stoppedEarly` is true when the trainer broke out before the requested
  // epoch budget (see coastal.support.train_support patience/min_delta); `stopEpoch` is the
  // 1-based epoch it actually stopped at, and `epochBudget` is what the user asked for. The
  // Training convergence plot's Detail view annotates the stop; the QC page suppresses the
  // "loss stayed flat" warn when early-stop caught the plateau (plateau is expected here).
  // PerChannel bundles carry BOTH the aggregate (any/max across channels) AND per-channel dicts.
  stoppedEarly?: boolean
  stopEpoch?: number
  epochBudget?: number
  perChannelStoppedEarly?: Record<string, boolean>
  perChannelStopEpoch?: Record<string, number>
  finalLoss?: number
  firstLoss?: number
  lossDrop?: number
}

export interface DenoiseManifest {
  kind: 'denoise-support'
  // A denoise model pools N channels into one training run (DENOISE_INTEGRATION_PLAN.md D3
  // amendment, measured on fXgbTl 2026-09-05); the list is what the vault label reads.
  channels?: string[]
  arch?: DenoiseArch
  training?: DenoiseTraining
}

/** One (term, values) pair the Training convergence plot consumes. `weight` and `floored` mirror
 *  the flow shape so both kinds flow through the same rendering path (`FlowTrainingView.vue`).
 *  `steps` + `stepValues` carry the sub-epoch trace for the Detail view; undefined for old models
 *  (they fall back to the per-epoch curve). */
export interface DenoiseSeries {
  term: string
  values: number[]
  weight: number
  floored: boolean
  steps?: number[]
  stepValues?: number[]
}

/**
 * Series for the Training convergence plot from a denoise manifest — pooled → one series
 * (`training.epochLosses`, term `"loss"`); perChannel bundle → one series PER CHANNEL from
 * `training.perChannelLosses`, term = channel name (matches the vault picker's channel labels, and
 * shows up as the chip label above the plot). Extracted from the SFC per the frontend test rule
 * — Vue templates aren't tested; the logic is.
 */
export function denoiseTrainingSeries(m: DenoiseManifest | null | undefined): DenoiseSeries[] {
  const tr = m?.training
  if (!tr) return []
  const perCh = tr.perChannelLosses
  if (perCh) {
    const perChSteps    = tr.perChannelStepLosses  ?? {}
    const perChStepsIdx = tr.perChannelStepIndices ?? {}
    const entries = Object.entries(perCh)
      .filter(([, v]) => Array.isArray(v) && v.length > 0)
      .map(([name, values]) => ({
        term: name, values, weight: 1, floored: false,
        stepValues: perChSteps[name],
        steps:      perChStepsIdx[name],
      }))
    if (entries.length > 0) return entries
  }
  const losses = tr.epochLosses ?? []
  if (!losses.length) return []
  return [{ term: 'loss', values: losses, weight: 1, floored: false,
            stepValues: tr.stepLosses, steps: tr.stepIndices }]
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
    'kind', 'channels', 'arch', 'training',
  ])
  const other = Object.entries(m).filter(([k]) => !known.has(k))
    .map(([k, v]) => field(k, v, true))

  // Number of loss points is user-relevant (matches `training.epochs` when the run wasn't cancelled);
  // the raw list is rendered by the Training convergence plot, so no need to dump the vector here.
  const nLosses = Array.isArray(tr.epochLosses) ? tr.epochLosses.length : null

  return [
    { label: 'Model', fields: filter([
        field('Kind', m.kind),
        field('Channels', m.channels),
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
        field('Channel indices', tr.channelIndices, true),
        field('Epochs',       tr.epochs),
        field('Batch size',   tr.batchSize),
        field('Learning rate', tr.learningRate),
        field('Middle Z only', tr.midZOnly),
        field('Frames per image', tr.framesPerImage, true),
        field('Final loss',   tr.finalLoss),
        field('Loss drop',    tr.lossDrop),
        field('Loss samples', nLosses),
    ])},
    ...(other.length ? [{ label: 'Other', fields: filter(other) }] : []),
  ].filter(g => g.fields.length)
}
