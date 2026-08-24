import type { FlowManifest } from './flowManifest'
import type { ParamValues } from '../tasks/types'

/**
 * A trained model's manifest, read back as the form that produced it — "that one looks good, but I
 * want to tweak it".
 *
 * This is possible because the manifest is very nearly the param set already: 13 of the 17 controls in
 * `opticalFlow/train.json` are recorded under the SAME key. So the interesting part is not the copying,
 * it is the four that are not, and the two that must not be copied at all.
 *
 * **Not the same as the existing param restore.** Typing a name into a `valueNameInput` restores from
 * `/api/tasks/funparams` — the run log of THIS project. That cannot work for a model somebody else
 * trained, or one fetched from a vault, which is exactly the case a published model is. The manifest
 * travels with the `.pt`; the run log does not.
 *
 * Emitted FLAT, including the params that live inside the Advanced section: `buildParamValues` reads a
 * section param from the section object or the flat bag (`savedSection[k] ?? saved[k]`), and it walks
 * the spec, so anything here the spec no longer offers is dropped rather than smuggled into a run.
 */
export function paramsFromManifest(
  m: FlowManifest | null | undefined, offeredMetrics: string[] = [],
): ParamValues {
  if (!m) return {}
  const out: ParamValues = {}
  const put = (k: string, v: unknown) => { if (v !== undefined && v !== null) out[k] = v }

  // ── recorded under the same key, copied as-is
  // `intensityWeight` is in this list for a reason worth stating: it was briefly NOT a form control,
  // and while it wasn't, this function silently dropped it — so "use this model's settings" on a model
  // trained at 1.0 handed back a form that would train at 0.25. A materially different model, with no
  // warning. Caught by another session's audit before it shipped.
  for (const k of ['cropSize', 'cumulativeWindow', 'embeddingDim', 'epochs', 'foregroundWeight',
                   'intensityWeight', 'maxFrames', 'normalise', 'temporalWeight', 'trainRatio',
                   'zPlanes', 'zSpacing'] as const) {
    put(k, (m as unknown as Record<string, unknown>)[k])
  }

  // ── the four that need translating
  //
  // `chipSelect` holds STRINGS and validates each against its options; the manifest holds ints.
  if (Array.isArray(m.temporalScales)) put('temporalScales', m.temporalScales.map(String))

  // The scale MODE only. The spans are NOT a form field: they are `temporalScales x the reference
  // interval`, so restoring `temporalScales` above has already restored them — and restoring a
  // second, independently-stored copy is how the two would come to disagree. Absent on every model
  // trained before the mode existed, and `put` skips undefined, so those come back as they were.
  if (m.temporalScaleUnit === 's' || m.temporalScaleUnit === 'frames') {
    put('temporalScaleMode', m.temporalScaleUnit === 's' ? 'seconds' : 'frames')
  }

  // The metric set is stored as what was USED (`metricKeys`, which includes the derived `mag_<scale>`
  // planes that are not options) and what was EXCLUDED. Reconstructed from the exclusions against
  // what the form offers, so it stays correct when the option list grows.
  if (Array.isArray(m.droppedMetrics) && offeredMetrics.length) {
    const dropped = new Set(m.droppedMetrics)
    put('flowMetrics', offeredMetrics.filter(k => !dropped.has(k)))
  }

  // The picker holds channel NAMES; the manifest holds indices, whose meaning depends on the image.
  // `channelName` is the same choice as names joined with "+" (train.jl), which is what makes this
  // portable to an image whose channels sit in a different order.
  if (typeof m.channelName === 'string' && m.channelName) {
    put('trainChannels', m.channelName.split('+').map(s => s.trim()).filter(Boolean))
  }

  // Which version of the image it read — `smoothed`, usually.
  put('valueName', m.sourceValueName)

  // ── deliberately NOT copied
  // `modelName`: filling it in points the run at the model being admired, and `overwrite` would then
  // replace it. Applying a model's settings is the opposite of intending to destroy it.
  return out
}

/** Which manifest fields could not be mapped, for the message the UI shows. Empty when all were. */
export function unmappedFields(m: FlowManifest | null | undefined): string[] {
  if (!m) return []
  const out: string[] = []
  if (!m.channelName) out.push('channels')
  if (!m.sourceValueName) out.push('image version')
  if (!Array.isArray(m.droppedMetrics)) out.push('metrics')
  return out
}

/**
 * Manifest keys this mapper deliberately does not carry, checked by a test against the spec so a new
 * one cannot appear silently. `seed` is 42 in every model on disk and changing it changes nothing
 * anyone is asking about; `modelName`/`overwrite` would target the model being copied.
 */
export const NOT_CARRIED = ['seed'] as const
