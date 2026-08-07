/**
 * A trained flow model's manifest → labelled groups for the details modal.
 *
 * In a util, not the SFC, for the reason the rest of `utils/` exists: this is the part with a rule
 * worth pinning. A manifest is the contract inference configures itself from, so a key this file has
 * never heard of can still change what a model does — the UNKNOWN keys must therefore be shown, not
 * dropped. That is a behaviour, and behaviours get tests; a template can't express it and can't be
 * tested if it did.
 */

export interface FlowManifest {
  [key: string]: unknown
  temporalScales?: number[]
  cumulativeWindow?: number
  droppedMetrics?: string[]
  metricKeys?: string[]
  channelName?: string
  trainChannels?: number[]
  epochs?: number
  embeddingDim?: number
  seed?: number
  normalise?: number
  sourceImages?: string[]
  sourceValueName?: string
  nFrames?: number
  /** How many evenly-spaced Z planes per movie. */
  zPlanes?: number
  /** Cap on the contiguous frames each movie contributed; 0 = all. */
  maxFrames?: number
  /** uID → `[start, stop)` — only the movies that were actually cut. */
  frameWindows?: Record<string, number[]>
  /** uID → the plane indices that movie contributed; depth differs per image. */
  zPlanesUsed?: Record<string, number[]>
  /** Pre-`zPlanes` models: a single plane index, `-1` meaning the middle. */
  zSlice?: number
  trainedAt?: string
  lossWeights?: Record<string, number>
  lossCurves?: Record<string, number[]>
}

export interface DetailField { label: string; value: string; mono?: boolean }
export interface DetailGroup { label: string; fields: DetailField[] }

/** Keys rendered by an explicit rule below — everything else falls through to "Other". */
const KNOWN = new Set([
  'temporalScales', 'cumulativeWindow', 'droppedMetrics', 'metricKeys', 'channelName',
  'trainChannels', 'epochs', 'embeddingDim', 'seed', 'normalise', 'sourceImages',
  'sourceValueName', 'nFrames', 'zPlanes', 'zPlanesUsed', 'zSlice', 'trainedAt', 'lossWeights',
  'maxFrames', 'frameWindows',
  // Shown as a plot (Training convergence), not as hundreds of numbers in a dialog.
  'lossCurves',
])

const list = (v: unknown): string =>
  Array.isArray(v) ? v.join(', ') : v === undefined || v === null ? '' : String(v)

/**
 * A row, or `null` when the manifest has nothing to say. An absent field is DROPPED rather than
 * rendered as a dash: a dialog of dashes reads as "we lost this", and a manifest legitimately omits
 * what its training run did not use.
 */
function field(label: string, value: unknown, mono = false): DetailField | null {
  if (value === undefined || value === null) return null
  const text = Array.isArray(value) ? list(value) : String(value)
  return text === '' ? null : { label, value: text, mono }
}

/**
 * How much of the stack the model saw. One row for the count, one for the actual indices.
 *
 * The indices are worth their own row because "3 planes" is not a depth: 3 of a 31-plane stack and
 * 3 of a 9-plane one are different tissue. Only shown when the movies disagree about *which* — with
 * one set of indices across every image, repeating it per uID is noise.
 */
function zPlaneFields(m: FlowManifest): (DetailField | null)[] {
  if (m.zPlanes === undefined) return [field('Z plane', m.zSlice === -1 ? 'middle' : m.zSlice)]

  const per = Object.entries(m.zPlanesUsed ?? {})
  const distinct = new Set(per.map(([, v]) => v.join(',')))
  return [
    field('Z planes', m.zPlanes === 1 ? '1 (middle)' : m.zPlanes),
    distinct.size === 1
      ? field('Planes', `[${[...distinct][0]}]`, true)
      : per.length
        ? { label: 'Planes', mono: true,
            value: per.map(([uid, v]) => `${uid}: [${v.join(', ')}]`).join('  ') }
        : null,
  ]
}

/**
 * The manifest grouped for display. Empty when there is no manifest at all — the caller says so in
 * its own words rather than rendering a set of dashes.
 *
 * Grouping follows what you would change if you re-trained: what it read, how it was trained, what
 * it was trained on. `droppedMetrics` sits with the metric set because it is only meaningful against
 * it, and it is the field that silently breaks inference when it disagrees.
 */
export function modelDetailGroups(manifest: FlowManifest | null | undefined): DetailGroup[] {
  if (!manifest || !Object.keys(manifest).length) return []
  const m = manifest

  const input: (DetailField | null)[] = [
    field('Channels', m.channelName || (m.trainChannels ?? []).join(', ')),
    field('Image version', m.sourceValueName),
    // Two spellings on purpose. `zPlanes` is the count a current run was given; `zSlice` is what
    // models trained before it recorded, and those models are still in people's vaults. Reading the
    // old key is how the modal keeps describing them instead of quietly dropping the row.
    ...zPlaneFields(m),
    field('Temporal scales', m.temporalScales),
    field('Cumulative window', m.cumulativeWindow),
    field('Normalise', m.normalise === undefined ? undefined : `${m.normalise}th percentile`),
  ]

  const metrics: (DetailField | null)[] = [
    field('Planes read', m.metricKeys?.length ? `${m.metricKeys.length}` : undefined),
    m.metricKeys?.length ? { label: 'Set', value: m.metricKeys.join(', '), mono: true } : null,
    m.droppedMetrics?.length
      ? { label: 'Excluded', value: m.droppedMetrics.join(', '), mono: true }
      : m.metricKeys?.length ? { label: 'Excluded', value: 'none' } : null,
  ]

  const training: (DetailField | null)[] = [
    field('Epochs', m.epochs),
    field('Embedding dim', m.embeddingDim),
    field('Seed', m.seed),
    ...Object.entries(m.lossWeights ?? {}).map(([term, w]) => field(`${term} weight`, w)),
  ]

  // Which frames, not just how many. The window is seed-derived, so without it "frames 40–89 of
  // 200" is only recoverable by re-deriving it from the seed by hand — and the pooled total cannot
  // say whether a movie was cut or simply short.
  const cut = Object.entries(m.frameWindows ?? {})
  const source: (DetailField | null)[] = [
    field('Trained', m.trainedAt),
    field('Frames pooled', m.nFrames),
    field('Max frames/movie', m.maxFrames ? m.maxFrames : m.maxFrames === 0 ? 'all' : undefined),
    cut.length
      ? { label: `Windows (${cut.length})`, mono: true,
          value: cut.map(([uid, [a, b]]) => `${uid}: ${a}–${(b ?? 0) - 1}`).join('  ') }
      : null,
    m.sourceImages?.length
      ? { label: `Images (${m.sourceImages.length})`, value: m.sourceImages.join(', '), mono: true }
      : null,
  ]

  // Anything a later training run started writing. Shown rather than dropped: an unknown key can
  // still be a field inference reads.
  const other: (DetailField | null)[] = Object.entries(m)
    .filter(([k]) => !KNOWN.has(k))
    .map(([k, v]) => field(k, Array.isArray(v) ? list(v) : typeof v === 'object'
      ? JSON.stringify(v) : v, true))

  return ([
    { label: 'Input', fields: input },
    { label: 'Flow metrics', fields: metrics },
    { label: 'Training', fields: training },
    { label: 'Source', fields: source },
    { label: 'Other', fields: other },
  ] as { label: string; fields: (DetailField | null)[] }[])
    .map(g => ({ label: g.label, fields: g.fields.filter((f): f is DetailField => f !== null) }))
    .filter(g => g.fields.length > 0)
}
