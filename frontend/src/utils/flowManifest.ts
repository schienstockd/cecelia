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
  /** How many Z planes per movie. */
  zPlanes?: number
  /** Planes between them, centred on the stack; 0/absent = spread over the whole stack. */
  zSpacing?: number
  /** Square window each sequence was trained on, at a random position; 0/absent = whole frame. */
  cropSize?: number
  /** uID → `[y, x, h, w]` per plane, in `zPlanesUsed` order. The position is random per sequence. */
  cropWindows?: Record<string, number[][]>
  /** How the pooled flow metrics were held — `float16` since the memory work. */
  metricDtype?: string
  /** Cap on the contiguous frames each movie contributed; 0 = all. */
  maxFrames?: number
  /** Fraction of each sequence trained on; the rest was held out. 1 = no split. */
  trainRatio?: number
  /** uID → `[start, stop)` — only the movies that were actually cut. */
  frameWindows?: Record<string, number[]>
  /** uID → the plane indices that movie contributed; depth differs per image. */
  zPlanesUsed?: Record<string, number[]>
  /** Pre-`zPlanes` models: a single plane index, `-1` meaning the middle. */
  zSlice?: number
  trainedAt?: string
  lossWeights?: Record<string, number>
  /**
   * The per-pixel brightness/edge weight, against `foregroundWeight`'s cell-scale one — the
   * merge/coverage dial (docs/SEGMENTATION.md). Declared separately from `lossWeights` because the
   * manifest records it at top level and `paramsFromManifest` carries it back to the form; while it
   * was undeclared, the mapper dropped it and a model trained at 1.0 came back as the default.
   */
  intensityWeight?: number
  lossCurves?: Record<string, number[]>
  /** Per-term irreducible loss — `mean H(target)`. Keyed like `lossCurves`; BCE terms only. */
  lossFloors?: Record<string, number[]>
  /** Cell-scale blur on the foreground target, in px. Decides the target's SHAPE, not its weight. */
  foregroundBlurSigma?: number
  /**
   * uID → what one pixel and one frame of that movie physically are, as OME recorded them:
   * `{x, xUnit, y?, yUnit?, z?, zUnit?, t?, tUnit?}`. `z` is the gap between the planes TRAINED ON,
   * not the stack's own step. Values are unconverted — a movie in nm keeps its unit.
   *
   * The field that says whether a model applies to a different movie at all: every number coastal
   * is configured with is in pixels (`cropSize`) or frames (`temporalScales`), and neither means
   * anything without this. Absent on every model trained before 2026-08-21.
   */
  physicalScales?: Record<string, Record<string, number | string>>
  /** `ome` = every source movie carried a scale, `partial` = some, `none` = the scale is unknown. */
  physicalScaleSource?: 'ome' | 'partial' | 'none' | string
  /**
   * Which coastal produced this model — `{version, commit}`. The version alone does not move
   * (`0.1.0`), so the commit is the identifier that means anything, and coastal's inference is under
   * active change. Absent for models trained before 2026-08-21 and for a non-VCS install.
   */
  coastalBuild?: { version?: string; commit?: string } | null
}

export interface DetailField { label: string; value: string; mono?: boolean }
export interface DetailGroup { label: string; fields: DetailField[] }

/** Keys rendered by an explicit rule below — everything else falls through to "Other". */
const KNOWN = new Set([
  'temporalScales', 'cumulativeWindow', 'droppedMetrics', 'metricKeys', 'channelName',
  'trainChannels', 'epochs', 'embeddingDim', 'seed', 'normalise', 'sourceImages',
  'sourceValueName', 'nFrames', 'zPlanes', 'zPlanesUsed', 'zSlice', 'trainedAt', 'lossWeights',
  'intensityWeight',
  'maxFrames', 'frameWindows', 'trainRatio', 'zSpacing', 'cropSize', 'cropWindows', 'metricDtype',
  // Shown as a plot (Training convergence), not as hundreds of numbers in a dialog.
  'lossCurves', 'lossFloors',
  'foregroundBlurSigma',
  'physicalScales', 'physicalScaleSource', 'coastalBuild',
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
    // The interval only when one was asked for. Without it the planes are spread over the whole
    // stack, so the gap is a consequence of the depth rather than a setting, and a row saying
    // "spacing: 0" would read as a choice nobody made.
    field('Z spacing', m.zSpacing ? `every ${m.zSpacing}` : undefined),
    distinct.size === 1
      ? field('Planes', `[${[...distinct][0]}]`, true)
      : per.length
        ? { label: 'Planes', mono: true,
            value: per.map(([uid, v]) => `${uid}: [${v.join(', ')}]`).join('  ') }
        : null,
  ]
}

/**
 * What a pixel and a frame were, physically. One row when every movie agrees, one row per movie when
 * they do not — pooling two magnifications is legitimate and a single averaged number would hide it.
 *
 * Reads as "unknown" rather than being dropped when the images carried no scale. This is the one
 * absent field worth SAYING is absent: everywhere else an omission means "not used", here it means
 * the model cannot be matched to anyone's data, which is a property of the model.
 */
function scaleFields(m: FlowManifest): (DetailField | null)[] {
  if (m.physicalScaleSource === 'none' || (m.physicalScaleSource && !m.physicalScales)) {
    return [{ label: 'Scale', value: 'unknown — the source images carried no physical size' }]
  }
  const per = Object.entries(m.physicalScales ?? {})
  if (!per.length) return []
  const one = (v: Record<string, number | string>) => {
    const xy = v.x === undefined ? '' :
      `${v.x}${v.y !== undefined ? `×${v.y}` : ''} ${v.xUnit ?? 'um'}/px`
    const z = v.z === undefined ? '' : `${v.z} ${v.zUnit ?? 'um'} between planes`
    const t = v.t === undefined ? '' : `${v.t} ${v.tUnit ?? 's'}/frame`
    return [xy, z, t].filter(Boolean).join(', ')
  }
  const distinct = new Set(per.map(([, v]) => one(v)))
  const label = m.physicalScaleSource === 'partial' ? 'Scale (some movies)' : 'Scale'
  return distinct.size === 1
    ? [field(label, [...distinct][0])]
    : [{ label, mono: true, value: per.map(([uid, v]) => `${uid}: ${one(v)}`).join('  ') }]
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

  // Short commit, not the full 40: the point is to identify a build at a glance and to be able to
  // compare two models, and the full hash pushes the row past the column.
  const build = m.coastalBuild
  const training: (DetailField | null)[] = [
    field('Engine', build
      ? ['coastal', build.version, build.commit ? build.commit.slice(0, 8) : null]
        .filter(Boolean).join(' ')
      : undefined),
    field('Epochs', m.epochs),
    // Spelled out rather than shown as "1": a model with no held-out split has a loss curve that
    // cannot distinguish convergence from memorising, and that is worth reading off the dialog.
    field('Train fraction', m.trainRatio === undefined ? undefined
      : m.trainRatio >= 1 ? 'all (no validation)' : m.trainRatio),
    field('Embedding dim', m.embeddingDim),
    field('Seed', m.seed),
    ...Object.entries(m.lossWeights ?? {}).map(([term, w]) => field(`${term} weight`, w)),
    // Beside the weights, because it is the other half of what the foreground term IS. Two models
    // at foregroundWeight 1.0 and different blurs fitted differently shaped targets and their loss
    // curves are not comparable — a wider blur softens the target and raises its entropy floor.
    field('Foreground blur', m.foregroundBlurSigma === undefined ? undefined
      : `${m.foregroundBlurSigma} px`),
  ]

  // Which frames, not just how many. The window is seed-derived, so without it "frames 40–89 of
  // 200" is only recoverable by re-deriving it from the seed by hand — and the pooled total cannot
  // say whether a movie was cut or simply short.
  const cut = Object.entries(m.frameWindows ?? {})
  // Same argument as the frame windows, in the other two axes: the XY window is random per sequence,
  // so the size alone does not say what the model saw. Counted rather than listed — one line per
  // (movie × plane) is a wall, and the exact corners are a question for the manifest file itself.
  const crops = Object.values(m.cropWindows ?? {}).reduce((n, v) => n + v.length, 0)
  const source: (DetailField | null)[] = [
    field('Trained', m.trainedAt),
    ...scaleFields(m),
    field('Frames pooled', m.nFrames),
    field('Max frames/movie', m.maxFrames ? m.maxFrames : m.maxFrames === 0 ? 'all' : undefined),
    field('Crop', m.cropSize
      ? `${m.cropSize}×${m.cropSize}${crops ? ` at random (${crops} windows)` : ''}`
      : m.cropSize === 0 ? 'whole frame' : undefined),
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
