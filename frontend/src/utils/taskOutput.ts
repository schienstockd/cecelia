// THE answer to "what does this task write, and into which namespace?"
//
// Value names are how most of this app's storage is addressed, and ELEVEN task params across SIX key
// spellings name something into it — `outputValueName` (segmentation), `valueNameSuffix` (clustering),
// `graphSuffix`, `statsSuffix`, `colName` (behaviour), `modelName` (optical flow). They are NOT
// synonyms: each names a different storage shape (a versioned-dict key, a column suffix, a filename
// stem, an obs column, a file in a global vault), which is why they keep their own key names and
// declare a `namespace` instead. See docs/todo/VALUE_NAME_INPUT_PLAN.md → D1.
//
// Before this module there were TWO partial answers that had already drifted apart, both correct only
// for the `outputValueName` spelling:
//   • `taskPreview.previewValueName` — the napari layer stem, params-only, always returns a string
//   • `ChainModule.nodeOutputValueName` — edge propagation, def-aware, returns null for non-producers
// Both now delegate here. A third variant is the bug (CLAUDE.md → divergent re-implementation), and
// it is an easy one to write, because the two callers genuinely want different SHAPES of the answer —
// that difference belongs in the callers, not in a second copy of the rule.
import type { TaskDef, ParamDef, ParamValues } from '../tasks/types'

/**
 * The storage namespaces a task can write a name into. `scope` is deliberately NOT uniform:
 * `models` is a GLOBAL vault (not per-image), and `clusters`/`regions`/`obsCols` hang off a
 * (image, value_name) pair rather than an image. See the plan → D6.
 */
export const VALUE_NAME_NAMESPACES = [
  // `labels` is segmentations with mask PIXELS; `labelProps` is anything with a measurement table.
  // Two independent ccid.json registries, and a task that writes a table but no mask — a direct track
  // import — belongs to the second. Naming it `labels` meant its own name-picker only ever offered
  // masks, so the name you wanted to overwrite was never suggested back to you.
  'filepaths', 'labels', 'labelProps', 'spatialGraphs', 'tracks', 'branches',
  'clusters', 'regions', 'stats', 'models', 'obsCols',
] as const

export type ValueNameNamespace = typeof VALUE_NAME_NAMESPACES[number]

export function isValueNameNamespace(v: unknown): v is ValueNameNamespace {
  return typeof v === 'string' && (VALUE_NAME_NAMESPACES as readonly string[]).includes(v)
}

export interface TaskOutput {
  name: string
  namespace: ValueNameNamespace
}

/** A non-empty string, or null — the only value shape worth propagating. */
function str(v: unknown): string | null {
  return typeof v === 'string' && v !== '' ? v : null
}

/** Walk `params` including `group`/`section` children, which nest one level deep in several specs. */
function* flatten(params: ParamDef[] | undefined): Generator<ParamDef> {
  for (const p of params ?? []) {
    yield p
    if (p.params) yield* flatten(p.params)
  }
}

/**
 * The name this task writes under, and the namespace it writes into — or `null` when the task
 * declares no output of its own (import, plots, measurement onto an existing set).
 *
 * Resolution order, most specific first:
 *   1. `def.outputValueName` — a FIXED output the task always writes (`smooth` → `smoothed`).
 *   2. a param declaring `namespace` — the user names it (the registry path).
 *   3. a param keyed `outputValueName` with no `namespace` — the pre-registry spelling, assumed
 *      `labels`. Kept so a spec that has not been migrated yet still propagates, and so a CUSTOM
 *      module written against the old convention keeps working (docs/CUSTOM_MODULES.md).
 */
export function taskOutput(
  def: TaskDef | undefined | null,
  params: ParamValues | undefined | null,
): TaskOutput | null {
  if (!def) return null

  const fixed = str(def.outputValueName)
  if (fixed) {
    return { name: fixed, namespace: namespaceOfDef(def) }
  }

  for (const p of flatten(def.params)) {
    if (!isValueNameNamespace(p.namespace)) continue
    const v = str(params?.[p.key]) ?? str(p.default)
    if (v) return { name: v, namespace: p.namespace }
  }

  // legacy spelling, no `namespace` declared
  for (const p of flatten(def.params)) {
    if (p.key !== 'outputValueName' || p.namespace) continue
    const v = str(params?.[p.key]) ?? str(p.default)
    if (v) return { name: v, namespace: 'labels' }
  }

  return null
}

/** The namespace a task's FIXED output lands in. `outputField` is the pre-registry spelling. */
function namespaceOfDef(def: TaskDef): ValueNameNamespace {
  if (isValueNameNamespace(def.outputNamespace)) return def.outputNamespace
  // Mapped explicitly, NOT through `normaliseField`. The two vocabularies overlap on `labels` and
  // `filepaths` and are otherwise different domains: a NAMESPACE is what a task writes into, a FIELD
  // is where the frontend reads names from, and the suffix namespaces read from fields named nothing
  // like them (`stats` → `statsSuffixes`). Reusing one for the other type-errors the moment a
  // namespace has no matching field, which is how this was caught.
  return def.outputField === 'labels' ? 'labels' : 'filepaths'
}

/**
 * The image-payload fields a picker can read names from.
 *
 * The first three are what a `valueNameSelection` may declare (`VALUE_NAME_FIELDS` in paramValues.ts);
 * the rest are suggestion-only sources for a `valueNameInput`, listed from disk rather than registered
 * in ccid.json. `models` is deliberately absent — that namespace is GLOBAL, so it has no image field
 * at all and its suggestions arrive as injected spec options (VALUE_NAME_INPUT_PLAN → D6).
 */
export type ConsumerField =
  'filepaths' | 'labels' | 'labelPropsNames' | 'spatialGraphs' | 'statsSuffixes'
  | 'clusterSuffixes' | 'regionSuffixes'

/**
 * The `valueNameSelection` `field` a CONSUMER would declare to read this namespace — the chain
 * whiteboard's question when it prefills a downstream node from an upstream output.
 *
 * Narrower than the namespace on purpose: only these three are readable from the image payload, so a
 * namespace with no consumer param to prefill (a cluster suffix, a model) reports `null` rather than
 * being coerced into a field it does not live in. The old `normField` collapsed every non-`labels`
 * value to `filepath`, which would offer a cluster suffix as an image version.
 */
export function consumerField(ns: ValueNameNamespace): ConsumerField | null {
  switch (ns) {
    case 'labels':        return 'labels'
    case 'labelProps':    return 'labelPropsNames'
    case 'filepaths':     return 'filepaths'
    case 'spatialGraphs': return 'spatialGraphs'
    case 'stats':         return 'statsSuffixes'
    case 'clusters':      return 'clusterSuffixes'
    case 'regions':       return 'regionSuffixes'
    // `models` is GLOBAL (the vault, not an image) and `tracks`/`branches`/`obsCols` name nothing a
    // task WRITES through a valueNameInput today — null means "no image field", which the caller
    // renders as a plain input rather than guessing.
    default:              return null
  }
}

/**
 * A consumer param's declared `field`, in ONE spelling.
 *
 * Two spellings are already on disk and they differ by an `s`: consumer params declare
 * `field: "filepaths"` (plural — `VALUE_NAME_FIELDS`), while a task's fixed output declares
 * `outputField: "filepath"` (singular). Comparing them raw silently never matches, so both sides go
 * through here. Absent means the image-version field, which is what `paramValues.ts` already assumes.
 */
export function normaliseField(field: string | undefined | null): ConsumerField {
  if (field === 'labels') return 'labels'
  if (field === 'spatialGraphs') return 'spatialGraphs'
  if (field === 'statsSuffixes' || field === 'clusterSuffixes' || field === 'regionSuffixes')
    return field
  return 'filepaths'
}
