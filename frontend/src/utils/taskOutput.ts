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
  'filepaths', 'labels', 'spatialGraphs', 'tracks', 'branches',
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
  return normaliseField(def.outputField)   // 'filepath' (singular) is the outputField spelling
}

/** The three `field` values a `valueNameSelection` can read — `VALUE_NAME_FIELDS` in paramValues.ts. */
export type ConsumerField = 'filepaths' | 'labels' | 'spatialGraphs'

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
    case 'filepaths':     return 'filepaths'
    case 'spatialGraphs': return 'spatialGraphs'
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
  return 'filepaths'
}
