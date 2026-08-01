// Building and flattening a task form's param values — extracted from TaskRunner.vue so the round-trip
// (saved params → form → submitted params → saved params) is unit-testable (docs/DEV.md: logic lives in
// plain .ts, not the SFC).
//
// THE INVARIANT: every param in the spec must appear in the submitted payload. It is easy to break
// silently, because `JSON.stringify` DROPS keys whose value is `undefined` — so a param missing from the
// form object vanishes from the run payload AND from the funParams record the server writes, and the
// task quietly falls back to its default. Nothing errors; the setting just stops being remembered.
//
// That is exactly what a param-set change does to a persisted DRAFT. Drafts are keyed by
// (project, fun, scope) and outlive a spec edit, so a draft written when `clustRegions.cluster` had
// `neighbourRadius`/`perTimepoint` has neither `graphSuffix` nor `includeOther` — and restoring it raw
// left those keys absent. Reconcile a draft through `buildParamValues` (same as a server record) and the
// gap closes: known keys survive, new params get their defaults, params that no longer exist drop out.

import type { TaskDef, ParamValues } from './types'

/**
 * Form values for every param the spec declares, preferring `saved` and falling back to the param's
 * default. Sections are containers: their children are read from the FLAT key (that is how they are
 * stored) with a legacy nested record honoured first.
 *
 * Use this for a server-saved record AND for a restored draft — anything that may predate the current
 * spec. `null` (not `undefined`) is the "no value" marker, because null survives JSON.
 */
export function buildParamValues(def: TaskDef, saved: ParamValues): ParamValues {
  const vals: ParamValues = {}
  for (const p of def.params) {
    if (p.type === 'section') {
      const savedSection = ((saved[p.key] ?? {}) as ParamValues)
      const sectionVals: ParamValues = {}
      for (const sp of p.params ?? []) {
        sectionVals[sp.key] = savedSection[sp.key] ?? saved[sp.key] ?? sp.default ?? null
      }
      vals[p.key] = sectionVals
    } else {
      vals[p.key] = saved[p.key] ?? p.default ?? null
    }
  }
  return vals
}

/**
 * The payload to send on run: top-level section containers hoisted away, one entry per real param.
 *
 * Falls back to the param's default rather than emitting `undefined` — an undefined value is dropped by
 * JSON.stringify, which is how a param silently stops being submitted and stops being remembered.
 */
export function flattenParams(def: TaskDef, vals: ParamValues): ParamValues {
  const flat: ParamValues = {}
  for (const p of def.params) {
    if (p.type === 'section') {
      const nested = ((vals[p.key] ?? {}) as ParamValues)
      for (const sp of p.params ?? []) {
        flat[sp.key] = nested[sp.key] ?? sp.default ?? null
      }
    } else {
      flat[p.key] = vals[p.key] ?? p.default ?? null
    }
  }
  return flat
}

/** Params the spec declares but the payload omits — the silent-loss check the tests assert on. */
export function missingParamKeys(def: TaskDef, payload: ParamValues): string[] {
  const keys = new Set(Object.keys(payload))
  const want: string[] = []
  for (const p of def.params) {
    if (p.type === 'section') { for (const sp of p.params ?? []) want.push(sp.key) }
    else want.push(p.key)
  }
  return want.filter(k => !keys.has(k) || payload[k] === undefined)
}

// ── valueNameSelection: which image field, and which name to preselect ─────────────────────────────
//
// A `valueNameSelection` param reads its options from ONE field of the image, named by `param.field`.
// The names are the frontend's own `CciaImage` field names (stores/project.ts) — `filepaths`, `labels`,
// `spatialGraphs` — NOT the ccid.json spelling (`filepath`, singular) and not the R version's
// (`imFilepath`), both of which have been used in task JSON by mistake.
//
// THE BUG THIS REPLACES: the auto-select preferred the image's ACTIVE version only when field was
// absent or the string `'filepath'`. Nothing declared `'filepath'` — four image-version tasks
// (afCorrect, driftCorrect, cropImage, copyImage) declared `imFilepath` — so all four silently took the
// "just pick the first option" branch and did NOT preselect the version the viewer has open, while
// cellpose (field absent) did. Same widget, two behaviours, no error. `FIELD_IS_IMAGE_VERSION` is now
// an explicit set, and `isKnownValueNameField` lets the suite reject an unrecognised name at source
// rather than having it degrade quietly.

/** Every `field` a `valueNameSelection` param may name. */
export const VALUE_NAME_FIELDS = ['filepaths', 'labels', 'spatialGraphs'] as const
export type ValueNameField = typeof VALUE_NAME_FIELDS[number]

/** Fields that hold IMAGE VERSIONS — the ones where the active version is the right default. */
const FIELD_IS_IMAGE_VERSION = new Set<string>(['filepaths'])

/** `field` omitted means image versions: the common case, and what most task JSON relies on. */
export const DEFAULT_VALUE_NAME_FIELD: ValueNameField = 'filepaths'

export function isKnownValueNameField(field: string | undefined): boolean {
  return field === undefined || (VALUE_NAME_FIELDS as readonly string[]).includes(field)
}

/**
 * Which name to select when the image selection changes.
 *
 * For image versions, the ACTIVE version — it is what the viewer shows and what a run with no explicit
 * choice would read, so anything else silently previews/segments a version the user isn't looking at.
 * For other fields (label sets, spatial graphs) there is no "active", so the first option.
 *
 * **This is the R version's behaviour** (Dominik, 2026-08-01): input fields there always selected the
 * active filepath. Worth recording, because the four tasks that had drifted off it (afCorrect,
 * driftCorrect, cropImage, copyImage — see above) looked like they might have been a deliberate choice
 * to start from the raw import. They were not; they were a dead string comparison.
 */
export function preferredValueName(
  available: string[], field: string | undefined, activeValueName?: string | null,
): string {
  const first = available[0] ?? 'default'
  const wantsActive = field === undefined || FIELD_IS_IMAGE_VERSION.has(field)
  const preferred = wantsActive ? (activeValueName ?? first) : first
  return available.includes(preferred) ? preferred : first
}
