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
