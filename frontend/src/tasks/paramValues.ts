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

import type { WatchSource } from 'vue'
import type { TaskDef, ParamValues, ParamDef } from './types'
import type { CciaImage } from '../stores/project'
import { imageAxes, type Axis } from '../utils/taskGating'
import { asRoleMap } from '../utils/channelRoles'
import { isUmParam, badLength } from './umRange'

/** A param's saved value: its own key, else the first former key (a renamed param) holding one. */
export function savedValueFor(p: ParamDef, saved: ParamValues): unknown {
  if (saved[p.key] !== undefined) return saved[p.key]
  for (const k of p.formerKeys ?? []) if (saved[k] !== undefined) return saved[k]
  return undefined
}

/**
 * One entry of a repeatable `group`, reconciled against the group's declared sub-params.
 *
 * Section children are stored FLAT in the entry (that is what `ParamRenderer.addGroupEntry` writes), so
 * they are read flat here too.
 */
function buildGroupEntry(params: TaskDef['params'][number]['params'], saved: ParamValues): ParamValues {
  const out: ParamValues = {}
  for (const p of params ?? []) {
    if (p.type === 'section') {
      for (const sp of p.params ?? []) out[sp.key] = clampToSpec(sp, savedValueFor(sp, saved) ?? sp.default ?? null)
    } else {
      out[p.key] = clampToSpec(p, savedValueFor(p, saved) ?? p.default ?? null)
    }
  }
  return out
}

/**
 * A stored `int`/`float` pulled into the spec's CURRENT `[min, max]`; anything else passes through.
 *
 * A stored value outlives a range change. When a spec tightens a bound (`cellDiameter` went from
 * 1–500 to 2–60), a remembered value outside it is restored verbatim, shown with the slider pegged at
 * the end, and then rejected by `validate_params` ("exceeds maximum") on every run until the user
 * happens to move it. Clamping where stored values meet the current spec means the form never offers
 * a value the server will refuse — and the user sees the value it actually runs with.
 */
export function clampToSpec(p: ParamDef, v: unknown): unknown {
  if ((p.type !== 'int' && p.type !== 'float') || typeof v !== 'number' || !Number.isFinite(v)) return v
  // A length/area's spec range is a slider span scaled to the image, not a bound (`umRange.ts`); the
  // server checks only its sign — mirror `_clamp_to_range` (validate.jl).
  if (isUmParam(p)) return badLength(p, v) ? Math.max(p.min ?? 0, 0) : v
  if (p.min !== undefined && v < p.min) return p.min
  if (p.max !== undefined && v > p.max) return p.max
  return v
}

/**
 * `clampToSpec` over a raw stored param bag (a chain node's `params`), leaving its shape alone:
 * top-level keys, section children whether stored flat or nested, and repeatable-group entries.
 * Keys the spec does not declare pass through untouched.
 */
export function clampParamsToSpec(def: TaskDef, params: ParamValues): ParamValues {
  const out: ParamValues = { ...params }
  const clampKey = (bag: ParamValues, p: ParamDef) => {
    if (bag[p.key] !== undefined) bag[p.key] = clampToSpec(p, bag[p.key])
  }
  for (const p of def.params) {
    if (p.type === 'section') {
      const nested = out[p.key]
      if (nested && typeof nested === 'object') out[p.key] = { ...(nested as ParamValues) }
      for (const sp of p.params ?? []) {
        clampKey(out, sp)
        if (nested && typeof nested === 'object') clampKey(out[p.key] as ParamValues, sp)
      }
    } else if (p.type === 'group') {
      const g = out[p.key]
      if (g && typeof g === 'object') {
        out[p.key] = Object.fromEntries(Object.entries(g as Record<string, ParamValues>).map(
          ([k, entry]) => [k, clampParamsToSpec({ ...def, params: p.params ?? [] }, entry ?? {})]))
      }
    } else {
      clampKey(out, p)
    }
  }
  return out
}

/**
 * `<groupKey>Order` for every REPEATABLE group the spec declares — the sibling keys that are not spec
 * params and so are invisible to anything walking `def.params`.
 *
 * That invisibility was the bug: `flattenParams` builds the run payload by walking `def.params`, so it
 * dropped the order on every run. `_apply_group_order` then found no key, treated it as "every entry,
 * in entry order", and the chips did nothing at all — silently, because that is also the correct
 * behaviour for a group nobody has reordered. Verified on a real run: the banked params for a
 * two-entry `segment.coastalMeasure` contain no `modelsOrder`.
 *
 * `buildParamValues` dropped it for the same reason, so any re-init reset the chips as well.
 */
export function groupOrderKeysFor(def: TaskDef): string[] {
  return repeatableGroups(def).map(p => `${p.key}Order`)
}

/**
 * Form values for every param the spec declares, preferring `saved` and falling back to the param's
 * default. Sections are containers: their children are read from the FLAT key (that is how they are
 * stored) with a legacy nested record honoured first.
 *
 * Use this for a server-saved record AND for a restored draft — anything that may predate the current
 * spec. `null` (not `undefined`) is the "no value" marker, because null survives JSON.
 *
 * **A `group`'s ENTRIES are reconciled too, not passed through.** This used to take the saved group
 * verbatim, which meant the invariant above stopped at the top level: a group entry kept sub-keys the
 * spec no longer declares and never gained the ones it does. Real cost, measured on live projects —
 * `zolIMa` and `4kS67f` still stored AF combinations carrying `channelPercentile`, `correctionMax`,
 * `medianFilter`, `denoiseFun` and a dozen more, deleted from the spec in #437 and re-persisted on every
 * run since. And when `quotientChannel`/`divisionChannels` were renamed to
 * `targetChannel`/`competingChannels`, the entries survived while their contents did not: the form
 * showed the right NUMBER of combinations with every channel picker blank, which reads as "my params
 * weren't remembered". Same reconciliation as the top level: known keys survive, new sub-params get
 * their defaults, sub-params that no longer exist drop out.
 */
export function buildParamValues(def: TaskDef, saved: ParamValues): ParamValues {
  const vals: ParamValues = {}
  for (const p of def.params) {
    if (p.type === 'section') {
      const savedSection = ((saved[p.key] ?? {}) as ParamValues)
      const sectionVals: ParamValues = {}
      for (const sp of p.params ?? []) {
        sectionVals[sp.key] = clampToSpec(sp, savedValueFor(sp, savedSection) ?? savedValueFor(sp, saved)
                                              ?? sp.default ?? null)
      }
      vals[p.key] = sectionVals
    } else if (p.type === 'group') {
      const savedGroup = ((savedValueFor(p, saved) ?? p.default ?? {}) as Record<string, unknown>)
      const groupVals: Record<string, ParamValues> = {}
      for (const [entryKey, entry] of Object.entries(savedGroup)) {
        groupVals[entryKey] = buildGroupEntry(p.params, (entry ?? {}) as ParamValues)
      }
      vals[p.key] = groupVals
    } else {
      vals[p.key] = clampToSpec(p, savedValueFor(p, saved) ?? p.default ?? null)
    }
  }
  // The order chips' sibling keys, which no walk of `def.params` can see. Absent stays absent — an
  // unset order means "every entry, in entry order", and inventing one here would turn a group nobody
  // has reordered into one carrying a stored order.
  for (const k of groupOrderKeysFor(def)) {
    if (saved[k] !== undefined) vals[k] = saved[k]
  }
  return vals
}

/**
 * What the form should become on (re)init, or `null` for "leave it alone".
 *
 * The distinction this exists to make: **"you have no saved params" and "I could not load your saved
 * params" are not the same answer, and only one of them should overwrite the form.** They used to be
 * the same value — `fetchSavedParams` returned `{}` when the project uid wasn't known yet, when the
 * request failed, and when it threw — and the caller fed that straight to `buildParamValues`, which
 * answers every param with its spec default. So a load that never happened silently reset a form the
 * user had just filled in, on any task, with nothing logged.
 *
 * `saved === null` means the load did not happen. Everything else is a real answer, including `{}`
 * (genuinely nothing saved → defaults, which is right on a first run).
 *
 * A DRAFT wins over both: it is what the user typed and has not run yet. Reconciled through
 * `buildParamValues` rather than restored raw, because a draft outlives a spec change.
 */
export function resolveInitialParams(
  def: TaskDef, draft: ParamValues | undefined, saved: ParamValues | null,
): ParamValues | null {
  if (draft) return buildParamValues(def, draft)
  if (saved === null) return null
  return buildParamValues(def, saved)
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
  // The order chips. Forwarded EXACTLY as stored, including an empty list: dropping that would turn
  // "the user unticked every entry" into "run all of them", which is the silent divergence this whole
  // key exists to avoid. Absent stays absent, and the server reads that as every entry in order.
  for (const k of groupOrderKeysFor(def)) {
    if (vals[k] !== undefined) flat[k] = vals[k]
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

// ── showIf: a param that only applies under some other param's value ───────────────────────────────
//
// Declared in the SPEC, beside the param it is about:
//
//     { "key": "maxDistance", "showIf": { "mode": "attach" } }
//     { "key": "sigma",       "showIf": { "method": ["gaussian", "bilateral"] } }
//
// Keys are AND-ed; a list of values is OR-ed within one key. `{ "key": { "minItems": n } }` asks for a
// multi-select holding at least n entries; add `"equals": v` to count only entries equal to v (a list's
// items, or a map's values). Comparison is on the STRING form,
// because a spec is JSON and a form control's value is a string: `"1"` in a spec has to match the
// number 1 that a slider produced, or the same condition would work in one widget and not another.
//
// **Why this exists at all.** `ParamRenderer` already honoured a `hidden` flag, but nothing could set
// it from a spec — the policy was hand-written Julia in each task's `_inject_dynamic_options!`, with
// the param keys as literals. A plugin author ships JSON and a task `.jl`, so making a param
// disappear meant writing a Julia hook: the highest-friction way to express the thing most tightly
// bound to the param itself.
//
// **Where the line is, and why both sides are needed.** `showIf` decides from the FORM alone. A
// condition that needs to read a file, the filesystem or Python — the track importer's "this XML
// export has no columns to map" — cannot be a spec field at any price, and stays a server hook
// setting `hidden`. The question to ask of a new condition is exactly that: is the form enough?
//
// An ABSENT value satisfies nothing: a param gated on `mode` stays hidden until `mode` has a value.
// The alternative (absent matches everything) would flash every conditional param on first render,
// before defaults are applied.
export function showIfSatisfied(
  showIf: Record<string, unknown> | undefined,
  values: ParamValues | undefined,
): boolean {
  if (!showIf) return true            // no condition declared → always shown
  for (const [key, want] of Object.entries(showIf)) {
    const have = values?.[key]
    if (have === undefined || have === null) return false
    const got = String(have)
    // Operator form: `{ "csvPath": { "notEndsWith": ".xml" } }`. Suffix matching earns its place
    // because a FILE PATH's meaning often lives in its extension, and that is a property of the
    // string the form already holds — no server round-trip, so it stays correct when the form is
    // restored from a previous run rather than typed.
    if (want && typeof want === 'object' && !Array.isArray(want)) {
      const ops = want as Record<string, unknown>
      // `{ "correctChannels": { "minItems": 2 } }` — a setting that only means something once a
      // multi-select holds enough entries (AF correction's "different cell types" needs two markers)
      // `equals` counts only the entries holding that value — of a list, or of a map's values (a
      // `channelRoles` map: "two or more channels set to Correct")
      if (ops.minItems !== undefined) {
        const items = Array.isArray(have) ? have
          : typeof have === 'object' ? Object.values(have as Record<string, unknown>)
          : (got === '' ? [] : [have])
        const n = ops.equals === undefined ? items.length
          : items.filter(i => String(i) === String(ops.equals)).length
        if (n < Number(ops.minItems)) return false
        continue
      }
      const suffixes = (k: string) => {
        const v = ops[k]
        return v === undefined ? null : (Array.isArray(v) ? v : [v]).map(x => String(x).toLowerCase())
      }
      const ends = suffixes('endsWith')
      if (ends && !ends.some(sfx => got.toLowerCase().endsWith(sfx))) return false
      const notEnds = suffixes('notEndsWith')
      if (notEnds && notEnds.some(sfx => got.toLowerCase().endsWith(sfx))) return false
      if (ends === null && notEnds === null) return false     // an operator nobody implements
      continue
    }
    const accepted = (Array.isArray(want) ? want : [want]).map(String)
    if (!accepted.includes(got)) return false
  }
  return true
}

// ── Finding a sibling param BY TYPE, not by name ───────────────────────────────────────────────────
//
// Several widgets need another param's value: a measure picker has to know which segmentation to list
// columns for, a `popSelection` in single mode has to know which segmentation to scope to. That was
// resolved by hardcoded KEY — `values.pops`, then `values.valueName` — which is a naming convention
// the specs do not actually share.
//
// It was already wrong. Of the four specs using `labelPropsColsSelection`, `hmm_states` and
// `hmm_transitions` call their picker `pops`, but `clustPops.cluster` and `clustTracks.cluster` call
// theirs `popsToCluster` and declare no `valueName` — so both fell through to "the image's FIRST
// label set", and on any project with more than one segmentation the Cluster cells / Cluster tracks
// measure picker listed the wrong segmentation's columns. Silently: a populated dropdown of plausible
// column names is indistinguishable from the right one.
//
// By TYPE there is nothing to keep in step and nothing for a plugin author to know — a spec that
// declares a `popSelection` gets scoped by it whatever it is called.
export function siblingKeyOfType(params: ParamDef[] | undefined, type: string): string | undefined {
  for (const p of params ?? []) {
    if (p.type === type) return p.key
    const nested = siblingKeyOfType(p.params, type)   // sections/groups store sub-values FLAT
    if (nested) return nested
  }
  return undefined
}

/** The param with this key, recursing into sections/groups; undefined when nothing matches. */
export function findParamByKey(params: ParamDef[] | undefined, key: string): ParamDef | undefined {
  for (const p of params ?? []) {
    if (p.key === key) return p
    const hit = findParamByKey(p.params, key)
    if (hit) return hit
  }
  return undefined
}

/** The key of the form's SEGMENTATION picker — a `valueNameSelection` over label sets. Not just any
 *  `valueNameSelection`: spatial tasks' `graphSuffix` lists neighbour graphs (`spatialGraphs`), and an
 *  image-version picker (`filepaths`) names an image — neither is a segmentation. */
export function segmentationKey(params: ParamDef[] | undefined): string | undefined {
  for (const p of params ?? []) {
    if (p.type === 'valueNameSelection' && (p.field === 'labelPropsNames' || p.field === 'labels')) return p.key
    const nested = segmentationKey(p.params)
    if (nested) return nested
  }
  return undefined
}

/** The segmentation a measure/population picker is scoped to, given the whole form. */
export function scopeValueName(
  params: ParamDef[] | undefined,
  values: ParamValues | undefined,
  labelKeys: string[],
): string {
  // 1. the segmentation prefix carried by the first selected population ("A/_tracked" → "A")
  const popKey = siblingKeyOfType(params, 'popSelection')
  const pops = popKey ? values?.[popKey] : undefined
  const first = Array.isArray(pops) && pops.length ? String(pops[0]) : ''
  if (first && !first.startsWith('/')) {
    const idx = first.indexOf('/')
    if (idx > 0) return first.slice(0, idx)
  }
  // 2. an explicit sibling segmentation picker (not a graph or image-version picker)
  const vnKey = segmentationKey(params)
  const vn = vnKey ? values?.[vnKey] : undefined
  if (typeof vn === 'string' && vn) return vn
  // 3. the image's first label set — a guess, and the reason 1 and 2 are tried by type first
  return labelKeys[0] ?? 'default'
}

/** A multi-select population picker's value as its list of segmentation-prefixed refs. A run saved
 *  while the picker took ONE value holds a string — `"NONE"` (the whole segmentation) or a bare path
 *  (`"/qc/OTI"`, on the form's segmentation `vn`) — which reads as the chip it means, so the form shows
 *  what the run would do and the first click adds to it rather than dropping it. A bare path inside a
 *  list (a hand-edited or agent-authored chain) is on `vn` too, as the backend's `_split_pop_ref` reads it. */
export function popRefList(value: unknown, vn: string): string[] {
  const one = (v: string) => (v.startsWith('/') ? `${vn}${v}` : v)
  if (Array.isArray(value)) return value.map(v => one(String(v)))
  if (typeof value !== 'string' || value === '' || value === 'NONE') return []
  return [one(value)]
}

/** The chips a population picker shows as selected. With a Segmentation param beside it (tracking),
 *  a bare path or an old single-picker string is on THAT segmentation (`popRefList`), as the backend
 *  reads it against `valueName`. Without one (HMM, clustering), a bare path's segmentation is the
 *  image's active one, which the form doesn't know — so a list is shown as stored, unchanged. */
export function popChipValues(params: ParamDef[] | undefined, values: ParamValues | undefined,
                              value: unknown): string[] {
  const segKey = segmentationKey(params)
  const vn = segKey ? values?.[segKey] : undefined
  if (typeof vn === 'string' && vn) return popRefList(value, vn)
  if (Array.isArray(value)) return value.map(String)
  return typeof value === 'string' && value !== '' && value !== 'NONE' ? [value] : []
}

/** The SET of segmentations behind a picker's currently selected pops — deduped, first-seen order.
 *  Used by `labelPropsColsSelection` to intersect columns across VNs (contract B: a track cluster
 *  can only be built on features present on every selected pop's VN). Empty when nothing is
 *  selected or every entry is prefix-less — the caller then falls back to `scopeValueName`.  */
export function scopeValueNames(
  params: ParamDef[] | undefined,
  values: ParamValues | undefined,
): string[] {
  const popKey = siblingKeyOfType(params, 'popSelection')
  const pops = popKey ? values?.[popKey] : undefined
  if (!Array.isArray(pops) || !pops.length) return []
  const seen = new Set<string>()
  const out: string[] = []
  for (const p of pops) {
    const s = String(p)
    if (!s || s.startsWith('/')) continue
    const idx = s.indexOf('/')
    if (idx <= 0) continue
    const vn = s.slice(0, idx)
    if (seen.has(vn)) continue
    seen.add(vn); out.push(vn)
  }
  return out
}

// ── Per-param image-gating: `requires.axes` on a single param ──────────────────────────────────────
//
// The image-side twin of `showIf`. `showIf` gates a param on the FORM ("show me only when
// mode=attach"); `param.requires.axes` gates a param on the IMAGE ("show me only when this
// image carries T"). Both feed the same `notApplicable` in `ParamRenderer`, so a caller adding a
// new gate does not choose between two mechanisms — it picks the one whose data source matches.
//
// Same rule as showIf and `TaskDef.requires.axes`: the intersection of REQUIRED axes with axes the
// images CARRY. Empty selection → satisfied (nothing to intersect against). A mixed selection
// where one image is static and another is a timelapse is failed: the guarded control cannot
// meaningfully apply to the static image, and quietly running the timelapse config on it is exactly
// the smearing the guard is here to prevent.
export function paramRequiredAxes(p: ParamDef): Set<Axis> {
  const raw = p.requires?.axes
  if (!raw?.length) return new Set()
  return new Set(raw.map(a => String(a).toUpperCase() as Axis).filter(Boolean))
}

export function paramAppliesToImages(p: ParamDef, images: CciaImage[] | undefined): boolean {
  const need = paramRequiredAxes(p)
  if (need.size === 0) return true
  if (!images || images.length === 0) return true    // no image yet → don't pre-hide
  for (const img of images) {
    const have = imageAxes(img)
    for (const ax of need) if (!have.has(ax)) return false
  }
  return true
}

/**
 * Whether a param is on the form at all — THE visibility rule, shared by `ParamRenderer` (what it
 * draws) and `missingRequired` (what it demands). Three ways out: the server `hidden` it, its
 * `showIf` fails against `values`, or the images lack its `requires.axes`.
 *
 * A `section` additionally needs at least one child that applies, recursively: its children are
 * stored flat, so they read the same `values`. Without this a section whose every entry is ruled
 * out (the neighbour graph's "Advanced" under Fixed windows) draws as an empty collapsible.
 */
export function paramApplies(p: ParamDef, values: ParamValues | undefined,
                             images: CciaImage[] | undefined): boolean {
  if (p.hidden === true || !showIfSatisfied(p.showIf, values) || !paramAppliesToImages(p, images))
    return false
  if (p.type === 'section') return (p.params ?? []).some(c => paramApplies(c, values, images))
  return true
}

/**
 * The tip to show, given the selected images. Picks the first entry in `param.tips` whose
 * `requires.axes` is satisfied by every selected image, falling back to `param.tip`. Absent tip
 * (no match, or the matching entry omits text) means no info icon — that is the point of the
 * guarded form for controls whose only tip is temporal.
 */
export function resolveParamTip(p: ParamDef, images: CciaImage[] | undefined): string | undefined {
  if (p.tips?.length) {
    for (const entry of p.tips) {
      const need = new Set((entry.requires?.axes ?? []).map(a => String(a).toUpperCase() as Axis))
      if (need.size === 0) return entry.text || undefined
      if (!images || images.length === 0) continue
      const ok = images.every(img => {
        const have = imageAxes(img)
        for (const ax of need) if (!have.has(ax)) return false
        return true
      })
      if (ok) return entry.text || undefined
    }
    return undefined
  }
  return p.tip
}

// ── Required params, checked BEFORE the run ────────────────────────────────────────────────────────
//
// `required` was declared in specs, enforced only server-side, and read by the frontend NOWHERE — no
// marker, no Run gate. So nine tasks re-implemented it as a runtime log line and the user learned
// they had picked nothing AFTER pressing Run, from the log, having waited for a pool slot.
//
// A param `showIf` has ruled out is NOT required: the two would otherwise combine into a form that
// cannot be submitted and gives no way to see why. Julia's `validate_params` applies the same rule.
// Same reasoning applies to `param.requires.axes` — a control the image cannot carry (the temporal
// window on a static image) is nothing the user is expected to fill in.
export function missingRequired(
  def: TaskDef, values: ParamValues | undefined,
  images: CciaImage[] | undefined = undefined,
): string[] {
  const out: string[] = []
  // `vals` is the scope a param is read in: the form, or — inside a repeatable group — one entry's
  // values over the form's, as `ParamRenderer`'s `entryContext` and the server's per-entry
  // validation read it
  const walk = (ps: ParamDef[] | undefined, vals: ParamValues | undefined) => {
    for (const p of ps ?? []) {
      const applies = paramApplies(p, vals, images)
      if (applies && p.required) {
        const v = vals?.[p.key]
        const empty = v === undefined || v === null || v === '' ||
                      (Array.isArray(v) && v.length === 0)
        if (empty) out.push(p.requiredMessage || `${p.label || p.key} is required`)
      }
      if (!applies) continue
      if (p.type === 'group') {
        const entries = vals?.[p.key]
        if (entries && typeof entries === 'object')
          for (const e of Object.values(entries as Record<string, ParamValues>))
            walk(p.params, { ...(vals ?? {}), ...(e ?? {}) })
      } else {
        walk(p.params, vals)
      }
    }
  }
  walk(def.params, values)
  // A repeatable group whose order chips are ALL unticked. Reachable only now that the order actually
  // reaches the run: before, the payload dropped it and the server silently ran every entry, so the
  // state existed in the form and never in a run. Forwarded truthfully it means "run no entries",
  // which for a segmentation is a task with no model — so it is blocked at the button rather than
  // discovered as an empty result twenty minutes later.
  for (const p of repeatableGroups(def)) {
    const order = values?.[`${p.key}Order`]
    if (Array.isArray(order) && order.length === 0) {
      out.push(`${p.label || p.key}: select at least one entry to run`)
    }
  }
  return out
}

/** Every repeatable group the spec declares, at any depth. */
function repeatableGroups(def: TaskDef): ParamDef[] {
  const out: ParamDef[] = []
  const walk = (ps: ParamDef[] | undefined) => {
    for (const p of ps ?? []) {
      if (p.type === 'group' && p.repeatable) out.push(p)
      walk(p.params)
    }
  }
  walk(def.params)
  return out
}

/** Every param key a spec's `showIf` conditions refer to — for a ratchet that they exist. */
export function showIfKeys(def: TaskDef): string[] {
  const out: string[] = []
  const walk = (ps: ParamDef[] | undefined) => {
    for (const p of ps ?? []) {
      if (p.showIf) out.push(...Object.keys(p.showIf))
      walk(p.params)
    }
  }
  walk(def.params)
  return out
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
// `labels` vs `labelPropsNames` is the choice that keeps going wrong, so state it once: they are two
// INDEPENDENT ccid.json registries. `labels` = value names with mask PIXELS; `labelPropsNames` =
// value names with a measurement TABLE. A directly-imported track set registers only the second (there
// are no mask pixels to register), and a freshly segmented image only the first (until it is measured).
//
// A picker gates on `labels` only when the task genuinely needs the MASK. Every track-consuming task
// reads the h5ad and nothing else — so gating those on `labels` silently dropped exactly the sets the
// track importer creates: you could import tracks and then not measure them, in either the plugin or
// the built-in `tracking.track_measures`.
export const VALUE_NAME_FIELDS = ['filepaths', 'labels', 'labelPropsNames', 'spatialGraphs'] as const
export type ValueNameField = typeof VALUE_NAME_FIELDS[number]

/** Fields that hold IMAGE VERSIONS — the ones where the active version is the right default. */
const FIELD_IS_IMAGE_VERSION = new Set<string>(['filepaths'])

/** `field` omitted means image versions: the common case, and what most task JSON relies on. */
export const DEFAULT_VALUE_NAME_FIELD: ValueNameField = 'filepaths'

export function isKnownValueNameField(field: string | undefined): boolean {
  return field === undefined || (VALUE_NAME_FIELDS as readonly string[]).includes(field)
}

/** The names one image carries under `field`. */
export function imageNamesForField(img: CciaImage, field: string | undefined | null): string[] {
  if (field === 'labels') return Object.keys(img.labels ?? {})
  // Value names with a measurement table — a superset of `labels`, and the only list a direct track
  // import appears in (it registers a table and no mask).
  if (field === 'labelPropsNames') return img.labelPropsNames ?? []
  // spatial neighbour graphs (spatialAnalysis.cellNeighbours), keyed by run suffix — the intersection
  // across the selected images is exactly the set of graphs a pooled analysis can run over.
  if (field === 'spatialGraphs') return Object.keys(img.spatialGraphs ?? {})
  // Suggestion-only sources for a `valueNameInput` — plain arrays, listed from disk rather than
  // registered in ccid.json, so there are no keys to take.
  if (field === 'statsSuffixes') return img.statsSuffixes ?? []
  if (field === 'roiSuffixes') return img.roiSuffixes ?? []
  if (field === 'patternSuffixes') return img.patternSuffixes ?? []
  if (field === 'proximitySuffixes') return img.proximitySuffixes ?? []
  if (field === 'clusterSuffixes') return img.clusterSuffixes ?? []
  if (field === 'regionSuffixes') return img.regionSuffixes ?? []
  return Object.keys(img.filepaths ?? { default: '' })
}

/**
 * The option list for a value-name picker: the names present on EVERY selected image, plus `extra`.
 *
 * The intersection is the point — the form is one config applied to all of them, so a name only one
 * image has cannot be run.
 *
 * `field` is three-valued and the distinction is load-bearing:
 *   * a known field  → read that field;
 *   * `undefined`    → image VERSIONS (`DEFAULT_VALUE_NAME_FIELD`). Most task JSON omits it, so this
 *     is the common case, not an edge one;
 *   * `null`         → there is NO source (a `valueNameInput` whose namespace has no image field, e.g.
 *     the global model vault). Only then is an empty list correct.
 *
 * Collapsing `undefined` into `null` emptied the version picker on six task specs — cellpose, coastal,
 * smooth, opticalFlow.train, remove and measureLabels all omit `field`. Nothing errored; the
 * dropdown was simply blank. That is why this lives here with a test on the LIST, rather than inline in
 * the component where only `preferredValueName` was covered.
 */
export function valueNameOptions(
  images: CciaImage[], field: string | undefined | null, extra: readonly string[] = [],
): string[] {
  const base = field === null ? []
    : images.length === 0 ? ['default']          // nothing selected yet — offer the universal default
    : (() => {
        const f = field ?? DEFAULT_VALUE_NAME_FIELD
        const sets = images.map(img => new Set(imageNamesForField(img, f)))
        return [...sets[0]].filter(k => sets.every(s => s.has(k)))
      })()
  return [...new Set([...base, ...extra])]
}

/**
 * Does this `field` name IMAGE VERSIONS — the names in `filepaths`, where the image has an ACTIVE
 * one? Label sets and spatial graphs are also `valueNameSelection`, and neither has an active.
 *
 * The one answer to that question. It was a private set read only by `preferredValueName` until the
 * version advisory (`paramAdvisors.ts`) needed the same test; a second copy is how the same param
 * ends up preselecting the active version and then advising against a different reference.
 */
export function isImageVersionField(field: string | undefined): boolean {
  return field === undefined || FIELD_IS_IMAGE_VERSION.has(field)
}

/**
 * Which name to select when the image selection changes.
 *
 * For image versions, the ACTIVE version — it is what the viewer shows and what a run with no explicit
 * choice would read, so anything else silently previews/segments a version the user isn't looking at.
 * For other fields (label sets, spatial graphs) there is no "active", so the first option.
 *
 * **This is the R version's behaviour**: input fields there always selected the
 * active filepath. Worth recording, because the four tasks that had drifted off it (afCorrect,
 * driftCorrect, cropImage, copyImage — see above) looked like they might have been a deliberate choice
 * to start from the raw import. They were not; they were a dead string comparison.
 */
export function preferredValueName(
  available: string[], field: string | undefined, activeValueName?: string | null,
): string {
  const first = available[0] ?? 'default'
  const preferred = isImageVersionField(field) ? (activeValueName ?? first) : first
  return available.includes(preferred) ? preferred : first
}

/**
 * Has the user actually CHOSEN this value name, or is it just what the form started with?
 *
 * The distinction is the whole reason `preferredValueName` above reaches anything. Its caller keeps
 * an already-valid selection — right, for a chain-propagated name like `driftCorrected` — but every
 * task JSON declares `"default": "default"`, and `"default"` is a valid version on essentially every
 * image. So the guard fired on first render for all of them and prefer-the-active-version never ran:
 * the docstring above described behaviour the app did not have, on every page, and the four tasks
 * recorded there as fixed were still opening on the raw import.
 *
 * A value equal to the spec's own default therefore does not count as a choice. The cost is that
 * deliberately picking `default` while the active version is something else does not survive a
 * change of image selection — a re-pick, against a version silently reading the wrong pixels.
 */
export function isChosenValueName(value: unknown, specDefault: unknown): boolean {
  return typeof value === 'string' && value !== '' && value !== specDefault
}

/**
 * What the value-name picker should switch to when the image selection changes, or `null` to leave
 * it. A chosen name stays while the images offer it, and while they offer NOTHING (every image of a
 * set ticked, only the exemplar segmented: an empty intersection). `keepChosen` keeps it always — a
 * chain node's name is the plan for images that may not have it yet, so replacing its `Tcells` with
 * the `default` every image shares silently rewires the node; a missing input is the pre-flight's to
 * report. Returns `null` when the pick is what is already stored: the caller emits, the form
 * re-renders, and an emit of the same value on every render is an update loop.
 */
export function autoValueName(current: unknown, specDefault: unknown, available: string[],
                              field: string | undefined, activeValueName?: string | null,
                              keepChosen = false): string | null {
  if (isChosenValueName(current, specDefault)
      && (keepChosen || available.length === 0 || available.includes(current as string))) return null
  const pick = preferredValueName(available, field, activeValueName)
  return pick === current ? null : pick
}

/**
 * Starting values for a NEW entry of a repeatable group — the sub-params' own defaults, overlaid
 * with the spec's `entryDefaults` for that position.
 *
 * **Why a position matters at all.** Entries of a repeatable group are not interchangeable. Coastal's
 * `models` group is applied in run order and each entry fills only the pixels an earlier one left
 * (`fill_unlabelled`), so the second entry is a FRAGMENT pass over what the first did not claim.
 * Born as a copy of the first, it grows to almost the same regions and is then clipped along the
 * first pass's boundaries — slivers hugging every cell, at twice the compute, which is the one
 * configuration two passes must never have. The engine's own reference puts them at opposite ends
 * (`affinity_threshold` 0.2 then 0.8); the form used to start them identical.
 *
 * Applied ONLY when an entry is added, deliberately: filling a saved config's missing keys from a
 * position default would silently change what an existing run computes. A form that starts you
 * somewhere sensible is a different thing from a resolver that rewrites history.
 *
 * Positions past the end of `entryDefaults` reuse its last element — a third pass is much more like
 * the second than like the first.
 */
export function newEntryDefaults(param: ParamDef, position: number,
                                 base?: ParamValues): ParamValues {
  const declared: ParamValues = {}
  for (const p of param.params ?? []) {
    // Section sub-params are stored FLAT in the entry dict, so they are collected, not nested.
    const leaves = p.type === 'section' ? (p.params ?? []) : [p]
    for (const leaf of leaves) {
      if (leaf.default !== undefined) declared[leaf.key] = leaf.default as ParamValues[string]
    }
  }
  const out: ParamValues = { ...declared }

  // Then the FIRST entry's live values, for everything the two passes should share: which model,
  // which channels, the normalisation percentile, what it matches as. A user who has tuned the cell
  // pass to their data should not have the fragment pass start from bare spec defaults — and this is
  // also the only thing that stops a second pass inheriting a DIFFERENT channel set from the first,
  // which resolves to `[0]` downstream and silently segments a channel nobody chose.
  if (base && position > 0) {
    for (const [k, v] of Object.entries(base)) {
      if (k in declared) out[k] = v as ParamValues[string]
    }
  }

  // Last, the values that must DIFFER, which is the whole point.
  const table = param.entryDefaults
  if (table && table.length && position > 0) {
    const seed = table[Math.min(position, table.length - 1)]
    // Only keys the group declares — a stale spec entry must not inject a param that no longer
    // exists, which would then be forwarded to the runner as an unknown key.
    if (seed) {
      for (const [k, v] of Object.entries(seed)) {
        if (k in declared) out[k] = v as ParamValues[string]
      }
    }
  }
  return out
}

/**
 * Which entries of a repeatable group to run, in order — the resolved form of `<groupKey>Order`,
 * which `_apply_group_order` (task.jl) applies for real before any runner sees the group.
 *
 * `null`/absent means **all of them, in entry order**. That is not a default to be filled in
 * somewhere: a task saved before the control existed, a chain node, and a REPL call all carry no
 * value, and every one of them has to keep running every entry. Only an explicit list narrows or
 * reorders. Keys that no longer name an entry are dropped rather than trusted — a saved param set
 * outlives the group it was saved against.
 */
export function groupOrderKeys(
  group: Record<string, unknown> | null | undefined,
  order: unknown,
): string[] {
  const keys = Object.keys(group ?? {}).sort((a, b) => Number(a) - Number(b))
  if (!Array.isArray(order)) return keys
  return order.map(String).filter(k => keys.includes(k))
}

// ── channelSelection: only the channels the selected images share ─────────────────────────────────
//
// The chips list the channels EVERY selected image has. A stored selection can still hold names from
// other images: the form keeps one value per task, so channels picked on one panel of a mixed set
// (or restored from a draft or a chain) stay in the value after the selection moves to images that
// lack them. They were not drawn, but they were sent, and the run died on the first name the image
// did not have. `sharedChannels` is the chip list; `pruneChannelSelections` cuts the run's values to
// it, so what is sent is what is shown.
//
// A selection whose EVERY name is hidden is not cut to `[]`: several tasks read an empty channel list
// as "all channels" (smooth, OME-TIFF export), so emptying it would turn a refused run into one over
// every channel. It is left as is and reported in `emptied`, for the form to refuse the run instead.

/** Channel names every image carries, in the first image's order. `[]` when unknown. */
export function sharedChannels(images: readonly CciaImage[] | undefined): string[] {
  if (!images?.length) return []
  const lists = images.map(img => img.channelNames ?? [])
  return lists[0].filter(n => lists.every(ns => ns.includes(n)))
}

// `scope` is what a param's `showIf` reads — the same scope `ParamRenderer` gives it: the form's
// values for top-level and section params (a section's children see the form, not the section's own
// sub-object), one entry's values over the form's inside a group (`entryContext`).
function pruneEntry(params: readonly ParamDef[] | undefined, vals: ParamValues, scope: ParamValues,
                    keep: Set<string>, flatSections: boolean, emptied: string[],
                    images: CciaImage[] | undefined): ParamValues {
  let out = vals
  const set = (k: string, v: unknown) => { out = out === vals ? { ...vals } : out; out[k] = v }
  for (const p of params ?? []) {
    // not on the form (itself, or the section/group holding it): not the user's to fix here
    if (!paramApplies(p, scope, images)) continue
    const v = vals[p.key]
    if (p.type === 'channelSelection' && Array.isArray(v)) {
      const kept = (v as unknown[]).filter(n => keep.has(String(n)))
      if (kept.length === v.length) continue
      if (kept.length === 0) { if (!emptied.includes(p.label)) emptied.push(p.label) }
      else set(p.key, kept)
    } else if (p.type === 'channelRoles') {
      // `{ channel: role }` — the same rule on its keys (see utils/channelRoles.ts)
      const roles = Object.entries(asRoleMap(v))
      const kept = roles.filter(([c]) => keep.has(c))
      const stored = v && typeof v === 'object' ? Object.keys(v).length : 0
      if (roles.length && !kept.length) { if (!emptied.includes(p.label)) emptied.push(p.label) }
      else if (kept.length !== stored) set(p.key, Object.fromEntries(kept))
    } else if (p.type === 'section') {
      // the form nests a section under its key; a group entry stores its children flat
      if (flatSections) {
        out = pruneEntry(p.params, out, scope, keep, true, emptied, images)
      } else if (v && typeof v === 'object' && !Array.isArray(v)) {
        const next = pruneEntry(p.params, v as ParamValues, scope, keep, true, emptied, images)
        if (next !== v) set(p.key, next)
      }
    } else if (p.type === 'group' && v && typeof v === 'object' && !Array.isArray(v)) {
      let changed = false
      const entries: Record<string, unknown> = {}
      for (const [k, e] of Object.entries(v as Record<string, unknown>)) {
        const next = e && typeof e === 'object'
          ? pruneEntry(p.params, e as ParamValues, { ...scope, ...(e as ParamValues) }, keep, true, emptied, images)
          : e
        if (next !== e) changed = true
        entries[k] = next
      }
      if (changed) set(p.key, entries)
    }
  }
  return out
}

/**
 * `vals` with every channelSelection and channelRoles (top level, sections, group entries) cut to
 * `available`, plus
 * the labels of those whose every channel is missing (left uncut — see above). `values` is `vals`
 * itself when nothing changes; everything is left alone when `available` is empty (no images
 * selected, or channel names not loaded) — an unknown list is not "no channels".
 *
 * A picker off the form (`paramApplies` — hidden, showIf, `requires.axes` against `images`) is left
 * alone: not the user's to fix. Without `images`, axes are unknown and do not rule a picker out.
 */
export function pruneChannelSelections(params: readonly ParamDef[] | undefined, vals: ParamValues,
                                       available: readonly string[], images?: CciaImage[],
): { values: ParamValues; emptied: string[] } {
  const emptied: string[] = []
  if (!available.length) return { values: vals, emptied }
  return { values: pruneEntry(params, vals, vals, new Set(available), false, emptied, images), emptied }
}

/** What a population / measure picker reloads on, as SEPARATE watch sources. Vue compares a
 *  multi-source watch per source; one getter returning a fresh array is never === the last, so it
 *  re-fetched on every param write — including a pick in that same picker, which flashed it empty.
 *  The image's `labels` is watched BY IDENTITY: on task:done `refreshImageMeta` merges freshly parsed
 *  metadata into the store image in place, so `labels` becomes a new object and both pickers re-read
 *  what the task added — while a param write leaves it untouched. */
export interface ReloadContext {
  images?: { uid?: string; labels?: Record<string, unknown>; channelNames?: string[] }[]
  params?: ParamDef[]
  values?: ParamValues
}
export function popReloadSources(ctx: () => ReloadContext | undefined): WatchSource[] {
  return [() => ctx()?.images?.[0]?.uid ?? '',
          () => ctx()?.images?.[0]?.labels,
          () => scopeValueName(ctx()?.params, ctx()?.values, Object.keys(ctx()?.images?.[0]?.labels ?? {}))]
}
/** + the channel names (a rename relabels the chips), the segmentation value and the picked pops —
 *  found BY TYPE, since specs name them differently (`popsToCluster`, `pops`). */
export function colsReloadSources(ctx: () => ReloadContext | undefined): WatchSource[] {
  const valueOf = (type: 'seg' | 'pops') => {
    const c = ctx()
    const key = type === 'seg' ? segmentationKey(c?.params) : siblingKeyOfType(c?.params, 'popSelection')
    return key ? JSON.stringify(c?.values?.[key] ?? null) : ''
  }
  return [() => ctx()?.images?.[0]?.uid ?? '',
          () => ctx()?.images?.[0]?.labels,
          () => ctx()?.images?.[0]?.channelNames?.join('|') ?? '',
          () => valueOf('seg'),
          () => valueOf('pops')]
}
