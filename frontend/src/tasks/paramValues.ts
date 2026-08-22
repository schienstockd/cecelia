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

import type { TaskDef, ParamValues, ParamDef } from './types'
import type { CciaImage } from '../stores/project'

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
      for (const sp of p.params ?? []) out[sp.key] = saved[sp.key] ?? sp.default ?? null
    } else {
      out[p.key] = saved[p.key] ?? p.default ?? null
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
        sectionVals[sp.key] = savedSection[sp.key] ?? saved[sp.key] ?? sp.default ?? null
      }
      vals[p.key] = sectionVals
    } else if (p.type === 'group') {
      const savedGroup = ((saved[p.key] ?? p.default ?? {}) as Record<string, unknown>)
      const groupVals: Record<string, ParamValues> = {}
      for (const [entryKey, entry] of Object.entries(savedGroup)) {
        groupVals[entryKey] = buildGroupEntry(p.params, (entry ?? {}) as ParamValues)
      }
      vals[p.key] = groupVals
    } else {
      vals[p.key] = saved[p.key] ?? p.default ?? null
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
// Keys are AND-ed; a list of values is OR-ed within one key. Comparison is on the STRING form,
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
  // 2. an explicit sibling segmentation picker
  const vnKey = siblingKeyOfType(params, 'valueNameSelection')
  const vn = vnKey ? values?.[vnKey] : undefined
  if (typeof vn === 'string' && vn) return vn
  // 3. the image's first label set — a guess, and the reason 1 and 2 are tried by type first
  return labelKeys[0] ?? 'default'
}

// ── Required params, checked BEFORE the run ────────────────────────────────────────────────────────
//
// `required` was declared in specs, enforced only server-side, and read by the frontend NOWHERE — no
// marker, no Run gate. So nine tasks re-implemented it as a runtime log line and the user learned
// they had picked nothing AFTER pressing Run, from the log, having waited for a pool slot.
//
// A param `showIf` has ruled out is NOT required: the two would otherwise combine into a form that
// cannot be submitted and gives no way to see why. Julia's `validate_params` applies the same rule.
export function missingRequired(def: TaskDef, values: ParamValues | undefined): string[] {
  const out: string[] = []
  const walk = (ps: ParamDef[] | undefined) => {
    for (const p of ps ?? []) {
      const applies = p.hidden !== true && showIfSatisfied(p.showIf, values)
      if (applies && p.required) {
        const v = values?.[p.key]
        const empty = v === undefined || v === null || v === '' ||
                      (Array.isArray(v) && v.length === 0)
        if (empty) out.push(p.requiredMessage || `${p.label || p.key} is required`)
      }
      applies && walk(p.params)
    }
  }
  walk(def.params)
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
 * **This is the R version's behaviour** (Dominik, 2026-08-01): input fields there always selected the
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
 * an already-valid selection — right, for a chain-propagated name like `cpCorrected` — but every
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
