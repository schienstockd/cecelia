// A parameter sweep: one task run N times, one param varied, each run writing its own named output
// (docs/todo/PARAM_SWEEP_PLAN.md). The task form expands it here into N ordinary param sets, and
// `TaskRunner.run()` sends each exactly as a single Run would — no chain, no new route.
//
// A task opts in with a `sweep` block in its spec (`TaskDef.sweep`): `output` names the param that
// keeps the runs apart, `params` the knobs offered. Whether two runs really coexist is a fact about the
// task's code, so it is declared, never inferred from the param types.

import type { ParamDef, ParamValues, TaskDef } from '../tasks/types'
import { imageNamesForField } from '../tasks/paramValues'
import { consumerField, isValueNameNamespace } from './taskOutput'
import { registryNames } from '../tasks/paramAdvisors'

/** The persisted sweep settings for one task (per fun_name, in profileStorage). */
export interface SweepState {
  on: boolean
  param: string      // the swept param's key
  values: string     // what the user typed: `2, 3, 4` or `2:5` or `0.5:2:0.5`
  names: string      // output-name template; `{v}` is the value, `{name}` the form's output name
}

/** More runs than this is a typo, not a sweep. */
export const SWEEP_MAX_RUNS = 20

export const SWEEP_DEFAULT_NAMES = '{name}_{v}'

export type SweepValue = number | string

/** The params a spec offers for sweeping, resolved against its (possibly composite-merged) params. */
export function sweepableParams(def: TaskDef | null | undefined): ParamDef[] {
  const keys = def?.sweep?.params ?? []
  return keys.map(k => findTopLevel(def!.params, k)).filter((p): p is ParamDef => !!p)
}

/** The param that names the output, or null when the spec declares none. */
export function sweepOutputParam(def: TaskDef | null | undefined): ParamDef | null {
  const k = def?.sweep?.output
  return k ? findTopLevel(def!.params, k) : null
}

// Params can sit inside a `section` (flattened on submit); a sweep never targets a repeatable group.
function findTopLevel(params: ParamDef[] | undefined, key: string): ParamDef | null {
  for (const p of params ?? []) {
    if (p.key === key && p.type !== 'group' && p.type !== 'section') return p
    if (p.type === 'section') {
      const hit = findTopLevel(p.params, key)
      if (hit) return hit
    }
  }
  return null
}

// Floats from a range accumulate error (0.1 + 0.2); round to the param's step precision.
function roundTo(v: number, step: number): number {
  const decimals = Math.min(10, Math.max(0, -Math.floor(Math.log10(Math.abs(step) || 1)) + 1))
  return Number(v.toFixed(decimals))
}

/**
 * The values the user typed for `p`: a comma/space separated list, where any item may be a range
 * `a:b` (step 1) or `a:b:step`. Every value must be valid for the param AS IT IS — inside its
 * `[min, max]`, an integer for `int`, an option for `select` — since a silently clamped value would
 * run under a name that says something else.
 */
export function parseSweepValues(text: string, p: ParamDef): { values: SweepValue[]; error?: string } {
  const items = text.split(/[,\s]+/).map(s => s.trim()).filter(Boolean)
  if (!items.length) return { values: [], error: 'Enter values' }
  const out: SweepValue[] = []
  const push = (v: SweepValue) => { if (!out.includes(v)) out.push(v) }

  if (p.type === 'select') {
    const allowed = (p.options ?? []).map(o => String(o.value))
    for (const it of items) {
      if (!allowed.includes(it)) return { values: [], error: `'${it}' is not an option` }
      push(it)
    }
    return { values: out }
  }

  const isInt = p.type === 'int'
  for (const it of items) {
    const parts = it.split(':')
    if (parts.length > 3) return { values: [], error: `Can't read '${it}'` }
    const nums = parts.map(Number)
    if (nums.some(n => !Number.isFinite(n)) || parts.some(s => s === '')) {
      return { values: [], error: `Can't read '${it}'` }
    }
    if (nums.length === 1) {
      push(nums[0])
    } else {
      const [a, b] = nums
      const step = nums[2] ?? 1
      if (step <= 0) return { values: [], error: 'Step must be above 0' }
      if (b < a) return { values: [], error: `Range '${it}' runs backwards` }
      const n = Math.floor((b - a) / step + 1e-9) + 1
      if (n > SWEEP_MAX_RUNS) return { values: [], error: `At most ${SWEEP_MAX_RUNS} runs` }
      for (let i = 0; i < n; i++) push(roundTo(a + i * step, step))
    }
    if (out.length > SWEEP_MAX_RUNS) return { values: [], error: `At most ${SWEEP_MAX_RUNS} runs` }
  }
  for (const v of out as number[]) {
    if (isInt && !Number.isInteger(v)) return { values: [], error: `${v} is not a whole number` }
    if (p.min !== undefined && v < p.min) return { values: [], error: `${v} is below ${p.min}` }
    if (p.max !== undefined && v > p.max) return { values: [], error: `${v} is above ${p.max}` }
  }
  return { values: out }
}

/** One run's output name from the template. `{v}` is required — without it every run shares a name. */
export function sweepRunName(template: string, base: string, v: SweepValue): string {
  return template.replaceAll('{name}', base).replaceAll('{v}', String(v)).trim()
}

/**
 * Output names already TAKEN on the selected images, read from what the image payload holds for the
 * output's namespace — the same lists the name field suggests from. An output with no such list
 * (`behaviour.hmm_states`' plain `colName`) answers `null`: the payload cannot say, so the caller
 * cannot refuse; the task replaces an existing run of that name, as a single Run does.
 */
export function sweepTakenNames(
  out: ParamDef, images: Parameters<typeof imageNamesForField>[0][],
): Set<string> | null {
  if (out.outputRegistry && out.outputRegistry !== 'labels') {
    const names = registryNames(out.outputRegistry)
    return names ? new Set(images.flatMap(i => Object.keys(names(i)))) : null
  }
  if (out.type !== 'valueNameInput' || !isValueNameNamespace(out.namespace)) return null
  const field = consumerField(out.namespace)
  // a GLOBAL namespace (the model vault) carries its names as the spec's injected options
  if (field === null) return out.options?.length ? new Set(out.options.map(o => String(o.value))) : null
  return new Set(images.flatMap(i => imageNamesForField(i, field)))
}

export interface SweepRun { value: SweepValue; name: string; params: ParamValues }

export interface SweepPlan {
  runs: SweepRun[]
  error?: string       // nothing can run
  replaces?: boolean   // no taken-name list for this output: an existing run of the same name is replaced
}

/**
 * Expand the form's (flattened) params into one param set per value. Refused — `error`, no runs —
 * when the values don't parse, the template lacks `{v}`, two runs would share a name, or a name is
 * already taken on a selected image.
 */
export function expandSweep(
  def: TaskDef, params: ParamValues, state: SweepState,
  images: Parameters<typeof imageNamesForField>[0][],
): SweepPlan {
  const p = sweepableParams(def).find(x => x.key === state.param)
  const out = sweepOutputParam(def)
  if (!p || !out) return { runs: [], error: 'Select a parameter' }
  const parsed = parseSweepValues(state.values, p)
  if (parsed.error) return { runs: [], error: parsed.error }
  if (!state.names.includes('{v}')) return { runs: [], error: 'Names need {v}' }

  const base = String(params[out.key] ?? '').trim()
  const runs = parsed.values.map(v => ({
    value: v,
    name: sweepRunName(state.names, base, v),
    params: { ...params, [p.key]: v, [out.key]: sweepRunName(state.names, base, v) },
  }))
  const names = runs.map(r => r.name)
  if (names.some(n => !n || n.includes('/') || n.includes('\\'))) return { runs: [], error: 'Names must not contain / or \\' }
  if (new Set(names).size !== names.length) return { runs: [], error: 'Two runs get the same name' }

  const taken = sweepTakenNames(out, images)
  if (taken) {
    const hit = names.filter(n => taken.has(n))
    if (hit.length) return { runs: [], error: `Already exists: ${hit.join(', ')}` }
  }
  return { runs, replaces: taken === null }
}

/** profileStorage key for one task's sweep settings (same per-fun shape as `runTargetKey`). */
export const sweepStateKey = (funName: string) => `cc-sweep:${funName}`

/** Stored settings, reconciled against the spec: an unknown or no-longer-offered param falls back to
 *  the first one offered, and anything unreadable to the defaults. */
export function parseSweepState(raw: string | null, def: TaskDef | null | undefined): SweepState {
  const offered = sweepableParams(def).map(p => p.key)
  let s: Partial<SweepState> = {}
  try { s = raw ? JSON.parse(raw) : {} } catch { s = {} }
  return {
    on: s.on === true && offered.length > 0,
    param: typeof s.param === 'string' && offered.includes(s.param) ? s.param : (offered[0] ?? ''),
    values: typeof s.values === 'string' ? s.values : '',
    names: typeof s.names === 'string' && s.names ? s.names : SWEEP_DEFAULT_NAMES,
  }
}
