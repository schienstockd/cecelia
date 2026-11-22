// The SUMMARY LEVEL of a summary plot: is one datapoint each cell/track ('individual'), or each
// image collapsed to one value ('image' — the pseudoreplication-safe view)? `imageAgg` says how that
// collapse is done, and is meaningless without it.
//
// Why this is persisted EXPLICITLY rather than left to a `?? 'individual'` fallback at each reader:
// the pair is written by one producer (SummaryPanel) and read by three consumers — the plot request,
// the board export, and `board_summaries` on the Julia side, which the observer reads back. A default
// resolved at the point of USE has to be copied into each of them, and the Julia copy cannot even be
// correct: whether a slot HAS a summary level depends on live panel state (a measure is selected, the
// chart is one of the each-dot types, the scope spans images) that the persisted board never records.
// So the producer resolves it once and stores the answer, and every consumer just reads a field.
//
// The same argument covers CLEARING it. Nothing used to remove the pair when the level stopped being
// settable — switch a boxplot to a histogram and a stale `statUnit: 'image'` stayed on disk — so a
// reader could not tell a real setting from a leftover. Presence now means "this slot has a summary
// level, and it is this one"; absence means "this slot has no summary level".

export type StatUnit = 'individual' | 'image' | 'replicate'   // replicate: the set's design names it (utils/experimentDesign.ts)
export type ImageAgg = 'mean' | 'median'

export interface StatUnitState {
  statUnit?: StatUnit
  imageAgg?: ImageAgg
}

export const DEFAULT_STAT_UNIT: StatUnit = 'individual'
export const DEFAULT_IMAGE_AGG: ImageAgg = 'mean'

/**
 * What the persisted pair SHOULD be for a slot, given whether the level is settable there.
 * `{}` when it is not — the fields are removed rather than left at a stale value.
 */
export function resolveStatUnitState(ui: StatUnitState, settable: boolean): StatUnitState {
  if (!settable) return {}
  return {
    statUnit: ui.statUnit ?? DEFAULT_STAT_UNIT,
    imageAgg: ui.imageAgg ?? DEFAULT_IMAGE_AGG,
  }
}

/**
 * Bring `ui` in line with `resolveStatUnitState`, in place. Returns whether anything changed, so a
 * caller can drive this from a reactive effect without looping: applying it twice is a no-op.
 */
export function applyStatUnitState(ui: StatUnitState, settable: boolean): boolean {
  const next = resolveStatUnitState(ui, settable)
  let changed = false
  for (const key of ['statUnit', 'imageAgg'] as const) {
    const want = next[key]
    if (want === undefined) {
      if (key in ui) { delete ui[key]; changed = true }
    } else if (ui[key] !== want) {
      (ui[key] as StatUnit | ImageAgg) = want
      changed = true
    }
  }
  return changed
}

/**
 * The n per group, as the plot footer and the design dialog's sketch both state it: one count per
 * group ("n = 5 vs 3 mice"), "n = 5 mice" for one, a range past four groups ("n = 3–5 mice per
 * group"), where a list stops being readable. '' when there is nothing to count. `noun` names the point.
 */
export function unitCountLabel(ns: number[], noun: { one: string; many: string }): string {
  const xs = ns.filter(n => Number.isFinite(n) && n > 0)
  if (!xs.length) return ''
  const name = xs.every(n => n === 1) ? noun.one : noun.many
  if (xs.length === 1) return `n = ${xs[0]} ${name}`
  if (xs.length <= 4) return `n = ${xs.join(' vs ')} ${name}`
  const lo = Math.min(...xs), hi = Math.max(...xs)
  return `n = ${lo === hi ? lo : `${lo}–${hi}`} ${name} per group`
}

/**
 * The line an exported summary plot carries under it, as a figure legend states it (Lord et al. 2020,
 * SuperPlots, J Cell Biol 219(6):e202001064, doi:10.1083/jcb.202001064): the n, what each point is, and
 * what the test ran on — "n = 5 vs 3 mice · mouse means, cells beneath · Mann-Whitney U (two-sided) on
 * the mouse means". '' when each point is a cell or track (`nLabel` is '' then): no unit to state.
 */
export function plotCaption(o: {
  nLabel: string; statUnit: StatUnit; imageAgg: ImageAgg; noun: { one: string; many: string }; test?: string
}): string {
  if (!o.nLabel || o.statUnit === 'individual') return ''
  const points = `${o.noun.one} ${o.imageAgg}s`
  const parts = [o.nLabel, o.statUnit === 'replicate' ? `${points}, cells beneath` : points]
  if (o.test) parts.push(`${o.test} on the ${points}`)
  return parts.join(' · ')
}
