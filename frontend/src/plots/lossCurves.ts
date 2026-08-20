/**
 * Reading a training run's loss history — the arithmetic behind `FlowTrainingView`.
 *
 * Extracted from the SFC because it is where the one thing that can be silently WRONG lives: coastal
 * records each loss term BEFORE its weight and reports `total` as the weighted sum, so comparing the
 * raw curves ranks the terms by the wrong number. A term at 0.9 with weight 0 contributes nothing; a
 * term at 0.05 with weight 2.0 contributes twice as much. Pairing each curve with its weight is the
 * whole job, and it is worth a test rather than a line inside a render function.
 */

/** Per-epoch values keyed by loss term, as `opticalFlow.train` writes them into the manifest. */
export type LossCurves = Record<string, number[]>

export interface LossSeries {
  term: string
  /** Per-epoch values, weighted unless `raw` was asked for, less the floor if `minusFloor` was. */
  values: number[]
  /** The weight this term was trained with. `1` when the manifest records none. */
  weight: number
  /** Same term on the held-out set, when the run had one. Absent otherwise. */
  val?: number[]
  /** True when this term had a recorded floor and it was subtracted. */
  floored?: boolean
}

/** `val_temporal` → `temporal`; anything else → `null`. */
const valTermOf = (key: string): string | null =>
  key.startsWith('val_') ? key.slice(4) : null

/**
 * `total`'s own floor: the same weighted sum `total` is.
 *
 * Derived rather than recorded, because it is not independent — coastal writes `total` as
 * `Σ weight × term`, so its floor is `Σ weight × floor(term)` by construction. Storing it as well
 * would create a number that can disagree with the terms it is made of.
 *
 * Terms with no recorded floor contribute 0, which is correct rather than a gap: the contrastive
 * terms (temporal, variance, warp, boundary) are hinges whose minimum genuinely is 0, so the only
 * terms that could contribute are the BCE ones, and those are exactly the ones with floors.
 */
function totalFloor(floors: LossCurves, weights?: Record<string, number> | null,
                    prefix = ''): number[] | undefined {
  const parts = Object.entries(floors)
    .filter(([k]) => (prefix ? k.startsWith(prefix) : !k.startsWith('val_')))
    .map(([k, vals]) => [weights?.[k.slice(prefix.length)] ?? 1, vals] as const)
    .filter(([w, vals]) => w !== 0 && vals.length)
  if (!parts.length) return undefined
  const n = Math.min(...parts.map(([, vals]) => vals.length))
  return Array.from({ length: n }, (_, i) => parts.reduce((sum, [w, vals]) => sum + w * vals[i]!, 0))
}

/**
 * The curves to draw, ordered `total` first and then alphabetically.
 *
 * `total` leads because it is the reference every other line is judged against, and it is already
 * the weighted sum — so it is never scaled again, whichever mode is on. `weight` defaults to 1 for a
 * term the manifest has no weight for: a model trained before `lossWeights` existed should show its
 * curves unscaled rather than vanish at zero.
 *
 * **`minusFloor` is what makes a converged run readable.** Every BCE term fits a SOFT target, so its
 * minimum is that target's entropy — a constant of the DATA, not of the model. On flow.cyto
 * `foreground` settles at 0.2651 against a floor of 0.2650: the whole remaining model error is
 * 0.0001, and 85% of the plotted TOTAL is a constant no model can move. Read raw, that run looks
 * like it stopped learning after five epochs; read minus its floor, it is converged and the curve
 * goes where the axis says it should. Terms with no recorded floor are left alone rather than
 * dropped — the contrastive ones genuinely bottom out at 0.
 *
 * **`val_<term>` is attached to its term, not listed beside it.** A run with a held-out split writes
 * both, and treating them as sixteen independent series would be wrong twice over: the chip list
 * would double, and — the part that matters — the pair would get two different colours, when the
 * only thing anyone reads off a validation curve is the GAP between it and its own training curve.
 * Two colours make that comparison a lookup; one colour and a dashed line make it the picture.
 *
 * The val curve is scaled by the same weight, because it has to be: the two are only comparable if
 * they are the same quantity, and `val_total` is already coastal's weighted sum for the same reason
 * `total` is.
 */
export function lossSeries(curves?: LossCurves | null, weights?: Record<string, number> | null,
                           raw = false, floors?: LossCurves | null,
                           minusFloor = false): LossSeries[] {
  const all = curves ?? {}
  const fl = floors ?? {}
  const out: LossSeries[] = []
  for (const [term, values] of Object.entries(all)) {
    if (!values?.length || valTermOf(term)) continue
    const weight = term === 'total' ? 1 : (weights?.[term] ?? 1)
    // A term at weight 0 contributes NOTHING to the total, so weighted it is the constant 0 — and a
    // flat line on the axis reads as "trained to nothing", which is the opposite of "switched off".
    // Dropped rather than annotated: the chip tooltip that used to carry this was a caption on a
    // misleading picture. `raw` brings it back, which is what `raw` is for — inspecting one term's
    // own scale while tuning its weight.
    if (weight === 0 && !raw) continue
    const scale = (vs: number[]) => raw || weight === 1 ? vs : vs.map(v => v * weight)
    // `total`'s floor is the weighted sum and is never scaled again, exactly as `total` is not.
    const floorOf = (p: '' | 'val_') => term === 'total'
      ? totalFloor(fl, weights, p === 'val_' ? 'val_' : '')
      : (fl[`${p}${term}`]?.length ? scale(fl[`${p}${term}`]!) : undefined)
    const floor = minusFloor ? floorOf('') : undefined
    const valFloor = minusFloor ? floorOf('val_') : undefined
    // Element-wise, and only as far as BOTH run: a floor series short of the curve (a coastal that
    // stopped recording it mid-run) leaves the later epochs unsubtracted rather than dropping them,
    // so the curve never silently loses its tail.
    const less = (vs: number[], f?: number[]) =>
      f ? vs.map((v, i) => (i < f.length ? v - f[i]! : v)) : vs
    const val = all[`val_${term}`]
    out.push({
      term, weight, values: less(scale(values), floor),
      ...(val?.length ? { val: less(scale(val), valFloor) } : {}),
      ...(floor ? { floored: true } : {}),
    })
  }
  return out.sort((a, b) =>
    a.term === 'total' ? -1 : b.term === 'total' ? 1 : a.term.localeCompare(b.term))
}

/**
 * The shown series as one row per epoch, one column per term — the shape a spreadsheet wants.
 *
 * A long/tidy dump (epoch, term, loss) would make the obvious next question — plot these against
 * each other — a pivot first. Epoch is 1-based, matching the axis.
 */
export function lossTable(series: LossSeries[]): Record<string, number>[] {
  const n = series.reduce((m, s) => Math.max(m, s.values.length, s.val?.length ?? 0), 0)
  return Array.from({ length: n }, (_, i) => {
    const row: Record<string, number> = { epoch: i + 1 }
    // A shorter series (a term coastal stopped recording mid-run) leaves the cell out rather than
    // writing a 0, which would read as "this term reached zero".
    for (const s of series) {
      if (i < s.values.length) row[s.term] = s.values[i]!
      // Its own column, next to its term — the export exists so the train/val gap can be worked
      // out in a spreadsheet, and that is a subtraction between two columns.
      if (s.val && i < s.val.length) row[`val_${s.term}`] = s.val[i]!
    }
    return row
  })
}
