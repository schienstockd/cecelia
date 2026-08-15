// Turning a 0–1 progress fraction into a CSS width — the one place that sum is done.
//
// Four hand-rolled progress bars did it three different ways: `(p * 100).toFixed(1)` (the task list
// and the task manager), `Math.round(p * 100)` (the project panel's export/import row) and a bare
// `p * 100` (the Settings data-patch bar, which therefore rendered `width: 33.33333333333333%`).
// Nobody chose any of those against the others; each was chosen once, alone. See
// `docs/todo/TASK_LIST_UNIFICATION_PLAN.md` → Decision 7a.
//
// It also has to fail soft. `progress` is optional on every task shape in the rail — it is absent
// until the producer emits its first `[PROGRESS] n/total`, and a task that reports no fraction never
// sets it at all. An `undefined` reaching a template as `${undefined * 100}%` is `width: NaN%`, which
// a browser drops silently: the bar renders at its previous width and simply stops moving.

/** One decimal — enough to move smoothly on a long run, short enough not to churn the DOM. */
const PRECISION = 1

/**
 * A CSS width for a 0–1 progress fraction, e.g. `0.5` → `'50.0%'`.
 *
 * Anything that isn't a usable number — `undefined`, `null`, `NaN`, `Infinity` — is `'0%'`, i.e. "we
 * have no reading", never a NaN width. Out-of-range input is clamped rather than rejected: a producer
 * that reports 11 of 10 steps has an off-by-one, and a bar overflowing its track is a worse way to
 * find that out than a full bar.
 */
export function progressWidth(value: number | null | undefined): string {
  if (typeof value !== 'number' || !Number.isFinite(value)) return '0%'
  const pct = Math.min(1, Math.max(0, value)) * 100
  return `${pct.toFixed(PRECISION)}%`
}
