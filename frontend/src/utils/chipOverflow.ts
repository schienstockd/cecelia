// The "+N" chip row (components/ChipOverflow.vue) — the pure split: which chips a cell shows and which
// collapse behind the "+N". A cell that wrapped every chip grew a row per tag (the Movies list's tag
// column), so a list shows the first `max` and keeps the rest one hover away.

export interface ChipSplit<T> { shown: T[], hidden: T[] }

/** The first `max` items shown (at least one), the rest hidden. */
export function splitChips<T>(items: readonly T[], max: number): ChipSplit<T> {
  const n = Math.max(1, Math.floor(Number.isFinite(max) ? max : 1))
  return { shown: items.slice(0, n), hidden: items.slice(n) }
}
