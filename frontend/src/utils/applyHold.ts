// Refreshes held back until Apply — the update half of manual apply (the selection half is
// `manualApplyStaging.ts`). With manual apply on, a host routes each "a population changed, redraw"
// through `hold`: the viewer after a gate write, the summary plots after a gate edited elsewhere.
// Apply (or switching manual apply off) runs every held refresh once. Same key = same refresh, so ten
// gate writes queue ONE viewer reload; `count` still reports all ten for the chip.
// Pure (no Vue); `usePopSelectionMode` owns the instance and mirrors `count` into a ref.

export interface ApplyHold {
  /** Queue `run` under `key` (replacing an earlier one under the same key). */
  hold(key: string, run: () => void): void
  /** Forget a held refresh that has been done some other way (e.g. an explicit viewer reload). */
  drop(key: string): void
  /** Run every held refresh, oldest key first, and clear the queue. */
  flush(): void
  /** Held writes since the last flush, counting repeats of a key. */
  readonly count: number
}

export function createApplyHold(onChange: (count: number) => void = () => {}): ApplyHold {
  const held = new Map<string, { run: () => void; n: number }>()
  const total = () => [...held.values()].reduce((s, h) => s + h.n, 0)
  return {
    hold(key, run) {
      held.set(key, { run, n: (held.get(key)?.n ?? 0) + 1 })
      onChange(total())
    },
    drop(key) {
      if (held.delete(key)) onChange(total())
    },
    flush() {
      if (!held.size) return
      const runs = [...held.values()].map(h => h.run)
      held.clear()
      onChange(0)
      for (const run of runs) run()
    },
    get count() { return total() },
  }
}
