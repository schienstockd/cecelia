// A page's default plots (docs/todo/GUI_LAYOUT_PLAN.md, M6; mock frame 1): the registry specs that name
// the page in `defaultOn`, in registry order. SummaryCanvas places them ONCE — when that canvas is first
// seen with no panels — and records that it did, so a user who removes them is never re-seeded.
import type { PlotSpec } from '../plots/types'

export function defaultSpecIds(specs: readonly Pick<PlotSpec, 'id' | 'defaultOn'>[], module: string | undefined): string[] {
  if (!module) return []
  return specs.filter(s => s.defaultOn?.includes(module)).map(s => s.id)
}

/** Seed now? Only an empty canvas that has never been seeded, on a page that has defaults. */
export function shouldSeedDefaults(o: { panelCount: number; seeded: boolean; defaults: number }): boolean {
  return !o.seeded && o.panelCount === 0 && o.defaults > 0
}
