// Pure signature of the gate outlines a gating plot draws for a given parent population — extracted
// from GatePlotPanel.vue so it's unit-testable (utils/childGateSig.test.ts). A plot draws the DIRECT
// CHILDREN of its displayed parent as gate outlines; this hashes their path + gate spec so the panel
// can watch it and re-fetch outlines when a child is added, removed, or its gate geometry changes.
// (The parent's own popVersion doesn't move when a CHILD changes, so watching the parent isn't enough.)

// Structural, store-independent (mirrors startDot.ts's EdgeLite): just the fields the signature reads.
export interface FlatPopLite { path: string; parent: string; gate?: unknown }

/**
 * Signature of `parent`'s direct-child gate outlines. Changes iff the set of children under `parent`
 * changes OR one of their gate specs changes; unaffected by siblings, grandchildren, or the parent's
 * own gate. Pops with no gate (e.g. cluster/filter pops) serialise stably as `null`.
 */
export function childGateSignature(flat: readonly FlatPopLite[], parent: string): string {
  return flat
    .filter(p => p.parent === parent)
    .map(p => `${p.path}:${JSON.stringify(p.gate ?? null)}`)
    .join('|')
}
