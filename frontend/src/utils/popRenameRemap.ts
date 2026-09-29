// Rename-preserving remap for path-keyed highlight sets. On a pop mutation (add / delete /
// rename) the pop's PATH may change but its `uid` (`PopulationMap._fresh_pop_uid`, server-side)
// does not. Watchers keyed on a `path[]` (`ClusterPlots.vue`'s `highlighted`, `useSummaryData`'s
// `gSel`) therefore need to look up the old path's uid and swap in the pop's NEW path — a plain
// path-intersect drops the entry silently, which reads as "the plot didn't update".
//
// Semantics — one function `makePopPathRemap(oldItems, newItems)` returns `path -> newPath | null`:
//  • old path known + has uid + uid still exists → returns new path (RENAME preserved)
//  • old path known + has uid + uid vanished    → null (DELETE, drop)
//  • old path unknown OR uid empty              → path-presence fallback: kept if still in the
//    new set (first-mount with persisted highlights, plus synthetic pops that carry no uid —
//    `/labels`, the "all cells" root, derived `_tracked` sets, per `SegmentationPops` docstring)
//
// Add is not represented — a new pop isn't in the highlight set until the user clicks the eye,
// so nothing to remap. That gate is deliberate (user confirmed 2026-09-29).

export interface PopIdent { key: string; uid: string }

export function makePopPathRemap(
  oldItems: readonly PopIdent[],
  newItems: readonly PopIdent[],
): (key: string) => string | null {
  const newByUid = new Map<string, string>()
  for (const it of newItems) if (it.uid) newByUid.set(it.uid, it.key)
  const oldByKey = new Map<string, PopIdent>()
  for (const it of oldItems) oldByKey.set(it.key, it)
  const newKeys = new Set(newItems.map(i => i.key))
  return (key) => {
    const old = oldByKey.get(key)
    if (old && old.uid) return newByUid.get(old.uid) ?? null
    // unknown (pre-load / fresh mount with persisted highlights) OR synthetic (no uid) — fall
    // back to path-presence, which matches the pre-uid pruner behaviour.
    return newKeys.has(key) ? key : null
  }
}

// Apply a remap to a list of path-keys, dropping deletes AND unknown-and-absent entries.
export function remapPopKeys(
  keys: readonly string[],
  remap: (key: string) => string | null,
): string[] {
  const out: string[] = []
  for (const k of keys) {
    const next = remap(k)
    if (next !== null) out.push(next)
  }
  return out
}
