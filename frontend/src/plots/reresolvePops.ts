// Re-resolve a captured plot-panel `sel` (series-target keys shaped `popType::valueName + popPath`)
// against the CURRENT segmentation populations. A capture's tkeys bake in the value_name that was
// live at share time; if the user is now viewing a different image or segmentation on
// zoom-to-source, those tkeys won't match anything in current segPops and SummaryCanvas's
// segPops-watcher prunes them to empty — the "populations not restored" bug.
//
// Two-pass resolution per captured entry:
//   PASS 1 — UID match. If a captured pop UID is non-empty AND the CURRENT segPops has a
//     population with that same UID, emit the tkey for that pop. UIDs are stable across
//     pop RENAMES / MOVES within the same image (see `PopulationMap._fresh_pop_uid`), so this is
//     the strongest identity match. Pop UIDs are per-image — a match across images doesn't happen
//     by coincidence.
//   PASS 2 — path fallback. UID didn't match (renamed since capture beyond the current image, or
//     the capture predates uid capture, or the current segPops populations lost their uid). Match
//     by `(popType, popPath)`:
//       (a) if a current group has the SAME value_name AND a matching pop → preserve the original
//           tkey verbatim (identity restore — the common case);
//       (b) else emit tkeys for EVERY current group carrying a matching pop (the user's current
//           image selection now provides the pops; a set-scoped canvas pools across them);
//       (c) if no group carries the pop → drop the entry (pop was renamed AND removed from every
//           loaded segmentation, or removed outright).
//
// Deliberately does NOT read `dataSlice.imageUids` — restoring the image selection is the parent's
// decision; this only re-maps pops given whichever images are currently loaded.
//
// `capturedUids` is parallel to `capturedSel` (same length; entries an empty string when the
// capture predates pop uids or the pop had none). A shorter array is treated as trailing empty
// uids so legacy captures round-trip through path fallback.
//
// Pure. Tests: `reresolvePops.test.ts`.

import type { SegmentationPops } from './types'
import { parseTkey, tkey } from './series'

export function reresolvePops(
  capturedSel: readonly string[],
  capturedUids: readonly string[],
  segPops: readonly SegmentationPops[],
): string[] {
  const out: string[] = []
  const seen = new Set<string>()
  const push = (k: string) => { if (!seen.has(k)) { seen.add(k); out.push(k) } }

  // Build the uid → (valueName, pop) index once; used for pass 1 lookups.
  const byUid = new Map<string, { valueName: string; popType: string; path: string }>()
  for (const g of segPops) {
    for (const p of g.populations) {
      if (p.uid) byUid.set(p.uid, { valueName: g.valueName, popType: p.popType, path: p.path })
    }
  }

  for (let i = 0; i < capturedSel.length; i++) {
    const raw = capturedSel[i]
    const capUid = capturedUids[i] ?? ''
    if (capUid) {
      const hit = byUid.get(capUid)
      if (hit) { push(tkey(hit.popType, hit.valueName, hit.path)); continue }
    }
    const t = parseTkey(raw)
    const sameVn = segPops.find(g => g.valueName === t.valueName)
    const hasSame = sameVn?.populations.some(p => p.popType === t.popType && p.path === t.pop)
    if (hasSame) {
      push(tkey(t.popType, t.valueName, t.pop))
      continue
    }
    for (const g of segPops) {
      for (const p of g.populations) {
        if (p.popType !== t.popType || p.path !== t.pop) continue
        push(tkey(p.popType, g.valueName, p.path))
      }
    }
  }
  return out
}
