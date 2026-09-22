/**
 * Priority-picked value_name for a per-panel segmentation (or image-version) chooser.
 *
 * The `eligible` set filters `all` down to the value_names that can carry the panel's data —
 * tracked segmentations for track plots, motif-labelled ones for motif cards, image filepath
 * versions when there's no eligibility filter (`eligible === all`).
 *
 * Priority (matches TrackDiagnostics/TrackScheme's historical `resolveTrackValueName`, which this
 * generalises — see docs/todo/BEHAVIOUR_CARDS_PLAN.md consolidation note):
 *   1. `wanted` if the persisted choice is still eligible → keep the user's pick
 *   2. `active` if the image's active segmentation is eligible → align with what the rest of the app shows
 *   3. first eligible → an arbitrary but deterministic pick
 *   4. `wanted` if it exists in `all` at all → say "not eligible" about a real name rather than an empty string
 *   5. first in `all` → same fallback for the case where `wanted` is also absent
 */
export function resolveValueName(
  wanted: string | undefined, eligible: readonly string[], all: readonly string[] = [],
  active?: string,
): string {
  if (wanted && eligible.includes(wanted)) return wanted
  if (active && eligible.includes(active)) return active
  if (eligible.length) return eligible[0]
  return wanted && all.includes(wanted) ? wanted : (all[0] ?? '')
}
