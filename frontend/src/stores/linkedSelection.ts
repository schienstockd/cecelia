import { defineStore, acceptHMRUpdate } from 'pinia'
import { computed, ref } from 'vue'

// Linked brushing bag (LINKED_BRUSHING_PLAN.md Decision 1). One selection at a time, tagged by
// SCOPE (`tracks` or `cells` — the id namespace, not the biology). Set by a producer (a plot's
// brush primitive), read by every subscribing consumer (plots that dim non-selected rows, the
// viewer, cell cards, TrackScheme, …).
//
// Session-only — no localStorage bridge (same discipline as `viewer.ts`'s TrackHighlight /
// PickHighlight, which this bag GENERALISES; PR #1165 already fans those to 10 subscribing plot
// families, and the migration path is Follow-up 1 of the plan). Clears on project swap and on
// page navigation (the module page calls `clear()` in `onBeforeUnmount`).
//
// Additive with the shipped `TrackHighlight` / `PickHighlight`: nothing renamed, nothing
// removed — MVP writers write here AND (during the transition) to the existing bags so the
// shipped fan-out keeps working unchanged. See LINKED_BRUSHING_PLAN.md Decision 7.

export type LinkedSelectionScope = 'tracks' | 'cells'

export interface LinkedSelectionBag {
  /** ID namespace. Track ids and label (cell) ids live in disjoint spaces — a selection is one
   *  or the other, never both. Consumers filter by scope; a `tracks` selection does not affect
   *  a `cells`-scoped subscriber, and vice versa. */
  scope: LinkedSelectionScope
  /** The selected id set. Empty array is meaningful and distinct from `null` at the bag level —
   *  see `isEmpty` below (a bag with `ids: []` reads as "cleared"). */
  ids: number[]
  /** Free-form label of who set this — a plot's `persistKey`, a component name, or `'claude'`
   *  when a `mark_*` MCP tool eventually migrates here (Follow-up 1). Rendered as a small chip
   *  on the "Clear selection" affordance so the user can see WHERE the selection came from. */
  source: string
  /** Optional plot id the selection came from — same value the producer registered via
   *  `useLinkedSelectionSource(plotId, …)`. Used by the source plot itself to render its own
   *  "I'm the source" affordance distinct from the "I'm a subscriber" dim. Absent when the
   *  writer isn't a plot (e.g. the "Show" button on TrackSchemeView, or Claude via `mark_*`). */
  sourcePlotId?: string
  /** Optional per-source ids. Track_ids and cell labels are per-(image, segmentation) numeric
   *  spaces — track_id=5 exists in every tracked segmentation, and a single image with two
   *  segmentations (B and T) can have colliding ids as well as two images can. When a producer
   *  knows which (imageUid, valueName) each id came from, it fills this map; subscribing plots
   *  that have per-dot source tags then match on `(uid, vn, id)` and only highlight the right
   *  source's dots. Key format is `${uid}\u0000${vn}` (null-separator compound) so both sides
   *  construct it the same way without collisions on real names. Absent when the producer is
   *  per-source by construction (Show button, MCP mark_*) — subscribers fall back to the flat
   *  `ids`. */
  perSource?: Record<string, number[]>
}

/** Compound key format for `perSource`. Exported so producers/consumers build the same key. */
export const linkedSourceKey = (uid: string, vn: string): string => `${uid}\u0000${vn}`

export const useLinkedSelectionStore = defineStore('linkedSelection', () => {
  /** The current bag, or `null` when nothing is selected. Deliberately `null` (not an empty
   *  bag) for the idle state — subscribers use `isEmpty` and the dim rule only fires when
   *  a selection is actively set, so the empty state renders as "full opacity, no highlight". */
  const bag = ref<LinkedSelectionBag | null>(null)

  const isEmpty = computed(() => bag.value === null || bag.value.ids.length === 0)

  /** Set the selection. A caller with an empty ids list clears the bag (idempotent with `clear`)
   *  so a producer can drop a brush that happened to select nothing without a special branch. */
  function set(next: LinkedSelectionBag) {
    if (!next.ids.length) { bag.value = null; return }
    // Defensive-copy the ids so a later mutation on the caller's array can't retroactively
    // change what subscribers see — same idiom as `TrackHighlight` setters.
    bag.value = {
      scope: next.scope,
      ids: [...next.ids],
      source: next.source,
      ...(next.sourcePlotId ? { sourcePlotId: next.sourcePlotId } : {}),
      // Defensive-copy the per-source structure too — same reason as ids: caller may mutate.
      ...(next.perSource ? { perSource: Object.fromEntries(
        Object.entries(next.perSource).map(([k, v]) => [k, [...v]])
      ) } : {}),
    }
  }

  /** Drop the selection. Idle state = `bag === null` (not `{ids: []}`) — subscribers key off
   *  `isEmpty`, and `null` is the cleaner "nothing to do" sentinel. */
  function clear() { bag.value = null }

  return { bag, isEmpty, set, clear }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useLinkedSelectionStore, import.meta.hot))
