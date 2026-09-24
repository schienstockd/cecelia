import { computed } from 'vue'
import { useLinkedSelectionStore, type LinkedSelectionScope } from '../stores/linkedSelection'

// Two-shape API over the linkedSelection store (LINKED_BRUSHING_PLAN.md Decision 2). Producers
// call `useLinkedSelectionSource(plotId, scope)` and get a `set` / `clear` pair — the plot's
// brush primitive is the only thing that should touch the store's `set()` directly, and this
// helper stamps the source metadata for it. Consumers call `useLinkedSelectionSubscriber(scope)`
// and get `isSelected(id)`, `anyActive`, `activeIds` — the only signals a consumer needs to
// drive its dim visual, no store access.
//
// Both shapes are scope-scoped: a `tracks` producer's writes are visible to `tracks` subscribers
// and INVISIBLE to `cells` subscribers, and vice versa. Track ids and label ids live in disjoint
// namespaces (per-vn, but scope is a coarser cut that catches the common case cheaply).

export interface LinkedSelectionSource {
  /** Publish a new selection under this producer's identity. An empty ids list clears the bag
   *  — same "brush selected nothing" idempotency the store guarantees. */
  set(ids: number[]): void
  /** Drop the selection unconditionally. Producers rarely need this — the "Clear selection"
   *  affordance on the page calls the store's `clear()` directly. Kept here so a producer that
   *  owns a specific gesture (a right-click "clear only my selection") has a symmetric handle. */
  clear(): void
}

export function useLinkedSelectionSource(plotId: string,
                                        scope: LinkedSelectionScope): LinkedSelectionSource {
  const store = useLinkedSelectionStore()
  return {
    set(ids: number[]) {
      store.set({ scope, ids, source: plotId, sourcePlotId: plotId })
    },
    clear() { store.clear() },
  }
}

export interface LinkedSelectionSubscriber {
  /** `true` when this row's id is in the active selection AND the active selection's scope
   *  matches this subscriber's scope. Idle state (no selection) reads as `false` — the
   *  consumer's dim rule keys off `anyActive`, not this, so an empty bag renders as
   *  full opacity across the board. */
  isSelected(id: number): boolean
  /** `true` when a selection with a matching scope is active. Consumers dim non-selected rows
   *  ONLY when this is true — an empty bag leaves everything at full opacity. */
  anyActive: import('vue').ComputedRef<boolean>
  /** The active id set as a Set (for O(1) membership checks in tight render loops). Empty
   *  when no matching selection is active.
   *
   *  This is the FLAT view — every id from every `perSource` entry unioned together. Suitable
   *  for a badge count or a single-source subscriber that doesn't discriminate by `(uid, vn, pop)`.
   *  A plot that renders per-source (boxplot/strip) should use `activePerSource` instead — the
   *  flat Set collapses cross-source numeric collisions (track_id=5 exists in every image) and
   *  would light up the wrong dots. See `linkedSourceKey` in `stores/linkedSelection.ts`. */
  activeIds: import('vue').ComputedRef<Set<number>>
  /** Per-source view — a `Map<linkedSourceKey(uid, vn, pop?), Set<number>>` matching the shape
   *  `plot.ts`'s `matches()` closure consumes. Empty map when no matching selection is active or
   *  the producer didn't fill `perSource` (a legacy writer that only knows the flat ids — the
   *  map is still returned empty and callers fall back to `activeIds`). Same specific-first,
   *  vn-key-fallback contract the boxplot/strip renderer already implements. */
  activePerSource: import('vue').ComputedRef<Map<string, Set<number>>>
}

export function useLinkedSelectionSubscriber(scope: LinkedSelectionScope): LinkedSelectionSubscriber {
  const store = useLinkedSelectionStore()
  // Scope-gated view: a bag with a different scope reads as "no selection" to this subscriber,
  // so a `tracks` brush doesn't accidentally dim `cells`-scoped renders. Recomputed lazily via
  // Vue's `computed` — cheap to subscribe to.
  const activeIds = computed<Set<number>>(() => {
    const b = store.bag
    if (!b || b.scope !== scope || b.ids.length === 0) return new Set()
    return new Set(b.ids)
  })
  const activePerSource = computed<Map<string, Set<number>>>(() => {
    const b = store.bag
    if (!b || b.scope !== scope || !b.perSource) return new Map()
    const out = new Map<string, Set<number>>()
    for (const [k, ids] of Object.entries(b.perSource)) {
      if (ids.length) out.set(k, new Set(ids))
    }
    return out
  })
  const anyActive = computed(() => activeIds.value.size > 0)
  return {
    isSelected(id: number) { return activeIds.value.has(id) },
    anyActive,
    activeIds,
    activePerSource,
  }
}
