// Ephemeral state for the canvas Share mode.
//
// A canvas host (`SummaryCanvas` today, other module pages later) enters share mode when Kiwi's
// canvas Share button fires. The user then drags / clicks to select panels and either confirms
// ("Share (N)") or cancels (ESC / outside click). This composable holds the tiny state machine
// that drives that flow — kept out of the SFC so the transitions are unit-testable and every
// canvas host uses the same rules.
//
// The overlay itself is `components/canvas/CanvasSelectionOverlay.vue`; the compositor + POST live
// separately. This composable is oblivious to both.

import { computed, ref } from 'vue'

export function useCanvasShareSelection() {
  // `null` = not in share mode. A Set of panelIds otherwise. The Set (rather than an array) makes
  // "add / remove / has" cheap for hit-testing during a drag.
  const selected = ref<Set<number> | null>(null)

  const active = computed(() => selected.value !== null)
  const count = computed(() => selected.value?.size ?? 0)
  const has = (id: number) => selected.value?.has(id) ?? false

  function begin() {
    // Fresh empty selection — every entry to share mode starts blank, even after a prior confirm
    // or cancel. Persisting the last selection across entries would be misleading (the layout may
    // have changed).
    selected.value = new Set()
  }
  function end() { selected.value = null }
  function add(id: number)    { selected.value?.add(id) }
  function remove(id: number) { selected.value?.delete(id) }
  function toggle(id: number) { has(id) ? remove(id) : add(id) }
  /** Replace the current selection wholesale — used at the end of a drag once the intersected
   *  panels are known. */
  function set(ids: Iterable<number>) { selected.value = new Set(ids) }

  return { selected, active, count, has, begin, end, add, remove, toggle, set }
}
