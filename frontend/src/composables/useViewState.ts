import { toRefs, type ToRefs, type Ref } from 'vue'

/**
 * Shiny-`reactiveValues`-style persisted view state.
 *
 * Given a STORE-BACKED reactive bag (e.g. the `canvasPanels` per-canvas `shared` object) and a
 * `defaults` literal, this seeds any missing keys and returns the bag as refs (`toRefs`). Every
 * returned ref reads/writes the bag, so ALL options declared in `defaults` persist across navigation
 * automatically — there is nothing to wire per-field. The convention is therefore simple and
 * forget-proof: **put every user-settable option in the `defaults` object**; adding one there is the
 * only step needed to persist it. (Contrast: a plain `ref()` in the component would silently reset on
 * remount — the bug class this exists to kill.)
 *
 * The bag must be reactive (it is, when it comes from a Pinia store) so the refs stay live.
 */
export function useViewState<T extends object>(bag: Ref<Record<string, unknown>>, defaults: T): ToRefs<T> {
  for (const k in defaults) {
    if (bag.value[k] === undefined) (bag.value as Record<string, unknown>)[k] = (defaults as Record<string, unknown>)[k]
  }
  return toRefs(bag.value as T)
}
