import { computed, watch, type ToRefs, type Ref, type WritableComputedRef } from 'vue'

/**
 * Shiny-`reactiveValues`-style persisted view state.
 *
 * Given a STORE-BACKED reactive bag `Ref` (e.g. the `canvasPanels` per-canvas `shared` object) and a
 * `defaults` literal, this seeds any missing keys and returns one ref per option. Each ref reads/writes
 * the CURRENT bag, so ALL options declared in `defaults` persist automatically — nothing to wire
 * per-field. Convention: **put every user-settable option in `defaults`**; that's the only step.
 *
 * The refs track the bag's IDENTITY, not just its contents: when `bag.value` becomes a different object
 * (e.g. a per-image canvas key rebinds `shared` to another image's entry — see `useCanvasPanels`), the
 * refs re-seed defaults into and read/write the new bag. This is what makes global-scope canvas state
 * (highlights, line width, scope, styling) per-image across every module page from one place, with no
 * per-page code. (Contrast: a plain `ref()` would silently reset on remount — the bug this kills.)
 */
export function useViewState<T extends object>(bag: Ref<Record<string, unknown>>, defaults: T): ToRefs<T> {
  const seed = (b: Record<string, unknown>) => {
    for (const k in defaults) if (b[k] === undefined) b[k] = (defaults as Record<string, unknown>)[k]
  }
  // Re-seed whenever the bag identity changes (immediate covers the initial bag).
  watch(bag, seed, { immediate: true })
  const out = {} as Record<string, WritableComputedRef<unknown>>
  for (const k in defaults) {
    out[k] = computed({
      get: () => { const v = bag.value[k]; return v === undefined ? (defaults as Record<string, unknown>)[k] : v },
      set: v => { bag.value[k] = v },
    })
  }
  return out as unknown as ToRefs<T>
}
