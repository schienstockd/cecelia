import { ref, watch, onUnmounted, type Ref } from 'vue'

/**
 * Delayed loading flag for plot spinners (docs/UI.md → "Plot loading state").
 *
 * Returns a `show` ref that becomes true ONLY if `loading` stays true for `delayMs` (default 350 ms),
 * and resets to false the instant loading ends. This is the "don't flash a spinner on small plots"
 * rule: a fast/cheap plot finishes before the threshold, so `show` never flips — only a genuinely
 * heavy load reveals the wheel. Use it in every plot host instead of hand-rolled "…" loading text.
 *
 *   const showSpinner = useDelayedLoading(loading)         // loading: Ref<boolean>
 *   const showSpinner = useDelayedLoading(toRef(props, 'loading'))   // when loading is a prop
 */
export function useDelayedLoading(loading: Ref<boolean>, delayMs = 350): Ref<boolean> {
  const show = ref(false)
  let timer: ReturnType<typeof setTimeout> | null = null
  const clear = () => { if (timer !== null) { clearTimeout(timer); timer = null } }
  watch(loading, (v) => {
    clear()
    if (v) timer = setTimeout(() => { show.value = true; timer = null }, delayMs)
    else show.value = false
  }, { immediate: true })
  onUnmounted(clear)
  return show
}
