import { ref, watch, onUnmounted, type Ref } from 'vue'
import { beginPlotLoad, endPlotLoad } from '../utils/plotReady'

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
  // also feed the board-wide load counter (utils/plotReady) so the PDF/CSV export can wait for genuine
  // idle instead of a fixed sleep. `counted` guards against double increments and leaking the count.
  let counted = false
  const clear = () => { if (timer !== null) { clearTimeout(timer); timer = null } }
  const uncount = () => { if (counted) { counted = false; endPlotLoad() } }
  watch(loading, (v) => {
    clear()
    if (v) {
      if (!counted) { counted = true; beginPlotLoad() }
      timer = setTimeout(() => { show.value = true; timer = null }, delayMs)
    } else {
      show.value = false
      uncount()
    }
  }, { immediate: true })
  onUnmounted(() => { clear(); uncount() })   // unmounting mid-load must release its count
  return show
}
