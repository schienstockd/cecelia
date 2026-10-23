// The one timed confirmation flash — "Copied", "Saved", "Applied": mark a key on for `ms`, then clear.
//
//   const { isOn, flash } = useFlash(ms)          // keyed (per-row buttons); '' is the single button
//   const { saved, flash } = useSavedFlash()       // the "Saved" confirmation after a write
//
// Re-flashing while one is pending restarts the window rather than letting the first timer clear the
// second flash early. useCopyFlash is this plus the clipboard write.
import { ref, computed, onUnmounted } from 'vue'

/** How long a "Saved"/"Applied" confirmation stays up. */
export const SAVED_FLASH_MS = 2000

export function useFlash(ms: number) {
  const onKey = ref<string | null>(null)
  let timer: ReturnType<typeof setTimeout> | null = null

  /** True when this key (or the single button) is inside its flash window. */
  const isOn = (key = '') => onKey.value === key

  /** Start (or restart) the flash for `key`. */
  function flash(key = '') {
    onKey.value = key
    if (timer) clearTimeout(timer)
    timer = setTimeout(() => { onKey.value = null; timer = null }, ms)
  }

  onUnmounted(() => { if (timer) clearTimeout(timer) })

  return { onKey, isOn, flash }
}

/** The "Saved" confirmation: call `flash()` after a write succeeds; `saved` is true for SAVED_FLASH_MS. */
export function useSavedFlash(ms: number = SAVED_FLASH_MS) {
  const { isOn, flash } = useFlash(ms)
  return { saved: computed(() => isOn()), flash: () => flash() }
}
