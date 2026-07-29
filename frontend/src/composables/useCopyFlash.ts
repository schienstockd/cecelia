// The "Copied!" flash that every copy affordance shares — copy the text, mark it copied for
// COPY_FLASH_MS, then clear. Supports both shapes the app already needed:
//
//   const { copied, copy } = useCopyFlash()                 // single button
//   const { isCopied, copy } = useCopyFlash()               // per-row (pass a key: uid, task id, …)
//
// Keyed and unkeyed use the same instance — a key of `''` is the single-button case. Re-copying while
// a flash is pending restarts it rather than letting the first timer clear the second flash early
// (the bug LabLogPanel had to fix by hand). See utils/clipboard.ts for why this is centralised.
import { ref, onUnmounted } from 'vue'
import { copyText, COPY_FLASH_MS } from '../utils/clipboard'

export function useCopyFlash(ms: number = COPY_FLASH_MS) {
  const copiedKey = ref<string | null>(null)
  let timer: ReturnType<typeof setTimeout> | null = null

  /** True when this key (or the single button) is inside its flash window. */
  const isCopied = (key = '') => copiedKey.value === key

  /** Start (or restart) the flash for `key` without touching the clipboard. */
  function flash(key = '') {
    copiedKey.value = key
    if (timer) clearTimeout(timer)
    timer = setTimeout(() => { copiedKey.value = null; timer = null }, ms)
  }

  /** Copy `text`; flash only if it actually reached the clipboard. Returns that success. */
  async function copy(text: string, key = ''): Promise<boolean> {
    const ok = await copyText(text)
    if (ok) flash(key)
    return ok
  }

  onUnmounted(() => { if (timer) clearTimeout(timer) })

  return { copiedKey, isCopied, flash, copy }
}
