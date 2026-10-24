// The "Copied!" flash that every copy affordance shares — copy the text, mark it copied for
// COPY_FLASH_MS, then clear. Supports both shapes the app already needed:
//
//   const { isCopied, copy } = useCopyFlash()   // isCopied() for one button, isCopied(key) per row
//
// Keyed and unkeyed use the same instance — a key of `''` is the single-button case. The timer is
// useFlash's, so a re-copy restarts the window (see composables/useFlash.ts). See utils/clipboard.ts
// for why this is centralised.
import { copyText, COPY_FLASH_MS } from '../utils/clipboard'
import { useFlash } from './useFlash'

export function useCopyFlash(ms: number = COPY_FLASH_MS) {
  const { onKey: copiedKey, isOn: isCopied, flash } = useFlash(ms)

  /** Copy `text`; flash only if it actually reached the clipboard. Returns that success. */
  async function copy(text: string, key = ''): Promise<boolean> {
    const ok = await copyText(text)
    if (ok) flash(key)
    return ok
  }

  return { copiedKey, isCopied, flash, copy }
}
