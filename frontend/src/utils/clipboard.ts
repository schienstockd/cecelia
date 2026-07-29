// Copy-to-clipboard — the ONE implementation. Ten surfaces had hand-rolled this (image/set UID chips,
// task logs ×2, packages list, project UID, image metadata paths, the Claude chat prompt + its setup
// command), and they had diverged in two ways that mattered:
//
//   1. Only 3 of them carried the textarea + execCommand fallback, so on the other 6 a copy silently
//      did nothing whenever `navigator.clipboard` is unavailable (non-secure context / older WebView).
//   2. The "Copied!" flash ran for 1200 / 1500 / 2500 ms depending on the file, and one had no flash
//      at all — the same affordance felt different on every screen.
//
// `copyText` fixes (1) in one place; `useCopyFlash` (composables/useCopyFlash.ts) fixes (2). New copy
// affordances MUST use them — see docs/UI.md → UX-primitive catalog.

/** Flash duration for a "Copied!" confirmation. One number, so every surface agrees. */
export const COPY_FLASH_MS = 1500

/**
 * Write `text` to the clipboard. Prefers the async Clipboard API and falls back to a hidden
 * textarea + `execCommand('copy')` when it's unavailable or blocked (http:// origins, embedded
 * WebViews). Returns whether the text made it out, so a caller can flash only on success.
 *
 * DOM-touching by nature — the pure part (flash timing/keys) lives in the composable and is
 * unit-tested there.
 */
export async function copyText(text: string): Promise<boolean> {
  try {
    await navigator.clipboard.writeText(text)
    return true
  } catch {
    // fall through to the legacy path
  }
  try {
    const ta = document.createElement('textarea')
    ta.value = text
    ta.style.position = 'fixed'
    ta.style.opacity = '0'
    document.body.appendChild(ta)
    ta.select()
    const ok = document.execCommand('copy')
    document.body.removeChild(ta)
    return ok
  } catch {
    return false
  }
}
