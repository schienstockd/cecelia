import { ref, onBeforeUnmount } from 'vue'

// Shared drag-to-resize for a right-hand sidebar whose drag handle sits on its LEFT edge (dragging
// left widens it). Used by TaskRunner and MetadataPanel so the resize behaviour + persistence live in
// ONE place. Width is clamped to [min, max] and, when `storageKey` is given, remembered in
// localStorage (a user-settable option must survive remount — see docs/UI.md).
export function usePanelResize(opts: { min?: number; max?: number; default?: number; storageKey?: string } = {}) {
  const min = opts.min ?? 220
  const max = opts.max ?? 600
  const clamp = (w: number) => Math.min(max, Math.max(min, w))

  const stored = opts.storageKey ? Number(localStorage.getItem(opts.storageKey)) : NaN
  const width = ref(Number.isFinite(stored) && stored > 0 ? clamp(stored) : (opts.default ?? 280))

  let dragging = false, startX = 0, startW = 0

  function onMove(e: MouseEvent) {
    if (!dragging) return
    width.value = clamp(startW + (startX - e.clientX))   // handle on LEFT edge → drag left = wider
  }
  function onEnd() {
    if (!dragging) return
    dragging = false
    document.body.style.userSelect = ''
    document.body.style.cursor = ''
    window.removeEventListener('mousemove', onMove)
    window.removeEventListener('mouseup', onEnd)
    if (opts.storageKey) { try { localStorage.setItem(opts.storageKey, String(Math.round(width.value))) } catch { /* ignore */ } }
  }
  function onResizeStart(e: MouseEvent) {
    dragging = true
    startX = e.clientX
    startW = width.value
    document.body.style.userSelect = 'none'
    document.body.style.cursor = 'col-resize'
    window.addEventListener('mousemove', onMove)
    window.addEventListener('mouseup', onEnd)
    e.preventDefault()
  }

  onBeforeUnmount(onEnd)   // release listeners if unmounted mid-drag
  return { width, onResizeStart }
}
