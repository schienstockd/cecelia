import { ref, computed, onBeforeUnmount } from 'vue'

// Shared drag-to-resize for a right-hand sidebar whose drag handle sits on its LEFT edge (dragging
// left widens it). Used by TaskRunner and MetadataPanel — and by `CollapsiblePanel.vue`, which adds
// the collapse half — so the resize behaviour + persistence live in ONE place. Width is clamped to
// [min, max] and, when `storageKey` is given, remembered in localStorage (a user-settable option must
// survive remount — see docs/UI.md).
//
// Bind `widthStyle`, not `width`: an auto-width panel has no number, and every consumer would
// otherwise spell the same null check (or silently render `nullpx`).
export function usePanelResize(opts: { min?: number; max?: number; default?: number | null;
                                       storageKey?: string } = {}) {
  const min = opts.min ?? 220
  const max = opts.max ?? 600
  const clamp = (w: number) => Math.min(max, Math.max(min, w))

  // `default: null` = SIZE TO CONTENT until the user drags — the panel gets no width style at all.
  // ModuleLayout's right panel has always behaved that way (a module page that never sets a width
  // lets its runner size itself), so the shared composable has to be able to express it or adopting
  // it would silently pin every one of those panels to a number.
  const auto = opts.default === null
  const stored = opts.storageKey ? Number(localStorage.getItem(opts.storageKey)) : NaN
  const width = ref<number | null>(
    Number.isFinite(stored) && stored > 0 ? clamp(stored) : (auto ? null : (opts.default ?? 280)))

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
    if (opts.storageKey && width.value !== null) {
      try { localStorage.setItem(opts.storageKey, String(Math.round(width.value))) } catch { /* ignore */ }
    }
  }
  function onResizeStart(e: MouseEvent) {
    dragging = true
    startX = e.clientX
    // an auto-width panel has no number to grow from — measure what it is rendering at right now.
    // The handle is always a direct child of the panel, in every consumer.
    startW = width.value ??
      ((e.currentTarget as HTMLElement | null)?.parentElement?.offsetWidth ?? min)
    document.body.style.userSelect = 'none'
    document.body.style.cursor = 'col-resize'
    window.addEventListener('mousemove', onMove)
    window.addEventListener('mouseup', onEnd)
    e.preventDefault()
  }

  // the panel's width style — `undefined` (no width at all) while it is sizing to content
  const widthStyle = computed(() => width.value === null ? undefined : { width: `${width.value}px` })

  onBeforeUnmount(onEnd)   // release listeners if unmounted mid-drag
  return { width, widthStyle, onResizeStart }
}
