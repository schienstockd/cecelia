import { ref, onBeforeUnmount } from 'vue'

// Shared drag-to-resize for TABLE COLUMNS — the per-column twin of `usePanelResize` (which sizes a
// whole panel). The handle sits on a header cell's RIGHT edge, so dragging right widens that column.
//
// Extracted from ImageTable, which was the only table that had it, when the movie list needed the
// same thing: a long filename is unreadable in whatever share of the width an even split gives it,
// and widening the panel doesn't help if every column grows with it.
//
// Widths are per column, keyed by the column key, and persisted under `storageKey` when given — a
// user-settable option must survive remount (docs/UI.md). ImageTable's were a bare `ref({})` and so
// were lost on every navigation; going through here fixes that as a side effect.
export function useColumnResize(opts: {
  /** px width for a column the user hasn't dragged. */
  defaultWidth: (key: string) => number
  min?: number
  storageKey?: string
} ) {
  const min = opts.min ?? 40

  function load(): Record<string, number> {
    if (!opts.storageKey) return {}
    try {
      const raw = localStorage.getItem(opts.storageKey)
      const p = raw ? JSON.parse(raw) : null
      if (!p || typeof p !== 'object') return {}
      // drop anything that isn't a usable number — a hand-edited or older payload must not wedge a
      // column at 0px with no way to drag it back
      return Object.fromEntries(Object.entries(p as Record<string, unknown>)
        .filter(([, v]) => typeof v === 'number' && Number.isFinite(v) && v >= min)) as Record<string, number>
    } catch { return {} }
  }

  const widths = ref<Record<string, number>>(load())

  /** CSS width for `key` — the dragged value, else the caller's default. */
  const widthOf = (key: string): string => `${widths.value[key] ?? opts.defaultWidth(key)}px`

  let drag: { key: string; startX: number; startW: number } | null = null

  function onMove(e: MouseEvent) {
    if (!drag) return
    widths.value = { ...widths.value, [drag.key]: Math.max(min, drag.startW + (e.clientX - drag.startX)) }
  }
  function onEnd() {
    if (!drag) return
    drag = null
    document.removeEventListener('mousemove', onMove)
    document.removeEventListener('mouseup', onEnd)
    document.body.style.userSelect = ''
    document.body.style.cursor = ''
    if (opts.storageKey) {
      try { localStorage.setItem(opts.storageKey, JSON.stringify(widths.value)) } catch { /* ignore */ }
    }
  }
  function onColumnResizeStart(key: string, e: MouseEvent) {
    drag = { key, startX: e.clientX, startW: widths.value[key] ?? opts.defaultWidth(key) }
    document.addEventListener('mousemove', onMove)
    document.addEventListener('mouseup', onEnd)
    document.body.style.userSelect = 'none'
    document.body.style.cursor = 'col-resize'
    e.preventDefault()
  }

  onBeforeUnmount(onEnd)   // release listeners if unmounted mid-drag

  return { widths, widthOf, onColumnResizeStart }
}
