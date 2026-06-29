import { ref, watch, onBeforeUnmount, type Ref } from 'vue'

/** Tile/Cascade command from the workspace; `seq` bumps to force a re-apply. */
export interface ArrangeCmd { x: number; y: number; w: number; h: number; seq: number }

/**
 * Shared drag-to-move + clamp-to-parent behaviour for free-floating canvas panels (the gating
 * plot panels, the population manager, and the upcoming summary / track plot panels). The panel
 * is positioned `absolute` within its `offsetParent`; `pos` is the `{x,y}` the caller binds to
 * `left`/`top`. Dragging is started from a handle via `startDrag` (mousedown). An optional
 * `arrange` getter lets the workspace tile/cascade: when its `seq` changes we set position (state)
 * and size (imperatively, since CSS `resize` otherwise owns the size).
 *
 * Extracted verbatim from the duplicated logic in GatePlotPanel + PopulationManager so every
 * floating panel behaves identically and there is one place to fix the clamp maths.
 */
export function useFloatingPanel(
  el: Ref<HTMLElement | null>,
  opts: {
    initial?: { x: number; y: number }
    margin?: number
    onActivate?: () => void
    arrange?: () => ArrangeCmd | null | undefined
  } = {},
) {
  const margin = opts.margin ?? 24
  const pos = ref({ x: opts.initial?.x ?? 16, y: opts.initial?.y ?? 16 })
  let dragging = false, ox = 0, oy = 0

  function clamp(x: number, y: number) {
    const node = el.value
    const par = node?.offsetParent as HTMLElement | null
    if (!node || !par) return { x, y }
    return {
      x: Math.min(Math.max(x, margin - node.offsetWidth), par.clientWidth - margin),
      y: Math.min(Math.max(y, 0), par.clientHeight - margin),
    }
  }
  function onMove(e: MouseEvent) { if (dragging) pos.value = clamp(e.clientX - ox, e.clientY - oy) }
  function endDrag() {
    dragging = false
    window.removeEventListener('mousemove', onMove)
    window.removeEventListener('mouseup', endDrag)
  }
  function startDrag(e: MouseEvent) {
    opts.onActivate?.()
    dragging = true
    ox = e.clientX - pos.value.x
    oy = e.clientY - pos.value.y
    window.addEventListener('mousemove', onMove)
    window.addEventListener('mouseup', endDrag)
  }
  onBeforeUnmount(endDrag)

  if (opts.arrange) {
    watch(() => opts.arrange!()?.seq, () => {
      const a = opts.arrange!(); const node = el.value
      if (!a || !node) return
      pos.value = { x: a.x, y: a.y }
      node.style.width = a.w + 'px'
      node.style.height = a.h + 'px'
    })
  }

  return { pos, startDrag, endDrag, clamp }
}
