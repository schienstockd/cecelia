import { onBeforeUnmount } from 'vue'
import { resizeRect, type Edges } from '../utils/panelResize'
import type { PanelBounds, Rect } from '../utils/panelBounds'

// ONE 8-handle resize gesture for every free-floating panel — top-level (`components/FloatingPanel.vue`)
// AND canvas (`canvas/CanvasPanel.vue`, `canvas/CanvasSidePanel.vue`). Owns the pointer loop; the
// arithmetic still lives in `utils/panelResize.ts` (pure, tested). The two wrappers stay split (see
// `docs/inventory/FRONTEND.md:44` — different coordinate systems, different persistence); this closes
// the "eight handles vs the browser's SE grip" duplication without merging them.
//
// The caller supplies:
//   - `getRect`/`setRect` — WHERE the panel's rect lives (reactive state for FloatingPanel; DOM styles
//      + a `pos` ref for CanvasPanel — the composable doesn't care).
//   - `bounds` — floor for N/W edges (viewport-fixed panels use `panelBounds`; canvas panels use 0/0
//     inside the offsetParent).
//   - `viewportSize` — ceiling for E/S edges. Default is the window; canvas panels pass their
//     offsetParent's clientWidth/Height so a panel can't be resized outside the canvas.
//   - `zoom` — for a panel inside a `transform: scale()` parent (the canvas): screen-pixel deltas
//     divide by the zoom, matching the drag path in `useFloatingPanel.ts`. Default 1.
//
// See docs/todo/PANEL_RESIZE_PRIMITIVE_PLAN.md.

export interface ResizeHandlesOpts {
  getRect: () => Rect
  // `edges` says which edges the user grabbed. A caller that snaps one dimension to another (e.g.
  // `CanvasPanel :square` snapping height to width) uses this to skip writing the passive dimension
  // — otherwise the composable would rewrite `h = start.h` every frame and fight the snap.
  setRect: (r: Rect, edges: Edges) => void
  bounds: () => PanelBounds
  min?: { w: number; h: number }
  viewportSize?: () => { w: number; h: number }
  zoom?: () => number
  onActivate?: () => void
}

export function useResizeHandles(opts: ResizeHandlesOpts) {
  const min = opts.min ?? { w: 220, h: 140 }
  let startRect: Rect = { x: 0, y: 0, w: 0, h: 0 }
  let startX = 0
  let startY = 0
  let edges: Edges = {}

  function onMove(e: PointerEvent) {
    const z = opts.zoom?.() ?? 1
    const dx = (e.clientX - startX) / z
    const dy = (e.clientY - startY) / z
    const vp = opts.viewportSize?.() ?? { w: window.innerWidth, h: window.innerHeight }
    opts.setRect(resizeRect(startRect, dx, dy, edges, {
      minW: min.w, minH: min.h,
      viewportW: vp.w, viewportH: vp.h,
      bounds: opts.bounds(),
    }), edges)
  }
  function endGesture() {
    window.removeEventListener('pointermove', onMove)
    window.removeEventListener('pointerup', endGesture)
  }
  function onResizeDown(e: PointerEvent, whichEdges: Edges) {
    opts.onActivate?.()
    edges = whichEdges
    startRect = opts.getRect()
    startX = e.clientX
    startY = e.clientY
    window.addEventListener('pointermove', onMove)
    window.addEventListener('pointerup', endGesture)
    // .capture bubble is already handled by the caller's `raise`; the handle itself must stop the
    // gesture from also starting a drag on the parent header. Preventing default keeps text selection
    // and native drag ghosts out of the way.
    e.preventDefault()
    e.stopPropagation()
  }
  onBeforeUnmount(endGesture)

  return { onResizeDown }
}
