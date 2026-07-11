import { ref, computed, onMounted, onBeforeUnmount, type Ref } from 'vue'

// The free-floating module canvases (summary / gating / cluster) size their logical WORKSPACE to the
// zoom level: at < 100% the workspace grows to `viewport / zoom`, so zooming OUT gives real extra room
// to lay plots across (and Tile spreads into it) instead of just shrinking everything into the
// top-left corner and wasting the rest of the page — the complaint that motivated this. At ≥ 100% the
// workspace stays viewport-sized (zoom-in inspects; it doesn't add room). Purely visual: a CSS
// `transform: scale` shrinks the (larger) workspace back to fit; the plots' own canvases and their
// exports are untouched (the export re-renders at full logical resolution).
//
// The scaled workspace is the panels' offsetParent, so `useFloatingPanel`'s clamp-to-parent lets a
// panel be dragged across the whole enlarged area; `arrangeGrid` (useCanvasPanels) tiles into it too.
export function useCanvasWorkspace(viewport: Ref<HTMLElement | null>, zoom: Ref<number>) {
  const vpW = ref(0), vpH = ref(0)
  let ro: ResizeObserver | null = null
  const measure = () => {
    const el = viewport.value
    if (el) { vpW.value = el.clientWidth; vpH.value = el.clientHeight }
  }
  onMounted(() => {
    measure()
    if (viewport.value && typeof ResizeObserver !== 'undefined') {
      ro = new ResizeObserver(measure); ro.observe(viewport.value)
    }
  })
  onBeforeUnmount(() => { ro?.disconnect(); ro = null })

  // workspace = viewport / min(zoom, 1): grows when zoomed out, stays viewport-sized at ≥ 100%.
  const size = computed(() => {
    const f = Math.min(zoom.value || 1, 1)
    return { w: Math.max(1, (vpW.value || 0) / f), h: Math.max(1, (vpH.value || 0) / f) }
  })
  const workspaceStyle = computed(() => ({
    width: `${size.value.w}px`,
    height: `${size.value.h}px`,
    transform: zoom.value !== 1 ? `scale(${zoom.value})` : undefined,
    transformOrigin: 'top left',
  }))
  return { workspaceStyle, workspaceSize: size }
}
