import { ref, computed, watch, onMounted, onBeforeUnmount, type Ref } from 'vue'

// The free-floating module canvases (summary / gating / cluster) size their logical WORKSPACE to the
// zoom level: at < 100% the workspace grows to `viewport / zoom`, so zooming OUT gives real extra room
// to lay plots across (and Tile spreads into it) instead of just shrinking everything into the
// top-left corner and wasting the rest of the page — the complaint that motivated this. At ≥ 100% the
// workspace stays viewport-sized (zoom-in inspects; it doesn't add room). Purely visual: a CSS
// `transform: scale` shrinks the (larger) workspace back to fit; the plots' own canvases and their
// exports are untouched (the export re-renders at full logical resolution).
//
// It also GROWS TALLER with its content: the workspace is at least as tall as the lowest panel, so a
// grid needing more rows than fit (Tile stops shrinking at a readable floor — utils/tileGrid.ts)
// extends downward and the canvas scrolls, instead of the bottom rows spilling out of the box. Only
// the HEIGHT grows: Tile takes its columns from the width and so never overflows it, and a horizontal
// scrollbar on a plot workspace is worse than a narrower grid.
//
// The scaled workspace is the panels' offsetParent, so `useFloatingPanel`'s clamp-to-parent lets a
// panel be dragged across the whole enlarged area — including the grown part, which is what makes the
// extra room reachable by hand and not only by Tile.
//
// `base` vs grown matters to ONE caller: Tile sizes its grid from `workspaceBase` (the viewport), never
// from the grown size. Feeding a content-derived height back into the layout that produced it is how
// you get a Tile that lays out differently the second time you press it.

/** Bottom breathing room under the lowest panel, so a grown workspace doesn't end flush at its edge. */
const GROW_PAD = 16

/** The logical workspace: `viewport / min(zoom, 1)`, then extended to hold `content`. Pure — the
 *  composable below is just the reactive wrapper. */
export function workspaceBox(
  vpW: number, vpH: number, zoom: number, content?: { w: number; h: number } | null,
): { base: { w: number; h: number }; size: { w: number; h: number } } {
  const f = Math.min(zoom || 1, 1)
  const base = { w: Math.max(1, (vpW || 0) / f), h: Math.max(1, (vpH || 0) / f) }
  const grown = content && content.h > 0 ? Math.max(base.h, content.h + GROW_PAD) : base.h
  return { base, size: { w: base.w, h: grown } }
}

export function useCanvasWorkspace(
  viewport: Ref<HTMLElement | null>, zoom: Ref<number>,
  // the placed panels' bounding box (`useCanvasPanels.contentBounds`). A GETTER, read lazily during
  // render, so a host may pass it before the panels composable is declared.
  content?: () => { w: number; h: number },
) {
  const vpW = ref(0), vpH = ref(0)
  let ro: ResizeObserver | null = null
  const measure = () => {
    const el = viewport.value
    if (el) { vpW.value = el.clientWidth; vpH.value = el.clientHeight }
  }
  // (Re-)attach the observer to whatever element `viewport` currently points at. Hosts often mount
  // the workspace inside a `v-else` that only renders once an image is selected — the ref is still
  // `null` at `onMounted`, so a one-shot observation there would silently never fire. Tile then reads
  // vpW=0 and collapses to a single column on a workspace that has plenty of room — the report on a
  // wide canvas. Rebinding whenever the ref changes closes that hole (and covers a later swap too).
  const attach = () => {
    ro?.disconnect(); ro = null
    measure()
    const el = viewport.value
    if (el && typeof ResizeObserver !== 'undefined') {
      ro = new ResizeObserver(measure); ro.observe(el)
    }
  }
  onMounted(attach)
  watch(viewport, attach)
  onBeforeUnmount(() => { ro?.disconnect(); ro = null })

  const box = computed(() => workspaceBox(vpW.value, vpH.value, zoom.value, content?.()))
  const size = computed(() => box.value.size)
  const workspaceStyle = computed(() => ({
    width: `${size.value.w}px`,
    height: `${size.value.h}px`,
    transform: zoom.value !== 1 ? `scale(${zoom.value})` : undefined,
    transformOrigin: 'top left',
  }))
  return { workspaceStyle, workspaceSize: size, workspaceBase: computed(() => box.value.base) }
}
