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
// extends downward instead of the bottom rows spilling out of the box. Only the HEIGHT grows: Tile
// takes its columns from the width and so never overflows it, and a horizontal scrollbar on a plot
// workspace is worse than a narrower grid.
//
// The canvas BOX grows with it (`hostStyle`, a min-height on the host's outer box), so the PAGE
// scrolls to a low panel. The canvas used to scroll inside its own fixed-height box instead, nested
// in a page that scrolls too — a panel past the box's bottom edge read as cut off.
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

/** The placed panels' bounding box (max right / bottom over the geometries whose key starts with
 *  `prefix`). Returns `prev` itself when the box is unchanged: a drag writes a panel's geometry on
 *  every mousemove, and a fresh-but-equal object would re-render the whole canvas host each time
 *  (the workspace style is derived from it) although nothing it shows moved. */
export function contentBox(
  geom: Record<string, { x: number; y: number; w: number; h: number }>, prefix: string,
  prev?: { w: number; h: number },
): { w: number; h: number } {
  let w = 0, h = 0
  for (const [k, g] of Object.entries(geom)) {
    if (!k.startsWith(prefix)) continue
    w = Math.max(w, g.x + g.w); h = Math.max(h, g.y + g.h)
  }
  return prev && prev.w === w && prev.h === h ? prev : { w, h }
}

/** The canvas box's own height without our growth — what `base` is measured from. While the box is
 *  held open by the min-height we applied (`applied`), its measured height is that min-height, not
 *  the layout's: keep the floor measured before it. Measuring the grown box would ratchet — the
 *  workspace never shrinks back, and Tile lays out into the grown height. A layout change while
 *  grown is picked up once the box measures taller than the min-height, or the content shrinks. */
export function floorHeight(measured: number, applied: number, prevFloor: number): number {
  if (applied <= 0 || measured > applied + 1 || prevFloor <= 0) return measured
  return prevFloor
}

/** The min-height (screen px) the canvas box needs to show the whole scaled workspace, or 0 when
 *  its floor already does. */
export function hostMinHeight(workspaceH: number, zoom: number, floor: number): number {
  const scaled = Math.ceil(workspaceH * (zoom || 1))
  return scaled > floor + 1 ? scaled : 0
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
    // a detached viewport (hidden kept-alive page) reads 0×0 — not a size to lay out for
    if (el?.isConnected) { vpW.value = el.clientWidth; vpH.value = floorHeight(el.clientHeight, hostMin.value, vpH.value) }
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
  // the min-height the box is held at — `measure` reads it (a ResizeObserver fires after layout, so
  // this is the value the measured box was laid out with)
  const hostMin = computed(() => hostMinHeight(size.value.h, zoom.value, vpH.value))
  const hostStyle = computed(() => (hostMin.value ? { minHeight: `${hostMin.value}px` } : {}))
  return { workspaceStyle, hostStyle, workspaceSize: size, workspaceBase: computed(() => box.value.base) }
}
