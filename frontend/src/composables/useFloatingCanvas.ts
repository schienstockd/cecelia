import { ref, provide, type Ref } from 'vue'
import { useCanvasPanels } from './useCanvasPanels'
import { useCanvasWorkspace } from './useCanvasWorkspace'
import { useCanvasZoom, CANVAS_ZOOM_KEY } from './useCanvasZoom'

/**
 * The wiring every free-floating plot canvas repeats: workspace shell + zoomable panels + a zoom
 * level provided to the panel components inside. Was hand-rolled three times (SummaryCanvas,
 * GatingPlots, ClusterPlots) with the same interlocking calls to `useCanvasPanels`,
 * `useCanvasZoom`, `useCanvasWorkspace` and `provide(CANVAS_ZOOM_KEY, …)`; the identical block was
 * a divergence risk (a fix landing on one host and not the other two — the plot-Share stuck bug
 * that shipped only on SummaryCanvas was that class of drift).
 *
 * Pair with `FloatingCanvasHost.vue` — the component owns the `.floating-canvas / -scroll /
 * -zoom` triple and binds `bindCanvas`/`bindZoom` as callback refs on its inner divs. Callback
 * refs (rather than Ref props) sidestep Vue's template auto-unwrap: a Ref passed as
 * `:canvas-ref="canvasEl"` arrives on the child as the unwrapped `HTMLElement | null`, breaking
 * the ref-forwarding chain.
 *
 * `provide` is called on the CALLER'S component instance (Vue provide reads `getCurrentInstance()`),
 * so the injection reaches the panel components the caller renders inside the slot — as it did
 * when each parent hand-rolled the provide.
 */
export function useFloatingCanvas<S>(
  canvasKey: string | Ref<string> | (() => string),
  makeState: () => S,
  opts: { squareCells?: boolean } = {},
) {
  const canvasEl = ref<HTMLElement | null>(null)
  const zoomEl = ref<HTMLElement | null>(null)
  // Vue's VNodeRef callback signature is `(ref: Element | ComponentPublicInstance | null, …)` — a
  // template `:ref="fn"` binding hands the mounted node (or a component instance for `:ref` on a
  // component). We only bind these on native `<div>`s, so it's always an Element; the wider input
  // type is what typechecks against the framework type.
  type BindTarget = Element | { $el?: unknown } | null
  const bindCanvas = (el: BindTarget) => { canvasEl.value = (el as HTMLElement | null) }
  const bindZoom = (el: BindTarget) => { zoomEl.value = (el as HTMLElement | null) }

  const canvas = useCanvasPanels<S>(zoomEl, makeState, canvasKey,
    // `workspace` is declared BELOW; the getter defers the read until Tile runs, so temporal-dead-zone
    // never bites. Same trick the three hand-rolled hosts already used inline.
    { squareCells: opts.squareCells, tileBox: () => workspace.workspaceBase.value })
  const zoomApi = useCanvasZoom(canvasEl,
    () => ({ w: canvas.contentBounds.value.w || null, h: canvas.contentBounds.value.h }))
  provide(CANVAS_ZOOM_KEY, zoomApi.zoom)
  const workspace = useCanvasWorkspace(canvasEl, zoomApi.zoom, () => canvas.contentBounds.value)

  return {
    bindCanvas, bindZoom,
    workspaceStyle: workspace.workspaceStyle,
    workspaceBase: workspace.workspaceBase,
    // useCanvasPanels
    panels: canvas.panels,
    activeId: canvas.activeId,
    activePanel: canvas.activePanel,
    shared: canvas.shared,
    add: canvas.add,
    remove: canvas.remove,
    removeAll: canvas.removeAll,
    arrangeGrid: canvas.arrangeGrid,
    arrangeCascade: canvas.arrangeCascade,
    contentBounds: canvas.contentBounds,
    // useCanvasZoom
    zoom: zoomApi.zoom,
    fitWidth: zoomApi.fitWidth,
    fitHeight: zoomApi.fitHeight,
    setZoom: zoomApi.setZoom,
    resetZoom: zoomApi.reset,
  }
}
