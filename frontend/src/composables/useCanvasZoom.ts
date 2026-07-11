import { ref, inject, type Ref, type InjectionKey } from 'vue'

// Generic VISUAL zoom for any plot canvas (the Analysis board's fixed grid AND the free-floating module
// canvases). A CSS `transform: scale` on the content — purely visual, so it never resizes the plots'
// own canvases or changes what's exported (the export re-renders at full logical resolution; hosts
// neutralise the zoom during capture). Fit-to-width/height mirror Word/Illustrator.
//
// Free-floating canvases ALSO inject the zoom into `useFloatingPanel` so drag deltas (screen px) are
// divided by the zoom — otherwise a panel would move `zoom`× too fast under a scaled workspace. The host
// `provide()`s the zoom ref under CANVAS_ZOOM_KEY; CanvasPanel injects it (default 1 when unhosted).
export const CANVAS_ZOOM_KEY: InjectionKey<Ref<number>> = Symbol('canvasZoom')
export function useInjectedZoom(): () => number {
  const z = inject(CANVAS_ZOOM_KEY, null)
  return () => z?.value ?? 1
}

export const ZOOM_MIN = 0.2, ZOOM_MAX = 2
const clampZoom = (z: number) => Math.max(ZOOM_MIN, Math.min(ZOOM_MAX, z))

// `viewport` = the scroll container the board/canvas lives in; `content()` = the board's natural
// (unscaled) size in px (`w` may be null when the content has no fixed width, e.g. a free canvas that
// fills the width — then fitWidth is a no-op).
export function useCanvasZoom(viewport: Ref<HTMLElement | null>, content: () => { w: number | null; h: number }) {
  const zoom = ref(1)
  function fitWidth() {
    const vp = viewport.value, c = content()
    if (!vp || c.w == null || c.w <= 0) return
    zoom.value = clampZoom((vp.clientWidth - 8) / c.w)
  }
  function fitHeight() {
    const vp = viewport.value, c = content()
    if (!vp || c.h <= 0) return
    const availH = window.innerHeight - vp.getBoundingClientRect().top - 16
    zoom.value = clampZoom(availH / c.h)
  }
  function setZoom(z: number) { zoom.value = clampZoom(z) }
  function reset() { zoom.value = 1 }
  // fit width only if the content actually overflows the viewport (used on first render so a big board
  // is visible without hiding the sidebar, but a board that already fits is left at 100%).
  function fitWidthIfOverflow() {
    const vp = viewport.value, c = content()
    if (vp && c.w != null && c.w > vp.clientWidth) fitWidth()
  }
  return { zoom, fitWidth, fitHeight, fitWidthIfOverflow, setZoom, reset }
}
