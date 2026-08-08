import { ref, inject, onMounted, onBeforeUnmount, type Ref, type InjectionKey } from 'vue'
import { rafCoalesce } from '../utils/rafCoalesce'

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
  // True while an interactive zoom is in flight (slider drag, wheel, key repeat). The host uses it to
  // promote the scaled subtree to its own compositor layer for the duration — scaling a board of SVG
  // plots otherwise re-rasterises the whole thing on every step, and the range input emits ~36 of them
  // across one drag, so the updates queue and land well after the mouse is released.
  const zooming = ref(false)
  // …and the steps are coalesced to one per animation frame: an input burst should cost one paint, not
  // one per event. Shared primitive (`utils/rafCoalesce`) — the same one PlotChart renders through.
  const frame = rafCoalesce<number>(z => { zoom.value = clampZoom(z) })
  let idleTimer: ReturnType<typeof setTimeout> | null = null
  // read the PENDING value when there is one, so successive steps within a frame compound
  const current = () => frame.peek() ?? zoom.value
  function endSoon() {
    if (idleTimer) clearTimeout(idleTimer)
    // outlive the tail of a drag, then drop the layer — will-change held forever costs memory
    idleTimer = setTimeout(() => { zooming.value = false }, 250)
  }
  // fit/reset SET the zoom outright, so they drop anything the frame still owes — otherwise a fit
  // clicked during the tail of a wheel gesture is overwritten a frame later by the step it interrupted.
  function fitWidth() {
    const vp = viewport.value, c = content()
    if (!vp || c.w == null || c.w <= 0) return
    frame.cancel()
    zoom.value = clampZoom((vp.clientWidth - 8) / c.w)
  }
  function fitHeight() {
    const vp = viewport.value, c = content()
    if (!vp || c.h <= 0) return
    frame.cancel()
    const availH = window.innerHeight - vp.getBoundingClientRect().top - 16
    zoom.value = clampZoom(availH / c.h)
  }
  function setZoom(z: number) {
    frame.schedule(z)
    zooming.value = true
    endSoon()
  }
  function reset() { frame.cancel(); zoom.value = 1 }
  // base the step on the PENDING value, not the applied one — successive steps within one frame must
  // compound, or a fast wheel/key repeat silently loses steps
  const zoomBy = (factor: number) => setZoom(current() * factor)

  // Keyboard/wheel shortcuts (mirrors Illustrator/Figma): shift+wheel over the canvas zooms; shift +/-
  // steps zoom, shift+0 resets. Bound here so every host (board + module canvases) gets them for free.
  // wheel listens on the viewport (passive:false so we can preventDefault the page scroll); keys listen
  // on window but only one canvas host is mounted per route, and we ignore typing in inputs.
  const STEP = 1.1
  const onWheel = (e: WheelEvent) => {
    if (!e.shiftKey) return
    e.preventDefault()
    zoomBy(e.deltaY < 0 ? STEP : 1 / STEP)
  }
  const onKey = (e: KeyboardEvent) => {
    if (!e.shiftKey) return
    const t = e.target as HTMLElement | null
    if (t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.isContentEditable)) return
    if (e.key === '+' || e.key === '=') { e.preventDefault(); zoomBy(STEP) }
    else if (e.key === '-' || e.key === '_') { e.preventDefault(); zoomBy(1 / STEP) }
    else if (e.key === ')' || e.key === '0') { e.preventDefault(); reset() }
  }
  onMounted(() => {
    viewport.value?.addEventListener('wheel', onWheel, { passive: false })
    window.addEventListener('keydown', onKey)
  })
  onBeforeUnmount(() => {
    viewport.value?.removeEventListener('wheel', onWheel)
    window.removeEventListener('keydown', onKey)
    frame.cancel()
    if (idleTimer) clearTimeout(idleTimer)
  })
  // fit width only if the content actually overflows the viewport (used on first render so a big board
  // is visible without hiding the sidebar, but a board that already fits is left at 100%).
  function fitWidthIfOverflow() {
    const vp = viewport.value, c = content()
    if (vp && c.w != null && c.w > vp.clientWidth) fitWidth()
  }
  return { zoom, zooming, fitWidth, fitHeight, fitWidthIfOverflow, setZoom, reset }
}
