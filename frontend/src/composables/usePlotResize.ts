// Re-render a plot when its box changes — without the feedback loop that pattern invites.
//
// THE BUG THIS EXISTS FOR. Five views had written, independently:
//
//   ro = new ResizeObserver(() => render()); ro.observe(host.value)
//
// and `render()` appends an `<svg>` INTO the observed element. Every one of them sizes that svg with a
// floor (`Math.max(200, host.clientWidth)`), so in a panel narrower than the floor the svg is wider
// than its host, the host grows, the observer fires again — and the browser reports
// "ResizeObserver loop completed with undelivered notifications". observed exactly that in the log
// rail. It is not fatal (the browser breaks the loop itself) but it is a real loop: the panel re-renders
// forever at frame rate, and the message is the only symptom.
//
// Two things fix it, and BOTH are needed:
//
//  1. **Coalesce into a frame** (`rafCoalesce`, the canonical scheduler for a paint — docs/UI.md →
//     *Continuous controls*). This moves the DOM write out of the observer's own delivery cycle, which
//     is what the warning is literally about, and collapses a drag's worth of events into one render.
//  2. **Skip a render the size did not ask for.** Coalescing alone still loops — one render per frame,
//     forever — if each render changes the box. Remembering the last-rendered size and returning early
//     when it is unchanged breaks the cycle after one pass.
//
// A caller that needs to draw for another reason (new data, a mode switch) calls `redraw()`, which
// ignores the size guard.

import { onBeforeUnmount, onMounted, type Ref } from 'vue'
import { rafCoalesce } from '../utils/rafCoalesce'

export interface PlotResize {
  /** Draw now, ignoring the size guard — for a data or option change. */
  redraw(): void
  /** Request a draw on the next frame, but only if the box actually changed. */
  schedule(): void
}

/**
 * @param host   the element the plot is appended to (and measured from)
 * @param render draws into `host`; may be async
 */
export function usePlotResize(
  host: Readonly<Ref<HTMLElement | null>>, render: () => void | Promise<void>,
): PlotResize {
  let ro: ResizeObserver | null = null
  let lastW = -1
  let lastH = -1

  const draw = async (force: boolean) => {
    const el = host.value
    if (!el) return
    const w = el.clientWidth
    const h = el.clientHeight
    // the guard: an unchanged box means this render was triggered BY the previous render
    if (!force && w === lastW && h === lastH) return
    lastW = w; lastH = h
    await render()
  }

  const frame = rafCoalesce<boolean>(force => { void draw(force) })

  onMounted(() => {
    if (!host.value || typeof ResizeObserver === 'undefined') return
    // detached = a hidden kept-alive page (see observeBoxChanges); its 0×0 is not a size to render at
    ro = new ResizeObserver(() => { if (host.value?.isConnected) frame.schedule(false) })
    ro.observe(host.value)
  })
  onBeforeUnmount(() => { frame.cancel(); ro?.disconnect(); ro = null })

  return {
    // a data change must draw even though the box is identical, so it resets the remembered size
    redraw() { lastW = -1; lastH = -1; frame.schedule(true) },
    schedule() { frame.schedule(false) },
  }
}

/**
 * A ResizeObserver that reports only a REAL box change of a CONNECTED element — for views that paint
 * straight from the callback (a <canvas> redraw, an rAF-coalesced render) and so aren't on
 * `usePlotResize`. It drops two non-changes, both produced by a module page kept under App.vue's
 * <KeepAlive> (composables/useKeepAlive.ts):
 *  - detached: the hidden page is moved out of the document and the observer reports 0×0. Drawing
 *    then blanks the plot, and persisting a size then saves 0×0;
 *  - unchanged: on return it is re-attached at the size it had, and the observer fires again. The
 *    last paint is still on screen, so a redraw would be pure cost (a board holds up to 36 plots).
 */
export function observeBoxChanges(el: HTMLElement, onChange: () => void): ResizeObserver {
  let w = -1, h = -1
  const ro = new ResizeObserver(() => {
    if (!el.isConnected) return
    if (el.clientWidth === w && el.clientHeight === h) return
    w = el.clientWidth; h = el.clientHeight
    onChange()
  })
  ro.observe(el)
  return ro
}
