// Name the ResizeObserver that resized what it observes — because the browser won't.
//
// "ResizeObserver loop completed with undelivered notifications" arrives as a bare `ErrorEvent` with
// no script origin, so `main.ts`'s window-error hook can only log it verbatim (`Script error: …`) and
// the log rail names no culprit. That is the whole difficulty with it: the message says a callback
// resized an observed element during delivery, and nothing says WHICH.
//
// So attribute it at the source instead of reading the message: wrap the constructor, remember where
// each observer was CREATED, and measure its targets across its own callback. A target whose box
// changed while its callback ran IS the loop — reported once per creation site, with the stack, so the
// next occurrence names itself.
//
// Two deliberate limits:
//   * DEV only. The measurement forces a layout read inside the delivery cycle (cheap, but it is real
//     work in the hot path of a drag), and this is a debugging aid, not a feature.
//   * `offsetWidth`/`offsetHeight`, not `getBoundingClientRect`. Integer CSS-pixel box, no transform
//     component — a zoomed canvas panel would otherwise look "resized" on every wheel tick.
//
// It reports honestly on OUR code and on a dependency's alike (`regl-scatterplot` and several PrimeVue
// components observe too), which is the point: knowing whether the loop is ours decides whether the
// answer is a fix or a classification.

/** The observed box, in integer CSS pixels. */
export interface RoBox { w: number; h: number }

/**
 * Did this callback resize what it observes? Any change counts — a guard like CanvasPanel's `>1px`
 * bounds the *loop*, but the browser reports the undelivered notification on the FIRST self-resize,
 * so a 1px settle still shows up in the rail.
 */
export function selfResized(before: RoBox, after: RoBox): boolean {
  return before.w !== after.w || before.h !== after.h
}

/** `div.panel.square#gate-1` — enough to recognise the element without dumping the DOM. */
export function describeTarget(tag: string, id: string, className: string): string {
  const cls = className.trim().split(/\s+/).filter(Boolean).slice(0, 3).map(c => `.${c}`).join('')
  return `${tag.toLowerCase()}${id ? `#${id}` : ''}${cls}`
}

/**
 * The frames of a creation stack that name OUR file, with the wrapper's own frames dropped. Vite
 * serves source paths, so `/src/components/canvas/CanvasPanel.vue:119` survives into the browser and
 * is the line worth showing first; a dependency's frame (`/node_modules/…`) is kept too, because
 * "it isn't ours" is exactly what we need to learn in that case.
 */
export function creationSite(stack: string): string {
  const frames = stack.split('\n')
    .map(l => l.trim())
    .filter(l => /https?:\/\/|\.vue|\.ts|\.js/.test(l))
    .filter(l => !/roLoopTrace/.test(l))          // the wrapper is never the answer
  return frames.slice(0, 4).join('\n') || stack.trim()
}

export function formatRoLoop(site: string, target: string, before: RoBox, after: RoBox): {
  message: string; detail: string
} {
  return {
    message: `ResizeObserver resized what it observes (${target}: ${before.w}x${before.h} → ${after.w}x${after.h})`,
    detail: `The callback wrote layout during delivery, which is what "ResizeObserver loop completed ` +
            `with undelivered notifications" reports. Coalesce the write into a frame (rafCoalesce / ` +
            `usePlotResize) so the resize is delivered as a fresh cycle.\n\nObserver created at:\n${site}`,
  }
}

/**
 * Wrap `globalThis.ResizeObserver` so a self-resizing callback reports itself through `report`.
 * Returns a restore function. Call once, in DEV, before anything constructs an observer — an
 * observer created earlier keeps the native class and is invisible to this.
 */
export function installRoLoopTrace(report: (message: string, detail: string) => void): () => void {
  const Native = globalThis.ResizeObserver
  if (typeof Native === 'undefined') return () => {}

  const seen = new Set<string>()               // one line per creation site per session, not per frame

  class TracedResizeObserver extends Native {
    constructor(callback: ResizeObserverCallback) {
      const site = creationSite(new Error().stack ?? '')
      super((entries, observer) => {
        const targets = entries.map(e => e.target as HTMLElement)
        const before: RoBox[] = targets.map(t => ({ w: t.offsetWidth, h: t.offsetHeight }))
        try {
          callback(entries, observer)
        } finally {
          targets.forEach((t, i) => {
            const after = { w: t.offsetWidth, h: t.offsetHeight }
            if (!selfResized(before[i], after)) return
            const target = describeTarget(t.tagName, t.id, t.className)
            const key = `${site}|${target}`
            if (seen.has(key)) return
            seen.add(key)
            const { message, detail } = formatRoLoop(site, target, before[i], after)
            report(message, detail)
          })
        }
      })
    }
  }

  globalThis.ResizeObserver = TracedResizeObserver as unknown as typeof Native
  return () => { globalThis.ResizeObserver = Native }
}
