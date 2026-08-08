// Coalesce a burst of updates into ONE application per animation frame.
//
// The render-side twin of `debouncedLatest.ts`. Same problem — a slider, a wheel gesture or a
// ResizeObserver fires far faster than the work behind it can keep up, and the effect visibly lags
// the input — but a different right answer, because the work here is pure drawing:
//
//  • `debouncedLatest` is for a REQUEST. It waits out the burst, never runs two at once, and lets a
//    superseded reply be discarded. A wait in milliseconds is the tuning knob.
//  • `rafCoalesce` is for a PAINT. There is no point drawing a value the browser will never show, so
//    the frame is the unit: whatever arrived last before the next frame is drawn, exactly once, and
//    nothing is deferred past a paint. There is no wait to tune and no result to discard.
//
// Extracted from the two hand-rolled copies (`PlotChart`, `useCanvasZoom`) rather than left inline, so
// the timing rules are unit-testable without a browser — the whole point of the rule being enforced is
// that "I coalesced it" should be checkable, not asserted. See docs/UI.md → *Continuous controls*.
//
// Deliberately framework-agnostic (same as `debouncedLatest`/`coalesce`): the caller owns the refs.
// `requestAnimationFrame` is looked up at call time so a test (or a non-DOM environment) can stub it.

export interface RafCoalesced<A> {
  /** Request `apply(arg)` on the next frame. Supersedes anything already pending. */
  schedule(arg: A): void
  /**
   * The argument that WOULD be applied next frame, or `undefined` when nothing is pending.
   *
   * This is what makes successive steps within one frame compound instead of silently cancelling:
   * a zoom step has to be computed from the pending value, not from the last painted one.
   */
  peek(): A | undefined
  /** Apply the pending argument NOW, skipping the frame. No-op when nothing is pending. */
  flush(): Promise<void>
  /** Drop the pending argument without applying it. */
  cancel(): void
}

export function rafCoalesce<A = void>(apply: (arg: A) => void | Promise<void>): RafCoalesced<A> {
  let handle = 0
  // a separate flag, because `undefined`/`null` are legitimate arguments — "is something pending"
  // cannot be inferred from the payload
  let has = false
  let pending: A | undefined

  const clear = () => {
    if (handle) globalThis.cancelAnimationFrame?.(handle)
    handle = 0
  }

  async function run(): Promise<void> {
    handle = 0
    if (!has) return
    const arg = pending as A
    has = false; pending = undefined
    await apply(arg)
  }

  return {
    schedule(arg: A) {
      pending = arg; has = true
      // Do NOT restart the frame on every call — unlike a debounce, a continuous stream must still
      // paint. Re-arming per event would push the frame forever and nothing would ever be drawn.
      if (!handle) handle = globalThis.requestAnimationFrame(() => { void run() })
    },
    peek: () => (has ? pending : undefined),
    async flush() { clear(); await run() },
    cancel() { clear(); has = false; pending = undefined },
  }
}
