// Show "working" BEFORE a synchronous redraw that is known to be slow.
//
// Some plots are redrawn entirely in the browser — Rotate 90° on a heatmap, a palette, a font size —
// with no request, so there is no loading flag for `useDelayedLoading` to watch. On a big set the
// rebuild (Observable Plot building thousands of SVG cells) blocks the page for a second or more, and
// because it is synchronous a spinner switched on INSIDE it never paints: the control looks like it
// did nothing until the new chart appears.
//
// So we learn from the previous draw. When the last one took `slowMs` or longer, the next one first
// raises `onBusy(true)` and waits two animation frames (the first commits the style change, the second
// guarantees it was painted) before doing the work. Fast plots never cross the threshold and never see
// the wheel — the same "no flash on small plots" rule as `useDelayedLoading` (docs/UI.md → *Plot
// loading state*). The wheel's CSS rotation runs on the compositor, so it keeps turning while the main
// thread is busy drawing.
//
// A draw requested while an earlier one is waiting for its frames supersedes it — only the latest is
// drawn, and the busy flag stays up until that one is done.
//
// Framework-agnostic like `rafCoalesce`: `requestAnimationFrame` and `performance.now` are looked up
// at call time so a test can stub them.

export const SLOW_PAINT_MS = 200

export interface SlowPaint {
  /** Run `draw`, announcing it first when the previous run was slow. Resolves when drawn (or superseded). */
  run(): Promise<void>
  /** Duration of the last completed draw, ms (0 before the first). */
  lastMs(): number
}

const nextFrame = () => new Promise<void>(r => globalThis.requestAnimationFrame(() => r()))

export function slowPaint(
  draw: () => void | Promise<void>,
  onBusy: (busy: boolean) => void,
  slowMs = SLOW_PAINT_MS,
): SlowPaint {
  let last = 0
  let seq = 0
  let busy = false
  const setBusy = (b: boolean) => { if (b !== busy) { busy = b; onBusy(b) } }

  return {
    async run() {
      const my = ++seq
      if (last >= slowMs) {
        setBusy(true)
        await nextFrame(); await nextFrame()
        if (my !== seq) return                   // a newer request will draw — and clear the flag
      }
      const t0 = globalThis.performance.now()
      try {
        await draw()
      } finally {
        last = globalThis.performance.now() - t0
        if (my === seq) setBusy(false)
      }
    },
    lastMs: () => last,
  }
}
