// Repaint a canvas on RESIZE without repainting it on every step of the drag.
//
// A canvas whose CSS box is `width/height: 100%` keeps its old bitmap while its box changes: the
// browser STRETCHES it to the new size for free. So a resize step has two honest answers:
//
//  • the last paint was cheap → paint now (a crisp frame costs less than the frame budget);
//  • the last paint was slow (tens of thousands of dots per tile, nine tiles) → let the browser
//    stretch the old bitmap while the size is still changing, and paint ONCE when it settles.
//
// Painting a slow layer on every ResizeObserver step is what made dragging a big population-positions
// panel lag: each step repainted every tile synchronously, so the page could not keep up with the
// pointer. A content change (new data, new extents, dot size) always paints at once — it is not a
// burst, and a stretched old picture would be WRONG, not just soft.
//
// The trailing paint goes through the canonical `debouncedLatest` (docs/UI.md → *Continuous controls*).
// `performance.now` is looked up at call time so a test can stub it.
import { debouncedLatest } from './debouncedLatest'

/** A paint at or under this many ms keeps painting live through a resize. */
export const LIVE_RESIZE_MS = 12
/** The pause after the last resize step before the crisp repaint. */
export const RESIZE_SETTLE_MS = 120

export interface ResizePaint {
  /** Paint now (content changed). Drops any pending settle paint. */
  paint(): void
  /** The box changed: paint now if painting is cheap, else once the resizing stops. */
  resized(): void
  /** Drop a pending settle paint (unmount). */
  cancel(): void
}

export function resizePaint(paint: () => void, liveMs = LIVE_RESIZE_MS, settleMs = RESIZE_SETTLE_MS): ResizePaint {
  let lastMs = 0
  const timed = () => {
    const t0 = globalThis.performance.now()
    paint()
    lastMs = globalThis.performance.now() - t0
  }
  const settle = debouncedLatest<void>(async () => timed(), { wait: settleMs })
  return {
    paint() { settle.cancel(); timed() },
    resized() { if (lastMs <= liveMs) { settle.cancel(); timed() } else settle.schedule() },
    cancel() { settle.cancel() },
  }
}
