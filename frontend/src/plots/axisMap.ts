/**
 * Data ↔ pixel mapping for the gating canvas — ONE implementation, forward and inverse together.
 *
 * **Why it is one module.** The same six lines were written out in four files: `PlotLayers` (points,
 * density, contour rings), `GateOverlay` (gate outlines, and the INVERSE that turns a dragged
 * rectangle into stored gate coordinates), `GateScatterCell` (axis ticks, and the export capture's own
 * redraw). Four copies of a mapping is survivable while the mapping never changes. It stops being
 * survivable the moment it must — which is now.
 *
 * **Y GROWS DOWNWARD IN AN IMAGE.** `centroid_y` is a row index: 0 is the top of the frame. That is
 * what viewer draws and what every pixel coordinate in the pipeline means. A plot that puts y
 * increasing upward — the default for a chart, and correct for an intensity — is a MIRROR of the
 * image whenever the axis is a position. A cell drifting down-screen appears to drift up, and
 * comparing the plot against the viewer means comparing a shape with its reflection without being
 * told.
 *
 * **The forward and inverse MUST flip together.** `pxToData` is how a drawn gate becomes the numbers
 * written to `gating/{value_name}.json`. Flip the drawing without flipping the inverse and every new
 * position gate is stored mirrored — a wrong gate on disk, applied to every future image, with
 * nothing on screen looking wrong at the moment it happens. That is the failure this module exists to
 * make impossible: both directions come from one `flipY`, and `axisMap.test.ts` pins the round trip.
 */

/** The visible data window. */
export interface Ext { xMin: number; xMax: number; yMin: number; yMax: number }

/** The pixel box, and which way y runs. */
export interface PxBox {
  w: number
  h: number
  /** true when the y axis is an IMAGE coordinate, so screen y increases with data y */
  flipY?: boolean
  /** origin offset, for a plot area inset within a larger canvas (the export capture) */
  x0?: number
  y0?: number
}

// a zero or inverted span would divide by zero and put every point in one place; 1 keeps the picture
// degenerate-but-drawn rather than NaN
const span = (lo: number, hi: number) => (hi > lo ? hi - lo : 1)

/** Fraction of the y axis, 0 at the TOP of the box and 1 at the bottom — the shared primitive. */
export function yFrac(e: Ext, v: number, flipY = false): number {
  const f = (v - e.yMin) / span(e.yMin, e.yMax)
  return flipY ? f : 1 - f
}

/** Fraction of the x axis, 0 at the left. x is never flipped — image columns already run left→right. */
export function xFrac(e: Ext, v: number): number {
  return (v - e.xMin) / span(e.xMin, e.xMax)
}

/** Data → pixel, within the box. */
export function dataToPx(e: Ext, b: PxBox, vx: number, vy: number): [number, number] {
  return [(b.x0 ?? 0) + xFrac(e, vx) * b.w,
          (b.y0 ?? 0) + yFrac(e, vy, b.flipY) * b.h]
}

/** Pixel → data. The exact inverse of `dataToPx`, including the flip. */
export function pxToData(e: Ext, b: PxBox, px: number, py: number): [number, number] {
  const fx = (px - (b.x0 ?? 0)) / (b.w || 1)
  const fy = (py - (b.y0 ?? 0)) / (b.h || 1)
  return [e.xMin + fx * span(e.xMin, e.xMax),
          e.yMin + (b.flipY ? fy : 1 - fy) * span(e.yMin, e.yMax)]
}

/**
 * A d3-contour ring coordinate (grid space `[0, G]`, col = x, row = y) → pixel.
 *
 * The grid is binned in DATA order (`density.ts` bins row 0 at `yMin`), so it converts back to data
 * first and then through the one mapping — rather than carrying a second orientation rule that could
 * disagree with the points drawn over it.
 */
export function gridToPx(e: Ext, b: PxBox, g: number, gx: number, gy: number): [number, number] {
  return dataToPx(e, b,
    e.xMin + (gx / g) * span(e.xMin, e.xMax),
    e.yMin + (gy / g) * span(e.yMin, e.yMax))
}
