/**
 * Dim a whole canvas LAYER — the "backdrop" wash the gating plots put under a population overlay.
 *
 * **Why this is a module and not two lines in the renderer.** `globalAlpha` applies to each drawing
 * OPERATION, not to the layer they add up to. Set it once and then stamp ~10k dots and every place two
 * dots overlap composites twice: coverage is `1-(1-alpha)^k`, so `alpha = 0.4` reaches 0.87 after four
 * overlaps and ~0.95 after six. The wash therefore vanishes precisely where the data is dense — which
 * is where the cells are, and the whole point of dimming. The gating dot plot shipped that bug the day
 * it replaced a single-`drawImage` density raster (which had been immune for free): a viewer cell
 * selection lit up cyan over a base that was supposed to grey out and didn't.
 *
 * So: paint the layer OPAQUE on an offscreen at the host's device scale, then lay it down ONCE at
 * `alpha`. One composite, one coverage, no dependence on how many primitives the layer is made of.
 */

/** Offscreen factory — injectable so the invariant above is testable without a real canvas. */
export type Offscreen = (wDev: number, hDev: number)
  => { ctx: CanvasRenderingContext2D; image: CanvasImageSource } | null

export const domOffscreen: Offscreen = (wDev, hDev) => {
  const el = document.createElement('canvas')
  el.width = wDev; el.height = hDev
  const ctx = el.getContext('2d')
  return ctx ? { ctx, image: el } : null
}

/**
 * Run `paint` as one layer over `w`×`h` CSS px of `c`, composited at `alpha`.
 *
 * `paint` gets the offscreen context and must draw OPAQUE (it is handed a context whose `globalAlpha`
 * this function never touches — that is the bug this exists to prevent). The offscreen carries the
 * host's device scale (`dpr` on screen, the export `scale` in the hi-res/SVG paths) so the dots stay
 * as crisp as a direct paint, and the blit is 1:1 in device pixels.
 *
 * Returns false when there is nothing to draw onto or no 2D offscreen is available — the caller should
 * then paint directly (an undimmed layer beats a blank one).
 */
export function paintDimmed(
  c: CanvasRenderingContext2D, w: number, h: number, alpha: number,
  paint: (target: CanvasRenderingContext2D) => void,
  offscreen: Offscreen = domOffscreen,
): boolean {
  if (!(w > 0) || !(h > 0)) return false
  const m = c.getTransform()
  const o = offscreen(Math.max(1, Math.round(w * m.a)), Math.max(1, Math.round(h * m.d)))
  if (!o) return false
  o.ctx.setTransform(m.a, 0, 0, m.d, 0, 0)
  paint(o.ctx)
  c.save()
  c.globalAlpha = alpha
  c.setTransform(1, 0, 0, 1, 0, 0)      // the offscreen is already in device pixels
  c.drawImage(o.image, 0, 0)
  c.restore()                            // restores globalAlpha AND the transform
  return true
}
