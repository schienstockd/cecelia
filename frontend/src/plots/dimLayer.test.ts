import { describe, it, expect } from 'vitest'
import { paintDimmed, type Offscreen } from './dimLayer'

// A single pixel with real source-over compositing, which is all this invariant is about: after a draw
// of source alpha `s`, coverage becomes `s + cov*(1-s)`. That is the whole reason per-primitive alpha
// cannot dim a layer — it makes `s` per DOT, so overlaps accumulate.
class Px {
  globalAlpha = 1
  cov = 0
  scale: number
  transform: number
  private stack: { a: number; t: number }[] = []
  constructor(scale = 1) { this.scale = scale; this.transform = scale }
  getTransform() { return { a: this.scale, d: this.scale } as DOMMatrix }
  setTransform(a: number) { this.transform = a }
  save() { this.stack.push({ a: this.globalAlpha, t: this.transform }) }
  restore() { const s = this.stack.pop()!; this.globalAlpha = s.a; this.transform = s.t }
  fillRect() { this.cov = this.globalAlpha + this.cov * (1 - this.globalAlpha) }
  drawImage(img: { cov: number }) {
    const s = this.globalAlpha * img.cov
    this.cov = s + this.cov * (1 - s)
  }
}
const asCtx = (p: Px) => p as unknown as CanvasRenderingContext2D

// six overlapping dots on the same pixel — a dense core of a gating dot plot
const sixDots = (c: CanvasRenderingContext2D) => { for (let i = 0; i < 6; i++) c.fillRect(0, 0, 1, 1) }

function offscreenOf(scale: number, made: Px[] = [], dims: [number, number][] = []): Offscreen {
  return (w, h) => { const p = new Px(scale); made.push(p); dims.push([w, h]); return { ctx: asCtx(p), image: p as never } }
}

describe('paintDimmed', () => {
  // THE BUG, stated as a test: this is what the renderer did before this helper existed, and it is
  // why a dimmed base came back to full opacity exactly where the cells were.
  it('per-primitive globalAlpha does NOT dim a layer — overlaps accumulate past it', () => {
    const host = new Px()
    host.globalAlpha = 0.4
    sixDots(asCtx(host))
    expect(host.cov).toBeCloseTo(1 - 0.6 ** 6, 6)      // 0.953, not 0.4
    expect(host.cov).toBeGreaterThan(0.9)
  })

  it('composites the layer ONCE, so coverage is alpha however many primitives overlap', () => {
    const host = new Px()
    expect(paintDimmed(asCtx(host), 100, 80, 0.4, sixDots, offscreenOf(1))).toBe(true)
    expect(host.cov).toBeCloseTo(0.4, 6)
  })

  it('is independent of how dense the layer is', () => {
    const one = new Px(); const many = new Px()
    paintDimmed(asCtx(one), 100, 80, 0.4, c => c.fillRect(0, 0, 1, 1), offscreenOf(1))
    paintDimmed(asCtx(many), 100, 80, 0.4, c => { for (let i = 0; i < 500; i++) c.fillRect(0, 0, 1, 1) }, offscreenOf(1))
    expect(many.cov).toBeCloseTo(one.cov, 6)
  })

  it('hands `paint` an OPAQUE offscreen context, not the host', () => {
    const host = new Px()
    const made: Px[] = []
    let seen: unknown = null
    paintDimmed(asCtx(host), 100, 80, 0.4, c => { seen = c; expect(c.globalAlpha).toBe(1) }, offscreenOf(1, made))
    expect(seen).toBe(made[0])
    expect(seen).not.toBe(host)
  })

  it('sizes the offscreen in DEVICE pixels and matches the host transform', () => {
    const host = new Px(2)                              // dpr 2, or a 2× export
    const dims: [number, number][] = []
    const made: Px[] = []
    paintDimmed(asCtx(host), 100, 80, 0.4, () => {}, offscreenOf(2, made, dims))
    expect(dims[0]).toEqual([200, 160])
    expect(made[0].transform).toBe(2)                   // offscreen paints in the same CSS-px space
  })

  it('restores the host alpha and transform', () => {
    const host = new Px(2)
    host.globalAlpha = 1
    paintDimmed(asCtx(host), 100, 80, 0.4, sixDots, offscreenOf(2))
    expect(host.globalAlpha).toBe(1)
    expect(host.transform).toBe(2)
  })

  it('reports false with no area or no offscreen, so the caller can paint undimmed', () => {
    const host = new Px()
    expect(paintDimmed(asCtx(host), 0, 80, 0.4, sixDots, offscreenOf(1))).toBe(false)
    expect(paintDimmed(asCtx(host), 100, 80, 0.4, sixDots, () => null)).toBe(false)
    expect(host.cov).toBe(0)
  })
})
