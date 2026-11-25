// The gate plot's large-cloud path (plots/dotSplat) against what it replaces: the same dots drawn as
// canvas arcs. Expected pixels come from the browser's own arc rasteriser, not from the splat code.
import { describe, it, expect } from 'vitest'
import { splatDots, cssRgb, type Rgb } from './dotSplat'

function canvas(w: number, h: number, dpr: number) {
  const el = document.createElement('canvas')
  el.width = w * dpr; el.height = h * dpr
  const c = el.getContext('2d')!
  c.setTransform(dpr, 0, 0, dpr, 0, 0)
  return c
}
// a seeded cloud, two colours, drawn in order (the second colour over the first where they overlap)
function cloud(n: number, w: number, h: number) {
  let s = 7
  const rnd = () => ((s = (s * 16807) % 2147483647) / 2147483647)
  return Array.from({ length: n }, (_, i) => ({ x: rnd() * w, y: rnd() * h, rgb: (i < n / 2 ? [40, 90, 200] : [230, 120, 20]) as Rgb }))
}

describe('splatDots', () => {
  for (const dpr of [1, 2]) it(`draws the arcs' disks, in their order (dpr ${dpr})`, () => {
    const W = 160, H = 120, r = 1.4
    const pts = cloud(600, W, H)
    const arcs = canvas(W, H, dpr)
    for (const p of pts) {
      arcs.fillStyle = `rgb(${p.rgb.join(',')})`
      arcs.beginPath(); arcs.moveTo(p.x + r, p.y); arcs.arc(p.x, p.y, r, 0, Math.PI * 2); arcs.fill()
    }
    const sp = canvas(W, H, dpr)
    expect(splatDots(sp, W, H, r, dot => { for (const p of pts) dot(p.x, p.y, p.rgb) })).toBe(true)

    const a = arcs.getImageData(0, 0, W * dpr, H * dpr).data, b = sp.getImageData(0, 0, W * dpr, H * dpr).data
    let covA = 0, covB = 0, colourDiff = 0, both = 0
    for (let i = 0; i < a.length; i += 4) {
      covA += a[i + 3]; covB += b[i + 3]
      if (a[i + 3] > 250 && b[i + 3] > 250) {           // a pixel both paint solidly: same colour on top
        both++
        colourDiff += Math.abs(a[i] - b[i]) + Math.abs(a[i + 1] - b[i + 1]) + Math.abs(a[i + 2] - b[i + 2])
      }
    }
    expect(Math.abs(covB - covA) / covA).toBeLessThan(0.05)   // the same ink, to antialiasing
    expect(both).toBeGreaterThan(500)
    expect(colourDiff / both / 3).toBeLessThan(4)              // and the later dot on top, as with arcs
  })

  // translucent dots (the cluster map): each composited at alpha like an arc filled under that globalAlpha,
  // so overlaps build up as they do with arcs — and the canvas's own globalAlpha does not dim the blit
  for (const dpr of [1, 2]) it(`translucent dots match arcs under globalAlpha (dpr ${dpr})`, () => {
    const W = 160, H = 120, r = 1.6, alpha = 0.6
    const pts = cloud(900, W, H)
    const arcs = canvas(W, H, dpr)
    arcs.globalAlpha = alpha
    for (const p of pts) {
      arcs.fillStyle = `rgb(${p.rgb.join(',')})`
      arcs.beginPath(); arcs.arc(p.x, p.y, r, 0, Math.PI * 2); arcs.fill()
    }
    const sp = canvas(W, H, dpr)
    sp.globalAlpha = 0.3
    expect(splatDots(sp, W, H, r, dot => { for (const p of pts) dot(p.x, p.y, p.rgb) }, { alpha })).toBe(true)
    expect(sp.globalAlpha).toBe(0.3)                            // the caller's state is left as it was

    const a = arcs.getImageData(0, 0, W * dpr, H * dpr).data, b = sp.getImageData(0, 0, W * dpr, H * dpr).data
    let covA = 0, covB = 0, alphaDiff = 0, colourDiff = 0, both = 0
    for (let i = 0; i < a.length; i += 4) {
      covA += a[i + 3]; covB += b[i + 3]
      if (a[i + 3] > 140 && b[i + 3] > 140) {                     // inside a dot in both: same colour, same build-up
        both++
        alphaDiff += Math.abs(a[i + 3] - b[i + 3])
        colourDiff += Math.abs(a[i] - b[i]) + Math.abs(a[i + 1] - b[i + 1]) + Math.abs(a[i + 2] - b[i + 2])
      }
    }
    expect(Math.abs(covB - covA) / covA).toBeLessThan(0.05)
    expect(both).toBeGreaterThan(500)
    expect(alphaDiff / both).toBeLessThan(6)
    expect(colourDiff / both / 3).toBeLessThan(6)
  })

  // a layer (the cluster map's facet ghost): drawn once per sub-pixel phase of its origins, and each copy
  // the same pixels as its dots placed directly at that origin
  for (const dpr of [1, 2]) it(`a layer is stamped once and lands where its dots would (dpr ${dpr})`, () => {
    const W = 200, H = 160, r = 1.5, alpha = 0.8
    const pts = cloud(400, 80, 60)
    const origins = [[4, 15], [104, 15], [4, 95], [104, 95]]          // one phase: one stamping
    const direct = canvas(W, H, dpr)
    splatDots(direct, W, H, r, dot => { for (const [ox, oy] of origins) for (const p of pts) dot(ox + p.x, oy + p.y, p.rgb) }, { alpha })
    let draws = 0
    const viaLayer = canvas(W, H, dpr)
    splatDots(viaLayer, W, H, r, (_dot, layer) => {
      const at = layer(80, 60, d => { draws++; for (const p of pts) d(p.x, p.y, p.rgb) })
      for (const [ox, oy] of origins) at(ox, oy)
    }, { alpha })
    expect(draws).toBe(1)
    const a = direct.getImageData(0, 0, W * dpr, H * dpr).data, b = viaLayer.getImageData(0, 0, W * dpr, H * dpr).data
    let diff = 0, ink = 0
    for (let i = 0; i < a.length; i++) { diff = Math.max(diff, Math.abs(a[i] - b[i])); if (i % 4 === 3) ink += a[i] }
    expect(ink).toBeGreaterThan(0)
    expect(diff).toBeLessThanOrEqual(2)                                // the same picture, to 8-bit rounding

    // origins off the pixel grid: a copy per phase, each still where the dots would be (to the phase step)
    draws = 0
    const frac = canvas(W, H, dpr), fracDirect = canvas(W, H, dpr)
    const odd = [[4.5, 15.25], [104.5, 15.25], [4.3, 95.7]]
    splatDots(fracDirect, W, H, r, dot => { for (const [ox, oy] of odd) for (const p of pts) dot(ox + p.x, oy + p.y, p.rgb) }, { alpha })
    splatDots(frac, W, H, r, (_dot, layer) => {
      const at = layer(80, 60, d => { draws++; for (const p of pts) d(p.x, p.y, p.rgb) })
      for (const [ox, oy] of odd) at(ox, oy)
    }, { alpha })
    expect(draws).toBeLessThanOrEqual(odd.length)
    const c = fracDirect.getImageData(0, 0, W * dpr, H * dpr).data, e = frac.getImageData(0, 0, W * dpr, H * dpr).data
    let covC = 0, covE = 0
    for (let i = 3; i < c.length; i += 4) { covC += c[i]; covE += e[i] }
    expect(Math.abs(covE - covC) / covC).toBeLessThan(0.02)
  })

  it('refuses a skewed transform (the caller draws its arcs)', () => {
    const c = canvas(10, 10, 1); c.setTransform(1, 0.2, 0, 1, 0, 0)
    expect(splatDots(c, 10, 10, 1, () => {})).toBe(false)
  })

  it('cssRgb reads a colour the way the canvas parses it', () => {
    const c = canvas(1, 1, 1)
    expect(cssRgb(c, '#ff8000')).toEqual([255, 128, 0])
    expect(cssRgb(c, 'rgb(1, 2, 3)')).toEqual([1, 2, 3])
  })
})
