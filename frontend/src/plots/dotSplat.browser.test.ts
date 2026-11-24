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
