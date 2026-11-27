// PlotLayers in a real browser: resizing a plot with many dots must not repaint it on every step of
// the drag. Each step re-sizing the canvas means a full repaint (setting `canvas.width` clears it), so
// the canvas's pixel width is the observable: it stays put while a slow layer is being resized, and
// lands at the final size once the resizing settles (utils/resizePaint.ts).
import { describe, expect, it } from 'vitest'
import { h, reactive } from 'vue'
import PlotLayers from './PlotLayers.vue'
import { frames, mountInto } from '../../test/browser/mount'
import { RESIZE_SETTLE_MS } from '../../utils/resizePaint'

const ext = { xMin: 0, xMax: 100, yMin: 0, yMax: 100 }
function cloud(n: number): Float32Array {
  const a = new Float32Array(2 * n)
  for (let i = 0; i < a.length; i++) a[i] = ((i * 2654435761) % 100000) / 1000   // deterministic spread
  return a
}
const noPops: never[] = []
const wait = (ms: number) => new Promise(r => setTimeout(r, ms))

function mountPlot(n: number) {
  const box = reactive({ w: 300 })
  const points = cloud(n)                 // built once: a resize must not look like new data
  const host = mountInto({
    render: () => h('div', { style: `position:relative;width:${box.w}px;height:300px` }, [
      h(PlotLayers, { viewExtents: ext, renderMode: 'points', basePoints: points, popLayers: noPops,
                      showPops: false, viewTick: 0 }),
    ]),
  })
  return { box, canvas: host.querySelector('canvas')! }
}

async function dragWidth(box: { w: number }, from: number, steps: number) {
  for (let i = 1; i <= steps; i++) { box.w = from + i * 4; await frames(1) }
}

describe('PlotLayers resize', () => {
  it('a slow (many-dot) layer keeps its bitmap through a resize drag and repaints once it settles', async () => {
    const { box, canvas } = mountPlot(100_000)
    await frames()
    const dpr = window.devicePixelRatio || 1
    const before = canvas.width
    expect(before).toBe(Math.round(300 * dpr))
    await dragWidth(box, 300, 10)
    expect(canvas.width).toBe(before)                         // stretched by CSS, not repainted
    await wait(RESIZE_SETTLE_MS + 100); await frames()
    expect(canvas.width).toBe(Math.round(340 * dpr))          // one crisp paint at the final size
  })

  it('a cheap layer still repaints live on every step', async () => {
    const { box, canvas } = mountPlot(50)
    await frames()
    const dpr = window.devicePixelRatio || 1
    await dragWidth(box, 300, 3)
    expect(canvas.width).toBe(Math.round(312 * dpr))
  })
})

// Population positions' "Populations on grey": every cell in the themed dim ink, not the blue-heat
// density ramp, so the population overlays are the only colour — on screen AND in the export.
describe('PlotLayers grey base', () => {
  // the painted pixels' worst channel spread (max - min of r, g, b): ~0 for a grey, large for the ramp
  function spread(c: HTMLCanvasElement): { painted: number; spread: number } {
    const d = c.getContext('2d')!.getImageData(0, 0, c.width, c.height).data
    let painted = 0, worst = 0
    for (let i = 0; i < d.length; i += 4) {
      if (d[i + 3]! < 200) continue                       // skip blank and anti-aliased edges
      painted++
      worst = Math.max(worst, Math.max(d[i]!, d[i + 1]!, d[i + 2]!) - Math.min(d[i]!, d[i + 1]!, d[i + 2]!))
    }
    return { painted, spread: worst }
  }
  function mountBase(baseGrey: boolean) {
    let layers: { exportCanvas: (s: number) => Promise<HTMLCanvasElement | null> } | null = null
    const host = mountInto({
      render: () => h('div', { style: 'position:relative;width:200px;height:200px;--cc-text-dim:#888888' }, [
        h(PlotLayers, { ref: (r: unknown) => { layers = r as typeof layers }, viewExtents: ext, renderMode: 'points',
                        basePoints: cloud(3000), popLayers: noPops, showPops: false, viewTick: 0, baseGrey }),
      ]),
    })
    return { canvas: host.querySelector('canvas')!, exp: () => layers!.exportCanvas(2) }
  }

  it('paints every base dot grey, on screen and in the export', async () => {
    const { canvas, exp } = mountBase(true)
    await frames()
    const live = spread(canvas)
    expect(live.painted).toBeGreaterThan(100)
    expect(live.spread).toBeLessThan(8)
    const out = spread((await exp())!)
    expect(out.painted).toBeGreaterThan(100)
    expect(out.spread).toBeLessThan(8)
  })

  it('without it the base keeps its density colours', async () => {
    const { canvas } = mountBase(false)
    await frames()
    expect(spread(canvas).spread).toBeGreaterThan(40)
  })
})
