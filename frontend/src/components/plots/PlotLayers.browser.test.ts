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
