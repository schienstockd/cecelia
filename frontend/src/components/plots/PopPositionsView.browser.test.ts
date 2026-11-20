// Population positions in a real browser: the map fills its panel at the image's own shape. Before
// this, the tile box took the image's aspect but the scatter inside drew its dots as a SQUARE off the
// tile width, plus fixed axis padding — so a portrait image left ~100 px blank under the plot and its
// y axis squashed (x and y µm drawn at different scales). Both are measured here, on the real CSS.
import { afterEach, describe, expect, it, vi } from 'vitest'
import { h } from 'vue'
import PopPositionsView from './PopPositionsView.vue'
import { frames, mountInto } from '../../test/browser/mount'

const rect = (el: Element) => el.getBoundingClientRect()

/** Serve `/api/plots/pop-positions` for one image with these extents (µm). */
function serve(ext: { xMin: number; xMax: number; yMin: number; yMax: number }) {
  const meta = {
    n: 3, unit: 'µm', extents: ext, frames: [0, 0],
    xTicks: [{ pos: ext.xMin, label: String(ext.xMin) }, { pos: ext.xMax, label: String(ext.xMax) }],
    yTicks: [{ pos: ext.yMin, label: String(ext.yMin) }, { pos: ext.yMax, label: String(ext.yMax) }],
  }
  // [x, y, t, popIdx] per cell
  const data = new Float32Array([ext.xMin, ext.yMin, 0, -1, ext.xMax, ext.yMax, 0, -1,
                                 (ext.xMin + ext.xMax) / 2, (ext.yMin + ext.yMax) / 2, 0, -1])
  vi.stubGlobal('fetch', vi.fn(async (url: string) => String(url).includes('meta=1')
    ? new Response(JSON.stringify(meta), { headers: { 'Content-Type': 'application/json' } })
    : new Response(data.buffer)))
}

async function settle(host: HTMLElement) {
  for (let i = 0; i < 40 && !host.querySelector('.ppv-plot'); i++) await new Promise(r => setTimeout(r, 25))
  await frames(4)
}

function mount(w: number, hgt: number, imageUids = ['img1']) {
  return mountInto({ render: () => h('div', { style: `width:${w}px;height:${hgt}px;display:flex;flex-direction:column` },
    [h(PopPositionsView, { projectUid: 'p', imageUids, setUid: null, state: {} })]) })
}

afterEach(() => vi.unstubAllGlobals())

describe('PopPositionsView fit', () => {
  for (const [name, ext] of [
    ['portrait', { xMin: 0, xMax: 5600, yMin: 0, yMax: 6500 }],
    ['landscape', { xMin: 0, xMax: 6500, yMin: 0, yMax: 4000 }],
  ] as const) {
    it(`${name}: the dots keep the image's shape (equal µm scale on x and y)`, async () => {
      serve(ext)
      const host = mount(460, 420)
      await settle(host)
      const dots = rect(host.querySelector('.panel-plot')!)
      expect(dots.width / dots.height).toBeCloseTo((ext.xMax - ext.xMin) / (ext.yMax - ext.yMin), 1)
    })

    it(`${name}: no blank band — the plot reaches the panel along its tight axis`, async () => {
      serve(ext)
      const host = mount(460, 420)
      await settle(host)
      const grid = rect(host.querySelector('.ppv-grid')!)
      const cap = rect(host.querySelector('.plot-capture')!)
      // the capture (dots + axis room) fills the grid on one axis and fits inside it on the other
      const slackW = grid.width - cap.width, slackH = grid.height - cap.height
      expect(Math.min(slackW, slackH)).toBeLessThan(2)
      expect(slackW).toBeGreaterThan(-1)
      expect(slackH).toBeGreaterThan(-1)
      // and the capture is exactly the tile box it was sized for — nothing drawn outside or short of it
      const tile = rect(host.querySelector('.ppv-plot')!)
      expect(cap.height).toBeCloseTo(tile.height, 0)
    })
  }

  it('several images (compact tiles): each map keeps its shape and fills its tile', async () => {
    serve({ xMin: 0, xMax: 5600, yMin: 0, yMax: 6500 })
    const host = mount(700, 420, ['a', 'b'])
    await settle(host)
    const caps = [...host.querySelectorAll('.plot-capture')], tiles = [...host.querySelectorAll('.ppv-plot')]
    expect(caps.length).toBe(2)
    caps.forEach((c, i) => {
      expect(rect(c).height).toBeCloseTo(rect(tiles[i]!).height, 0)
      const d = rect(c.querySelector('.panel-plot')!)
      expect(d.width / d.height).toBeCloseTo(5600 / 6500, 1)
    })
  })
})
