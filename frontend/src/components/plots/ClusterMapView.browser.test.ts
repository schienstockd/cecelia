// Cluster map in a real browser: resizing a map with many points must not repaint it on every step of
// the drag (utils/resizePaint). Setting `canvas.width` clears and repaints, so the canvas's pixel width
// is the observable: it holds while the drag is on and lands at the final size once it settles. The
// HTML cluster labels must NOT wait for the settle: they follow the box on every step, so they stay on
// the (stretched) dots.
import { afterEach, describe, expect, it, vi } from 'vitest'
import { defineComponent, h, provide, reactive } from 'vue'
import { routeLocationKey } from 'vue-router'
import ClusterMapView from './ClusterMapView.vue'
import { frames, mountInto } from '../../test/browser/mount'
import { RESIZE_SETTLE_MS } from '../../utils/resizePaint'

const wait = (ms: number) => new Promise(r => setTimeout(r, ms))

/** Serve `/api/plots/cluster-map` as n points of [x, y, clusterCode, popIdx, label] in 5 clusters. */
function serve(n: number) {
  const f = new Float32Array(5 * n)
  for (let i = 0; i < n; i++) {
    const c = i % 5
    f.set([c * 10 + ((i * 7919) % 1000) / 200, c * 10 + ((i * 104729) % 1000) / 200, c, -1, i], 5 * i)
  }
  vi.stubGlobal('fetch', vi.fn(async (url: string) => String(url).includes('/api/plots/cluster-map')
    ? new Response(f.buffer.slice(0))
    : new Response('[]', { headers: { 'Content-Type': 'application/json' } })))
}

// the view reads the current route (its plot catalogue entry); a bare location is all it needs
const WithRoute = defineComponent({
  setup(_, { slots }) { provide(routeLocationKey, reactive({ path: '/analysis' }) as never); return () => slots.default?.() },
})

function mountMap() {
  const box = reactive({ w: 300 })
  const state = {}, imageUids = ['img1']
  const host = mountInto({
    render: () => h(WithRoute, () => h('div', { style: `width:${box.w}px;height:${box.w}px;display:flex;flex-direction:column` }, [
      h(ClusterMapView, { projectUid: 'p', imageUids, setUid: null, popType: 'clust', suffix: 'default', state }),
    ])),
  })
  return { box, host }
}

async function loaded(host: HTMLElement) {
  for (let i = 0; i < 80 && !host.querySelector('.uv-label'); i++) await wait(25)
  await frames(4)
  return { canvas: host.querySelector('canvas')!, label: host.querySelector<HTMLElement>('.uv-label')! }
}

afterEach(() => vi.unstubAllGlobals())

describe('ClusterMapView resize', () => {
  it('a many-point map keeps its bitmap through a drag, labels follow every step, one repaint on settle', async () => {
    serve(100_000)
    const { box, host } = mountMap()
    const { canvas, label } = await loaded(host)
    const before = canvas.width, left0 = parseFloat(label.style.left)
    expect(before).toBeGreaterThan(0)
    for (let i = 1; i <= 10; i++) { box.w = 300 + i * 4; await frames(1) }
    await frames(2)
    expect(canvas.width).toBe(before)                                 // stretched by CSS, not repainted
    expect(parseFloat(label.style.left)).toBeGreaterThan(left0)       // the labels moved with the box
    await wait(RESIZE_SETTLE_MS + 100); await frames()
    const dpr = window.devicePixelRatio || 1
    expect(canvas.width).toBe(Math.max(1, canvas.clientWidth * dpr))  // one crisp paint at the final size
    expect(canvas.width).toBeGreaterThan(before)
  })
})
