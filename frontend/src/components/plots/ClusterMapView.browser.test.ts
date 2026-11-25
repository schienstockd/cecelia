// Cluster map in a real browser: resizing a map with many points must not repaint it on every step of
// the drag (utils/resizePaint). Setting `canvas.width` clears and repaints, so the canvas's pixel width
// is the observable: it holds while the drag is on and lands at the final size once it settles. The
// HTML cluster labels must NOT wait for the settle: they follow the box on every step, so they stay on
// the (stretched) dots.
import { afterEach, describe, expect, it, vi } from 'vitest'
import { defineComponent, h, nextTick, provide, reactive } from 'vue'
import { routeLocationKey } from 'vue-router'
import ClusterMapView from './ClusterMapView.vue'
import { frames, mountInto } from '../../test/browser/mount'
import { RESIZE_SETTLE_MS } from '../../utils/resizePaint'
import { activePlotLoads } from '../../utils/plotReady'

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

// Ticking several populations in a row (colour by population) is ONE map fetch of the last pick, not
// one fetch + redraw per tick — each fetch is the whole map per image.
describe('ClusterMapView colour by population', () => {
  it('a burst of population picks fetches the map once, with the final selection', async () => {
    const mapCalls: string[] = []
    vi.stubGlobal('fetch', vi.fn(async (url: string) => {
      const u = String(url)
      if (u.startsWith('/api/plots/cluster-map')) {
        mapCalls.push(new URL(u, location.origin).searchParams.get('colourPops') ?? '')
        return new Response(new Float32Array([0, 0, 0, 0, 1, 1, 1, 1, 1, 2]).buffer)
      }
      return new Response('[]', { headers: { 'Content-Type': 'application/json' } })
    }))
    const state = reactive({ colourBy: 'population' as const, colourPops: [] as string[] })
    mountInto(() => h(WithRoute, null, () => h('div', { style: 'width:500px;height:500px' }, [h(ClusterMapView, {
      projectUid: 'p', imageUids: ['A'], setUid: null, popType: 'clust', suffix: 'default', state })])))
    await wait(400); await frames(2)
    mapCalls.length = 0
    for (const p of ['a', 'b', 'c', 'd', 'e']) {
      state.colourPops = [...state.colourPops, `clust::default/${p}`]
      await nextTick(); await wait(30)
      // a queued refetch already counts as a load, so an export can't capture the old colouring
      expect(activePlotLoads.value).toBeGreaterThan(0)
    }
    await wait(500)
    expect(activePlotLoads.value).toBe(0)
    expect(mapCalls).toHaveLength(1)
    expect(mapCalls[0]!.split(',')).toHaveLength(5)
  })
})

// Colour by a feature the run clustered on: the map asks for that column and shows its range as a colour
// bar instead of a cluster legend.
describe('ClusterMapView colour by feature', () => {
  it('fetches the feature with the map and draws a colour bar over its range', async () => {
    const urls: string[] = []
    vi.stubGlobal('fetch', vi.fn(async (url: string) => {
      const u = String(url)
      if (u.startsWith('/api/plots/cluster-map')) {
        urls.push(u)
        // [x, y, code, popIdx, label, value] × 3 — one point has no value
        return new Response(new Float32Array([0, 0, 0, -1, 1, 2, 1, 1, 1, -1, 2, 8, 2, 0, 0, -1, 3, NaN]).buffer)
      }
      return new Response('[]', { headers: { 'Content-Type': 'application/json' } })
    }))
    const state = reactive({ colourBy: 'feature' as const, colourFeature: 'live.cell.speed' })
    const host = mountInto(() => h(WithRoute, null, () => h('div', { style: 'width:500px;height:500px' }, [h(ClusterMapView, {
      projectUid: 'p', imageUids: ['A'], setUid: null, popType: 'clust', suffix: 'default', state,
      features: ['live.cell.speed'], nameMap: { 'live.cell.speed': 'Speed' } })])))
    await wait(400); await frames(3)
    expect(new URL(urls.at(-1)!, location.origin).searchParams.get('colourFeature')).toBe('live.cell.speed')
    const legend = host.querySelector('.uv-legend')
    expect(legend?.querySelector('svg'), 'colour bar').toBeTruthy()
    expect(legend!.textContent).toContain('Speed')
    expect(legend!.textContent).toContain('2')
    expect(legend!.textContent).toContain('8')
    expect(host.querySelector('.leg-row'), 'no cluster legend rows').toBeNull()
  })
})

// Switching clustering run: the new run's pops arrive at once, its embedding seconds later. Until it
// lands the old map keeps its OWN labels (no new names over the old dots) under a loading wheel.
describe('ClusterMapView run switch', () => {
  it('keeps the old run’s labels under a spinner until the new run’s map arrives', async () => {
    let release: () => void = () => {}
    vi.stubGlobal('fetch', vi.fn(async (url: string) => {
      const u = String(url)
      if (u.startsWith('/api/plots/cluster-map')) {
        if (new URL(u, location.origin).searchParams.get('suffix') === 'b') await new Promise<void>(r => { release = r })
        return new Response(new Float32Array([0, 0, 0, -1, 1, 1, 1, 0, -1, 2, 5, 5, 1, -1, 3]).buffer)
      }
      return new Response('[]', { headers: { 'Content-Type': 'application/json' } })
    }))
    const run = reactive({ suffix: 'a', shownPops: [{ path: 'a/x', name: 'runA', colour: '#ff0000', clusterIds: [0] }] })
    const host = mountInto(() => h(WithRoute, null, () => h('div', { style: 'width:500px;height:500px' }, [h(ClusterMapView, {
      projectUid: 'p', imageUids: ['A'], setUid: null, popType: 'clust', suffix: run.suffix, shownPops: run.shownPops, state: {} })])))
    const legendText = () => host.querySelector('.uv-legend')?.textContent ?? ''
    await wait(400); await frames(2)
    expect(legendText()).toContain('runA')
    run.suffix = 'b'
    run.shownPops = [{ path: 'b/y', name: 'runB', colour: '#00ff00', clusterIds: [0] }]
    await wait(700); await frames(2)
    expect(host.querySelector('.plot-spinner'), 'loading wheel over the old map').toBeTruthy()
    expect(legendText()).toContain('runA')
    expect(legendText()).not.toContain('runB')
    release()
    await wait(100); await frames(2)
    expect(host.querySelector('.plot-spinner')).toBeNull()
    expect(legendText()).toContain('runB')
  })
})
