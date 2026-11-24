// SummaryCanvas's global/local scope switch in a real browser: going global → local keeps what every
// plot showed. It used to read each plot's own (usually empty) local state, so every plot dropped its
// populations and settings at once. utils/scopeSwitch.ts.
import { afterEach, describe, expect, it, vi } from 'vitest'
import { h, provide, reactive } from 'vue'
import { routeLocationKey } from 'vue-router'
import SummaryCanvas from './SummaryCanvas.vue'
import { useCanvasPanelsStore } from '../../stores/canvasPanels'
import { defaultVis } from '../../plots/plot'
import { frames, mountInto } from '../../test/browser/mount'
import spec from '../../../../app/src/plotDefinitions/cell_properties.json'
import trackSpec from '../../../../app/src/plotDefinitions/track_measures.json'

const KEY = 'test:scope'
const POP = 'live::default/T cells'

afterEach(() => vi.unstubAllGlobals())

describe('SummaryCanvas scope switch', () => {
  it('global → local: every plot keeps the global populations and look', async () => {
    vi.stubGlobal('fetch', vi.fn(async (url: string) => {
      const u = String(url)
      const body = u.startsWith('/api/plots/definitions') ? [spec] : u.startsWith('/api/plots/populations') ? [] : {}
      return new Response(JSON.stringify(body), { headers: { 'Content-Type': 'application/json' } })
    }))
    let store!: ReturnType<typeof useCanvasPanelsStore>
    const host = mountInto({
      setup() {
        provide(routeLocationKey, reactive({ path: '/analysis', query: {}, params: {} }) as never)
        store = useCanvasPanelsStore()
        const e = store.ensure(KEY)
        const blank = () => ({ specId: 'cell_properties', sel: [] as string[], vis: defaultVis() })
        e.panels = [{ id: 1, arrange: null, state: blank() }, { id: 2, arrange: null, state: blank() }]
        e.nextId = 2; e.activeId = 1
        e.shared = { scope: 'global', sel: [POP], vis: { ...defaultVis(), pointSize: 7 } }
      },
      render: () => h('div', { style: 'width:1000px;height:700px' }, [h(SummaryCanvas, { imageUids: ['A'], canvasKey: KEY })]),
    })
    await frames(4)
    const local = host.querySelector<HTMLElement>('.pi-map-marker')?.closest('button')
    expect(local, 'local scope control rendered').toBeTruthy()
    local!.click()
    await frames(2)
    const e = store.ensure(KEY)
    expect(e.shared.scope).toBe('local')
    for (const p of e.panels) {
      const s = p.state as { sel: string[]; vis: { pointSize?: number } }
      expect(s.sel).toEqual([POP])
      expect(s.vis.pointSize).toBe(7)
    }
    // copies, not the global array itself
    expect((e.panels[0]!.state as { sel: string[] }).sel).not.toBe(e.shared.sel)
  })
})

// "Show series" opened its copies at the Tile grid's cell size (and Tile resized the source too), so a
// plot sized for reading came back as a set of differently shaped plots. Each copy now takes the
// source's size and the set lays out beside it (useCanvasPanels.duplicate).
describe('SummaryCanvas Show series', () => {
  it('every series plot opens at the source plot\'s size, none on top of another', async () => {
    vi.stubGlobal('fetch', vi.fn(async (url: string) => {
      const u = String(url)
      const body = u.startsWith('/api/plots/definitions') ? [trackSpec] : u.startsWith('/api/plots/populations') ? []
        : u.includes('/api/plot_data') ? { chartType: 'boxplot', measure: 'live.track.speed', series: [] } : {}
      return new Response(JSON.stringify(body), { headers: { 'Content-Type': 'application/json' } })
    }))
    const SRC = { x: 24, y: 20, w: 520, h: 390 }
    const host = mountInto({
      setup() {
        provide(routeLocationKey, reactive({ path: '/analysis', query: {}, params: {} }) as never)
        const store = useCanvasPanelsStore()
        const e = store.ensure('test:series')
        e.panels = [{ id: 1, arrange: null, state: { specId: 'track_measures', sel: [], vis: defaultVis(), chartType: 'boxplot' } }]
        e.nextId = 1; e.activeId = 1
        store.setGeom('test:series:1', SRC)
      },
      render: () => h('div', { style: 'width:1600px;height:900px' }, [h(SummaryCanvas, { imageUids: ['A'], canvasKey: 'test:series' })]),
    })
    await frames(6)
    const btn = [...host.querySelectorAll<HTMLButtonElement>('button')].find(b => b.querySelector('.pi-chart-bar'))
    expect(btn, 'Show series button rendered').toBeTruthy()
    btn!.click()
    await frames(4)
    const pop = document.querySelector<HTMLElement>('.sp-explode-pop')!
    const rows = pop.querySelectorAll<HTMLInputElement>('input[type="checkbox"]')
    for (const r of [...rows].filter(r => r.checked).slice(3)) r.click()   // keep three: a 2x2 set with the source
    await frames(2)
    ;[...pop.querySelectorAll('button')].find(b => b.textContent?.startsWith('Show'))!.click()
    await frames(6)
    const panels = [...host.querySelectorAll<HTMLElement>('.panel')]
    expect(panels).toHaveLength(4)
    const boxes = panels.map(p => ({ x: p.offsetLeft, y: p.offsetTop, w: p.offsetWidth, h: p.offsetHeight }))
    for (const b of boxes) expect({ w: b.w, h: b.h }).toEqual({ w: SRC.w, h: SRC.h })
    expect({ x: boxes[0].x, y: boxes[0].y }).toEqual({ x: SRC.x, y: SRC.y })   // the source stays put
    const overlap = (a: typeof SRC, b: typeof SRC) => a.x < b.x + b.w && b.x < a.x + a.w && a.y < b.y + b.h && b.y < a.y + a.h
    for (let i = 0; i < boxes.length; i++)
      for (let j = i + 1; j < boxes.length; j++) expect(overlap(boxes[i], boxes[j]), `${i} vs ${j}`).toBe(false)
  })
})
