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
