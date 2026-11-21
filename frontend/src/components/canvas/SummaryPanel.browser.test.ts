// SummaryPanel's linked-brushing "Select" chip strip in a real browser: on a categorical frequency
// chart (Track cluster frequency) the strip sits BELOW the plot, never over it. It used to be an
// absolute overlay at the bottom of the body, so it covered the rotated x-axis labels.
import { afterEach, describe, expect, it, vi } from 'vitest'
import { h, provide, reactive } from 'vue'
import { routeLocationKey } from 'vue-router'
import SummaryPanel from './SummaryPanel.vue'
import { frames, mountInto } from '../../test/browser/mount'
import spec from '../../../../app/src/plotDefinitions/track_cluster_frequency.json'
import type { PlotSpec } from '../../plots/types'

const CATS = ['fast high-turning', 'fast straight', 'long meandering', 'long-range displacing',
              'slow confined', 'straight short slow']
const MEASURE = 'clusters.behaviour'

function serve() {
  const plot = {
    chartType: 'stacked100', measure: MEASURE, measureType: 'categorical', granularity: 'track',
    categories: CATS,
    series: ['A', 'B', 'C'].map(uID => ({ pop: 'default/tracked', value_name: 'default', uID,
      counts: CATS.map(() => 10), values: CATS.map(() => 1 / CATS.length) })),
  }
  vi.stubGlobal('fetch', vi.fn(async (url: string) => {
    const u = String(url)
    const body = u.includes('/api/gating/channels') ? { columns: [], obsColumns: [], trackObsColumns: [MEASURE] }
      : u.includes('/api/plot_data') ? plot : {}
    return new Response(JSON.stringify(body), { headers: { 'Content-Type': 'application/json' } })
  }))
}

async function settle(host: HTMLElement) {
  for (let i = 0; i < 80 && !host.querySelector('.sp-brush-strip'); i++)
    await new Promise(r => setTimeout(r, 25))
  for (let i = 0; i < 80 && !host.querySelector('.plot-host svg'); i++) await new Promise(r => setTimeout(r, 25))
  await frames(4)
}

afterEach(() => vi.unstubAllGlobals())

describe('SummaryPanel Select strip', () => {
  it('sits below the plot, inside the panel body — never over the x-axis labels', async () => {
    serve()
    const host = mountInto({ setup() { provide(routeLocationKey, reactive({ path: '/analysis', query: {}, params: {} }) as never) },
      render: () => h('div', { style: 'width:560px;height:480px;display:flex;flex-direction:column' },
      [h(SummaryPanel, {
        index: 0, active: true, spec: spec as unknown as PlotSpec, projectUid: 'p', imageUid: 'A',
        series: [{ valueName: 'default', pop: 'default/tracked', popType: 'live' }],
        seriesColor: () => '#888', ui: { chartType: 'stacked100', measure: MEASURE }, docked: true,
      })]) })
    await settle(host)
    const strip = host.querySelector('.sp-brush-strip')
    const plot = host.querySelector('.plot-host')
    const body = host.querySelector('.sp-body')
    expect(strip, 'Select strip rendered').not.toBeNull()
    expect(plot, 'chart rendered').not.toBeNull()
    const s = strip!.getBoundingClientRect(), p = plot!.getBoundingClientRect(), b = body!.getBoundingClientRect()
    expect(s.top).toBeGreaterThanOrEqual(p.bottom - 0.5)      // no overlap with the chart (axis labels included)
    expect(s.bottom).toBeLessThanOrEqual(b.bottom + 0.5)      // and not clipped off the panel body
    expect(p.height).toBeGreaterThan(150)                     // the chart still gets the room left over
  })
})
