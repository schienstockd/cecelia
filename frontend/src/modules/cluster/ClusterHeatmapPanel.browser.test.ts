// Cluster heatmap in a real browser: switching clustering run keeps the old heatmap up under a loading
// wheel until the new run's matrix arrives — a big run takes seconds, and nothing said it was reloading.
import { afterEach, describe, expect, it, vi } from 'vitest'
import { h, provide, reactive } from 'vue'
import { routeLocationKey } from 'vue-router'
import ClusterHeatmapPanel from './ClusterHeatmapPanel.vue'
import { frames, mountInto } from '../../test/browser/mount'

const wait = (ms: number) => new Promise(r => setTimeout(r, ms))
const matrix = (cols: string[]) => ({
  measure: 'm', granularity: 'cell', chartType: 'matrix', matrixMode: 'profile', series: [],
  xLabels: cols, yLabels: ['CD3', 'CD4'],
  cells: cols.flatMap((x, i) => [{ x, y: 'CD3', value: i }, { x, y: 'CD4', value: 10 - i }]),
})

afterEach(() => vi.unstubAllGlobals())

describe('ClusterHeatmapPanel run switch', () => {
  it('shows a spinner over the old heatmap until the new run’s matrix lands', async () => {
    let release: () => void = () => {}
    vi.stubGlobal('fetch', vi.fn(async (url: string, init?: RequestInit) => {
      if (String(url).includes('/api/plot_data')) {
        const slow = String(init?.body ?? '').includes('"b"')
        if (slow) await new Promise<void>(r => { release = r })
        return new Response(JSON.stringify(matrix(slow ? ['b0', 'b1'] : ['a0', 'a1', 'a2'])),
          { headers: { 'Content-Type': 'application/json' } })
      }
      return new Response('{}', { headers: { 'Content-Type': 'application/json' } })
    }))
    const run = reactive({ suffix: 'a' })
    const state = reactive({ features: ['CD3', 'CD4'] })
    const host = mountInto({ setup() { provide(routeLocationKey, reactive({ path: '/analysis', query: {}, params: {} }) as never) },
      render: () => h('div', { style: 'width:520px;height:420px;display:flex;flex-direction:column' }, [h(ClusterHeatmapPanel, {
        index: 0, active: true, docked: true, projectUid: 'p', setUid: null, imageUids: ['A'], popType: 'clust',
        suffix: run.suffix, featureOptions: ['CD3', 'CD4'], nameMap: {}, state })]) })
    for (let i = 0; i < 80 && !host.querySelector('.hm-body svg'); i++) await wait(25)
    await frames(2)
    expect(host.querySelector('.hm-body svg'), 'heatmap drawn').toBeTruthy()
    expect(host.querySelector('.hm-empty'), 'no empty state over a drawn heatmap').toBeNull()
    expect(host.querySelector('.plot-spinner')).toBeNull()
    run.suffix = 'b'
    await wait(600); await frames(2)
    expect(host.querySelector('.plot-spinner'), 'loading wheel over the old heatmap').toBeTruthy()
    expect(host.querySelector('.hm-body svg'), 'old heatmap still up').toBeTruthy()
    release()
    await wait(150); await frames(2)
    expect(host.querySelector('.plot-spinner')).toBeNull()
    expect(host.querySelector('.hm-body')!.textContent).toContain('b0')
  })
})
