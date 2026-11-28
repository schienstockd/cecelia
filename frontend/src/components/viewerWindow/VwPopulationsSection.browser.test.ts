// The viewer's Populations section owns the population point size (the population picker no longer
// carries a copy). Its slider spans the Preferences range (1–30 image px), and a change
// reports out — the window writes it to the same per-set setting (`settings.setPointSize`).
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import VwPopulationsSection from './VwPopulationsSection.vue'
import { frames, mountInto } from '../../test/browser/mount'

describe('VwPopulationsSection point size', () => {
  it('slider spans 1–30 px and emits the new size', async () => {
    const sizes: number[] = []
    const host = mountInto({
      render: () => h('div', { style: 'width:260px' }, [h(VwPopulationsSection, {
        panelOn: true, summary: { cells: 10, pops: 1, dropped: 0 }, error: '',
        layers: [{ vn: 'default', popType: 'region',
                   payload: { pops: [{ path: '/r1', name: 'r1', colour: '#f00', labels: [1, 2] }] } as never }],
        runs: new Map(), segs: 1, types: 1, isDrawn: () => true, allVisible: true, solo: false, colourBy: '', legend: null,
        rampStyle: {}, singlePlane: true, hasZ: false, pointSize: 1, pointBorder: 0, pointCount: 2,
        mode: 'plane', zLoaded: [0, 0],
        'onUpdate:pointSize': (v: number) => sizes.push(v),
      })]),
    })
    await frames()
    const body = host.querySelector<HTMLElement>('.cs-body')
    if (body && body.style.display === 'none') { host.querySelector<HTMLElement>('.cs-toggle')!.click(); await frames() }
    const row = [...host.querySelectorAll<HTMLElement>('.cc-row')].find(r => /Point size/.test(r.textContent ?? ''))!
    const slider = row.querySelector<HTMLInputElement>('input[type=range]')!
    expect([slider.min, slider.max]).toEqual(['1', '30'])
    expect(slider.value).toBe('1')
    slider.value = '9'; slider.dispatchEvent(new Event('input'))
    expect(sizes).toEqual([9])
  })
})

// Two clustering runs on one segmentation: the layer gets one chip per run, lit for the runs listed,
// and a click reports the new set out — the window stores it and hides that run's pops.
describe('VwPopulationsSection run chips', () => {
  const mount = (runs: Map<string, { all: string[]; shown: string[] }>, picked: string[][]) => mountInto({
    render: () => h('div', { style: 'width:260px' }, [h(VwPopulationsSection, {
      panelOn: true, summary: { cells: 10, pops: 2, dropped: 0 }, error: '',
      layers: [{ vn: 'cells', popType: 'clust',
                 payload: { pops: [{ path: '/Epithelial', name: 'Epithelial', colour: '#f00', labels: [1], run: 'tissues' }] } as never }],
      runs, segs: 1, types: 1, isDrawn: () => true, allVisible: true, solo: false, colourBy: '', legend: null,
      rampStyle: {}, singlePlane: true, hasZ: false, pointSize: 1, pointBorder: 0, pointCount: 1,
      mode: 'plane', zLoaded: [0, 0],
      onSetRuns: (_l: unknown, v: string[]) => picked.push(v),
    })]),
  })
  const open = async (host: HTMLElement) => {
    await frames()
    const body = host.querySelector<HTMLElement>('.cs-body')
    if (body && body.style.display === 'none') { host.querySelector<HTMLElement>('.cs-toggle')!.click(); await frames() }
  }
  const runRow = (host: HTMLElement) => [...host.querySelectorAll<HTMLElement>('.cc-row')].find(r => /Runs/.test(r.textContent ?? ''))

  it('offers a chip per run and reports the runs left on', async () => {
    const picked: string[][] = []
    const host = mount(new Map([['cells::clust', { all: ['tissues', 'tissues_af'], shown: ['tissues'] }]]), picked)
    await open(host)
    const row = runRow(host)!
    const chips = [...row.querySelectorAll<HTMLElement>('button')].filter(b => /tissues/.test(b.textContent ?? ''))
    expect(chips.map(c => c.textContent?.trim())).toEqual(['tissues', 'tissues_af'])
    chips[1]!.click()
    await frames()
    expect(picked).toEqual([['tissues', 'tissues_af']])
  })

  it('has no run row for a single run', async () => {
    const host = mount(new Map([['cells::clust', { all: ['tissues'], shown: ['tissues'] }]]), [])
    await open(host)
    expect(runRow(host)).toBeUndefined()
  })
})
