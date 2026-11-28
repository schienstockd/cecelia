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
        segs: 1, types: 1, isDrawn: () => true, allVisible: true, solo: false, colourBy: '', legend: null,
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
