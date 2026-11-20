// The lens chip over the viewer canvas: a pinned chip takes its own clicks (unpin, @ Kiwi, Draw and
// note) instead of letting them through to the canvas, where a click re-pins the lens.
import { describe, expect, it } from 'vitest'
import { h, ref } from 'vue'
import { userEvent } from 'vitest/browser'
import VwLensChip from './VwLensChip.vue'
import type { LensChip } from '../../composables/useVwLens'
import { mountInto } from '../../test/browser/mount'
import type { LensReadiness } from '../../utils/viewer/lensReadiness'

function chip(pinned: boolean, ready: LensReadiness[] = []): LensChip {
  return { picked: [], pinned, cells: 3, more: 0, groups: [], ready, timed: false, allFrames: false,
           shown: [{ key: 'a', name: 'CD20 B', colour: '#7a3', n: 3, share: '100%' }] }
}

function mountChip(pinned: boolean, ready: LensReadiness[] = []) {
  const canvasClicks = ref(0), unpins = ref(0), draws = ref(0), readied = ref<LensReadiness[]>([])
  const host = mountInto({
    render: () => h('div', { style: 'position:relative;width:600px;height:400px' }, [
      h('canvas', { width: 600, height: 400, style: 'position:absolute;inset:0',
                    onPointerup: () => { canvasClicks.value++ } }),
      h(VwLensChip, { chip: chip(pinned, ready), onReady: (r: LensReadiness) => { readied.value.push(r) }, at: { cx: 150, cy: 150 }, radiusPx: 60, canvasW: 600, canvasH: 400,
                      kiwiRef: () => null, onUnpin: () => { unpins.value++ }, onDraw: () => { draws.value++ } }),
    ]),
  })
  return { host, canvasClicks, unpins, draws, readied }
}

describe('VwLensChip over the canvas', () => {
  it('pinned: the unpin and draw buttons take the click; the canvas never sees it', async () => {
    const { host, canvasClicks, unpins, draws } = mountChip(true)
    // the window's copy of vwReadout.css also reaches the chip's root (a child root carries its parent's
    // scope) and loads after the chip's styles — stand in for it with the same rule, added last
    const root = host.querySelector<HTMLElement>('.vw-readout')!
    const scope = root.getAttributeNames().find(a => a.startsWith('data-v-'))!   // same weight as a scoped rule
    const parentRule = document.createElement('style')
    parentRule.textContent = `.vw-readout[${scope}] { pointer-events: none; }`
    document.head.appendChild(parentRule)
    const unpin = host.querySelector<HTMLElement>('[aria-label="Unpin the lens"]')!
    const r = unpin.getBoundingClientRect()
    expect(document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2)?.closest('button')).toBe(unpin)
    await userEvent.click(unpin)
    await userEvent.click(host.querySelector<HTMLElement>('[aria-label="Draw and note on the lens"]')!)
    expect(unpins.value).toBe(1)
    expect(draws.value).toBe(1)
    expect(canvasClicks.value).toBe(0)
    parentRule.remove()
  })

  it('readiness: one dim line per missing layer — Show for a layer that is off, the task for one never run', async () => {
    const off: LensReadiness = { popType: 'r', label: 'Regions', state: 'notDrawn', fun: 'clustRegions.cluster', action: 'Show' }
    const none: LensReadiness = { popType: 't', label: 'Track clusters', state: 'notRun', fun: 'clustTracks.cluster', action: 'Cluster tracks' }
    const { host, readied, canvasClicks } = mountChip(true, [off, none])
    const lines = [...host.querySelectorAll<HTMLElement>('.vw-lens-ready')]
    expect(lines.map(l => l.textContent!.replace(/\s+/g, ' ').trim()))
      .toEqual(['Regions not shown · Show', 'No track clusters · Cluster tracks'])
    for (const b of host.querySelectorAll<HTMLElement>('.vw-lens-ready-act')) await userEvent.click(b)
    expect(readied.value.map(r => r.state)).toEqual(['notDrawn', 'notRun'])
    expect(canvasClicks.value).toBe(0)
    // unpinned, the line says why but takes no click (the chip follows the pointer)
    const loose = mountChip(false, [off])
    expect(loose.host.querySelector('.vw-lens-ready')!.textContent).toContain('Regions not shown')
    expect(loose.host.querySelector('.vw-lens-ready-act')).toBeNull()
  })

  it('a pinned time-lapse lens has the all-frames switch in its header, with no cell layer drawn', async () => {
    const frames = ref<boolean[]>([])
    const c = { ...chip(true), timed: true, cells: null, shown: [] }
    const host = mountInto({ render: () => h(VwLensChip, { chip: c, at: { cx: 150, cy: 150 }, radiusPx: 60, canvasW: 600, canvasH: 400,
                                                           kiwiRef: () => null, onAllFrames: (v: boolean) => { frames.value.push(v) } }) })
    const b = host.querySelector<HTMLElement>('[aria-label="Count all frames"]')!
    expect(b.textContent!.trim()).toBe('This frame')
    await userEvent.click(b)
    expect(frames.value).toEqual([true])
  })
})
