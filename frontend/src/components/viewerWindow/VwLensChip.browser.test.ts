// The lens chip over the viewer canvas: a pinned chip takes its own clicks (unpin, @ Kiwi, Draw and
// note) instead of letting them through to the canvas, where a click re-pins the lens.
import { describe, expect, it } from 'vitest'
import { h, ref } from 'vue'
import { userEvent } from 'vitest/browser'
import VwLensChip from './VwLensChip.vue'
import type { LensChip } from '../../composables/useVwLens'
import { mountInto } from '../../test/browser/mount'

function chip(pinned: boolean): LensChip {
  return { picked: [], pinned, cells: 3, more: 0, groups: [],
           shown: [{ key: 'a', name: 'CD20 B', colour: '#7a3', n: 3, share: '100%' }] }
}

function mountChip(pinned: boolean) {
  const canvasClicks = ref(0), unpins = ref(0), draws = ref(0)
  const host = mountInto({
    render: () => h('div', { style: 'position:relative;width:600px;height:400px' }, [
      h('canvas', { width: 600, height: 400, style: 'position:absolute;inset:0',
                    onPointerup: () => { canvasClicks.value++ } }),
      h(VwLensChip, { chip: chip(pinned), at: { cx: 150, cy: 150 }, radiusPx: 60, canvasW: 600, canvasH: 400,
                      kiwiRef: () => null, onUnpin: () => { unpins.value++ }, onDraw: () => { draws.value++ } }),
    ]),
  })
  return { host, canvasClicks, unpins, draws }
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
})
