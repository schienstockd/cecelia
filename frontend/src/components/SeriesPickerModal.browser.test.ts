// SeriesPickerModal in a real browser: hovering a series card shows no tip over the card above —
// the old per-card tip only repeated the card's name and dims, and opened over the row the pointer
// was heading for.
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import { userEvent } from 'vitest/browser'
import SeriesPickerModal from './SeriesPickerModal.vue'
import { frames, mountInto } from '../test/browser/mount'
import type { ProbeResult } from '../utils/seriesPicker'

const probe: ProbeResult = {
  format: 'lif',
  path: '/data/movie.lif',
  series: Array.from({ length: 8 }, (_, i) => ({
    index: i, name: `Position ${i}`, sizeX: 512, sizeY: 512, sizeZ: 20, sizeT: 30, sizeC: 3,
  })),
}

const overlaps = (a: DOMRect, b: DOMRect) =>
  a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom

describe('SeriesPickerModal hover', () => {
  it('a card shows no tip over its neighbours', async () => {
    mountInto({ render: () => h(SeriesPickerModal, { filepath: probe.path, probe }) })
    await frames()
    const cards = [...document.querySelectorAll<HTMLElement>('.sp-card')]
    expect(cards).toHaveLength(8)
    const last = cards[cards.length - 1]
    await userEvent.hover(last)
    await new Promise(r => setTimeout(r, 250))
    await frames()
    const t = document.querySelector<HTMLElement>('.cc-tooltip')
    if (t) for (const c of cards.slice(0, -1)) expect(overlaps(t.getBoundingClientRect(), c.getBoundingClientRect())).toBe(false)
  })
})
