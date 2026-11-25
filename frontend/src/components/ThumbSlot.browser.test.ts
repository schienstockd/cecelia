// ThumbSlot — the image table's row thumbnail — measured in a real browser: one box whatever the image's
// aspect or loading state, and a hover preview whose picture is a fixed box before it has decoded.
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import ThumbSlot from './ThumbSlot.vue'
import { previewHtml } from '../utils/imageThumbs'
import { frames, mountInto } from '../test/browser/mount'

/** A solid PNG of `w` × `h` px as a data URL — what `GET /api/images/thumbnail` hands the slot. */
function png(w: number, hgt: number): string {
  const c = document.createElement('canvas')
  c.width = w; c.height = hgt
  const g = c.getContext('2d')!
  g.fillStyle = '#c33'; g.fillRect(0, 0, w, hgt)
  return c.toDataURL('image/png')
}

const box = (el: Element) => { const r = el.getBoundingClientRect(); return [Math.round(r.width), Math.round(r.height)] }

describe('ThumbSlot', () => {
  it('is the same square for portrait, landscape and very wide images, loading and missing', async () => {
    const srcs: Array<string | null> = [png(40, 160), png(160, 40), png(400, 10), png(160, 160), '', null]
    const host = mountInto({
      render: () => h('div', { style: 'display:flex;align-items:flex-start;gap:4px' },
        srcs.map((src, i) => h(ThumbSlot, { src, alt: `img ${i}` }))),
    })
    await Promise.all([...host.querySelectorAll('img')].map(i => i.decode()))
    await frames()
    const boxes = [...host.querySelectorAll('.thumb-slot')].map(box)
    expect(boxes).toHaveLength(srcs.length)
    for (const b of boxes) expect(b).toEqual([80, 80])
    // the image is contained in the slot, never spilling past it
    for (const img of host.querySelectorAll<HTMLImageElement>('img'))
      expect(box(img).every(v => v <= 80)).toBe(true)
  })

  it('the hover preview measures its full size before the image decodes', async () => {
    const tip = { value: previewHtml(png(300, 100), 'Tumour 1', '3 channels'), escape: false, class: 'thumb-tip' }
    const host = mountInto({ render: () => h(ThumbSlot, { src: png(160, 160), alt: 'Tumour 1', tip }) })
    host.querySelector('.thumb-slot')!.dispatchEvent(new MouseEvent('mouseenter'))
    const pic = document.querySelector('.cc-tooltip.thumb-tip .thumb-tip-img')
    expect(pic).not.toBeNull()
    expect(box(pic!)).toEqual([240, 240])
    expect(document.querySelector('.cc-tooltip.thumb-tip')!.textContent).toContain('Tumour 1')
  })
})
