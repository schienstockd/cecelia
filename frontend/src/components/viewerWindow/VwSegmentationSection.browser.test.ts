// The viewer's Segmentation section in a real browser: at the rail's narrow width the Colour control
// stays inside it. The segmented chips it replaced (Cell id / Populations / Track clusters / Colour by)
// ran past the rail's edge and were clipped.
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import VwSegmentationSection from './VwSegmentationSection.vue'
import { frames, mountInto } from '../../test/browser/mount'

// The rail as it was when the chips clipped (~220 px wide).
const RAIL_PX = 220

describe('VwSegmentationSection Colour', () => {
  it('every colour choice is offered inside a narrow rail', async () => {
    const host = mountInto({
      render: () => h('div', { class: 't-rail', style: `width:${RAIL_PX}px;overflow:hidden` }, [
        h(VwSegmentationSection, {
          maskName: 'Tcells', maskState: '', hidden: false, shownLabelCount: 1, labelName: 'Tcells',
          colourNote: '', truncatedPast: null, singlePlane: true, mode: 'plane', timecourse: true,
          graphShown: false,
        }),
      ]),
    })
    await frames()
    // the section may start collapsed (accordion); open it if its body is hidden
    const body = host.querySelector<HTMLElement>('.cs-body')
    if (body && body.style.display === 'none') { host.querySelector<HTMLElement>('.cs-toggle')!.click(); await frames() }

    const rail = host.querySelector<HTMLElement>('.t-rail')!.getBoundingClientRect()
    const control = host.querySelector<HTMLElement>('[aria-label="Colour mask by"]')!
    const box = control.getBoundingClientRect()
    expect(box.width).toBeGreaterThan(0)
    expect(box.right).toBeLessThanOrEqual(rail.right + 0.5)
    // its own content fits too — nothing scrolled or cut inside it
    expect(control.scrollWidth).toBeLessThanOrEqual(control.clientWidth + 1)
    // all four choices are still there, reachable from the control
    const labels = [...control.querySelectorAll('option, button, .chip')].map(e => e.textContent?.trim())
    expect(labels).toEqual(expect.arrayContaining(['Cell id', 'Populations', 'Track clusters', 'Colour by']))
  })
})
