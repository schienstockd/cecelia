// The viewer's Tracks section when nothing draws. Tracks whose population was deleted draw under no
// population's ribbon, so with Cell-track ribbons on the section read only "turn tracks on" and gave
// no reason the switch already on drew nothing. It now names where those tracks came from.
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import VwTracksSection from './VwTracksSection.vue'
import { frames, mountInto } from '../../test/browser/mount'

const ORPHAN = { valueName: 'Bcells', path: '/qc/Kat-/cells-', cells: 127851 }
const TURN_ON = 'turn tracks on under On the image'

async function emptySection(props: Record<string, unknown>): Promise<string> {
  const host = mountInto({
    render: () => h('div', { style: 'width:260px' }, [h(VwTracksSection, {
      segCount: 0, pops: [], colorMode: 'track', speedRange: null, rampStyle: {}, sources: [],
      singlePlane: true, hasZ: false, dimUntracked: false, ...props,
    })]),
  })
  await frames()
  const body = host.querySelector<HTMLElement>('.cs-body')
  if (body && body.style.display === 'none') { host.querySelector<HTMLElement>('.cs-toggle')!.click(); await frames() }
  return [...host.querySelectorAll<HTMLElement>('.cc-empty-inline')].map(e => e.innerText.trim()).join('\n')
}

describe('VwTracksSection with nothing drawn', () => {
  it('tracks orphaned: names the deleted population they came from', async () => {
    const text = await emptySection({ orphans: [ORPHAN] })
    expect(text).toContain('Bcells tracks came from /qc/Kat-/cells- — deleted since')
    // their segmentation's tracks button still draws them, so the hint stays
    expect(text).toContain(TURN_ON)
  })
  it('no orphans: the hint alone, as before', async () => {
    const text = await emptySection({ orphans: [] })
    expect(text).toContain(TURN_ON)
    expect(text).not.toContain('deleted since')
  })
})
