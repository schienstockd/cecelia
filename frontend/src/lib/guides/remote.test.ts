import { describe, it, expect } from 'vitest'
import { remoteStep, viewerStandIn, mainStandIn, parseGuideRemote, parseGuideCmd } from './remote'
import { GUIDES } from './index'

describe('guide remote (main window ↔ viewer window)', () => {
  it('a step crosses as data: no predicates', () => {
    const s = remoteStep({ anchor: 'viewer.share', text: 't', window: 'viewer', clickAnchor: true, when: () => true })
    expect(s).toEqual({ anchor: 'viewer.share', text: 't', window: 'viewer', clickAnchor: true,
                        title: undefined, bullets: undefined, placement: undefined })
    expect(JSON.parse(JSON.stringify(s))).toEqual({ anchor: 'viewer.share', text: 't', window: 'viewer', clickAnchor: true })
  })
  it('each window points at its way to the other', () => {
    expect(viewerStandIn({ text: 'x', title: 'Share' }).anchor).toBe('launcher.viewer')
    expect(mainStandIn({ text: 'x', title: 'Movies' }).anchor).toBe('viewer.back')
  })
  it('reads only well-formed payloads', () => {
    expect(parseGuideRemote('{"title":"a","index":1,"total":3,"phase":"step","gateSatisfied":false,"step":null}')?.index).toBe(1)
    expect(parseGuideRemote('nope')).toBeNull()
    expect(parseGuideRemote('{"title":"a"}')).toBeNull()
    expect(parseGuideCmd('{"cmd":"next","at":1}')?.cmd).toBe('next')
    expect(parseGuideCmd('{"cmd":"rm -rf","at":1}')).toBeNull()
  })
  it('every viewer step anchors on a viewer control, or the overlay', () => {
    for (const g of GUIDES) for (const s of g.steps) {
      if (s.window === 'viewer') expect(s.anchor, `${g.id}: ${s.title}`).toMatch(/^viewer\./)
    }
  })
})
