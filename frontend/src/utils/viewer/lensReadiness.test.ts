import { describe, it, expect } from 'vitest'
import { LENS_READINESS_LAYERS, lensReadiness, lensReadinessText } from './lensReadiness'

const region = LENS_READINESS_LAYERS.find(l => !l.timed)!
const tracks = LENS_READINESS_LAYERS.find(l => l.timed)!

describe('lensReadiness', () => {
  it('has the regions and the track clusters, from the registry', () => {
    expect(region.fun).toBe('clustRegions.cluster')
    expect(tracks.fun).toBe('clustTracks.cluster')
  })

  it('not drawn: the pops exist, the layer is off', () => {
    const r = lensReadiness({ present: [region.popType], visible: {}, timecourse: false })
    expect(r).toEqual([{ popType: region.popType, label: region.label, state: 'notDrawn', fun: region.fun, action: 'Show' }])
    expect(lensReadinessText(r[0])).toBe(`${region.label} not shown`)
  })

  it('not run: no pops of that type on the image', () => {
    const r = lensReadiness({ present: [], visible: { [region.popType]: true }, timecourse: false,
                              taskLabel: f => (f === region.fun ? 'Cluster regions' : f) })
    expect(r.map(x => [x.state, x.action])).toEqual([['notRun', 'Cluster regions']])
    expect(lensReadinessText(r[0])).toBe(`No ${region.label.toLowerCase()}`)
  })

  it('a drawn layer gets no line; track clusters only on a time-lapse; nothing before the probe answers', () => {
    expect(lensReadiness({ present: [region.popType], visible: { [region.popType]: true }, timecourse: false })).toEqual([])
    expect(lensReadiness({ present: [], visible: {}, timecourse: true }).map(x => x.popType))
      .toEqual(expect.arrayContaining([region.popType, tracks.popType]))
    expect(lensReadiness({ present: [], visible: {}, timecourse: false }).map(x => x.popType)).not.toContain(tracks.popType)
    expect(lensReadiness({ present: null, visible: {}, timecourse: true })).toEqual([])
  })
})
