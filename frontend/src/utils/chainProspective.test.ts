import { describe, it, expect } from 'vitest'
import { prospectivePops, singlePopOptions, prospectivePopGroups } from './chainProspective'
import type { ParamDef } from '../tasks/types'

const defs: Record<string, { params: ParamDef[] }> = {
  'tracking.bayesian_track_measures': { params: [
    { key: 'valueName', type: 'valueNameSelection' },
    { key: 'popsToTrack', type: 'popSelection', accepts: ['live'], producesPop: '_tracked' },
  ] as ParamDef[] },
}
const nodes = [
  { fn: 'segment.cellposeMeasure', params: { outputValueName: 'Tcells' } },
  { fn: 'gate.manual_gate', params: { valueName: 'Tcells', requiredPops: '/qc/OTI, /qc/non-OTI/P14' } },
  { fn: 'tracking.bayesian_track_measures', params: { valueName: 'Tcells', popsToTrack: '/qc/OTI' } },
  { fn: 'tracking.bayesian_track_measures', params: { valueName: 'Bcells', popsToTrack: 'NONE' } },
]

describe('prospectivePops', () => {
  it('reads a gate step’s required pops and what tracking derives', () => {
    expect(prospectivePops(nodes, fn => defs[fn])).toEqual([
      { valueName: 'Tcells', path: '/qc/OTI', kind: 'live' },
      { valueName: 'Tcells', path: '/qc/non-OTI/P14', kind: 'live' },
      { valueName: 'Tcells', path: '/qc/OTI/_tracked', kind: 'track' },
      { valueName: 'Bcells', path: '/_tracked', kind: 'track' },
    ])
  })
})

describe('prospectivePops — tracking several pops in one run', () => {
  const run = (popsToTrack: unknown) => prospectivePops(
    [{ fn: 'tracking.bayesian_track_measures', params: { valueName: 'Tcells', popsToTrack } as never }], fn => defs[fn])
  it('derives `_tracked` for each picked ref, on the ref’s own label set', () => {
    expect(run(['Tcells/qc/OTI', 'Tcells/qc/non-OTI/gBT'])).toEqual([
      { valueName: 'Tcells', path: '/qc/OTI/_tracked', kind: 'track' },
      { valueName: 'Tcells', path: '/qc/non-OTI/gBT/_tracked', kind: 'track' },
    ])
  })
  it('reads nothing picked, "NONE" and the all-cells root as the whole segmentation', () => {
    const whole = [{ valueName: 'Tcells', path: '/_tracked', kind: 'track' }]
    expect(run([])).toEqual(whole)
    expect(run('NONE')).toEqual(whole)
    expect(run(['Tcells/'])).toEqual(whole)
  })
})

describe('singlePopOptions', () => {
  const none = [{ label: 'NONE (whole segmentation)', value: 'NONE' }]
  const extra = prospectivePops(nodes, fn => defs[fn])
  it('offers the gates a step will draw on this label set when the image has none', () => {
    expect(singlePopOptions(none, extra, 'Tcells', '/qc/OTI').map(o => o.value))
      .toEqual(['NONE', '/qc/OTI', '/qc/non-OTI/P14'])
    expect(singlePopOptions(none, extra, 'Bcells', null).map(o => o.value)).toEqual(['NONE'])
  })
  it('keeps a saved value visible when nothing offers it', () => {
    const o = singlePopOptions(none, [], 'Tcells', '/qc/OTI')
    expect(o.at(-1)).toEqual({ label: '/qc/OTI · not on this image', value: '/qc/OTI' })
  })
})

describe('prospectivePopGroups', () => {
  const extra = prospectivePops(nodes, fn => defs[fn])
  it('adds what upstream tracking will derive to a track picker, and the saved leftovers', () => {
    const g = prospectivePopGroups([], extra, ['track', 'trackclust'], ['Tcells/qc/OTI/_tracked', 'Tcells/qc/X/_tracked'])
    expect(g).toEqual([
      { title: 'From this chain', opts: [{ label: 'Tcells/qc/OTI/_tracked', value: 'Tcells/qc/OTI/_tracked' },
                                        { label: 'Bcells/_tracked', value: 'Bcells/_tracked' }] },
      { title: 'Saved, not on this image', opts: [{ label: 'Tcells/qc/X/_tracked', value: 'Tcells/qc/X/_tracked' }] },
    ])
  })
  it('does not repeat what the image already offers', () => {
    const on = [{ title: 'Tracks', opts: [{ label: 'Tcells/qc/OTI/_tracked', value: 'Tcells/qc/OTI/_tracked' }] }]
    expect(prospectivePopGroups(on, extra, ['track'], []).flatMap(x => x.opts.map(o => o.value)))
      .toEqual(['Tcells/qc/OTI/_tracked', 'Bcells/_tracked'])
  })
})
