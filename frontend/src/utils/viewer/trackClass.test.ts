import { describe, expect, it } from 'vitest'
import { buildMultiTrackBuffer, SEG_STRIDE, type OverlayPayload } from '../viewerOverlays'
import type { ViewerMeta } from '../volumeViewer'
import { stepRgbFor, TRACK_MIXED_HEX, trackClassesUrl, type TrackClassInfo } from './trackClass'
import palettesJson from '../../plots/palettes.json'

const RED: [number, number, number] = [1, 0, 0]
const GREEN: [number, number, number] = [0, 1, 0]
const GREY = [0x9c / 255, 0xa3 / 255, 0xaf / 255]

// one frags track (id 5) over four frames — cells 1..4 — and a second track (id 6), cells 5..6
const payload = (): OverlayPayload => ({
  nCells: 6, nDropped: 0, axes: ['x', 'y', 'z'], hasT: true,
  cells: { label: [1, 2, 3, 4, 5, 6], t: [0, 1, 2, 3, 0, 1], x: [0, 1, 2, 3, 9, 9], y: [0, 0, 0, 0, 1, 2],
           z: [0, 0, 0, 0, 0, 0], track: [5, 5, 5, 5, 6, 6] },
  pops: [], colourColumns: [], colourBy: null, values: null,
})
const pops = [{ path: '/f/A', labels: [1, 2, 4] }, { path: '/f/B', labels: [3, 4, 5] }]
const info = (over: Partial<TrackClassInfo> = {}): TrackClassInfo => ({
  pop: '/f', threshold: 0.5, level: 1, maxLevel: 1,
  children: [{ path: '/f/A', name: 'A', colour: '#ff0000', tracks: 1 },
             { path: '/f/B', name: 'B', colour: '#00ff00', tracks: 0 }],
  mixed: 1, total: 2, mixedPct: 50, trackIds: [5, 6], classes: [0, -1], ...over,
})

describe('stepRgbFor — sub-population colours per ribbon step', () => {
  it('the mixed grey is the shared palettes.json one', () => {
    expect(TRACK_MIXED_HEX).toBe(palettesJson.trackMixed)
  })
  it('subpop: each row in its first sub-population, grey in none', () => {
    const f = stepRgbFor('subpop', info(), payload(), pops)!
    // labels 1,2 → A; 3 → B; 4 is in both → A (first in children order); 6 in none → grey
    expect([0, 1, 2, 3].map(r => f(r, 5))).toEqual([RED, RED, GREEN, RED])
    expect(f(5, 6).map(v => +v.toFixed(4))).toEqual(GREY.map(v => +v.toFixed(4)))
  })
  it('subpop: a merge (two cells of one track in one frame) takes the first sub-population, as a recording', () => {
    const p = payload()
    // cell 3 (B) and a second cell of track 5 at frame 2, label 7, in A
    p.cells = { label: [...p.cells.label!, 7], t: [...p.cells.t!, 2], x: [...p.cells.x!, 5], y: [...p.cells.y!, 5],
                z: [...p.cells.z!, 0], track: [...p.cells.track!, 5] }
    const f = stepRgbFor('subpop', info(), p, [{ path: '/f/A', labels: [1, 2, 4, 7] }, pops[1]])!
    expect(f(2, 5)).toEqual(RED)
  })
  it('class: a whole track in its class colour, mixed and unknown tracks grey', () => {
    const f = stepRgbFor('class', info(), payload(), pops)!
    expect(f(0, 5)).toEqual(RED)
    expect(f(3, 5)).toEqual(RED)
    expect(f(4, 6).map(v => +v.toFixed(4))).toEqual(GREY.map(v => +v.toFixed(4)))
    expect(f(0, 99).map(v => +v.toFixed(4))).toEqual(GREY.map(v => +v.toFixed(4)))
  })
  it('null where there is nothing to colour by', () => {
    expect(stepRgbFor('pop', info(), payload(), pops)).toBeNull()
    expect(stepRgbFor('class', undefined, payload(), pops)).toBeNull()
    expect(stepRgbFor('class', info({ children: [] }), payload(), pops)).toBeNull()
  })
  it('asks for the per-track call', () => {
    expect(trackClassesUrl({ projectUid: 'p', imageUid: 'i', valueName: 'B', pop: '/qc/frags' }))
      .toBe('/api/gating/track-classes?projectUid=p&imageUid=i&valueName=B&pop=%2Fqc%2Ffrags&tracks=1')
  })
})

describe('buildMultiTrackBuffer — subpop / class ribbons', () => {
  const meta: ViewerMeta = {
    nT: 4, nC: 1, nZ: 1, nX: 10, nY: 10, bytesPerVoxel: 2, slabBytes: 200, contrastSource: 'sampled',
    voxelUm: [1, 1, 1], calibrated: { xy: true, z: true, t: true }, spaceUnit: null, frameIntervalMin: 1,
    channels: [],
  }
  const rgbs = (buf: ReturnType<typeof buildMultiTrackBuffer>) =>
    Array.from({ length: buf.segments.count },
               (_, i) => Array.from(buf.segments.data.slice(i * SEG_STRIDE + 6, i * SEG_STRIDE + 9)))
  it('subpop: each hop in its arriving cell\'s sub-population', () => {
    const p = payload()
    const buf = buildMultiTrackBuffer([{ vn: 'k', payload: p, popColour: '#0000ff',
                                         stepRgb: stepRgbFor('subpop', info(), p, pops) }], meta, ['#ffffff'], 'subpop')
    // track 5 hops arrive at cells 2, 3, 4 (A, B, A); track 6's arrives at cell 6 (none)
    const got = rgbs(buf).map(c => c.map(v => +v.toFixed(3)).join())
    expect(got.filter(c => c === '1,0,0')).toHaveLength(2)
    expect(got.filter(c => c === '0,1,0')).toHaveLength(1)
    expect(buf.sources[0].drawn).toBeNull()
  })
  it('a source with no step colours keeps its population colour', () => {
    const buf = buildMultiTrackBuffer([{ vn: 'k', payload: payload(), popColour: '#0000ff' }], meta, ['#ffffff'], 'class')
    expect(new Set(rgbs(buf).map(c => c.join()))).toEqual(new Set(['0,0,1']))
    expect(buf.sources[0].drawn).toBe('#0000ff')
  })
})
