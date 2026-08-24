import { describe, it, expect } from 'vitest'
import {
  overlaysUrl, buildPointBuffer, timepointRange, hexToUnit, overlaySummary,
  POINT_STRIDE, type OverlayPayload,
} from './viewerOverlays'
import type { ViewerMeta } from './volumeViewer'

const meta = (over: Partial<ViewerMeta> = {}): ViewerMeta => ({
  nT: 3, nC: 1, nZ: 4, nX: 10, nY: 8, bytesPerVoxel: 2, slabBytes: 10 * 8 * 4 * 2,
  contrastSource: 'sampled', voxelUm: [0.5, 0.5, 2],
  calibrated: { xy: true, z: true, t: true }, spaceUnit: null, frameIntervalMin: 1,
  channels: [{ name: 'a', lo: 0, hi: 10, visible: true, lut: [[0, 0, 0], [1, 0, 0]] }],
  ...over,
})

/** Four cells: two at t=0, two at t=1, on z planes 0 and 2 (vz = 2 µm → z = 0 and 4). */
const payload = (over: Partial<OverlayPayload> = {}): OverlayPayload => ({
  nCells: 4,
  nDropped: 0,
  axes: ['x', 'y', 'z'],
  hasT: true,
  cells: {
    label: [10, 11, 12, 13],
    t: [0, 1, 0, 1],
    x: [1, 2, 3, 4],
    y: [5, 6, 7, 8],
    z: [0, 4, 4, 0],
    track: [1, 1, -1, 2],
  },
  pops: [
    { path: '/A', name: 'A', colour: '#ff0000', show: true, isTrack: false, labels: [10, 11] },
    { path: '/B', name: 'B', colour: '#00ff00', show: true, isTrack: false, labels: [12, 13] },
  ],
  colourColumns: ['track_id'],
  colourBy: null,
  values: null,
  ...over,
})

const at = (buf: { data: Float32Array }, i: number) =>
  Array.from(buf.data.slice(i * POINT_STRIDE, (i + 1) * POINT_STRIDE))

describe('overlaysUrl', () => {
  it('omits what was not asked for, so the server picks its own defaults', () => {
    expect(overlaysUrl({ projectUid: 'p', imageUid: 'i' }))
      .toBe('/api/viewer/overlays?projectUid=p&imageUid=i')
  })
  it('carries the value name, pop type and colour column when given', () => {
    const u = overlaysUrl({
      projectUid: 'p', imageUid: 'i', valueName: 'memTom', popType: 'flow', colourBy: 'live.cell.speed',
    })
    expect(u).toContain('valueName=memTom')
    expect(u).toContain('popType=flow')
    expect(u).toContain('colourBy=live.cell.speed')
  })
})

describe('buildPointBuffer', () => {
  it('orders instances BY TIMEPOINT, so one draw covers a whole frame', () => {
    // The design, not a detail: a contiguous range per timepoint means no per-frame filtering, no
    // per-frame allocation, and no upload when the timepoint changes.
    const buf = buildPointBuffer(payload(), meta())
    expect(buf.count).toBe(4)
    expect(timepointRange(buf, 0)).toEqual([0, 2])
    expect(timepointRange(buf, 1)).toEqual([2, 2])
    expect(timepointRange(buf, 2)).toBeNull()       // nothing there, and that is not an error
  })

  it('emits a cell ONCE PER POPULATION it belongs to', () => {
    // napari draws one layer per population, so a cell in /A and /A/B shows in both. Collapsing would
    // mean silently picking a winner, and with a hierarchy the overlap is the normal case.
    const p = payload({
      pops: [
        { path: '/A', name: 'A', colour: '#ff0000', show: true, isTrack: false, labels: [10] },
        { path: '/A/B', name: 'B', colour: '#0000ff', show: true, isTrack: false, labels: [10] },
      ],
    })
    const buf = buildPointBuffer(p, meta())
    expect(buf.count).toBe(2)
    expect(at(buf, 0).slice(3, 6)).toEqual([1, 0, 0])
    expect(at(buf, 1).slice(3, 6)).toEqual([0, 0, 1])
  })

  it('carries the population colour per instance', () => {
    const buf = buildPointBuffer(payload(), meta())
    const colours = [0, 1, 2, 3].map(i => at(buf, i).slice(3, 6))
    expect(colours).toContainEqual([1, 0, 0])
    expect(colours).toContainEqual([0, 1, 0])
  })

  it('carries the z PLANE, so the 2D view can hide off-plane points without a rebuild', () => {
    // vz = 2 µm, so z = 4 µm is plane 2. Floor, not round: a plane covers [k, k+1) in voxel units,
    // the same convention the slab route indexes with.
    const buf = buildPointBuffer(payload(), meta())
    const planes = [0, 1, 2, 3].map(i => at(buf, i)[6])
    expect(planes.filter(p => p === 0).length).toBe(2)
    expect(planes.filter(p => p === 2).length).toBe(2)
  })

  it('skips hidden populations, and the ones the server already marked hidden', () => {
    const both = buildPointBuffer(payload(), meta())
    expect(both.count).toBe(4)
    const oneHidden = buildPointBuffer(payload(), meta(), new Set(['/B']))
    expect(oneHidden.count).toBe(2)
    const serverHidden = payload()
    serverHidden.pops[0].show = false
    expect(buildPointBuffer(serverHidden, meta()).count).toBe(2)
  })

  it('ignores a member the cell table no longer holds', () => {
    // Membership is cached against mtimes and the table can move under it. A label with no row is a
    // skipped point, never an entry reading past the end of the coordinate arrays.
    const p = payload()
    p.pops[0].labels = [10, 999]
    const buf = buildPointBuffer(p, meta())
    expect(buf.count).toBe(3)
    expect(buf.data.every(Number.isFinite)).toBe(true)
  })

  it('answers empty for the states that are not failures', () => {
    expect(buildPointBuffer(null, meta()).count).toBe(0)
    expect(buildPointBuffer(payload(), null).count).toBe(0)
    // segmented but ungated — thousands of cells, no populations, nothing to draw
    expect(buildPointBuffer(payload({ pops: [] }), meta()).count).toBe(0)
    // an image with no cell table at all
    expect(buildPointBuffer(payload({ nCells: 0, cells: {}, pops: [] }), meta()).count).toBe(0)
  })

  it('puts every point at t=0 when the image has no time axis', () => {
    const p = payload({ hasT: false, cells: { ...payload().cells, t: [] } })
    const buf = buildPointBuffer(p, meta())
    expect(timepointRange(buf, 0)).toEqual([0, 4])
  })
})

describe('hexToUnit', () => {
  it('parses with or without the hash', () => {
    expect(hexToUnit('#ff0000')).toEqual([1, 0, 0])
    expect(hexToUnit('00ff00')).toEqual([0, 1, 0])
  })
  it('falls back to WHITE, never to invisible', () => {
    // A point in the wrong colour is a bug you can see; one that is not drawn reads as missing data.
    expect(hexToUnit('')).toEqual([1, 1, 1])
    expect(hexToUnit(null)).toEqual([1, 1, 1])
    expect(hexToUnit('rebeccapurple')).toEqual([1, 1, 1])
  })
})

describe('overlaySummary', () => {
  it('separates "no cells" from "no populations"', () => {
    // Two different problems with one symptom (an empty overlay), so the panel has to be able to say
    // which: a segmented-but-ungated image has thousands of cells and nothing to show.
    expect(overlaySummary(payload({ pops: [] })))
      .toEqual({ cells: 4, pops: 0, visible: 0, tracked: 3, dropped: 0 })
    expect(overlaySummary(payload({ nCells: 0, cells: {}, pops: [] })))
      .toEqual({ cells: 0, pops: 0, visible: 0, tracked: 0, dropped: 0 })
    expect(overlaySummary(null).cells).toBe(0)
  })
  it('counts tracked cells by the -1 sentinel, not by truthiness', () => {
    // 0 would be a legitimate track id if the server used it; it does not, and -1 is the one sentinel.
    const p = payload({ cells: { ...payload().cells, track: [-1, -1, 5, 6] } })
    expect(overlaySummary(p).tracked).toBe(2)
  })
})
