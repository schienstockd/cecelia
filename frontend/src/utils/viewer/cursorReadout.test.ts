import { describe, it, expect } from 'vitest'
import { peekPoint, fmtPosition, fmtIntensity, rulerLength, labelPopIndex, fmtMeasure, readoutChannels } from './cursorReadout'
import { VIEW_HALF_ANGLE, type OrbitCamera, type ViewerMeta } from '../volumeViewer'

const meta = (over: Partial<ViewerMeta> = {}): ViewerMeta => ({
  nT: 1, nC: 2, nZ: 1, nX: 1000, nY: 800, bytesPerVoxel: 2, slabBytes: 0,
  contrastSource: 'sampled', voxelUm: [0.5, 0.5, 2],
  calibrated: { xy: true, z: true, t: false }, spaceUnit: null, frameIntervalMin: null,
  channels: [], ...over,
})
/** a camera showing `visL0H` L0 rows top to bottom, centred on the image */
const camFor = (visL0H: number, um: number): OrbitCamera =>
  ({ yaw: 0, pitch: 0, dist: (visL0H * um) / (2 * VIEW_HALF_ANGLE), panX: 0, panY: 0 })

describe('peekPoint', () => {
  // 800×600 canvas showing all 800 rows: the canvas centre is the image centre, and the displayed
  // level (half size) reads the same spot at half the index.
  const m = meta()
  const cam = camFor(800, 0.5)
  it('reads L0 for the intensities and the displayed level for the cell', () => {
    const p = peekPoint(400, 300, 800, 600, cam, m, 500, 400)
    expect(p).toEqual({ x: 500, y: 400, lx: 250, ly: 200, in: true })
  })
  it('is not inside on the margin beside the image', () => {
    // 800 px of canvas width show 800·(800/600) ≈ 1067 columns, so x = 0 is left of column 0
    expect(peekPoint(0, 300, 800, 600, cam, m, 500, 400).in).toBe(false)
  })
})

describe('fmtPosition', () => {
  it('prints the pixel centre in the image unit, and z / t only when there are several', () => {
    expect(fmtPosition({ x: 10, y: 4 }, 0, 0, meta())).toBe('x 5.25 · y 2.25 µm')
    expect(fmtPosition({ x: 10, y: 4 }, 3, 7, meta({ nZ: 5, nT: 9 }))).toBe('x 5.25 · y 2.25 µm · z 3 · t 7')
  })
  it('falls back to pixels when the image is not calibrated', () => {
    expect(fmtPosition({ x: 10, y: 4 }, 0, 0, meta({ calibrated: { xy: false, z: false, t: false } })))
      .toBe('x 10.5 · y 4.50 px')
  })
  it('names the unit the image is in', () => {
    expect(fmtPosition({ x: 300, y: 0 }, 0, 0, meta({ spaceUnit: 'nm' }))).toBe('x 150 · y 0.25 nm')
  })
})

describe('rulerLength', () => {
  it('scales x and y by their own voxel size before the hypotenuse', () => {
    // 3 px across at 1 µm, 4 px down at 2 µm → a 3-by-8 µm triangle, not 5 px × anything
    const r = rulerLength([0, 0], [3, 4], meta({ voxelUm: [1, 2, 1] }))
    expect(r.value).toBeCloseTo(Math.hypot(3, 8))
    expect(r.unit).toBe('µm')
  })
  it('measures in pixels when the image is not calibrated', () => {
    const r = rulerLength([1, 1], [4, 5], meta({ calibrated: { xy: false, z: false, t: false } }))
    expect(r).toEqual({ value: 5, unit: 'px' })
  })
})

describe('fmtMeasure / fmtIntensity', () => {
  it('keeps three-ish significant digits', () => {
    expect([fmtMeasure(1.234), fmtMeasure(12.34), fmtMeasure(123.4)]).toEqual(['1.23', '12.3', '123'])
  })
  it('prints stored integers whole and floats short', () => {
    expect([fmtIntensity(4095), fmtIntensity(0.123456), fmtIntensity(1234.5)]).toEqual(['4095', '0.123', '1230'])
  })
})

describe('labelPopIndex', () => {
  const pops = [
    { path: '/T', name: 'T', colour: '#111111', labels: [1, 2, 3] },
    { path: '/T/CD8', name: 'CD8', colour: '#222222', labels: [1, 2] },
    { path: '/T/CD8/Act', name: 'Act', colour: '#333333', labels: [1] },
    { path: '/Tx', name: 'Tx', colour: '#444444', labels: [1] },   // a prefix of /T's NAME, not a child
  ]
  const idx = labelPopIndex(pops)
  it('lists only the deepest populations a cell is in', () => {
    expect(idx.get(1)?.map(p => p.name)).toEqual(['Act', 'Tx'])
    expect(idx.get(2)?.map(p => p.name)).toEqual(['CD8'])
    expect(idx.get(3)?.map(p => p.name)).toEqual(['T'])
  })
  it('has nothing for a cell in no population', () => {
    expect(idx.get(9)).toBeUndefined()
  })
})

describe('readoutChannels', () => {
  const ch = (name: string, visible: boolean, lut = [[0, 0, 0], [1, 0, 0]]) => ({ name, visible, lut })
  it('lists the drawn channels with their colour and counts the hidden ones', () => {
    const r = readoutChannels([10, 20.5, 30], [ch('DAPI', true, [[0, 0, 0], [0, 0, 1]]), ch('CD4', false), ch('CD8', true)])
    expect(r.shown).toEqual([
      { name: 'DAPI', value: '10', colour: '#0000ff' },
      { name: 'CD8', value: '30', colour: '#ff0000' },
    ])
    expect(r.hidden).toBe(1)
  })
})

describe('unit symbol', () => {
  it('prints µm for an OME unit spelled out', () => {
    expect(fmtPosition({ x: 0, y: 0 }, 0, 0, meta({ spaceUnit: 'micrometer' }))).toBe('x 0.25 · y 0.25 µm')
    expect(rulerLength([0, 0], [1, 0], meta({ spaceUnit: 'micrometer' })).unit).toBe('µm')
  })
})
