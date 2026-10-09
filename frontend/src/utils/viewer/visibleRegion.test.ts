import { describe, it, expect } from 'vitest'
import { visibleRegion, PREVIEW_REGION_MAX_SIDE } from './visibleRegion'
import { cameraViewL0, l0RectToScreen, screenToL0, screenToImagePx } from '../viewerPick'
import { VIEW_HALF_ANGLE, type OrbitCamera, type ViewerMeta } from '../volumeViewer'

// A base state — the view shows the whole 512² image, plane view.
const base = {
  view: { x0: 0, y0: 0, x1: 512, y1: 512 },
  imageW: 512, imageH: 512,
  currentZ: 3, currentT: 7, ndisplay: 2,
}

describe('visibleRegion', () => {
  it('reports the whole image when it is all on screen and under the cap', () => {
    const r = visibleRegion(base)
    expect(r.xy.X).toEqual([0, 512])
    expect(r.xy.Y).toEqual([0, 512])
    expect(r.z).toBe(3)
    expect(r.t).toBe(7)
    expect(r.ndisplay).toBe(2)
    expect(r.capped).toBe(false)
  })

  it('reports the visible window', () => {
    const r = visibleRegion({ ...base, view: { x0: 228, y0: 128, x1: 484, y1: 384 } })
    expect(r.xy.X).toEqual([228, 484])
    expect(r.xy.Y).toEqual([128, 384])
  })

  it('clamps to the image bounds when the view hangs off the edge', () => {
    const r = visibleRegion({ ...base, view: { x0: 392, y0: -50, x1: 520, y1: 100 } })
    expect(r.xy.X).toEqual([392, 512])
    expect(r.xy.Y).toEqual([0, 100])
  })

  it('caps a large view at maxSide per axis, centred on the visible part', () => {
    const r = visibleRegion({ ...base, imageW: 40000, imageH: 30000,
                              view: { x0: 10000, y0: 5000, x1: 14000, y1: 7000 } })
    expect(r.xy.X).toEqual([11488, 12512])
    expect(r.xy.Y).toEqual([5488, 6512])
    expect(r.capped).toBe(true)
  })

  it('caps only the axis that is over budget', () => {
    const r = visibleRegion({ ...base, imageW: 40000, imageH: 30000, maxSide: 1000,
                              view: { x0: 0, y0: 0, x1: 3000, y1: 800 } })
    expect(r.xy.X).toEqual([1000, 2000])
    expect(r.xy.Y).toEqual([0, 800])
    expect(r.capped).toBe(true)
  })

  it('reports the whole XY extent in 3D display mode, capped around the image centre', () => {
    const small = visibleRegion({ ...base, ndisplay: 3, view: { x0: 5, y0: 5, x1: 6, y1: 6 } })
    expect(small.xy.X).toEqual([0, 512])
    expect(small.xy.Y).toEqual([0, 512])
    expect(small.ndisplay).toBe(3)
    const big = visibleRegion({ ...base, ndisplay: 3, imageW: 4096, imageH: 4096 })
    expect(big.xy.X).toEqual([1536, 2560])
    expect(big.capped).toBe(true)
  })

  it('floors z and t to integers', () => {
    const r = visibleRegion({ ...base, currentZ: 3.7, currentT: 12.2 })
    expect(r.z).toBe(3)
    expect(r.t).toBe(12)
  })

  it('never returns an empty span', () => {
    // camera entirely off the image — the whole image rather than a span the worker reads as empty
    const r = visibleRegion({ ...base, view: { x0: 9000, y0: 9000, x1: 9100, y1: 9100 } })
    expect(r.xy.X[1]).toBeGreaterThan(r.xy.X[0])
    expect(r.xy.Y[1]).toBeGreaterThan(r.xy.Y[0])
  })
})

// The region is what is ON SCREEN — a canvas much smaller than the image must not shrink it. The old
// conversion fed `zoom = imageH / visibleH` into `visH = canvasH / zoom`, so on a 45932×28356 image a
// view ~1.5 mm wide previewed a box of a few dozen pixels (#1553). The old unit tests used canvas
// size == image size, where the error cancels.
describe('camera → preview region', () => {
  const meta = (nX: number, nY: number, um = 0.5): ViewerMeta => ({
    nT: 1, nC: 1, nZ: 1, nX, nY, bytesPerVoxel: 2, slabBytes: 0,
    contrastSource: 'sampled', voxelUm: [um, um, 1],
    calibrated: { xy: true, z: false, t: false }, spaceUnit: null, frameIntervalMin: null,
    channels: [],
  })
  /** a camera showing `visL0H` L0 pixels top-to-bottom, centred `(panUmX, panUmY)` off image centre */
  const camFor = (visL0H: number, um: number, panX = 0, panY = 0): OrbitCamera =>
    ({ yaw: 0, pitch: 0, dist: (visL0H * um) / (2 * VIEW_HALF_ANGLE), panX, panY })

  it('a 600-px-tall view on an 800×1200 canvas previews ~600 px, not ~17', () => {
    const m = meta(45932, 28356)
    const view = cameraViewL0(camFor(600, 0.5), m, 1200, 800)
    const r = visibleRegion({ view, imageW: m.nX, imageH: m.nY, currentZ: 0, currentT: 0, ndisplay: 2 })
    expect(r.xy.Y[1] - r.xy.Y[0]).toBeGreaterThanOrEqual(600)
    expect(r.xy.Y[1] - r.xy.Y[0]).toBeLessThanOrEqual(602)
    expect(r.xy.X[1] - r.xy.X[0]).toBeGreaterThanOrEqual(900)    // 600 × 1200/800
    expect(r.xy.X[1] - r.xy.X[0]).toBeLessThanOrEqual(902)
    expect(r.capped).toBe(false)
    // centred on the image centre at zero pan
    expect((r.xy.X[0] + r.xy.X[1]) / 2).toBeCloseTo(45932 / 2, -1)
    expect((r.xy.Y[0] + r.xy.Y[1]) / 2).toBeCloseTo(28356 / 2, -1)
  })

  it('a ~1.5 mm view is capped to the budget around the view centre', () => {
    const m = meta(45932, 28356)
    // pan 1 mm right and 0.5 mm up (screen-up = smaller image rows)
    const view = cameraViewL0(camFor(3000, 0.5, 1000, 500), m, 1200, 800)
    const r = visibleRegion({ view, imageW: m.nX, imageH: m.nY, currentZ: 0, currentT: 0, ndisplay: 2 })
    expect(r.xy.X[1] - r.xy.X[0]).toBe(PREVIEW_REGION_MAX_SIDE)
    expect(r.xy.Y[1] - r.xy.Y[0]).toBe(PREVIEW_REGION_MAX_SIDE)
    expect(r.capped).toBe(true)
    expect((r.xy.X[0] + r.xy.X[1]) / 2).toBeCloseTo(45932 / 2 + 2000, -1)
    expect((r.xy.Y[0] + r.xy.Y[1]) / 2).toBeCloseTo(28356 / 2 - 1000, -1)
  })

  it('anisotropic pixels: each axis converts with its own voxel size', () => {
    const m = { ...meta(4000, 4000), voxelUm: [0.25, 0.5, 1] as [number, number, number] }
    const view = cameraViewL0(camFor(400, 0.5), m, 800, 800)
    expect(view.y1 - view.y0).toBeCloseTo(400)
    expect(view.x1 - view.x0).toBeCloseTo(800)    // 200 µm at 0.25 µm/px
  })

  it('l0RectToScreen is the inverse of the view: the view maps to the whole canvas', () => {
    const m = meta(5000, 3000)
    const cam = camFor(700, 0.5, 120, -40)
    const s = l0RectToScreen(cameraViewL0(cam, m, 1000, 600), cam, m, 1000, 600)
    expect(s.x).toBeCloseTo(0); expect(s.y).toBeCloseTo(0)
    expect(s.w).toBeCloseTo(1000); expect(s.h).toBeCloseTo(600)
    const [x, y] = screenToL0(s.x + s.w, s.y + s.h, cam, m, 1000, 600)
    const v = cameraViewL0(cam, m, 1000, 600)
    expect(x).toBeCloseTo(v.x1); expect(y).toBeCloseTo(v.y1)
  })
  it('agrees with the picker (the shader ground truth) on which pixel is at a canvas corner', () => {
    const m = meta(5000, 3000)
    const cam = camFor(700, 0.5, 120, -40)
    const v = cameraViewL0(cam, m, 1000, 600)
    const p = screenToImagePx(0.5, 0.5, 1000, 600, cam, m)
    expect(p.x).toBe(Math.floor(v.x0 + 0.5 * (v.x1 - v.x0) / 1000))
    expect(p.y).toBe(Math.floor(v.y0 + 0.5 * (v.y1 - v.y0) / 600))
  })
})
