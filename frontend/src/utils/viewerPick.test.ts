import { describe, it, expect } from 'vitest'
import { screenToImagePx } from './viewerPick'
import { fitCamera, extentUm, VIEW_HALF_ANGLE, type OrbitCamera, type ViewerMeta } from './volumeViewer'

/** 100 x 80 image, 1 µm/px in x and y — a click at the canvas centre with a face-on fitted camera
 *  should land on the image centre pixel. Anything else is a bug. */
const meta = (over: Partial<ViewerMeta> = {}): ViewerMeta => ({
  nT: 1, nC: 1, nZ: 1, nX: 100, nY: 80, bytesPerVoxel: 2, slabBytes: 100 * 80 * 2,
  contrastSource: 'sampled', voxelUm: [1, 1, 1],
  calibrated: { xy: true, z: false, t: false }, spaceUnit: null, frameIntervalMin: null,
  channels: [{ name: 'a', lo: 0, hi: 10, visible: true, lut: [[0, 0, 0], [1, 0, 0]] }],
  ...over,
})

/** A camera that would fit `meta`'s extent into the canvas at zero pan. Uses the same `fitCamera`
 *  the viewer starts with, so a golden test can't drift from the shader convention. */
const fittedCam = (m: ViewerMeta, canvasW: number, canvasH: number): OrbitCamera =>
  fitCamera(extentUm(m, 1), canvasW / canvasH)

describe('screenToImagePx', () => {
  it('centre of the canvas → centre pixel of the image', () => {
    const m = meta()
    const W = 200, H = 200
    const cam = fittedCam(m, W, H)
    const p = screenToImagePx(W / 2, H / 2, W, H, cam, m)
    expect(p.in).toBe(true)
    expect(p.x).toBe(50)
    expect(p.y).toBe(40)
  })

  it('top-left of the fitted image sits at (0, 0)', () => {
    // Fit puts the image's short axis touching the viewport with ~2% padding — so the pixel at
    // the image's top-left corner is slightly inside the canvas. Verify no y-flip: pointing at the
    // TOP-LEFT of the image (image row 0, col 0) must resolve to (0, 0), not (0, nY-1).
    const m = meta({ nX: 100, nY: 100 })  // square, so fit centres it perfectly
    const W = 200, H = 200
    const cam = fittedCam(m, W, H)
    // Image half-extent in world = 50 µm; visible half-height in world = dist * VIEW_HALF_ANGLE.
    const halfH_world = cam.dist * VIEW_HALF_ANGLE
    // Canvas y-pixel for image top-left (world y = +50):
    //   ndcY = 50 / halfH_world; canvas cy = (1 - ndcY) * H / 2
    const cy = (1 - 50 / halfH_world) * H / 2
    const cx = (0 / halfH_world + 1) * W / 2 - 50 / halfH_world * W / 2  // world x = -50
    const p = screenToImagePx(cx, cy, W, H, cam, m)
    expect(p.x).toBe(0)
    expect(p.y).toBe(0)
  })

  it('pan shifts what is under the pointer, opposite direction to the eye', () => {
    // `panDrag(cam, +10, 0, H)` decreases panX by (10 * umPerPx). At W=H=200 and dist=fittedDist,
    // umPerPx = (2 * dist * VIEW_HALF_ANGLE) / H. So panning the eye LEFT by that amount moves the
    // world origin to appear RIGHT of centre — a click at canvas centre lands on a LARGER x pixel.
    const m = meta()
    const W = 200, H = 200
    const cam0 = fittedCam(m, W, H)
    const p0 = screenToImagePx(W / 2, H / 2, W, H, cam0, m)
    const cam1 = { ...cam0, panX: cam0.panX - 5 }
    const p1 = screenToImagePx(W / 2, H / 2, W, H, cam1, m)
    expect(p1.x).toBeGreaterThan(p0.x)
  })

  it('respects anisotropic voxels', () => {
    // 100 x 80 image, 0.5 µm/px x, 2 µm/px y. World extent = 50 µm × 160 µm.
    const m = meta({ voxelUm: [0.5, 2, 1] })
    const W = 200, H = 200
    const cam = fittedCam(m, W, H)
    const p = screenToImagePx(W / 2, H / 2, W, H, cam, m)
    expect(p.x).toBe(50)     // half of nX
    expect(p.y).toBe(40)     // half of nY
  })

  it('outside the image → in:false, coords still computed', () => {
    const m = meta()
    const W = 200, H = 200
    const cam = fittedCam(m, W, H)
    // Click well beyond the image, past the right edge in world µm.
    const p = screenToImagePx(W, H / 2, W, H, cam, m)
    expect(p.in).toBe(false)
    // The x/y are still returned (the caller may want to log), just outside the image bounds.
    expect(typeof p.x).toBe('number')
    expect(typeof p.y).toBe('number')
  })

  it('uncalibrated axis: voxelUm 0 treated as 1 µm/px (avoids divide by zero)', () => {
    const m = meta({ voxelUm: [0, 0, 0], calibrated: { xy: false, z: false, t: false } })
    const W = 200, H = 200
    const cam = fittedCam(m, W, H)
    const p = screenToImagePx(W / 2, H / 2, W, H, cam, m)
    expect(Number.isFinite(p.x)).toBe(true)
    expect(Number.isFinite(p.y)).toBe(true)
  })
})
