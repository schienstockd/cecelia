import { describe, it, expect } from 'vitest'
import { cameraViewL0, screenToImagePx, l0RectToScreen, l0BoxToScreen, l0BoxCorners, BOX_EDGES } from './viewerPick'
import { fitCamera, extentUm, visibleExtentUm, type OrbitCamera, type ViewerMeta } from './volumeViewer'

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
    // The exact centre lands between rows 39 and 40 for nY=80 — floor(H/2 * vy) can round either
    // way with the 2% padding round-off.
    expect([39, 40]).toContain(p.y)
  })

  it('screen top → row 0; screen bottom → last row (mask store row 0 at canvas top)', () => {
    // Mask store's row 0 is at the TOP of the canvas — see the shader orientation check in
    // `docs/todo/spike/webgpu/shader_check.mjs`.
    const m = meta({ nX: 100, nY: 100 })
    const W = 200, H = 200
    const cam = fittedCam(m, W, H)
    const top    = screenToImagePx(W / 2, 2,      W, H, cam, m)
    const bottom = screenToImagePx(W / 2, H - 2,  W, H, cam, m)
    expect(top.y).toBeLessThanOrEqual(5)              // near 0
    expect(bottom.y).toBeGreaterThanOrEqual(m.nY - 5) // near nY-1
  })

  it('pan shifts what is under the pointer, opposite direction to the eye', () => {
    // Shader ground truth (see viewerPick.ts): screen-centre = world `(panX, -panY)`.
    // `panDrag(cam, +10, 0, H)` — a drag RIGHT — decreases panX (the eye moves LEFT), so the
    // image APPEARS to shift right under a stationary pointer. The pixel now at canvas centre is
    // one that USED to sit LEFT of centre → a SMALLER x pixel. The old test asserted the
    // opposite because the picker itself was pan-sign-inverted vs the shader (a mirror that
    // only showed on a panned image, and produced "picks the diagonally opposite cell" reports
    // in the correction cockpit); fixing that turned this test over too.
    const m = meta()
    const W = 200, H = 200
    const cam0 = fittedCam(m, W, H)
    const p0 = screenToImagePx(W / 2, H / 2, W, H, cam0, m)
    const cam1 = { ...cam0, panX: cam0.panX - 5 }
    const p1 = screenToImagePx(W / 2, H / 2, W, H, cam1, m)
    expect(p1.x).toBeLessThan(p0.x)
  })

  it('respects anisotropic voxels', () => {
    // 100 x 80 image, 0.5 µm/px x, 2 µm/px y. World extent = 50 µm × 160 µm.
    const m = meta({ voxelUm: [0.5, 2, 1] })
    const W = 200, H = 200
    const cam = fittedCam(m, W, H)
    const p = screenToImagePx(W / 2, H / 2, W, H, cam, m)
    expect(p.x).toBe(50)                // half of nX
    expect([39, 40]).toContain(p.y)     // half of nY, ± 1 for the reflection's half-pixel round-off
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

  it('nX/nY override returns level-N pixel indices (physical extent is level-invariant)', () => {
    // L0 = 100 x 100; L1 = 50 x 50 at the same physical extent. The centre pixel of L0 (~50) becomes
    // the centre pixel of L1 (~25) at the same click. Prevents "picked wrong cell" when the display
    // is at LOD > 0 and the pick reads a different level than the mask on screen.
    const m = meta({ nX: 100, nY: 100 })
    const W = 200, H = 200
    const cam = fittedCam(m, W, H)
    const p0 = screenToImagePx(W / 2, H / 2, W, H, cam, m)
    const p1 = screenToImagePx(W / 2, H / 2, W, H, cam, m, 50, 50)
    expect(p0.x).toBeGreaterThanOrEqual(49); expect(p0.x).toBeLessThanOrEqual(50)
    expect(p1.x).toBeGreaterThanOrEqual(24); expect(p1.x).toBeLessThanOrEqual(25)
    expect(p1.in).toBe(true)
  })
})

describe('cameraViewL0', () => {
  it('spans exactly what visibleExtentUm says is on screen, centred on the pan', () => {
    const m = meta({ voxelUm: [0.5, 2, 1] })
    const W = 300, H = 200
    const cam: OrbitCamera = { ...fittedCam(m, W, H), panX: 7, panY: -3 }
    const v = cameraViewL0(cam, m, W, H)
    const [w, h] = visibleExtentUm(cam.dist, W / H)
    expect((v.x1 - v.x0) * 0.5).toBeCloseTo(w)
    expect((v.y1 - v.y0) * 2).toBeCloseTo(h)
    expect(((v.x0 + v.x1) / 2) * 0.5).toBeCloseTo(7 + (100 * 0.5) / 2)   // panX + image half-width, µm
    expect(((v.y0 + v.y1) / 2) * 2).toBeCloseTo(3 + (80 * 2) / 2)      // -panY + image half-height, µm
  })
})

describe('l0BoxToScreen', () => {
  const m = meta({ nZ: 10, voxelUm: [0.5, 0.5, 2] })
  const box = { x0: 10, x1: 29, y0: 40, y1: 59, z0: 3, z1: 5 }
  const near = (a: { x: number, y: number, w: number, h: number } | null, b: { x: number, y: number, w: number, h: number }) => {
    expect(a).not.toBeNull()
    for (const k of ['x', 'y', 'w', 'h'] as const) expect(a![k]).toBeCloseTo(b[k], 6)
  }

  it('face-on and orthographic, it is the plane view\'s own rectangle (two derivations agree)', () => {
    const cam: OrbitCamera = { ...fitCamera(extentUm(m), 400 / 300), panX: 3, panY: -7, dist: 20 }
    near(l0BoxToScreen(box, cam, m, [0, 9], true, 400, 300),
         l0RectToScreen({ x0: 10, y0: 40, x1: 30, y1: 60 }, cam, m, 400, 300))
  })

  it('under perspective, a box flat on the loaded slab\'s centre plane projects as under orthographic', () => {
    const cam: OrbitCamera = { ...fitCamera(extentUm(m), 1, true), panX: 2, panY: 1 }
    // slab 0–9 → centre at z = 5 voxels; z0 5 / z1 4 puts both z faces on that plane (z1 + 1 = 5)
    const flat = { ...box, z0: 5, z1: 4 }
    near(l0BoxToScreen(flat, cam, m, [0, 9], false, 300, 300), l0BoxToScreen(flat, cam, m, [0, 9], true, 300, 300)!)
  })

  it('follows rotation: turned 90° about y, the box\'s screen width is its depth, not its x extent', () => {
    const cam: OrbitCamera = { yaw: Math.PI / 2, pitch: 0, dist: 50, panX: 0, panY: 0 }
    const r = l0BoxToScreen(box, cam, m, [0, 9], true, 300, 300)!
    const pxPerUm = 300 / (2 * 50 * visibleExtentUm(1, 1)[1] / 2)
    expect(r.w).toBeCloseTo((box.z1 + 1 - box.z0) * 2 * pxPerUm, 6)   // 3 planes × 2 µm
    expect(r.h).toBeCloseTo((box.y1 + 1 - box.y0) * 0.5 * pxPerUm, 6)
  })

  it('is null when the box is behind the eye under perspective', () => {
    const cam: OrbitCamera = { yaw: 0, pitch: 0, dist: 1, panX: 0, panY: 0 }
    expect(l0BoxToScreen({ ...box, z0: 9, z1: 9 }, cam, m, [0, 9], false, 300, 300)).toBeNull()
  })
})

describe('BOX_EDGES', () => {
  it('are the twelve edges of a box: each joins corners that differ on exactly one axis, each once', () => {
    expect(BOX_EDGES).toHaveLength(12)
    const key = new Set(BOX_EDGES.map(([a, b]) => `${Math.min(a, b)}-${Math.max(a, b)}`))
    expect(key.size).toBe(12)
    for (const [a, b] of BOX_EDGES) expect([1, 2, 4]).toContain(a ^ b)
  })
  it('index the corners in l0BoxCorners\' order (x bit 4, y bit 2, z bit 1)', () => {
    const m = meta({ nZ: 4 })
    const cam: OrbitCamera = { ...fitCamera(extentUm(m), 1), panX: 0, panY: 0 }
    const c = l0BoxCorners({ x0: 10, x1: 19, y0: 20, y1: 29, z0: 0, z1: 1 }, cam, m, [0, 3], true, 200, 200)!
    expect(c[4].x).toBeGreaterThan(c[0].x)   // x bit → further right
    expect(c[2].y).toBeGreaterThan(c[0].y)   // y bit → further down
  })
})
