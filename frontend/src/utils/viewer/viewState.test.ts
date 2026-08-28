import { describe, it, expect } from 'vitest'
import { buildViewState } from './viewState'
import type { ViewerMeta, OrbitCamera } from '../volumeViewer'

// A minimal meta for a hypothetical 512x512x10, 2-channel image with 0.5 µm voxels.
function fakeMeta(): ViewerMeta {
  return {
    nT: 5, nC: 2, nZ: 10, nX: 512, nY: 512,
    bytesPerVoxel: 2, slabBytes: 512 * 512 * 2,
    voxelUm: [0.5, 0.5, 1.0],
    calibrated: { xy: true, z: true, t: true },
    spaceUnit: 'µm', frameIntervalMin: 1,
    contrastSource: 'sampled',
    channels: [
      { name: 'DAPI', lo: 100, hi: 800, visible: true,  lut: [[0, 0, 0], [0, 0, 1]] },
      { name: 'CD3',  lo: 200, hi: 900, visible: false, lut: [[0, 0, 0], [0, 1, 0]] },
    ],
  }
}

function fakeCam(overrides: Partial<OrbitCamera> = {}): OrbitCamera {
  return { yaw: 0, pitch: 0, dist: 1000, panX: 0, panY: 0, ...overrides }
}

const VIEW_HALF_ANGLE = 0.25    // arbitrary; the test locks the arithmetic, not the constant

describe('buildViewState', () => {
  it('centres a face-on 2D view at the image middle', () => {
    const vs = buildViewState({
      cam: fakeCam(), meta: fakeMeta(), t: 2, zPlane: 3, ndisplay: 2,
      canvasW: 800, canvasH: 600, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(vs.camera.center).toEqual([3, 256, 256])
    expect(vs.camera.angles).toEqual([0, 0, 0])
    expect(vs.dims.ndisplay).toBe(2)
    expect(vs.dims.current_step).toEqual([2, 3])
  })

  it('translates pan into an image-pixel centre offset', () => {
    // panX = +100 µm at 0.5 µm/px → +200 image px. Napari center moves the OTHER way: cx = W/2 - panXpx.
    const vs = buildViewState({
      cam: fakeCam({ panX: 100 }), meta: fakeMeta(), t: 0, zPlane: 5, ndisplay: 2,
      canvasW: 512, canvasH: 512, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(vs.camera.center[2]).toBe(256 - 200)     // cx
    expect(vs.camera.center[1]).toBe(256)           // cy unchanged
  })

  it('zoom = canvas_h / visible_L0_h — an all-image fit produces a small zoom', () => {
    // dist=1000, viewHalfAngle=0.25 → visibleHeightUm = 500. voxelUm[1]=0.5 → visibleL0H = 1000.
    // canvasH=500 → zoom = 500/1000 = 0.5.
    const vs = buildViewState({
      cam: fakeCam(), meta: fakeMeta(), t: 0, zPlane: 0, ndisplay: 2,
      canvasW: 500, canvasH: 500, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(vs.camera.zoom).toBeCloseTo(0.5, 6)
  })

  it('3D mode picks the volume-centre z and converts orbit angles to degrees', () => {
    const cam = fakeCam({ yaw: Math.PI / 4, pitch: Math.PI / 6 })  // 45° / 30°
    const vs = buildViewState({
      cam, meta: fakeMeta(), t: 1, zPlane: 3, ndisplay: 3,
      canvasW: 512, canvasH: 512, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(vs.dims.ndisplay).toBe(3)
    expect(vs.camera.center[0]).toBe(Math.floor((10 - 1) / 2))     // volume centre
    expect(vs.camera.angles[0]).toBeCloseTo(30, 4)                 // pitch → rx
    expect(vs.camera.angles[1]).toBeCloseTo(45, 4)                 // yaw → ry
  })

  it('emits per-channel layers with contrast + visibility, colormap null', () => {
    const vs = buildViewState({
      cam: fakeCam(), meta: fakeMeta(), t: 0, zPlane: 0, ndisplay: 2,
      canvasW: 512, canvasH: 512, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(vs.layers.DAPI).toEqual({ visible: true,  contrast_limits: [100, 800], colormap: null })
    expect(vs.layers.CD3).toEqual({  visible: false, contrast_limits: [200, 900], colormap: null })
  })

  it('records the canvas the zoom was written against', () => {
    const vs = buildViewState({
      cam: fakeCam(), meta: fakeMeta(), t: 0, zPlane: 0, ndisplay: 2,
      canvasW: 640, canvasH: 480, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(vs.canvas).toEqual({ width: 640, height: 480 })
  })
})
