import { describe, it, expect } from 'vitest'
import { buildViewState, applyViewStateToBrowser } from './viewState'
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
    // panX = +100 µm at 0.5 µm/px → +200 image px. Viewer center moves the OTHER way: cx = W/2 - panXpx.
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

  it('emits per-channel layers with contrast + visibility + LUT-derived colormap name', () => {
    // DAPI LUT top = [0,0,1] → #0000ff → 'blue' (picker canonical over 'i blue'); CD3 = [0,1,0] → 'green'.
    // The name is what `seedConfigFromViewState` reads, so an empty channels map here is what broke
    // fill-from-view.
    const vs = buildViewState({
      cam: fakeCam(), meta: fakeMeta(), t: 0, zPlane: 0, ndisplay: 2,
      canvasW: 512, canvasH: 512, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(vs.layers.DAPI).toEqual({ visible: true,  contrast_limits: [100, 800], colormap: 'blue' })
    expect(vs.layers.CD3).toEqual({  visible: false, contrast_limits: [200, 900], colormap: 'green' })
  })

  it('emits the raw hex when the LUT top is not in the picker palette', () => {
    const meta = fakeMeta()
    meta.channels[0].lut = [[0, 0, 0], [0.12, 0.34, 0.56]]      // custom mix, not any named palette
    const vs = buildViewState({
      cam: fakeCam(), meta, t: 0, zPlane: 0, ndisplay: 2,
      canvasW: 512, canvasH: 512, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(vs.layers.DAPI.colormap).toBe('#1f578f')      // toHex([0.12*255, 0.34*255, 0.56*255])
  })

  it('leaves colormap null when a channel has no LUT at all', () => {
    const meta = fakeMeta()
    meta.channels[0].lut = []
    const vs = buildViewState({
      cam: fakeCam(), meta, t: 0, zPlane: 0, ndisplay: 2,
      canvasW: 512, canvasH: 512, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(vs.layers.DAPI.colormap).toBeNull()
  })

  it('records the canvas the zoom was written against', () => {
    const vs = buildViewState({
      cam: fakeCam(), meta: fakeMeta(), t: 0, zPlane: 0, ndisplay: 2,
      canvasW: 640, canvasH: 480, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(vs.canvas).toEqual({ width: 640, height: 480 })
  })
})

describe('applyViewStateToBrowser (round-trip)', () => {
  // The forward + reverse mappings live next to each other so they can't drift silently. Round-
  // trip a non-trivial 2D state and check every scalar comes back to itself; a broken sign here
  // = keyframe restoration drifts by a pan every time it's clicked.
  it('round-trips a 2D pan / zoom / t / z / channels', () => {
    const startCam = { yaw: 0, pitch: 0, dist: 800, panX: 42, panY: -17 } as OrbitCamera
    const vs = buildViewState({
      cam: startCam, meta: fakeMeta(), t: 3, zPlane: 4, ndisplay: 2,
      canvasW: 600, canvasH: 400, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    const applied = applyViewStateToBrowser({
      vs, meta: fakeMeta(), currentCam: fakeCam(), canvasH: 400,
      viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(applied.cam.panX).toBeCloseTo(startCam.panX, 5)
    expect(applied.cam.panY).toBeCloseTo(startCam.panY, 5)
    expect(applied.cam.dist).toBeCloseTo(startCam.dist, 4)
    expect(applied.t).toBe(3)
    expect(applied.zPlane).toBe(4)
    expect(applied.ndisplay).toBe(2)
    expect(applied.channels.map(c => `${c.name}|${c.lo}|${c.hi}|${c.visible}`))
      .toEqual(['DAPI|100|800|true', 'CD3|200|900|false'])
  })

  it('round-trips a 3D yaw + pitch', () => {
    const startCam = { yaw: Math.PI / 3, pitch: -Math.PI / 8, dist: 1200, panX: 0, panY: 0 } as OrbitCamera
    const vs = buildViewState({
      cam: startCam, meta: fakeMeta(), t: 0, zPlane: 0, ndisplay: 3,
      canvasW: 512, canvasH: 512, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    const applied = applyViewStateToBrowser({
      vs, meta: fakeMeta(), currentCam: fakeCam(), canvasH: 512,
      viewHalfAngle: VIEW_HALF_ANGLE,
    })
    expect(applied.cam.yaw).toBeCloseTo(startCam.yaw, 5)
    expect(applied.cam.pitch).toBeCloseTo(startCam.pitch, 5)
    expect(applied.ndisplay).toBe(3)
  })

  it('keeps CURRENT channel state when the snapshot has no matching layer', () => {
    const vs = buildViewState({
      cam: fakeCam(), meta: fakeMeta(), t: 0, zPlane: 0, ndisplay: 2,
      canvasW: 512, canvasH: 512, viewHalfAngle: VIEW_HALF_ANGLE,
    })
    // Remove one layer from the snapshot — simulating a keyframe captured before that channel
    // existed. The applier must fall back to what the CURRENT viewer already shows.
    delete (vs.layers as Record<string, unknown>).CD3
    const applied = applyViewStateToBrowser({
      vs, meta: fakeMeta(), currentCam: fakeCam(), canvasH: 512,
      viewHalfAngle: VIEW_HALF_ANGLE,
    })
    const cd3 = applied.channels.find(c => c.name === 'CD3')
    expect(cd3?.lo).toBe(200)         // untouched, from meta
    expect(cd3?.hi).toBe(900)
    expect(cd3?.visible).toBe(false)
  })
})
