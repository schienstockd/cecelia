import { describe, it, expect } from 'vitest'
import { viewerLook, timelapseKeyframes, volumeViewState, type ViewerLookInput } from './viewerLook'
import type { ViewerViewState } from './viewState'

const base = (over: Partial<ViewerLookInput> = {}): ViewerLookInput => ({
  viewState: {
    layers: { DAPI: { visible: true, colormap: 'blue' }, GFP: { visible: false, colormap: 'green' } },
    dims: { ndisplay: 2, current_step: [3, 7], point: [3, 7] },
  },
  channelNames: ['DAPI', 'GFP'],
  version: 'corrected',
  maskValueName: 'cpSAM',
  gating: { valueName: 'flowTom', popType: 'clust' },
  popVisible: () => false,
  trackVisible: {},
  trackSourceColours: {},
  showGatedTracks: false,
  pointSize: 8, pointBorder: 2, labelOpacity: 0.4, tailWidth: 3, tailLength: 12, labelContour: 2,
  trackColourMode: 'speed', colourBy: '', colourOverrides: {},
  ...over,
})

describe('viewerLook', () => {
  it('reads the version, mask, z and visible channels off the viewer', () => {
    const l = viewerLook(base())
    expect(l.valueNames).toEqual(['corrected'])
    expect(l.labelValueNames).toEqual(['cpSAM'])
    expect(l.labelContour).toBe(2)
    expect(l.show3D).toBe(false)
    expect(l.zSlice).toBe(7)
    expect(l.channels).toEqual({ DAPI: 'blue' })
  })

  it('no mask drawn → an explicit empty list, not absent', () => {
    expect(viewerLook(base({ maskValueName: '' })).labelValueNames).toEqual([])
  })

  it('3D → captures the camera + canvas for a batch', () => {
    const l = viewerLook(base({ viewState: { layers: {},
      camera: { center: [1, 2, 3], zoom: 2, angles: [30, 45, 0], perspective: 0 },
      canvas: { width: 800, height: 600 },
      dims: { ndisplay: 3, current_step: [0, 0], point: [0, 0] } } }))
    expect(l.camera3d).toEqual({ angles: [30, 45, 0], zoom: 2, width: 800, height: 600 })
    expect(viewerLook(base()).camera3d).toBeUndefined()   // 2D → no 3D camera
  })

  it('3D perspective → the batch camera carries it', () => {
    const l = viewerLook(base({ viewState: { layers: {},
      camera: { center: [1, 2, 3], zoom: 2, angles: [30, 45, 0], perspective: 1 },
      dims: { ndisplay: 3, current_step: [0, 0], point: [0, 0] } } }))
    expect(l.camera3d).toEqual({ angles: [30, 45, 0], zoom: 2, perspective: 1 })
  })

  it('3D → show3D with no z slice', () => {
    const l = viewerLook(base({ viewState: { layers: {}, dims: { ndisplay: 3, current_step: [0, 4], point: [0, 4] } } }))
    expect(l.show3D).toBe(true)
    expect(l.zSlice).toBeNull()
  })

  it('pops on → the pop manager’s segmentation and popType', () => {
    const l = viewerLook(base({ popVisible: pt => pt === 'clust' }))
    expect(l.showPopulations).toBe(true)
    expect(l.popType).toBe('clust')
    expect(l.popValueName).toBe('flowTom')
  })

  it('tracks only → the first tracked segmentation, with its source colour', () => {
    const l = viewerLook(base({ trackVisible: { cpSAM: true, other: false },
                                trackSourceColours: { cpSAM: '#ff0000' } }))
    expect(l.showTracks).toBe(true)
    expect(l.showPopulations).toBe(false)
    expect(l.popValueName).toBe('cpSAM')
    expect(l.trackSources).toEqual({ cpSAM: { visible: true, colour: '#ff0000' } })
  })

  it('carries the overlay sizes, colour mode and colour-by', () => {
    const l = viewerLook(base({ colourBy: 'clusters', colourOverrides: { '1': '#00ff00' } }))
    expect([l.pointsSize, l.tailWidth, l.tailLength, l.trackColourMode]).toEqual([8, 3, 12, 'speed'])
    expect([l.pointBorder, l.labelOpacity]).toEqual([2, 0.4])
    expect(l.colourBy).toBe('clusters')
    expect(l.colourOverrides).toEqual({ '1': '#00ff00' })
  })

  it('no popType published → flow', () => {
    expect(viewerLook(base({ gating: { valueName: '', popType: '' } })).popType).toBe('flow')
  })
})

describe('timelapseKeyframes', () => {
  const vs: ViewerViewState = {
    camera: { center: [5, 10, 20], zoom: 2, angles: [30, 45, 0], perspective: 0 },
    dims: { ndisplay: 3, current_step: [9, 5], point: [9, 5] },
    layers: {}, canvas: { width: 800, height: 600 },
  }

  it('sweeps t from start to end with one frame per timepoint, the view held', () => {
    const [a, b] = timelapseKeyframes(vs, 2, 10)
    expect(a.viewState.dims.current_step).toEqual([2, 5])
    expect(b.viewState.dims.current_step).toEqual([10, 5])
    expect(b.viewState.dims.point[0]).toBe(10)
    expect(b.steps).toBe(8)                        // + the first keyframe = 9 frames, t = 2..10
    expect(b.viewState.camera).toEqual(vs.camera)
  })

  it('never goes backwards and never asks for zero steps', () => {
    const [a, b] = timelapseKeyframes(vs, 4, 1)
    expect(a.viewState.dims.current_step[0]).toBe(4)
    expect(b.viewState.dims.current_step[0]).toBe(4)
    expect(b.steps).toBe(1)
  })

  it('does not mutate the live snapshot', () => {
    timelapseKeyframes(vs, 0, 3)
    expect(vs.dims.current_step).toEqual([9, 5])
  })
})

describe('volumeViewState', () => {
  it('keeps a 3D view as-is', () => {
    const vs = { camera: { center: [1, 2, 3], zoom: 2, angles: [10, 20, 0], perspective: 0 },
                 dims: { ndisplay: 3, current_step: [4, 0], point: [4, 0] }, layers: {},
                 canvas: { width: 10, height: 10 } } as ViewerViewState
    expect(volumeViewState(vs)).toBe(vs)
  })
  it('turns a 2D view into a straight-on volume with no centre, keeping channels + t', () => {
    const vs = { camera: { center: [5, 50, 60], zoom: 3, angles: [0, 0, 0], perspective: 0 },
                 dims: { ndisplay: 2, current_step: [7, 5], point: [7, 5] },
                 layers: { DAPI: { visible: true, contrast_limits: [0, 1], colormap: 'blue' } },
                 canvas: { width: 640, height: 480 } } as ViewerViewState
    const v = volumeViewState(vs)
    expect(v.dims.ndisplay).toBe(3)
    expect(v.dims.current_step).toEqual([7, 0])
    expect(v.camera.zoom).toBe(1)
    expect('center' in v.camera).toBe(false)
    expect(v.layers).toBe(vs.layers)
    expect(v.canvas).toEqual({ width: 640, height: 480 })
    expect(v.camera.perspective).toBe(0)
    expect(volumeViewState(vs, true).camera.perspective).toBe(1)   // the 3D toggle, not 2D's ortho
  })
})
