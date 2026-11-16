import { describe, it, expect } from 'vitest'
import { mergeViewState, fitLongSide } from './viewerRequest'
import type { ViewerViewState } from './viewState'

const live = (): ViewerViewState => ({
  camera: { center: [5, 100, 200], zoom: 2, angles: [0, 0, 0], perspective: 0 },
  dims: { ndisplay: 2, current_step: [40, 7], point: [40, 7] },
  layers: {
    GFP: { visible: true, contrast_limits: [0, 1000], colormap: 'green' },
    CTV: { visible: false, contrast_limits: [10, 500], colormap: 'blue' },
  },
  canvas: { width: 800, height: 600 },
})

describe('mergeViewState', () => {
  it('changes only the fields the patch names', () => {
    const { view, unknown } = mergeViewState(live(), { layers: { GFP: { contrast_limits: [50, 400] } } })
    expect(view.layers.GFP).toEqual({ visible: true, contrast_limits: [50, 400], colormap: 'green' })
    expect(view.layers.CTV).toEqual(live().layers.CTV)
    expect(view.camera).toEqual(live().camera)
    expect(view.dims).toEqual(live().dims)
    expect(unknown).toEqual([])
  })

  it('switches to 3D and keeps the live z when only t is given', () => {
    const { view } = mergeViewState(live(), { dims: { ndisplay: 3, current_step: [12] } })
    expect(view.dims.ndisplay).toBe(3)
    expect(view.dims.current_step).toEqual([12, 7])
    expect(view.dims.point).toEqual([12, 7])
  })

  it('reports a layer the image has no channel of instead of applying it', () => {
    const { view, unknown } = mergeViewState(live(), { layers: { mCherry: { visible: true } } })
    expect(unknown).toEqual(['mCherry'])
    expect(Object.keys(view.layers)).toEqual(['GFP', 'CTV'])
  })

  it('leaves the live view untouched', () => {
    const base = live()
    mergeViewState(base, { camera: { zoom: 9 }, layers: { GFP: { visible: false } } })
    expect(base).toEqual(live())
  })

  it('treats a non-object patch as no change', () => {
    expect(mergeViewState(live(), null).view).toEqual(live())
  })
})

describe('fitLongSide', () => {
  it('scales the long side down to maxSide', () => {
    expect(fitLongSide(3200, 1600, 1600)).toEqual({ width: 1600, height: 800 })
    expect(fitLongSide(900, 1800, 1200)).toEqual({ width: 600, height: 1200 })
  })
  it('never scales up, and no maxSide keeps the size', () => {
    expect(fitLongSide(800, 600, 1600)).toEqual({ width: 800, height: 600 })
    expect(fitLongSide(3200, 1600)).toEqual({ width: 3200, height: 1600 })
  })
})
