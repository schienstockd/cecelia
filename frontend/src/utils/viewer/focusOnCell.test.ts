import { describe, it, expect } from 'vitest'
import { buildFocusViewState } from './focusOnCell'
import type { ViewerViewState } from './viewState'

const state = (over: Partial<ViewerViewState> = {}): ViewerViewState => ({
  camera: { center: [2, 100, 200], zoom: 1.5, angles: [0, 0, 0], perspective: 0 },
  dims:   { ndisplay: 2, current_step: [7, 2], point: [7, 2] },
  layers: { 'A': {} as never },
  canvas: { width: 800, height: 600 },
  ...over,
})

describe('buildFocusViewState', () => {
  it('is null when the viewer never emitted a state — caller must handle', () => {
    expect(buildFocusViewState(null, { t: 5, cx: 10, cy: 20 })).toBeNull()
  })

  it('sets camera.center to [cz, cy, cx] and current_step[0]/point[0] to t', () => {
    const s = buildFocusViewState(state(), { t: 5, cx: 300, cy: 250, cz: 4 })!
    expect(s.camera.center).toEqual([4, 250, 300])
    expect(s.dims.current_step[0]).toBe(5)
    expect(s.dims.point[0]).toBe(5)
  })

  it('preserves zoom / angles / perspective / ndisplay / layers / canvas', () => {
    const src = state({
      camera: { center: [0, 0, 0], zoom: 3.14, angles: [45, 30, 15], perspective: 0.7 },
    })
    const s = buildFocusViewState(src, { t: 5, cx: 1, cy: 2, cz: 3 })!
    expect(s.camera.zoom).toBe(3.14)
    expect(s.camera.angles).toEqual([45, 30, 15])
    expect(s.camera.perspective).toBe(0.7)
    expect(s.dims.ndisplay).toBe(src.dims.ndisplay)
    expect(s.layers).toBe(src.layers)
    expect(s.canvas).toBe(src.canvas)
  })

  it('fits the bbox when halfW/halfH are given — smaller zoom of the two wins so the box fits', () => {
    // canvas 800×600; halfW=100, halfH=50 → bbox 200×100 px + 20% padding → 240×120
    // zoomH = 600 / 120 = 5;  zoomW = 800 / 240 = 3.33 → min is 3.33
    const s = buildFocusViewState(state(), { t: 0, cx: 400, cy: 300, halfWpx: 100, halfHpx: 50 })!
    expect(s.camera.zoom).toBeCloseTo(800 / 240, 4)
  })

  it('leaves zoom alone when halfW/halfH are omitted — "just move, don\'t zoom"', () => {
    const src = state({ camera: { center: [0, 0, 0], zoom: 2.7, angles: [0, 0, 0], perspective: 0 } })
    const s = buildFocusViewState(src, { t: 0, cx: 1, cy: 2 })!
    expect(s.camera.zoom).toBe(2.7)
  })

  it('degenerate bbox (halfW=1, halfH=1) still yields a finite zoom, not NaN', () => {
    // a single-point track has halfW/halfH floored to 1 in the caller; the fit math must not divide by 0
    const s = buildFocusViewState(state(), { t: 0, cx: 100, cy: 100, halfWpx: 1, halfHpx: 1 })!
    expect(Number.isFinite(s.camera.zoom)).toBe(true)
    expect(s.camera.zoom).toBeGreaterThan(0)
  })

  it('falls through to current zoom when the canvas is empty (viewer never rendered)', () => {
    const src = state({ canvas: { width: 0, height: 0 },
                        camera: { center: [0, 0, 0], zoom: 1.5, angles: [0, 0, 0], perspective: 0 } })
    const s = buildFocusViewState(src, { t: 0, cx: 1, cy: 2, halfWpx: 100, halfHpx: 50 })!
    expect(s.camera.zoom).toBe(1.5)
  })

  it('falls through to the current cz when the caller omits it — 2D uses the current plane', () => {
    const s = buildFocusViewState(state(), { t: 5, cx: 300, cy: 250 })!
    expect(s.camera.center).toEqual([2, 250, 300])
  })

  it('leaves the other slider indices (z/y/x) in current_step alone — only t moves', () => {
    const s = buildFocusViewState(state(), { t: 9, cx: 0, cy: 0, cz: 0 })!
    // fixture had current_step=[7, 2]; only [0] changes
    expect(s.dims.current_step).toEqual([9, 2])
    expect(s.dims.point).toEqual([9, 2])
  })

  it('does NOT mutate the input — current_step/point are cloned', () => {
    const src = state()
    const srcStep = src.dims.current_step
    const srcPoint = src.dims.point
    buildFocusViewState(src, { t: 5, cx: 1, cy: 2 })
    expect(src.dims.current_step).toBe(srcStep)   // ref identity intact
    expect(src.dims.point).toBe(srcPoint)
    expect(src.dims.current_step[0]).toBe(7)      // value untouched
  })

  it('is a no-op on t when the state has no time axis (empty dims arrays)', () => {
    // a pre-first-frame state can be shaped with empty arrays; writing to [0] would extend it
    const s = buildFocusViewState(state({ dims: { ndisplay: 2, current_step: [], point: [] } }),
                                  { t: 5, cx: 1, cy: 2 })!
    expect(s.dims.current_step).toEqual([])
    expect(s.dims.point).toEqual([])
    // camera still moves — the t bail only skips the time write. cz falls through to the state's
    // current cz (fixture: 2), since the caller passed no cz.
    expect(s.camera.center).toEqual([2, 2, 1])
  })
})
