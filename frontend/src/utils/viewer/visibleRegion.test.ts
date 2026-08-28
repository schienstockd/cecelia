import { describe, it, expect } from 'vitest'
import { visibleRegion } from './visibleRegion'

// A base camera state — image fills the canvas exactly, plane view, centred.
const base = {
  panX: 0, panY: 0, zoom: 1,
  canvasW: 512, canvasH: 512,
  imageW: 512, imageH: 512,
  currentZ: 3, currentT: 7, ndisplay: 2,
}

describe('visibleRegion', () => {
  it('reports the whole image when fully zoomed out', () => {
    const r = visibleRegion(base)
    expect(r.xy.X).toEqual([0, 512])
    expect(r.xy.Y).toEqual([0, 512])
    expect(r.z).toBe(3)
    expect(r.t).toBe(7)
    expect(r.ndisplay).toBe(2)
  })

  it('shrinks the window when zoomed in', () => {
    // 2x zoom halves the visible extent (256×256) still centred
    const r = visibleRegion({ ...base, zoom: 2 })
    expect(r.xy.X).toEqual([128, 384])
    expect(r.xy.Y).toEqual([128, 384])
  })

  it('shifts the window under pan', () => {
    // Pan the camera 100 image-px to the right → the visible window shifts left by 100
    const r = visibleRegion({ ...base, zoom: 2, panX: 100 })
    expect(r.xy.X).toEqual([28, 284])
    expect(r.xy.Y).toEqual([128, 384])
  })

  it('clamps to the image bounds when the camera hangs off the edge', () => {
    // Panned enough that half the visible rect would be at negative X: clamp, don't emit negatives
    const r = visibleRegion({ ...base, zoom: 4, panX: 200 })
    // vis width = 128, centre_x = 256 - 200 = 56 → nominal [-8, 120] → clamped [0, 120]
    expect(r.xy.X[0]).toBeGreaterThanOrEqual(0)
    expect(r.xy.X[1]).toBeLessThanOrEqual(512)
  })

  it('reports the full XY extent in 3D display mode', () => {
    // A volume view has no single plane, so we report the whole XY and let the worker preview at `z`
    const r = visibleRegion({ ...base, ndisplay: 3, zoom: 5, panX: 100, panY: 100 })
    expect(r.xy.X).toEqual([0, 512])
    expect(r.xy.Y).toEqual([0, 512])
    expect(r.ndisplay).toBe(3)
  })

  it('floors z and t to integers', () => {
    const r = visibleRegion({ ...base, currentZ: 3.7, currentT: 12.2 })
    expect(r.z).toBe(3)
    expect(r.t).toBe(12)
  })

  it('never returns an empty span', () => {
    // Pathological: zoom too high, camera off the edge — clamp to the whole image rather than emit
    // a span the worker would treat as an empty region
    const r = visibleRegion({ ...base, zoom: 100, panX: 10_000, panY: 10_000 })
    expect(r.xy.X[1]).toBeGreaterThan(r.xy.X[0])
    expect(r.xy.Y[1]).toBeGreaterThan(r.xy.Y[0])
  })
})
