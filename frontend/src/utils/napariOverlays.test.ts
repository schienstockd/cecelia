import { describe, it, expect } from 'vitest'
import { unionViewSnapshot, labelsRequestBody } from './napariOverlays'

// The show-labels endpoint REBUILDS the Labels layer and defaults `labelContour` to 0, so a push that
// drops the outline silently refills a mask the user had outlined — which is why recorded movies came
// out filled: the outline was gone before the recorder ran, lost on the last mask toggle or overlay
// restore. These pin the outline onto the wire.
describe('labelsRequestBody', () => {
  it('carries labelContour when given, including 0 (an explicit "filled")', () => {
    expect(labelsRequestBody({ labels: { A: ['a.zarr'] }, show: true, labelContour: 3 }))
      .toMatchObject({ allLabels: { A: ['a.zarr'] }, showLabels: true, labelContour: 3 })
    expect(labelsRequestBody({ labels: { A: ['a.zarr'] }, show: true, labelContour: 0 }))
      .toHaveProperty('labelContour', 0)
  })

  it('omits labelContour entirely when the caller has no set to read it from', () => {
    // absent ≠ 0 on the wire: the backend still defaults it, but the request must not ASSERT "filled"
    expect(labelsRequestBody({ labels: { A: ['a.zarr'] }, show: true }))
      .not.toHaveProperty('labelContour')
  })

  it('keeps the rest of the shape (both payloads, the shared show flag, preview) with labelsCache pinned true', () => {
    // `labelsCache` is now hardcoded in the request body — the caller no longer decides it. The bridge
    // default matches; keeping `true` on the wire preserves the pre-P6 behaviour until the bridge is
    // deleted in P9.
    const b = labelsRequestBody({ labels: { A: ['a'] }, branchLabels: { A: ['b'] },
                                  show: false, preview: true, labelContour: 2 })
    expect(b).toEqual({ allLabels: { A: ['a'] }, allBranchLabels: { A: ['b'] },
                        showLabels: false, labelsCache: true, preview: true, labelContour: 2 })
  })

  it('drops an empty payload rather than sending an empty map', () => {
    const b = labelsRequestBody({ labels: {}, branchLabels: { A: ['b'] }, show: true })
    expect(b).not.toHaveProperty('allLabels')
    expect(b).toHaveProperty('allBranchLabels')
  })
})

// Only the pure helper is tested here; the fetch-backed capture/build functions are exercised live.
describe('unionViewSnapshot', () => {
  it('includes a layer visible in ANY keyframe, keeping a colormap from where it is shown', () => {
    const u = unionViewSnapshot([
      { layers: { gBT: { colormap: 'green', visible: true }, SHG: { colormap: 'gray', visible: false } } },
      { layers: { SHG: { colormap: 'gray', visible: true }, '(flow) segA/tcells': { visible: true } } },
    ])
    const layers = u.layers as Record<string, { colormap?: string; visible?: boolean }>
    expect(layers.gBT).toMatchObject({ visible: true, colormap: 'green' })
    expect(layers.SHG).toMatchObject({ visible: true, colormap: 'gray' })   // shown in kf2 → included
    expect(layers['(flow) segA/tcells'].visible).toBe(true)                 // overlay layer carried through
  })

  it('a layer hidden in every keyframe stays not-visible', () => {
    const u = unionViewSnapshot([
      { layers: { X: { colormap: 'red', visible: false } } },
      { layers: { X: { colormap: 'red', visible: false } } },
    ])
    expect((u.layers as Record<string, { visible?: boolean }>).X.visible).toBe(false)
  })

  it('tolerates empty / missing snapshots', () => {
    expect(unionViewSnapshot([undefined, null, {}]).layers).toEqual({})
  })
})

// The live-slider push tests (`pushZView` / `pushLabelContour` coalescing) were retired with the
// napari mirror they exercised: those knobs now flow only through the shared bag the WebGPU viewer
// subscribes to, so there is no HTTP call to count. Kept the file for the pure-helper tests above.
