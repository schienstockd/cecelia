import { describe, it, expect } from 'vitest'
import { unionViewSnapshot } from './napariOverlays'

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
