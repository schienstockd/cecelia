import { describe, it, expect } from 'vitest'
import { unionViewSnapshot } from './titleCard'

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

  it("keeps a track layer's colour from a keyframe where it is shown, not a later hidden one", () => {
    const tr = '(flow) (segA) Tracks tcells'
    const u = unionViewSnapshot([
      { layers: { [tr]: { visible: true, colour: '#ff0000' } } },
      { layers: { [tr]: { visible: false, colour: null } } },
    ])
    expect(u.layers[tr]).toMatchObject({ visible: true, colour: '#ff0000' })
    // null ("by track") from a shown keyframe wins over a colour from a hidden one
    const v = unionViewSnapshot([
      { layers: { [tr]: { visible: false, colour: '#00ff00' } } },
      { layers: { [tr]: { visible: true, colour: null } } },
      { layers: { [tr]: { visible: false, colour: '#0000ff' } } },
    ])
    expect((v.layers[tr] as { colour?: unknown }).colour).toBeNull()
  })

  it('tolerates empty / missing snapshots', () => {
    expect(unionViewSnapshot([undefined, null, {}]).layers).toEqual({})
  })
})
