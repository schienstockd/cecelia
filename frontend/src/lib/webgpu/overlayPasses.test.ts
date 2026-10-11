import { describe, it, expect } from 'vitest'
import { POINT_VERTEX_LAYOUT, SEG_VERTEX_LAYOUT } from './overlayPasses'
import { POINT_STRIDE, SEG_STRIDE } from '../../utils/viewerOverlays'

const BYTES: Record<string, number> = { float32: 4, float32x2: 8, float32x3: 12, float32x4: 16 }

// The instance layouts every renderer's overlay pipeline uses must tile the buffers `buildPointBuffer` /
// `buildTrackBuffer` write: contiguous attributes, locations 0..n-1, exactly one stride.
describe('overlay instance layouts', () => {
  for (const [name, layout, stride] of [['points', POINT_VERTEX_LAYOUT, POINT_STRIDE],
                                        ['segments', SEG_VERTEX_LAYOUT, SEG_STRIDE]] as const) {
    it(`${name}: contiguous attributes filling one ${stride}-float stride`, () => {
      expect(layout.arrayStride).toBe(stride * 4)
      expect(layout.stepMode).toBe('instance')
      let at = 0
      ;[...layout.attributes].forEach((a, i) => {
        expect(a.shaderLocation).toBe(i)
        expect(a.offset).toBe(at)
        at += BYTES[a.format]
      })
      expect(at).toBe(stride * 4)
    })
  }
})
