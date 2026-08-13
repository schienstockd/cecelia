import { describe, expect, it } from 'vitest'
import {
  DEFAULT_FLOW_REGION_PX,
  FLOW_REGION_OPTIONS,
  flowRegionLabel,
} from './flowRegion'

describe('flowRegionLabel', () => {
  it('reports the crop the worker actually rendered', () => {
    expect(flowRegionLabel({ X: [269, 781], Y: [267, 779], Z: [18, 19], T: [0, 1] }))
      .toBe('512 × 512')
  })

  // The whole point of reading it back from the reply: a 418 px image cannot honour a 512 px request,
  // and the label has to say 418 rather than repeat the chip.
  it('reports the whole frame when the image is smaller than the size asked for', () => {
    expect(flowRegionLabel({ X: [0, 434], Y: [0, 418] })).toBe('434 × 418')
  })

  it('says nothing when there is no region yet', () => {
    expect(flowRegionLabel(null)).toBe('')
    expect(flowRegionLabel(undefined)).toBe('')
    expect(flowRegionLabel({})).toBe('')
    expect(flowRegionLabel({ X: [0, 0], Y: [0, 512] })).toBe('')
    expect(flowRegionLabel({ X: [7], Y: [0, 512] })).toBe('')
  })
})

describe('the offered sizes', () => {
  it('offers the default', () => {
    expect(FLOW_REGION_OPTIONS).toContain(DEFAULT_FLOW_REGION_PX)
  })

  // The cap is a TRANSPORT bound, not a preference: the whole 16-plane reply is one websocket frame,
  // and 1024 px of noisy planes would exceed the 64 MiB cap — which is how this panel failed in the
  // first place, at 16 MiB. A wider option needs that number raised in the same change.
  //
  // Rate is per pixel of the WHOLE reply: 36.3 MB over 1044x1102 measured on real metric planes is
  // ~32 B/px, and synthetic noise renders about twice as poorly. The pessimistic figure is the one to
  // bound with, since how well a metric plane compresses is a property of the movie.
  it('stays inside the websocket frame cap', () => {
    const NOISY_BYTES_PER_PX = 67
    const WS_MAX_FRAME_BYTES = 64 * 1024 * 1024
    for (const px of FLOW_REGION_OPTIONS)
      expect(px * px * NOISY_BYTES_PER_PX).toBeLessThan(WS_MAX_FRAME_BYTES)
    // ...and the next size up is what it excludes, so this is a real bound rather than a tautology
    expect(1024 * 1024 * NOISY_BYTES_PER_PX).toBeGreaterThan(WS_MAX_FRAME_BYTES)
  })
})
