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

  // The whole 16-plane reply is one websocket frame, and this panel's original failure was blowing
  // through that cap at 16 MiB. The cap is now 64 MiB and the reply is a PALETTE PNG rather than an
  // expanded RGB one, so the bound moved a long way out: re-measured, the whole reply is ~13 B/px on
  // real metric planes and ~22 on synthetic noise, against ~32 and ~67 before. The pessimistic figure
  // is the one to bound with, since how well a metric plane compresses is a property of the movie.
  //
  // So the transport no longer decides the top of the list — 768 is now a TIME and legibility choice
  // (~1.8 s per scrub, drawn at ~180 px per cell; see FLOW_INSPECT_MAX_PX). Widening it is a UX call,
  // not a transport one — but this stays a real bound, and it is what excludes a 2048 px option.
  it('stays inside the websocket frame cap', () => {
    const NOISY_BYTES_PER_PX = 22
    const WS_MAX_FRAME_BYTES = 64 * 1024 * 1024
    for (const px of FLOW_REGION_OPTIONS)
      expect(px * px * NOISY_BYTES_PER_PX).toBeLessThan(WS_MAX_FRAME_BYTES)
    expect(2048 * 2048 * NOISY_BYTES_PER_PX).toBeGreaterThan(WS_MAX_FRAME_BYTES)
  })
})
