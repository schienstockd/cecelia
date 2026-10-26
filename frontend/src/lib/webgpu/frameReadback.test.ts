import { describe, it, expect } from 'vitest'
import { paddedRowBytes, packRows, ROW_ALIGN } from './frameReadback'

describe('paddedRowBytes', () => {
  it('rounds a row up to the 256-byte copy alignment', () => {
    expect(paddedRowBytes(64)).toBe(256)       // 256 bytes exactly
    expect(paddedRowBytes(65)).toBe(512)
    expect(paddedRowBytes(1)).toBe(ROW_ALIGN)
    expect(paddedRowBytes(1366)).toBe(5632)    // a real laptop canvas width: 5464 → 5632
  })
})

describe('packRows', () => {
  // 3×2 frame, stride 16 (12 bytes of pixels + 4 of padding per row); pixel (x, y) = [x, y, 9, 255]
  const w = 3, h = 2, stride = 16
  const src = new Uint8Array(stride * h).fill(77)            // padding bytes must not leak through
  for (let y = 0; y < h; y++) for (let x = 0; x < w; x++) src.set([x, y, 9, 255], y * stride + x * 4)

  it('drops the row padding', () => {
    const out = packRows(src, w, h, stride, false)
    expect(out.length).toBe(w * h * 4)
    expect(Array.from(out.subarray(0, 4))).toEqual([0, 0, 9, 255])
    expect(Array.from(out.subarray(20, 24))).toEqual([2, 1, 9, 255])   // last pixel of row 1
    expect(out.includes(77)).toBe(false)
  })

  it('swaps R and B for a bgra target, leaving G and A', () => {
    const out = packRows(src, w, h, stride, true)
    expect(Array.from(out.subarray(4, 8))).toEqual([9, 0, 1, 255])     // (1, 0): [1,0,9] read as BGR
  })
})

