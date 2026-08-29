import { describe, it, expect } from 'vitest'
import {
  paddedBytesPerRow, paddedPerChannelBytes, stagingBufferBytes,
  sourceBytesPerRow, sourcePerChannelBytes, packStaging,
} from './brickAtlasStaging'

describe('brickAtlasStaging — row stride', () => {
  it('rounds a sub-256 row up to 256', () => {
    expect(paddedBytesPerRow(64, 1)).toBe(256)   // 64 → 256
    expect(paddedBytesPerRow(64, 2)).toBe(256)   // 128 → 256
    expect(paddedBytesPerRow(1, 1)).toBe(256)    // pathological
  })
  it('leaves an already-aligned row alone', () => {
    expect(paddedBytesPerRow(256, 1)).toBe(256)
    expect(paddedBytesPerRow(128, 2)).toBe(256)
    expect(paddedBytesPerRow(512, 1)).toBe(512)
  })
  it('rounds a non-aligned above-256 row up', () => {
    expect(paddedBytesPerRow(200, 2)).toBe(512)   // 400 → 512
    expect(paddedBytesPerRow(300, 1)).toBe(512)   // 300 → 512
  })
})

describe('brickAtlasStaging — buffer sizing', () => {
  it('per channel = paddedRow × by × bz', () => {
    expect(paddedPerChannelBytes(64, 64, 64, 1)).toBe(256 * 64 * 64)
    expect(paddedPerChannelBytes(64, 32, 16, 2)).toBe(256 * 32 * 16)
  })
  it('staging = per channel × nc', () => {
    expect(stagingBufferBytes(64, 64, 64, 1, 4)).toBe(256 * 64 * 64 * 4)
    expect(stagingBufferBytes(64, 64, 64, 2, 25)).toBe(256 * 64 * 64 * 25)
  })
  it('source per channel matches writeBrick doc — bx × by × bz × bpv', () => {
    expect(sourceBytesPerRow(64, 1)).toBe(64)
    expect(sourceBytesPerRow(64, 2)).toBe(128)
    expect(sourcePerChannelBytes(64, 64, 64, 1)).toBe(64 * 64 * 64)
    expect(sourcePerChannelBytes(64, 32, 16, 2)).toBe(64 * 32 * 16 * 2)
  })
})

describe('brickAtlasStaging — packStaging', () => {
  it('fast path — aligned row is one memcpy per channel', () => {
    const bx = 128, by = 2, bz = 2, bpv = 2, nc = 2   // 128*2 = 256, already aligned
    expect(paddedBytesPerRow(bx, bpv)).toBe(sourceBytesPerRow(bx, bpv))
    const source = new Uint8Array(sourcePerChannelBytes(bx, by, bz, bpv) * nc)
    for (let i = 0; i < source.length; i++) source[i] = (i * 31 + 7) & 255
    const dest = new Uint8Array(stagingBufferBytes(bx, by, bz, bpv, nc))
    packStaging(dest, source, bx, by, bz, bpv, nc)
    // Since padded === tight, the whole payload lands identically at offset 0..source.length
    expect(dest.slice(0, source.length)).toEqual(source)
  })
  it('slow path — pads rows to 256 while preserving byte order within a row', () => {
    const bx = 4, by = 2, bz = 1, bpv = 1, nc = 2
    // Source: 4 B/row × 2 rows × 1 z × 2 ch = 16 bytes total
    const source = new Uint8Array([
      1, 2, 3, 4,        // ch 0, row 0
      5, 6, 7, 8,        // ch 0, row 1
      9, 10, 11, 12,     // ch 1, row 0
      13, 14, 15, 16,    // ch 1, row 1
    ])
    const paddedRow = paddedBytesPerRow(bx, bpv)
    expect(paddedRow).toBe(256)
    const chOff = paddedPerChannelBytes(bx, by, bz, bpv)
    const dest = new Uint8Array(stagingBufferBytes(bx, by, bz, bpv, nc))
    packStaging(dest, source, bx, by, bz, bpv, nc)
    // Row 0 ch 0 at offset 0
    expect(Array.from(dest.slice(0, 4))).toEqual([1, 2, 3, 4])
    // Row 1 ch 0 at paddedRow (bytes 4-255 are zeros — the pad)
    expect(Array.from(dest.slice(paddedRow, paddedRow + 4))).toEqual([5, 6, 7, 8])
    // Channel 1 at paddedPerChannelBytes
    expect(Array.from(dest.slice(chOff, chOff + 4))).toEqual([9, 10, 11, 12])
    expect(Array.from(dest.slice(chOff + paddedRow, chOff + paddedRow + 4))).toEqual([13, 14, 15, 16])
    // Sanity: nothing bled into the padded gap on any row
    expect(Array.from(dest.slice(4, paddedRow))).toEqual(Array(paddedRow - 4).fill(0))
  })
  it('slow path — 64×64 r8uint brick (realistic shape) preserves round-trip', () => {
    const bx = 64, by = 64, bz = 4, bpv = 1, nc = 4
    const srcPerCh = sourcePerChannelBytes(bx, by, bz, bpv)
    const source = new Uint8Array(srcPerCh * nc)
    for (let i = 0; i < source.length; i++) source[i] = (i * 13 + 3) & 255
    const dest = new Uint8Array(stagingBufferBytes(bx, by, bz, bpv, nc))
    packStaging(dest, source, bx, by, bz, bpv, nc)
    // Reverse the pack — read one row at a time out of dest and compare to source
    const paddedRow = paddedBytesPerRow(bx, bpv)
    const tightRow = sourceBytesPerRow(bx, bpv)
    const dstPerCh = paddedPerChannelBytes(bx, by, bz, bpv)
    for (let c = 0; c < nc; c++) {
      for (let r = 0; r < by * bz; r++) {
        const srcSlice = source.slice(c * srcPerCh + r * tightRow, c * srcPerCh + (r + 1) * tightRow)
        const dstSlice = dest.slice(c * dstPerCh + r * paddedRow, c * dstPerCh + r * paddedRow + tightRow)
        expect(Array.from(dstSlice)).toEqual(Array.from(srcSlice))
      }
    }
  })
})
