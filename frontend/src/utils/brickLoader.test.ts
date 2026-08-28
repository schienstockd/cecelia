import { describe, it, expect } from 'vitest'
import {
  parseBrickSlabShape, brickBounds, brickSlabQuery, brickSlabUrl, brickShapeError,
  padBrickPayload,
} from './brickLoader'

describe('parseBrickSlabShape', () => {
  it('parses the 4-tuple header emitted with cTo', () => {
    expect(parseBrickSlabShape('38,4,128,128')).toEqual({ nc: 38, nz: 4, ny: 128, nx: 128 })
  })

  // A 3-tuple response is the scalar-c path — silently accepting it here would let the atlas
  // upload `nz*ny*nx` bytes into a slot expecting `nc*nz*ny*nx`, drawing a shifted image with
  // no error. This is the exact silent-render trap `slabShapeError` guards against.
  it('rejects the legacy 3-tuple response', () => {
    expect(parseBrickSlabShape('4,128,128')).toBeNull()
  })

  it('rejects unparseable or missing headers', () => {
    expect(parseBrickSlabShape('')).toBeNull()
    expect(parseBrickSlabShape(null)).toBeNull()
    expect(parseBrickSlabShape('a,b,c,d')).toBeNull()
    expect(parseBrickSlabShape('38,-1,128,128')).toBeNull()
    expect(parseBrickSlabShape('38,0,128,128')).toBeNull()
  })
})

describe('brickBounds', () => {
  const brickSize: readonly [number, number, number] = [128, 128, 4]

  it('places brick (0,0,0) at the origin', () => {
    expect(brickBounds({ t: 0, level: 0, bx: 0, by: 0, bz: 0 }, brickSize)).toEqual({
      xLo: 0, xHi: 127, yLo: 0, yHi: 127, zLo: 0, zHi: 3,
    })
  })

  it('walks in brick-unit steps, inclusive bounds', () => {
    expect(brickBounds({ t: 0, level: 0, bx: 2, by: 3, bz: 0 }, brickSize)).toEqual({
      xLo: 256, xHi: 383, yLo: 384, yHi: 511, zLo: 0, zHi: 3,
    })
  })

  // The Julia route clamps against the actual level size, so an edge brick that hangs off the
  // store gets a shorter body back. The BRICK bounds we send are still full-size; the atlas
  // (P4) is responsible for detecting the shorter response and writing the shorter region.
  it('does NOT clamp against the store — server does that', () => {
    expect(brickBounds({ t: 0, level: 0, bx: 100, by: 100, bz: 0 }, brickSize)).toEqual({
      xLo: 12800, xHi: 12927, yLo: 12800, yHi: 12927, zLo: 0, zHi: 3,
    })
  })
})

describe('brickSlabQuery + brickSlabUrl', () => {
  const base = { projectUid: '4rNbMp', imageUid: 'SispLk' }
  const brickSize: readonly [number, number, number] = [128, 128, 4]

  it('always sets c=0 and cTo=nC-1 — one request, all channels', () => {
    const q = brickSlabQuery(base, { t: 0, level: 0, bx: 0, by: 0, bz: 0 }, 38, brickSize)
    expect(q.c).toBe(0)
    expect(q.cTo).toBe(37)
  })

  it('encodes the full URL against /api/viewer/slab', () => {
    const url = brickSlabUrl(base, { t: 3, level: 1, bx: 2, by: 3, bz: 0 }, 25, brickSize)
    // Guard against character-order regressions in the querystring: check every axis is present.
    expect(url).toContain('projectUid=4rNbMp')
    expect(url).toContain('imageUid=SispLk')
    expect(url).toContain('t=3')
    expect(url).toContain('level=1')
    expect(url).toContain('c=0')
    expect(url).toContain('cTo=24')
    expect(url).toContain('x=256')
    expect(url).toContain('xTo=383')
    expect(url).toContain('y=384')
    expect(url).toContain('yTo=511')
    expect(url).toContain('z=0')
    expect(url).toContain('zTo=3')
  })

  it('omits level for L0 (matches the flat atlas convention — cache-key parity)', () => {
    const url = brickSlabUrl(base, { t: 0, level: 0, bx: 0, by: 0, bz: 0 }, 38, brickSize)
    expect(url).not.toContain('level=')
  })
})

describe('brickShapeError', () => {
  const size: readonly [number, number, number] = [128, 128, 4]

  it('passes a good SispLk brick (38 channels, uint8)', () => {
    const bytes = 38 * 4 * 128 * 128 * 1
    expect(brickShapeError('38,4,128,128', bytes, 1, 38, size)).toBeNull()
  })

  it('flags a shape/response mismatch (a store swap left a stale nC in play)', () => {
    const bytes = 38 * 4 * 128 * 128 * 1
    // Server says 25 channels, we asked for 38 → the store swapped mid-request.
    expect(brickShapeError('25,4,128,128', bytes, 1, 38, size))
      .toMatch(/25x4x128x128 \(c,z,y,x\) but 38x4x128x128/)
  })

  it('flags a truncated body (retry, but do not upload)', () => {
    const half = 38 * 4 * 128 * 128 / 2
    expect(brickShapeError('38,4,128,128', half, 1, 38, size)).toMatch(/expected/)
  })

  // The trap that hit the flat atlas in #684 — dtype disagreement between the header and the
  // atlas. If the layout says uint16 but the server delivers uint8, half the bytes upload as
  // nonsense pairs.
  it('flags a dtype × response bytes mismatch', () => {
    const bytes = 38 * 4 * 128 * 128 * 1    // half of what a uint16 layout would expect
    expect(brickShapeError('38,4,128,128', bytes, 2, 38, size)).toMatch(/expected/)
  })

  it('flags an absent or malformed X-Slab-Shape header', () => {
    expect(brickShapeError(null, 0, 1, 38, size)).toMatch(/no 4-tuple/)
    expect(brickShapeError('4,128,128', 0, 1, 38, size)).toMatch(/no 4-tuple/)
  })

  // Edge bricks: the server clamps xTo/yTo at the store boundary (e.g. store nX=441, brick x=384..
  // 511 returns nx=57 — 441-384). The client's atlas slot is fixed at brickX, so we accept the
  // shorter payload and pad; the shader skips oversampled voxels via `vi.x >= p.dims.x`.
  it('accepts a payload with shorter nx/ny — edge brick at the store boundary', () => {
    const bytes = 38 * 4 * 128 * 57 * 1        // 128 y-rows × 57 x-cols
    expect(brickShapeError('38,4,128,57', bytes, 1, 38, size)).toBeNull()
    const bytes2 = 38 * 4 * 36 * 128 * 1
    expect(brickShapeError('38,4,36,128', bytes2, 1, 38, size)).toBeNull()
  })

  it('flags a response LARGER than expected on nx/ny — never a valid clamp', () => {
    const bytes = 38 * 4 * 200 * 128 * 1
    expect(brickShapeError('38,4,200,128', bytes, 1, 38, size)).toMatch(/nx\/ny exceeds/)
  })
})

describe('padBrickPayload', () => {
  const layout: readonly [number, number, number] = [4, 4, 2]     // tiny brick for arithmetic

  it('leaves the interior intact, zero-fills the padded columns', () => {
    // 1 channel × 2 z × 4 y × 2 x uint8, all 0x11. Padded to 1×2×4×4.
    const src = new Uint8Array(1 * 2 * 4 * 2).fill(0x11)
    const out = new Uint8Array(padBrickPayload(src.buffer, { nc: 1, nz: 2, ny: 4, nx: 2 },
                                               layout, 1))
    // Expected: for each z, each y-row: two 0x11 + two 0x00.
    for (let z = 0; z < 2; z++) {
      for (let y = 0; y < 4; y++) {
        const row = out.subarray((z * 4 + y) * 4, (z * 4 + y + 1) * 4)
        expect(Array.from(row)).toEqual([0x11, 0x11, 0x00, 0x00])
      }
    }
  })

  it('respects channel-major ordering under padding', () => {
    // 2 channels; ch0 = 0xAA, ch1 = 0xBB. Actual 1×1×2 (nz=1, ny=1, nx=2). Layout [4, 4, 2]
    // ⇒ padded to 2 × 2 z × 4 y × 4 x.
    const src = new Uint8Array([0xAA, 0xAA, 0xBB, 0xBB])
    const out = new Uint8Array(padBrickPayload(src.buffer, { nc: 2, nz: 1, ny: 1, nx: 2 },
                                               layout, 1))
    // Channel 0 sits at bytes 0..(z*y*x=2*4*4)-1 = 0..31. First row (z=0, y=0): 0xAA, 0xAA, 0, 0.
    expect(Array.from(out.subarray(0, 4))).toEqual([0xAA, 0xAA, 0x00, 0x00])
    // Channel 1 starts at byte 32. First row: 0xBB, 0xBB, 0, 0.
    expect(Array.from(out.subarray(32, 36))).toEqual([0xBB, 0xBB, 0x00, 0x00])
  })
})
