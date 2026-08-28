import { describe, it, expect } from 'vitest'
import {
  parseBrickSlabShape, brickBounds, brickSlabQuery, brickSlabUrl, brickShapeError,
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
})
