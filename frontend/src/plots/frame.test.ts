import { describe, expect, it } from 'vitest'
import { rectFrame, letterboxFrame, type FrameRect } from './frame'

const R = (left: number, top: number, width: number, height: number): FrameRect =>
  ({ left, top, width, height })

describe('rectFrame', () => {
  const rect = R(100, 50, 200, 100)
  const f = rectFrame(() => rect)

  it('maps a client point to normalised coords', () => {
    expect(f.toNorm(100, 50)).toEqual({ u: 0, v: 0 })
    expect(f.toNorm(300, 150)).toEqual({ u: 1, v: 1 })
    expect(f.toNorm(200, 100)).toEqual({ u: 0.5, v: 0.5 })
  })

  it('returns null for a client point outside the rect', () => {
    expect(f.toNorm(50, 100)).toBeNull()
    expect(f.toNorm(400, 100)).toBeNull()
    expect(f.toNorm(200, 40)).toBeNull()
    expect(f.toNorm(200, 160)).toBeNull()
  })

  it('round-trips fromNorm ↔ toNorm', () => {
    for (const [u, v] of [[0.1, 0.2], [0.5, 0.5], [0.9, 0.05]] as const) {
      const p = f.fromNorm(u, v)!
      expect(f.toNorm(p.clientX, p.clientY)).toEqual({ u, v })
    }
  })

  it('fromNorm does NOT clamp (for off-frame anchors like leader lines)', () => {
    expect(f.fromNorm(-0.1, 0.5)).toEqual({ clientX: 80, clientY: 100 })
    expect(f.fromNorm(1.2, 0.5)).toEqual({ clientX: 340, clientY: 100 })
  })

  it('returns null when the rect getter returns null', () => {
    const empty = rectFrame(() => null)
    expect(empty.toNorm(0, 0)).toBeNull()
    expect(empty.fromNorm(0.5, 0.5)).toBeNull()
  })

  it('returns null on a zero-sized rect (avoids divide-by-zero)', () => {
    const zero = rectFrame(() => R(0, 0, 0, 0))
    expect(zero.toNorm(0, 0)).toBeNull()
    expect(zero.fromNorm(0.5, 0.5)).toBeNull()
  })

  it('exposes no subFrames on a single-cell frame', () => {
    expect(f.subFrames).toBeUndefined()
  })
})

describe('letterboxFrame', () => {
  it('with matching aspect, behaves like rectFrame (no letterbox)', () => {
    const rect = R(0, 0, 100, 50)   // 2:1
    const f = letterboxFrame(() => rect, 2)
    expect(f.fromNorm(0.5, 0.5)).toEqual({ clientX: 50, clientY: 25 })
    expect(f.toNorm(50, 25)).toEqual({ u: 0.5, v: 0.5 })
  })

  it('letterboxes TOP/BOTTOM when natural is wider than container', () => {
    // container 100×100, natural 2:1 → drawn 100×50, centred (top=25, bottom=25)
    const f = letterboxFrame(() => R(0, 0, 100, 100), 2)
    expect(f.fromNorm(0, 0)).toEqual({ clientX: 0, clientY: 25 })
    expect(f.fromNorm(1, 1)).toEqual({ clientX: 100, clientY: 75 })
    expect(f.fromNorm(0.5, 0.5)).toEqual({ clientX: 50, clientY: 50 })
  })

  it('letterboxes LEFT/RIGHT when natural is taller than container', () => {
    // container 100×100, natural 1:2 → drawn 50×100, centred (left=25, right=25)
    const f = letterboxFrame(() => R(0, 0, 100, 100), 0.5)
    expect(f.fromNorm(0, 0)).toEqual({ clientX: 25, clientY: 0 })
    expect(f.fromNorm(1, 1)).toEqual({ clientX: 75, clientY: 100 })
    expect(f.fromNorm(0.5, 0.5)).toEqual({ clientX: 50, clientY: 50 })
  })

  it('toNorm returns null for a client point in the letterbox gutter', () => {
    // container 100×100, natural 2:1 → gutters top y<25 and bottom y>75
    const f = letterboxFrame(() => R(0, 0, 100, 100), 2)
    expect(f.toNorm(50, 10)).toBeNull()
    expect(f.toNorm(50, 90)).toBeNull()
    expect(f.toNorm(50, 25)).toEqual({ u: 0.5, v: 0 })
    expect(f.toNorm(50, 75)).toEqual({ u: 0.5, v: 1 })
  })

  it('with naturalAspect === 0, falls back to full-container semantics', () => {
    const f = letterboxFrame(() => R(0, 0, 100, 100), 0)
    expect(f.fromNorm(0.5, 0.5)).toEqual({ clientX: 50, clientY: 50 })
    expect(f.toNorm(50, 50)).toEqual({ u: 0.5, v: 0.5 })
  })

  it('round-trips fromNorm ↔ toNorm on the drawn area', () => {
    const f = letterboxFrame(() => R(0, 0, 100, 100), 2)   // 100×50 drawn area
    for (const [u, v] of [[0.1, 0.2], [0.5, 0.5], [0.9, 0.9]] as const) {
      const p = f.fromNorm(u, v)!
      expect(f.toNorm(p.clientX, p.clientY)).toEqual({ u, v })
    }
  })

  it('returns null when the rect getter returns null or zero-sized', () => {
    expect(letterboxFrame(() => null, 1).fromNorm(0.5, 0.5)).toBeNull()
    expect(letterboxFrame(() => R(0, 0, 0, 0), 1).fromNorm(0.5, 0.5)).toBeNull()
  })

  it('re-reads a getter aspect on every call (image loads late)', () => {
    let a = 0
    const f = letterboxFrame(() => R(0, 0, 100, 100), () => a)
    expect(f.fromNorm(0.5, 0.5)).toEqual({ clientX: 50, clientY: 50 })  // full rect while a=0
    a = 2
    expect(f.fromNorm(0.5, 0.5)).toEqual({ clientX: 50, clientY: 50 })  // centre unchanged
    expect(f.fromNorm(0, 0)).toEqual({ clientX: 0, clientY: 25 })       // now letterboxed
  })
})
