import { describe, it, expect } from 'vitest'
import {
  ROLE_OFF, ROLE_PICK, ROLE_FOCUS, MAX_PICK_LUT_IDS,
  buildPickOutlineLUT, pickOutlineLUTsEqual, pickOutlineRoleAt,
} from './pickOutlineLUT'

describe('buildPickOutlineLUT', () => {
  it('marks pick ids as PICK and everything else OFF', () => {
    const lut = buildPickOutlineLUT({ pickIds: [3, 7, 12], focusId: null, maxId: 15 })
    expect(lut.length).toBe(16)
    expect(lut[3]).toBe(ROLE_PICK)
    expect(lut[7]).toBe(ROLE_PICK)
    expect(lut[12]).toBe(ROLE_PICK)
    for (const i of [0, 1, 2, 4, 5, 6, 8, 9, 10, 11, 13, 14, 15]) expect(lut[i]).toBe(ROLE_OFF)
  })

  it('focus id gets FOCUS and beats PICK when both cover the same id', () => {
    const lut = buildPickOutlineLUT({ pickIds: [1, 2, 3], focusId: 2, maxId: 5 })
    expect(lut[1]).toBe(ROLE_PICK)
    expect(lut[2]).toBe(ROLE_FOCUS)
    expect(lut[3]).toBe(ROLE_PICK)
  })

  it('background id (0) stays OFF even if named', () => {
    const lut = buildPickOutlineLUT({ pickIds: [0, 1], focusId: 0, maxId: 3 })
    expect(lut[0]).toBe(ROLE_OFF)
    expect(lut[1]).toBe(ROLE_PICK)
  })

  it('ids past maxId are silently dropped, not thrown', () => {
    const lut = buildPickOutlineLUT({ pickIds: [3, 99, 100], focusId: 500, maxId: 5 })
    expect(lut.length).toBe(6)
    expect(lut[3]).toBe(ROLE_PICK)
    // 99, 100, 500 never landed anywhere — the LUT is still a clean, no-op-outside-the-range one.
    expect(Array.from(lut)).toEqual([0, 0, 0, 1, 0, 0])
  })

  it('empty pick + null focus → all OFF, length = maxId + 1', () => {
    const lut = buildPickOutlineLUT({ pickIds: [], focusId: null, maxId: 4 })
    expect(lut.length).toBe(5)
    for (const v of lut) expect(v).toBe(ROLE_OFF)
  })

  it('negative / zero maxId → single OFF entry (background only)', () => {
    for (const m of [-1, 0, -1000]) {
      const lut = buildPickOutlineLUT({ pickIds: [1], focusId: 2, maxId: m })
      expect(lut.length).toBe(1)
      expect(lut[0]).toBe(ROLE_OFF)
    }
  })

  it('fractional ids floor down (defensive — h5ad reads should be integer, but ids from JSON are floats)', () => {
    const lut = buildPickOutlineLUT({ pickIds: [3.7, 5.2], focusId: 4.9, maxId: 6 })
    expect(lut[3]).toBe(ROLE_PICK)          // 3.7 → 3
    expect(lut[4]).toBe(ROLE_FOCUS)         // 4.9 → 4
    expect(lut[5]).toBe(ROLE_PICK)          // 5.2 → 5
  })

  it('duplicate pick ids are idempotent', () => {
    const lut = buildPickOutlineLUT({ pickIds: [3, 3, 3, 3], focusId: null, maxId: 5 })
    expect(lut[3]).toBe(ROLE_PICK)
    expect(Array.from(lut)).toEqual([0, 0, 0, 1, 0, 0])
  })

  it('accepts any Iterable — Set, generator, array', () => {
    const set = new Set([2, 4])
    function* gen() { yield 1; yield 3 }
    expect(buildPickOutlineLUT({ pickIds: set, focusId: null, maxId: 5 })[2]).toBe(ROLE_PICK)
    expect(buildPickOutlineLUT({ pickIds: gen(), focusId: null, maxId: 5 })[1]).toBe(ROLE_PICK)
  })

  it('caps maxId at MAX_PICK_LUT_IDS - 1', () => {
    const lut = buildPickOutlineLUT({ pickIds: [10], focusId: null, maxId: 10_000_000 })
    expect(lut.length).toBe(MAX_PICK_LUT_IDS)
    expect(lut[10]).toBe(ROLE_PICK)
  })

  it('ids just past the cap are dropped rather than crashing', () => {
    const lut = buildPickOutlineLUT({
      pickIds: [MAX_PICK_LUT_IDS - 1, MAX_PICK_LUT_IDS, MAX_PICK_LUT_IDS + 100],
      focusId: MAX_PICK_LUT_IDS + 1,
      maxId: 10_000_000,
    })
    expect(lut[MAX_PICK_LUT_IDS - 1]).toBe(ROLE_PICK)
    // No IndexError, no throw — the past-cap ids simply didn't land anywhere.
    expect(lut.length).toBe(MAX_PICK_LUT_IDS)
  })
})

describe('pickOutlineLUTsEqual', () => {
  it('true for byte-identical LUTs', () => {
    const a = buildPickOutlineLUT({ pickIds: [1, 3], focusId: 2, maxId: 5 })
    const b = buildPickOutlineLUT({ pickIds: [1, 3], focusId: 2, maxId: 5 })
    expect(pickOutlineLUTsEqual(a, b)).toBe(true)
  })

  it('false when a byte differs', () => {
    const a = buildPickOutlineLUT({ pickIds: [1, 3], focusId: 2, maxId: 5 })
    const b = buildPickOutlineLUT({ pickIds: [1, 3], focusId: 4, maxId: 5 })
    expect(pickOutlineLUTsEqual(a, b)).toBe(false)
  })

  it('false on differing lengths (maxId shrank)', () => {
    const a = buildPickOutlineLUT({ pickIds: [1], focusId: null, maxId: 10 })
    const b = buildPickOutlineLUT({ pickIds: [1], focusId: null, maxId: 5 })
    expect(pickOutlineLUTsEqual(a, b)).toBe(false)
  })

  it('true for two empty LUTs', () => {
    expect(pickOutlineLUTsEqual(new Uint8Array(0), new Uint8Array(0))).toBe(true)
  })
})

describe('pickOutlineRoleAt', () => {
  const lut = buildPickOutlineLUT({ pickIds: [1, 4], focusId: 3, maxId: 5 })

  it('reads roles in range', () => {
    expect(pickOutlineRoleAt(lut, 1)).toBe(ROLE_PICK)
    expect(pickOutlineRoleAt(lut, 3)).toBe(ROLE_FOCUS)
    expect(pickOutlineRoleAt(lut, 4)).toBe(ROLE_PICK)
    expect(pickOutlineRoleAt(lut, 2)).toBe(ROLE_OFF)
  })

  it('out-of-range reads as OFF (matches the shader out-of-range rule)', () => {
    expect(pickOutlineRoleAt(lut, -1)).toBe(ROLE_OFF)
    expect(pickOutlineRoleAt(lut, 0)).toBe(ROLE_OFF)          // background never picked
    expect(pickOutlineRoleAt(lut, 999)).toBe(ROLE_OFF)
  })

  it('fractional id floors down', () => {
    expect(pickOutlineRoleAt(lut, 1.7)).toBe(ROLE_PICK)
    expect(pickOutlineRoleAt(lut, 3.99)).toBe(ROLE_FOCUS)
  })
})
