// S1 of WEBGPU_MULTI_ATLAS_SHADER_VARIANTS_PLAN.md — makeBrickShader factory.
//
// S0 (PR #941) proved the WGSL shape parses + samples correctly on Chromium/Dawn. These tests
// pin the JS-side factory contract: binding numbers per N, arm count per N, and that N=1 is
// literally byte-identical to today's shader (Decision 2 — the common path pays nothing).

import { describe, it, expect } from 'vitest'
import {
  makeBrickShader,
  BRICK_WGSL,
  BRICK_PICK_BINDING,
  BRICK_N1_BINDINGS,
  BRICK_MAX_ATLASES,
} from './brickShader'

describe('makeBrickShader — bindings per N', () => {
  it('N=1 keeps the N=1 slot map (uniform 0, pt 1, atlas 2, prevPt 3, lut 4, labAtlas 5, pal 6, pick 7)', () => {
    const v = makeBrickShader({ nAtlases: 1 })
    expect(v.bindings).toEqual({
      uniform: 0, pt: 1, atlas: [2], prevPt: 3, lut: 4, labAtlas: 5, pal: 6, pick: BRICK_PICK_BINDING,
    })
    expect(v.bindings).toEqual(BRICK_N1_BINDINGS)
  })

  it('atlas bindings occupy 2..2+N-1 for every N; downstream bindings shift by N-1', () => {
    for (let n = 1; n <= BRICK_MAX_ATLASES; n++) {
      const v = makeBrickShader({ nAtlases: n })
      expect(v.bindings.atlas).toHaveLength(n)
      expect(v.bindings.atlas[0]).toBe(2)
      expect(v.bindings.atlas[n - 1]).toBe(2 + n - 1)
      expect(v.bindings.prevPt).toBe(2 + n)
      expect(v.bindings.lut).toBe(2 + n + 1)
      expect(v.bindings.labAtlas).toBe(2 + n + 2)
      expect(v.bindings.pal).toBe(2 + n + 3)
      expect(v.bindings.pick).toBe(2 + n + 4)
    }
  })

  it('binding numbers are pairwise distinct at every N (no double-binding)', () => {
    for (let n = 1; n <= BRICK_MAX_ATLASES; n++) {
      const b = makeBrickShader({ nAtlases: n }).bindings
      const all = [b.uniform, b.pt, ...b.atlas, b.prevPt, b.lut, b.labAtlas, b.pal, b.pick]
      expect(new Set(all).size).toBe(all.length)
    }
  })

  it('rejects nAtlases outside 1..MAX_ATLASES', () => {
    expect(() => makeBrickShader({ nAtlases: 0 })).toThrow()
    expect(() => makeBrickShader({ nAtlases: BRICK_MAX_ATLASES + 1 })).toThrow()
  })
})

describe('makeBrickShader — code shape per N', () => {
  it('N=1 returns the existing BRICK_WGSL literal byte-for-byte (Decision 2 byte-identity)', () => {
    // The common path pays nothing: N=1 is literally today's shader, no factory-induced drift.
    // If this test ever fails, the factory has grown a code path that reformats BRICK_WGSL and
    // needs to be reined back in — do NOT update the snapshot without checking that the
    // resulting shader still parses on Dawn (S0 §I only proved the multi-atlas variants).
    const v = makeBrickShader({ nAtlases: 1 })
    expect(v.code).toBe(BRICK_WGSL)
  })

  it('N>=2 declares exactly N atlas texture bindings', () => {
    for (let n = 2; n <= BRICK_MAX_ATLASES; n++) {
      const code = makeBrickShader({ nAtlases: n }).code
      const bindings = code.match(/var atlas\d+: texture_3d<u32>/g) ?? []
      expect(bindings).toHaveLength(n)
    }
  })

  it('N>=2 emits N-1 case arms + one default (switch is exhaustive)', () => {
    for (let n = 2; n <= BRICK_MAX_ATLASES; n++) {
      const code = makeBrickShader({ nAtlases: n }).code
      const cases = code.match(/case \d+u:/g) ?? []
      const defaults = code.match(/default:/g) ?? []
      expect(cases).toHaveLength(n - 1)
      expect(defaults).toHaveLength(1)
    }
  })

  it('N>=2 references every atlas index in the switch (no orphan bindings)', () => {
    for (let n = 2; n <= BRICK_MAX_ATLASES; n++) {
      const code = makeBrickShader({ nAtlases: n }).code
      for (let i = 0; i < n; i++) {
        expect(code).toContain(`textureLoad(atlas${i}, coord, 0)`)
      }
    }
  })

  it('N>=2 uses the shifted binding numbers for prevPt/lut/labAtlas/pal/pick', () => {
    for (let n = 2; n <= BRICK_MAX_ATLASES; n++) {
      const v = makeBrickShader({ nAtlases: n })
      expect(v.code).toContain(`@binding(${v.bindings.prevPt}) var<storage, read> prevPt`)
      expect(v.code).toContain(`@binding(${v.bindings.lut}) var lut`)
      expect(v.code).toContain(`@binding(${v.bindings.labAtlas}) var labAtlas`)
      expect(v.code).toContain(`@binding(${v.bindings.pal}) var pal`)
      // pickBufferWgsl emits its own @binding line — check the number appears.
      expect(v.code).toMatch(new RegExp(`@binding\\(${v.bindings.pick}\\)`))
    }
  })
})
