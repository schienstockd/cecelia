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
  it('N=1 keeps today\'s slot map (uniform 0, pt 1, atlas 2, prevPt 3, lut 4, labAtlas 5, pal 6, pick 7)', () => {
    const v = makeBrickShader({ nAtlases: 1 })
    expect(v.bindings).toEqual({
      uniform: 0, pt: 1, atlas: [2], prevPt: 3, lut: 4, labAtlas: [5], pal: 6, pick: BRICK_PICK_BINDING,
    })
    expect(v.bindings).toEqual(BRICK_N1_BINDINGS)
  })

  it('atlas + labAtlas each occupy N contiguous slots; pal/pick shift by 2*(N-1) at N>=2', () => {
    for (let n = 1; n <= BRICK_MAX_ATLASES; n++) {
      const v = makeBrickShader({ nAtlases: n })
      // Intensity atlases at 2..2+N-1.
      expect(v.bindings.atlas).toHaveLength(n)
      expect(v.bindings.atlas[0]).toBe(2)
      expect(v.bindings.atlas[n - 1]).toBe(2 + n - 1)
      expect(v.bindings.prevPt).toBe(2 + n)
      expect(v.bindings.lut).toBe(3 + n)
      // Label atlases at 4+N..4+2N-1 (S2).
      expect(v.bindings.labAtlas).toHaveLength(n)
      expect(v.bindings.labAtlas[0]).toBe(4 + n)
      expect(v.bindings.labAtlas[n - 1]).toBe(4 + 2 * n - 1)
      expect(v.bindings.pal).toBe(4 + 2 * n)
      expect(v.bindings.pick).toBe(5 + 2 * n)
    }
  })

  it('binding numbers are pairwise distinct at every N (no double-binding)', () => {
    for (let n = 1; n <= BRICK_MAX_ATLASES; n++) {
      const b = makeBrickShader({ nAtlases: n }).bindings
      const all = [b.uniform, b.pt, ...b.atlas, b.prevPt, b.lut, ...b.labAtlas, b.pal, b.pick]
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

  it('N>=2 declares exactly N intensity atlas + N label atlas texture bindings', () => {
    for (let n = 2; n <= BRICK_MAX_ATLASES; n++) {
      const code = makeBrickShader({ nAtlases: n }).code
      const atlases = code.match(/var atlas\d+: texture_3d<u32>/g) ?? []
      const labAtlases = code.match(/var labAtlas\d+: texture_3d<u32>/g) ?? []
      expect(atlases).toHaveLength(n)
      expect(labAtlases).toHaveLength(n)
    }
  })

  it('N>=2 emits one switch per sampler — N-1 case arms + one default each', () => {
    for (let n = 2; n <= BRICK_MAX_ATLASES; n++) {
      const code = makeBrickShader({ nAtlases: n }).code
      // Two switches: intensity + label. Each has N-1 case arms and 1 default.
      const cases = code.match(/case \d+u:/g) ?? []
      const defaults = code.match(/default:/g) ?? []
      expect(cases).toHaveLength(2 * (n - 1))
      expect(defaults).toHaveLength(2)
    }
  })

  it('N>=2 references every intensity + label atlas index in its switch (no orphan bindings)', () => {
    for (let n = 2; n <= BRICK_MAX_ATLASES; n++) {
      const code = makeBrickShader({ nAtlases: n }).code
      for (let i = 0; i < n; i++) {
        expect(code).toContain(`textureLoad(atlas${i}, coord, 0)`)
        expect(code).toContain(`textureLoad(labAtlas${i}, coord, 0)`)
      }
    }
  })

  it('N>=2 uses the shifted binding numbers for prevPt/lut/labAtlas/pal/pick', () => {
    for (let n = 2; n <= BRICK_MAX_ATLASES; n++) {
      const v = makeBrickShader({ nAtlases: n })
      expect(v.code).toContain(`@binding(${v.bindings.prevPt}) var<storage, read> prevPt`)
      expect(v.code).toContain(`@binding(${v.bindings.lut}) var lut`)
      for (let i = 0; i < n; i++) {
        expect(v.code).toContain(`@binding(${v.bindings.labAtlas[i]}) var labAtlas${i}`)
      }
      expect(v.code).toContain(`@binding(${v.bindings.pal}) var pal`)
      expect(v.code).toMatch(new RegExp(`@binding\\(${v.bindings.pick}\\)`))
    }
  })
})
