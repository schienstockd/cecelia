import { describe, expect, it } from 'vitest'
import GOLDEN from './shaders/golden.json'
import UNIFORMS from './shaders/uniforms.json'
import { expandSource, expandWgsl, packUniforms, SHADER_CONSTANTS, uniformLayout, type WgslVars } from './shaderSource'
import { MIP_WGSL, POINTS_WGSL, SEGMENTS_WGSL } from './mipShader'
import { TILE_WGSL } from './tileShader'
import { BRICK_WGSL, BRICK_POINTS_WGSL, BRICK_SEGMENTS_WGSL, makeBrickShader } from './brickShader'
import { MAX_CHANNELS, LUT_STOPS, VIEW_HALF_ANGLE } from '../../utils/volumeViewer'
import { PICK_BITSET_CAPACITY, PICK_BITSET_WORDS, labelPaletteBytes, LABEL_PALETTE_N } from '../../utils/viewerLabels'
import { lutTextureBytes, type ViewerChannel, type ViewerMeta } from '../../utils/volumeViewer'
import { applyViewStateToBrowser, type ViewerViewState } from '../../utils/viewer/viewState'
import LABEL_PALETTE from './shaders/label_palette.json'

interface ExpandCase { name: string; files: Record<string, string>; vars: WgslVars; out?: string; error?: boolean }

// The same cases `python/cecelia/tests/test_wgsl_utils.py` runs: passing on both sides is what
// makes the browser's shader text and the movie host's the same text.
describe('expandSource — the shared golden', () => {
  for (const c of GOLDEN.expand as unknown as ExpandCase[]) {
    it(c.name, () => {
      const read = (f: string) => {
        if (!(f in c.files)) throw new Error(`no ${f}`)
        return c.files[f]
      }
      if (c.error) expect(() => expandSource('main.wgsl', c.vars, read)).toThrow()
      else expect(expandSource('main.wgsl', c.vars, read)).toBe(c.out)
    })
  }
})

describe('uniform layouts — the shared golden', () => {
  for (const [name, g] of Object.entries(GOLDEN.layout)) {
    it(`${name}: size and hand-derived slots`, () => {
      const L = uniformLayout(name)
      expect(L.floats).toBe(g.floats)
      expect(L.bytes).toBe(g.floats * 4)
      const u = packUniforms(name, Object.fromEntries(Object.keys(g.slots).map((k, i) => [k, i + 1])))
      Object.values(g.slots).forEach((slot, i) => expect(u[slot]).toBe(i + 1))
      expect(u.reduce((n, v) => n + (v !== 0 ? 1 : 0), 0)).toBe(Object.keys(g.slots).length)
    })
  }
  for (const [name, lanes] of Object.entries(GOLDEN.badLanes)) {
    for (const lane of lanes) {
      it(`${name}: '${lane}' is rejected`, () => {
        expect(() => packUniforms(name, { [lane]: 1 })).toThrow()
      })
    }
  }
})

/** `name: type` per member of `struct <name> { … };` in expanded WGSL. */
function structMembers(wgsl: string, struct: string): { name: string; type: string }[] {
  const m = new RegExp(`struct\\s+${struct}\\s*\\{([\\s\\S]*?)\\};`).exec(wgsl)
  if (!m) throw new Error(`no struct ${struct}`)
  return m[1].split('\n')
    .map(l => l.replace(/\/\/.*$/, '').trim())
    .filter(Boolean)
    .map(l => {
      const mm = /^(\w+)\s*:\s*(.+?),?$/.exec(l)
      if (!mm) throw new Error(`unparsed struct line '${l}'`)
      return { name: mm[1], type: mm[2].replace(/\s+/g, '') }
    })
}

describe('uniforms.json matches the shader structs', () => {
  for (const name of Object.keys(UNIFORMS)) {
    it(`${name}: same fields, same order, same types`, () => {
      const L = uniformLayout(name)
      const members = structMembers(expandWgsl(L.file), L.struct)
      const want = L.fields.map(f => ({
        name: f.name,
        type: f.count ? `array<vec4<f32>,${SHADER_CONSTANTS[f.count]}>` : 'vec4<f32>',
      }))
      expect(members).toEqual(want)
    })
  }
})

describe('the shaders the renderers compile', () => {
  const brick = [1, 2, 3, 4].map(n => makeBrickShader({ nAtlases: n }).code)
  const all = { MIP_WGSL, POINTS_WGSL, SEGMENTS_WGSL, TILE_WGSL, BRICK_WGSL, BRICK_POINTS_WGSL, BRICK_SEGMENTS_WGSL,
                BRICK_N2: brick[1], BRICK_N3: brick[2], BRICK_N4: brick[3] }
  for (const [k, code] of Object.entries(all)) {
    it(`${k} has nothing left to expand`, () => {
      expect(code).not.toMatch(/\$\{/)
      expect(code).not.toMatch(/^\s*#include/m)
    })
  }

  it('the TS constants are the shader constants', () => {
    expect([MAX_CHANNELS, LUT_STOPS, VIEW_HALF_ANGLE, PICK_BITSET_CAPACITY, PICK_BITSET_WORDS]).toEqual([
      SHADER_CONSTANTS.MAX_CHANNELS, SHADER_CONSTANTS.LUT_STOPS, SHADER_CONSTANTS.VIEW_HALF_ANGLE,
      SHADER_CONSTANTS.PICK_BITSET_CAPACITY, SHADER_CONSTANTS.PICK_BITSET_WORDS,
    ])
    expect(PICK_BITSET_WORDS * 32).toBe(PICK_BITSET_CAPACITY)
  })

  it('every constant is a plain number both hosts print the same way', () => {
    for (const v of Object.values(SHADER_CONSTANTS)) {
      expect(typeof v).toBe('number')
      expect(String(v)).toMatch(/^\d+(\.\d+)?$/)
    }
  })
})

// What the movie host (`python/cecelia/utils/wgpu_host.py`) must compute the same way the viewer does:
// the camera a view state applies to, the LUT rows, the label palette. Same golden, both sides.
interface CameraCase {
  name: string; camera: { zoom: number; center: number[]; angles: number[] }; snapH: number | null
  canvasH: number; nX: number; nY: number; voxelUm: number[]; halfAngle: number
  want: { dist: number; panX: number; panY: number; yaw: number; pitch: number }
}

describe('the host-side inputs — the shared golden', () => {
  for (const c of GOLDEN.viewCamera as unknown as CameraCase[]) {
    it(`camera: ${c.name}`, () => {
      const vs = { camera: { ...c.camera, perspective: 0 }, dims: { ndisplay: 3, current_step: [0, 0] }, layers: {},
                   ...(c.snapH ? { canvas: { width: 1, height: c.snapH } } : {}) } as unknown as ViewerViewState
      const meta = { nX: c.nX, nY: c.nY, nZ: 1, voxelUm: c.voxelUm, channels: [] } as unknown as ViewerMeta
      const { cam } = applyViewStateToBrowser({
        vs, meta, currentCam: { yaw: 0, pitch: 0, dist: 1, panX: 0, panY: 0 },
        canvasH: c.canvasH, viewHalfAngle: c.halfAngle,
      })
      for (const k of ['dist', 'panX', 'panY', 'yaw', 'pitch'] as const) expect(cam[k]).toBeCloseTo(c.want[k], 9)
    })
  }

  it('LUT rows', () => {
    const channels = GOLDEN.lut.luts.map(lut => ({ lut }) as unknown as ViewerChannel)
    const bytes = lutTextureBytes(channels)
    for (const [c, stops] of Object.entries(GOLDEN.lut.rows)) {
      for (const [i, want] of Object.entries(stops as Record<string, number[]>)) {
        const o = (Number(c) * LUT_STOPS + Number(i)) * 4
        expect(Array.from(bytes.slice(o, o + 4))).toEqual(want)
      }
    }
  })

  it('label palette = shaders/label_palette.json', () => {
    const bytes = labelPaletteBytes()
    expect(LABEL_PALETTE.rgb.length).toBe(LABEL_PALETTE_N)
    LABEL_PALETTE.rgb.forEach((rgb, i) => expect(Array.from(bytes.slice(i * 4, i * 4 + 4))).toEqual([...rgb, 255]))
  })
})
