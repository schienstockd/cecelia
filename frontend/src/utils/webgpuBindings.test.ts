import { describe, it, expect } from 'vitest'
import { bindingDecls, functions, bindingStages, layoutVisibility, visibilityGaps } from './webgpuBindings'
import { MIP_WGSL, POINTS_WGSL, SEGMENTS_WGSL } from '../lib/webgpu/mipShader'

const RENDERER = import.meta.glob('/src/lib/webgpu/volumeRenderer.ts',
  { query: '?raw', import: 'default', eager: true }) as Record<string, string>

describe('bindingDecls', () => {
  it('reads both the uniform and the textures', () => {
    const d = bindingDecls(`
      @group(0) @binding(0) var<uniform> p: P;
      @group(0) @binding(1) var vol: texture_3d<u32>;
    `)
    expect(d.get(0)).toBe('p')
    expect(d.get(1)).toBe('vol')
  })
})

describe('functions', () => {
  it('finds the stage attribute and stops at the matching brace', () => {
    const fns = functions(`
      fn helper() -> f32 { if (true) { return 1.0; } return 0.0; }
      @vertex fn vs() -> vec4<f32> { return vec4(helper()); }
    `)
    expect(fns.map(f => [f.name, f.stage])).toEqual([['helper', null], ['vs', 'vertex']])
    expect(fns[0].body).toContain('return 0.0;')
    expect(fns[0].body.endsWith('}')).toBe(true)
  })
})

describe('bindingStages', () => {
  // the whole point: no entry point mentions `p` — `camera()` does, and it is called from both.
  it('follows calls, so a helper lends its bindings to its callers', () => {
    const stages = bindingStages(`
      @group(0) @binding(0) var<uniform> p: P;
      fn camera() -> f32 { return p.cam.x; }
      fn wrap() -> f32 { return camera(); }
      @vertex fn vs() -> vec4<f32> { return vec4(wrap()); }
      @fragment fn fs() -> vec4<f32> { return vec4(0.0); }
    `)
    expect([...stages.get(0)!]).toEqual(['vertex'])
  })
  it('does not match a binding name inside a longer identifier or a field', () => {
    const stages = bindingStages(`
      @group(0) @binding(1) var vol: texture_3d<u32>;
      @fragment fn fs() -> vec4<f32> { let volume = 1.0; let a = x.vol; return vec4(volume); }
    `)
    expect(stages.has(1)).toBe(false)
  })
})

describe('layoutVisibility', () => {
  it('parses an entry that wraps across lines', () => {
    const v = layoutVisibility(`
      { binding: 0, visibility: GPUShaderStage.VERTEX | GPUShaderStage.FRAGMENT,
        buffer: { type: 'uniform' } },
      { binding: 1, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'uint' } },
    `)
    expect([...v.get(0)!].sort()).toEqual(['fragment', 'vertex'])
    expect([...v.get(1)!]).toEqual(['fragment'])
  })
})

describe('visibilityGaps', () => {
  it('names the gap that black-screens the viewer', () => {
    const wgsl = `
      @group(0) @binding(0) var<uniform> p: P;
      fn camera() -> f32 { return p.cam.x; }
      @vertex fn vs() -> vec4<f32> { return vec4(camera()); }
    `
    const layout = `{ binding: 0, visibility: GPUShaderStage.FRAGMENT, buffer: {} },`
    expect(visibilityGaps([wgsl], layout))
      .toEqual(['binding 0 is used in the vertex stage but not visible to it'])
  })

  // The ratchet. Every pipeline in the viewer shares ONE bind group layout, so the layout has to cover
  // the union of what the three shaders need. A gap here is not a lint failure — it is a render pass
  // that cannot be created, and the volume draws in that same pass.
  it('the real shaders and the real layout agree', () => {
    const ts = Object.values(RENDERER)[0]
    expect(ts).toBeTruthy()
    expect(ts).toContain('createBindGroupLayout')
    expect(visibilityGaps([MIP_WGSL, POINTS_WGSL, SEGMENTS_WGSL], ts)).toEqual([])
  })
})
