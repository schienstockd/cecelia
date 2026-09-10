import { describe, it, expect } from 'vitest'
import { verdictFrom, classifyAdapter, isAppleAdapter, adapterNameText } from './webgpuProbe'

const noName = { vendor: '', architecture: '', device: '', description: '' }

describe('classifyAdapter', () => {
  it('names an NVIDIA vendor as discrete even when the limit reads integrated', () => {
    // The bug that triggered this: Dawn on Linux Vulkan reports 2048 for an RTX 2000 Ada. Without the
    // name check the app tags it "Integrated". Dominik confirmed via brave://gpu that WebGPU IS on the
    // RTX with a Vulkan backend, so the name is the truth here.
    expect(classifyAdapter({ ...noName, vendor: 'nvidia' }, 2048)).toBe(true)
    expect(classifyAdapter({ ...noName, description: 'NVIDIA RTX 2000 Ada Generation' }, 2048)).toBe(true)
    expect(classifyAdapter({ ...noName, vendor: 'amd' }, 2048)).toBe(true)
    expect(classifyAdapter({ ...noName, device: 'Radeon RX 7900 XTX' }, 2048)).toBe(true)
  })

  it('names an Intel iGPU as integrated even when Mesa reports 16384', () => {
    // The other side of the same bug: Mesa's `iris` on Linux reports 16384 for Intel iGPU (the WebGPU
    // spec's ceiling), which the limit-only proxy read as "discrete" — and then Mesa segfaulted opening
    // an image. The name says otherwise; trust the name.
    expect(classifyAdapter({ ...noName, vendor: 'intel' }, 16384)).toBe(false)
    expect(classifyAdapter({ ...noName, description: 'Intel Iris Xe Graphics' }, 16384)).toBe(false)
  })

  it('names a software rasterizer as integrated regardless of the limit', () => {
    expect(classifyAdapter({ ...noName, description: 'llvmpipe (LLVM 20)' }, 16384)).toBe(false)
    expect(classifyAdapter({ ...noName, description: 'SwiftShader Device' }, 16384)).toBe(false)
    expect(classifyAdapter({ ...noName, description: 'Microsoft Basic Render Driver' }, 16384)).toBe(false)
  })

  it('falls back to the limit when the browser blanks every info field', () => {
    // Firefox has historically blanked adapter.info entirely; the limit is the only tell.
    expect(classifyAdapter(noName, 16384)).toBe(true)
    expect(classifyAdapter(noName, 2048)).toBe(false)
  })

  it('tags Apple as NOT integrated regardless of the limit', () => {
    // The user-facing verdict is "reduced" vs "ready". Apple M-series is unified-memory but comfortably
    // runs the viewer, and Safari's WebGPU reports the spec baseline (2048) rather than the hardware
    // limit — so trusting the limit flipped Apple back into the reduced bucket ("apple apple apple
    // apple / maxTextureDimension3D 2048", Dominik 2026-09-10). Trust the name here; the copy branch
    // in `verdictFrom` renders "Apple GPU — ready" instead of "Discrete GPU detected".
    expect(classifyAdapter({ ...noName, vendor: 'apple' }, 16384)).toBe(true)
    expect(classifyAdapter({ ...noName, vendor: 'apple' }, 2048)).toBe(true)
  })
})

describe('adapterNameText', () => {
  it('joins the non-empty fields with spaces', () => {
    expect(adapterNameText({ vendor: 'nvidia', architecture: 'ampere', device: 'RTX 4090', description: '' }))
      .toBe('nvidia ampere RTX 4090')
  })

  it('collapses consecutive duplicate tokens — Safari fills every field with the same "apple" string', () => {
    expect(adapterNameText({ vendor: 'apple', architecture: 'apple', device: 'apple', description: 'apple' }))
      .toBe('apple')
  })

  it('is case-insensitive when deduping so Apple/apple/APPLE do not stutter', () => {
    expect(adapterNameText({ vendor: 'Apple', architecture: 'apple', device: 'APPLE', description: '' }))
      .toBe('Apple')
  })
})

describe('isAppleAdapter', () => {
  it('matches on any Apple token, regardless of which field it lands in', () => {
    expect(isAppleAdapter({ ...noName, vendor: 'apple' })).toBe(true)
    expect(isAppleAdapter({ ...noName, description: 'Apple M2' })).toBe(true)
    expect(isAppleAdapter(noName)).toBe(false)
    expect(isAppleAdapter({ ...noName, vendor: 'nvidia' })).toBe(false)
  })
})

describe('verdictFrom', () => {
  it('returns unavailable when the browser has no WebGPU', () => {
    const r = verdictFrom({ supported: false, adapterFound: false, looksDiscrete: false, hasR16Uint: null })
    expect(r.verdict).toBe('unavailable')
    expect(r.reason).toMatch(/browser/i)
  })

  it('returns unavailable when no adapter came back', () => {
    // hybrid laptop with the browser routed to a disabled GPU still hits this
    const r = verdictFrom({ supported: true, adapterFound: false, looksDiscrete: false, hasR16Uint: null })
    expect(r.verdict).toBe('unavailable')
    expect(r.reason).toMatch(/adapter/i)
  })

  it('returns unavailable when r16uint is refused — the volume viewer cannot render without it', () => {
    const r = verdictFrom({ supported: true, adapterFound: true, looksDiscrete: true, hasR16Uint: false })
    expect(r.verdict).toBe('unavailable')
    expect(r.reason).toMatch(/r16uint/i)
  })

  it('returns reduced when the integrated GPU was handed back (the ADAPTER TRAP)', () => {
    // maxTextureDimension3D=2048 means looksDiscrete=false. Firefox on this machine has blanked
    // adapter.info, so the limit is the only tell we have.
    const r = verdictFrom({ supported: true, adapterFound: true, looksDiscrete: false, hasR16Uint: true })
    expect(r.verdict).toBe('reduced')
    expect(r.reason).toMatch(/integrated/i)
  })

  it('returns ready on discrete GPU with r16uint', () => {
    const r = verdictFrom({ supported: true, adapterFound: true, looksDiscrete: true, hasR16Uint: true })
    expect(r.verdict).toBe('ready')
    expect(r.reason).toMatch(/discrete/i)
  })

  it('returns ready with Apple-specific copy when isApple is set', () => {
    // Same verdict as a discrete card, different reason — the ready line should not call
    // unified-memory hardware "Discrete".
    const r = verdictFrom({ supported: true, adapterFound: true, looksDiscrete: true, hasR16Uint: true, isApple: true })
    expect(r.verdict).toBe('ready')
    expect(r.reason).toMatch(/apple/i)
    expect(r.reason).not.toMatch(/discrete/i)
  })

  it('accepts hasR16Uint=null (probe could not build a device) as long as the discrete side is there', () => {
    // A driver crash inside the check should not silently downgrade a good adapter to "unavailable"
    const r = verdictFrom({ supported: true, adapterFound: true, looksDiscrete: true, hasR16Uint: null })
    expect(r.verdict).toBe('ready')
  })
})
