import { describe, it, expect } from 'vitest'
import { verdictFrom } from './webgpuProbe'

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
  })

  it('accepts hasR16Uint=null (probe could not build a device) as long as the discrete side is there', () => {
    // A driver crash inside the check should not silently downgrade a good adapter to "unavailable"
    const r = verdictFrom({ supported: true, adapterFound: true, looksDiscrete: true, hasR16Uint: null })
    expect(r.verdict).toBe('ready')
  })
})
