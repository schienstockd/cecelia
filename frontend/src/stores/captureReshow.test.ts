import { describe, it, expect, beforeEach } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useCaptureReshowStore } from './captureReshow'
import type { CaptureEnvelope } from '../utils/kiwiCaptures'

const ENV: CaptureEnvelope = {
  captureId: 'cap-x', surface: 'plot', address: null, overlay: [],
  viewStateSnapshot: null, landscape: null, notes: '', panels: null, frame: '',
}

describe('useCaptureReshowStore', () => {
  beforeEach(() => setActivePinia(createPinia()))

  it('starts empty', () => {
    const s = useCaptureReshowStore()
    expect(s.hasPending).toBe(false)
    expect(s.pending).toBeNull()
  })

  it('setPending + hasPending + clear', () => {
    const s = useCaptureReshowStore()
    s.setPending({ module: 'behaviourAnalysis', envelope: ENV })
    expect(s.hasPending).toBe(true)
    s.clear()
    expect(s.hasPending).toBe(false)
  })

  it('consumeFor returns and clears when the module matches', () => {
    const s = useCaptureReshowStore()
    s.setPending({ module: 'behaviourAnalysis', envelope: ENV })
    expect(s.consumeFor('behaviourAnalysis')).toStrictEqual(ENV)
    expect(s.hasPending).toBe(false)
    // Idempotent
    expect(s.consumeFor('behaviourAnalysis')).toBeNull()
  })

  it('consumeFor keeps the bag when the module does not match', () => {
    const s = useCaptureReshowStore()
    s.setPending({ module: 'behaviourAnalysis', envelope: ENV })
    expect(s.consumeFor('phenotype')).toBeNull()
    expect(s.hasPending).toBe(true)   // still waiting for the behaviourAnalysis page
  })

  it('setPending overwrites — a fresh refocus replaces a stale bag', () => {
    const s = useCaptureReshowStore()
    s.setPending({ module: 'behaviourAnalysis', envelope: ENV })
    const env2: CaptureEnvelope = { ...ENV, captureId: 'cap-y' }
    s.setPending({ module: 'phenotype', envelope: env2 })
    expect(s.consumeFor('behaviourAnalysis')).toBeNull()
    expect(s.consumeFor('phenotype')).toStrictEqual(env2)
  })
})
