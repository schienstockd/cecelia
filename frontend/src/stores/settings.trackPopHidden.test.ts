import { describe, it, expect, beforeEach } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'

// Node lacks a DOM; the settings store reads localStorage at construction. In-memory shim is enough
// for round-trip testing (no other test in this suite exercises the DOM).
const _mem = new Map<string, string>()
;(globalThis as any).localStorage = {
  getItem: (k: string) => _mem.get(k) ?? null,
  setItem: (k: string, v: string) => { _mem.set(k, String(v)) },
  removeItem: (k: string) => { _mem.delete(k) },
  clear: () => { _mem.clear() },
}

import { useSettingsStore } from './settings'

// Per-image × vn set of pop paths whose track ribbons the user has hidden. Persisted to localStorage,
// consumed as a Set. See ViewerWindow.vue `hiddenTrackPops` docstring for the why (pop-manager pings
// were clobbering the transient set within a second).
describe('settings.getTrackPopHidden / setTrackPopHidden', () => {
  beforeEach(() => { localStorage.clear(); setActivePinia(createPinia()) })

  it('default empty for an unknown image', () => {
    const s = useSettingsStore()
    expect([...s.getTrackPopHidden('imgX', 'flowTom')]).toEqual([])
  })

  it('round-trips a set through localStorage', () => {
    const s = useSettingsStore()
    s.setTrackPopHidden('imgA', 'flowTom', new Set(['/qc/CD169-/cells', '/qc/CD169-/fragments']))
    const raw = JSON.parse(localStorage.getItem('cc.viewerTrackPopHidden') ?? '{}')
    expect(raw.imgA.flowTom.sort()).toEqual(['/qc/CD169-/cells', '/qc/CD169-/fragments'])
    const s2 = useSettingsStore()
    expect([...s2.getTrackPopHidden('imgA', 'flowTom')].sort())
      .toEqual(['/qc/CD169-/cells', '/qc/CD169-/fragments'])
  })

  it('per-image × vn isolation — one image or vn cannot leak into another', () => {
    const s = useSettingsStore()
    s.setTrackPopHidden('imgA', 'flowTom', new Set(['/a']))
    s.setTrackPopHidden('imgA', 'flowKat', new Set(['/b']))
    s.setTrackPopHidden('imgB', 'flowTom', new Set(['/c']))
    expect([...s.getTrackPopHidden('imgA', 'flowTom')]).toEqual(['/a'])
    expect([...s.getTrackPopHidden('imgA', 'flowKat')]).toEqual(['/b'])
    expect([...s.getTrackPopHidden('imgB', 'flowTom')]).toEqual(['/c'])
    expect([...s.getTrackPopHidden('imgB', 'flowKat')]).toEqual([])
  })

  it('empty set clears the entry cleanly', () => {
    const s = useSettingsStore()
    s.setTrackPopHidden('imgA', 'flowTom', new Set(['/a']))
    s.setTrackPopHidden('imgA', 'flowTom', new Set())
    expect([...s.getTrackPopHidden('imgA', 'flowTom')]).toEqual([])
  })
})
