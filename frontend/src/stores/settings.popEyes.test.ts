import { describe, it, expect, beforeEach } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'

// Node lacks a DOM; the settings store reads localStorage at construction. In-memory shim, as
// settings.trackPopHidden.test.ts.
const _mem = new Map<string, string>()
;(globalThis as any).localStorage = {
  getItem: (k: string) => _mem.get(k) ?? null,
  setItem: (k: string, v: string) => { _mem.set(k, String(v)) },
  removeItem: (k: string) => { _mem.delete(k) },
  clear: () => { _mem.clear() },
}

import { useSettingsStore } from './settings'

// Per-image × vn point eyes of the viewer's population rows (utils/viewer/popEyes.ts).
describe('settings.getPopEyes / setPopEyes', () => {
  beforeEach(() => { localStorage.clear(); setActivePinia(createPinia()) })

  it('default empty for an unknown image', () => {
    expect(useSettingsStore().getPopEyes('imgX', 'segA')).toEqual({})
  })

  it('round-trips through localStorage and isolates image × vn', () => {
    const s = useSettingsStore()
    s.setPopEyes('imgA', 'segA', { 'flow::/a': false, 'flow::/b': true })
    s.setPopEyes('imgA', 'segB', { 'flow::/a': true })
    setActivePinia(createPinia())
    const s2 = useSettingsStore()
    expect(s2.getPopEyes('imgA', 'segA')).toEqual({ 'flow::/a': false, 'flow::/b': true })
    expect(s2.getPopEyes('imgA', 'segB')).toEqual({ 'flow::/a': true })
    expect(s2.getPopEyes('imgB', 'segA')).toEqual({})
  })

  it('a returned bag is a copy — mutating it does not write the store', () => {
    const s = useSettingsStore()
    s.setPopEyes('imgA', 'segA', { 'flow::/a': false })
    s.getPopEyes('imgA', 'segA')['flow::/a'] = true
    expect(s.getPopEyes('imgA', 'segA')).toEqual({ 'flow::/a': false })
  })

  it('points and ribbons are separate bags', () => {
    const s = useSettingsStore()
    s.setPopEyes('imgA', 'segA', { 'flow::/a': false })
    s.setPopEyes('imgA', 'segA', { 'flow::/a': true }, 'ribbons')
    setActivePinia(createPinia())
    const s2 = useSettingsStore()
    expect(s2.getPopEyes('imgA', 'segA')).toEqual({ 'flow::/a': false })
    expect(s2.getPopEyes('imgA', 'segA', 'points')).toEqual({ 'flow::/a': false })
    expect(s2.getPopEyes('imgA', 'segA', 'ribbons')).toEqual({ 'flow::/a': true })
    expect(JSON.parse(localStorage.getItem('cc.viewerRibbonEyes') ?? '{}'))
      .toEqual({ imgA: { segA: { 'flow::/a': true } } })
  })
})
