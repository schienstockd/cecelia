import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { effectScope, nextTick } from 'vue'

const store = new Map<string, string>()
vi.mock('./profileStorage', () => ({
  profileStorage: {
    getItem: (k: string) => store.get(k) ?? null,
    setItem: (k: string, v: string) => { store.set(k, v) },
    removeItem: (k: string) => { store.delete(k) },
  },
}))

import { setSectionOpen, useStoredSectionOpen } from './sectionOpen'

beforeEach(() => {
  store.clear()
  vi.stubGlobal('window', new EventTarget())
})
afterEach(() => { vi.unstubAllGlobals() })

describe('useStoredSectionOpen', () => {
  it('reads the CollapsibleSection format, open by default', () => {
    const scope = effectScope()
    scope.run(() => {
      expect(useStoredSectionOpen('k').value).toBe(true)
      store.set('k2', '0')
      expect(useStoredSectionOpen('k2').value).toBe(false)
      expect(useStoredSectionOpen('k3', false).value).toBe(false)
    })
    scope.stop()
  })

  it('persists a toggle as 1/0', async () => {
    const scope = effectScope()
    const open = scope.run(() => useStoredSectionOpen('k'))!
    open.value = false
    await nextTick()
    expect(store.get('k')).toBe('0')
    scope.stop()
  })

  // TaskRunner's "When to use" fold (cc-task-about-open-v2) is not a CollapsibleSection, so a
  // setSectionOpen from outside must still move it — not only rewrite storage until remount.
  it('follows setSectionOpen on its key while mounted, and stops when its scope ends', () => {
    const scope = effectScope()
    const open = scope.run(() => useStoredSectionOpen('cc-task-about-open-v2'))!
    setSectionOpen('cc-task-about-open-v2', false)
    expect(open.value).toBe(false)
    setSectionOpen('other-key', true)
    expect(open.value).toBe(false)
    setSectionOpen('cc-task-about-open-v2', true)
    expect(open.value).toBe(true)
    scope.stop()
    setSectionOpen('cc-task-about-open-v2', false)
    expect(open.value).toBe(true)
  })
})
