import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'

// Node lacks a DOM; viewProfiles pulls in the settings store, which reads localStorage at
// construction. In-memory shim, as settings.popEyes.test.ts.
const _mem = new Map<string, string>()
;(globalThis as any).localStorage = {
  getItem: (k: string) => _mem.get(k) ?? null,
  setItem: (k: string, v: string) => { _mem.set(k, String(v)) },
  removeItem: (k: string) => { _mem.delete(k) },
  clear: () => { _mem.clear() },
}

import { useViewProfilesStore } from './viewProfiles'
import { useCustomModulesStore } from './customModules'

// ensureLoaded() must hand a caller that arrives DURING a load the in-flight promise. It used to
// return at once (`!loadedOnce && !loading`), so `await ensureLoaded()` resolved before the data was set.

// A fetch whose response the test releases by hand, so a second caller can arrive mid-load.
function gatedFetch(body: unknown) {
  let release!: () => void
  const gate = new Promise<void>(r => { release = r })
  const f = vi.fn(async () => {
    await gate
    return { ok: true, json: () => Promise.resolve(body) }
  })
  vi.stubGlobal('fetch', f)
  return { f, release }
}

const ticks = async () => { for (let i = 0; i < 5; i++) await Promise.resolve() }

describe('ensureLoaded waits for an in-flight load', () => {
  beforeEach(() => { setActivePinia(createPinia()) })
  afterEach(() => { vi.unstubAllGlobals() })

  it('viewProfiles: an awaiting caller during refresh() sees the profiles', async () => {
    const profile = { id: 'imaging', label: 'Imaging', items: ['/explore'] }
    const { f, release } = gatedFetch({ dir: '/cfg/view-profiles', profiles: [profile], errors: [] })
    const store = useViewProfilesStore()
    const first = store.refresh()                // e.g. SettingsModule's onMounted refresh
    let seen: unknown = 'not yet'
    const second = store.ensureLoaded().then(() => { seen = store.profiles.map(p => p.id) })
    await ticks()
    expect(seen).toBe('not yet')                 // must not resolve before the data exists
    release()
    await Promise.all([first, second])
    expect(seen).toEqual(['imaging'])
    expect(f).toHaveBeenCalledTimes(1)           // joined the load, did not start a second
    expect(store.loading).toBe(false)
  })

  it('customModules: an awaiting caller during refresh() sees modules/registry', async () => {
    const { f, release } = gatedFetch({
      dir: '/cfg/modules', modules: [{ file: 'my.jl', ok: true }], categories: ['custom'],
      registry: { 'custom.myTask': { label: 'My task' } },
    })
    const store = useCustomModulesStore()
    const first = store.refresh()
    let seen: unknown = 'not yet'
    const second = store.ensureLoaded().then(() => {
      seen = [store.modules.length, Object.keys(store.registry)]
    })
    await ticks()
    expect(seen).toBe('not yet')
    release()
    await Promise.all([first, second])
    expect(seen).toEqual([1, ['custom.myTask']])
    expect(f).toHaveBeenCalledTimes(1)
    expect(store.loading).toBe(false)
  })

  it('customModules: an awaiting caller during reload() waits for it too', async () => {
    const { f, release } = gatedFetch({ modules: [{ file: 'new.jl', ok: true }] })
    const store = useCustomModulesStore()
    const first = store.reload()
    let seen = -1
    const second = store.ensureLoaded().then(() => { seen = store.modules.length })
    await ticks()
    expect(seen).toBe(-1)
    release()
    await Promise.all([first, second])
    expect(seen).toBe(1)
    expect(f).toHaveBeenCalledTimes(1)
  })

  it('ensureLoaded loads once when nothing is in flight, then is a no-op', async () => {
    const { f, release } = gatedFetch({ profiles: [] })
    release()
    const store = useViewProfilesStore()
    await store.ensureLoaded()
    await store.ensureLoaded()
    expect(f).toHaveBeenCalledTimes(1)
  })
})
