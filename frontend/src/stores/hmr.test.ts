import { describe, it, expect } from 'vitest'

// RATCHET: every Pinia store accepts its own hot update.
//
// A setup-store instance is not replaced when its module hot-reloads — it keeps the shape it had at page
// load. So adding a field to a store and saving leaves the COMPONENT reading the new field off the old
// instance, and the page dies on code that is correct in the source:
//
//     UI error (render function): can't access property "length", $setup.customModules.clashes is undefined
//
// It cost a real debugging session precisely because nothing is wrong with the source — the fix is a
// reload, so it looks like a ghost. `acceptHMRUpdate` must be called in each store's own module (it needs
// that module's `import.meta.hot`), which means it is per-file boilerplate, which means it WILL be
// forgotten on the next store. Hence a test rather than a convention: the failure mode is dev-only, so
// nothing else would ever catch it.
const STORES = import.meta.glob('/src/stores/*.ts', {
  query: '?raw', import: 'default', eager: true }) as Record<string, string>

const sources = Object.entries(STORES).filter(([p]) => !p.endsWith('.test.ts'))

describe('store HMR', () => {
  it('found the stores (else this guard is watching nothing)', () => {
    expect(sources.length).toBeGreaterThan(10)
  })

  it('every store calls acceptHMRUpdate on itself', () => {
    const missing = sources
      .filter(([, src]) => !/import\.meta\.hot\.accept\(acceptHMRUpdate\(/.test(src))
      .map(([p]) => p)
    expect(missing).toEqual([])
  })

  it('the guard can actually fail (a store without the line is reported)', () => {
    const fake = [['/src/stores/fake.ts', 'export const useFakeStore = defineStore("fake", () => ({}))']]
    const missing = fake.filter(([, src]) => !/import\.meta\.hot\.accept\(acceptHMRUpdate\(/.test(src))
    expect(missing.length).toBe(1)
  })

  it('each one names its OWN store — a copy-pasted line is worse than a missing one', () => {
    // registering store A's updater in store B's module makes B never refresh AND corrupts A's
    const wrong: string[] = []
    for (const [path, src] of sources) {
      const declared = src.match(/export const (use[A-Za-z]+) = defineStore/)?.[1]
      const accepted = src.match(/acceptHMRUpdate\((use[A-Za-z]+),/)?.[1]
      if (!declared || declared !== accepted) wrong.push(`${path}: declares ${declared}, accepts ${accepted}`)
    }
    expect(wrong).toEqual([])
  })
})
