import { describe, it, expect } from 'vitest'
import { adoptProfileBag, renameMirrorOwner, OWNER_KEY, INDEX_KEY, type KV } from './profileStorage'

function memKV(init: Record<string, string> = {}): KV & { data: Record<string, string> } {
  const data = { ...init }
  return {
    data,
    getItem: k => (k in data ? data[k] : null),
    setItem: (k, v) => { data[k] = v },
    removeItem: k => { delete data[k] },
  }
}

describe('adoptProfileBag', () => {
  it('first run (no owner) keeps the mirror and bootstraps mirror-only keys', () => {
    const kv = memKV({ 'cc.kiwiPrompt': 'half typed', [INDEX_KEY]: '["cc.kiwiPrompt"]' })
    const plan = adoptProfileBag(kv, 'alice', {})
    expect(plan).toEqual({ reload: false, bootstrap: { 'cc.kiwiPrompt': 'half typed' } })
    expect(kv.data[OWNER_KEY]).toBe('alice')
    expect(kv.data['cc.kiwiPrompt']).toBe('half typed')
  })

  it('same owner: the bag wins over the mirror and is written down', () => {
    const kv = memKV({ [OWNER_KEY]: 'alice', 'cc.movies.tags': '["a"]', [INDEX_KEY]: '["cc.movies.tags"]' })
    const plan = adoptProfileBag(kv, 'alice', { 'ls:cc.movies.tags': '["b"]', 'ls:cc.guide.x.done': '1' })
    expect(plan.reload).toBe(true)
    expect(plan.bootstrap).toEqual({})
    expect(kv.data['cc.movies.tags']).toBe('["b"]')
    expect(kv.data['cc.guide.x.done']).toBe('1')
    expect(JSON.parse(kv.data[INDEX_KEY]).sort()).toEqual(['cc.guide.x.done', 'cc.movies.tags'])
  })

  it('different owner: clears the previous person, writes the bag, asks for a reload', () => {
    const kv = memKV({
      [OWNER_KEY]: 'alice',
      [INDEX_KEY]: '["cc.kiwiDraft","cc.kiwiPrompt"]',
      'cc.kiwiDraft': '{"refs":[1]}', 'cc.kiwiPrompt': "alice's question",
      'cc.sidebarCollapsed': 'true',          // a settings-store mirror key
      'cc.viewerCacheMB': '2048',             // per-machine — never touched
      'cc.openProject': 'xyz',                // cross-window channel — never touched
    })
    const plan = adoptProfileBag(kv, 'ben', { 'ls:cc.kiwiPrompt': "ben's question", kiwiOpen: true },
                                 ['cc.sidebarCollapsed'])
    expect(plan).toEqual({ reload: true, bootstrap: {} })
    expect(kv.data['cc.kiwiPrompt']).toBe("ben's question")
    expect(kv.data['cc.kiwiDraft']).toBeUndefined()
    expect(kv.data['cc.sidebarCollapsed']).toBeUndefined()
    expect(kv.data['cc.viewerCacheMB']).toBe('2048')
    expect(kv.data['cc.openProject']).toBe('xyz')
    expect(kv.data[OWNER_KEY]).toBe('ben')
    // the next boot is the same owner → no reload loop
    expect(adoptProfileBag(kv, 'ben', { 'ls:cc.kiwiPrompt': "ben's question" }).reload).toBe(false)
  })

  it('ignores non-ls bag keys and non-string ls values', () => {
    const kv = memKV({ [OWNER_KEY]: 'alice' })
    adoptProfileBag(kv, 'alice', { kiwiOpen: true, 'ls:cc.x': 3 })
    expect(kv.data['kiwiOpen']).toBeUndefined()
    expect(kv.data['cc.x']).toBeUndefined()
  })

  it('tolerates a corrupt index', () => {
    const kv = memKV({ [OWNER_KEY]: 'alice', [INDEX_KEY]: '{not json' })
    expect(adoptProfileBag(kv, 'alice', {}).reload).toBe(false)
  })
})

describe('adoptProfileBag: readers that read the mirror before adopt', () => {
  // The settings store hydrates without being awaited, so the guide store and the first page's
  // components have already read the mirror by the time the bag lands. A bag that changes the mirror
  // must reload, or they keep the stale values for the session.
  it('fresh origin (no owner, empty mirror) with a bag reloads', () => {
    const kv = memKV()
    const plan = adoptProfileBag(kv, 'alice', { 'ls:cc.guide.x.done': '1', 'ls:cc.hint.seg': '1' })
    expect(plan.reload).toBe(true)
    expect(kv.data['cc.guide.x.done']).toBe('1')
    expect(kv.data[OWNER_KEY]).toBe('alice')
    // the next boot finds the mirror matching → no reload loop
    expect(adoptProfileBag(kv, 'alice', { 'ls:cc.guide.x.done': '1', 'ls:cc.hint.seg': '1' }).reload).toBe(false)
  })

  it('fresh origin with an empty bag does not reload', () => {
    expect(adoptProfileBag(memKV(), 'alice', { kiwiOpen: true }).reload).toBe(false)
  })

  it('same owner, bag changed elsewhere (another origin or browser) reloads', () => {
    const kv = memKV({ [OWNER_KEY]: 'alice', [INDEX_KEY]: '["cc.hint.seg"]' })
    expect(adoptProfileBag(kv, 'alice', { 'ls:cc.hint.seg': '1' }).reload).toBe(true)
    expect(kv.data['cc.hint.seg']).toBe('1')
  })

  it('same owner, bag matches the mirror: no reload', () => {
    const kv = memKV({ [OWNER_KEY]: 'alice', 'cc.hint.seg': '1', [INDEX_KEY]: '["cc.hint.seg"]' })
    expect(adoptProfileBag(kv, 'alice', { 'ls:cc.hint.seg': '1' }).reload).toBe(false)
  })

  it('a reload still carries the mirror-only keys up as bootstrap', () => {
    const kv = memKV({ 'cc.kiwiPrompt': 'half typed', [INDEX_KEY]: '["cc.kiwiPrompt"]' })
    const plan = adoptProfileBag(kv, 'alice', { 'ls:cc.hint.seg': '1' })
    expect(plan).toEqual({ reload: true, bootstrap: { 'cc.kiwiPrompt': 'half typed' } })
  })

  it('different owner whose owner write fails does not reload — that would loop', () => {
    const kv = memKV({ [OWNER_KEY]: 'alice' })
    const set = kv.setItem
    kv.setItem = (k, v) => { if (k === OWNER_KEY) throw new Error('quota'); set(k, v) }
    const guarded: KV = { getItem: kv.getItem, removeItem: kv.removeItem,
                          setItem: (k, v) => { try { kv.setItem(k, v) } catch { /* as the live _ls */ } } }
    expect(adoptProfileBag(guarded, 'ben', { 'ls:cc.hint.seg': '1' }).reload).toBe(false)
  })

  it('does not reload when the writes do not stick (private mode) — that would loop', () => {
    const kv: KV = { getItem: () => null, setItem: () => {}, removeItem: () => {} }
    expect(adoptProfileBag(kv, 'alice', { 'ls:cc.hint.seg': '1' }).reload).toBe(false)
  })
})

describe('renameMirrorOwner', () => {
  it('moves the owner only when it matches', () => {
    const kv = memKV({ [OWNER_KEY]: 'alice' })
    renameMirrorOwner(kv, 'bob', 'robert')
    expect(kv.data[OWNER_KEY]).toBe('alice')
    renameMirrorOwner(kv, 'alice', 'alicia')
    expect(kv.data[OWNER_KEY]).toBe('alicia')
  })
})
