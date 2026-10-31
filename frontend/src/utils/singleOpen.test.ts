import { describe, expect, it } from 'vitest'
import { createSingleOpenRegistry, type SingleOpenKV } from './singleOpen'

function memKV(init: Record<string, string> = {}) {
  const m = new Map(Object.entries(init))
  const writes: [string, string][] = []
  const kv: SingleOpenKV = {
    getItem: k => m.get(k) ?? null,
    setItem: (k, v) => { m.set(k, v); writes.push([k, v]) },
  }
  return { kv, m, writes }
}

describe('single-open accordion registry', () => {
  it('one keyword = one shared state, however many callers ask for it', () => {
    const { kv } = memKV()
    const reg = createSingleOpenRegistry(() => kv)
    const script = reg('cc.vw.section', 'channels')
    const section = reg('cc.vw.section')
    expect(section).toBe(script)
    section.set('seg')
    expect(script.open.value).toBe('seg')
    expect(script.isOpen('channels')).toBe(false)
  })

  it('keeps reading the stored value under the same key (existing users keep their choice)', () => {
    const { kv } = memKV({ 'cc.kiwi.openSection': 'captures' })
    const g = createSingleOpenRegistry(() => kv)('cc.kiwi.openSection', 'ask')
    expect(g.open.value).toBe('captures')
    expect(g.chosen.value).toBe(true)
  })

  it("a stored '' (all collapsed) is a choice, not a missing value", () => {
    const { kv } = memKV({ k: '' })
    const g = createSingleOpenRegistry(() => kv)('k', 'ask')
    expect(g.open.value).toBe('')
    expect(g.chosen.value).toBe(true)
  })

  it('the default shows until the user chooses, and is never written', () => {
    const { kv, writes } = memKV()
    const g = createSingleOpenRegistry(() => kv)('k', 'ask')
    expect(g.open.value).toBe('ask')
    expect(g.chosen.value).toBe(false)
    expect(writes).toEqual([])
    g.toggle('ask')
    expect(g.open.value).toBe('')
    expect(writes).toEqual([['k', '']])
  })

  it('first non-empty default wins; a section offering one later does not move it', () => {
    const { kv } = memKV()
    const reg = createSingleOpenRegistry(() => kv)
    reg('k')                    // a section with no default mounts first
    reg('k').offerDefault('ask')
    reg('k', 'other')
    expect(reg('k').open.value).toBe('ask')
  })

  it('toggle opens a closed section, closes the open one; set and open= persist', () => {
    const { kv, m } = memKV()
    const g = createSingleOpenRegistry(() => kv)('k')
    g.toggle('a'); expect(g.open.value).toBe('a')
    g.toggle('b'); expect(g.open.value).toBe('b')
    g.toggle('b'); expect(g.open.value).toBe('')
    g.open.value = 'c'
    expect(m.get('k')).toBe('c')
  })

  it('persist: false keeps state in memory only', () => {
    const { kv, writes } = memKV({ k: 'stale' })
    const g = createSingleOpenRegistry(() => kv)('k', '', { persist: false })
    expect(g.open.value).toBe('')
    g.set('x')
    expect(g.open.value).toBe('x')
    expect(writes).toEqual([])
  })

  it('works with no storage at all', () => {
    const g = createSingleOpenRegistry(() => null)('k', 'a')
    g.set('b')
    expect(g.open.value).toBe('b')
  })
})
