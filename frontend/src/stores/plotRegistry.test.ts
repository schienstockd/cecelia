// Live plot registry — pure-logic tests of the fetch shape and the lifecycle. No Vue components
// mount here; `effectScope` gives us the onScopeDispose the composable needs, so a "panel unmount"
// is one `scope.stop()`. Mirrors `canvasPanelExports.test.ts` in style (no jsdom / DOM).

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { effectScope, ref, nextTick } from 'vue'
import { usePlotRegistry, usePlotRegistryStore } from './plotRegistry'
import { wsClientId } from '../utils/clientId'
import { useProjectMetaStore } from './projectMeta'

// Node's default env has no `window`; the log store's setup does. `useProjectMetaStore` (imported
// transitively by plotRegistry) instantiates the log store, so give it a `window` stub that
// implements just enough to make `addEventListener` a no-op. Vitest's default env is `node`;
// switching to jsdom would violate the "no DOM" convention for utils tests (frontend/CLAUDE.md).
globalThis.window = globalThis.window ?? ({
  addEventListener() {}, removeEventListener() {}, location: { host: '' },
} as unknown as Window & typeof globalThis)

// A tiny fetch stub that captures every call. Returns a resolved promise (the store fires
// fire-and-forget; the returned promise is only used for .catch()).
function stubFetch() {
  const calls: Array<{ url: string; init: RequestInit }> = []
  const spy = vi.fn(async (url: string, init: RequestInit) => {
    calls.push({ url, init })
    return new Response('{"ok":true}', { status: 200, headers: { 'Content-Type': 'application/json' } })
  })
  // @ts-expect-error — replacing globalThis.fetch in test scope
  globalThis.fetch = spy
  return { calls, spy }
}

function bodyOf(init: RequestInit): Record<string, unknown> {
  return JSON.parse(String(init.body ?? '{}'))
}

describe('usePlotRegistry', () => {
  let calls: Array<{ url: string; init: RequestInit }>

  beforeEach(() => {
    setActivePinia(createPinia())
    const s = stubFetch(); calls = s.calls
    // Seed a project — most tests need one open.
    useProjectMetaStore().current = { uid: 'PROJ1', name: 'test' } as any
  })

  afterEach(() => {
    // @ts-expect-error
    delete globalThis.fetch
  })

  it('POSTs a register when a persistKey + project are present', async () => {
    const scope = effectScope()
    scope.run(() => {
      usePlotRegistry(() => 'panel:7', () => ({ family: 'umap', title: 'UMAP', route: '/analysis' }))
    })
    await nextTick()
    expect(calls.length).toBe(1)
    expect(calls[0].url).toBe('/api/viewer/plots/register')
    const body = bodyOf(calls[0].init)
    expect(body).toMatchObject({
      clientId: wsClientId, projectUid: 'PROJ1', plotId: 'panel:7',
      family: 'umap', title: 'UMAP', route: '/analysis',
    })
    scope.stop()
  })

  it('re-POSTs on meta change (title updates while the key stays)', async () => {
    const scope = effectScope()
    const title = ref('UMAP')
    scope.run(() => {
      usePlotRegistry(() => 'panel:7',
        () => ({ family: 'umap', title: title.value, route: '/analysis' }))
    })
    await nextTick()
    expect(calls.filter(c => c.url.endsWith('/register')).length).toBe(1)
    title.value = 'UMAP (flowTom)'
    await nextTick()
    const regs = calls.filter(c => c.url.endsWith('/register'))
    expect(regs.length).toBe(2)
    expect(bodyOf(regs[1].init).title).toBe('UMAP (flowTom)')
    scope.stop()
  })

  it('does NOT re-POST when the getter returns identical meta', async () => {
    const scope = effectScope()
    const tick = ref(0)
    scope.run(() => {
      usePlotRegistry(() => 'panel:7',
        // tick.value is read to force the watcher to fire, but the returned object is identical
        () => (tick.value >= 0 ? { family: 'umap', title: 'UMAP', route: '/analysis' } : {} as any))
    })
    await nextTick()
    tick.value = 1
    await nextTick()
    expect(calls.filter(c => c.url.endsWith('/register')).length).toBe(1)
    scope.stop()
  })

  it('POSTs a deregister on scope dispose', async () => {
    const scope = effectScope()
    scope.run(() => {
      usePlotRegistry(() => 'panel:7', () => ({ family: 'umap', title: 'UMAP', route: '/analysis' }))
    })
    await nextTick()
    scope.stop()
    await nextTick()
    const dereg = calls.filter(c => c.url.endsWith('/deregister'))
    expect(dereg.length).toBe(1)
    const body = bodyOf(dereg[0].init)
    expect(body).toMatchObject({ clientId: wsClientId, projectUid: 'PROJ1', plotId: 'panel:7' })
  })

  it('skips register when persistKey is empty', async () => {
    const scope = effectScope()
    scope.run(() => {
      usePlotRegistry(() => '', () => ({ family: 'umap', title: 'UMAP', route: '/analysis' }))
    })
    await nextTick()
    expect(calls.length).toBe(0)
    scope.stop()
  })

  it('skips register when no project is open', async () => {
    useProjectMetaStore().current = null
    const scope = effectScope()
    scope.run(() => {
      usePlotRegistry(() => 'panel:7', () => ({ family: 'umap', title: 'UMAP', route: '/analysis' }))
    })
    await nextTick()
    expect(calls.length).toBe(0)
    scope.stop()
  })

  it('includes `content` on the POST body when the meta supplies it', async () => {
    const scope = effectScope()
    scope.run(() => {
      usePlotRegistry(() => 'panel:7',
        () => ({ family: 'summary', title: 'Track measures', route: '/analysis',
                 content: { measure: 'speed', chartType: 'box' } }))
    })
    await nextTick()
    const body = bodyOf(calls[0].init)
    expect(body.content).toEqual({ measure: 'speed', chartType: 'box' })
    scope.stop()
  })

  it('omits `content` from the POST body when the meta does not supply it', async () => {
    const scope = effectScope()
    scope.run(() => {
      usePlotRegistry(() => 'panel:7',
        () => ({ family: 'summary', title: 'Track measures', route: '/analysis' }))
    })
    await nextTick()
    const body = bodyOf(calls[0].init)
    expect('content' in body).toBe(false)
    scope.stop()
  })

  it('re-POSTs when only `content` changes (dedupe sees the deep diff)', async () => {
    const scope = effectScope()
    const m = ref('speed')
    scope.run(() => {
      usePlotRegistry(() => 'panel:7',
        () => ({ family: 'summary', title: 'Track measures', route: '/analysis',
                 content: { measure: m.value } }))
    })
    await nextTick()
    expect(calls.filter(c => c.url.endsWith('/register')).length).toBe(1)
    m.value = 'displacement'
    await nextTick()
    const regs = calls.filter(c => c.url.endsWith('/register'))
    expect(regs.length).toBe(2)
    expect((bodyOf(regs[1].init).content as Record<string, unknown>).measure).toBe('displacement')
    scope.stop()
  })

  it('does NOT re-POST when the getter returns identical `content` in a new object literal', async () => {
    const scope = effectScope()
    const tick = ref(0)
    scope.run(() => {
      usePlotRegistry(() => 'panel:7',
        // `tick` is read to force the watcher; the returned object is a fresh literal each tick
        // but the fields are identical — dedupe (deep JSON.stringify on content) must catch this.
        () => (tick.value >= 0
          ? { family: 'summary', title: 'Track measures', route: '/analysis',
              content: { measure: 'speed', chartType: 'box' } }
          : {} as any))
    })
    await nextTick()
    tick.value = 1
    await nextTick()
    expect(calls.filter(c => c.url.endsWith('/register')).length).toBe(1)
    scope.stop()
  })

  it('the store dedupe cache tracks the last-sent entry', async () => {
    const scope = effectScope()
    scope.run(() => {
      usePlotRegistry(() => 'panel:7', () => ({ family: 'umap', title: 'UMAP', route: '/analysis' }))
    })
    await nextTick()
    const s = usePlotRegistryStore()
    expect(s.getLast('panel:7')).toBeDefined()
    scope.stop()
    await nextTick()
    expect(s.getLast('panel:7')).toBeUndefined()
  })
})
