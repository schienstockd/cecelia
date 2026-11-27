// RESULT-REVIEW: result-f405de42 — the server broadcasts the new tree BEFORE it answers the POST, so
// the store has already applied it when the reply lands; diffing the reply against the current tree
// then finds no change and a gate move sent a `look` tick — the viewer kept the old gate's cells.
import { describe, it, expect, vi } from 'vitest'
import { effectScope, reactive, nextTick } from 'vue'
import { setActivePinia, createPinia } from 'pinia'
import { createGatingStore, type PopTree } from './gating'
import { useWsStore } from './ws'
import { parseOverlayTick, OVERLAY_TICK_KEY } from '../utils/viewer/overlayTick'

const rect = (x: number) => ({ kind: 'rectangle', x_channel: 'a', y_channel: 'b', x_min: x, x_max: 1, y_min: 0, y_max: 1,
  x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' } })
const tree = (ax: number): PopTree => ({ value_name: 'seg', pop_type: 'flow', populations: [
  { name: 'A', colour: '#ff0000', show: true, gate: rect(ax), children: [] }] } as unknown as PopTree)

describe('gate move whose broadcast lands before its reply', () => {
  it('still asks the viewer for the populations', async () => {
    const store = new Map<string, string>()
    vi.stubGlobal('localStorage', { getItem: (k: string) => store.get(k) ?? null,
                                    setItem: (k: string, v: string) => { store.set(k, v) }, removeItem: () => {} })
    vi.stubGlobal('window', { addEventListener() {}, removeEventListener() {} })
    const handlers: ((d: unknown) => void)[] = []
    const push = (t: PopTree) => handlers.forEach(h => h({ imageUid: 'img', valueName: 'seg', popType: 'flow', tree: t }))
    vi.stubGlobal('fetch', vi.fn(async (_url: string, init?: { method?: string }) => {
      if (init?.method === 'POST') push(tree(0.2))          // `_persist_and_broadcast!`: broadcast, then reply
      return new Response(JSON.stringify(init?.method === 'POST' ? { tree: tree(0.2) } : { count: 1, pctParent: 0 }), { status: 200 })
    }))
    setActivePinia(createPinia())
    const ws = useWsStore()
    vi.spyOn(ws, 'on').mockImplementation((t, h) => { if (t === 'gating:pop-tree') handlers.push(h as never) })
    vi.spyOn(ws, 'off').mockImplementation(() => {})
    const g = effectScope().run(() => reactive(createGatingStore('flow')))!
    g.imageUid = 'img'; g.valueName = 'seg'
    push(tree(0.1))
    await nextTick(); store.delete(OVERLAY_TICK_KEY)
    await g.setGate('/A', rect(0.2) as never)
    expect(parseOverlayTick(store.get(OVERLAY_TICK_KEY))?.scope).toBe('pops')
    vi.unstubAllGlobals()
  })
})
