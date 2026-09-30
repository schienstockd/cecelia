import { describe, it, expect, beforeEach, vi } from 'vitest'
import { effectScope, reactive } from 'vue'
import { setActivePinia, createPinia } from 'pinia'
import { createGatingStore, type PopTree } from './gating'
import { useWsStore } from './ws'

// One gating store per canvas (stores/gating.ts → provideGatingStore). What makes that safe to share a
// document across canvases is that EVERY store subscribes to the server's `gating:popmap` push itself,
// applies only its own document's, and unsubscribes with its owner.
const tree = (name: string, pop_type = 'flow'): PopTree =>
  ({ value_name: 'seg', pop_type, populations: [{ name, colour: '#fff', show: true, children: [] }] })

type Handler = Parameters<ReturnType<typeof useWsStore>['on']>[1]

describe('per-canvas gating store', () => {
  let pushHandlers: Handler[]
  beforeEach(() => {
    // no DOM in this suite (frontend/CLAUDE.md): the log store listens on window, the store's fetches
    // are irrelevant here (stats) — stub both to inert
    vi.stubGlobal('window', { addEventListener() {}, removeEventListener() {} })
    vi.stubGlobal('fetch', vi.fn(async () => ({ ok: false, json: async () => ({}) })))
    setActivePinia(createPinia())
    const ws = useWsStore()
    pushHandlers = []
    vi.spyOn(ws, 'on').mockImplementation((type, h) => { if (type === 'gating:popmap') pushHandlers.push(h) })
    vi.spyOn(ws, 'off').mockImplementation((type, h) => {
      if (type === 'gating:popmap') pushHandlers = pushHandlers.filter(x => x !== h)
    })
  })
  const make = (popType: string) => {
    const scope = effectScope()
    const g = scope.run(() => reactive(createGatingStore(popType)))!
    g.imageUid = 'img'; g.valueName = 'seg'
    return { g, scope }
  }
  const push = (d: Record<string, unknown>) => pushHandlers.forEach(h => h(d))

  it('two canvases are independent — one binding does not move the other', () => {
    const { g: gate } = make('flow')
    const { g: track } = make('track')
    track.imageUid = 'other'
    expect(gate.imageUid).toBe('img')
    expect(gate.popType).toBe('flow')
    expect(track.popType).toBe('track')
  })

  it('each store applies the push for ITS document only', () => {
    const { g: gate } = make('flow')
    const { g: track } = make('track')
    push({ imageUid: 'img', valueName: 'seg', popType: 'track', tree: tree('T', 'track') })
    expect(track.flat.map(p => p.name)).toEqual(['T'])
    expect(gate.flat).toEqual([])
  })

  it('two stores on the SAME document both follow the push (cluster page + board)', () => {
    const { g: page } = make('clust')
    const { g: board } = make('clust')
    push({ imageUid: 'img', valueName: 'seg', popType: 'clust', tree: tree('C1', 'clust') })
    expect(page.flat.map(p => p.name)).toEqual(['C1'])
    expect(board.flat.map(p => p.name)).toEqual(['C1'])
  })

  it('a disposed store stops listening', () => {
    const { scope } = make('flow')
    expect(pushHandlers).toHaveLength(1)
    scope.stop()
    expect(pushHandlers).toHaveLength(0)
  })
})
