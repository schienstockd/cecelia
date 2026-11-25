// RESULT-REVIEW: result-2307c62e — two quick edits: the second's stats refresh must not put back the first pop's
// pre-edit count (fetchStats merged its replies into a snapshot taken before the first reply landed).
import { describe, it, expect, vi } from 'vitest'
import { effectScope, reactive } from 'vue'
import { setActivePinia, createPinia } from 'pinia'
import { createGatingStore, type PopTree } from './gating'
import { useWsStore } from './ws'

const rect = (x: number) => ({ kind: 'rectangle', x_channel: 'a', y_channel: 'b', x_min: x, x_max: 1, y_min: 0, y_max: 1,
  x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' } })
const tree = (ax: number, bx: number): PopTree => ({ value_name: 'seg', pop_type: 'flow', populations: [
  { name: 'A', colour: '#fff', show: true, gate: rect(ax), children: [] },
  { name: 'B', colour: '#fff', show: true, gate: rect(bx), children: [] }] } as unknown as PopTree)

describe('fetchStats after two quick edits', () => {
  it('keeps the latest count of the first-edited pop', async () => {
    vi.stubGlobal('window', { addEventListener() {}, removeEventListener() {} })
    // server state: count = 100 * gate x_min of the pop (as saved on the server at request time)
    let server: Record<string, number> = { '/A': 1, '/B': 1 }
    const pending: (() => void)[] = []
    vi.stubGlobal('fetch', vi.fn((url: string) => {
      const pop = decodeURIComponent(/pop=([^&]+)/.exec(url)![1])
      const count = server[pop] * 100
      return new Promise(res => pending.push(() => res({ ok: true, json: async () => ({ count, pctParent: 0 }) })))
    }))
    setActivePinia(createPinia())
    const ws = useWsStore(); const handlers: any[] = []
    vi.spyOn(ws, 'on').mockImplementation((t, h) => { if (t === 'gating:pop-tree') handlers.push(h) })
    vi.spyOn(ws, 'off').mockImplementation(() => {})
    const g = effectScope().run(() => reactive(createGatingStore('flow')))!
    g.imageUid = 'img'; g.valueName = 'seg'
    const push = (t: PopTree) => handlers.forEach(h => h({ imageUid: 'img', valueName: 'seg', popType: 'flow', tree: t }))
    const flush = async () => { while (pending.length) pending.shift()!(); for (let i = 0; i < 20; i++) await Promise.resolve() }

    push(tree(1, 1)); await flush()
    expect(g.stats['/A'].count).toBe(100)
    // edit 1: move A's gate (server saves it), stats request for A in flight
    server = { '/A': 2, '/B': 1 }; push(tree(2, 1))
    const firstReq = pending.length
    // edit 2 before edit 1's stats come back: move B
    server = { '/A': 2, '/B': 3 }; push(tree(2, 3))
    await flush()
    console.log('requests', firstReq, 'final', JSON.stringify(g.stats))
    expect(g.stats['/A'].count).toBe(200)   // A's gate is at 2 on the server
    expect(g.stats['/B'].count).toBe(300)
  })
})
