// RESULT-REVIEW: result-9944e845 — the stats race on the EDIT-REPLY path (`_post` → `setTree` →
// `fetchStats(changed)`): two quick gate edits, the second's refresh must not put back the first pop's
// pre-edit count. The broadcast path refreshes every count, so it cannot show this.
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

describe('fetchStats on the edit reply', () => {
  it('a later edit keeps the earlier edit\'s new count', async () => {
    vi.stubGlobal('window', { addEventListener() {}, removeEventListener() {} })
    // the server: each pop's count = 100 × its gate's x_min, as saved when the stats request is answered
    const gates: Record<string, number> = { '/A': 1, '/B': 1 }
    const pending: (() => void)[] = []
    vi.stubGlobal('fetch', vi.fn((url: string, init?: { body?: string }) => {
      if (url.startsWith('/api/gating/pop/set-gate')) {
        const b = JSON.parse(init!.body!) as { path: string; gate: { x_min: number } }
        gates[b.path] = b.gate.x_min
        const t = tree(gates['/A'], gates['/B'])
        return Promise.resolve(new Response(JSON.stringify({ tree: t }), { status: 200 }))
      }
      const pop = decodeURIComponent(/pop=([^&]+)/.exec(url)![1])
      // answered when released, with the count AS SAVED NOW (the edit landed before the request)
      const count = gates[pop] * 100
      return new Promise(res => pending.push(() => res(new Response(JSON.stringify({ count, pctParent: 0 }), { status: 200 }))))
    }))
    setActivePinia(createPinia())
    const ws = useWsStore()
    vi.spyOn(ws, 'on').mockImplementation(() => {})
    vi.spyOn(ws, 'off').mockImplementation(() => {})
    const g = effectScope().run(() => reactive(createGatingStore('flow')))!
    g.imageUid = 'img'; g.valueName = 'seg'
    const settle = async () => { for (let i = 0; i < 20; i++) await Promise.resolve() }
    const release = async (n: number) => { for (let i = 0; i < n; i++) pending.shift()!(); await settle() }
    const releaseLast = async () => { pending.pop()!(); await settle() }

    await g.setGate('/A', rect(1) as never); await release(pending.length)   // first tree: every count
    expect(g.stats['/A'].count).toBe(100)
    await g.setGate('/A', rect(2) as never)          // edit 1: A's count request in flight
    await g.setGate('/B', rect(3) as never)          // edit 2, before edit 1's reply
    await releaseLast()                              // edit 2's reply (B) lands first …
    await release(1)                                 // … then edit 1's (A)
    expect(g.stats['/A'].count).toBe(200)
    expect(g.stats['/B'].count).toBe(300)
  })
})
