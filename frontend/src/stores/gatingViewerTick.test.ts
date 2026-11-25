// The viewer ping a gate write sends is scoped by what changed: a colour change ships its look and asks
// the viewer to fetch nothing; a gate move asks for the edited document's populations alone. Before,
// every write sent a bare tick, and the viewer refetched the whole movie's overlays (42 MB on a
// 608k-cell movie) for each — a colour change included.
import { describe, it, expect, vi, beforeEach } from 'vitest'
import { effectScope, reactive, nextTick } from 'vue'
import { setActivePinia, createPinia } from 'pinia'
import { createGatingStore, type PopTree } from './gating'
import { useWsStore } from './ws'
import { parseOverlayTick, OVERLAY_TICK_KEY } from '../utils/viewer/overlayTick'

const rect = (x: number) => ({ kind: 'rectangle', x_channel: 'a', y_channel: 'b', x_min: x, x_max: 1, y_min: 0, y_max: 1,
  x_transform: { kind: 'linear' }, y_transform: { kind: 'linear' } })
const tree = (ax: number, colour: string): PopTree => ({ value_name: 'seg', pop_type: 'flow', populations: [
  { name: 'A', colour, show: true, gate: rect(ax), children: [
    { name: 'A1', colour: '#111111', show: false, gate: rect(0.5), children: [] }] }] } as unknown as PopTree)

let reply: PopTree
async function setup() {
  const store = new Map<string, string>()
  vi.stubGlobal('localStorage', { getItem: (k: string) => store.get(k) ?? null,
                                  setItem: (k: string, v: string) => { store.set(k, v) }, removeItem: (k: string) => { store.delete(k) } })
  vi.stubGlobal('window', { addEventListener() {}, removeEventListener() {} })
  vi.stubGlobal('fetch', vi.fn(async (_url: string, init?: { method?: string }) => ({
    ok: true, json: async () => init?.method === 'POST' ? { tree: reply } : { count: 1, pctParent: 0 } })))
  setActivePinia(createPinia())
  const ws = useWsStore()
  vi.spyOn(ws, 'on').mockImplementation(() => {}); vi.spyOn(ws, 'off').mockImplementation(() => {})
  const g = effectScope().run(() => reactive(createGatingStore('flow')))!
  g.imageUid = 'img'; g.valueName = 'seg'
  g.applyBroadcast({ imageUid: 'img', valueName: 'seg', popType: 'flow', tree: tree(0.1, '#ff0000') })
  // selecting the document pings the viewer unscoped (its own watcher) — let that settle and clear it
  await nextTick(); store.delete(OVERLAY_TICK_KEY)
  return { g, tick: () => parseOverlayTick(store.get(OVERLAY_TICK_KEY)) }
}

describe('viewer tick after a gate write', () => {
  beforeEach(() => { vi.unstubAllGlobals() })

  it('a colour change sends its look, so the viewer fetches nothing', async () => {
    const { g, tick } = await setup()
    reply = tree(0.1, '#00ff00')
    await g.updatePop('/A', { colour: '#00ff00' })
    expect(tick()).toEqual({ imageUid: 'img', scope: 'look', valueName: 'seg', popType: 'flow',
      look: { '/A': { colour: '#00ff00', show: true }, '/A/A1': { colour: '#111111', show: false } } })
  })

  it('a gate move asks for the populations only', async () => {
    const { g, tick } = await setup()
    reply = tree(0.2, '#ff0000')
    await g.setGate('/A', rect(0.2) as never)
    expect(tick()).toEqual({ imageUid: 'img', scope: 'pops', valueName: 'seg', popType: 'flow' })
  })

  it('a held gate move followed by a colour change still refetches the populations', async () => {
    const { g, tick } = await setup()
    const held: (() => void)[] = []
    g.holdViewerPings({ defer: run => { held.length = 0; held.push(run) }, drop: () => { held.length = 0 } })
    reply = tree(0.2, '#ff0000'); await g.setGate('/A', rect(0.2) as never)
    reply = tree(0.2, '#0000ff'); await g.updatePop('/A', { colour: '#0000ff' })
    expect(tick()).toBeNull()                       // nothing sent while held
    held.forEach(run => run())                      // Apply: the last held ping runs
    expect(tick()?.scope).toBe('pops')
  })

  it('the manager\'s viewer toggle refresh fetches nothing', async () => {
    const { g, tick } = await setup()
    await g.refreshOverlays()
    expect(tick()?.scope).toBe('look')
  })
})
