import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { nextTick } from 'vue'
import { useCanvasPanelsStore } from './canvasPanels'

// the save reads only the open project's uid; the real store needs a browser window
vi.mock('./projectMeta', () => ({ useProjectMetaStore: () => ({ current: { uid: 'PRJ' } }) }))

// The autosave watches `geom` only one level deep (a drag writes it per mousemove). These pin that
// every way geometry changes — a moved panel, a removed one — still reaches disk, and that a panel's
// nested state, watched deep, does too.

const KEY = 'summary:phenotype:IMG1'
let posts: Array<{ objects: Record<string, { entries: Record<string, unknown>; geom: Record<string, unknown> }> }>

async function settleSave() {
  await nextTick()                      // the watcher runs → schedules the debounced save
  await vi.advanceTimersByTimeAsync(500) // past the 400ms debounce; the save's lazy import resolves
  await vi.dynamicImportSettled()
  await vi.advanceTimersByTimeAsync(0)
}

describe('canvasPanels autosave', () => {
  beforeEach(() => {
    vi.useFakeTimers()
    setActivePinia(createPinia())
    posts = []
    vi.stubGlobal('fetch', vi.fn(async (_url: string, init: RequestInit) => {
      posts.push(JSON.parse(String(init.body)))
      return new Response('{}')
    }))
  })
  afterEach(() => { vi.unstubAllGlobals(); vi.useRealTimers() })

  it('saves a moved panel — each move replaces the geometry object', async () => {
    const s = useCanvasPanelsStore()
    s.setGeom(`${KEY}:1`, { x: 10, y: 20, w: 300, h: 200 })
    await settleSave()
    s.setGeom(`${KEY}:1`, { x: 40, y: 60, w: 300, h: 200 })
    await settleSave()
    expect(posts.at(-1)!.objects.IMG1.geom[`${KEY}:1`]).toEqual({ x: 40, y: 60, w: 300, h: 200 })
  })

  it('saves a removed panel geometry', async () => {
    const s = useCanvasPanelsStore()
    s.setGeom(`${KEY}:1`, { x: 10, y: 20, w: 300, h: 200 })
    s.setGeom(`${KEY}:2`, { x: 400, y: 20, w: 300, h: 200 })
    await settleSave()
    s.delGeom(`${KEY}:2`)
    await settleSave()
    expect(Object.keys(posts.at(-1)!.objects.IMG1.geom)).toEqual([`${KEY}:1`])
  })

  it('saves an edit deep inside a panel state', async () => {
    const s = useCanvasPanelsStore()
    s.ensure(KEY).panels.push({ id: 0, arrange: null, state: { vis: { opts: { size: 1 } } } })
    await settleSave()
    ;(s.entries[KEY].panels[0].state as { vis: { opts: { size: number } } }).vis.opts.size = 3
    await settleSave()
    const saved = posts.at(-1)!.objects.IMG1.entries[KEY] as { panels: Array<{ state: unknown }> }
    expect(saved.panels[0].state).toEqual({ vis: { opts: { size: 3 } } })
  })
})
