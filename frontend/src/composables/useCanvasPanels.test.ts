import { describe, it, expect, beforeEach, vi } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { ref } from 'vue'
import { useCanvasPanels } from './useCanvasPanels'
import { useCanvasPanelsStore } from '../stores/canvasPanels'

// the store's autosave reads the open project; the real one needs a browser window
vi.mock('../stores/projectMeta', () => ({ useProjectMetaStore: () => ({ current: null }) }))

const KEY = 'summary:phenotype:IMG1'

// Duplicate used to `add()` a panel at the default size — the copy of a plot you had resized came up
// a different shape, so comparing the two meant resizing one by hand first.
describe('useCanvasPanels.duplicate', () => {
  beforeEach(() => setActivePinia(createPinia()))

  it('gives the copy the source size, offset so both stay visible', () => {
    const c = useCanvasPanels(ref(null), () => ({}), KEY)
    const src = c.add()
    const store = useCanvasPanelsStore()
    store.setGeom(`${KEY}:${src}`, { x: 100, y: 50, w: 420, h: 610 })
    const id = c.duplicate(src)
    expect(id).not.toBe(src)
    expect(c.activeId.value).toBe(id)
    const g = store.getGeom(`${KEY}:${id}`)!
    expect({ w: g.w, h: g.h }).toEqual({ w: 420, h: 610 })
    expect(g.x).toBeGreaterThan(100)
    expect(g.y).toBeGreaterThan(50)
  })

  it('a source with no saved geometry leaves the copy at the default placement', () => {
    const c = useCanvasPanels(ref(null), () => ({}), KEY)
    const id = c.duplicate(c.add())
    expect(useCanvasPanelsStore().getGeom(`${KEY}:${id}`)).toBeUndefined()
  })
})

// Show series used to `add()` one panel per measure and then Tile the whole canvas: every copy came up
// at the grid cell's size, not the plot you asked to see the series of (and the source was resized too).
describe('useCanvasPanels.duplicate — series', () => {
  beforeEach(() => setActivePinia(createPinia()))

  const SRC = { x: 100, y: 50, w: 420, h: 610 }
  const overlap = (a: typeof SRC, b: typeof SRC) =>
    a.x < b.x + b.w && b.x < a.x + a.w && a.y < b.y + b.h && b.y < a.y + a.h

  it('every series panel takes the source size, laid out beside it without overlapping', () => {
    const c = useCanvasPanels(ref(null), () => ({}), KEY, { tileBox: () => ({ w: 1400, h: 900 }) })
    const src = c.add()
    const store = useCanvasPanelsStore()
    store.setGeom(`${KEY}:${src}`, SRC)
    const ids = c.duplicate(src, 4)
    expect(ids).toHaveLength(4)
    const geoms = ids.map(id => store.getGeom(`${KEY}:${id}`)!)
    for (const g of geoms) expect({ w: g.w, h: g.h }).toEqual({ w: 420, h: 610 })
    expect(store.getGeom(`${KEY}:${src}`)).toEqual(SRC)                 // the source is left alone
    const all = [SRC, ...geoms]
    for (let i = 0; i < all.length; i++)
      for (let j = i + 1; j < all.length; j++) expect(overlap(all[i], all[j]), `${i} vs ${j}`).toBe(false)
    // wraps at the workspace edge: 1400 wide from x=100 fits 3 source-sized cells per row
    for (const g of geoms) expect(g.x + g.w).toBeLessThanOrEqual(1400)
  })
})
