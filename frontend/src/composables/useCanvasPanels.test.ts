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
