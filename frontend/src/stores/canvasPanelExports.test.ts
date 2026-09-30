import { describe, it, expect, beforeEach } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useCanvasPanelExportsStore } from './canvasPanelExports'

describe('useCanvasPanelExportsStore', () => {
  beforeEach(() => setActivePinia(createPinia()))

  it('registers + retrieves exporters by key', () => {
    const s = useCanvasPanelExportsStore()
    const exporter = async () => 'data:image/png;base64,AAA'
    s.register('summary:behaviour:none:7', exporter)
    expect(s.get('summary:behaviour:none:7')).toBe(exporter)
  })

  it('unregister removes the entry', () => {
    const s = useCanvasPanelExportsStore()
    s.register('k', async () => null)
    s.unregister('k')
    expect(s.get('k')).toBeUndefined()
  })

  it('unregister of an absent key is a no-op (defensive against remount races)', () => {
    const s = useCanvasPanelExportsStore()
    expect(() => s.unregister('nope')).not.toThrow()
  })

  it('unregister with an owner leaves another canvas\'s registration under the same key', () => {
    // two kept-alive canvases sharing a key: the one unmounting must not remove the live one's entry
    const s = useCanvasPanelExportsStore()
    const a = async () => 'a', b = async () => 'b'
    s.register('k', a)
    s.register('k', b)
    s.unregister('k', a)
    expect(s.get('k')).toBe(b)
    s.unregister('k', b)
    expect(s.get('k')).toBeUndefined()
  })

  it('re-registering overwrites (a remount replaces the previous ref)', async () => {
    const s = useCanvasPanelExportsStore()
    s.register('k', async () => 'first')
    s.register('k', async () => 'second')
    const png = await s.get('k')?.()
    expect(png).toBe('second')
  })
})
