// A pinned lens on a fresh window (a lens figure's Go to, a saved pin): the first hand-over to the
// renderer saw the canvas at its default 300 px backing store, and nothing re-applied it once the
// renderer resized — the lens drew at a quarter of its place and size until a second click.
import { describe, expect, it, vi } from 'vitest'
import { createPinia, setActivePinia } from 'pinia'
import { nextTick } from 'vue'
import { useVwLens } from './useVwLens'
import { useSettingsStore } from '../stores/settings'
import type { ViewerLens, ViewerMeta } from '../utils/volumeViewer'

describe('useVwLens — the renderer copy follows the canvas backing store', () => {
  it('re-applies a pinned lens when the backing store resizes under it', async () => {
    const m = new Map<string, string>()
    vi.stubGlobal('localStorage', { getItem: (k: string) => m.get(k) ?? null, setItem: (k: string, v: string) => { m.set(k, String(v)) },
                                    removeItem: (k: string) => { m.delete(k) } })
    setActivePinia(createPinia())
    useSettingsStore().viewerLens = true
    const canvas = { width: 300, height: 150, clientWidth: 1200, clientHeight: 600 } as HTMLCanvasElement
    const meta = { nX: 1200, nY: 600, nZ: 1, nT: 1, voxelUm: [1, 1, 1], channels: [] } as unknown as ViewerMeta
    let last: ViewerLens | null = null
    const lens = useVwLens({
      meta: () => meta, mode: () => 'plane', canvas: () => canvas,
      canvasSize: () => ({ w: 1200, h: 600 }), cam: () => ({ yaw: 0, pitch: 0, dist: 1, panX: 0, panY: 0 }),
      renderers: () => [{ setLens: l => { last = l } }], redraw: () => {}, pushChannels: () => {},
      counts: () => ({ picked: [], cells: 0, more: 0, groups: [], shown: [] }) as never,
    })
    lens.lensPin.value = { x: 600, y: 300 }
    await nextTick()
    const at = lens.lensAt.value!
    // the renderer resizes its backing store to the CSS size (dpr 1); the lens stays put
    canvas.width = 1200; canvas.height = 600
    lens.syncLensScale()
    expect(last).not.toBeNull()
    expect(last!.x).toBeCloseTo(at.cx)   // device px = CSS px at dpr 1, not a quarter of it
    expect(last!.y).toBeCloseTo(at.cy)
    expect(last!.r).toBeCloseTo(lens.lensRadiusPx.value)
  })
})
