import { computed, unref, watch, type Ref } from 'vue'
import { useCanvasPanelsStore } from '../stores/canvasPanels'
import type { ArrangeCmd } from './useFloatingPanel'

/** One panel on a canvas: a stable id, an optional Tile/Cascade command, and opaque per-panel
 *  state owned by the host (e.g. gating's displayed-parent/highlight; a summary plot's spec). */
export interface CanvasItem<S> { id: number; arrange: ArrangeCmd | null; state: S }

/**
 * Workspace logic shared by every plot canvas (gating, summary, track-gating, the universal canvas):
 * an array of floating panels with add / remove / Tile / Cascade and a tracked active panel. The
 * panel CHROME is `CanvasPanel`; what's inside each panel and the per-panel `state` shape are the
 * host's concern. `canvasRef` is the workspace element (used to size the Tile grid).
 *
 * `key` identifies the canvas. It may be a plain string OR a reactive source (Ref / getter) — pass a
 * reactive key that embeds the active image (+ segmentation), e.g. `summary:{module}:{imageUid}` or
 * `gate:{popType}:{imageUid}:{valueName}`, and the canvas rebinds to THAT (image, …)'s own entry when
 * the selection changes. State is held in the `canvasPanels` store under the current key, so open
 * plots PERSIST across navigation AND per image, instead of being pruned to a single shared entry.
 */
export function useCanvasPanels<S>(
  canvasRef: Ref<HTMLElement | null>, makeState: () => S,
  key: string | Ref<string> | (() => string),
) {
  const store = useCanvasPanelsStore()
  const keyRef = computed(() => (typeof key === 'function' ? key() : unref(key)))
  // Ensure the entry for the CURRENT key exists; re-ensure when the key changes (image/segmentation
  // switch) so each gets its own panels. The store watcher persists; nothing is pruned across images.
  watch(keyRef, k => store.ensure(k), { immediate: true })
  const cur = () => store.ensure(keyRef.value)

  // Writable views over the CURRENT entry (re-evaluated when the key changes).
  const panels = computed<CanvasItem<S>[]>({
    get: () => cur().panels as unknown as CanvasItem<S>[],
    set: v => { cur().panels = v as unknown as CanvasItem<unknown>[] },
  })
  const activeId = computed<number>({ get: () => cur().activeId, set: v => { cur().activeId = v } })
  const shared = computed<Record<string, unknown>>({ get: () => cur().shared, set: v => { cur().shared = v } })

  function add(): number {
    const e = cur()
    const id = ++e.nextId
    e.panels.push({ id, arrange: null, state: makeState() })
    e.activeId = id
    return id
  }
  function remove(id: number) {
    const e = cur()
    e.panels = e.panels.filter(p => p.id !== id)
    if (e.activeId === id) e.activeId = e.panels.at(-1)?.id ?? 0
    store.delGeom(`${keyRef.value}:${id}`)        // drop the removed panel's persisted geometry
  }
  // Cascade ("windowed"): stagger overlapping windows at a default size
  function arrangeCascade() {
    const e = cur()
    e.panels = e.panels.map((p, i) =>
      ({ ...p, arrange: { x: 16 + i * 34, y: 16 + i * 34, w: 460, h: 440, seq: ++e.arrangeSeq } }))
  }
  // Tile (grid): fill the workspace with a near-square grid
  function arrangeGrid() {
    const e = cur()
    const el = canvasRef.value, n = e.panels.length
    if (!el || !n) return
    const gap = 8, W = el.clientWidth, H = el.clientHeight
    const cols = Math.ceil(Math.sqrt(n)), rows = Math.ceil(n / cols)
    const w = Math.max(300, Math.floor((W - gap * (cols + 1)) / cols))
    const h = Math.max(260, Math.floor((H - gap * (rows + 1)) / rows))
    e.panels = e.panels.map((p, i) => {
      const r = Math.floor(i / cols), c = i % cols
      return { ...p, arrange: { x: gap + c * (w + gap), y: gap + r * (h + gap), w, h, seq: ++e.arrangeSeq } }
    })
  }
  const activePanel = computed(() => panels.value.find(p => p.id === activeId.value))

  return { panels, activeId, activePanel, shared, add, remove, arrangeGrid, arrangeCascade }
}
