import { computed, toRef, type Ref } from 'vue'
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
 * `key` identifies the canvas (e.g. `summary:behaviourAnalysis`, `gate:flow`). State is held in the
 * `canvasPanels` store under that key, so the open plots PERSIST across navigation — leaving a
 * module page and returning re-binds the same panels instead of starting empty.
 */
export function useCanvasPanels<S>(canvasRef: Ref<HTMLElement | null>, makeState: () => S, key: string) {
  const store = useCanvasPanelsStore()
  const e = store.ensure(key)
  // typed views over the persisted entry; writes propagate back to the store.
  const panels = toRef(e, 'panels') as unknown as Ref<CanvasItem<S>[]>
  const activeId = toRef(e, 'activeId')
  // canvas-level state (e.g. global-scope selection / vis / scope) — persists with the canvas
  const shared = toRef(e, 'shared') as Ref<Record<string, unknown>>

  function add(): number {
    const id = ++e.nextId
    panels.value.push({ id, arrange: null, state: makeState() })
    activeId.value = id
    return id
  }
  function remove(id: number) {
    panels.value = panels.value.filter(p => p.id !== id)
    if (activeId.value === id) activeId.value = panels.value.at(-1)?.id ?? 0
    store.delGeom(`${key}:${id}`)        // drop the removed panel's persisted geometry
  }
  // Cascade ("windowed"): stagger overlapping windows at a default size
  function arrangeCascade() {
    panels.value = panels.value.map((p, i) =>
      ({ ...p, arrange: { x: 16 + i * 34, y: 16 + i * 34, w: 460, h: 440, seq: ++e.arrangeSeq } }))
  }
  // Tile (grid): fill the workspace with a near-square grid
  function arrangeGrid() {
    const el = canvasRef.value, n = panels.value.length
    if (!el || !n) return
    const gap = 8, W = el.clientWidth, H = el.clientHeight
    const cols = Math.ceil(Math.sqrt(n)), rows = Math.ceil(n / cols)
    const w = Math.max(300, Math.floor((W - gap * (cols + 1)) / cols))
    const h = Math.max(260, Math.floor((H - gap * (rows + 1)) / rows))
    panels.value = panels.value.map((p, i) => {
      const r = Math.floor(i / cols), c = i % cols
      return { ...p, arrange: { x: gap + c * (w + gap), y: gap + r * (h + gap), w, h, seq: ++e.arrangeSeq } }
    })
  }
  const activePanel = computed(() => panels.value.find(p => p.id === activeId.value))

  return { panels, activeId, activePanel, shared, add, remove, arrangeGrid, arrangeCascade }
}
