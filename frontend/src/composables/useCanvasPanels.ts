import { computed, unref, watch, type Ref } from 'vue'
import { useCanvasPanelsStore } from '../stores/canvasPanels'
import type { ArrangeCmd } from './useFloatingPanel'
import { tileGrid, tileCell } from '../utils/tileGrid'

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
  // `squareCells`: this canvas's panels snap themselves to 1:1 (`CanvasPanel :square`), so Tile must
  // hand out SQUARE cells — a cell wider than it is tall makes them overflow their own row. Set by
  // the gating pages; see utils/tileGrid.ts for the arithmetic and the failure it fixes.
  //
  // `tileBox`: the box Tile lays its grid in. Pass the VIEWPORT-derived workspace size
  // (`useCanvasWorkspace`'s `workspaceBase`), not the live element: the workspace GROWS to hold the
  // grid Tile produced, so measuring it here would feed that height back in and Tile would lay out
  // differently the second time you pressed it. Falls back to measuring `canvasRef` when a host
  // hasn't been wired for growth.
  opts: { squareCells?: boolean; tileBox?: () => { w: number; h: number } } = {},
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
  // Close every plot on THIS canvas — the "I opened fifteen and want to start over" reset, since
  // closing them one X at a time is the only alternative. Scoped to the current key, so the other
  // images'/segmentations' canvases keep their panels (each key is its own entry, see above).
  // Drops each panel's persisted geometry exactly as `remove` does — leaving it behind would place
  // the next panel that happens to reuse a freed id at the closed one's position.
  function removeAll() {
    const e = cur()
    for (const p of e.panels) store.delGeom(`${keyRef.value}:${p.id}`)
    e.panels = []
    e.activeId = 0
  }
  // Cascade ("windowed"): stagger overlapping windows at a default size
  function arrangeCascade() {
    const e = cur()
    e.panels = e.panels.map((p, i) =>
      ({ ...p, arrange: { x: 16 + i * 34, y: 16 + i * 34, w: 460, h: 440, seq: ++e.arrangeSeq } }))
  }
  // Tile (grid): fill the workspace with a near-square grid. `cols` (optional) pins the column
  // count — the UI's Columns knob (`CanvasArrangeButtons`) → escape hatch for the
  // narrow/unmeasured-viewport collapse to 1 column. Auto (undefined/0) keeps the default sqrt-shape.
  function arrangeGrid(cols?: number) {
    const e = cur()
    const n = e.panels.length
    const box = opts.tileBox?.() ??
      (canvasRef.value && { w: canvasRef.value.clientWidth, h: canvasRef.value.clientHeight })
    if (!box || !n) return
    const g = tileGrid(n, box.w, box.h,
      { mode: opts.squareCells ? 'square' : 'fill', cols: cols && cols > 0 ? cols : undefined })
    e.panels = e.panels.map((p, i) =>
      ({ ...p, arrange: { ...tileCell(i, g), w: g.w, h: g.h, cell: true, seq: ++e.arrangeSeq } }))
  }
  const activePanel = computed(() => panels.value.find(p => p.id === activeId.value))

  // Bounding box of the placed panels (max right/bottom over this canvas's persisted geometry), in
  // unscaled workspace px. Hosts feed it to `useCanvasZoom` so "Fit" fits the actual plots, not the
  // (zoom-dependent) workspace size — see useCanvasWorkspace.
  const contentBounds = computed(() => {
    let w = 0, h = 0
    const pre = `${keyRef.value}:`
    for (const [k, g] of Object.entries(store.geom)) {
      if (!k.startsWith(pre)) continue
      w = Math.max(w, g.x + g.w); h = Math.max(h, g.y + g.h)
    }
    return { w, h }
  })

  return { panels, activeId, activePanel, shared, add, remove, removeAll, arrangeGrid, arrangeCascade, contentBounds }
}
