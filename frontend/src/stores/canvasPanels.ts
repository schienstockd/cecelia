import { defineStore } from 'pinia'
import { ref } from 'vue'
import type { CanvasItem } from '../composables/useCanvasPanels'

// Persisted canvas state, keyed per canvas (e.g. `summary:behaviourAnalysis`, `gate:flow`). Lives in
// a store rather than the component so the open plots SURVIVE navigating away from and back to a
// module page — the panel array is re-bound from here on remount instead of starting empty.
// `shared` holds canvas-LEVEL state that isn't per-panel — the global-scope selection / vis props /
// scope toggle (SummaryCanvas) and the gating global highlights/options (GatingPlots). It persists
// here so those survive navigation too (panels alone weren't enough — the eye-selected pops live in
// the global scope by default, so they were lost on remount).
interface CanvasEntry { panels: CanvasItem<unknown>[]; activeId: number; nextId: number; arrangeSeq: number; shared: Record<string, unknown> }
export interface PanelGeom { x: number; y: number; w: number; h: number }

export const useCanvasPanelsStore = defineStore('canvasPanels', () => {
  const entries = ref<Record<string, CanvasEntry>>({})
  // panel geometry (drag position + size), keyed `${canvasKey}:${panelId}`, so dragged/resized
  // layouts survive navigation too (not just the panels' existence).
  const geom = ref<Record<string, PanelGeom>>({})

  function ensure(key: string): CanvasEntry {
    if (!entries.value[key]) entries.value[key] = { panels: [], activeId: 0, nextId: 0, arrangeSeq: 0, shared: {} }
    return entries.value[key]
  }
  const getGeom = (k: string): PanelGeom | undefined => geom.value[k]
  const setGeom = (k: string, g: PanelGeom) => { geom.value[k] = g }
  const delGeom = (k: string) => { delete geom.value[k] }
  // drop ONE canvas: its entry + every panel-geometry key prefixed `${key}:` (e.g. closing an
  // Analysis-canvas tab, so its board doesn't linger in the store).
  function drop(key: string) {
    delete entries.value[key]
    for (const k of Object.keys(geom.value)) if (k.startsWith(`${key}:`)) delete geom.value[k]
  }
  // drop all canvases' panels + geometry (e.g. on project close, so stale plots don't carry over)
  function clear() { entries.value = {}; geom.value = {} }
  return { entries, geom, ensure, getGeom, setGeom, delGeom, drop, clear }
})
