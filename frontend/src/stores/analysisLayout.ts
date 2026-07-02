import { defineStore } from 'pinia'
import { ref } from 'vue'
import { uniform, type LayoutTemplate } from '../plots/layoutTemplates'

// Per-tab grid layout for the Analysis board (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase A2). Keyed by
// the tab's canvas key (`analysis:tab:<id>`), parallel to `canvasPanels`/`analysisTabs`. Holds the
// chosen template (cols/rows + per-slot grid-area) and each slot's CONTENT, plus the canvas-level
// `shared` bag consumed by useSummaryData. In-memory (survives navigation, not reload); cleared
// per-project from stores/project.ts. A slot's content is routed by `kind`:
//   summary     → a SummaryPanel bound to a plot spec (ref = specId)
//   interactive → an InteractivePanel view (ref = view key; Phase B)
//   image       → a static PNG (napari screenshot; Phase D)
//   filmstrip   → N captioned images with separators (Phase D)
export interface SlotContent { kind: 'summary' | 'interactive' | 'image' | 'filmstrip'; ref: string; state: Record<string, unknown> }
interface LayoutEntry {
  cols: number; rows: number; slotAreas: string[]
  rowTracks?: string; colTracks?: string   // non-uniform plates (e.g. a short header row)
  rowHeight?: number                       // px per grid row (board-level slot-height slider); the board
                                           // scrolls in the page if taller than the viewport
  contents: (SlotContent | null)[]     // aligned 1:1 with slotAreas
  activeIndex: number
  shared: Record<string, unknown>      // canvas-level view-state for useSummaryData (compare/scope/sel/vis)
}

export const useAnalysisLayoutStore = defineStore('analysisLayout', () => {
  const entries = ref<Record<string, LayoutEntry>>({})

  function ensure(key: string): LayoutEntry {
    if (!entries.value[key]) {
      const t = uniform(2, 2)
      entries.value[key] = { cols: t.cols, rows: t.rows, slotAreas: t.slots, contents: t.slots.map(() => null), activeIndex: 0, shared: {} }
    }
    return entries.value[key]
  }

  // Switch template, preserving slot CONTENTS by index (extra new slots empty; dropped slots discarded).
  function applyTemplate(key: string, t: LayoutTemplate) {
    const e = ensure(key)
    const old = e.contents
    e.cols = t.cols; e.rows = t.rows; e.slotAreas = t.slots
    e.rowTracks = t.rowTracks; e.colTracks = t.colTracks   // undefined for uniform templates → clears any prior
    e.contents = t.slots.map((_, i) => old[i] ?? null)
    if (e.activeIndex >= e.contents.length) e.activeIndex = 0
  }

  function setContent(key: string, i: number, c: SlotContent | null) {
    const e = ensure(key)
    if (i >= 0 && i < e.contents.length) e.contents[i] = c
  }
  function setActive(key: string, i: number) {
    const e = ensure(key)
    if (i >= 0 && i < e.contents.length) e.activeIndex = i
  }
  // swap two slots' contents (drag-to-rearrange)
  function swap(key: string, a: number, b: number) {
    const e = ensure(key)
    if (a === b || a < 0 || b < 0 || a >= e.contents.length || b >= e.contents.length) return
    const t = e.contents[a]; e.contents[a] = e.contents[b]; e.contents[b] = t
  }

  function clear() { entries.value = {} }

  // persistence with the project (analysisBoards.json): dump/restore the tab layouts for a project
  // (all entries whose key starts with `analysis:<uid>:tab:`)
  function serialize(prefix: string): Record<string, LayoutEntry> {
    const out: Record<string, LayoutEntry> = {}
    for (const [k, v] of Object.entries(entries.value)) if (k.startsWith(prefix)) out[k] = v
    return out
  }
  function load(map: Record<string, LayoutEntry> | null | undefined) {
    for (const [k, v] of Object.entries(map ?? {})) entries.value[k] = v
  }

  return { entries, ensure, applyTemplate, setContent, setActive, swap, clear, serialize, load }
})
