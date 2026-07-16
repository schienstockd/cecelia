import { defineStore } from 'pinia'
import { ref, watch } from 'vue'
import { uniform, type LayoutTemplate } from '../plots/layoutTemplates'
import { useAnalysisTabsStore } from './analysisTabs'

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
  // A4 sheet lock: 'a4-portrait' | 'a4-landscape' constrain the board's on-screen box to page
  // proportions (WYSIWYG with the PDF); 'free' lets it fill the page width (the old behaviour).
  // Undefined (older persisted boards) is read as 'a4-portrait' so the fix applies retroactively.
  sheet?: 'free' | 'a4-portrait' | 'a4-landscape'
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

  // Deep-clone a board's whole layout (template, slot contents incl. their state, shared view-state)
  // onto a new key — backs "duplicate board". Autosave then persists the new key. Falls back to a fresh
  // default board if the source has no layout yet. NB: any sidecar assets referenced in slot state
  // (filmstrip/image assetIds) are still SHARED after this raw clone — the caller must re-copy them to
  // new ids so the duplicate is independent (see TabbedCanvas.duplicateBoard).
  function duplicateEntry(srcKey: string, tgtKey: string) {
    const src = entries.value[srcKey]
    if (src) entries.value[tgtKey] = JSON.parse(JSON.stringify(src))
    else ensure(tgtKey)
  }

  // Drop ONE board's layout (e.g. closing a tab) so it doesn't linger in the store / autosaved JSON.
  function drop(key: string) { delete entries.value[key] }

  function clear() { entries.value = {} }

  // persistence with the project (analysisBoards.json): dump/restore the tab layouts for a project
  // (all entries whose key starts with `analysis:<uid>:tab:`)
  function serialize(prefix: string): Record<string, LayoutEntry> {
    const out: Record<string, LayoutEntry> = {}
    for (const [k, v] of Object.entries(entries.value)) if (k.startsWith(prefix)) out[k] = v
    return out
  }
  // ── Board autosave (→ analysisBoards.json) ────────────────────────────────
  // The /analysis board persisted on its own (no manual save button): a debounced, dirty-tracked POST
  // of the WHOLE board payload {tabs, layouts} for the current project, mirroring the module-canvas
  // autosave (canvasPanels). Board IMAGES are sidecar files (board-assets/), so this JSON stays small
  // and cheap to rewrite. Triggered by any deep change to the layouts OR the tab list.
  const _boardLastSaved: Record<string, string> = {}
  const _restoring = ref(false)
  let _boardTimer: ReturnType<typeof setTimeout> | null = null
  function scheduleBoardAutosave() {
    if (_restoring.value) return
    // lazy import projectMeta to avoid a store-init cycle (projectMeta → analysisLayout)
    import('./projectMeta').then(({ useProjectMetaStore }) => {
      const uid = useProjectMetaStore().current?.uid
      if (!uid) return
      const groupKey = `analysis:${uid}`
      if (_boardTimer) clearTimeout(_boardTimer)
      _boardTimer = setTimeout(() => {
        const boards = { tabs: useAnalysisTabsStore().serialize(groupKey), layouts: serialize(`${groupKey}:tab:`) }
        const s = JSON.stringify(boards)
        if (_boardLastSaved[uid] === s) return   // nothing changed → no request
        _boardLastSaved[uid] = s
        fetch('/api/projects/boards', {
          method: 'POST', headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ projectUid: uid, boards }),
        }).catch(() => {})
      }, 800)
    })
  }

  function load(map: Record<string, LayoutEntry> | null | undefined) {
    _restoring.value = true   // don't echo the just-loaded board straight back to disk
    try {
      for (const [k, v] of Object.entries(map ?? {})) entries.value[k] = v
    } finally {
      setTimeout(() => { _restoring.value = false }, 900)   // > the 800ms autosave debounce
    }
  }

  // Autosave on any change to the grid layouts (slot contents incl. strip cells) or the tab list.
  const tabsStore = useAnalysisTabsStore()
  watch([entries, () => tabsStore.entries], scheduleBoardAutosave, { deep: true })

  return { entries, ensure, applyTemplate, setContent, setActive, swap, duplicateEntry, drop, clear, serialize, load }
})
