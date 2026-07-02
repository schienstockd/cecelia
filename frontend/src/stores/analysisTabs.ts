import { defineStore } from 'pinia'
import { ref } from 'vue'

// Tab metadata for the multipage Analysis canvas (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase A). Each
// "tab" is an independent board; its plots live in the `canvasPanels` store under the canvas key
// `${groupKey}:tab:${id}` (so tabs reuse the whole existing SummaryCanvas persistence machinery —
// this store only owns the tab LIST + names + which one is active).
//
// Keyed per group (`analysis:${projectUid}`) so boards are per-project. In-memory like `canvasPanels`
// (survives navigation, not a full reload); cleared on project open/close from `stores/project.ts`.
interface Tab { id: number; name: string }
interface TabGroup { tabs: Tab[]; activeId: number; nextId: number }

export const useAnalysisTabsStore = defineStore('analysisTabs', () => {
  const entries = ref<Record<string, TabGroup>>({})

  // Seed a first board so the canvas is never tab-less, and return the REACTIVE entry (re-read from
  // `entries.value` — returning the local literal would hand back the raw, untracked object).
  function ensure(groupKey: string): TabGroup {
    if (!entries.value[groupKey]) entries.value[groupKey] = { tabs: [], activeId: 0, nextId: 0 }
    const g = entries.value[groupKey]
    if (g.tabs.length === 0) {
      const id = ++g.nextId
      g.tabs.push({ id, name: 'Board 1' })
      g.activeId = id
    }
    return g
  }

  function addTab(groupKey: string, name?: string): number {
    const g = ensure(groupKey)
    const id = ++g.nextId
    g.tabs.push({ id, name: name ?? `Board ${g.tabs.length + 1}` })
    g.activeId = id
    return id
  }

  function renameTab(groupKey: string, id: number, name: string) {
    const t = entries.value[groupKey]?.tabs.find(x => x.id === id)
    if (t) t.name = name.trim() || t.name
  }

  // Remove a tab; caller drops the tab's canvas panels (canvasPanels.drop) since this store doesn't
  // know the canvas-key mapping. Keeps at least one board.
  function removeTab(groupKey: string, id: number) {
    const g = entries.value[groupKey]
    if (!g || g.tabs.length <= 1) return
    const idx = g.tabs.findIndex(t => t.id === id)
    if (idx < 0) return
    g.tabs.splice(idx, 1)
    if (g.activeId === id) g.activeId = (g.tabs[idx] ?? g.tabs[idx - 1] ?? g.tabs[0]).id
  }

  function setActive(groupKey: string, id: number) {
    const g = entries.value[groupKey]
    if (g && g.tabs.some(t => t.id === id)) g.activeId = id
  }

  // Move the tab with `id` to a new index (drag-reorder in the tab bar).
  function reorderTab(groupKey: string, id: number, toIndex: number) {
    const g = entries.value[groupKey]
    if (!g) return
    const from = g.tabs.findIndex(t => t.id === id)
    if (from < 0) return
    const to = Math.max(0, Math.min(toIndex, g.tabs.length - 1))
    if (from === to) return
    const [t] = g.tabs.splice(from, 1)
    g.tabs.splice(to, 0, t)
  }

  function clear() { entries.value = {} }

  // persistence with the project (analysisBoards.json): dump/restore the tab group for a project
  function serialize(groupKey: string): TabGroup | null { return entries.value[groupKey] ?? null }
  function load(groupKey: string, g: TabGroup | null | undefined) { if (g) entries.value[groupKey] = g }

  return { entries, ensure, addTab, renameTab, removeTab, setActive, reorderTab, clear, serialize, load }
})
