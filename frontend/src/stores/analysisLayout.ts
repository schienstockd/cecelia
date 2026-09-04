import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, watch } from 'vue'
import { uniform, type LayoutTemplate } from '../plots/layoutTemplates'
import { useAnalysisTabsStore } from './analysisTabs'
import { relBoardKey, rekeyBoards } from '../utils/boardKeys'
import { boardsPayload, tabGroupOf, shouldReloadBoards, type BoardsDoc } from '../utils/boardDoc'
import { shortId } from '../utils/id'
import { debouncedSave } from '../utils/debouncedSave'

// Per-tab grid layout for the Analysis board (docs/todo/ANALYSIS_CANVAS_PLAN.md, Phase A2). Keyed by
// the tab's canvas key (`analysis:tab:<id>`), parallel to `canvasPanels`/`analysisTabs`. Holds the
// chosen template (cols/rows + per-slot grid-area) and each slot's CONTENT, plus the canvas-level
// `shared` bag consumed by useSummaryData. PERSISTED per project: the autosave below POSTs the whole
// {tabs, layouts} payload to /api/projects/boards → settings/analysisBoards.json, restored by `load()`
// at project open; the store is also cleared per-project from stores/project.ts. (This said "in-memory,
// not reload" until 2026-08-08 — it predated the autosave, and had a reader conclude boards weren't
// persisted at all.) A slot's content is routed by `kind`:
//   summary     → a SummaryPanel bound to a plot spec (ref = specId)
//   interactive → an InteractivePanel view (ref = view key; Phase B)
//   image       → a static PNG (viewer screenshot; Phase D)
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

  // persistence with the project (analysisBoards.json): dump the tab layouts for a project, keyed
  // PROJECT-RELATIVE (`tab:<id>`, stripped of the `analysis:<uid>:` prefix) so the file never embeds the
  // uid — see utils/boardKeys. `groupKey` = `analysis:<uid>` for the current project.
  function serialize(groupKey: string): Record<string, LayoutEntry> {
    const out: Record<string, LayoutEntry> = {}
    const prefix = `${groupKey}:tab:`
    for (const [k, v] of Object.entries(entries.value)) if (k.startsWith(prefix)) out[relBoardKey(k)] = v
    return out
  }
  // ── Board autosave (→ analysisBoards.json) ────────────────────────────────
  // The /analysis board persisted on its own (no manual save button): a debounced, dirty-tracked POST
  // of the WHOLE board payload {tabs, layouts} for the current project, mirroring the module-canvas
  // autosave (canvasPanels). Board IMAGES are sidecar files (board-assets/), so this JSON stays small
  // and cheap to rewrite. Triggered by any deep change to the layouts OR the tab list.
  const _boardLastSaved: Record<string, string> = {}
  // The document version each project was last READ or WRITTEN at — the optimistic-concurrency token
  // the next write echoes back. See utils/boardDoc.ts.
  const _boardVersion: Record<string, number> = {}
  // Identifies THIS client's writes in the boards:changed broadcast. The server broadcasts before it
  // returns, so a writer cannot recognise its own echo by version — it still holds the pre-write one.
  const _clientId = shortId()

  function setVersion(uid: string, v: number) { if (Number.isFinite(v)) _boardVersion[uid] = v }
  function versionOf(uid: string) { return _boardVersion[uid] ?? 0 }

  // Pull the current document and put it in the stores. Used by both recovery paths: a write rejected
  // as stale, and another client's `boards:changed`.
  async function reloadBoards(uid: string) {
    const groupKey = `analysis:${uid}`
    const res = await fetch(`/api/projects/boards?projectUid=${encodeURIComponent(uid)}`).catch(() => null)
    if (!res?.ok) return false
    const body = await res.json().catch(() => null) as { boards?: BoardsDoc } | null
    if (!body?.boards) return false
    useAnalysisTabsStore().load(groupKey, tabGroupOf(body.boards) as never)
    load(groupKey, body.boards.layouts as never)
    setVersion(uid, body.boards.version)
    _boardLastSaved[uid] = ''   // whatever we were about to save no longer describes the document
    return true
  }

  // Write-behind autosave (shared helper — utils/debouncedSave). The helper owns the timer and the
  // restore suppression; the version/conflict handling below is this store's own.
  const _autosave = debouncedSave(async () => {
    // lazy import projectMeta to avoid a store-init cycle (projectMeta → analysisLayout)
    const { useProjectMetaStore } = await import('./projectMeta')
    const uid = useProjectMetaStore().current?.uid
    if (!uid) return
    const groupKey = `analysis:${uid}`
    const boards = boardsPayload(useAnalysisTabsStore().serialize(groupKey), serialize(groupKey))
    const s = JSON.stringify(boards)
    if (_boardLastSaved[uid] === s) return   // nothing changed → no request
    _boardLastSaved[uid] = s
    const res = await fetch('/api/projects/boards', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: uid, boards, version: versionOf(uid), clientId: _clientId }),
    }).catch(() => null)
    if (!res) return
    if (res.status === 409) {
      // Another client (or tab) wrote since we loaded. Take THEIR document rather than overwrite
      // it — this whole file is one blob, so "retry with our copy" would just move the clobber one
      // step later and lose their boards instead of ours. The debounced edit that lost the race is
      // dropped; the user sees the current state instead of silently destroying someone's work.
      _boardLastSaved[uid] = ''
      if (await reloadBoards(uid)) {
        const { useLogStore } = await import('./log')
        useLogStore().warn('Boards were changed elsewhere — reloaded, your last edit was not saved.',
                           { source: 'analysis' })
      }
      return
    }
    if (res.ok) {
      const body = await res.json().catch(() => null) as { version?: number } | null
      if (typeof body?.version === 'number') setVersion(uid, body.version)
    }
  }, { wait: 800 })
  const scheduleBoardAutosave = () => _autosave.schedule()

  // Re-key the loaded layouts onto the CURRENT project's group (`analysis:<uid>`) — tolerating both the
  // new relative form (`tab:<id>`) and a legacy baked-in `analysis:<oldUid>:tab:<id>` — so a project's
  // boards survive a uid change (import-as-copy / rename) instead of orphaning. See utils/boardKeys.
  function load(groupKey: string, map: Record<string, LayoutEntry> | null | undefined) {
    // don't echo the just-loaded board straight back to disk. The suppression window is derived from
    // the autosave's own debounce (utils/debouncedSave), so the two can't drift apart.
    _autosave.duringRestore(() => {
      for (const [k, v] of Object.entries(rekeyBoards(groupKey, map))) entries.value[k] = v
    })
  }

  // Autosave on any change to the grid layouts (slot contents incl. strip cells) or the tab list.
  const tabsStore = useAnalysisTabsStore()
  watch([entries, () => tabsStore.entries], scheduleBoardAutosave, { deep: true })

  // Another client wrote the boards → pick it up, so a second browser tab converges instead of sitting
  // on a stale document until reload. `load()` already suppresses the echo autosave (duringRestore
  // holds past the debounce window), and shouldReloadBoards drops our own broadcast.
  import('./ws').then(({ useWsStore }) => {
    useWsStore().on('boards:changed', (frame: unknown) => {
      import('./projectMeta').then(({ useProjectMetaStore }) => {
        const uid = useProjectMetaStore().current?.uid
        if (uid && shouldReloadBoards(frame as never, uid, versionOf(uid), _clientId)) void reloadBoards(uid)
      })
    })
  })

  return { entries, ensure, applyTemplate, setContent, setActive, swap, duplicateEntry, drop, clear,
           serialize, load, setVersion, reloadBoards }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useAnalysisLayoutStore, import.meta.hot))
