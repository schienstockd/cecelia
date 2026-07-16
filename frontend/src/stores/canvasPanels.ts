import { defineStore } from 'pinia'
import { ref, watch } from 'vue'
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
  // drop ONE module canvas: its entry + every panel-geometry key prefixed `${key}:`, so a discarded
  // canvas doesn't linger in the store. (The `/analysis` board lives in `analysisLayout`, which has its
  // own `drop` — closing a board tab goes through that, not here.)
  function drop(key: string) {
    delete entries.value[key]
    for (const k of Object.keys(geom.value)) if (k.startsWith(`${key}:`)) delete geom.value[k]
  }
  // Deep-copy one canvas's full layout (entry + all panel geometry) to another key. Used by "copy
  // gating strategy → other images" with the plot-layout option: clones gate:{pt}:{src}:{vn} onto
  // gate:{pt}:{tgt}:{vn}. Autosave then persists the new key to the TARGET object's file (the key's
  // 3rd colon-segment). No-op if the source has no saved layout.
  function copyEntry(srcKey: string, tgtKey: string) {
    const src = entries.value[srcKey]
    if (!src) return
    entries.value[tgtKey] = JSON.parse(JSON.stringify(src))
    for (const [k, v] of Object.entries(geom.value)) {
      if (k.startsWith(`${srcKey}:`)) geom.value[`${tgtKey}${k.slice(srcKey.length)}`] = { ...v }
    }
  }
  // last-persisted JSON per object uid — the dirty-tracking baseline so autosave sends ONLY objects
  // whose canvas state actually changed since the last save (not every visited object).
  let _lastSaved: Record<string, string> = {}

  // drop all canvases' panels + geometry (e.g. on project close, so stale plots don't carry over)
  function clear() { entries.value = {}; geom.value = {}; _lastSaved = {} }

  // ── Per-object persistence (debounced autosave → 1/{objUid}/moduleCanvases.json) ────────────────
  // MODULE-PAGE canvases only — keys embed the object: `summary:{module}:{img}`, `gate:{pt}:{img}:{vn}`,
  // `clust:{pt}:{set}`. The `/analysis` board (`analysis:*`) persists separately (analysisBoards.json)
  // and is excluded. One engine → every module page persists per-object, no per-page code.
  const MODULE_PREFIXES = ['summary:', 'gate:', 'clust:']
  const isModuleKey = (k: string) => MODULE_PREFIXES.some(p => k.startsWith(p))

  // The object (image/set) a module-canvas key or geom-key is scoped to = the 3rd colon-segment:
  //   summary:{module}:{img} · gate:{pt}:{img}:{vn} · clust:{pt}:{set}   (geom adds `:{panelId}`)
  // RULE: a canvas key is colon-delimited, so its segments (module / popType / object uid / value_name)
  // MUST NOT contain ':'. This holds — object uids are alphanumeric `gen_uid`, popTypes/modules are
  // fixed identifiers, value_names are simple segmentation labels. Do not introduce a ':' into any of
  // them (this positional parse, and the persist-keys built as `${ckey}:${panelId}`, depend on it).
  function objectOf(key: string): string | null {
    if (!isModuleKey(key)) return null
    const parts = key.split(':')
    return parts.length >= 3 ? parts[2] : null
  }

  // Group module-page canvases BY OBJECT so each is persisted with its object at
  // 1/{objUid}/moduleCanvases.json (the backend writes one file per object).
  function serializeByObject() {
    const out: Record<string, { entries: Record<string, CanvasEntry>; geom: Record<string, PanelGeom> }> = {}
    const bucket = (o: string) => (out[o] ??= { entries: {}, geom: {} })
    for (const [k, v] of Object.entries(entries.value)) { const o = objectOf(k); if (o) bucket(o).entries[k] = v }
    for (const [k, v] of Object.entries(geom.value))    { const o = objectOf(k); if (o) bucket(o).geom[k] = v }
    return out
  }

  // Restore from disk. Merges module-page keys (never clobbers the board or in-session non-module keys),
  // then sets the dirty-tracking baseline so the restored state is NOT re-sent on open.
  const _restoring = ref(false)
  function load(data: { entries?: Record<string, CanvasEntry>; geom?: Record<string, PanelGeom> } | null | undefined) {
    if (!data) return
    _restoring.value = true
    try {
      if (data.entries) for (const [k, v] of Object.entries(data.entries)) if (isModuleKey(k)) entries.value[k] = v
      if (data.geom)    for (const [k, v] of Object.entries(data.geom))    if (isModuleKey(k)) geom.value[k] = v
      for (const [obj, d] of Object.entries(serializeByObject())) _lastSaved[obj] = JSON.stringify(d)
    } finally {
      // Clear after the debounce window so the restore's own mutations don't trigger a write-back.
      setTimeout(() => { _restoring.value = false }, 600)
    }
  }

  // Debounced autosave: ~400ms after the last edit, send ONLY the objects whose serialization changed
  // since the last save (dirty-tracking) — off the interaction path (UI already re-rendered), no lag.
  let _timer: ReturnType<typeof setTimeout> | null = null
  function _scheduleAutosave() {
    if (_restoring.value) return
    // lazy import to avoid a store-init cycle (projectMeta → project → this store)
    import('./projectMeta').then(({ useProjectMetaStore }) => {
      const uid = useProjectMetaStore().current?.uid
      if (!uid) return
      if (_timer) clearTimeout(_timer)
      _timer = setTimeout(() => {
        const changed: Record<string, { entries: Record<string, CanvasEntry>; geom: Record<string, PanelGeom> }> = {}
        for (const [obj, d] of Object.entries(serializeByObject())) {
          const s = JSON.stringify(d)
          if (_lastSaved[obj] !== s) { changed[obj] = d; _lastSaved[obj] = s }
        }
        if (Object.keys(changed).length === 0) return   // nothing actually changed → no request
        fetch('/api/projects/canvases', {
          method: 'POST', headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ projectUid: uid, objects: changed }),
        }).catch(() => {})
      }, 400)
    })
  }
  watch([entries, geom], _scheduleAutosave, { deep: true })

  return { entries, geom, ensure, getGeom, setGeom, delGeom, drop, copyEntry, clear, load }
})
