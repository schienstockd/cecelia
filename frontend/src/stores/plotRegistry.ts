// Live plot-panel registry (BIDIR PR #8) — populated by `useVisualPanel(persistKey, meta)` on
// every mounted plot host (InteractivePanel, SummaryPanel, cluster panels, gate panels, cards).
// The server's Julia mirror (`api/src/plots_registry_api.jl`) is the source of truth Claude reads
// through the `list_plots` MCP tool; this store is the FRONTEND side that keeps it in step by
// POSTing register / deregister as panels mount / unmount / rebind their persistKey.
//
// Session-only. No persistence. Fire-and-forget POSTs — a failed register is silent so a panel
// still renders during a dev restart. The server-side clientId cleanup drops stale entries when
// this tab's socket disconnects; see `stores/ws.ts::wsClientId` + `api/src/server.jl`'s
// `register_ws_disconnect_hook!`.
//
// Shape mirrors `stores/canvasPanelExports.ts`: a tiny keyed map, a bare register/unregister pair,
// and a helper composable that ties both to the caller's scope. The DIFFERENCE from
// `usePanelExport`: this composable watches the meta getter and re-registers on change (a panel's
// title / route can update while the persistKey stays stable, and Claude should read the current
// title).

import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, watch, onScopeDispose } from 'vue'
import { wsClientId } from '../utils/clientId'
import { useProjectMetaStore } from './projectMeta'

/** Meta a caller ships with a persistKey — enough for Claude to name "the UMAP" back to the user
 *  and pick the right one when the same family has more than one panel. `route` disambiguates a
 *  panel that lives on the wrong page for the current question; `cellKeys` / `bboxScreen` are
 *  future hints, unused today. Stays JSON-serialisable — the payload goes straight to fetch(). */
export interface PlotMeta {
  family: string
  title: string
  route: string
  cellKeys?: string[]
  bboxScreen?: { x: number; y: number; w: number; h: number }
  // content — panel-specific discriminating fields (e.g. summary panel's measure/chartType/popType/
  // valueNames/statsEnabled) so a caller with N same-family panels can pick the right one without a
  // shared frame. Optional; a host that has nothing useful to declare leaves it off. Kept
  // JSON-serialisable; goes on the register payload verbatim.
  content?: Record<string, unknown>
}

// The store holds just enough state to dedupe redundant register POSTs (a panel's meta getter
// might re-fire on unrelated re-renders; the last-sent meta is what we compare against). Not
// exposed for reads — Claude reads through the Julia mirror, not the Pinia state.
export const usePlotRegistryStore = defineStore('plotRegistry', () => {
  // key → the last successful register payload for this key (used for dedupe + for the deregister
  // POST that has to fire with the same clientId). A rebind of the key writes under the new key.
  const lastSent = ref<Record<string, { clientId: string; meta: PlotMeta; projectUid: string }>>({})

  function noteSent(key: string, entry: { clientId: string; meta: PlotMeta; projectUid: string }) {
    lastSent.value[key] = entry
  }
  function forget(key: string) {
    if (key in lastSent.value) delete lastSent.value[key]
  }
  function getLast(key: string) { return lastSent.value[key] }

  return { lastSent, noteSent, forget, getLast }
})

// Fire-and-forget POST — swallows every failure. A dev restart makes the backend unreachable
// mid-mount; a registry POST that 500s must not break the panel. Same discipline as
// `_client._maybe_pair` on the MCP side.
function _post(path: string, body: Record<string, unknown>): void {
  void fetch(path, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  }).catch(() => {})
}

// Structural equality on meta — the getter can return a new object each tick even when the fields
// are identical, and re-POSTing on every tick would flood the wire.
function _metaEq(a: PlotMeta | undefined, b: PlotMeta): boolean {
  if (!a) return false
  if (a.family !== b.family || a.title !== b.title || a.route !== b.route) return false
  const aKeys = a.cellKeys ?? [], bKeys = b.cellKeys ?? []
  if (aKeys.length !== bKeys.length) return false
  for (let i = 0; i < aKeys.length; i++) if (aKeys[i] !== bKeys[i]) return false
  const aBox = a.bboxScreen, bBox = b.bboxScreen
  if (aBox || bBox) {
    if (!aBox || !bBox) return false
    if (aBox.x !== bBox.x || aBox.y !== bBox.y || aBox.w !== bBox.w || aBox.h !== bBox.h) return false
  }
  // `content` is a small JSON-serialisable dict; JSON.stringify equality is the cheap, correct check
  // (a hand-written recursive comparator would be overkill for the sizes involved here).
  const aHasC = !!a.content && Object.keys(a.content).length > 0
  const bHasC = !!b.content && Object.keys(b.content).length > 0
  if (aHasC !== bHasC) return false
  if (aHasC && JSON.stringify(a.content) !== JSON.stringify(b.content)) return false
  return true
}

/** Lifecycle-safe registration helper — mount registers, meta change re-registers, unmount
 *  deregisters. Skips silently if the persistKey is empty or no project is open (nothing to
 *  address the entry against). Mirrors `usePanelExport` shape.
 *
 *  The DIFFERENCE from `usePanelExport`: this composable watches the meta getter. A panel whose
 *  title updates from "UMAP" → "UMAP (flowTom)" is what Claude reads when the user names it back,
 *  so the freshest value has to reach the registry without waiting for a remount. */
export function usePlotRegistry(key: () => string, meta: () => PlotMeta): void {
  const store = usePlotRegistryStore()
  const projectMeta = useProjectMetaStore()

  let lastKey = ''

  function doRegister() {
    const k = key()
    const projectUid = projectMeta.current?.uid ?? ''
    if (!k || !projectUid) {
      // If we had previously registered under a now-empty key, drop it. This is the "the panel
      // still exists but its persistKey rebound to empty" edge case; the server-side entry would
      // otherwise linger until WS disconnect.
      if (lastKey) {
        const prev = store.getLast(lastKey)
        if (prev) {
          _post('/api/viewer/plots/deregister', {
            clientId: prev.clientId, projectUid: prev.projectUid, plotId: lastKey,
          })
          store.forget(lastKey)
        }
        lastKey = ''
      }
      return
    }

    const m = meta()
    // Rebound key: deregister the old entry before registering the new one — a fresh POST under a
    // new plotId is not itself a signal to drop the old id on the server.
    if (lastKey && lastKey !== k) {
      const prev = store.getLast(lastKey)
      if (prev) {
        _post('/api/viewer/plots/deregister', {
          clientId: prev.clientId, projectUid: prev.projectUid, plotId: lastKey,
        })
        store.forget(lastKey)
      }
    }

    // Dedupe: nothing changed → no POST. Compares against the last-sent snapshot for THIS key.
    const prev = store.getLast(k)
    if (prev && prev.projectUid === projectUid && _metaEq(prev.meta, m)) {
      lastKey = k
      return
    }

    const payload: Record<string, unknown> = {
      clientId: wsClientId,
      projectUid,
      plotId: k,
      family: m.family,
      title: m.title,
      route: m.route,
    }
    if (m.cellKeys && m.cellKeys.length) payload.cellKeys = m.cellKeys
    if (m.bboxScreen) payload.bboxScreen = m.bboxScreen
    if (m.content && Object.keys(m.content).length) payload.content = m.content
    _post('/api/viewer/plots/register', payload)
    store.noteSent(k, { clientId: wsClientId, meta: { ...m }, projectUid })
    lastKey = k
  }

  // Initial register at setup; re-fire on either the key OR the meta changing. `deep` on meta so
  // an object literal returned by the getter that stays reference-stable but has a mutated field
  // still triggers (rare but correct).
  doRegister()
  watch([key, meta, () => projectMeta.current?.uid], () => doRegister(), { deep: true })

  onScopeDispose(() => {
    if (!lastKey) return
    const prev = store.getLast(lastKey)
    if (prev) {
      _post('/api/viewer/plots/deregister', {
        clientId: prev.clientId, projectUid: prev.projectUid, plotId: lastKey,
      })
      store.forget(lastKey)
    }
  })
}

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(usePlotRegistryStore, import.meta.hot))
