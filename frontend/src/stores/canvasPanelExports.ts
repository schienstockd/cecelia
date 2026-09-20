// Per-panel PNG export registry — one entry per mounted plot panel that can render itself to a PNG.
//
// Why a store rather than template refs: SummaryCanvas mounts panels via `v-for` where each panel's
// `id` is a store-generated number, not its index. A `[...refs]` array collected by Vue gives us
// order + count but no stable id ↔ ref mapping. The canvas Share compositor asks "please export
// the panel with id 7"; that has to be an id-keyed lookup. Registering + unregistering by id keeps
// the wiring straight even across duplicate / explode / remove operations.
//
// Registration is opt-in per host, not a base-class contract on CanvasPanel — a panel that has no
// export path (e.g. a placeholder / notice) simply doesn't register, and the share compositor
// will silently skip it. Cheaper than teaching every panel to say "I can't export."

import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, onScopeDispose } from 'vue'

/** A registered panel's export contract. Returns a PNG data URL or `null` on failure (a plot that
 *  hasn't rendered yet, or a raster the browser refused). Async because the underlying path goes
 *  through an `<img>` decode. */
export interface PanelExporter { (): Promise<string | null> }

export const useCanvasPanelExportsStore = defineStore('canvasPanelExports', () => {
  // Keyed `${canvasKey}:${panelId}` — same shape as `useCanvasPanelsStore.geom`, so a consumer
  // can look up the export next to the geom without a second index. The alternative — a nested
  // map — buys nothing and complicates the register/unregister path.
  const exporters = ref<Record<string, PanelExporter>>({})

  function register(key: string, exporter: PanelExporter) { exporters.value[key] = exporter }
  function unregister(key: string) {
    if (key in exporters.value) delete exporters.value[key]
  }
  function get(key: string): PanelExporter | undefined { return exporters.value[key] }

  return { exporters, register, unregister, get }
})

/** Lifecycle-safe registration helper — the caller unregisters automatically on unmount. */
export function usePanelExport(key: () => string, exporter: PanelExporter): void {
  const store = useCanvasPanelExportsStore()
  // Register under the current key; re-register when the key changes (a persistKey rebind).
  let lastKey = key()
  store.register(lastKey, exporter)
  // The key is expected to be stable for the lifetime of the panel (panel id + canvas key don't
  // change under a mounted CanvasPanel), so a watch is overkill; refresh only if the caller opts
  // to re-invoke this composable, and clean the previous entry.
  onScopeDispose(() => store.unregister(lastKey))
  // Return void — the caller doesn't need the store. Kept as a bare function on purpose.
  void lastKey
}

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useCanvasPanelExportsStore, import.meta.hot))
