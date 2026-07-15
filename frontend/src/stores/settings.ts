import { defineStore } from 'pinia'
import { ref, watch } from 'vue'

export const useSettingsStore = defineStore('settings', () => {
  const taskListAutoFollow = ref(
    localStorage.getItem('cc.taskListAutoFollow') !== 'false'  // default true
  )

  // Auto-refresh plots + pop lists when a task finishes successfully (the per-image task-refresh; see
  // composables/useDataRefresh). On by default; users who find plots refetching under them distracting
  // can turn it off (they then refresh on the next navigation / input change).
  const autoRefreshOnTask = ref(
    localStorage.getItem('cc.autoRefreshOnTask') !== 'false'   // default true
  )

  const napariUpdateImage = ref(
    localStorage.getItem('cc.napariUpdateImage') === 'true'    // default false
  )

  // Clean capture (E1): hide napari's baked scale bar + timestamp when taking a screenshot, for a clean
  // publication still (add a vector scale bar / timestamp externally). Applies to strip + animation
  // captures. Default false (keep the on-screen annotations). See docs/todo/ANIMATION_PLAN.md → E.
  const cleanCapture = ref(
    localStorage.getItem('cc.cleanCapture') === 'true'         // default false
  )

  // Reload behaviour: reloading a shown image (the eye / a finished task) refreshes DATA only
  // (labels + population/track overlays, re-read from disk) — NOT the image pyramid. Tick "reset" to
  // reopen the image too (needed when a task changed the pixels: drift/denoise). Default false.
  const napariResetOnReload = ref(
    localStorage.getItem('cc.napariResetOnReload') === 'true'  // default false
  )

  const napariAutoSaveLayerProps = ref(
    localStorage.getItem('cc.napariAutoSaveLayerProps') === 'true'  // default false
  )

  const napariAsDask = ref(
    localStorage.getItem('cc.napariAsDask') !== 'false'  // default true
  )

  // Render napari on the discrete GPU (hybrid-graphics machines, Linux only). Persisted here and
  // re-asserted to the backend each session (App.vue) so the bridge launches on the right GPU; the
  // backend holds the authoritative launch-time flag. Effective on the next bridge (re)start.
  const napariDiscreteGpu = ref(
    localStorage.getItem('cc.napariDiscreteGpu') === 'true'  // default false
  )

  // ── Layout: collapse the main nav sidebar (left) and the module function/tasks panel (right)
  // to free up working space. Both default expanded, both persist across sessions.
  const sidebarCollapsed = ref(localStorage.getItem('cc.sidebarCollapsed') === 'true')
  const rightPanelCollapsed = ref(localStorage.getItem('cc.rightPanelCollapsed') === 'true')
  // the napari Viewer controls are a floating dockable panel (not a sidebar section) — this is its
  // open/closed state, toggled from the sidebar "Viewer" button. Off by default (opt-in, no intrusion).
  const viewerPanelOpen = ref(localStorage.getItem('cc.viewerPanelOpen') === 'true')
  // the lab log is a floating dockable panel too (open/closed state, toggled from the sidebar).
  // Off by default (opt-in). See components/LabLogPanel.vue, docs/ai-assist/LAB-LOG.md.
  const labLogPanelOpen = ref(localStorage.getItem('cc.labLogPanelOpen') === 'true')

  // per-image label-layer visibility: { [imageUid]: { [valueName]: boolean } }
  // unknown labels default to true; persisted across sessions
  const _labelVisStore = ref<Record<string, Record<string, boolean>>>(
    JSON.parse(localStorage.getItem('cc.napariLabelVisibility') ?? '{}')
  )
  function getLabelVisibility(imageUid: string, labelNames: string[]): Record<string, boolean> {
    const stored = _labelVisStore.value[imageUid] ?? {}
    const out: Record<string, boolean> = {}
    for (const vn of labelNames) out[vn] = stored[vn] ?? true   // default visible
    return out
  }
  function setLabelVisibility(imageUid: string, vis: Record<string, boolean>) {
    _labelVisStore.value = { ..._labelVisStore.value, [imageUid]: { ...vis } }
    localStorage.setItem('cc.napariLabelVisibility', JSON.stringify(_labelVisStore.value))
  }

  // per-image track-overlay visibility: { [imageUid]: { [valueName]: boolean } } — which
  // segmentations have their tracks shown in napari. Default OFF (tracks are a heavier overlay).
  const _trackVisStore = ref<Record<string, Record<string, boolean>>>(
    JSON.parse(localStorage.getItem('cc.napariTrackVisibility') ?? '{}')
  )
  function getTrackVisibility(imageUid: string, valueNames: string[]): Record<string, boolean> {
    const stored = _trackVisStore.value[imageUid] ?? {}
    const out: Record<string, boolean> = {}
    for (const vn of valueNames) out[vn] = stored[vn] ?? false   // default hidden
    return out
  }
  function setTrackVisibility(imageUid: string, vis: Record<string, boolean>) {
    _trackVisStore.value = { ..._trackVisStore.value, [imageUid]: { ...vis } }
    localStorage.setItem('cc.napariTrackVisibility', JSON.stringify(_trackVisStore.value))
  }

  // ── Per-SET napari viewer preferences, keyed by set uid: { [setUid]: {...} } ──────────────────
  // These are the viewer-level DISPLAY toggles (colour-by, show-3D, point size, per-popType overlay
  // visibility, show-gated-tracks). They were always MEANT to be per-set (one experiment = consistent
  // viewing); the old R app made them global only because Shiny bookmarks made that easy. Per-set (not
  // global) so a choice made in one experiment never bleeds onto another's images (e.g. a colour-by
  // column that a different set's segmentation doesn't have), and not per-image so you set it ONCE and
  // it holds as you click through the set's images. Per-image state (which segmentations/tracks are
  // shown — the per-segmentation rows) stays keyed by image uid above.
  interface NapariSetPrefs {
    colourBy?: string                       // obs column to colour labels/tracks by ('' = default)
    show3D?: boolean                        // open images volumetric (only applied where a z-axis exists)
    showGatedTracks?: boolean               // overlay gated track populations
    pointSize?: number                      // population centroid point size in napari
    popVis?: Record<string, boolean>        // per-popType point-overlay visibility (flow/clust/track/trackclust)
    // user recolouring of a categorical colour-by, keyed by column then category value → hex. For
    // categories with no population (HMM states, raw clusters) there's no colour defined anywhere, so
    // the user can override the default palette; these win over pop/default when colouring. Per-column
    // so different colour-by columns keep independent schemes.
    colourByOverrides?: Record<string, Record<string, string>>
    // timelapse-recording params (extensible — F1.2 adds channels/pops/T-range here). fps = frame rate,
    // scale = supersample factor (2 = 2× resolution). Per-set like the other viewer prefs.
    movie?: { fps?: number; scale?: number }
    // 3D-crop z-range and t-range as 0–100 % (per set — the XY crop box itself is per-session, drawn in
    // napari each time since a region is image-specific). Only the ranges persist, like other prefs.
    cropZ?: { lo?: number; hi?: number }
    cropT?: { lo?: number; hi?: number }
    // batch-movie authoring config (F1.3 "make a movie for all images"): one config applied across the
    // selected images. `channels` = {channelName → colormap} for channels to SHOW (rest hidden). Per-set.
    batchMovie?: {
      valueName?: string                    // image version to open ('' = active)
      channels?: Record<string, string>     // channelName → colormap (only these shown)
      colourBy?: string                     // colour-by measure/obs column
      showTracks?: boolean; trackValueNames?: string[]; tailWidth?: number
      showGatedTracks?: boolean; showTrackclust?: boolean
      showPopulations?: boolean; popType?: string; pointsSize?: number
      colourLabels?: boolean
      fileAttrs?: string[]                  // attr names composing the output filename
      tStart?: number; tEnd?: number | null
    }
  }
  const _setPrefs = ref<Record<string, NapariSetPrefs>>(
    JSON.parse(localStorage.getItem('cc.napariSetPrefs') ?? '{}')
  )
  function _patchSet(setUid: string, patch: Partial<NapariSetPrefs>) {
    _setPrefs.value = { ..._setPrefs.value, [setUid]: { ...(_setPrefs.value[setUid] ?? {}), ...patch } }
    localStorage.setItem('cc.napariSetPrefs', JSON.stringify(_setPrefs.value))
  }
  const getColourBy = (setUid: string): string => _setPrefs.value[setUid]?.colourBy ?? ''
  const setColourBy = (setUid: string, column: string) => _patchSet(setUid, { colourBy: column })
  const getShow3D = (setUid: string): boolean => _setPrefs.value[setUid]?.show3D ?? false
  const setShow3D = (setUid: string, v: boolean) => _patchSet(setUid, { show3D: v })
  const getShowGatedTracks = (setUid: string): boolean => _setPrefs.value[setUid]?.showGatedTracks ?? false
  const setShowGatedTracks = (setUid: string, v: boolean) => _patchSet(setUid, { showGatedTracks: v })
  const getPointSize = (setUid: string): number => _setPrefs.value[setUid]?.pointSize ?? 6   // old GUI default 6
  const setPointSize = (setUid: string, v: number) => _patchSet(setUid, { pointSize: v })
  const getPopVisible = (setUid: string, popType: string): boolean =>
    _setPrefs.value[setUid]?.popVis?.[popType] ?? false                                       // default hidden
  function setPopVisible(setUid: string, popType: string, v: boolean) {
    _patchSet(setUid, { popVis: { ...(_setPrefs.value[setUid]?.popVis ?? {}), [popType]: v } })
  }
  // user colour-by recolouring, per column: {value → hex}
  const getColourOverrides = (setUid: string, column: string): Record<string, string> =>
    _setPrefs.value[setUid]?.colourByOverrides?.[column] ?? {}
  function setColourOverride(setUid: string, column: string, value: string, hex: string) {
    const all = _setPrefs.value[setUid]?.colourByOverrides ?? {}
    _patchSet(setUid, { colourByOverrides: { ...all, [column]: { ...(all[column] ?? {}), [value]: hex } } })
  }
  function clearColourOverrides(setUid: string, column: string) {
    const all = { ...(_setPrefs.value[setUid]?.colourByOverrides ?? {}) }
    delete all[column]
    _patchSet(setUid, { colourByOverrides: all })
  }
  // timelapse-recording params (per set); defaults match the backend (fps 15, scale 1×)
  const getMovieConfig = (setUid: string): { fps: number; scale: number } => ({
    fps: _setPrefs.value[setUid]?.movie?.fps ?? 15,
    scale: _setPrefs.value[setUid]?.movie?.scale ?? 1,
  })
  function setMovieConfig(setUid: string, patch: { fps?: number; scale?: number }) {
    _patchSet(setUid, { movie: { ...(_setPrefs.value[setUid]?.movie ?? {}), ...patch } })
  }
  // 3D-crop z-range (per set) as 0–100 %; default full depth (0–100)
  const getCropZ = (setUid: string): { lo: number; hi: number } => ({
    lo: _setPrefs.value[setUid]?.cropZ?.lo ?? 0,
    hi: _setPrefs.value[setUid]?.cropZ?.hi ?? 100,
  })
  function setCropZ(setUid: string, patch: { lo?: number; hi?: number }) {
    _patchSet(setUid, { cropZ: { ...(_setPrefs.value[setUid]?.cropZ ?? {}), ...patch } })
  }
  const getCropT = (setUid: string): { lo: number; hi: number } => ({
    lo: _setPrefs.value[setUid]?.cropT?.lo ?? 0,
    hi: _setPrefs.value[setUid]?.cropT?.hi ?? 100,
  })
  function setCropT(setUid: string, patch: { lo?: number; hi?: number }) {
    _patchSet(setUid, { cropT: { ...(_setPrefs.value[setUid]?.cropT ?? {}), ...patch } })
  }
  // batch-movie authoring config (per set); the reactive bag the BatchMovies page drives via useViewState
  type BatchMovieCfg = NonNullable<NapariSetPrefs['batchMovie']>
  const getBatchMovieConfig = (setUid: string): BatchMovieCfg => _setPrefs.value[setUid]?.batchMovie ?? {}
  function setBatchMovieConfig(setUid: string, patch: Partial<BatchMovieCfg>) {
    _patchSet(setUid, { batchMovie: { ...(_setPrefs.value[setUid]?.batchMovie ?? {}), ...patch } })
  }

  watch(taskListAutoFollow,       v => localStorage.setItem('cc.taskListAutoFollow',       String(v)))
  watch(autoRefreshOnTask,        v => localStorage.setItem('cc.autoRefreshOnTask',        String(v)))
  watch(napariUpdateImage,        v => localStorage.setItem('cc.napariUpdateImage',        String(v)))
  watch(cleanCapture,             v => localStorage.setItem('cc.cleanCapture',             String(v)))
  watch(napariResetOnReload,      v => localStorage.setItem('cc.napariResetOnReload',      String(v)))
  watch(napariAutoSaveLayerProps, v => localStorage.setItem('cc.napariAutoSaveLayerProps', String(v)))
  watch(napariAsDask,             v => localStorage.setItem('cc.napariAsDask',             String(v)))
  watch(napariDiscreteGpu,        v => localStorage.setItem('cc.napariDiscreteGpu',        String(v)))
  watch(sidebarCollapsed,         v => localStorage.setItem('cc.sidebarCollapsed',         String(v)))
  watch(rightPanelCollapsed,      v => localStorage.setItem('cc.rightPanelCollapsed',      String(v)))
  watch(viewerPanelOpen,          v => localStorage.setItem('cc.viewerPanelOpen',          String(v)))
  watch(labLogPanelOpen,          v => localStorage.setItem('cc.labLogPanelOpen',          String(v)))

  return { taskListAutoFollow, autoRefreshOnTask, napariUpdateImage, cleanCapture, napariResetOnReload, napariAutoSaveLayerProps, napariAsDask, napariDiscreteGpu, sidebarCollapsed, rightPanelCollapsed, viewerPanelOpen, labLogPanelOpen, getLabelVisibility, setLabelVisibility, getTrackVisibility, setTrackVisibility, getColourBy, setColourBy, getShow3D, setShow3D, getShowGatedTracks, setShowGatedTracks, getPointSize, setPointSize, getPopVisible, setPopVisible, getColourOverrides, setColourOverride, clearColourOverrides, getMovieConfig, setMovieConfig, getCropZ, setCropZ, getCropT, setCropT, getBatchMovieConfig, setBatchMovieConfig }
})
