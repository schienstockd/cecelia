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

  watch(taskListAutoFollow,       v => localStorage.setItem('cc.taskListAutoFollow',       String(v)))
  watch(autoRefreshOnTask,        v => localStorage.setItem('cc.autoRefreshOnTask',        String(v)))
  watch(napariUpdateImage,        v => localStorage.setItem('cc.napariUpdateImage',        String(v)))
  watch(napariResetOnReload,      v => localStorage.setItem('cc.napariResetOnReload',      String(v)))
  watch(napariAutoSaveLayerProps, v => localStorage.setItem('cc.napariAutoSaveLayerProps', String(v)))
  watch(napariAsDask,             v => localStorage.setItem('cc.napariAsDask',             String(v)))
  watch(napariDiscreteGpu,        v => localStorage.setItem('cc.napariDiscreteGpu',        String(v)))
  watch(sidebarCollapsed,         v => localStorage.setItem('cc.sidebarCollapsed',         String(v)))
  watch(rightPanelCollapsed,      v => localStorage.setItem('cc.rightPanelCollapsed',      String(v)))

  return { taskListAutoFollow, autoRefreshOnTask, napariUpdateImage, napariResetOnReload, napariAutoSaveLayerProps, napariAsDask, napariDiscreteGpu, sidebarCollapsed, rightPanelCollapsed, getLabelVisibility, setLabelVisibility, getTrackVisibility, setTrackVisibility, getColourBy, setColourBy, getShow3D, setShow3D, getShowGatedTracks, setShowGatedTracks, getPointSize, setPointSize, getPopVisible, setPopVisible }
})
