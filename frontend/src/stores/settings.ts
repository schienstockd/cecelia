import { defineStore } from 'pinia'
import { ref, watch } from 'vue'

export const useSettingsStore = defineStore('settings', () => {
  const taskListAutoFollow = ref(
    localStorage.getItem('cc.taskListAutoFollow') !== 'false'  // default true
  )

  const napariUpdateImage = ref(
    localStorage.getItem('cc.napariUpdateImage') === 'true'    // default false
  )

  const napariAutoSaveLayerProps = ref(
    localStorage.getItem('cc.napariAutoSaveLayerProps') === 'true'  // default false
  )

  const napariShow3D = ref(
    localStorage.getItem('cc.napariShow3D') === 'true'   // default false
  )

  const napariAsDask = ref(
    localStorage.getItem('cc.napariAsDask') !== 'false'  // default true
  )

  // size of population centroid points drawn in napari (old GUI default: 6)
  const napariPointSize = ref(
    Number(localStorage.getItem('cc.napariPointSize') ?? 6)
  )

  // remember whether gating populations are shown as points in napari, so opening an image
  // restores the user's last choice instead of always starting hidden (default true)
  const napariShowPopulations = ref(
    localStorage.getItem('cc.napariShowPopulations') !== 'false'
  )

  // remember whether track populations are shown as napari Tracks layers (default false — tracks
  // are a heavier, more specialised overlay than population points). Mirrors napariShowPopulations.
  const napariShowTracks = ref(
    localStorage.getItem('cc.napariShowTracks') === 'true'
  )

  // remember the obs column the napari tracks/labels are coloured by ('' = default colouring)
  const napariColourBy = ref(localStorage.getItem('cc.napariColourBy') ?? '')

  // master "show gated track populations" toggle (TEST/SDGF track-measure gates), like Show
  // populations but for the Tracks overlay. Default off (heavier). Mirrors napariShowPopulations.
  const napariShowGatedTracks = ref(localStorage.getItem('cc.napariShowGatedTracks') === 'true')

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

  watch(taskListAutoFollow,       v => localStorage.setItem('cc.taskListAutoFollow',       String(v)))
  watch(napariUpdateImage,        v => localStorage.setItem('cc.napariUpdateImage',        String(v)))
  watch(napariAutoSaveLayerProps, v => localStorage.setItem('cc.napariAutoSaveLayerProps', String(v)))
  watch(napariShow3D,             v => localStorage.setItem('cc.napariShow3D',             String(v)))
  watch(napariAsDask,             v => localStorage.setItem('cc.napariAsDask',             String(v)))
  watch(napariPointSize,          v => localStorage.setItem('cc.napariPointSize',          String(v)))
  watch(napariShowPopulations,    v => localStorage.setItem('cc.napariShowPopulations',    String(v)))
  watch(napariShowTracks,         v => localStorage.setItem('cc.napariShowTracks',         String(v)))
  watch(napariColourBy,           v => localStorage.setItem('cc.napariColourBy',           String(v)))
  watch(napariShowGatedTracks,    v => localStorage.setItem('cc.napariShowGatedTracks',    String(v)))
  watch(sidebarCollapsed,         v => localStorage.setItem('cc.sidebarCollapsed',         String(v)))
  watch(rightPanelCollapsed,      v => localStorage.setItem('cc.rightPanelCollapsed',      String(v)))

  return { taskListAutoFollow, napariUpdateImage, napariAutoSaveLayerProps, napariShow3D, napariAsDask, napariPointSize, napariShowPopulations, napariShowTracks, napariShowGatedTracks, napariColourBy, sidebarCollapsed, rightPanelCollapsed, getLabelVisibility, setLabelVisibility, getTrackVisibility, setTrackVisibility }
})
