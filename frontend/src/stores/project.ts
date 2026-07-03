import { defineStore } from 'pinia'
import { ref } from 'vue'
import { useCanvasPanelsStore } from './canvasPanels'
import { useAnalysisTabsStore } from './analysisTabs'
import { useAnalysisLayoutStore } from './analysisLayout'

export interface CciaImage {
  uid: string
  name: string
  kind: string
  status: 'pending' | 'converting' | 'done' | 'failed'
  sizeC?: number | null
  sizeT?: number | null
  sizeZ?: number | null
  physicalSizeX?: number | null
  physicalSizeY?: number | null
  physicalSizeZ?: number | null
  physicalSizeUnit?: string | null
  physicalSizeZCorrected?: boolean | null
  timeIncrement?: number | null
  timeIncrementUnit?: string | null
  channelNames?: string[]
  filepath?: string                       // active version filename (for display)
  activeValueName?: string                // active value name (the _active key from versioned dict)
  filepaths?: Record<string, string>      // valueName → filename (all versions, excludes _active)
  labels?: Record<string, string[]>        // valueName → [filename, …] (segmentation outputs)
  attr?: Record<string, string>           // user-defined metadata attributes
}

export interface CciaSet {
  uid: string
  name: string
  images: CciaImage[]
}

export const useProjectStore = defineStore('project', () => {
  const sets = ref<CciaSet[]>([])
  const activeSetUid = ref<string | null>(null)
  const napariImageUid = ref<string | null>(null)
  // Reload signal for the napari viewer: bumped by anything asking to refresh the SHOWN image (the
  // image-table eye clicked on the already-open image). ViewerPanel owns the overlay logic, so it
  // watches this tick and decides data-only vs full reopen (see settings.napariResetOnReload).
  const napariReloadTick = ref(0)
  const requestNapariReload = () => { napariReloadTick.value++ }
  // Data freshness — TARGETED per-image invalidation. A finished task (ws `task:status` == 'done')
  // bumps only the image(s) it touched; a plot watches `dataVersionFor(itsImages)` and refetches ONLY
  // when one of the images IT shows changed — not on every task in the project. This replaces the
  // per-plot reload buttons without the Shiny-style "invalidate the world". Set-scope tasks report one
  // representative member uid (see api/src/sockets.jl), so set plots — which watch all their member
  // images incl. the rep — still refresh; a non-rep member's single-image plot is the one gap (rare).
  // See docs/todo/TASK_DATA_REFRESH_PLAN.md.
  const dataVersion = ref<Record<string, number>>({})
  const bumpDataVersion = (imageUid: string) => {
    if (imageUid) dataVersion.value[imageUid] = (dataVersion.value[imageUid] ?? 0) + 1
  }
  // combined version for a set of images — reactive (reads the per-uid counters), so a plot can
  // `watch(() => dataVersionFor(itsImageUids), refetch)` and only fire when one of them bumps.
  const dataVersionFor = (uids: string[]): number =>
    uids.reduce((sum, u) => sum + (dataVersion.value[u] ?? 0), 0)

  // Remembered per-page image selection (the run-table checkboxes), keyed by `${scope}|${setUid}`
  // so it survives navigating away from a module page and back. `scope` is the module name (so a
  // module's selection is its own — e.g. gating's single-select doesn't bleed into segment).
  // In-memory (session-scoped); generic for every module page via ModuleLayout/ImageTable.
  const imageSelection = ref<Record<string, string[]>>({})
  const _selKey = (scope: string, setUid: string) => `${scope}|${setUid}`
  const getImageSelection = (scope: string, setUid: string): string[] =>
    imageSelection.value[_selKey(scope, setUid)] ?? []
  function setImageSelection(scope: string, setUid: string, uids: string[]) {
    imageSelection.value = { ...imageSelection.value, [_selKey(scope, setUid)]: uids }
  }

  const activeSet = () => sets.value.find(s => s.uid === activeSetUid.value) ?? null

  // Called when a project is opened — replaces the in-memory set/image list.
  function loadFromApi(apiSets: CciaSet[]) {
    sets.value = apiSets
    activeSetUid.value = sets.value[0]?.uid ?? null
    imageSelection.value = {}     // selections are per-project; don't carry across loads
    dataVersion.value = {}        // per-image versions are per-project too (uids don't cross projects)
    useCanvasPanelsStore().clear()   // open plots are per-project too
    useAnalysisTabsStore().clear()   // …and the Analysis-canvas boards
    useAnalysisLayoutStore().clear() // …and their grid layouts
  }

  function clear() {
    sets.value = []
    activeSetUid.value = null
    napariImageUid.value = null
    imageSelection.value = {}
    dataVersion.value = {}
    useCanvasPanelsStore().clear()
    useAnalysisTabsStore().clear()
    useAnalysisLayoutStore().clear()
  }

  function addSetFromApi(uid: string, name: string): CciaSet {
    const s: CciaSet = { uid, name, images: [] }
    sets.value.push(s)
    activeSetUid.value = s.uid
    return s
  }

  function deleteSet(uid: string) {
    sets.value = sets.value.filter(s => s.uid !== uid)
    if (activeSetUid.value === uid)
      activeSetUid.value = sets.value[0]?.uid ?? null
  }

  // Called after /api/images/register — images already have Cecelia UIDs on disk.
  function addImagesFromApi(setUid: string, images: CciaImage[]) {
    const set = sets.value.find(s => s.uid === setUid)
    if (!set) return
    set.images.push(...images)
  }

  function addImages(setUid: string, paths: string[]) {
    const set = sets.value.find(s => s.uid === setUid)
    if (!set) return
    for (const p of paths) {
      set.images.push({
        uid: crypto.randomUUID(),
        name: p.split('/').pop() ?? p,
        kind: 'static',
        status: 'pending',
        filepath: p,
      })
    }
  }

  function deleteImage(setUid: string, imageUid: string) {
    const set = sets.value.find(s => s.uid === setUid)
    if (!set) return
    set.images = set.images.filter(i => i.uid !== imageUid)
  }

  function updateImageStatus(imageUid: string, status: CciaImage['status']) {
    for (const set of sets.value) {
      const img = set.images.find(i => i.uid === imageUid)
      if (img) { img.status = status; return }
    }
  }

  // Merge partial image metadata — called on task:result to sync dimensions/channels.
  function updateImageMeta(imageUid: string, patch: Partial<CciaImage>) {
    for (const set of sets.value) {
      const img = set.images.find(i => i.uid === imageUid)
      if (img) { Object.assign(img, patch); return }
    }
  }

  // Add an attr key (empty string value) to all images in a set.
  function addAttrKey(setUid: string, attrName: string) {
    const set = sets.value.find(s => s.uid === setUid)
    if (!set) return
    for (const img of set.images) {
      if (!img.attr) img.attr = {}
      if (!(attrName in img.attr)) img.attr[attrName] = ''
    }
  }

  // Remove an attr key from all images in a set.
  function removeAttrKey(setUid: string, attrName: string) {
    const set = sets.value.find(s => s.uid === setUid)
    if (!set) return
    for (const img of set.images) {
      if (img.attr) delete img.attr[attrName]
    }
  }

  function removeLabelSet(imageUid: string, valueName: string) {
    for (const set of sets.value) {
      const img = set.images.find(i => i.uid === imageUid)
      if (img?.labels) { delete img.labels[valueName]; return }
    }
  }

  // Set per-image attr values for one key. values = {imageUid: value}
  function setAttrValues(attrName: string, values: Record<string, string>) {
    for (const set of sets.value) {
      for (const img of set.images) {
        if (img.uid in values) {
          if (!img.attr) img.attr = {}
          img.attr[attrName] = values[img.uid]
        }
      }
    }
  }

  return { sets, activeSetUid, napariImageUid, napariReloadTick, requestNapariReload, dataVersion, bumpDataVersion, dataVersionFor, activeSet, getImageSelection, setImageSelection, loadFromApi, clear, addSetFromApi, deleteSet, addImages, addImagesFromApi, deleteImage, updateImageStatus, updateImageMeta, addAttrKey, removeAttrKey, setAttrValues, removeLabelSet }
})
