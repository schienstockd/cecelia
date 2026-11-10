// Per-image stale outputs for the open project — `GET /api/analysis/lineage` (whole project, one call:
// measured ~40 ms on 17 images, ~120 ms on 11 processed ones), read by the image table's QC dot and
// the Run panel's advisory. Rendering: lib/staleOutputs.ts.
//
// Refetched when the project changes and when a task finishes (any image's data version bumps). One
// debounced sink, so a set run finishing image after image costs one request, not one per image.
import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, watch } from 'vue'
import { useProjectMetaStore } from './projectMeta'
import { useProjectStore } from './project'
import { debouncedLatest } from '../utils/debouncedLatest'
import { staleByImage, type Lineage, type StaleOutput } from '../lib/staleOutputs'

export const useStaleOutputsStore = defineStore('staleOutputs', () => {
  const byImage = ref<Record<string, StaleOutput[]>>({})
  const projectMeta = useProjectMetaStore()
  const projectStore = useProjectStore()

  const sink = debouncedLatest<string>(async (uid, isCurrent) => {
    if (!uid) { byImage.value = {}; return }
    const res = await fetch(`/api/analysis/lineage?projectUid=${encodeURIComponent(uid)}`)
    if (!res.ok || !isCurrent()) return
    const lin = await res.json() as Lineage
    if (isCurrent() && projectMeta.current?.uid === uid) byImage.value = staleByImage(lin)
  }, { wait: 1500, onError: () => { /* advisory: a failed lookup shows nothing, as before */ } })

  watch(() => projectMeta.current?.uid ?? '', uid => {
    byImage.value = {}          // never show one project's findings on another
    sink.schedule(uid)
    sink.flush()
  }, { immediate: true })
  watch(() => Object.values(projectStore.dataVersion).reduce((a, b) => a + b, 0),
        () => sink.schedule(projectMeta.current?.uid ?? ''))

  return { byImage }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useStaleOutputsStore, import.meta.hot))
