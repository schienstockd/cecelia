// Animation page state: the captured napari "view snapshots" for the current project — each a screenshot
// (sidecar PNG via assetId, shared with the board strip) + its view state + the image it came from.
// Debounced autosave → settings/animations.json (mirrors the boards autosave). See AnimationModule.vue
// and docs/todo/ANIMATION_PLAN.md.
import { defineStore } from 'pinia'
import { ref, watch } from 'vue'
import { useProjectMetaStore } from './projectMeta'

export interface AnimSnapshot {
  id: string
  assetId?: string                      // sidecar PNG id (served via /api/board-assets)
  snapshot?: Record<string, unknown>    // napari view state (camera + dims + per-layer props)
  imageUid?: string | null              // the image the view was captured from
  imageName?: string
  title?: string
}

export const useAnimationStore = defineStore('animation', () => {
  const snapshots = ref<AnimSnapshot[]>([])
  const _restoring = ref(false)         // suppress autosave while hydrating from the project load

  // hydrate from the project-load response (or clear on a project with none / on switch)
  function load(data: { snapshots?: AnimSnapshot[] } | null | undefined) {
    _restoring.value = true
    snapshots.value = data?.snapshots ?? []
    _restoring.value = false
  }
  function add(s: AnimSnapshot) { snapshots.value = [...snapshots.value, s] }
  function remove(id: string) { snapshots.value = snapshots.value.filter(s => s.id !== id) }

  // debounced autosave → /api/projects/animations (dirty on any snapshot add/remove/edit)
  let timer: ReturnType<typeof setTimeout> | null = null
  watch(snapshots, () => {
    if (_restoring.value) return
    const uid = useProjectMetaStore().current?.uid
    if (!uid) return
    if (timer) clearTimeout(timer)
    timer = setTimeout(() => {
      fetch('/api/projects/animations', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid: uid, animations: { snapshots: snapshots.value } }),
      }).catch(() => { /* autosave is best-effort */ })
    }, 600)
  }, { deep: true })

  return { snapshots, load, add, remove }
})
