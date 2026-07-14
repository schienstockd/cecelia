// Animation page state: captured napari "view snapshots" = the keyframes of the timeline editor. Each
// is a screenshot (sidecar PNG via assetId, shared with the board strip) + its view state + the image
// it came from + a duration. A timeline is per-image (keyframes interpolate views of ONE image); the
// page filters to the open image, in list order. Debounced autosave → settings/animations.json (mirrors
// the boards autosave). See AnimationModule.vue and docs/todo/ANIMATION_PLAN.md (F2).
import { defineStore } from 'pinia'
import { ref, watch } from 'vue'
import { useProjectMetaStore } from './projectMeta'

export interface AnimSnapshot {
  id: string
  assetId?: string                      // sidecar PNG id (served via /api/board-assets)
  snapshot?: Record<string, unknown>    // napari view state (camera + dims + per-layer props) — the keyframe (edited)
  original?: Record<string, unknown>    // the captured baseline viewState — reset target; unchanged by row edits
  imageUid?: string | null              // the image this keyframe belongs to
  imageName?: string
  title?: string
  duration?: number                     // seconds this keyframe tweens FROM the previous (default 1)
}

export const useAnimationStore = defineStore('animation', () => {
  const snapshots = ref<AnimSnapshot[]>([])
  const fps = ref(15)                    // output frame rate (per project)
  const _restoring = ref(false)         // suppress autosave while hydrating from the project load

  // hydrate from the project-load response (or clear on a project with none / on switch)
  function load(data: { snapshots?: AnimSnapshot[]; fps?: number } | null | undefined) {
    _restoring.value = true
    snapshots.value = data?.snapshots ?? []
    fps.value = data?.fps ?? 15
    _restoring.value = false
  }
  function add(s: AnimSnapshot) { snapshots.value = [...snapshots.value, s] }
  function remove(id: string) { snapshots.value = snapshots.value.filter(s => s.id !== id) }

  // move a keyframe one place earlier/later among its OWN image's keyframes (swap with the neighbour of
  // the same imageUid), so per-image timeline order is what changes.
  function move(id: string, dir: -1 | 1) {
    const arr = snapshots.value
    const i = arr.findIndex(s => s.id === id)
    if (i < 0) return
    const uid = arr[i].imageUid
    let j = i + dir
    while (j >= 0 && j < arr.length && arr[j].imageUid !== uid) j += dir
    if (j < 0 || j >= arr.length) return
    const next = [...arr]; [next[i], next[j]] = [next[j], next[i]]
    snapshots.value = next
  }

  // debounced autosave → /api/projects/animations (dirty on any keyframe/fps change, incl. deep edits
  // to a keyframe's viewState from the row toggles)
  let timer: ReturnType<typeof setTimeout> | null = null
  function _save() {
    if (_restoring.value) return
    const uid = useProjectMetaStore().current?.uid
    if (!uid) return
    if (timer) clearTimeout(timer)
    timer = setTimeout(() => {
      fetch('/api/projects/animations', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid: uid, animations: { snapshots: snapshots.value, fps: fps.value } }),
      }).catch(() => { /* autosave is best-effort */ })
    }, 600)
  }
  watch(snapshots, _save, { deep: true })
  watch(fps, _save)

  return { snapshots, fps, load, add, remove, move }
})
