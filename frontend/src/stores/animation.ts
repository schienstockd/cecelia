// Animation page state: captured napari "view snapshots" = the keyframes of the timeline editor. Each
// is a screenshot (sidecar PNG via assetId, shared with the board strip) + its view state + the image
// it came from + a duration. A timeline is per-image (keyframes interpolate views of ONE image); the
// page filters to the open image, in list order. Debounced autosave → settings/animations.json (mirrors
// the boards autosave). See AnimationModule.vue and docs/todo/ANIMATION_PLAN.md (F2).
import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref, watch } from 'vue'
import { useProjectMetaStore } from './projectMeta'
import { TITLE_CARD_DEFAULT, type TitleCardCfg } from '../utils/batchMovie'
import { debouncedSave } from '../utils/debouncedSave'

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
  // Which keyframe is highlighted. Here rather than in a component because the page is two of them —
  // the timeline matrix does the selecting and the controls panel acts on it ("Update selected").
  // Deliberately NOT persisted and not watched by the autosave below: it is a pointer at the current
  // edit, not part of the animation.
  const selectedId = ref<string | null>(null)
  const fps = ref(15)                    // output frame rate (per project)
  // output size in pixels (per project); null = the napari canvas size, which is the default. A `scale`
  // supersample lived here and was removed — see MovieOutputControls.vue.
  const sizeX = ref<number | null>(null)
  const sizeY = ref<number | null>(null)
  const suffix = ref('')                 // filename addition, so two renders of one image can coexist
  const titleCard = ref<TitleCardCfg>({ ...TITLE_CARD_DEFAULT })   // Phase H4 description slide (per project)
  // Write-behind autosave → /api/projects/animations (dirty on any keyframe/fps change, incl. deep
  // edits to a keyframe's viewState from the row toggles). Shared helper — utils/debouncedSave.
  const _autosave = debouncedSave(async () => {
    const uid = useProjectMetaStore().current?.uid
    if (!uid) return
    await fetch('/api/projects/animations', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid: uid, animations: { snapshots: snapshots.value, fps: fps.value, sizeX: sizeX.value,
                                                 sizeY: sizeY.value, suffix: suffix.value,
                                                 titleCard: titleCard.value } }),
    }).catch(() => { /* autosave is best-effort */ })
  }, { wait: 600 })
  const _save = () => _autosave.schedule()
  watch(snapshots, _save, { deep: true })
  watch([fps, sizeX, sizeY, suffix], _save)
  watch(titleCard, _save, { deep: true })

  // hydrate from the project-load response (or clear on a project with none / on switch).
  // `duringRestore` holds the autosave past the debounce window: Vue's watchers run AFTER this function
  // returns, so clearing a flag on the last line (what this used to do) suppressed nothing and every
  // project open posted the animations it had just read straight back.
  function load(data: { snapshots?: AnimSnapshot[]; fps?: number; sizeX?: number | null;
                        sizeY?: number | null; suffix?: string;
                        titleCard?: TitleCardCfg } | null | undefined) {
    _autosave.duringRestore(() => {
      snapshots.value = data?.snapshots ?? []
      fps.value = data?.fps ?? 15
      sizeX.value = data?.sizeX ?? null
      sizeY.value = data?.sizeY ?? null
      suffix.value = data?.suffix ?? ''
      titleCard.value = data?.titleCard ?? { ...TITLE_CARD_DEFAULT }
    })
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

  // drag-and-drop: place the dragged keyframe at the target's position (both must be the same image —
  // the timeline is per-image).
  function reorder(draggedId: string, targetId: string) {
    if (draggedId === targetId) return
    const arr = [...snapshots.value]
    const from = arr.findIndex(s => s.id === draggedId)
    const to = arr.findIndex(s => s.id === targetId)
    if (from < 0 || to < 0 || arr[from].imageUid !== arr[to].imageUid) return
    const [item] = arr.splice(from, 1)
    arr.splice(arr.findIndex(s => s.id === targetId), 0, item)
    snapshots.value = arr
  }

  return { snapshots, selectedId, fps, sizeX, sizeY, suffix, titleCard, load, add, remove, move, reorder }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useAnimationStore, import.meta.hot))
