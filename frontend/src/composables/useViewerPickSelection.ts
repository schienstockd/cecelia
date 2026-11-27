// The viewer window's copy of `/Pick selection` for its image — so the cells picked ON the viewer are
// marked on the viewer too, not only on the gating plots (`utils/viewer/pickSelection.ts` says how).
// Follows the server's `gating:pop-tree` broadcast, which every pick / pick-rect / pick-set /
// pick-clear sends (api/src/viewer_api.jl), whichever window made it.
import { ref, watch, onMounted, onUnmounted, type Ref } from 'vue'
import { useWsStore } from '../stores/ws'
import {
  fetchPickLabels, pickBroadcastAction, treeHasPickSelection, type ViewerPickSelection,
} from '../utils/viewer/pickSelection'

export function useViewerPickSelection(
  projectUid: string, imageUid: string,
  /** the segmentation a pick on this viewer goes to — read once on mount and whenever it changes */
  pickVn: () => string,
): Ref<ViewerPickSelection | null> {
  const ws = useWsStore()
  const sel = ref<ViewerPickSelection | null>(null)
  let seq = 0

  async function load(vn: string): Promise<void> {
    if (!projectUid || !imageUid || !vn) return
    const mine = ++seq
    const labels = await fetchPickLabels(projectUid, imageUid, vn)
    if (mine === seq) sel.value = labels.length ? { valueName: vn, labels } : null
  }

  function onPopTree(data: Record<string, unknown>) {
    if (String(data.imageUid ?? '') !== imageUid) return
    const vn = String(data.valueName ?? '')
    const has = treeHasPickSelection(data.tree as Parameters<typeof treeHasPickSelection>[0])
    const act = pickBroadcastAction(sel.value, vn, has)
    if (act === 'fetch') void load(vn)
    else if (act === 'clear') { seq++; sel.value = null }
  }

  watch(pickVn, vn => { void load(vn) })
  onMounted(() => { ws.on('gating:pop-tree', onPopTree); void load(pickVn()) })
  onUnmounted(() => ws.off('gating:pop-tree', onPopTree))
  return sel
}
