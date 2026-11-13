// Keeps an open viewer current as tasks finish (Preferences → Auto-update), and drops cached pixels a
// task rewrote. Mount ONCE, app-level (App.vue, main window only), like `useOverlayAutoShow`: it used
// to live in the floating Viewer panel, so with the panel closed — the default — nothing listened.
// The decisions are `utils/viewerAutoUpdate.ts` (pure, tested); this applies them.
import { onMounted, onUnmounted } from 'vue'
import { useWsStore } from '../stores/ws'
import { isTerminalStatus } from '../lib/taskStatus'
import { useProjectStore } from '../stores/project'
import { useSettingsStore } from '../stores/settings'
import { publishViewerCacheClear } from '../lib/viewerCacheClearChannel'
import { onResultActions, onStatusActions, type ViewerAutoUpdateAction } from '../utils/viewerAutoUpdate'
import { onlyMask } from '../utils/viewer/segRows'

/** Ask the open viewer (pop-out included) to refetch its overlays — the `cc.viewerOverlaysTick` ping. */
export function pingViewerOverlays(imageUid: string | null | undefined) {
  if (!imageUid || typeof localStorage === 'undefined') return
  localStorage.setItem('cc.viewerOverlaysTick', `${imageUid}:${Date.now()}`)
}

export function useViewerAutoUpdate() {
  const ws = useWsStore()
  const project = useProjectStore()
  const settings = useSettingsStore()

  const labelNamesOf = (uid: string) => Object.keys(project.imageByUid(uid)?.labels ?? {})
  function apply(imageUid: string, actions: ViewerAutoUpdateAction[]) {
    for (const a of actions) {
      if (a.kind === 'ping') pingViewerOverlays(imageUid)
      else if (a.kind === 'cacheClear') publishViewerCacheClear({ imageUid, valueName: a.valueName, labelValueName: a.labelValueName })
      else if (a.kind === 'version') settings.setImageVersion(imageUid, a.valueName)
      else settings.setLabelVisibility(imageUid, onlyMask(labelNamesOf(imageUid), a.valueName))
    }
  }
  // What each task's result already did to the viewer, by task id — its status then skips those.
  const resultDid = new Map<string, Set<ViewerAutoUpdateAction['kind']>>()
  const onStatus = (data: Record<string, unknown>) => {
    const taskId = String(data.taskId ?? '')
    const covered = resultDid.get(taskId)
    if (isTerminalStatus(String(data.status ?? ''))) resultDid.delete(taskId)
    apply(String(data.imageUid ?? ''), onStatusActions(data, project.openImageUid, settings.viewerAutoUpdate, covered))
  }
  const onResult = (data: Record<string, unknown>) => {
    const actions = onResultActions(data, project.openImageUid, settings.viewerAutoUpdate)
    const taskId = String(data.taskId ?? '')
    if (taskId && actions.length) resultDid.set(taskId, new Set(actions.map(a => a.kind)))
    apply(String(data.imageUid ?? ''), actions)
  }

  onMounted(() => { ws.on('task:status', onStatus); ws.on('task:result', onResult) })
  onUnmounted(() => { ws.off('task:status', onStatus); ws.off('task:result', onResult) })
}
