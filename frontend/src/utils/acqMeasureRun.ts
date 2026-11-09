// "Measure N movies" in the acquisition check: one `importImages.measureSignal` task per movie, on the
// io pool, so the probe (about a minute per movie) runs in the background with progress in the task
// manager instead of inside one HTTP request. Same dispatch shape as CopyDialog's batch (addMany + one
// `task:run` per image). The task: app/src/tasks/importImages/measureSignal.jl.
import { useTaskStore } from '../stores/tasks'
import { useWsStore } from '../stores/ws'
import { useProjectStore } from '../stores/project'

export const MEASURE_FUN = 'importImages.measureSignal'

export function submitMeasure(projectUid: string, setUid: string, imageUids: string[]): number {
  if (!imageUids.length) return 0
  const tasks = useTaskStore(); const ws = useWsStore(); const project = useProjectStore()
  const entries = tasks.addMany(imageUids.map(uid => ({
    module: 'manageImages', label: 'Measure signal', imageUid: uid,
    imageName: project.imageByUid(uid)?.name || uid,
    status: 'queued' as const, taskName: 'measureSignal', funName: MEASURE_FUN, params: {}, projectUid,
  })), `Measure ${imageUids.length} movie${imageUids.length === 1 ? '' : 's'}`)
  entries.forEach((t, i) => ws.send({ type: 'task:run', taskId: t.id, funName: MEASURE_FUN, params: {},
                                      imageUid: imageUids[i], projectUid, setUid, poolName: 'io' }))
  return entries.length
}
