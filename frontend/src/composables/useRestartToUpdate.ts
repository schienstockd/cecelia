// "Restart to finish installing" — one gate for the three places that offer it (header chip, What's
// New footer, Settings → Software). A staged update only lands when the launcher restarts the server;
// the restart also stops the task runner so it comes back on the new code, so it is blocked while a
// task is in flight rather than silently cancelling it.
import { computed } from 'vue'
import { useAppControlStore } from '../stores/appControl'
import { useTaskStore } from '../stores/tasks'
import { runningTaskCount } from '../utils/runningTasks'
import { quitTaskPhrase } from '../utils/quitWarning'

export function useRestartToUpdate() {
  const app = useAppControlStore()
  const tasks = useTaskStore()
  const tasksRunning = computed(() => tasks.running().length > 0)
  const show = computed(() => app.updatePending && app.canApplyUpdate)
  const blocked = computed(() => tasksRunning.value || app.busy)
  const tip = computed(() => app.busy ? 'Installing the update…'
    : tasksRunning.value ? 'Finish or cancel running tasks first, then restart to update'
    : 'Restart Cecelia now to finish installing the update')
  // The store's list drives the live disabled state; the click re-asks the backend (same as Quit), so
  // a runner task this tab never adopted still blocks it.
  async function restart() {
    if (blocked.value) return
    const n = await runningTaskCount()
    if (n > 0) { app.updateMsg = `${quitTaskPhrase(n)} — finish or cancel them, then restart`; return }
    await app.restartToUpdate()
  }
  return { show, blocked, tip, restart }
}
