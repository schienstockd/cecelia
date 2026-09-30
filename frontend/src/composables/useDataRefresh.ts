import { watch, onActivated } from 'vue'
import { useProjectStore } from '../stores/project'
import { useSettingsStore } from '../stores/settings'

// The one primitive of the task-refresh framework: "refetch when a task finishes on one of THESE
// images." A plot passes the images it shows + its reload fn; this watches the per-image data version
// (project.dataVersion, bumped in ws.ts on `task:status == 'done'` for the touched image) and calls
// `onRefresh` only when one of those images changed — targeted, not project-wide. This is what replaced
// the per-plot reload buttons; every plot uses it the same way instead of importing the store and
// weaving `dataVersionFor` into its own watch. See docs/todo/TASK_DATA_REFRESH_PLAN.md.
//
// Gated by the global `autoRefreshOnTask` setting (on by default): when off, a finished task doesn't
// pull plots out from under the user — they refresh on the next navigation / input change instead. This
// is the ONE chokepoint, so the setting governs every plot at once.
//
// "The next navigation" used to be a remount. Plot pages are now kept alive (App.vue), so returning to
// one is an activation — catch up there on any bump the setting held back. (With the setting ON, a
// bump that landed while the page was hidden needs nothing extra: the paused watcher runs once on
// return — composables/useKeepAlive.ts.)
export function useDataRefresh(imageUids: () => string[], onRefresh: () => void) {
  const project = useProjectStore()
  const settings = useSettingsStore()
  const version = () => project.dataVersionFor(imageUids())
  let seen = version()
  watch(version, (v) => { if (settings.autoRefreshOnTask) { seen = v; onRefresh() } })
  onActivated(() => {
    if (settings.autoRefreshOnTask) return
    const v = version()
    if (v !== seen) { seen = v; onRefresh() }
  })
}
