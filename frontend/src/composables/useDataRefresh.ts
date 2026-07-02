import { watch } from 'vue'
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
export function useDataRefresh(imageUids: () => string[], onRefresh: () => void) {
  const project = useProjectStore()
  const settings = useSettingsStore()
  watch(() => project.dataVersionFor(imageUids()), () => { if (settings.autoRefreshOnTask) onRefresh() })
}
