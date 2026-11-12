// Shared open/close state for the Settings modal — same pattern as `lib/preferencesOpen.ts`: the
// modal is mounted ONCE in App.vue and driven by these refs. The sidebar footer opens it; so does
// the old `/settings` URL (main.ts), so a bookmark or a link still lands somewhere.
import { ref } from 'vue'

export const isSettingsOpen = ref(false)
export type SettingsPane = 'project' | 'interface' | 'software-updates' | 'storage' | 'storage-defaults'
  | 'custom-modules' | 'plugins' | 'system' | 'server' | 'shared-workstation' | 'other-mcp-connections'
  | 'hpc' | 'troubleshooting'

/** The pane shown; kept across closes so reopening returns to where the user was. */
export const settingsPane = ref<SettingsPane>('project')

export function openSettings(pane?: SettingsPane) {
  if (pane) settingsPane.value = pane
  isSettingsOpen.value = true
}

export function closeSettings() {
  isSettingsOpen.value = false
}
