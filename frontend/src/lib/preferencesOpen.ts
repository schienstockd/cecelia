// Shared open/close state for the Preferences modal (USER_PROFILE_PLAN Phase 4).
//
// Same pattern as `lib/whatsNew.ts` / `lib/guideOpen.ts`: the modal is mounted ONCE in App.vue
// and driven by the shared ref below. Any caller — header button, sidebar footer, KiwiCockpit's
// read-only profile row (Phase 6) — calls `openPreferences()`. One modal, one state.
import { ref } from 'vue'

export const isPreferencesOpen = ref(false)
export type PreferencesPane = 'profiles' | 'interface' | 'viewer' | 'movies' | 'layout' | 'kiwi' | 'lablog' | 'hpc'

/** The pane the modal opens on; a caller may name one (Settings → HPC links to `hpc`). */
export const preferencesPane = ref<PreferencesPane>('profiles')

export function openPreferences(pane: PreferencesPane = 'profiles') {
  preferencesPane.value = pane
  isPreferencesOpen.value = true
}

export function closePreferences() {
  isPreferencesOpen.value = false
}
