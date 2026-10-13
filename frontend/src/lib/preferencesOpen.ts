// Shared open/close state for the Preferences modal (USER_PROFILE_PLAN Phase 4).
//
// Same pattern as `lib/whatsNew.ts` / `lib/guideOpen.ts`: the modal is mounted ONCE in App.vue
// and driven by the shared ref below. Any caller — header button, sidebar footer, KiwiCockpit's
// read-only profile row (Phase 6) — calls `openPreferences()`. One modal, one state.
import { ref } from 'vue'

export const isPreferencesOpen = ref(false)

export function openPreferences() {
  isPreferencesOpen.value = true
}

export function closePreferences() {
  isPreferencesOpen.value = false
}
