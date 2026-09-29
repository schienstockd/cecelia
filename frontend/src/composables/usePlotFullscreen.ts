import { computed, type WritableComputedRef } from 'vue'
import { useSettingsStore } from '../stores/settings'

// Whether the current module page's plot canvas (ModuleLayout's #plots CollapsibleSection) is
// expanded to fill the browser window (covering AppHeader, AppSidebar, floating panels and the
// module page's own SetBar / image table / right panel). ONE app-wide flag — module pages switch,
// the maximised layout persists — kept in the per-profile settings store (`plotsMaximised`).
// The Esc handler is a module-level singleton: install once on first use, no matter how many
// module pages mount.
let escInstalled = false
function ensureEsc(settings: ReturnType<typeof useSettingsStore>) {
  if (escInstalled || typeof window === 'undefined') return
  window.addEventListener('keydown', (e) => {
    if (e.key === 'Escape' && settings.plotsMaximised) settings.plotsMaximised = false
  })
  escInstalled = true
}

export function usePlotFullscreen(): {
  maximised: WritableComputedRef<boolean>
  toggle: () => void
} {
  const settings = useSettingsStore()
  ensureEsc(settings)
  const maximised = computed<boolean>({
    get: () => settings.plotsMaximised,
    set: (v) => { settings.plotsMaximised = v },
  })
  return {
    maximised,
    toggle: () => { maximised.value = !maximised.value },
  }
}
