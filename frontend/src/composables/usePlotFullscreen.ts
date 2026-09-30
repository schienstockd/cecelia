import { computed, type WritableComputedRef } from 'vue'
import { useSettingsStore } from '../stores/settings'

// Whether the current module page's image panel (ModuleLayout's action bar + filters + Images +
// #plots) is expanded to fill the browser window (covering AppHeader, AppSidebar, floating panels
// and the module page's own SetBar / right panel). ONE app-wide flag — module pages switch,
// the maximised layout persists — kept in the per-profile settings store (`plotsMaximised`).
// The Esc handler is a module-level singleton: install once on first use, no matter how many
// module pages mount.
//
// Esc is also how a popover, a dialog, a gate/lasso draw or a linked selection is dismissed — all
// window listeners too — and one keypress must do ONE of those things, not also drop the layout. So
// restoring is the LOWEST-priority use of Esc: a handler that consumed the key calls
// `preventDefault()`, and this one only acts on an Esc nobody claimed. It has to look AFTER the whole
// dispatch — it was installed first (first module-page mount), so it runs before every popover's
// listener; `setTimeout`, not `queueMicrotask`, because microtasks drain between listeners.
let escInstalled = false
function ensureEsc(settings: ReturnType<typeof useSettingsStore>) {
  if (escInstalled || typeof window === 'undefined') return
  window.addEventListener('keydown', (e) => {
    if (e.key !== 'Escape' || !settings.plotsMaximised) return
    setTimeout(() => { if (!e.defaultPrevented) settings.plotsMaximised = false })
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
