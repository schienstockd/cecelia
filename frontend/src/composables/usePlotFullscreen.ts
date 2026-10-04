import { computed, ref, watch, onActivated, onDeactivated, onBeforeUnmount,
         type Ref, type WritableComputedRef } from 'vue'
import { useSettingsStore } from '../stores/settings'

// Whether the current module page's image panel (ModuleLayout's action bar + filters + Images +
// #plots) is expanded to fill the browser window (covering AppHeader, AppSidebar and the module
// page's own SetBar / right panel; floating panels lift above it — PANEL_Z_LIFTED in
// utils/panelStack.ts). ONE app-wide flag — module pages switch,
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
    // `canvasMaximised`, not the flag: on a page that isn't showing the maximised layout (Settings, no
    // active set) Esc must not silently forget the layout the next module page would restore.
    if (e.key !== 'Escape' || !canvasMaximised.value) return
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

// The FLAG above is not the same as "a maximised canvas is on screen": it persists across page
// switches (and reloads), but only a ModuleLayout page with plots and an active set actually takes
// the maximised layout. Things that must react to the real layout (FloatingPanel lifting above it)
// read `canvasMaximised`; ModuleLayout is the one reporter. Keyed per reporter because KeepAlive
// keeps several module pages mounted — only the ACTIVE one counts, so a hidden kept-alive page
// cannot hold the lift on a page that isn't maximised.
const shownBy = ref(new Set<symbol>())
export const canvasMaximised = computed(() => shownBy.value.size > 0)

export function reportCanvasMaximised(on: Ref<boolean>): void {
  const id = Symbol('canvas')
  let active = true
  const sync = () => {
    const next = new Set(shownBy.value)
    if (active && on.value) next.add(id); else next.delete(id)
    shownBy.value = next
  }
  watch(on, sync, { immediate: true })
  onActivated(() => { active = true; sync() })
  onDeactivated(() => { active = false; sync() })
  onBeforeUnmount(() => { active = false; sync() })
}
