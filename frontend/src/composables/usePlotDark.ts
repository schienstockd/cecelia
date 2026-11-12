import { computed, type ComputedRef } from 'vue'
import { useSettingsStore } from '../stores/settings'

/** Whether on-screen plots draw dark — the app's colour scheme, nothing per plot. Exports are always
 *  light (`plotTheme(false)`), whatever this says. docs/UI.md → *Colour schemes*. */
export function usePlotDark(): ComputedRef<boolean> {
  const settings = useSettingsStore()
  return computed(() => settings.colourScheme === 'dark')
}
