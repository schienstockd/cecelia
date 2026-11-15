// The SHOWN fold of the main window's menu (☰) and functions panel — the saved choice
// (`settings.sidebarCollapsed` / `settings.rightPanelCollapsed`) adjusted for a narrow window.
// Read and write the fold through here, never the settings refs directly: the rule (fold by default
// below NARROW_LAYOUT_PX, a narrow click is a session override, the saved choice is untouched) is
// `utils/narrowLayout.ts`, and a reader of the raw ref would show a panel this store has folded.
import { defineStore, acceptHMRUpdate } from 'pinia'
import { computed, ref } from 'vue'
import { useSettingsStore } from './settings'
import { NARROW_LAYOUT_PX, effectiveFold, foldWrite } from '../utils/narrowLayout'

type FoldKey = 'sidebarCollapsed' | 'rightPanelCollapsed'

export const useLayoutFoldStore = defineStore('layoutFold', () => {
  const settings = useSettingsStore()
  const query = typeof window !== 'undefined' && window.matchMedia
    ? window.matchMedia(`(max-width: ${NARROW_LAYOUT_PX - 0.02}px)`) : null
  const narrow = ref(query?.matches ?? false)
  const override = ref<Record<FoldKey, boolean | null>>({ sidebarCollapsed: null, rightPanelCollapsed: null })
  query?.addEventListener('change', e => {
    narrow.value = e.matches
    // a narrow spell's clicks end with it: the next narrow spell folds again
    if (!e.matches) override.value = { sidebarCollapsed: null, rightPanelCollapsed: null }
  })

  function fold(key: FoldKey) {
    return computed<boolean>({
      get: () => effectiveFold(settings[key], narrow.value, override.value[key]),
      set: v => {
        const w = foldWrite(narrow.value, v)
        if ('saved' in w) settings[key] = w.saved
        else override.value = { ...override.value, [key]: w.override }
      },
    })
  }
  const sidebarFolded = fold('sidebarCollapsed')
  const rightPanelFolded = fold('rightPanelCollapsed')
  return { narrow, sidebarFolded, rightPanelFolded }
})

if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useLayoutFoldStore, import.meta.hot))
