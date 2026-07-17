import { defineStore } from 'pinia'
import { ref } from 'vue'

// User drop-in custom modules (see docs/CUSTOM_MODULES.md). Mirrors the backend
// /api/tasks/custom-modules payload: the load report + the categories present among the user's
// custom specs. `categories` with builtin === false drive a generic page + a "Custom" nav group;
// tasks in a builtin category already surface on that category's existing page.
export interface CustomModuleEntry { path: string; status: 'ok' | 'error'; error: string | null }
export interface CustomCategory { name: string; builtin: boolean; funNames: string[] }

export const useCustomModulesStore = defineStore('customModules', () => {
  const dir        = ref('')
  const modules    = ref<CustomModuleEntry[]>([])
  const categories = ref<CustomCategory[]>([])
  const loading    = ref(false)
  let   loadedOnce = false

  function apply(data: { dir?: string; modules?: CustomModuleEntry[]; categories?: CustomCategory[] }) {
    if (data.dir !== undefined) dir.value = data.dir
    if (data.modules)    modules.value    = data.modules
    if (data.categories) categories.value = data.categories
  }

  async function refresh() {
    loading.value = true
    try {
      const res = await fetch('/api/tasks/custom-modules')
      if (res.ok) apply(await res.json())
    } catch { /* ignore — leave last-known state */ }
    finally { loading.value = false; loadedOnce = true }
  }

  async function ensureLoaded() {
    if (!loadedOnce && !loading.value) await refresh()
  }

  // Rescan the config dir for NEWLY dropped .jl (edits to loaded modules still need a restart).
  async function reload() {
    loading.value = true
    try {
      const res = await fetch('/api/tasks/custom-modules/reload', { method: 'POST' })
      if (res.ok) apply(await res.json())
    } catch { /* ignore */ }
    finally { loading.value = false; loadedOnce = true }
  }

  return { dir, modules, categories, loading, ensureLoaded, refresh, reload }
})
