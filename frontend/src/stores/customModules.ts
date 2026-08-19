import { defineStore } from 'pinia'
import { ref } from 'vue'

// User drop-in custom modules (see docs/CUSTOM_MODULES.md). Mirrors the backend
// /api/tasks/custom-modules payload: the load report + the categories present among the user's
// custom specs. `categories` with builtin === false drive a generic page + a "Custom" nav group;
// tasks in a builtin category already surface on that category's existing page.
export interface CustomModuleEntry { path: string; plugin: string | null; status: 'ok' | 'error'; error: string | null }
// `views` = interactive plots a plugin asked for on this category's page, by stable registry id
// (PLUGINS_PLAN Decision 11). Resolved against `interactiveViews.ts` by the canvas, which is also
// where an id that resolves to nothing is reported — Julia cannot see that registry.
export interface CustomCategory {
  name: string; builtin: boolean; funNames: string[]; cohortFuns?: string[]
  views?: { view: string; label: string; plugin: string }[]
}

// What one plugin contributes, in one grammar: what its directory layout implies, merged with what
// its manifest's OPTIONAL `contributions` block declares (PLUGINS_PLAN Decision 10). `declared` says
// the manifest also named it — a declaration that resolves to nothing lands in `problems` instead.
// `views` and `layers` are understood but not acted on yet (Decisions 11/12).
export interface PluginContributions {
  tasks:  { funName: string; category: string; path: string; declared: boolean }[]
  plots:  { id: string; spec: string; moduleName: string; declared: boolean }[]
  views:  { moduleName: string; view: string; label: string }[]
  layers: { fromTask: string; layerType: string; options: Record<string, unknown> }[]
}
// An installed plugin: one directory under <modules>/plugins/ (docs/todo/PLUGINS_PLAN.md).
// `categories` is what it actually ships on disk, not what its manifest claims.
//
// THREE fault fields, kept apart because they fail for unrelated reasons: `error` is a manifest that
// would not parse, `warning` is the advisory requiresCecelia mismatch (never a hard block, absent on
// a dev checkout), `problems` is a `contributions` block that disagrees with the directory.
export interface PluginEntry {
  name: string; dir: string; version: string; description: string; homepage: string
  categories: string[]; contributions: PluginContributions
  error: string | null; warning: string | null; problems: string[]
}
// A fun_name a module registered but did NOT get, because a higher tier already owned it
// (built-in > hand-dropped > plugin). NOT a load failure — the file loaded fine, so this is the only
// place that explains why its task is missing from the UI.
// A curated, vouched-for plugin (shipped with the app). `installed` is matched on the directory the
// url would install to, not the manifest name — that is author-controlled.
// An example plugin that ships IN THIS CHECKOUT (docs/examples/plugins). That copy is the SOURCE the
// GitHub mirror is published from, so on a checkout it is newer than what an install would fetch —
// installing from here skips the network entirely. Empty in an installed app with no `docs/`.
export interface BundledPlugin { name: string; dir: string; description: string; version: string }
export interface RegistryPlugin {
  name: string; url: string; description: string; categories: string[]; ref?: string; installed: boolean
}
export interface TaskClash {
  funName: string; path: string; plugin: string | null
  tier: string; winner: string | null; winnerTier: string
}

export const useCustomModulesStore = defineStore('customModules', () => {
  const dir        = ref('')
  const modules    = ref<CustomModuleEntry[]>([])
  const categories = ref<CustomCategory[]>([])
  const plugins    = ref<PluginEntry[]>([])
  const clashes    = ref<TaskClash[]>([])
  const registry   = ref<RegistryPlugin[]>([])
  const bundled    = ref<BundledPlugin[]>([])
  const loading    = ref(false)
  let   loadedOnce = false

  function apply(data: { dir?: string; modules?: CustomModuleEntry[]; categories?: CustomCategory[]
                         plugins?: PluginEntry[]; clashes?: TaskClash[]; registry?: RegistryPlugin[]
                         bundled?: BundledPlugin[] }) {
    if (data.dir !== undefined) dir.value = data.dir
    if (data.modules)    modules.value    = data.modules
    if (data.categories) categories.value = data.categories
    if (data.plugins)    plugins.value    = data.plugins
    if (data.bundled)    bundled.value    = data.bundled
    if (data.clashes)    clashes.value    = data.clashes
    if (data.registry)   registry.value   = data.registry
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

  // `apply` is exported because /api/plugins/{install,remove} return the SAME payload as the
  // status route — the caller refreshes from the response instead of a follow-up fetch that could
  // race the reload those endpoints just performed.
  return { dir, modules, categories, plugins, clashes, registry, bundled, loading, apply, ensureLoaded, refresh, reload }
})
