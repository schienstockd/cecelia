import { defineStore, acceptHMRUpdate } from 'pinia'
import { ref } from 'vue'
import type { TaskDef } from '../tasks/types'

// A session-long label/identity index of the task definitions — fetched ONCE, never refetched.
// It deliberately holds no `params`: their `optionsFrom` options (flow-model vault, cellpose
// checkpoints, …) are resolved server-side per request and change mid-session, so a once-fetched
// copy goes stale. A caller that needs params or defaults reads them through `useTaskDefs`, which
// refetches on activation — the type below makes `byFn.get(fn).params` a compile error here.
export type TaskDefSummary = Omit<TaskDef, 'params'>

export function summarizeTaskDefs(data: Record<string, TaskDef[]>): Map<string, TaskDefSummary> {
  const map = new Map<string, TaskDefSummary>()
  for (const [cat, defs] of Object.entries(data)) {
    for (const def of defs) {
      const { params: _params, ...rest } = def
      map.set(def.fun_name, { ...rest, category: cat })
    }
  }
  return map
}

export const useTaskDefsStore = defineStore('taskDefs', () => {
  const byFn  = ref(new Map<string, TaskDefSummary>())
  // the in-flight load — a second caller awaits it instead of returning before the defs exist
  let loading: Promise<void> | null = null

  async function ensureLoaded() {
    if (byFn.value.size > 0) return
    loading ??= (async () => {
      try {
        const res = await fetch('/api/tasks/definitions')
        if (!res.ok) return
        byFn.value = summarizeTaskDefs(await res.json() as Record<string, TaskDef[]>)
      } catch { /* ignore — caller falls back to fn name */ }
      finally { loading = null }
    })()
    return loading
  }

  function labelFor(fn: string): string {
    return byFn.value.get(fn)?.label ?? (fn.split('.').pop() ?? fn)
  }

  function all(): TaskDefSummary[] {
    return [...byFn.value.values()]
  }

  return { byFn, ensureLoaded, labelFor, all }
})

// Replace the live instance on hot-reload — see the note in `stores/customModules.ts`.
if (import.meta.hot) import.meta.hot.accept(acceptHMRUpdate(useTaskDefsStore, import.meta.hot))
