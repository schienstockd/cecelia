import { defineStore } from 'pinia'
import { ref } from 'vue'
import type { TaskDef } from '../tasks/types'

export const useTaskDefsStore = defineStore('taskDefs', () => {
  const byFn  = ref(new Map<string, TaskDef>())
  let loading = false

  async function ensureLoaded() {
    if (byFn.value.size > 0 || loading) return
    loading = true
    try {
      const res = await fetch('/api/tasks/definitions')
      if (!res.ok) return
      const data = await res.json() as Record<string, TaskDef[]>
      const map = new Map<string, TaskDef>()
      for (const [cat, defs] of Object.entries(data)) {
        for (const def of defs) map.set(def.fun_name, { ...def, category: cat })
      }
      byFn.value = map
    } catch { /* ignore — caller falls back to fn name */ }
    finally { loading = false }
  }

  function labelFor(fn: string): string {
    return byFn.value.get(fn)?.label ?? (fn.split('.').pop() ?? fn)
  }

  function defaultParamsFor(fn: string): Record<string, unknown> {
    const def = byFn.value.get(fn)
    if (!def) return {}
    const out: Record<string, unknown> = {}
    for (const p of def.params ?? []) {
      if (p.default !== undefined) out[p.key] = p.default
    }
    return out
  }

  function all(): TaskDef[] {
    return [...byFn.value.values()]
  }

  return { byFn, ensureLoaded, labelFor, defaultParamsFor, all }
})
