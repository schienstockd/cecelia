import { ref, onMounted } from 'vue'
import type { TaskDef } from '../tasks/types'

// Fetches task definitions from the package-owned JSON specs via the API.
// Retries automatically (up to MAX_RETRIES times, RETRY_DELAY ms apart) so that
// a cold server start or brief unavailability doesn't leave the panel empty.
// Exposes `reload` for the manual refresh button shown when defs are empty.
const MAX_RETRIES  = 5
const RETRY_DELAY  = 2000

export function useTaskDefs(category: string) {
  const defs    = ref<TaskDef[]>([])
  const loading = ref(false)

  async function load(attempt = 0): Promise<void> {
    loading.value = true
    try {
      const res = await fetch(`/api/tasks/definitions?category=${encodeURIComponent(category)}`)
      if (!res.ok) throw new Error(`HTTP ${res.status}`)
      const data = await res.json() as Record<string, TaskDef[]>
      defs.value = data[category] ?? []
    } catch (e) {
      if (attempt < MAX_RETRIES) {
        await new Promise(r => setTimeout(r, RETRY_DELAY))
        return load(attempt + 1)
      }
      console.warn(`[useTaskDefs] Failed to load defs for "${category}" after ${MAX_RETRIES} retries:`, e)
    } finally {
      loading.value = false
    }
  }

  async function reload() {
    defs.value = []
    await load(0)
  }

  onMounted(() => load(0))

  return { defs, loading, reload }
}
