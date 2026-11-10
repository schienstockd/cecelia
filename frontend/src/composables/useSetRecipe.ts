// The active set's recipe: what it is, what its axes suggest, and how to save a pick
// (docs/todo/GUI_CLARITY_PLAN.md, Decisions 13 and 16). Shared by the set bar's picker and the
// Welcome page; the logic is in utils/setRecipe.ts.
import { computed, ref } from 'vue'
import { useProjectStore } from '../stores/project'
import { useProjectMetaStore } from '../stores/projectMeta'
import { useLogStore } from '../stores/log'
import { writtenRecipe, suggestRecipe } from '../utils/setRecipe'

export function useSetRecipe() {
  const project = useProjectStore()
  const projectMeta = useProjectMetaStore()
  const log = useLogStore()
  const busy = ref(false)

  const set = computed(() => project.activeSet())
  const own = computed(() => writtenRecipe(set.value?.recipe))
  const suggestion = computed(() => (set.value && !own.value) ? suggestRecipe(set.value) : null)

  /** Store `id` as the active set's recipe (`null` clears it) via `POST /api/sets/recipe`. */
  async function save(id: string | null) {
    const s = set.value, pu = projectMeta.current?.uid
    if (!s || !pu) return
    busy.value = true
    try {
      const res = await fetch('/api/sets/recipe', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ projectUid: pu, setUid: s.uid, recipe: id ?? '' }),
      })
      const body = await res.json().catch(() => ({})) as { error?: string }
      if (!res.ok) throw new Error(body.error ?? `HTTP ${res.status}`)
      project.setSetRecipe(s.uid, id)
    } catch (e) {
      log.error(`Failed to set the recipe: ${e instanceof Error ? e.message : String(e)}`, { source: 'manageImages' })
    } finally {
      busy.value = false
    }
  }

  return { set, own, suggestion, busy, save }
}
