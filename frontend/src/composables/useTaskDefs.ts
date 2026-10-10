import { ref, onMounted, onUnmounted, onActivated, onDeactivated } from 'vue'
import type { TaskDef } from '../tasks/types'
import { useWsStore } from '../stores/ws'
import { onSystemEnvsChanged } from '../utils/systemEnvs'

// Fetches task definitions from the package-owned JSON specs via the API.
// Retries automatically (up to MAX_RETRIES times, RETRY_DELAY ms apart) so that
// a cold server start or brief unavailability doesn't leave the panel empty.
// Exposes `reload` for the manual refresh button shown when defs are empty.
const MAX_RETRIES  = 5
const RETRY_DELAY  = 2000

// `category` may be one name or several. A page whose subject spans categories (Manage images hosts
// both `importImages` and `exportImages`) passes a list and gets them concatenated in the order
// given, so the function picker reads import-then-export rather than in directory order. The route
// already returns every category when the filter is omitted — ChainModule and the taskDefs label
// store have always fetched it that way — so multi-category needs no server change.
export function useTaskDefs(category: string | string[]) {
  const cats    = Array.isArray(category) ? category : [category]
  const defs    = ref<TaskDef[]>([])
  const loading = ref(false)

  // The last form state a caller asked us to resolve options against, so an automatic RETRY re-sends
  // it. Holding it here rather than threading it through `load(attempt)` keeps the retry path honest:
  // a retry that silently dropped the form state would come back with the suggestions missing and
  // look like the file had no columns.
  let formParams: Record<string, unknown> | null = null

  async function load(attempt = 0): Promise<void> {
    loading.value = true
    try {
      // One category → let the server filter. Several → fetch all and pick, rather than firing N
      // requests that would each retry independently and land out of order.
      const parts: string[] = []
      if (cats.length === 1) parts.push(`category=${encodeURIComponent(cats[0])}`)
      // Current form values, for params whose options depend on what the user just typed (an
      // importer offering the columns of the file they picked). Sent as one JSON blob because param
      // keys are the task's own and would otherwise collide with `category`. Omitted entirely when
      // there is nothing to send, so an ordinary page load is byte-identical to before.
      if (formParams && Object.keys(formParams).length)
        parts.push(`params=${encodeURIComponent(JSON.stringify(formParams))}`)
      const qs  = parts.length ? `?${parts.join('&')}` : ''
      const res = await fetch(`/api/tasks/definitions${qs}`)
      if (!res.ok) throw new Error(`HTTP ${res.status}`)
      const data = await res.json() as Record<string, TaskDef[]>
      // `hidden` tasks stay registered and runnable (REPL, chains) but are kept out of the module
      // page's function list — their job has a purpose-built UI. Filtered HERE, not at the route:
      // ChainModule and the taskDefs label store fetch the same endpoint and must still see them.
      // Sort alphabetically WITHIN a category (server order is directory-scan, so random-looking),
      // preserving the caller's cross-category order so a multi-category page still reads
      // import-then-export rather than one mixed alphabetised block.
      defs.value = cats.flatMap(c =>
        (data[c] ?? [])
          .filter(d => !d.hidden)
          .sort((a, b) => a.label.localeCompare(b.label))
      )
    } catch (e) {
      if (attempt < MAX_RETRIES) {
        await new Promise(r => setTimeout(r, RETRY_DELAY))
        return load(attempt + 1)
      }
      console.warn(`[useTaskDefs] Failed to load defs for "${cats.join(', ')}" after ${MAX_RETRIES} retries:`, e)
    } finally {
      loading.value = false
    }
  }

  // `form` resolves param options against the values currently in the form. Passing it does NOT blank
  // `defs` first: this runs while the user is typing, and emptying the list would collapse the form
  // they are filling in and lose focus on every keystroke. The manual refresh button (no argument)
  // keeps the old blank-then-refetch behaviour, where an empty panel is the thing being fixed.
  async function reload(form?: Record<string, unknown>) {
    if (form === undefined) defs.value = []
    else formParams = form
    await load(0)
  }

  // Runtime option sources (`optionsFrom`: the flow-model vault, cellpose checkpoints, …) are resolved
  // by the server INTO these defs, so the defs go stale whenever a model is trained, renamed or deleted.
  // The module pages live under <KeepAlive> and mount once per project, so a fetch-on-mount alone
  // showed a model trained on Model Training as "None" on Segment until the page was reloaded.
  // Refetch quietly (no blanking — `load`, not `reload()`, keeps the form and the last form state) on
  // every return to the page, and on a finished task while the page is visible (a run that ends while
  // it is hidden is picked up by the activation refetch). The route costs ~5 ms. Not `useWhenVisible`:
  // the activation refetch already covers a hidden-time finish, and it must run regardless (a vault
  // rename or delete is not a task), so a deferred call on top of it would fetch twice.
  const ws = useWsStore()
  let visible = true
  let mounted = false
  const onTaskDone = (data: Record<string, unknown>) => {
    if (visible && String(data.status ?? '') === 'done') void load(0)
  }
  const onNodeDone = () => { if (visible) void load(0) }
  // An opt-in env appearing changes the cellpose model list (`list_cellpose_models` hides the v3
  // models until the env exists). A failed install or one that was already there sends no `done`.
  let offEnvsChanged: (() => void) | null = null
  onMounted(() => {
    offEnvsChanged = onSystemEnvsChanged(() => { if (visible) void load(0) })
    void load(0)
    ws.on('task:status', onTaskDone)
    ws.on('chain:node:done', onNodeDone)
  })
  // onActivated also fires right after the first mount — skip that one, onMounted already fetched
  onActivated(() => { visible = true; if (mounted) void load(0); mounted = true })
  onDeactivated(() => { visible = false })
  onUnmounted(() => {
    offEnvsChanged?.()
    ws.off('task:status', onTaskDone)
    ws.off('chain:node:done', onNodeDone)
  })

  return { defs, loading, reload }
}
