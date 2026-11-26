// The names lenses were given, by score column (`lensNamesOf`) — for any list that shows a lens score
// (Population positions' Colour). Read from Movies & figures (a lens IS a figure there, renamed there or
// in the viewer's Lenses section), refreshed when the server says a figure changed (`movies:changed`).
import { ref, watch, type Ref } from 'vue'
import { usePushStore } from '../stores/push'
import type { MovieEntry } from '../utils/movies'
import { getJson, tryApi } from '../utils/api'
import { lensNamesOf } from '../utils/viewer/lensQuery'

export function useLensNames(projectUid: () => string): Ref<Record<string, string>> {
  const names = ref<Record<string, string>>({})
  const push = usePushStore()
  async function refresh() {
    const pu = projectUid()
    if (!pu) { names.value = {}; return }
    const r = await tryApi(getJson<{ movies?: MovieEntry[] }>('/api/movies', { query: { projectUid: pu } }))
    if (r.ok) names.value = lensNamesOf(r.data?.movies ?? [])
    // a non-2xx reply clears the names, as before; no reply keeps the last ones (must not rename back)
    else if (r.status !== 0) names.value = {}
  }
  watch(projectUid, () => void refresh(), { immediate: true })
  watch(() => push.tick, () => {
    const ev = push.lastEvent
    if (ev?.kind === 'movies:changed' && (!ev.projectUid || ev.projectUid === projectUid())) void refresh()
  })
  return names
}
