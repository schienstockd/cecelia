// The names lenses were given, by score column (`lensNamesOf`) — for any list that shows a lens score
// (Population positions' Colour). Read from Movies & figures (a lens IS a figure there, renamed there or
// in the viewer's Lenses section), refreshed when the server says a figure changed (`movies:changed`).
import { ref, watch, type Ref } from 'vue'
import { usePushStore } from '../stores/push'
import type { MovieEntry } from '../utils/movies'
import { lensNamesOf } from '../utils/viewer/lensQuery'

export function useLensNames(projectUid: () => string): Ref<Record<string, string>> {
  const names = ref<Record<string, string>>({})
  const push = usePushStore()
  async function refresh() {
    const pu = projectUid()
    if (!pu) { names.value = {}; return }
    try {
      const res = await fetch(`/api/movies?projectUid=${encodeURIComponent(pu)}`)
      names.value = res.ok ? lensNamesOf(((await res.json()).movies ?? []) as MovieEntry[]) : {}
    } catch { /* keep the last names: a failed read must not rename the list back */ }
  }
  watch(projectUid, () => void refresh(), { immediate: true })
  watch(() => push.tick, () => {
    const ev = push.lastEvent
    if (ev?.kind === 'movies:changed' && (!ev.projectUid || ev.projectUid === projectUid())) void refresh()
  })
  return names
}
