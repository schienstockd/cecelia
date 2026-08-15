// The `name` suffixes already used in this project, for the recorder's suggestion list.
//
// The three recorder panels (viewer, animation, batch) each own a suffix field but none of them lists
// movies — only the Movies page does. Rather than give all three their own fetch, this caches ONE
// list per project and hands out the distinct suffixes.
//
// Lazy on purpose: the field lives in an options popover most recordings never open, so the request
// only happens when something actually asks. Cached per project uid, because the vocabulary only
// grows when a recording finishes — and a stale-by-one-entry suggestion list is not worth a refetch
// on every popover open. `refresh()` is there for a caller that has just recorded.
import { ref } from 'vue'
import { movieSuffixesInUse, type MovieEntry } from '../utils/movies'

const cache = new Map<string, string[]>()
const inflight = new Map<string, Promise<string[]>>()

async function load(projectUid: string): Promise<string[]> {
  const res = await fetch(`/api/movies?projectUid=${encodeURIComponent(projectUid)}`)
  if (!res.ok) throw new Error(`HTTP ${res.status}`)
  const body = await res.json() as { movies?: MovieEntry[] }
  return movieSuffixesInUse(body.movies ?? [])
}

export function useMovieSuffixes() {
  const suffixes = ref<string[]>([])

  /** Populate `suffixes` for `projectUid`. Safe to call repeatedly — one request per project. */
  async function ensure(projectUid: string): Promise<void> {
    if (!projectUid) { suffixes.value = []; return }
    const hit = cache.get(projectUid)
    if (hit) { suffixes.value = hit; return }
    try {
      // de-duplicate concurrent callers: three panels can mount against one project
      let p = inflight.get(projectUid)
      if (!p) { p = load(projectUid); inflight.set(projectUid, p) }
      const names = await p
      cache.set(projectUid, names)
      suffixes.value = names
    } catch {
      suffixes.value = []      // a suggestion list is an affordance; failing to get one is not an error
    } finally {
      inflight.delete(projectUid)
    }
  }

  /** Drop the cache for `projectUid` and reload — for after a recording lands. */
  async function refresh(projectUid: string): Promise<void> {
    cache.delete(projectUid)
    await ensure(projectUid)
  }

  return { suffixes, ensure, refresh }
}
