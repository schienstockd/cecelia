// "Edit this movie" — the arrival half. The Movies page navigates to the page that owns a movie's
// config kind with `?fromMovie=<file>.mp4`; this fetches that movie's registry entry and hands it to
// whichever page mounted it. Phase 6 of docs/todo/MOVIE_MANAGEMENT_PLAN.md.
//
// Shared because both destinations do the identical five things — read the query, fetch the entry,
// check the kind is theirs, snapshot what they are about to overwrite, and show one line with an Undo.
// Only the middle step differs (a Pinia timeline vs two per-set settings bags), so that is the one
// thing the caller supplies.
//
// THE QUERY PARAM IS CONSUMED, not left in the URL. It is an instruction, not a location: leaving it
// there means a reload silently re-applies the config over whatever the user has since edited, and the
// Undo they were offered would restore something two edits old.
import { ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { restoreKind, type MovieRegistryEntry, type RestoreKind } from '../utils/movieRestore'

/** What the page did, and how to put it back. Returned by the caller's `apply`. */
export interface RestoreResult {
  /** Undo it. Called at most once; the notice hides itself afterwards. */
  undo: () => void
  /** What could not be restored, already worded (`restoreNote`). Empty when everything came back. */
  note?: string
}

export interface RestoreNoticeState {
  /** The movie the page was filled in from — shown, and the sign the bar is up at all. */
  movie: string
  note: string
}

export function useMovieRestore(opts: {
  /** The config kind this page owns. An entry of the other kind is ignored, not an error: the two
   *  pages are both mounted by the router at different times and only one of them is the destination. */
  kind: RestoreKind
  /** The project to read the registry of. Reactive — the fetch waits for it. */
  projectUid: () => string
  /** Apply the entry. Return null to decline (nothing usable in it), else how to undo it. */
  apply: (entry: MovieRegistryEntry) => RestoreResult | null
  /** Reported to the user when the fetch itself fails. */
  onError?: (message: string) => void
}) {
  const route = useRoute()
  const router = useRouter()
  const notice = ref<RestoreNoticeState | null>(null)
  let _undo: (() => void) | null = null

  /** Take the parameter out of the URL, keeping every other query intact. */
  function consumeQuery() {
    const q = { ...route.query }
    if (q.fromMovie === undefined) return
    delete q.fromMovie
    router.replace({ path: route.path, query: q })
  }

  async function run(name: string) {
    const uid = opts.projectUid()
    if (!uid || !name) return
    consumeQuery()
    try {
      const res = await fetch(
        `/api/movies/meta?projectUid=${encodeURIComponent(uid)}&name=${encodeURIComponent(name)}`)
      if (!res.ok) throw new Error((await res.json().catch(() => ({}))).error ?? res.statusText)
      const entry = ((await res.json()) as { entry?: MovieRegistryEntry }).entry ?? {}
      // Not ours — the other page is the destination and will have handled it. Silence is right here:
      // a movie has exactly one kind, so this is a page that simply was not asked.
      if (restoreKind(entry) !== opts.kind) return
      const done = opts.apply(entry)
      if (!done) return
      _undo = done.undo
      notice.value = { movie: name, note: done.note ?? '' }
    } catch (e) {
      opts.onError?.(`Could not read the config for ${name}: ${e instanceof Error ? e.message : String(e)}`)
    }
  }

  function undo() {
    _undo?.()
    _undo = null
    notice.value = null
  }
  function dismiss() {
    _undo = null
    notice.value = null
  }

  // On arrival, on a second "edit" while the page is already open (the router reuses the mounted
  // component, so the query change is the only event there is), and once the project finishes loading.
  // The last case is why this waits rather than firing on mount: a deep link into a cold app mounts the
  // page before the project store has a uid, and consuming the parameter then would drop the request
  // on the floor with nothing to show for it.
  const waiting = ref('')
  watch([() => route.query.fromMovie, () => opts.projectUid()], ([n, uid]) => {
    if (typeof n === 'string' && n) waiting.value = n
    if (waiting.value && uid) {
      const name = waiting.value
      waiting.value = ''
      run(name)
    }
  }, { immediate: true })

  return { notice, undo, dismiss }
}
