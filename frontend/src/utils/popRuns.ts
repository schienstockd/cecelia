/**
 * Which clustering RUN's populations a population list shows. A cluster pop (cell clusters, track
 * clusters, regions) picks its clusters from one run — `clusters.tissues` vs `clusters.tissues_af` —
 * and the server tags every pop with that run (`run`, "" for gates and derived pops). Two runs on one
 * segmentation used to list as one long run-mixed list in the viewer and the canvas picker; this is
 * the one rule both use to show only the runs picked.
 *
 * What is HIDDEN is stored (not what is shown), so a run the user has not touched — a new one — shows.
 * Kept per project in profile storage and shared by the viewer and the canvas picker: the run you are
 * working with is the same question in both. The `storage` event carries a change to the pop-out
 * viewer window.
 */
import { ref, watch, type Ref } from 'vue'
import { profileStorage } from './profileStorage'

/** A run within one segmentation × pop type — run names repeat across them. */
export const runKey = (vn: string, popType: string, run: string) => `${vn}::${popType}::${run}`

/** The distinct runs of a list of pops, in first-seen order; pops with no run ("" — gates) don't count. */
export function popRuns(pops: readonly { run?: string }[]): string[] {
  const out: string[] = []
  for (const p of pops) if (p.run && !out.includes(p.run)) out.push(p.run)
  return out
}

/** Whether a pop of `run` is listed — a pop with no run always is. */
export const runShown = (hidden: ReadonlySet<string>, vn: string, popType: string, run: string | undefined) =>
  !run || !hidden.has(runKey(vn, popType, run))

/** The runs of `all` still shown. */
export const shownRuns = (hidden: ReadonlySet<string>, vn: string, popType: string, all: readonly string[]) =>
  all.filter(r => runShown(hidden, vn, popType, r))

/** `hidden` with exactly `shown` of `all` shown. Refuses to hide every run (→ `hidden` unchanged): a
 *  list emptied by its own filter reads as "no populations", and its run chips would go with it. */
export function withShownRuns(hidden: ReadonlySet<string>, vn: string, popType: string,
                              all: readonly string[], shown: readonly string[]): Set<string> {
  const next = new Set(hidden)
  if (!all.some(r => shown.includes(r))) return next
  for (const r of all) {
    if (shown.includes(r)) next.delete(runKey(vn, popType, r))
    else next.add(runKey(vn, popType, r))
  }
  return next
}

const storeKey = (projectUid: string) => `cc.popRunsHidden.${projectUid}`

function readHidden(projectUid: string): Set<string> {
  if (!projectUid) return new Set()
  try {
    const v = JSON.parse(profileStorage.getItem(storeKey(projectUid)) ?? '[]')
    return new Set(Array.isArray(v) ? v.filter((x): x is string => typeof x === 'string') : [])
  } catch { return new Set() }
}

/** The project's hidden runs, live: `hidden` follows the project and other windows; `setShown` stores. */
export function usePopRunFilter(projectUid: () => string): {
  hidden: Ref<Set<string>>
  setShown: (vn: string, popType: string, all: readonly string[], shown: readonly string[]) => void
} {
  const hidden = ref(readHidden(projectUid()))
  watch(projectUid, uid => { hidden.value = readHidden(uid) })
  if (typeof window !== 'undefined') {
    window.addEventListener('storage', e => {
      if (e.key === storeKey(projectUid())) hidden.value = readHidden(projectUid())
    })
  }
  function setShown(vn: string, popType: string, all: readonly string[], shown: readonly string[]) {
    const uid = projectUid()
    const next = withShownRuns(hidden.value, vn, popType, all, shown)
    hidden.value = next
    if (uid) profileStorage.setItem(storeKey(uid), JSON.stringify([...next].sort()))
  }
  return { hidden, setShown }
}
