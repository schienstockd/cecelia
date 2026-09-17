// Shared per-source-path cache for `/api/import/peek-pyramid` results. Two readers:
//
//   1. `paramAdvisors.ts` — the inline "Recommended: N levels" line under `pyramidLevels`.
//   2. `TaskRunner.vue` — pre-fills the field on the omezarr importer's initial load.
//
// Two priming strategies, matched to the reader cost on the backend:
//
//   * **Eager**, batch on set-add, for FAST_EXT files (.tif/.ome.tif/.lif/.ims — pure-Python readers,
//     ~50 ms per file). By the time the user opens the wizard, the recommendation is already there.
//   * **Lazy**, per file on wizard open, for JVM formats (.czi/.nd2/.oir/... — Bio-Formats via
//     `showinf`, ~2 s cold-start). A set-add batch of 20 CZI files would otherwise pay 40 s of JVM
//     spin for a recommendation the user only ever sees for the first image (a wizard's params are
//     applied to the whole batch uniformly).
//
// Both live in a module-scope map, not a Pinia store — the data is a pure function of the source
// path (immutable while the file sits on disk), so a reactive store would add subscription noise
// without any consumer needing reactivity. `paramAdvisors.ts` triggers a re-read via its own
// `reloadOn` key on the advisor context, so once a lazy peek settles the advisor re-renders.

export interface PeekResult {
  /** Absolute source path (echoed by the server). Absent on a singular fetch's cached view;
   *  used by the batch primer to route each row back to its request path. */
  path?: string
  reader: string
  nX?: number; nY?: number; nZ?: number; nT?: number; nC?: number
  recommendedPyramidLevels?: number
  targetChunk?: number
  error?: string
}

// Fast Python readers on the backend. Anything else is either JVM-eligible (Bio-Formats) or an
// extension we don't peek at all — either way, deferred to the lazy path so set-add stays cheap.
// Kept in lockstep with `_READERS_BY_SUFFIX` in `peek_pyramid_run.py`.
const FAST_EXTS = ['.ims', '.lif', '.ome.tif', '.ome.tiff', '.tif', '.tiff'] as const

const _cache = new Map<string, Promise<PeekResult | null>>()
const _settled = new Map<string, PeekResult | null>()
// Bumps whenever `_settled` gains an entry, so a Vue computed watching this counter re-runs and
// the advisor picks up a lazy peek that landed after the wizard was already mounted.
import { ref } from 'vue'
const _rev = ref(0)

/** True if `oriPath` has one of the fast-reader extensions (batch-primed on set-add). Everything
 *  else is JVM-eligible or unrecognised — the wizard's lazy trigger owns the peek for those. */
export function isFastPeekPath(oriPath: string): boolean {
  const p = oriPath.toLowerCase()
  return FAST_EXTS.some(s => p.endsWith(s))
}

/** Fetch and cache the peek for one source path. Multiple calls for the same path share the
 *  same in-flight request. Returns null on network/parse failure — never throws. */
export function peekPyramid(oriPath: string): Promise<PeekResult | null> {
  const cached = _cache.get(oriPath)
  if (cached) return cached
  const req = (async () => {
    try {
      const r = await fetch('/api/import/peek-pyramid', {
        method:  'POST',
        headers: { 'Content-Type': 'application/json' },
        body:    JSON.stringify({ paths: [oriPath] }),
      })
      if (!r.ok) { _settled.set(oriPath, null); _rev.value++; return null }
      const body = await r.json() as { results?: PeekResult[] }
      const result = body.results?.[0] ?? null
      _settled.set(oriPath, result); _rev.value++
      return result
    } catch {
      _settled.set(oriPath, null); _rev.value++
      return null
    }
  })()
  _cache.set(oriPath, req)
  return req
}

/** Fire-and-forget prime: request the peek so a later read finds it settled. Used by
 *  ManageImagesModule after `/api/images/register` so the omezarr form's first open has the
 *  recommendation without waiting for a network round-trip. */
export function primePyramidPeek(oriPath: string): void {
  void peekPyramid(oriPath)
}

/** Batch prime: N paths in one request. Skips JVM-eligible extensions — those wait for the lazy
 *  wizard-open trigger, so a set of 20 CZI files doesn't spin the JVM 20 times up front for a
 *  recommendation the user will only ever see for the first image in the wizard. Populates the
 *  same cache the singular primer / fetcher use, so callers don't need to know which strategy
 *  handled a given path. */
export async function primePyramidPeekBatch(paths: string[]): Promise<void> {
  const fresh = paths.filter(p => p && isFastPeekPath(p) && !_cache.has(p))
  if (!fresh.length) return
  // Register each path's promise upfront so a concurrent singular call de-dupes onto this batch.
  let resolve: (r: (PeekResult | null)[]) => void
  const batch = new Promise<(PeekResult | null)[]>(r => { resolve = r })
  fresh.forEach((p, i) => _cache.set(p, batch.then(rs => {
    const r = rs[i] ?? null
    _settled.set(p, r); _rev.value++
    return r
  })))
  try {
    const r = await fetch('/api/import/peek-pyramid', {
      method:  'POST',
      headers: { 'Content-Type': 'application/json' },
      body:    JSON.stringify({ paths: fresh }),
    })
    if (!r.ok) { resolve!(fresh.map(() => null)); return }
    const body = await r.json() as { results?: PeekResult[] }
    const byPath = new Map<string, PeekResult>()
    for (const res of body.results ?? []) {
      if (res.path) byPath.set(res.path, res)
    }
    resolve!(fresh.map(p => byPath.get(p) ?? null))
  } catch {
    resolve!(fresh.map(() => null))
  }
}

/** True while a peek for `oriPath` has been requested but not settled — the advisor uses this to
 *  render "Peeking source dims…" instead of nothing during a JVM cold-start. */
export function isPeekInFlight(oriPath: string): boolean {
  return _cache.has(oriPath) && !_settled.has(oriPath)
}

/** Reactive revision counter for `_settled`. A Vue computed touching this value re-runs whenever
 *  a peek lands, which is how the advisor picks up a lazy peek that settled after mount. */
export function pyramidPeekRev(): number {
  return _rev.value
}

/** Synchronous read of a settled peek. Returns undefined if the peek has not been requested yet
 *  OR is still in flight; null if the fetch resolved with no usable result. The distinction
 *  matters for `TaskRunner`: an unknown/in-flight recommendation must NOT overwrite a saved
 *  value with the spec default (silent regression), so only a settled `null` counts as "we asked
 *  and there is nothing to say". */
export function cachedPyramidRecommendation(oriPath: string): number | null | undefined {
  if (!_settled.has(oriPath)) return undefined
  const r = _settled.get(oriPath)
  if (!r) return null
  const n = r.recommendedPyramidLevels
  return (typeof n === 'number' && n > 0) ? n : null
}

/** Synchronous read of the full settled `PeekResult`. Same undefined/null contract as
 *  `cachedPyramidRecommendation`. Used by the advisor to render the deepest-dims readout without
 *  awaiting a new fetch — pairs with `pyramidPeekRev()` so a lazy peek landing re-runs the
 *  advisor and swaps the placeholder for the real recommendation. */
export function cachedPyramidPeek(oriPath: string): PeekResult | null | undefined {
  if (!_settled.has(oriPath)) return undefined
  return _settled.get(oriPath) ?? null
}

/** Test-only: clear both maps. Never called from app code — the peek result is a pure function
 *  of the on-disk source file, so an in-session invalidation isn't needed. */
export function _resetPyramidPeekCache(): void {
  _cache.clear()
  _settled.clear()
  _rev.value = 0
}
