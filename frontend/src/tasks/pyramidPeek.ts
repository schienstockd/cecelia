// Shared per-source-path cache for `/api/import/peek-pyramid` results. Two readers:
//
//   1. `paramAdvisors.ts` — the inline "Recommended: N levels" line under `pyramidLevels`.
//   2. `TaskRunner.vue` — pre-fills the field on the omezarr importer's initial load.
//
// Both cache misses AND fires eagerly from `ManageImagesModule.submitRegister` on successful add,
// so by the time the user opens the form the recommendation is already available for a synchronous
// pre-fill. Live in a module-scope map, not a Pinia store — the data is a pure function of the
// source path (immutable while the file sits on disk), so a reactive store would add subscription
// noise without any consumer needing reactivity.

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

const _cache = new Map<string, Promise<PeekResult | null>>()
const _settled = new Map<string, PeekResult | null>()

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
      if (!r.ok) { _settled.set(oriPath, null); return null }
      const body = await r.json() as { results?: PeekResult[] }
      const result = body.results?.[0] ?? null
      _settled.set(oriPath, result)
      return result
    } catch {
      _settled.set(oriPath, null)
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

/** Batch prime: N paths in one request. Cheaper than N calls when the frontend already knows
 *  every path the user just added (one add-images event). Populates the same cache the singular
 *  primer / fetcher use, so callers reading with `peekPyramid(path)` don't need to know whether
 *  the batch primer ran. */
export async function primePyramidPeekBatch(paths: string[]): Promise<void> {
  const fresh = paths.filter(p => p && !_cache.has(p))
  if (!fresh.length) return
  // Register each path's promise upfront so a concurrent singular call de-dupes onto this batch.
  let resolve: (r: (PeekResult | null)[]) => void
  const batch = new Promise<(PeekResult | null)[]>(r => { resolve = r })
  fresh.forEach((p, i) => _cache.set(p, batch.then(rs => {
    const r = rs[i] ?? null
    _settled.set(p, r)
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

/** Test-only: clear both maps. Never called from app code — the peek result is a pure function
 *  of the on-disk source file, so an in-session invalidation isn't needed. */
export function _resetPyramidPeekCache(): void {
  _cache.clear()
  _settled.clear()
}
