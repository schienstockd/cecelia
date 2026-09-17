import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import {
  peekPyramid, primePyramidPeekBatch,
  cachedPyramidRecommendation, cachedPyramidPeek,
  isFastPeekPath, isPeekInFlight, pyramidPeekRev,
  _resetPyramidPeekCache,
} from './pyramidPeek'

describe('pyramidPeek — shared cache the advisor + TaskRunner both read', () => {
  const _fetch = globalThis.fetch
  beforeEach(() => { _resetPyramidPeekCache() })
  afterEach(() => { globalThis.fetch = _fetch })

  const mockFetch = (results: unknown[]) => {
    globalThis.fetch = vi.fn().mockResolvedValue({
      ok: true, json: async () => ({ results }),
    }) as unknown as typeof fetch
  }

  it('cachedPyramidRecommendation is UNDEFINED for a path we never asked about', () => {
    // The undefined-vs-null distinction is load-bearing: TaskRunner uses undefined to mean "still
    // unknown, do not pre-fill" and null to mean "we asked and there is nothing to say".
    expect(cachedPyramidRecommendation('/never/asked')).toBeUndefined()
  })

  it('caches the recommendation after a successful peek so TaskRunner can read it sync', async () => {
    mockFetch([{ path: '/x.ims', reader: 'h5py', nX: 1111, nY: 1057, nT: 181, recommendedPyramidLevels: 4 }])
    await peekPyramid('/x.ims')
    expect(cachedPyramidRecommendation('/x.ims')).toBe(4)
  })

  it('caches NULL after a failed peek — a settled "nothing to say" answer, not "still loading"', async () => {
    globalThis.fetch = vi.fn().mockResolvedValue({ ok: false }) as unknown as typeof fetch
    await peekPyramid('/x.ims')
    expect(cachedPyramidRecommendation('/x.ims')).toBeNull()
  })

  it('caches NULL for a reader-unsupported source (no recommendation to give)', async () => {
    mockFetch([{ path: '/x.oir', reader: 'unsupported' }])
    await peekPyramid('/x.oir')
    // The peek settled, but the recommendation field is missing → cached view is null.
    expect(cachedPyramidRecommendation('/x.oir')).toBeNull()
  })

  it('de-dupes concurrent requests for the same path onto one in-flight fetch', async () => {
    const fetchSpy = vi.fn().mockResolvedValue({
      ok: true, json: async () => ({ results: [{ path: '/x.ims', reader: 'h5py', recommendedPyramidLevels: 3 }] }),
    })
    globalThis.fetch = fetchSpy as unknown as typeof fetch
    // Two callers, one HTTP request.
    await Promise.all([peekPyramid('/x.ims'), peekPyramid('/x.ims'), peekPyramid('/x.ims')])
    expect(fetchSpy).toHaveBeenCalledTimes(1)
  })

  it('batch primer routes each result back to its own path', async () => {
    mockFetch([
      { path: '/a.ims', reader: 'h5py', nT: 100, recommendedPyramidLevels: 4 },
      { path: '/b.ims', reader: 'h5py', nT: 1,   recommendedPyramidLevels: 2 },
    ])
    await primePyramidPeekBatch(['/a.ims', '/b.ims'])
    expect(cachedPyramidRecommendation('/a.ims')).toBe(4)
    expect(cachedPyramidRecommendation('/b.ims')).toBe(2)
  })

  it('batch primer SKIPS JVM-eligible extensions — those wait for the wizard-open lazy trigger', async () => {
    // A .czi/.oir batch of 20 files would otherwise spin the JVM 20 times up front for a
    // recommendation the user only ever sees for the first image in the wizard. So the batch
    // primer filters to fast-reader paths only; JVM extensions stay unsettled until the advisor
    // asks for them.
    const fetchSpy = vi.fn().mockResolvedValue({
      ok: true, json: async () => ({ results: [{ path: '/a.ims', reader: 'h5py', recommendedPyramidLevels: 3 }] }),
    })
    globalThis.fetch = fetchSpy as unknown as typeof fetch
    await primePyramidPeekBatch(['/a.ims', '/b.czi', '/c.oir', '/d.nd2'])
    // Only /a.ims was in the outgoing request.
    expect(fetchSpy).toHaveBeenCalledTimes(1)
    const body = JSON.parse((fetchSpy.mock.calls[0][1] as RequestInit).body as string) as { paths: string[] }
    expect(body.paths).toEqual(['/a.ims'])
    expect(cachedPyramidRecommendation('/a.ims')).toBe(3)
    // JVM extensions are UNDEFINED (not asked yet), not null (asked and got nothing) — the
    // wizard-open path treats those two states differently.
    expect(cachedPyramidRecommendation('/b.czi')).toBeUndefined()
    expect(cachedPyramidRecommendation('/c.oir')).toBeUndefined()
    expect(cachedPyramidRecommendation('/d.nd2')).toBeUndefined()
  })

  it('isFastPeekPath classifies extensions the way the backend does', () => {
    // Fast readers on the backend — mirrors `_READERS_BY_SUFFIX` in peek_pyramid_run.py.
    expect(isFastPeekPath('/img.tif')).toBe(true)
    expect(isFastPeekPath('/img.ome.tif')).toBe(true)
    expect(isFastPeekPath('/img.ome.tiff')).toBe(true)
    expect(isFastPeekPath('/img.lif')).toBe(true)
    expect(isFastPeekPath('/img.ims')).toBe(true)
    expect(isFastPeekPath('/IMG.TIF')).toBe(true)
    // JVM-eligible / unknown — anything else is deferred.
    expect(isFastPeekPath('/img.czi')).toBe(false)
    expect(isFastPeekPath('/img.oir')).toBe(false)
    expect(isFastPeekPath('/img.nd2')).toBe(false)
    expect(isFastPeekPath('/img.lsm')).toBe(false)
  })

  it('isPeekInFlight tracks the in-flight window and the settled bump comes through pyramidPeekRev', async () => {
    // The advisor uses this to render "Peeking source dims…" during a JVM cold-start, and the
    // rev counter to re-run once the peek settles. Both signals matter — a stuck placeholder
    // that never clears would be worse than showing nothing.
    let resolveFetch: (v: unknown) => void
    const fetchPromise = new Promise(r => { resolveFetch = r })
    globalThis.fetch = vi.fn().mockReturnValue(fetchPromise) as unknown as typeof fetch
    const before = pyramidPeekRev()
    expect(isPeekInFlight('/x.czi')).toBe(false)
    const req = peekPyramid('/x.czi')
    expect(isPeekInFlight('/x.czi')).toBe(true)
    resolveFetch!({ ok: true, json: async () => ({ results: [{ path: '/x.czi', reader: 'showinf', recommendedPyramidLevels: 4 }] }) })
    await req
    expect(isPeekInFlight('/x.czi')).toBe(false)
    expect(pyramidPeekRev()).toBeGreaterThan(before)
    // And the full-result read is populated so the advisor can render dims (not just the number).
    expect(cachedPyramidPeek('/x.czi')?.reader).toBe('showinf')
  })

  it('batch primer + singular fetch share the same cache (singular waits on the batch)', async () => {
    // If the batch is already in flight for a path, a singular call for that path MUST NOT fire a
    // second HTTP request — it should resolve to the batch's result. Otherwise adding N images
    // would trigger 1 batch + N singulars once every form opens.
    const fetchSpy = vi.fn().mockResolvedValue({
      ok: true, json: async () => ({ results: [{ path: '/x.ims', reader: 'h5py', recommendedPyramidLevels: 5 }] }),
    })
    globalThis.fetch = fetchSpy as unknown as typeof fetch
    const batchPromise = primePyramidPeekBatch(['/x.ims'])
    const singularPromise = peekPyramid('/x.ims')
    await Promise.all([batchPromise, singularPromise])
    expect(fetchSpy).toHaveBeenCalledTimes(1)
    expect(cachedPyramidRecommendation('/x.ims')).toBe(5)
  })
})
