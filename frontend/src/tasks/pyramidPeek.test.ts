import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import {
  peekPyramid, primePyramidPeekBatch,
  cachedPyramidRecommendation, _resetPyramidPeekCache,
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
      { path: '/c.oir', reader: 'unsupported' },
    ])
    await primePyramidPeekBatch(['/a.ims', '/b.ims', '/c.oir'])
    expect(cachedPyramidRecommendation('/a.ims')).toBe(4)
    expect(cachedPyramidRecommendation('/b.ims')).toBe(2)
    expect(cachedPyramidRecommendation('/c.oir')).toBeNull()
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
