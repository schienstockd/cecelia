import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import {
  listCaptures, getCapture, addressLine, relativeAgo,
  publishCapturesTick, capturesTickKey,
  type CaptureAddress,
} from './capturesApi'

const _fetch = globalThis.fetch
beforeEach(() => { (globalThis as { fetch: unknown }).fetch = vi.fn() })
afterEach(()  => { (globalThis as { fetch: unknown }).fetch = _fetch })

function mockResponse(body: unknown, init: { status?: number; ok?: boolean } = {}) {
  return {
    ok: init.ok ?? (init.status ?? 200) < 400,
    status: init.status ?? 200,
    json: async () => body,
  } as unknown as Response
}

describe('listCaptures', () => {
  it('builds URL with projectUid + limit, returns items array', async () => {
    const fetchMock = vi.mocked(globalThis.fetch)
    fetchMock.mockResolvedValueOnce(mockResponse({ items: [{ captureId: 'cap-1' }] }))
    const items = await listCaptures('proj', 5)
    expect(fetchMock).toHaveBeenCalledOnce()
    const url = fetchMock.mock.calls[0][0] as string
    expect(url).toContain('/api/viewer/captures?')
    expect(url).toContain('projectUid=proj')
    expect(url).toContain('limit=5')
    expect(items).toEqual([{ captureId: 'cap-1' }])
  })
  it('returns [] when projectUid empty (no request)', async () => {
    const fetchMock = vi.mocked(globalThis.fetch)
    expect(await listCaptures('', 5)).toEqual([])
    expect(fetchMock).not.toHaveBeenCalled()
  })
  it('returns [] when body has no items array', async () => {
    vi.mocked(globalThis.fetch).mockResolvedValueOnce(mockResponse({}))
    expect(await listCaptures('proj')).toEqual([])
  })
  it('throws on non-ok status', async () => {
    vi.mocked(globalThis.fetch).mockResolvedValueOnce(mockResponse({}, { status: 500 }))
    await expect(listCaptures('proj')).rejects.toThrow(/HTTP 500/)
  })
  it('encodes projectUid safely', async () => {
    const fetchMock = vi.mocked(globalThis.fetch)
    fetchMock.mockResolvedValueOnce(mockResponse({ items: [] }))
    await listCaptures('has space')
    const url = fetchMock.mock.calls[0][0] as string
    expect(url).toContain('projectUid=has%20space')
  })
})

describe('getCapture', () => {
  it('builds URL with projectUid + captureId', async () => {
    const fetchMock = vi.mocked(globalThis.fetch)
    fetchMock.mockResolvedValueOnce(mockResponse({ capture: { captureId: 'cap-1' }, frame: 'data:…' }))
    const env = await getCapture('proj', 'cap-20260919T140000-abcdef')
    const url = fetchMock.mock.calls[0][0] as string
    expect(url).toContain('/api/viewer/capture?')
    expect(url).toContain('projectUid=proj')
    expect(url).toContain('captureId=cap-20260919T140000-abcdef')
    expect(env?.capture.captureId).toBe('cap-1')
  })
  it('returns null on 404 (a hallucinated id / deleted project)', async () => {
    vi.mocked(globalThis.fetch).mockResolvedValueOnce(mockResponse({}, { status: 404 }))
    expect(await getCapture('proj', 'cap-nope')).toBeNull()
  })
  it('returns null when either id is empty (no request)', async () => {
    const fetchMock = vi.mocked(globalThis.fetch)
    expect(await getCapture('', 'cap-1')).toBeNull()
    expect(await getCapture('proj', '')).toBeNull()
    expect(fetchMock).not.toHaveBeenCalled()
  })
  it('throws on non-ok status other than 404', async () => {
    vi.mocked(globalThis.fetch).mockResolvedValueOnce(mockResponse({}, { status: 500 }))
    await expect(getCapture('proj', 'cap-1')).rejects.toThrow(/HTTP 500/)
  })
})

describe('addressLine — human-readable one-liner', () => {
  it('omits missing fields, joins with the middle dot', () => {
    const a: CaptureAddress = { projectUid: 'p', imageUid: 'IMG1', valueName: 'flowTom', t: 3, z: 5 }
    expect(addressLine(a)).toBe('IMG1 · flowTom · t=3 · z=5')
  })
  it('renders a slab range', () => {
    expect(addressLine({ projectUid: 'p', imageUid: 'IMG1', t: [2, 7] }))
      .toBe('IMG1 · t=2..7')
  })
  it('is empty when no meaningful fields present', () => {
    expect(addressLine({ projectUid: 'p' })).toBe('')
    expect(addressLine(null)).toBe('')
    expect(addressLine(undefined)).toBe('')
  })
})

describe('relativeAgo', () => {
  it('under a minute reads "just now"', () => {
    expect(relativeAgo(1000, 1030)).toBe('just now')
  })
  it('minutes vs hours vs date', () => {
    expect(relativeAgo(0, 5 * 60)).toBe('5 min ago')
    expect(relativeAgo(0, 2 * 3600)).toBe('2 h ago')
    // past a day → an absolute date (locale-formatted; just assert non-empty and not 'ago')
    const s = relativeAgo(0, 2 * 86400)
    expect(s).not.toContain('ago')
    expect(s.length).toBeGreaterThan(0)
  })
})

describe('publishCapturesTick — cross-window Save→refetch signal', () => {
  it('names a stable key so writer + reader agree on what to watch', () => {
    // Value shape isn't the contract; the KEY is (the reader listens for `storage` events with
    // this key). Pin it here so a rename in one file without the other is caught.
    expect(capturesTickKey()).toBe('cc.viewer.capturesTick')
  })
  it('is a no-op when window is absent (node env)', () => {
    // The tests run without jsdom, so this exercises the SSR guard — the function must not throw.
    expect(() => publishCapturesTick()).not.toThrow()
  })
})
