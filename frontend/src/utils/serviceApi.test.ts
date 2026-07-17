import { describe, it, expect, vi, afterEach } from 'vitest'
import { svcPost, notebooksApi, observerApi } from './serviceApi'

function mockFetch(status: number, body: unknown) {
  return vi.fn().mockResolvedValue({
    ok: status >= 200 && status < 300,
    status,
    json: () => Promise.resolve(body),
  })
}

afterEach(() => { vi.restoreAllMocks() })

describe('svcPost', () => {
  it('returns the parsed body on success', async () => {
    vi.stubGlobal('fetch', mockFetch(200, { starting: true, url: 'u' }))
    expect(await svcPost('/api/notebooks/launch', { projectUid: 'p' }))
      .toEqual({ starting: true, url: 'u' })
  })

  it('throws the server error message on a non-2xx response', async () => {
    vi.stubGlobal('fetch', mockFetch(500, { error: 'boom' }))
    await expect(svcPost('/x')).rejects.toThrow('boom')
  })

  it('throws HTTP <status> when the body has no error field', async () => {
    vi.stubGlobal('fetch', mockFetch(404, {}))
    await expect(svcPost('/x')).rejects.toThrow('HTTP 404')
  })

  it('POSTs JSON with the content-type header', async () => {
    const f = mockFetch(200, {})
    vi.stubGlobal('fetch', f)
    await svcPost('/api/x', { a: 1 })
    expect(f).toHaveBeenCalledWith('/api/x', expect.objectContaining({
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ a: 1 }),
    }))
  })

  it('notebooksApi.shutdown hits the one shutdown endpoint', async () => {
    const f = mockFetch(200, {})
    vi.stubGlobal('fetch', f)
    await notebooksApi.shutdown()
    expect(f.mock.calls[0][0]).toBe('/api/notebooks/shutdown')
  })
})

describe('observerApi', () => {
  it('status returns availability from the endpoint', async () => {
    vi.stubGlobal('fetch', mockFetch(200, { available: true }))
    expect(await observerApi.status()).toEqual({ available: true })
  })

  it('status degrades to unavailable on a non-2xx (never throws → drives the disabled UI)', async () => {
    vi.stubGlobal('fetch', mockFetch(500, {}))
    expect(await observerApi.status()).toEqual({ available: false })
  })

  it('status degrades to unavailable when fetch rejects', async () => {
    vi.stubGlobal('fetch', vi.fn().mockRejectedValue(new Error('offline')))
    expect(await observerApi.status()).toEqual({ available: false })
  })

  it('feedback POSTs the projectUid (+ model + trigger) to the feedback endpoint', async () => {
    const f = mockFetch(200, { ok: true, message: 'noted' })
    vi.stubGlobal('fetch', f)
    const r = await observerApi.feedback('NRUBxU', 'haiku', 'auto')
    expect(f.mock.calls[0][0]).toBe('/api/observer/feedback')
    expect(f.mock.calls[0][1].body).toBe(
      JSON.stringify({ projectUid: 'NRUBxU', model: 'haiku', trigger: 'auto' }))
    expect(r).toEqual({ ok: true, message: 'noted' })
  })
  it('feedback defaults trigger to manual', async () => {
    const f = mockFetch(200, { ok: true })
    vi.stubGlobal('fetch', f)
    await observerApi.feedback('NRUBxU')
    expect(f.mock.calls[0][1].body).toBe(
      JSON.stringify({ projectUid: 'NRUBxU', model: undefined, trigger: 'manual' }))
  })
})
