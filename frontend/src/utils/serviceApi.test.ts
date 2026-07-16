import { describe, it, expect, vi, afterEach } from 'vitest'
import { svcPost, notebooksApi } from './serviceApi'

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
