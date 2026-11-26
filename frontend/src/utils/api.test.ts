import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { ApiError, apiFetch, getBuffer, getJson, postJson, tryApi, withQuery } from './api'

// The real log store needs `window` (its cross-window channel); the client only calls `warn`.
const logged: { message: string; source?: string }[] = []
let noPinia = false
vi.mock('../stores/log', () => ({
  useLogStore: () => {
    if (noPinia) throw new Error('getActivePinia() was called but there was no active Pinia')
    return { warn: (message: string, opts?: { source?: string }) => logged.push({ message, source: opts?.source }) }
  },
}))

// The backend's error shape: `_gerr(status, msg) = (status, JSON3.write((; error = msg)))` (api/src/gating_api.jl).
const jsonReply = (status: number, body: unknown) =>
  new Response(JSON.stringify(body), { status, headers: { 'Content-Type': 'application/json' } })

let fetchMock: ReturnType<typeof vi.fn>
beforeEach(() => {
  logged.length = 0
  noPinia = false
  fetchMock = vi.fn()
  vi.stubGlobal('fetch', fetchMock)
})
afterEach(() => { vi.unstubAllGlobals() })

const apiLines = () => logged.filter(e => e.source === 'api').map(e => e.message)

describe('api client', () => {
  it('a binary read of an error reply throws the server message instead of decoding it as floats', async () => {
    fetchMock.mockResolvedValue(jsonReply(400, { error: 'No values for CD4 / CD8 on cells' }))
    const err = await getBuffer('/api/gating/plotdata?x=CD4').catch(e => e)
    expect(err).toBeInstanceOf(ApiError)
    expect(err.status).toBe(400)
    expect(err.message).toBe('No values for CD4 / CD8 on cells')
    expect(err.route).toBe('/api/gating/plotdata')
    expect(apiLines()).toEqual(['GET /api/gating/plotdata failed — No values for CD4 / CD8 on cells'])
  })

  it('a 2xx binary body comes back untouched', async () => {
    const buf = new Float32Array([1, 2, 3]).buffer
    fetchMock.mockResolvedValue(new Response(buf, { status: 200 }))
    expect(Array.from(new Float32Array(await getBuffer('/api/gating/plotdata')))).toEqual([1, 2, 3])
    expect(apiLines()).toEqual([])
  })

  it('no reply is status 0, logged, and says the backend did not answer', async () => {
    fetchMock.mockRejectedValue(new TypeError('Failed to fetch'))
    const r = await tryApi(getJson('/api/projects'))
    expect(r).toMatchObject({ ok: false, status: 0 })
    expect(!r.ok && r.error).toContain('no reply from the backend')
    expect(apiLines()).toHaveLength(1)
  })

  it('postJson sends JSON and parses the reply; an empty reply is null', async () => {
    fetchMock.mockResolvedValueOnce(jsonReply(200, { ok: true, uid: 'abc' }))
    expect(await postJson('/api/projects/create', { name: 'p' })).toEqual({ ok: true, uid: 'abc' })
    const [url, init] = fetchMock.mock.calls[0]
    expect(url).toBe('/api/projects/create')
    expect(init.method).toBe('POST')
    expect(init.headers['Content-Type']).toBe('application/json')
    expect(JSON.parse(init.body)).toEqual({ name: 'p' })
    fetchMock.mockResolvedValueOnce(new Response(null, { status: 204 }))
    expect(await postJson('/api/x', {})).toBeNull()
  })

  it('a 409 the caller handles is quiet, and its body (the current state) reaches the caller', async () => {
    fetchMock.mockResolvedValue(jsonReply(409, { code: 'stale_version', boards: { version: 7 } }))
    const r = await tryApi(postJson('/api/projects/boards', {}, { quiet: [409] }))
    expect(r).toMatchObject({ ok: false, status: 409, body: { code: 'stale_version', boards: { version: 7 } } })
    expect(apiLines()).toEqual([])
  })

  it('a non-JSON error body becomes the message; no body falls back to the status', async () => {
    fetchMock.mockResolvedValueOnce(new Response('Internal error', { status: 500 }))
    expect((await getJson('/api/a').catch((e: Error) => e) as Error).message).toBe('Internal error')
    fetchMock.mockResolvedValueOnce(new Response(null, { status: 404 }))
    expect((await getJson('/api/b').catch((e: Error) => e) as Error).message).toBe('HTTP 404')
  })

  it('an abort is re-thrown as the AbortError and not logged — the caller asked for it', async () => {
    fetchMock.mockRejectedValue(new DOMException('aborted', 'AbortError'))
    await expect(tryApi(getJson('/api/a'))).rejects.toMatchObject({ name: 'AbortError' })
    expect(apiLines()).toEqual([])
  })

  it('timeoutMs running out is a reported failure with code timeout, not a silent abort', async () => {
    fetchMock.mockRejectedValue(new DOMException('signal timed out', 'TimeoutError'))
    const r = await tryApi(postJson('/api/preview/start', {}, { timeoutMs: 15000 }))
    expect(r).toMatchObject({ ok: false, status: 0, code: 'timeout', error: 'No answer within 15 s' })
    expect(apiLines()).toEqual(['POST /api/preview/start failed — No answer within 15 s'])
  })

  it("the backend's machine-readable code is carried on the error", async () => {
    fetchMock.mockResolvedValue(jsonReply(409, { error: 'busy', code: 'already_running' }))
    const err = await postJson('/api/preview/start', {}).catch((e: Error) => e) as ApiError
    expect([err.status, err.code, err.message]).toEqual([409, 'already_running', 'busy'])
  })

  it('without Pinia the error still reaches the caller', async () => {
    noPinia = true
    fetchMock.mockResolvedValue(jsonReply(500, { error: 'boom' }))
    await expect(apiFetch('/api/a')).rejects.toMatchObject({ status: 500, message: 'boom' })
  })

  it('withQuery drops null/undefined, repeats arrays and appends to an existing query', () => {
    expect(withQuery('/api/a', { x: 1, y: null, z: undefined, t: ['a', 'b'], s: 'a b' }))
      .toBe('/api/a?x=1&t=a&t=b&s=a+b')
    expect(withQuery('/api/a?p=1', { q: 2 })).toBe('/api/a?p=1&q=2')
    expect(withQuery('/api/a', {})).toBe('/api/a')
  })
})
