import { describe, it, expect, vi, afterEach } from 'vitest'
import { fetchVersions } from './versions'

function mockFetch(status: number, body: unknown) {
  return vi.fn().mockResolvedValue({
    ok: status >= 200 && status < 300,
    status,
    json: () => Promise.resolve(body),
  })
}
afterEach(() => { vi.restoreAllMocks() })

describe('fetchVersions', () => {
  it('encodes imageUids as a comma-joined list and returns the versions array', async () => {
    const f = mockFetch(200, { versions: ['v1', 'v2', 'v3'] })
    vi.stubGlobal('fetch', f)
    const res = await fetchVersions({
      projectUid: 'P', imageUids: ['A', 'B'], valueName: 'default',
    })
    expect(res).toEqual({ versions: ['v1', 'v2', 'v3'] })
    const url = f.mock.calls[0][0] as string
    // Same-parameter contract as the backend expects — a URL rearrange later still passes
    // projectUid / imageUids / valueName by name, not position.
    expect(url).toMatch(/^\/api\/versions\?/)
    expect(url).toContain('projectUid=P')
    expect(url).toContain('imageUids=A%2CB')          // encoded comma
    expect(url).toContain('valueName=default')
  })

  it('omits optional params when unset (server-side default = active value_name)', async () => {
    const f = mockFetch(200, { versions: [] })
    vi.stubGlobal('fetch', f)
    await fetchVersions({ projectUid: 'P', imageUids: ['A'] })
    const url = f.mock.calls[0][0] as string
    expect(url).not.toContain('valueName=')
    expect(url).not.toContain('field=')
  })

  it('throws the backend error message on non-2xx', async () => {
    vi.stubGlobal('fetch', mockFetch(400, { error: 'imageUids required' }))
    await expect(fetchVersions({ projectUid: 'P', imageUids: [] }))
      .rejects.toThrow('imageUids required')
  })

  it('degrades to an empty versions array when the body is malformed (never throws)', async () => {
    // The picker is decorative — never crash the inspector on a garbled response
    vi.stubGlobal('fetch', mockFetch(200, {}))
    expect((await fetchVersions({ projectUid: 'P', imageUids: ['A'] })).versions).toEqual([])
  })
})
