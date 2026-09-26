import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import { fetchProfileSettings, patchProfileSettings } from './profileSettingsApi'

// Stub `fetch`, assert URL/method/body and the shape returned. Round-trip semantics — merge,
// null-delete, active-profile resolution — live server-side and are covered by
// `api/test/suite/profile_settings.jl`.
describe('profileSettingsApi', () => {
  const originalFetch = globalThis.fetch
  let fetchMock: ReturnType<typeof vi.fn>
  beforeEach(() => {
    fetchMock = vi.fn()
    globalThis.fetch = fetchMock as unknown as typeof fetch
  })
  afterEach(() => { globalThis.fetch = originalFetch })

  function jsonRes(status: number, body: object): Response {
    return {
      ok: status >= 200 && status < 300,
      status,
      json: async () => body,
    } as unknown as Response
  }

  describe('fetchProfileSettings', () => {
    it('returns {profile, settings} on 200', async () => {
      fetchMock.mockResolvedValue(jsonRes(200, {
        profile: 'alice',
        settings: { theme: 'dark', kiwiOpen: true, ribbonThickness: 3 },
      }))
      const r = await fetchProfileSettings()
      expect(r.profile).toBe('alice')
      expect(r.settings.theme).toBe('dark')
      expect(r.settings.kiwiOpen).toBe(true)
      expect(r.settings.ribbonThickness).toBe(3)
    })

    it('GETs /api/profile/settings with no-store cache', async () => {
      fetchMock.mockResolvedValue(jsonRes(200, { profile: 'default', settings: {} }))
      await fetchProfileSettings()
      const [url, init] = fetchMock.mock.calls[0]
      expect(url).toBe('/api/profile/settings')
      expect((init as RequestInit).cache).toBe('no-store')
    })

    it('honours apiBase prefix', async () => {
      fetchMock.mockResolvedValue(jsonRes(200, { profile: 'default', settings: {} }))
      await fetchProfileSettings('https://example.test')
      expect(fetchMock).toHaveBeenCalledWith(
        'https://example.test/api/profile/settings',
        expect.any(Object),
      )
    })

    it('falls back to {default, {}} on network failure', async () => {
      fetchMock.mockRejectedValue(new Error('network'))
      const r = await fetchProfileSettings()
      expect(r).toEqual({ profile: 'default', settings: {} })
    })

    it('falls back to {default, {}} on non-2xx', async () => {
      fetchMock.mockResolvedValue(jsonRes(503, { error: 'boot' }))
      const r = await fetchProfileSettings()
      expect(r).toEqual({ profile: 'default', settings: {} })
    })

    it('tolerates a malformed body without throwing', async () => {
      fetchMock.mockResolvedValue({
        ok: true, status: 200,
        json: async () => { throw new Error('bad json') },
      } as unknown as Response)
      const r = await fetchProfileSettings()
      expect(r).toEqual({ profile: 'default', settings: {} })
    })
  })

  describe('patchProfileSettings', () => {
    it('POSTs the patch body as JSON with Content-Type + keepalive', async () => {
      fetchMock.mockResolvedValue(jsonRes(200, {
        profile: 'alice', settings: { theme: 'dark' },
      }))
      await patchProfileSettings({ theme: 'dark' })
      const [url, init] = fetchMock.mock.calls[0]
      expect(url).toBe('/api/profile/settings/patch')
      const req = init as RequestInit
      expect(req.method).toBe('POST')
      expect(req.keepalive).toBe(true)
      expect((req.headers as Record<string, string>)['Content-Type']).toBe('application/json')
      expect(JSON.parse(req.body as string)).toEqual({ theme: 'dark' })
    })

    it('preserves null values in the wire payload (server-side delete semantic)', async () => {
      fetchMock.mockResolvedValue(jsonRes(200, { profile: 'alice', settings: {} }))
      await patchProfileSettings({ ribbonThickness: null })
      const [, init] = fetchMock.mock.calls[0]
      expect(JSON.parse((init as RequestInit).body as string)).toEqual({ ribbonThickness: null })
    })

    it('returns the merged {profile, settings} on 200', async () => {
      fetchMock.mockResolvedValue(jsonRes(200, {
        profile: 'alice', settings: { theme: 'dark', kiwiOpen: false },
      }))
      const r = await patchProfileSettings({ kiwiOpen: false })
      expect(r.profile).toBe('alice')
      expect(r.settings.kiwiOpen).toBe(false)
      expect(r.settings.theme).toBe('dark')
    })

    it('honours apiBase prefix', async () => {
      fetchMock.mockResolvedValue(jsonRes(200, { profile: 'default', settings: {} }))
      await patchProfileSettings({ x: 1 }, 'https://example.test')
      expect(fetchMock.mock.calls[0][0]).toBe('https://example.test/api/profile/settings/patch')
    })

    it('returns {profile: "", settings: patch} on network failure', async () => {
      fetchMock.mockRejectedValue(new Error('network'))
      const r = await patchProfileSettings({ theme: 'dark' })
      expect(r.profile).toBe('')
      expect(r.settings).toEqual({ theme: 'dark' })
    })

    it('returns {profile: "", settings: patch} on non-2xx', async () => {
      fetchMock.mockResolvedValue(jsonRes(500, { error: 'boom' }))
      const r = await patchProfileSettings({ theme: 'dark' })
      expect(r.profile).toBe('')
      expect(r.settings).toEqual({ theme: 'dark' })
    })
  })
})
