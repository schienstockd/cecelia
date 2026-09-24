import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import {
  isValidKiwiProfileName,
  fetchKiwiProfiles,
  selectKiwiProfile,
  createKiwiProfile,
  retireKiwiProfile,
  fetchKiwiTerminalCommand,
} from './kiwiProfileApi'

// The validator mirror-tests. Same cases the backend suite pins in `api/test/suite/kiwi_profile.jl` —
// keep both green together so the dialog can't accept a name the round-trip will reject.
describe('isValidKiwiProfileName', () => {
  it('accepts lower-ASCII alnum + `-` / `_`, 1..32 chars', () => {
    expect(isValidKiwiProfileName('alice')).toBe(true)
    expect(isValidKiwiProfileName('dominik')).toBe(true)
    expect(isValidKiwiProfileName('lab-user_2')).toBe(true)
    expect(isValidKiwiProfileName('a')).toBe(true)
    expect(isValidKiwiProfileName('z'.repeat(32))).toBe(true)
  })
  it('rejects empty, too-long, uppercase, punctuation, whitespace', () => {
    expect(isValidKiwiProfileName('')).toBe(false)
    expect(isValidKiwiProfileName('z'.repeat(33))).toBe(false)
    expect(isValidKiwiProfileName('Alice')).toBe(false)
    expect(isValidKiwiProfileName('has space')).toBe(false)
    expect(isValidKiwiProfileName('with/slash')).toBe(false)
    expect(isValidKiwiProfileName('with.dot')).toBe(false)
  })
  it('rejects reserved / magic names', () => {
    expect(isValidKiwiProfileName('legacy')).toBe(false)   // D10 sentinel
    expect(isValidKiwiProfileName('default')).toBe(false)  // maps to ~/.claude
  })
})

// Fetch helpers — stub `fetch`, assert URL/method/body and the shape returned. We do NOT exercise
// the real HTTP layer here (that's what `api/test/suite/kiwi_profile.jl` covers).
describe('kiwi profile API round-trip', () => {
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

  it('fetchKiwiProfiles returns roster on 200', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, {
      active: 'alice',
      profiles: [{ name: 'default', dir: '', isDefault: true },
                 { name: 'alice',   dir: '/tmp/kiwi-profiles/alice', isDefault: false }],
      legacyReserved: ['legacy'],
    }))
    const r = await fetchKiwiProfiles()
    expect(fetchMock).toHaveBeenCalledWith('/api/kiwi/profiles')
    expect(r.active).toBe('alice')
    expect(r.profiles.map(p => p.name)).toEqual(['default', 'alice'])
    expect(r.legacyReserved).toEqual(['legacy'])
  })

  it('fetchKiwiProfiles falls back to a `default`-only roster on network failure', async () => {
    fetchMock.mockRejectedValue(new Error('network'))
    const r = await fetchKiwiProfiles()
    expect(r.active).toBe('default')
    expect(r.profiles).toEqual([{ name: 'default', dir: '', isDefault: true, retired: false }])
  })

  it('selectKiwiProfile POSTs and returns ok+active on 200', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, { ok: true, active: 'alice' }))
    const r = await selectKiwiProfile('alice')
    const [url, init] = fetchMock.mock.calls[0]
    expect(url).toBe('/api/kiwi/profiles/select')
    expect((init as RequestInit).method).toBe('POST')
    expect(JSON.parse((init as RequestInit).body as string)).toEqual({ name: 'alice' })
    expect(r).toEqual({ ok: true, active: 'alice' })
  })

  it('selectKiwiProfile surfaces backend error on 404 without throwing', async () => {
    fetchMock.mockResolvedValue(jsonRes(404, { ok: false, error: 'Unknown profile `ghost`' }))
    const r = await selectKiwiProfile('ghost')
    expect(r.ok).toBe(false)
    expect(r.error).toBe('Unknown profile `ghost`')
  })

  it('createKiwiProfile returns terminalCommand on success', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, {
      ok: true, name: 'alice', dir: '/tmp/kiwi-profiles/alice',
      terminalCommand: 'env -u ANTHROPIC_API_KEY CLAUDE_CONFIG_DIR=/tmp/kiwi-profiles/alice /bin/bash -i',
    }))
    const r = await createKiwiProfile('alice')
    expect(r.ok).toBe(true)
    expect(r.terminalCommand).toContain('CLAUDE_CONFIG_DIR=/tmp/kiwi-profiles/alice')
  })

  it('retireKiwiProfile POSTs and surfaces snappedToDefault', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, { ok: true, name: 'alice',
                                                active: 'default', snappedToDefault: true }))
    const r = await retireKiwiProfile('alice')
    const [url, init] = fetchMock.mock.calls[0]
    expect(url).toBe('/api/kiwi/profiles/retire')
    expect((init as RequestInit).method).toBe('POST')
    expect(JSON.parse((init as RequestInit).body as string)).toEqual({ name: 'alice' })
    expect(r).toEqual({ ok: true, name: 'alice', active: 'default',
                        snappedToDefault: true, alreadyRetired: undefined })
  })

  it('retireKiwiProfile surfaces backend error without throwing', async () => {
    fetchMock.mockResolvedValue(jsonRes(400, { ok: false,
                                                error: '`default` can\'t be retired' }))
    const r = await retireKiwiProfile('default')
    expect(r.ok).toBe(false)
    expect(r.error).toMatch(/can't be retired/)
  })

  it('createKiwiProfile surfaces the 409 duplicate error', async () => {
    fetchMock.mockResolvedValue(jsonRes(409, { ok: false, error: 'Profile `alice` already exists.' }))
    const r = await createKiwiProfile('alice')
    expect(r.ok).toBe(false)
    expect(r.error).toMatch(/already exists/)
  })

  it('fetchKiwiTerminalCommand omits the query when no profile is given', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, { command: 'env -u X /bin/bash -i',
                                                profile: 'default', profileDir: '' }))
    const r = await fetchKiwiTerminalCommand()
    expect(fetchMock).toHaveBeenCalledWith('/api/kiwi/terminal/command')
    expect(r?.profile).toBe('default')
  })

  it('fetchKiwiTerminalCommand url-encodes the profile query', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, { command: 'env … alice /bin/bash -i',
                                                profile: 'alice', profileDir: '/tmp/x' }))
    await fetchKiwiTerminalCommand('alice')
    expect(fetchMock).toHaveBeenCalledWith('/api/kiwi/terminal/command?profile=alice')
  })

  it('fetchKiwiTerminalCommand returns null on non-2xx', async () => {
    fetchMock.mockResolvedValue(jsonRes(400, { error: 'bad name' }))
    const r = await fetchKiwiTerminalCommand('Bad Name')
    expect(r).toBeNull()
  })
})
