import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest'
import {
  isValidProfileName,
  fetchProfiles,
  selectProfile,
  createProfile,
  retireProfile,
  fetchTerminalCommand,
  profileNameLabel,
  fetchClaudeLogin,
  logoutClaude,
  startClaudeLogin,
  fetchClaudeLoginRun,
  sendClaudeLoginCode,
  claudeLoginPrompt,
  claudeLoginLabel,
} from './profileApi'

describe('profileNameLabel', () => {
  it('shows the default profile by its display alias, others as-is', () => {
    expect(profileNameLabel('default')).toBe('peanut')
    expect(profileNameLabel('alice')).toBe('alice')
  })
})

// The validator mirror-tests. Same cases the backend suite pins in `api/test/suite/kiwi_profile.jl` —
// keep both green together so the dialog can't accept a name the round-trip will reject.
describe('isValidProfileName', () => {
  it('accepts lower-ASCII alnum + `-` / `_`, 1..32 chars', () => {
    expect(isValidProfileName('alice')).toBe(true)
    expect(isValidProfileName('dominik')).toBe(true)
    expect(isValidProfileName('lab-user_2')).toBe(true)
    expect(isValidProfileName('a')).toBe(true)
    expect(isValidProfileName('z'.repeat(32))).toBe(true)
  })
  it('rejects empty, too-long, uppercase, punctuation, whitespace', () => {
    expect(isValidProfileName('')).toBe(false)
    expect(isValidProfileName('z'.repeat(33))).toBe(false)
    expect(isValidProfileName('Alice')).toBe(false)
    expect(isValidProfileName('has space')).toBe(false)
    expect(isValidProfileName('with/slash')).toBe(false)
    expect(isValidProfileName('with.dot')).toBe(false)
  })
  it('rejects reserved / magic names', () => {
    expect(isValidProfileName('legacy')).toBe(false)   // D10 sentinel
    expect(isValidProfileName('default')).toBe(false)  // maps to ~/.claude
    expect(isValidProfileName('peanut')).toBe(false)   // display alias for `default`
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

  it('fetchProfiles returns roster on 200', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, {
      active: 'alice',
      profiles: [{ name: 'default', dir: '', isDefault: true },
                 { name: 'alice',   dir: '/tmp/user-profiles/alice', isDefault: false }],
      legacyReserved: ['legacy'],
    }))
    const r = await fetchProfiles()
    expect(fetchMock).toHaveBeenCalledWith('/api/kiwi/profiles')
    expect(r.active).toBe('alice')
    expect(r.profiles.map(p => p.name)).toEqual(['default', 'alice'])
    expect(r.legacyReserved).toEqual(['legacy'])
  })

  it('fetchProfiles falls back to a `default`-only roster on network failure', async () => {
    fetchMock.mockRejectedValue(new Error('network'))
    const r = await fetchProfiles()
    expect(r.active).toBe('default')
    expect(r.profiles).toEqual([{ name: 'default', dir: '', isDefault: true, retired: false }])
  })

  it('selectProfile POSTs and returns ok+active on 200', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, { ok: true, active: 'alice' }))
    const r = await selectProfile('alice')
    const [url, init] = fetchMock.mock.calls[0]
    expect(url).toBe('/api/kiwi/profiles/select')
    expect((init as RequestInit).method).toBe('POST')
    expect(JSON.parse((init as RequestInit).body as string)).toEqual({ name: 'alice' })
    expect(r).toEqual({ ok: true, active: 'alice' })
  })

  it('selectProfile surfaces backend error on 404 without throwing', async () => {
    fetchMock.mockResolvedValue(jsonRes(404, { ok: false, error: 'Unknown profile `ghost`' }))
    const r = await selectProfile('ghost')
    expect(r.ok).toBe(false)
    expect(r.error).toBe('Unknown profile `ghost`')
  })

  it('createProfile returns terminalCommand on success', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, {
      ok: true, name: 'alice', dir: '/tmp/user-profiles/alice',
      terminalCommand: 'env -u ANTHROPIC_API_KEY CLAUDE_CONFIG_DIR=/tmp/user-profiles/alice /bin/bash -i',
    }))
    const r = await createProfile('alice')
    expect(r.ok).toBe(true)
    expect(r.terminalCommand).toContain('CLAUDE_CONFIG_DIR=/tmp/user-profiles/alice')
  })

  it('retireProfile POSTs and surfaces snappedToDefault', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, { ok: true, name: 'alice',
                                                active: 'default', snappedToDefault: true }))
    const r = await retireProfile('alice')
    const [url, init] = fetchMock.mock.calls[0]
    expect(url).toBe('/api/kiwi/profiles/retire')
    expect((init as RequestInit).method).toBe('POST')
    expect(JSON.parse((init as RequestInit).body as string)).toEqual({ name: 'alice' })
    expect(r).toEqual({ ok: true, name: 'alice', active: 'default',
                        snappedToDefault: true, alreadyRetired: undefined })
  })

  it('retireProfile surfaces backend error without throwing', async () => {
    fetchMock.mockResolvedValue(jsonRes(400, { ok: false,
                                                error: '`default` can\'t be retired' }))
    const r = await retireProfile('default')
    expect(r.ok).toBe(false)
    expect(r.error).toMatch(/can't be retired/)
  })

  it('createProfile surfaces the 409 duplicate error', async () => {
    fetchMock.mockResolvedValue(jsonRes(409, { ok: false, error: 'Profile `alice` already exists.' }))
    const r = await createProfile('alice')
    expect(r.ok).toBe(false)
    expect(r.error).toMatch(/already exists/)
  })

  it('fetchTerminalCommand omits the query when no profile is given', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, { command: 'env -u X /bin/bash -i',
                                                profile: 'default', profileDir: '' }))
    const r = await fetchTerminalCommand()
    expect(fetchMock).toHaveBeenCalledWith('/api/kiwi/terminal/command')
    expect(r?.profile).toBe('default')
  })

  it('fetchTerminalCommand url-encodes the profile query', async () => {
    fetchMock.mockResolvedValue(jsonRes(200, { command: 'env … alice /bin/bash -i',
                                                profile: 'alice', profileDir: '/tmp/x' }))
    await fetchTerminalCommand('alice')
    expect(fetchMock).toHaveBeenCalledWith('/api/kiwi/terminal/command?profile=alice')
  })

  it('fetchTerminalCommand returns null on non-2xx', async () => {
    fetchMock.mockResolvedValue(jsonRes(400, { error: 'bad name' }))
    const r = await fetchTerminalCommand('Bad Name')
    expect(r).toBeNull()
  })
})

describe('Claude login per profile', () => {
  const originalFetch = globalThis.fetch
  let fetchMock: ReturnType<typeof vi.fn>
  beforeEach(() => {
    fetchMock = vi.fn()
    globalThis.fetch = fetchMock as unknown as typeof fetch
  })
  afterEach(() => { globalThis.fetch = originalFetch })
  const res = (status: number, body: object) =>
    ({ ok: status >= 200 && status < 300, status, json: async () => body }) as unknown as Response

  it('fetchClaudeLogin passes profile + fresh and returns the server shape', async () => {
    fetchMock.mockResolvedValue(res(200, { profile: 'alice', state: 'loggedIn', email: 'a@lab.org',
                                           method: 'claude.ai', org: 'Lab' }))
    const l = await fetchClaudeLogin('alice', true)
    expect(fetchMock).toHaveBeenCalledWith('/api/kiwi/profiles/auth?profile=alice&fresh=1')
    expect(l.state).toBe('loggedIn')
    expect(l.email).toBe('a@lab.org')
  })

  it('fetchClaudeLogin is `unknown` on an error status or network failure — never "logged out"', async () => {
    fetchMock.mockResolvedValue(res(404, { ok: false }))
    expect((await fetchClaudeLogin('ghost')).state).toBe('unknown')
    fetchMock.mockRejectedValue(new Error('network'))
    expect((await fetchClaudeLogin()).state).toBe('unknown')
  })

  it('logoutClaude surfaces the server error', async () => {
    fetchMock.mockResolvedValue(res(500, { ok: false, error: 'No assistant CLI found.' }))
    expect(await logoutClaude('alice')).toEqual({ ok: false, error: 'No assistant CLI found.' })
    fetchMock.mockResolvedValue(res(200, { ok: true, name: 'alice' }))
    expect(await logoutClaude('alice')).toEqual({ ok: true })
  })

  it('claudeLoginLabel: email, not-logged-in, or nothing when unknown', () => {
    const base = { profile: 'a', method: '', org: '' }
    expect(claudeLoginLabel({ ...base, state: 'loggedIn', email: 'a@lab.org' })).toBe('a@lab.org')
    expect(claudeLoginLabel({ ...base, state: 'loggedIn', email: '' })).toBe('Logged in')
    expect(claudeLoginLabel({ ...base, state: 'loggedOut', email: '' })).toBe('Not logged in')
    expect(claudeLoginLabel({ ...base, state: 'unknown', email: '' })).toBe('')
    expect(claudeLoginLabel(null)).toBe('')
  })
})

describe('Logging in from Cecelia (P9)', () => {
  const originalFetch = globalThis.fetch
  let fetchMock: ReturnType<typeof vi.fn>
  beforeEach(() => {
    fetchMock = vi.fn()
    globalThis.fetch = fetchMock as unknown as typeof fetch
  })
  afterEach(() => { globalThis.fetch = originalFetch })
  const res = (status: number, body: object) =>
    ({ ok: status >= 200 && status < 300, status, json: async () => body }) as unknown as Response
  const run = { state: 'waiting', url: 'https://claude.com/x', browser: 'Firefox', message: '' }

  it('startClaudeLogin POSTs the name and returns the run; a refusal is a failed run', async () => {
    fetchMock.mockResolvedValue(res(200, { ok: true, ...run }))
    const r = await startClaudeLogin('alice')
    expect(fetchMock).toHaveBeenCalledWith('/api/kiwi/profiles/login',
      expect.objectContaining({ method: 'POST', body: JSON.stringify({ name: 'alice' }) }))
    expect(r.state).toBe('waiting')
    fetchMock.mockResolvedValue(res(500, { ok: false, error: 'No assistant CLI found.' }))
    expect(await startClaudeLogin('alice')).toMatchObject({ state: 'failed', error: 'No assistant CLI found.' })
  })

  it('fetchClaudeLoginRun: a dropped poll is `none` + error, not a failed login', async () => {
    fetchMock.mockResolvedValue(res(200, { ok: true, ...run }))
    expect((await fetchClaudeLoginRun('alice')).browser).toBe('Firefox')
    expect(fetchMock).toHaveBeenCalledWith('/api/kiwi/profiles/login?profile=alice')
    fetchMock.mockRejectedValue(new Error('network'))
    expect(await fetchClaudeLoginRun('alice')).toMatchObject({ state: 'none', error: 'network' })
  })

  it('sendClaudeLoginCode surfaces the server error', async () => {
    fetchMock.mockResolvedValue(res(409, { ok: false, error: 'No login is waiting for a code.' }))
    expect(await sendClaudeLoginCode('alice', 'c#s')).toEqual({ ok: false, error: 'No login is waiting for a code.' })
    fetchMock.mockResolvedValue(res(200, { ok: true }))
    expect(await sendClaudeLoginCode('alice', 'c#s')).toEqual({ ok: true })
  })

  it('claudeLoginPrompt: names the private window, else points at the link', () => {
    const w = { state: 'waiting' as const, url: '', browser: '', message: '' }
    expect(claudeLoginPrompt(null)).toBe('Starting…')
    expect(claudeLoginPrompt({ ...w })).toBe('Starting…')              // URL not printed yet
    expect(claudeLoginPrompt({ ...w, browser: 'Brave' })).toBe('Sign in in the private Brave window')
    expect(claudeLoginPrompt({ ...w, url: 'https://x' })).toBe('Open the link in a private window and sign in')
    expect(claudeLoginPrompt({ ...w, state: 'failed', message: 'Login failed: 400' })).toBe('Login failed: 400')
    expect(claudeLoginPrompt({ ...w, state: 'failed' })).toBe('Login did not finish')
  })
})
