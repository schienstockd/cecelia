import { describe, it, expect } from 'vitest'
import { isAuthError, observerSetupReason, pluginCardAction, terminalCta, terminalSetupTooltip } from './observerSetup'

describe('isAuthError', () => {
  it('flags login/auth-shaped failures', () => {
    for (const m of [
      'Please run /login to authenticate',
      'Not authenticated',
      'Invalid API key',
      'unauthorized',
      'You are not logged in',
      'missing credentials',
    ]) expect(isAuthError(m)).toBe(true)
  })
  it('does not flag ordinary tool/task errors', () => {
    for (const m of ['agent exited 1', 'tool failed: no such image', 'timeout', '', null, undefined]) {
      expect(isAuthError(m as any)).toBe(false)
    }
  })
})

describe('observerSetupReason', () => {
  it('missing when the CLI is not available', () => {
    expect(observerSetupReason(false, false)).toBe('missing')
    expect(observerSetupReason(false, true)).toBe('missing')   // not-installed wins
  })
  it('auth when available but the last pass failed auth', () => {
    expect(observerSetupReason(true, true)).toBe('auth')
  })
  it('null when available and no auth failure', () => {
    expect(observerSetupReason(true, false)).toBe(null)
  })
})

describe('terminalCta — which terminal button Kiwi\'s Terminal row shows', () => {
  it('offers setup until the terminal is registered', () => {
    expect(terminalCta(true, 'missing')).toBe('setup')
    expect(terminalCta(true, undefined)).toBe('setup')   // status not read yet → assume not set up
  })
  it('offers chat once registered and current', () => {
    expect(terminalCta(true, 'current')).toBe('chat')
  })
  it('treats a stale registration as needing a re-sync, not as ready', () => {
    // a stale entry (other checkout's python / different port) fails SILENTLY in the user's session,
    // so showing Chat here would be the worst of the three outcomes
    expect(terminalCta(true, 'stale')).toBe('resync')
  })
  it('treats a shadowed registration as needing a re-sync, not as ready', () => {
    // ours is registered correctly, but a per-folder (`local`-scope) entry overrides it — so the user's
    // terminal has no tools while the app claims setup is done. This is what read as "button broken".
    expect(terminalCta(true, 'shadowed')).toBe('resync')
  })
  it('falls back to chat when the claude CLI is absent — the prompt suits any MCP assistant', () => {
    for (const st of ['missing', 'stale', 'shadowed', 'current', undefined]) {
      expect(terminalCta(false, st)).toBe('chat')
    }
  })
})

describe('terminalSetupTooltip', () => {
  it('names the actual blocker per state', () => {
    expect(terminalSetupTooltip('shadowed')).toMatch(/overrides/)
    expect(terminalSetupTooltip('stale')).toMatch(/out of date/)
    expect(terminalSetupTooltip('missing')).toMatch(/^Install the Cecelia plugin/)
    expect(terminalSetupTooltip(undefined)).toMatch(/^Install the Cecelia plugin/)
  })
  it('stays one short line — a tooltip, not an explanation', () => {
    for (const st of ['missing', 'stale', 'shadowed', undefined]) {
      const t = terminalSetupTooltip(st)
      expect(t.length).toBeLessThanOrEqual(80)
      expect(t).not.toMatch(/\./)      // no sentence breaks
    }
  })
})

describe('pluginCardAction — what the Cecelia-plugin card in Preferences offers', () => {
  it('points at the Claude Code install page when the CLI is missing, whatever the config says', () => {
    // Set up and the --mcp-config fallback both run `claude`; without it they can only fail.
    for (const st of ['missing', 'stale', 'shadowed', 'current', undefined]) {
      expect(pluginCardAction(false, st)).toBe('install-cli')
    }
  })
  it('offers Set up when the CLI is there but the plugin is not', () => {
    expect(pluginCardAction(true, 'missing')).toBe('setup')
  })
  it('offers Fix for an out-of-date or overridden entry', () => {
    expect(pluginCardAction(true, 'stale')).toBe('resync')
    expect(pluginCardAction(true, 'shadowed')).toBe('resync')
  })
  it('offers nothing once connected, or before the config has been read', () => {
    expect(pluginCardAction(true, 'current')).toBeNull()
    expect(pluginCardAction(true, undefined)).toBeNull()
  })
})
