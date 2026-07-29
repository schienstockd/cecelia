import { describe, it, expect } from 'vitest'
import { isAuthError, observerSetupReason, terminalCta } from './observerSetup'

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

describe('terminalCta — which terminal button the lab-log toolbar shows', () => {
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
  it('falls back to chat when the claude CLI is absent — the prompt suits any MCP assistant', () => {
    for (const st of ['missing', 'stale', 'current', undefined]) {
      expect(terminalCta(false, st)).toBe('chat')
    }
  })
})
