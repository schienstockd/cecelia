import { describe, it, expect } from 'vitest'
import { isAuthError, observerSetupReason } from './observerSetup'

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
