// Setup-hint helpers for the in-app observer. Availability only tells us the `claude` CLI is on PATH
// — NOT that it's authenticated. So a run can fail purely because Claude Code was never logged in;
// this classifies that so the panel can show "connect Claude Code" guidance instead of a raw error.
// Kept out of the SFC so it's unit-testable.

// Auth/login-shaped failure text from `claude -p` (installed but not connected). Deliberately broad —
// it only swaps the hint wording, so a false positive is cheap.
const AUTH_ERROR_RE = /log ?in|logged in|authenticat|unauthor|credential|api key|not authenticated|please run/i

/** Does this failure message look like Claude Code isn't logged in (vs a real task/tool error)? */
export function isAuthError(msg?: string | null): boolean {
  return !!msg && AUTH_ERROR_RE.test(msg)
}

export type ObserverSetupReason = 'missing' | 'auth' | null

/**
 * What setup guidance (if any) to show. `available` = CLI on PATH; `lastFailedAuth` = the most recent
 * observer pass failed with an auth-shaped error. Returns 'missing' (install), 'auth' (log in), or
 * null (all good).
 */
export function observerSetupReason(available: boolean, lastFailedAuth: boolean): ObserverSetupReason {
  if (!available) return 'missing'
  if (lastFailedAuth) return 'auth'
  return null
}
