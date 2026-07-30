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

// ── Which terminal button the lab-log toolbar shows ───────────────────────────────────────────────
// One button in one slot, so the setup step isn't hidden in the info dialog: until the user's own
// terminal is set up they get "Set up my terminal"; after that, "Chat to Claude".
export type TerminalCta = 'setup' | 'resync' | 'chat'

/**
 * `available` = the `claude` CLI is on PATH. `state` = the backend's registration reading
 * (missing/stale/shadowed/current, `terminal.state` on /api/observer/status).
 *
 * - No CLI → 'chat'. The starter prompt works with ANY MCP assistant, so we must not hide it behind a
 *   Claude-specific registration the user may not want.
 * - 'stale' → 'resync', not 'chat': the entry points at another interpreter/port and would fail
 *   silently in their session.
 * - 'shadowed' → 'resync' too: our entry is correct, but a per-folder (`local`-scope) one overrides it,
 *   so the user's terminal still has no tools. This is the case that looked like a broken button.
 */
export function terminalCta(available: boolean, state?: string): TerminalCta {
  if (!available) return 'chat'
  if (state === 'stale' || state === 'shadowed') return 'resync'
  return state === 'current' ? 'chat' : 'setup'
}

/** Tooltip for that button — one line per state; the reasoning lives in docs/ai-assist/OBSERVER.md. */
export function terminalSetupTooltip(state?: string): string {
  if (state === 'shadowed') return 'Another folder\'s entry overrides Cecelia\'s — clear it'
  if (state === 'stale') return 'Your terminal\'s cecelia-observer points somewhere else — re-register it'
  return 'Register cecelia-observer in Claude Code so you can chat in a terminal'
}
