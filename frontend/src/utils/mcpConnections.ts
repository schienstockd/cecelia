// Row model for the Settings → "MCP connections" panel. Pure + unit-tested (mcpConnections.test.ts);
// the component only renders what this returns. See docs/ai-assist/OBSERVER.md.
//
// There are TWO kinds of connector and they are not comparable, which is the whole reason this file
// exists rather than a `v-for` over one list:
//
//   • machine  — registered in the user's Claude config on THIS machine (`~/.claude.json`). The
//                backend reads it (GET /api/mcp/connections), so the state is real and a dot means
//                something. Cecelia's own observer is one of these.
//   • account  — managed by the user's claude.ai ACCOUNT (LabArchives today: it authenticates through
//                `/mcp` and never touches the local config). We CANNOT see whether it is connected.
//   • cli      — Claude Code itself: on PATH, and logged in? Not an MCP server, but the precondition
//                for every machine row, and the state users most often have to act on. It used to be
//                a banner in the lab-log panel; it belongs with the rest of "what Claude can reach".
//
// An account connector therefore never gets a green/red dot. Showing one would be a lie in both
// directions — red for everyone who is connected, green for nobody. It is listed anyway, so people
// discover that Cecelia can use it at all; setting it up is theirs to do (it is institution-hosted,
// so there is no URL we could offer).
import type { Tone } from './serviceStatus'
import { observerSetupReason } from './observerSetup'

export type McpKind = 'machine' | 'account' | 'cli'

export interface McpConnection {          // one row from GET /api/mcp/connections
  name: string
  scope?: string                          // 'user' | 'local'
  dir?: string
  transport?: string
  ours?: boolean
  installPath?: string                    // spec.env.PYTHONPATH — the checkout the entry points at.
                                          // Only meaningful for the observer; empty for others.
}

export interface McpRow {
  name: string
  kind: McpKind
  tone: Tone
  label: string                           // the pill text
  detail: string                          // ONE short phrase, ~4 words — the row is a status line,
                                          // not an explanation (docs/ui/COPY.md)
  hint: string                            // the why/how, as a tooltip — keeps the row on one line
  scope: string
  dismissable: boolean                    // account rows only: plenty of sites have no LabArchives
  href?: string                           // an external help link (the CLI row's setup guide)
  installPath?: string                    // observer only — shown so an "out of date" row names the
                                          // install it points at without needing a hover
}

// Account-managed connectors we know Cecelia can USE. A registry, not a special case — the next one
// is a line here. `detail` says where setup happens, because it is never in Cecelia.
export const ACCOUNT_CONNECTORS: { name: string; label: string; detail: string; hint: string }[] = [
  {
    name: 'LabArchives',
    label: 'account',
    detail: 'Connect in Claude (/mcp)',
    hint: 'Institution-hosted — ask your IT service desk for the URL. Cecelia cannot see whether it is connected.',
  },
]

// Cecelia's own observer: the one machine row whose state we both know AND care about.
// 'current' → ok; 'missing'/'stale'/'shadowed' → warn (a stale entry fails SILENTLY in the user's
// terminal, which is the worst outcome, so it must not read as fine).
// `detail` stays a phrase because the Set-up button IS the instruction; the why goes in `hint`.
export function observerRowState(state?: string): { tone: Tone; label: string; detail: string; hint: string } {
  switch (state) {
    case 'current':  return { tone: 'ok', label: 'connected', detail: '', hint: 'Claude in your terminal has the Cecelia tools' }
    case 'stale':    return { tone: 'warn', label: 'out of date', detail: 'Points at another install',
                              hint: 'A stale entry fails silently in your terminal — re-run setup' }
    case 'shadowed': return { tone: 'warn', label: 'shadowed', detail: 'A per-folder entry overrides it',
                              hint: 'A local-scope entry takes precedence over the user-scope one — re-run setup' }
    case 'missing':  return { tone: 'warn', label: 'not set up', detail: 'Not in your Claude config',
                              hint: 'Register it to use Claude in your own terminal' }
    default:         return { tone: 'idle', label: 'unknown', detail: '', hint: '' }
  }
}

export const CLAUDE_SETUP_GUIDE = 'https://docs.anthropic.com/en/docs/claude-code/setup'

/**
 * The Claude Code CLI row. `available` = on PATH; `authFailed` = a run failed with an auth error.
 *
 * The not-installed-beats-not-logged-in precedence is `observerSetupReason`'s, not a second copy of
 * it — that helper was the lab-log banner's brain, and when the banner moved here it would otherwise
 * have been left as dead code beside a re-derivation of the same rule.
 */
export function cliRow(available: boolean, authFailed = false): McpRow {
  const base = { name: 'Claude Code', kind: 'cli' as const, scope: 'this machine', dismissable: false }
  switch (observerSetupReason(available, authFailed)) {
    case 'missing':
      return { ...base, tone: 'warn', label: 'not detected', detail: 'Install it, then run claude once',
               hint: 'Everything below needs the Claude Code CLI on your PATH', href: CLAUDE_SETUP_GUIDE }
    case 'auth':
      return { ...base, tone: 'warn', label: 'not logged in', detail: 'Run claude in a terminal',
               hint: 'The CLI is installed but a run failed to authenticate', href: CLAUDE_SETUP_GUIDE }
    default:
      return { ...base, tone: 'ok', label: 'ready', detail: '', hint: 'Claude Code is installed and logged in' }
  }
}

/**
 * Build the panel's rows: the CLI, then every machine connection the backend found, then the account
 * connectors. `observerState` is the observer's registration state (from the observer store).
 * `hiddenAccounts` are account connectors the user dismissed — they stay out entirely.
 */
export function mcpRows(
  connections: McpConnection[] | null | undefined,
  observerState?: string,
  hiddenAccounts: string[] = [],
  cli?: { available: boolean; authFailed?: boolean },
): McpRow[] {
  const machine: McpRow[] = (connections ?? []).map(c => {
    const scope = c.scope === 'local' ? `local · ${c.dir || ''}`.trim() : 'user'
    if (c.ours) {
      const s = observerRowState(observerState)
      return { name: c.name, kind: 'machine', tone: s.tone, label: s.label, detail: s.detail,
               hint: s.hint, scope, dismissable: false, installPath: c.installPath || '' }
    }
    // Someone else's server: registered is all we can honestly claim — we don't health-check it
    // (that would spawn every server just to draw a dot).
    return { name: c.name, kind: 'machine', tone: 'ok', label: 'registered',
             detail: c.transport ? `${c.transport} transport` : '',
             hint: 'Registered in your Claude config — Cecelia does not health-check it',
             scope, dismissable: false }
  })

  // The observer may be absent from the config entirely — it still deserves a row saying so, rather
  // than silently vanishing exactly when it needs attention.
  if (!machine.some(r => r.name === 'cecelia-observer')) {
    const s = observerRowState(observerState || 'missing')
    machine.unshift({ name: 'cecelia-observer', kind: 'machine', tone: s.tone, label: s.label,
                      detail: s.detail, hint: s.hint, scope: 'user', dismissable: false,
                      installPath: '' })
  }

  const hidden = new Set(hiddenAccounts)
  const account: McpRow[] = ACCOUNT_CONNECTORS
    .filter(a => !hidden.has(a.name))
    .map(a => ({ name: a.name, kind: 'account' as const, tone: 'idle' as Tone, label: a.label,
                 detail: a.detail, hint: a.hint, scope: 'account', dismissable: true }))

  const head = cli ? [cliRow(cli.available, cli.authFailed)] : []
  return [...head, ...machine, ...account]
}
