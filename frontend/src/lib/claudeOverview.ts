// Content model for the "What Kiwi does here" overview (ClaudeOverviewDialog). Kept as data (not
// inline template) so it's testable and edited in one place. The dialog renders the loop (how Claude
// fits the work) and one Kiwi-vs-terminal table (what each way in can do) — nothing else: what Claude
// can read in detail is the tools' business, not the user's. Keep it honest — if a tool lands or a
// limit changes, update here.
//
// FRAMING: Kiwi is a **documentation helper**, not a discussion helper. Its job is a faithful
// record of what was observed and what was tried — dead ends included — not to be right about the
// interpretation. Replies from Claude are provisional; the record is the product. See
// `docs/archive/kiwi-purpose-and-framing.md` for the full framing — its ARCHIVED banner also
// covers the deliberate 2026-09 drift toward *curating and warning* (bad-tagged repeat-failure
// digest at ≥3 hits, `bad`>`good`>untagged ranking); the "Warns when a bad-tagged failure keeps
// coming back" row below is how that surfaces to the user.

// ── The loop: how Claude fits the work ───────────────────────────────────────────────────────────
// THE wording of the four steps. Kiwi's `?` dialog renders it, the "Assist with Kiwi" guide uses it for
// its step titles and text, and user-docs/README.md → *Working with Claude* says the same thing — so a
// user meets one loop, not three. Change it here and the guide follows (guides.json regenerates).
export interface LoopStep {
  title: string
  text: string      // one line
}
export const CLAUDE_LOOP: LoopStep[] = [
  { title: 'Point at it', text: '@ Add to Kiwi on what puzzles you — or Share a frame and draw on it' },
  { title: 'Ask Kiwi', text: 'It points and counts, it doesn’t judge — click a claim’s ref to check it' },
  { title: 'Keep it', text: 'Save the reply, then tag it good or bad on the Blackboard' },
  { title: 'Go further', text: 'Continue in terminal when one question isn’t enough' },
]

// ── Kiwi vs a terminal session ───────────────────────────────────────────────────────────────────
// What each way in can do. `needs` marks a row that depends on an ACCOUNT-managed MCP connector: if
// the user hid that connector in Settings → MCP connections, the row would advertise something they
// switched off. (The observer's own guidance handles this differently, because it cannot see a browser
// setting: it makes the LabArchives direction conditional on the session actually HAVING the
// connector — see mcp/cecelia_mcp/guidance.py.) Mirrors the real tool lists: Kiwi's is
// `KIWI_READ_TOOLS` (api/src/kiwi_turn.jl), a terminal's is the observer's (docs/ai-assist/OBSERVER.md).
export interface AccessRow {
  text: string
  kiwi: boolean
  terminal: boolean
  needs?: string
}
export const CLAUDE_ACCESS: AccessRow[] = [
  { text: 'Reads your project, captures, Blackboard and lab log', kiwi: true, terminal: true },
  { text: 'Warns when a bad-tagged failure keeps coming back', kiwi: true, terminal: true },
  { text: 'Marks cells and tracks on the viewer', kiwi: false, terminal: true },
  { text: 'Adds a chain, board, notebook or lab-log line for you to review', kiwi: false, terminal: true },
  { text: 'Proposes a gate (marked ✦, undoable); moves or removes only its own', kiwi: false, terminal: true },
  { text: 'Writes a custom module for a step Cecelia lacks — you reload and run it', kiwi: false, terminal: true },
  { text: 'Reads your LabArchives notebook, once linked', kiwi: false, terminal: true, needs: 'LabArchives' },
  { text: 'Runs a task or chain, or changes or deletes your data and work', kiwi: false, terminal: false },
]

/** The rows with connector-dependent ones dropped when the user hid that connector. */
export function claudeAccess(hiddenConnectors: string[] = []): AccessRow[] {
  const hidden = new Set(hiddenConnectors)
  return CLAUDE_ACCESS.filter(r => !r.needs || !hidden.has(r.needs))
}

/** Said once under the table: every reply is a claim to check, not a verdict. */
export const CLAUDE_PROVISIONAL = 'Replies are provisional — not checked against the biology'

// ── Terminal hand-off ────────────────────────────────────────────────────────────────────────────
// Kiwi needs NO setup (Cecelia passes `--mcp-config` to the agent it spawns). A session the
// USER starts in their own terminal does — so the dialog offers ONE BUTTON that registers the server
// in their Claude Code config (POST /api/observer/register), after which plain `claude` has the tools.
// Nothing to copy and no path to mistype: a pasted half-command is exactly how this breaks for someone
// who doesn't read shell errors. The `--mcp-config` line below is only a fallback, shown when
// registration fails — and only ever with a REAL resolved path (never a placeholder).

/** Fallback one-liner for a single session. Empty string when the path isn't known yet — callers must
 *  render nothing rather than a placeholder a user could copy verbatim and have fail. */
export function claudeChatCommand(mcpConfigPath: string): string {
  if (!mcpConfigPath) return ''
  // Double quotes keep a path with a space in one word in bash, zsh, PowerShell and cmd alike.
  const p = /\s/.test(mcpConfigPath) ? `"${mcpConfigPath}"` : mcpConfigPath
  return `claude --mcp-config ${p}`
}

/** The terminal-setup button's labels, as Kiwi's Terminal row and Settings → Cecelia MCP render them.
 *  Short: the button sits in a compact row; the reason for each state is `terminalSetupTooltip`
 *  (`utils/observerSetup.ts`). */
export const CLAUDE_TERMINAL = {
  action: 'Set up',
  resync: 'Fix',
  busy: 'Setting up…',
} as const

/** Kiwi's standing data notice — whatever Claude reads goes to Anthropic. Always shown, amber; why it
 *  is not dismissible: docs/ai-assist/OBSERVER.md → *The data notice*. */
export const CLAUDE_DATA_NOTICE = {
  short: 'What Claude reads is sent to Anthropic',
  detail: "Check your institute's rules for unpublished or patient data",
}
