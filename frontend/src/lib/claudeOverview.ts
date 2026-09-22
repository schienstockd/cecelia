// Content model for the "What Kiwi does here" overview (ClaudeOverviewDialog). Kept as data (not
// inline template) so it's testable and edited in one place. The dialog renders these as two
// entry-point cards + a four-cell capability grid + a few example prompts — a clean, brief how-to,
// not a wall of text. Mirrors the observer's real capabilities (docs/ai-assist/OBSERVER.md); keep
// it honest — if a tool lands or a limit changes, update here.
//
// FRAMING: Kiwi is a **documentation helper**, not a discussion helper. Its job is a faithful
// record of what was observed and what was tried — dead ends included — not to be right about the
// interpretation. Replies from Claude are provisional; the record is the product. See
// `docs/archive/kiwi-purpose-and-framing.md` for the full framing — its ARCHIVED banner also
// covers the deliberate 2026-09 drift toward *curating and warning* (bad-tagged repeat-failure
// digest at ≥3 hits, `bad`>`good`>untagged ranking); the "Suggests" line about recurring
// failures below is how that surfaces to the user.

export interface EntryPoint {
  name: string
  icon: string      // PrimeIcons class, e.g. 'pi-sparkles'
  what: string      // one line
  steps: string[]   // 2-3 short how-to steps
}

/** A capability line. `needs` marks one that depends on an ACCOUNT-managed MCP connector: if the user
 *  hid that connector in Settings → MCP connections, listing it here would advertise something they
 *  have switched off. (The observer's own guidance handles this differently, because it cannot see a
 *  browser setting: it makes the LabArchives direction conditional on the session actually HAVING the
 *  connector — see mcp/cecelia_mcp/guidance.py.) */
export type CapabilityItem = string | { text: string; needs: string }

export interface CapabilityGroup {
  key: 'sees' | 'suggests' | 'creates' | 'cant'
  title: string
  icon: string
  tone: 'neutral' | 'good' | 'muted'   // 'muted' for the "Can't" group
  items: CapabilityItem[]
}

/** The groups with connector-dependent lines resolved to plain strings — hidden connectors dropped,
 *  and a group left empty by that is dropped whole rather than rendering an empty box. */
export function claudeCapabilities(hiddenConnectors: string[] = []): {
  key: string; title: string; icon: string; tone: string; items: string[] }[] {
  const hidden = new Set(hiddenConnectors)
  return CLAUDE_CAPABILITIES
    .map(g => ({ ...g, items: g.items
      .filter(i => typeof i === 'string' || !hidden.has(i.needs))
      .map(i => (typeof i === 'string' ? i : i.text)) }))
    .filter(g => g.items.length > 0)
}

// The two ways in — both live in the lab-log toolbar next to this dialog's trigger.
// Framing: these describe the RECORD Kiwi leaves behind, not the answer it gives.
export const CLAUDE_ENTRY_POINTS: EntryPoint[] = [
  {
    name: 'Ask Claude',
    icon: 'pi-sparkles',
    what: 'Reads your marks + activity, drops a provisional lab-log note.',
    steps: [
      'Mark what looks off',
      'Click Ask Claude',
      'Finding lands as a [Claude] lab-log entry — provisional',
    ],
  },
  {
    name: 'Chat to Claude',
    icon: 'pi-comments',
    what: 'A back-and-forth in Claude Code — captures + notes accrue as you go.',
    steps: [
      'Run claude in a terminal',
      'Ask it to check your project',
      'Trail lands as captures, chains, blackboard',
    ],
  },
]

export const CLAUDE_CAPABILITIES: CapabilityGroup[] = [
  {
    key: 'sees', title: 'Sees', icon: 'pi-eye', tone: 'neutral',
    items: [
      'Your marks — frame, plot, landscape tile',
      'Chained captures (dead ends included)',
      'Blackboard entries + good/bad tags',
      'Analysis lineage, populations, gates, measures, HMM, QC',
      'Task history + lab log',
      { text: 'Your LabArchives experiment summary, once linked', needs: 'LabArchives' },
    ],
  },
  {
    key: 'suggests', title: 'Suggests', icon: 'pi-lightbulb', tone: 'neutral',
    items: [
      'Prior captures on this cell, track or plot',
      'Images that may need a parameter tweak — and which knob',
      'A plot or board for a pattern it spotted',
      'A heads-up when the same bad-tagged failure has recurred',
    ],
  },
  {
    key: 'creates', title: 'Creates', icon: 'pi-file', tone: 'good',
    items: [
      'Captures — chained one-line notes on your marks',
      'Blackboard entries — good/bad + required why',
      'Analysis boards, chains, Pluto notebooks — you review, then run',
      'CSV exports for Prism / R',
      { text: 'An experiment summary pulled from your LabArchives notebook', needs: 'LabArchives' },
    ],
  },
  {
    key: 'cant', title: "Can't", icon: 'pi-ban', tone: 'muted',
    items: [
      'Change your data (h5ad, gates, config)',
      'Run anything — not a task, not a chain it built',
      'Overwrite, rename or delete your chains, notebooks, boards or blackboard entries',
      'Verify the biology — replies are provisional, not domain-checked',
    ],
  },
]

// ── Terminal hand-off ────────────────────────────────────────────────────────────────────────────
// "Ask Claude" needs NO setup (Cecelia passes `--mcp-config` to the agent it spawns). A session the
// USER starts in their own terminal does — so the dialog offers ONE BUTTON that registers the server
// in their Claude Code config (POST /api/observer/register), after which plain `claude` has the tools.
// Nothing to copy and no path to mistype: a pasted half-command is exactly how this breaks for someone
// who doesn't read shell errors. The `--mcp-config` line below is only a fallback, shown when
// registration fails — and only ever with a REAL resolved path (never a placeholder).

/** Fallback one-liner for a single session. Empty string when the path isn't known yet — callers must
 *  render nothing rather than a placeholder a user could copy verbatim and have fail. */
export function claudeChatCommand(mcpConfigPath: string): string {
  return mcpConfigPath ? `claude --mcp-config ${mcpConfigPath}` : ''
}

/** The button's own label + the states around it. Short, imperative (docs/UI.md house style). */
export const CLAUDE_TERMINAL = {
  note: 'Ask Claude needs no setup. To chat in your own terminal, set it up once:',
  action: 'Set up my terminal',
  resync: 'Fix terminal setup',
  busy: 'Setting up…',
  done: 'Terminal ready — run claude, then use Chat to Claude',
  staleWhy: 'Your registered server points elsewhere (moved install or a different port)',
  failedPrefix: 'Setup failed. Start Claude Code with this instead:',
} as const

// Copyable one-liners — documentation prompts first (the shape Kiwi's built for), then a couple of
// generative ones (board / notebook / chain). Short and uniform so they read as a clean row of chips.
export const CLAUDE_EXAMPLES: string[] = [
  'Circle this — note what looks off',
  'Trace prior captures on this cell',
  'Add a behaviour board',
  'Cell-speed notebook',
  'Segment + track chain',
]
