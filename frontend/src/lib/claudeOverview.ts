// Content model for the "What can Claude do here?" overview (ClaudeOverviewDialog). Kept as data
// (not inline template) so it's testable and edited in one place. The dialog renders these as two
// entry-point cards + a four-cell capability grid + a few example prompts — a clean, brief how-to,
// not a wall of text. Mirrors the observer's real capabilities (docs/ai-assist/OBSERVER.md); keep it
// honest — if a tool lands or a limit changes, update here.

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
export const CLAUDE_ENTRY_POINTS: EntryPoint[] = [
  {
    name: 'Ask Claude',
    icon: 'pi-sparkles',
    what: 'A one-shot QC pass over what just ran — it flags anything off, in the lab log.',
    steps: [
      'Click Ask Claude',
      'It reads recent activity + cohort QC',
      'Findings land as [Claude] lab-log entries',
    ],
  },
  {
    name: 'Chat to Claude',
    icon: 'pi-comments',
    what: 'A full back-and-forth session about this project in Claude Code (or any MCP assistant).',
    steps: [
      'Run claude in a terminal',
      'Ask it to check your project in Cecelia',
      'It gets oriented, then asks what you want to do',
    ],
  },
]

export const CLAUDE_CAPABILITIES: CapabilityGroup[] = [
  {
    key: 'sees', title: 'Sees', icon: 'pi-eye', tone: 'neutral',
    items: [
      'Analysis lineage — what ran, in what order',
      'Populations, gates & measures (speed, intensity, morphology)',
      'HMM states, clusters, QC flags & cohort outliers',
      'Task + parameter history and the lab log',
      { text: 'Your LabArchives experiment summary, once linked', needs: 'LabArchives' },
    ],
  },
  {
    key: 'suggests', title: 'Suggests', icon: 'pi-lightbulb', tone: 'neutral',
    items: [
      'Which images may need a parameter tweak — and which knob',
      'How to visualise a pattern it spotted',
      'A plot to add to the analysis board',
    ],
  },
  {
    key: 'creates', title: 'Creates', icon: 'pi-file-edit', tone: 'good',
    items: [
      'Analysis boards — a page of plots, added beside your own',
      'Chains — a wired pipeline you review, then run',
      'Pluto notebooks — runnable analysis you then own & edit',
      'CSV exports for Prism / R',
      'Lab-log notes (only when you ask)',
      { text: 'An experiment summary pulled from your LabArchives notebook', needs: 'LabArchives' },
    ],
  },
  {
    key: 'cant', title: "Can't", icon: 'pi-ban', tone: 'muted',
    items: [
      'Change your data (h5ad, gates, project config)',
      'Run anything — not a task, not a chain it built',
      'Overwrite, rename or delete your chains, notebooks & boards',
      'Open raw image pixels',
      'Draw the biological conclusion — that stays yours',
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

// Copyable one-liners that show the range — from QC to a notebook.
// Keep them short and uniform so they read as a clean row of chips.
export const CLAUDE_EXAMPLES: string[] = [
  'Why is this image off?',
  'Add a board of behaviour plots',
  'Build a cell-speed notebook',
  'Design a segment + track chain',
]
