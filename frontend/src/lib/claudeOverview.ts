// Content model for Kiwi's `?` dialog (ClaudeOverviewDialog) and the Guides map's AI help: the two
// modes, Kiwi and Claude Code. Kept as data (not inline template) so it's testable and edited in one
// place. The per-tool "What each can use" list is lib/claudeTools.ts.
//
// Kiwi and a Claude Code session are two ways in, NOT two steps: either can come first, and some
// people go straight to Claude Code. Kiwi is the structured rubber duck (points at the evidence, you
// decide); Claude Code is the interactive route. The Blackboard links them (Continue in terminal).
// Why, and what it replaced: WORKFLOW_RECIPES_PLAN.md → *Two ways in*. The "Assist with Kiwi" guide
// uses KIWI_LOOP for its step titles and text, and user-docs/README.md → *Working with Claude* says
// the same thing. Change it here and the guide follows (guides.json regenerates).
//
// FRAMING: Kiwi's replies are provisional; the record is the product — see
// `docs/archive/kiwi-purpose-and-framing.md`, whose banner also covers the drift toward *curating and
// warning* (bad-tagged repeat-failure digest). That warning is a line in claudeTools.ts.
export interface LoopStep {
  title: string
  text: string      // one line
}
// Kiwi's own steps — what the "Assist with Kiwi" guide walks, one per control.
export const KIWI_LOOP: LoopStep[] = [
  { title: 'Point at it', text: '@ Add to Kiwi on what puzzles you — or Share a frame and draw on it' },
  { title: 'Ask Kiwi', text: 'It points and counts, it doesn’t judge — click a claim’s ref to check it' },
  { title: 'Keep it', text: 'Save the reply, then tag it good or bad on the Blackboard' },
]

// ── What each mode is for, and what you can do with it ──────────────────────────────────────────
// The two cards (ClaudeWaysIn.vue), each led by a small schematic so a glance says "I need Kiwi" or
// "I need Claude Code". Every line must fit on ONE line in the narrowest place the cards render
// (Kiwi's `?` dialog) — claudeOverview.test.ts caps them. The full per-tool list is claudeTools.ts. Kiwi's `can` are QUESTIONS you'd ask it; Claude Code's are THINGS it
// makes. Each must be backed by the mode's real tool list: Kiwi's is `KIWI_READ_TOOLS`
// (api/src/kiwi_turn.jl), a terminal session's is the observer's (docs/ai-assist/OBSERVER.md). If a
// tool lands or a limit changes, update here.
export interface Mode {
  title: string
  use: string        // when to use it — one line
  canLabel: string   // what the `can` list is: questions to ask, or things it makes
  can: string[]
  start: string      // how to begin
  never: string      // the limit
}
export const KIWI_MODE: Mode = {
  title: 'Kiwi',
  use: 'In the app — it points, you decide',
  canLabel: 'Ask it',
  can: [
    'Why does this mask look off?',          // get_landscape, get_module_params, get_task_log
    'What sets this cluster apart?',         // get_cluster_summary, get_measure_summary
    'Which images stand out in QC?',         // get_cohort_qc, get_qc_metrics
    'Have I seen this before?',              // get_recent_captures, search_blackboard
  ],
  start: '@ Add to Kiwi or Share a frame, then ask',
  never: 'Never judges, runs or changes your work',
}
export const TERMINAL_MODE: Mode = {
  title: 'Claude Code',
  use: 'In a terminal — it builds for you',
  canLabel: 'It makes',
  can: [
    'Marks on cells and tracks',              // mark_cells, mark_tracks
    'Chains from what you ran',               // create_chain (never run)
    'Boards and notebooks',                   // add_analysis_board, create_notebook
    'Lab-log and Blackboard entries',         // append_lab_log, create_blackboard_entry
  ],
  start: 'Start in terminal on Kiwi’s Chat row',
  // Not "it can't run or delete": Claude Code's own shell sits outside Cecelia's tools, so the honest
  // limit is who presses Run on what it builds (docs/ai-assist/OBSERVER.md).
  never: 'You run what it builds',
}
/** How the two meet. Said once, under both. */
export const CLAUDE_BRIDGE = 'Either can come first — Continue in terminal hands a Kiwi reply over via the Blackboard'

/** Said once under the cards: every reply is a claim to check, not a verdict. */
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
