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

export interface CapabilityGroup {
  key: 'sees' | 'suggests' | 'creates' | 'cant'
  title: string
  icon: string
  tone: 'neutral' | 'good' | 'muted'   // 'muted' for the "Can't" group
  items: string[]
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
      'Click Chat to Claude (copies a starter prompt)',
      'Paste it into Claude Code',
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
      'Pluto notebooks — runnable analysis you then own & edit',
      'CSV exports for Prism / R',
      'Lab-log notes (only when you ask)',
    ],
  },
  {
    key: 'cant', title: "Can't", icon: 'pi-ban', tone: 'muted',
    items: [
      'Change your data (h5ad, gates, project config)',
      'Run tasks — you approve and run everything',
      'Open raw image pixels',
      'Draw the biological conclusion — that stays yours',
    ],
  },
]

// Copyable one-liners that show the range — from QC to a notebook.
// Keep them short and uniform so they read as a clean row of chips.
export const CLAUDE_EXAMPLES: string[] = [
  'Why is this image off?',
  'Plot the behaviour differences',
  'Build a cell-speed notebook',
]
