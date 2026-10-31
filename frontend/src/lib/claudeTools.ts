// What Kiwi and a Claude Code session can each USE — the "What each can use" accordion in Kiwi's `?`
// dialog (ClaudeOverviewDialog). The words are for users, so they are written here, not lifted from the
// tool docstrings (those are written for the model). What keeps them true is `claudeTools.test.ts`:
//   - every tool `mcp/cecelia_mcp/server.py` registers is in exactly one capability or in HIDDEN_TOOLS —
//     a new tool fails the test until it is worded here;
//   - a capability's `kiwi` tick matches `KIWI_READ_TOOLS` (api/src/kiwi_turn.jl) for every tool it
//     lists — so the ticks can't claim more (or less) than Kiwi's allow-list.
// Capabilities with no `tools` are the app's own controls (Kiwi's @, Share, refs) or Claude Code's
// own abilities (writing a module); those are worded and ticked by hand.

export type ToolGroupKey = 'app' | 'project' | 'runs' | 'results' | 'record' | 'viewer' | 'builds'

export const TOOL_GROUPS: { key: ToolGroupKey; title: string }[] = [
  { key: 'app',     title: 'Working with you' },
  { key: 'project', title: 'Your project' },
  { key: 'runs',    title: 'Runs and QC' },
  { key: 'results', title: 'Populations and results' },
  { key: 'record',  title: 'What you shared and wrote' },
  { key: 'viewer',  title: 'Points on your screen' },
  { key: 'builds',  title: 'Builds for you' },
]

export interface Capability {
  group: ToolGroupKey
  label: string        // one short line
  tools: string[]      // the MCP tools behind it; [] for an app control or a Claude Code ability
  kiwi: boolean
  terminal: boolean
}

export const CAPABILITIES: Capability[] = [
  // ── Working with you in the app — worded by OUTCOME, not by Kiwi's buttons: the table answers
  // "can it do this for me?", and where both can (by different means) both are ticked. A dash must
  // mean it really can't. Hand-ticked; the tools behind the terminal side are listed in their own groups.
  // Kiwi only: the @ buttons attach to Kiwi's prompt; a terminal session gets context by Share or by asking.
  { group: 'app', label: '@ Add to Kiwi — images, populations, plots, viewer, tasks', tools: [], kiwi: true, terminal: false },
  { group: 'app', label: 'Share a frame and draw on it', tools: [], kiwi: true, terminal: true },
  // Kiwi: you click a claim's ref. Claude Code: it seeks/marks the viewer itself ('viewer' group).
  { group: 'app', label: 'Take you to the data behind a claim', tools: [], kiwi: true, terminal: true },
  // Kiwi: you click the proposed plot. Claude Code: add_analysis_board ('builds' group).
  { group: 'app', label: 'Turn a plot it suggests into a board', tools: [], kiwi: true, terminal: true },
  // Both: Kiwi through its reply's Save button, Claude Code through create_blackboard_entry (that tool
  // is listed once, on the 'builds' line below — this line is the user-facing fact, ticked by hand)
  { group: 'app', label: 'Save to the Blackboard', tools: [], kiwi: true, terminal: true },

  { group: 'project', label: 'Project, sets and images', tools: ['get_project_info', 'list_images', 'find_object'], kiwi: true, terminal: true },
  { group: 'project', label: 'An image’s metadata, channels and notes', tools: ['get_image_info', 'get_image_notes'], kiwi: true, terminal: true },
  { group: 'project', label: 'Image attributes — treatment, mouse, timepoint', tools: ['get_image_attributes'], kiwi: true, terminal: true },
  { group: 'project', label: 'Other projects on this machine', tools: ['list_projects'], kiwi: false, terminal: true },
  { group: 'project', label: 'Your LabArchives notebook, once linked', tools: ['get_labarchives_context'], kiwi: false, terminal: true },

  { group: 'runs', label: 'QC flags, and cohort outliers for a step', tools: ['get_qc_metrics', 'get_cohort_qc'], kiwi: true, terminal: true },
  { group: 'runs', label: 'Run logs and recent runs', tools: ['get_task_log', 'get_task_history'], kiwi: true, terminal: true },
  { group: 'runs', label: 'Step settings and their valid ranges', tools: ['get_module_params'], kiwi: true, terminal: true },
  { group: 'runs', label: 'Which step does what, and the guides', tools: ['get_task_catalogue', 'get_guide'], kiwi: true, terminal: true },
  { group: 'runs', label: 'How each result was produced', tools: ['get_analysis_lineage'], kiwi: true, terminal: true },
  { group: 'runs', label: 'The app console', tools: ['get_recent_logs'], kiwi: false, terminal: true },

  { group: 'results', label: 'Populations, and gates as pictures', tools: ['get_populations', 'gate_plot', 'gate_cells_view'], kiwi: true, terminal: true },
  { group: 'results', label: 'A gate’s distribution, counts and example cells', tools: ['gate_histogram', 'gate_stats', 'gate_examples'], kiwi: true, terminal: true },
  { group: 'results', label: 'Phenotype and motility summaries', tools: ['get_measure_summary'], kiwi: true, terminal: true },
  { group: 'results', label: 'Behaviour states', tools: ['get_behaviour_summary'], kiwi: true, terminal: true },
  { group: 'results', label: 'Clusters and spatial regions', tools: ['get_cluster_summary', 'get_region_clusters'], kiwi: true, terminal: true },
  { group: 'results', label: 'Cell–cell contacts', tools: ['get_contact_stats'], kiwi: true, terminal: true },
  { group: 'results', label: 'Your boards, plots, chains and notebooks',
    tools: ['get_analysis_boards', 'get_available_plots', 'list_plots', 'get_chains', 'list_notebooks', 'get_notebook'], kiwi: true, terminal: true },

  { group: 'record', label: 'Frames you shared, and the viewer right now',
    tools: ['get_recent_captures', 'get_capture', 'get_capture_landscape_tiles', 'get_landscape'], kiwi: true, terminal: true },
  { group: 'record', label: 'Your Blackboard', tools: ['list_blackboard_entries', 'read_blackboard_entry', 'search_blackboard'], kiwi: true, terminal: true },
  { group: 'record', label: 'The lab log', tools: ['read_lab_log'], kiwi: true, terminal: true },
  // the repeat-failure digest rides on the session briefing (RUBBER_DUCK_FIT_PLAN D1) — both see it
  { group: 'record', label: 'Warns when a bad-tagged failure keeps coming back', tools: [], kiwi: true, terminal: true },

  { group: 'viewer', label: 'Mark cells, tracks and regions on the viewer', tools: ['mark_cells', 'mark_tracks', 'mark_tile'], kiwi: false, terminal: true },
  { group: 'viewer', label: 'Mark a spot on a plot or a shared frame', tools: ['mark_plot', 'mark_freeform', 'select_on_plot'], kiwi: false, terminal: true },
  { group: 'viewer', label: 'Jump to a frame, open a plot, point at a control',
    tools: ['seek_viewer', 'open_analysis_board_plot', 'point_at_ui'], kiwi: false, terminal: true },

  { group: 'builds', label: 'A chain from what you ran — you run it', tools: ['suggest_chain', 'create_chain'], kiwi: false, terminal: true },
  { group: 'builds', label: 'An analysis board', tools: ['add_analysis_board'], kiwi: false, terminal: true },
  { group: 'builds', label: 'A gate as a new population — marked ✦, undoable; moves only its own', tools: ['add_gate', 'set_gate', 'delete_gate'], kiwi: false, terminal: true },
  { group: 'builds', label: 'A Pluto notebook, or a new version of one',
    tools: ['create_notebook', 'revise_notebook', 'set_notebook_description'], kiwi: false, terminal: true },
  { group: 'builds', label: 'Blackboard entries it writes, revises and tags good/bad',
    tools: ['create_blackboard_entry', 'revise_blackboard_entry', 'set_blackboard_status', 'set_blackboard_outcome', 'set_blackboard_section_outcome'],
    kiwi: false, terminal: true },
  { group: 'builds', label: 'Dated lab-log lines', tools: ['append_lab_log'], kiwi: false, terminal: true },
  { group: 'builds', label: 'Your LabArchives context, saved to the project', tools: ['set_labarchives_context'], kiwi: false, terminal: true },
  // Claude Code's own file tools, not an MCP tool — Kiwi runs with none (`--tools ""`)
  { group: 'builds', label: 'A custom module for a missing step', tools: [], kiwi: false, terminal: true },
]

/** Registered tools a user never needs to know about, with why — still accounted for by the test. */
export const HIDDEN_TOOLS: Record<string, string> = {
  get_session_briefing: 'session start-up',
  register_push_target: 'pairing',
  set_observer_active: 'live-observer plumbing',
  get_observer_stats: 'live-observer plumbing',
  poll_observations: 'live-observer plumbing',
  get_repl_api: 'how it writes notebook code',
  get_object_ids: 'ids it looks up before marking',
}

/** The groups in order, each with its capabilities — what the accordion renders. */
export function capabilityGroups(): { key: ToolGroupKey; title: string; items: Capability[] }[] {
  return TOOL_GROUPS.map(g => ({ ...g, items: CAPABILITIES.filter(c => c.group === g.key) }))
    .filter(g => g.items.length > 0)
}
