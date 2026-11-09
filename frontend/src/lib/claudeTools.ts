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
  { group: 'project', label: 'Why an image looks off at acquisition', tools: ['get_acquisition_knowledge'], kiwi: true, terminal: true },
  { group: 'project', label: 'Image attributes — treatment, mouse, timepoint', tools: ['get_image_attributes'], kiwi: true, terminal: true },
  { group: 'project', label: 'Other projects on this machine', tools: ['list_projects'], kiwi: false, terminal: true },
  { group: 'project', label: 'Your LabArchives notebook, once linked', tools: ['get_labarchives_context'], kiwi: false, terminal: true },

  { group: 'runs', label: 'QC flags, and cohort outliers for a step', tools: ['get_qc_metrics', 'get_cohort_qc'], kiwi: true, terminal: true },
  { group: 'runs', label: 'What the record can’t support in a claim', tools: ['check_claim'], kiwi: true, terminal: true },
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
  { group: 'results', label: 'Cell–cell contacts and spatial patterns', tools: ['get_contact_stats', 'get_spatial_patterns'], kiwi: true, terminal: true },
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
  { group: 'builds', label: 'A claim you state, as a Blackboard entry with its checks', tools: ['state_claim'], kiwi: false, terminal: true },
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

// ── Every way AI reaches Cecelia — the AI switch's registry (docs/todo/AI_TOGGLE_PLAN.md P2) ──────────
// One row per surface that starts a model, hands out a way to start one, touches a Claude login or
// sends something to a Claude session. The opt-in dialog's "what is sent" and "what you gain" lists
// read `sends` / `gain` from here. Each row names the server-side point that refuses it with AI off
// (`app/src/ai/ai_switch.jl`); `claudeTools.test.ts` pins the rows to the backend:
//   - every route wrapped in `_ai_gated(…)` in api/src/server.jl is a `route` row's route, and back;
//   - every api/src file that calls `kiwi_start_turn(` is a `turn` row's source, and back;
//   - `_agent_spawn_cmd(` (the one place a `claude` process is built) is used only in agent_runner.jl
//     and claude_auth.jl — a new spawn elsewhere fails until it is routed through a gated seam.
// The CAPABILITIES above are what a session can USE once AI is on; this is what the switch turns off.

/** Which server-side point refuses the surface with AI off. */
export type AiEnforcedBy =
  | 'turn'      // kiwi_start_turn — every in-app model turn
  | 'request'   // handle_stream — every request marked `X-Cecelia-Client: claude` (plan D1)
  | 'route'     // _ai_gated(…) on the route table
  | 'push'      // push_capture_notification — outbound, so no request to refuse

export interface AiSurface {
  id: string
  label: string          // what the user knows it as
  enforcedBy: AiEnforcedBy
  source: string         // the backend file holding the refusal (`turn`: the caller of kiwi_start_turn)
  routes: string[]       // `route` rows: the gated paths; [] otherwise
  sends: string          // what leaves the machine — user wording, '' when nothing does
  gain: string           // what the user gets — user wording, '' for plumbing
}

export const AI_SURFACES: AiSurface[] = [
  { id: 'kiwi', label: 'Ask Kiwi', enforcedBy: 'turn', source: 'api/src/kiwi_api.jl', routes: [],
    sends: 'Your question, and what Kiwi reads to answer it: names, notes, settings and numbers from the project, plus image crops and the frames you share',
    gain: 'Answers about your project, each one linked to the data behind it' },
  { id: 'claim-review', label: 'Review a claim', enforcedBy: 'turn', source: 'api/src/claim_review.jl', routes: [],
    sends: 'The claim and the project record behind it, image crops included',
    gain: 'A second reading of a claim before it leaves Cecelia' },
  { id: 'acq-review', label: 'Review a test acquisition', enforcedBy: 'turn', source: 'api/src/acq_review.jl', routes: [],
    sends: 'The set’s acquisition plan and the movies’ record: settings, photon and drift numbers, QC',
    gain: 'What to change at the microscope before acquiring the rest' },
  { id: 'terminal', label: 'Claude in your terminal', enforcedBy: 'request', source: 'api/src/server.jl', routes: [],
    sends: 'Whatever your Claude session asks Cecelia’s tools for — the project, results and images',
    gain: 'A Claude session that reads your project, points at the viewer and builds chains, boards, gates and notebooks for you' },
  { id: 'terminal-start', label: 'Start Claude from Cecelia', enforcedBy: 'route', source: 'api/src/server.jl',
    routes: ['/api/observer/chat', '/api/kiwi/terminal/command'], sends: '', gain: '' },
  { id: 'terminal-setup', label: 'Set up Claude in your terminal', enforcedBy: 'route', source: 'api/src/server.jl',
    routes: ['/api/observer/register'], sends: '', gain: '' },
  { id: 'claude-login', label: 'Claude login', enforcedBy: 'route', source: 'api/src/server.jl',
    routes: ['/api/kiwi/profiles/auth', '/api/kiwi/profiles/login', '/api/kiwi/profiles/login/code'], sends: '', gain: '' },
  { id: 'capture-push', label: 'Send a shared frame to your Claude session', enforcedBy: 'push', source: 'api/src/push_writer.jl', routes: [],
    sends: 'A shared frame’s id and your note, to your own paired Claude session',
    gain: 'Your session sees what you shared without copy and paste' },
]

/** The rows the opt-in dialog lists: everything that sends data or gives the user something. */
export function aiDialogRows(): AiSurface[] {
  return AI_SURFACES.filter(s => s.sends || s.gain)
}
