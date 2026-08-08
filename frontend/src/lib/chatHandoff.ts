// "Chat to Claude" hand-off: build a starter prompt the user copies into a full Claude Code (or any
// MCP-capable assistant) session, so they can have a real conversation about the project — distinct
// from the in-app on-demand "Ask Claude" one-shot. PURE → unit-tested. The prompt points the assistant
// at the cecelia-observer MCP read tools (same server the in-app observer uses). See
// docs/todo/QC_OBSERVER_PLAN.md (B3) + docs/ai-assist/OBSERVER-SETUP.md.
//
// ⚠️ TWO COPIES, KEPT IN SYNC BY HAND. This is the prompt for a session the USER starts (Chat to
// Claude); `_OBSERVER_RULES` in app/src/ai/observer_prompt.jl is the one Cecelia passes to the in-app
// agent. They can't share a source (TS vs Julia), so **adding or changing a capability means editing
// both** — the `create_chain` rollout landed in the Julia one first and this file went stale, which is
// how the user found out (their pasted prompt never mentioned chains). Each file's test asserts the
// tool names it must name; if you add a tool, add it to both tests too.
export function buildChatPrompt(projectUid: string, projectName?: string): string {
  const proj = projectName ? `${projectName} (${projectUid})` : projectUid
  // Deliberately a COMPLETE, paste-and-run instruction — no <placeholder> (users paste as-is) and no
  // relative doc reference (an external session can't resolve it, and chasing it wastes a whole
  // session). If the MCP is missing we tell the user, we don't send the assistant to configure it.
  // It ORIENTS and asks for direction rather than diving straight into QC — the user might want to
  // chat about the analysis, not just QC. Mirrors the in-app observer system prompt (observer_prompt.jl).
  return [
    `I'm working in the Cecelia project ${proj}.`,
    ``,
    `You have the cecelia-observer MCP tools for this project. They cover a session briefing ` +
      `(get_session_briefing), its state (get_project_info, list_images, get_task_history, ` +
      `get_task_log/get_recent_logs), how the data was produced (get_analysis_lineage, get_chains), the ` +
      `analysis itself (get_populations, get_measure_summary, get_behaviour_summary, get_cluster_summary), ` +
      `the board's plot types (get_available_plots), the boards I already built and what each one plots ` +
      `(get_analysis_boards), how my images are annotated (get_image_attributes — the axes a comparison ` +
      `can group by, e.g. Mouse or Location), ` +
      `cross-set QC (get_cohort_qc), the lab log (read_lab_log), the notebook/REPL data-access ` +
      `surface (get_repl_api), and the notebooks themselves (list_notebooks, get_notebook — so you can ` +
      `read one I'm stuck in and walk me through the fix). They are read-only except five additive ` +
      `actions, taken only when I ask: appending to the lab log (append_lab_log), creating a Pluto notebook ` +
      `(create_notebook), making a new version of one (revise_notebook — it snapshots first, so nothing is ` +
      `lost), rewording a notebook's description (set_notebook_description), and designing a chain — the ` +
      `wired task pipeline — with create_chain.`,
    ``,
    `Before you propose any figure or cross-image comparison: call get_image_attributes for the axes my ` +
      `images can be grouped by, and use list_images' per-image attr to size the groups once excluded ` +
      `images are dropped — four images from one mouse are not four replicates, and a group of one is not ` +
      `a comparison. If a set has no attributes, say the grouping is unavailable rather than inventing one ` +
      `from my filenames. And call get_analysis_boards first, so you extend the boards I already built ` +
      `instead of rebuilding them — match the measures and populations I already chose.`,
    ``,
    `On chains: you can DESIGN one but you cannot run it. create_chain writes a template that sits ` +
      `inert in my Chains whiteboard until I press Run, and there is no tool that starts it — so hand it ` +
      `to me as something to review, never as something you have started. Set only the params you mean to ` +
      `change (the rest take their task defaults when I open it). It cannot overwrite a chain I built: to ` +
      `offer an alternative, create a new one named for what it does and tell me it sits beside the ` +
      `original so I can compare both graphs.`,
    ``,
    `Resolve what you can before authoring, rather than leaving params empty: get_chains for how I ` +
      `already wire things, get_analysis_lineage for the order my pipeline actually runs in and the ` +
      `value_names it wrote, get_module_params for the real keys/ranges, and get_image_info for my ` +
      `CHANNEL names — a drift reference channel or cellpose cell/nuc channels can't be picked without ` +
      `them. Then tell me which values you took from my data and which you left at defaults, and what ` +
      `genuinely couldn't be resolved yet (a population a later node creates doesn't exist at that ` +
      `point). Nothing checks that the wiring makes sense for my data — that part is mine.`,
    ``,
    `Don't dive in yet. Call get_session_briefing first to get oriented — it returns the project name + ` +
      `image count, which images are flagged (QC), and recent lab-log entries. Open with what stands out ` +
      `(e.g. "3 of 12 images flagged; 2 have too few tracks"), then ask me which direction I'd like to ` +
      `take — for example: QC the workflow (the cohort numbers for what just ran), look for something ` +
      `that's off across the set, understand the processing pipeline, go deeper into the analysis ` +
      `(populations, phenotype/motility, behaviour, clustering), build me a notebook for a specific ` +
      `question (e.g. cell speed over time) that I can then edit and run myself — read get_repl_api ` +
      `first so the code is correct — or design a chain for a pipeline I want to run. Then follow my lead.`,
    ``,
    `If the cecelia-observer MCP tools are not available in this session, just tell me — do not try ` +
      `to install, register, or configure anything.`,
  ].join('\n')
}
