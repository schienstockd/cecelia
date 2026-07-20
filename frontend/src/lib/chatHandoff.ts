// "Chat to Claude" hand-off: build a starter prompt the user copies into a full Claude Code (or any
// MCP-capable assistant) session, so they can have a real conversation about the project — distinct
// from the in-app on-demand "Ask Claude" one-shot. PURE → unit-tested. The prompt points the assistant
// at the cecelia-observer MCP read tools (same server the in-app observer uses). See
// docs/todo/QC_OBSERVER_PLAN.md (B3) + docs/ai-assist/OBSERVER-SETUP.md.
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
      `the board's plot types (get_available_plots), ` +
      `cross-set QC (get_cohort_qc), the lab log (read_lab_log), and the notebook/REPL data-access ` +
      `surface (get_repl_api). They are read-only except two additive actions, taken only when I ask: ` +
      `appending to the lab log (append_lab_log) and creating a Pluto notebook (create_notebook).`,
    ``,
    `Don't dive in yet. Call get_session_briefing first to get oriented — it returns the project name + ` +
      `image count, which images are flagged (QC), and recent lab-log entries. Open with what stands out ` +
      `(e.g. "3 of 12 images flagged; 2 have too few tracks"), then ask me which direction I'd like to ` +
      `take — for example: QC the workflow (the cohort numbers for what just ran), look for something ` +
      `that's off across the set, understand the processing pipeline, go deeper into the analysis ` +
      `(populations, phenotype/motility, behaviour, clustering), or build me a notebook for a specific ` +
      `question (e.g. cell speed over time) that I can then edit and run myself — read get_repl_api ` +
      `first so the code is correct. Then follow my lead.`,
    ``,
    `If the cecelia-observer MCP tools are not available in this session, just tell me — do not try ` +
      `to install, register, or configure anything.`,
  ].join('\n')
}
