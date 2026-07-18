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
  return [
    `I'm working in the Cecelia project ${proj}.`,
    ``,
    `Use the cecelia-observer MCP tools (get_project_info, get_task_history, get_qc_metrics, ` +
      `get_cohort_qc, get_image_notes, and the lab log) to review my recent analysis activity and QC ` +
      `across this project, and flag anything that looks off or inconsistent across the set. ` +
      `Read only — do not append to the lab log unless I ask.`,
    ``,
    `If the cecelia-observer MCP tools are not available in this session, just tell me — do not try ` +
      `to install, register, or configure anything.`,
  ].join('\n')
}
