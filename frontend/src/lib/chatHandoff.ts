// "Chat to Claude" hand-off: build a starter prompt the user copies into a full Claude Code (or any
// MCP-capable assistant) session, so they can have a real conversation about the project — distinct
// from the in-app on-demand "Ask Claude" one-shot. PURE → unit-tested. The prompt points the assistant
// at the cecelia-observer MCP read tools (same server the in-app observer uses). See
// docs/todo/QC_OBSERVER_PLAN.md (B3) + docs/ai-assist/OBSERVER-SETUP.md.
export function buildChatPrompt(projectUid: string, projectName?: string): string {
  const proj = projectName ? `${projectName} (${projectUid})` : projectUid
  return [
    `I'm working in the Cecelia project ${proj}.`,
    ``,
    `Use the cecelia-observer MCP tools to read my project — get_project_info, get_task_history, ` +
      `get_qc_metrics, get_cohort_qc, get_image_notes and the lab log — to review my recent analysis ` +
      `activity and QC, then help me with:`,
    ``,
    `<describe what you need — e.g. "check my segmentation counts are consistent across the set", or ` +
      `"summarise what I did today and flag anything off">`,
    ``,
    `(If the cecelia-observer MCP isn't connected in this session, set it up first — see ` +
      `docs/ai-assist/OBSERVER-SETUP.md. Please read only; append to the lab log only if I ask.)`,
  ].join('\n')
}
