// "Chat to Claude" hand-off: the starter line the user copies into a full Claude Code (or any
// MCP-capable assistant) session, so they can have a real conversation about the project — distinct
// from the in-app on-demand "Ask Claude" one-shot. PURE → unit-tested. See
// docs/ai-assist/OBSERVER-SETUP.md.
//
// It is ONE LINE, and that is the design. This used to be a ~900-word prompt that named every MCP
// tool and every discipline — which made the assistant's competence depend on the user pasting a wall
// of text, and put a copy of the observer's rules in a Vue file where it twice went stale against
// `mcp/cecelia_mcp/server.py` (create_chain, then get_analysis_boards/get_image_attributes: a tool
// added to the server and to the Julia prompt but not here, so the capability was simply never
// offered). Both halves now live in the MCP server itself — `SERVER_INSTRUCTIONS` (delivered in the
// `initialize` response, so the assistant knows to pull the briefing first) and `BRIEFING_GUIDANCE`
// (returned by get_session_briefing) in `mcp/cecelia_mcp/guidance.py`. There is nothing left here to
// keep in sync, and "hey claude, check my project in cecelia" now works with nothing pasted at all.
//
// So all this has to do is the one thing the server cannot: say WHICH project. The assistant can
// resolve it itself via list_projects (most-recently-opened first), but that is an inference; the app
// knows. Keep it that way — if you find yourself explaining a tool or a rule here, it belongs in
// guidance.py.
import type { BlackboardEntrySummary } from '../utils/blackboardApi'

export function buildChatPrompt(projectUid: string, projectName?: string,
                                entry?: Pick<BlackboardEntrySummary, 'entryId' | 'title'>): string {
  const proj = projectName ? `${projectName} (${projectUid})` : projectUid
  // Naming the server orients an assistant whose registration went stale (it can say "those tools
  // aren't here" instead of guessing what Cecelia is), and the second sentence stops it going off to
  // install one — without the tools there is no `instructions` payload carrying that rule.
  const base = `I'm working in the Cecelia project ${proj}. Have a look at it with the ` +
    `cecelia-observer MCP tools. If they are not available in this session, just tell me — do not ` +
    `try to install, register, or configure anything.`
  if (!entry) return base
  // Kiwi → terminal hand-off ("Continue in terminal"): the turn was just saved as a Blackboard entry,
  // which is the only Kiwi state a terminal session can read. Like the project, this only says WHICH.
  const title = entry.title.replace(/[\s"<>]+/g, ' ').trim()
  return `${base} I left off in Kiwi on the Blackboard entry "${title}" (${entry.entryId}) — pick up from there.`
}
