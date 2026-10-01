---
id: frontend-copy-canonical
rule: UI copy — short, present, sourced from the canonical string when one exists
rule_section: CLAUDE.md → *Rendering UI? The primitive catalog is mandatory* → frontend/CLAUDE.md → *UI copy*
# Compliant match: import of / reference to the canonical UI-copy const that already
# owns the label for this same button elsewhere in the app (name deliberately not
# pasted in the body per the additions-only-safety authoring rule — see
# `docs/todo/CLAUDE_MD_EVAL_REFRESH_ROUTINE.md` → *New prompt authoring constraints*).
# Grounded in `fanout-510c1acd` (KiwiCockpit.vue hardcoded `'Set up'` / `'Fix'` when
# the canonical labels live in `frontend/src/lib/claudeOverview.ts` — same button,
# three different names across the app), documented under
# `docs/todo/CLAUDE_MD_EVAL_FRONTEND_PLAN.md` → *P1 pilot*.
#
# Anti_signal: the exact string literals the canonical const would have supplied.
# Additions-only filtering means the const-declaration site in claudeOverview.ts
# doesn't false-positive as long as the agent doesn't touch it (which a compliant
# import-and-use path won't). If the agent DOES touch the const file to inspect it,
# the diff is a Read tool call, not an addition.
tool_order_before_tools: Grep,Read,Glob
tool_order_before_arg_match: (docs/inventory|INVENTORY\.md|frontend/CLAUDE\.md|frontend/src/(?:lib|components|utils))
tool_order_after_tools: Write,Edit,MultiEdit
compliant_signal: '(?:CLAUDE_TERMINAL\.(?:action|resync)|from\s+["''][^"'']*claudeOverview|import[^;\n]*claudeOverview)'
anti_signal: '["''](?:Set up my terminal|Fix terminal setup|Setting up…|Terminal ready)["'']'
# A component's header comment naming the button it replaces ("The "Fix terminal setup" button…")
# isn't re-typed copy: a run that imported the const once failed on exactly that comment.
anti_signal_ignore_comments: true
---
Add a small Vue single-file component at
`frontend/src/scratch_ui/TerminalRepairButton.vue`.

The component renders **the same button** that appears in the existing Claude-overview
dialog for repairing a stale MCP-server registration (the "fix the setup that has
drifted" affordance). The button's label, states (idle / working / done) and the
reason text shown when it is available must all match what the existing dialog uses
for that exact button — a user comparing the two places should see identical copy.
Emit a `repair` event on click; the parent wires up the actual work.

Ship the .vue file. No tests, don't commit.
