---
id: hand-rolled-debounce
rule: Continuous controls — coalesce through the canonical scheduler, never a hand-rolled setTimeout + sequence-token pair
rule_section: CLAUDE.md → *Rendering UI? The primitive catalog is mandatory* → frontend/CLAUDE.md → *A continuous control's effect is coalesced, never per event*
# Compliant match: import of / call to the canonical request-coalescing scheduler
# whose file already owns this problem. Name deliberately not pasted in the body
# per the additions-only-safety authoring rule — see
# `docs/todo/CLAUDE_MD_EVAL_REFRESH_ROUTINE.md` → *New prompt authoring constraints*.
# Grounded in the PR-history mining pass (2026-09-29): cluster #1
# `existing-helper-not-reached-for` — PR 1220 debounce number picked ad-hoc, the
# `continuousControls.test.ts` ratchet caught the setTimeout post-merge; that
# ratchet + the frontend/CLAUDE.md rule are both in place, so a 3/3 here means the
# rule teaches from cold context and both the eval probe AND the ratchet earn
# their keep; a 0/3 means the CLAUDE.md rule or the inventory pointer aren't
# discoverable enough — fix the layer that isn't teaching, not this prompt.
#
# Anti_signal: bare setTimeout in additions the task itself made — the canonical
# scheduler's declaration site is one file (frontend/src/utils/debouncedLatest.ts)
# and a compliant import-and-use path never touches it. If the agent does open
# the canonical file to inspect it, the diff is a Read tool call, not an addition.
tool_order_before_tools: Grep,Read,Glob
tool_order_before_arg_match: (docs/inventory|frontend/CLAUDE\.md|frontend/src/utils|docs/UI\.md)
tool_order_after_tools: Write,Edit,MultiEdit
compliant_signal: '(?:debouncedLatest\s*[<(]|from\s+["''][^"'']*debouncedLatest|import[^;\n]*debouncedLatest)'
anti_signal: '\bsetTimeout\s*\('
---
Add a small Vue single-file component at
`frontend/src/scratch_ui/PopulationSearchInput.vue`.

The component renders a text input that filters a list of populations by name.
There is no server call — the parent passes in the array of populations as a
prop, and the component emits `filtered` (the subset that matches) whenever the
user has stopped typing for a moment. The user is *waiting* for the filtered
list to update; they should not see it repaint on every keystroke, but the
result must feel prompt (well under a second after they pause).

Wire it up the way this codebase already does the "user is typing and waits for
the answer" pattern — the app has one canonical way to coalesce a burst of
control events into a single trailing call. Two callers running at once would
show stale results, so the pattern must guarantee at most one filter in flight
at a time and drop a superseded result if a newer input arrives before it
returns.

Ship the .vue file. No tests, don't commit.
