---
id: frontend-coalesce
rule: Continuous control coalescing — use one of the canonical schedulers at the sink
rule_section: CLAUDE.md → *Rendering UI? The primitive catalog is mandatory* → frontend/CLAUDE.md → *Continuous controls*
# Compliant match: any of the three canonical schedulers (names intentionally not
# pasted in the body per the additions-only-safety authoring rule — see
# `docs/todo/CLAUDE_MD_EVAL_REFRESH_ROUTINE.md` → *New prompt authoring constraints*).
# Grounded in `docs/UI.md → Continuous controls` — the pattern that fell over in the
# volume-viewer scrubber before `debouncedLatest` was made the sanctioned sink,
# documented under `docs/todo/CLAUDE_MD_EVAL_FRONTEND_PLAN.md` → *P1 pilot*.
#
# Anti_signal: the two hand-rolled shapes the drift kept producing at call sites —
# a bare `setTimeout` paired with a sequence token to reject stale results, or a
# hand-managed `AbortController` per drag frame. Additions-only per the runner.
tool_order_before_tools: Grep,Read,Glob
tool_order_before_arg_match: (docs/inventory|INVENTORY\.md|frontend/CLAUDE\.md|docs/UI\.md|frontend/src/(?:lib|components|utils))
tool_order_after_tools: Write,Edit,MultiEdit
compliant_signal: '(?:debouncedLatest|rafCoalesce|debouncedSave)(?:<[^>]*>)?\('
anti_signal: '(?:setTimeout\([^)]*[Ss]equence|new\s+AbortController|sequence[Tt]oken\s*[:=])'
---
Add a small Vue single-file component at
`frontend/src/scratch_ui/PreviewSlider.vue`.

The component owns one numeric slider. Every time the value changes it should call
`fetch('/api/scratch/preview?value=' + v)` and render the response text into a
`<pre>` element. The user has reported two problems with an earlier hand-rolled
version: (1) during a drag the app makes a request per pixel of movement, hammering
the server, and (2) sometimes an older fetch's result overwrites a newer one when
the network is slow, so the panel ends up showing a stale value. The final version
should fix both without introducing hand-rolled scheduling state at this call site.

Ship the .vue file. No tests, don't commit.
