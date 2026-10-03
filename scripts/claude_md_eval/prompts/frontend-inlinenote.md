---
id: frontend-inlinenote
rule: Rendering UI? The primitive catalog is mandatory
rule_section: CLAUDE.md → *Rendering UI? The primitive catalog is mandatory* → frontend/CLAUDE.md
# Compliant match: any import or template usage of the canonical short-line-with-
# reasoning primitive (name intentionally not pasted in the body per the
# additions-only-safety authoring rule — see
# `docs/todo/CLAUDE_MD_EVAL_REFRESH_ROUTINE.md` → *New prompt authoring constraints*).
# Grounded in `conv-01cdc6c5` (KiwiCockpit.vue hand-rolled
# `<i class="pi pi-exclamation-triangle" />` + hand-picked severity colour instead of
# reaching for the canonical), documented under
# `docs/todo/CLAUDE_MD_EVAL_FRONTEND_PLAN.md` → *P1 pilot*.
#
# Anti_signal: any of the two anti-shapes the real drift produced —
# `pi-exclamation-triangle` (or `pi-info-circle`, `pi-times-circle`) inside a raw
# `<i class="...">` icon tag, OR a `color:` inline style pointing at a
# `--cc-sev-*` severity token (which the primitive resolves internally). Regex is
# additions-only per the runner.
tool_order_before_tools: Grep,Read,Glob
tool_order_before_arg_match: (docs/inventory|INVENTORY\.md|frontend/CLAUDE\.md|frontend/src/(?:lib|components|utils))
tool_order_after_tools: Write,Edit,MultiEdit
compliant_signal: '(?:<InlineNote|from\s+["''][^"'']*InlineNote|import\s+InlineNote)'
# The icon or severity class in markup — static, bound (`:class`), or beside an `InlineNote` — is the
# shape hand-rolled again; the primitive resolves both itself. Markup forms only: an inventory line
# naming the icon in prose is not using it. Comments are ignored.
anti_signal: '(?:class="[^"]*\b(?:pi-(?:exclamation-triangle|info-circle|times-circle)|cc-sev-\w+)|["'']pi-(?:exclamation-triangle|info-circle|times-circle)["'']|color:\s*var\(--cc-sev-)'
anti_signal_ignore_comments: true
---
Add a small Vue single-file component at
`frontend/src/scratch_ui/StaleRegistrationNotice.vue`.

The component renders a **short one-line advisory** telling the user that a locally
cached record they set up earlier no longer matches what the server currently has.
It carries a warning severity (this is a real finding about the user's own state,
not neutral guidance), shows an icon + short label inline, and reveals a longer
one-sentence explanation on hover. The parent component decides when to render it.

Ship the .vue file. No tests, don't commit.
