# CLAUDE.md compliance eval — frontend rule coverage

**Status:** DESIGN. No prompts shipped yet. Written 2026-09-28 alongside PR #1274 to
capture the plan before scope slips. Sibling:
[`CLAUDE_MD_EVAL_PLAN.md`](CLAUDE_MD_EVAL_PLAN.md) — the parent eval this extends.

## Why this exists

The current 11-prompt catalog tests backend I/O rules (h5ad, zarr, `run_py`,
utf-8 / atomic writes, Windows helpers, discovery-first, `zarr_utils`). Zero coverage
of `frontend/CLAUDE.md`'s rules — and the effectiveness log shows that's exactly where
the fanout / convention findings keep catching real drift on shipped PRs.

Recent live findings the current eval catalog could not have surfaced:

- `conv-01cdc6c5` (KiwiCockpit.vue) — hand-rolled `<i class="pi pi-exclamation-triangle" />`
  + hand-picked colour instead of `<InlineNote severity="warn">`. Exactly the failure
  mode `InlineNote.vue`'s own docstring warns about.
- `fanout-510c1acd` (KiwiCockpit.vue) — button hardcodes `'Set up'` / `'Fix'` when the
  canonical labels live in `frontend/src/lib/claudeOverview.ts` (`CLAUDE_TERMINAL.action`
  / `.resync`). Same button, three different names.
- `fanout-c3552e14` (LabLogPanel.vue) — `onMounted(observer.refresh())` removed with a
  comment saying it kept the button label fresh; the successor site declined to refresh
  and the freshness contract was silently lost.

Root [`CLAUDE.md`](../../CLAUDE.md) → *Rendering UI? The primitive catalog is mandatory*
points at [`frontend/CLAUDE.md`](../../frontend/CLAUDE.md), which pins four rules the
eval could test:

1. **Primitive catalog** — use canonical (`InlineNote`, `CcToggle`, `.cc-btn*`,
   `ChipSelect`, `SwatchSelect`, `BaseModal`, `TeleportPopover`, `TabbedCanvas`,
   `CollapsibleSection`, `ConfirmButton`, …); never hand-roll a variant. Enforced by
   tests but that's for shipped code — the eval measures whether an agent *reaches for*
   the canonical primitive in the first place.
2. **UI copy** — short, present, sourced from the canonical string when one exists in
   `frontend/src/lib/*.ts`. Never inline a duplicate label.
3. **Persist every user-settable option** — `useViewState` over store, never a bare
   `ref()` on a module page / canvas.
4. **Continuous control coalescing** — one of three canonical schedulers
   (`debouncedLatest` / `rafCoalesce` / `debouncedSave`) at the sink; never a
   hand-rolled `setTimeout` + sequence-token pair at the call site.

## Non-negotiables

- **Deterministic scoring stays.** No LLM judge. Same discipline as the parent plan.
- **Rules only, not aesthetics.** No prompt tests "does the layout look good" — that's
  what the visual-validation rule covers (`CLAUDE.md → Real-data visual validation`).
- **Rules under test must not already be ratcheted.** Ratchets catch anti-patterns in
  shipped code; the eval measures pre-shipping reach for the canonical. Testing a
  rule that a ratchet already blocks is redundant — the ratchet would fail the build
  before the eval even runs. Filter the catalog against `frontend/src/**` ratchets
  in the same commit as the prompt lands.

## Locked decisions

- **D1. Same runner, same scorer.** No fork of `run_prompt.py`. `_regex_hits` +
  `tool_order_passes` + additions-only filtering already cover the shapes we need
  (canonical import present, anti-pattern absent, inventory read before write). Vue
  files score the same way as Python files — a `+import InlineNote` line is just a
  `+` line to the regex.

- **D2. Prompts live flat in `scripts/claude_md_eval/prompts/`** with a `frontend-`
  prefix (e.g. `frontend-inlinenote.md`, `frontend-copy-canonical.md`). Rejected a
  subdirectory (`prompts/frontend/*.md`) — the runner would need to recurse, and the
  catalog rollup would need a "backend vs frontend" split we don't otherwise have.
  Prefix + `_list_prompt_ids` sorting is enough.

- **D3. Prompts are indirect by default.** Bug-report shape, symptom first, no
  primitive named. Direct frontend prompts would defeat the purpose the same way
  direct backend prompts did — the agent would just grep the primitive name. The
  three real findings above are the exact voice to imitate.

- **D4. Screenshot-bearing past prompts get hand-translated to text descriptions**
  in the prompt frontmatter. `[Image #N]` refs are lost, but the reported symptom
  usually survives. If a screenshot IS the whole ask, skip that prompt.

- **D5. Scorer surface for the primitive-catalog rule:**
  - `compliant_signal` — matches the canonical import or usage
    (e.g. `import InlineNote`, `<InlineNote`).
  - `anti_signal` — matches the hand-rolled shape (the severity icon or colour in markup). Regex of
    record: `scripts/claude_md_eval/prompts/frontend-inlinenote.md`.
  - `tool_order` — inventory-touching Grep/Read/Glob before Write/Edit/MultiEdit,
    same widened matcher as the backend catalog.

- **D6. Scorer surface for the UI-copy rule:**
  - Trickier — the canonical string lives in a `lib/*.ts` const, and the prompt
    asks the agent to add a button somewhere else. Compliant = import the const;
    anti = copy re-typed as a literal (regex of record:
    `scripts/claude_md_eval/prompts/frontend-copy-canonical.md`).
  - Requires a per-prompt authoring pass ("here's the canonical string; here's
    the token to grep as anti"). Two prompts of this shape is enough for the
    pilot; scale after we see if the signal is clean.

- **D7. Scorer surface for the coalescing rule:**
  - `compliant_signal` — any of the three canonical schedulers; `anti_signal` — the hand-rolled shapes.
    The regexes of record are in `scripts/claude_md_eval/prompts/frontend-coalesce.md` (tightened
    2026-10-03, `CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md` phase 12).
  - Hard to catch every valid vs invalid shape; may need to add prompts iteratively
    as we see what agents actually reach for. Land this rule last.

- **D8. Persistence rule scorer:**
  - `compliant_signal` — `useViewState\(`
  - `anti_signal` — `\bref\(` inside a `.vue` `<script setup>` block for a value
    that then gets bound to a control's `v-model`. Regex is coarse; false positives
    on legitimate `ref()` uses (DOM refs, etc.). Not a candidate for the first pilot.

- **D9. Pilot at N=1 first, one prompt per rule (or one per rule I have a good
  scoring recipe for).** ~4 prompts × ~\$0.50 each = ~\$2/pass. Cheap enough to run
  before committing to N=3. Batch after signal is clean.

- **D10. Ablation cost budget.** Frontend prompts probably 3-5× the direct backend
  cost (longer sessions, more files touched). Full frontend ablation at N=3 could
  land ~\$20-\$30. Do NOT run the ablation until every pilot prompt has been
  trace-inspected per D12. Traces are the ground truth here — a Vue-side compliant
  agent can pass regex + still hand-roll a variant the regex missed.

## Non-goals

- **No LLM-judged grader.** Same discipline as parent plan; deterministic only. If
  a rule genuinely can't be regex-scored (e.g. "used correct spacing / grid alignment"),
  that's not this eval's job — it belongs in the visual-validation rule which is
  domain-expert-checked, not automated.
- **No Vue AST parsing.** Regex on the diff suffices for every rule I'm confident
  about. If a rule turns out to need AST parsing, defer — the marginal rule isn't
  worth the extra machinery.
- **No frontend rules that already have a ratchet.** Redundant with the ratchet;
  the eval would score noncompliant for code that failed the build already.
- **No screenshot-anchored prompts.** CLI eval can't render an image. Hand-translate
  where feasible; skip where the screenshot IS the whole ask.

## Deliverable phases

- **P1 — pilot.** Author 3 prompts, one per rule where I have a clean scoring recipe:
  - `frontend-inlinenote.md` (primitive catalog — grounded in `conv-01cdc6c5`)
  - `frontend-copy-canonical.md` (UI copy — grounded in `fanout-510c1acd`)
  - `frontend-coalesce.md` (continuous control coalescing — grounded in
    `docs/UI.md → Continuous controls`)
  Run each once with `--arm with`; trace-inspect. Iterate the regexes until each
  compliant / noncompliant / tool_order fires against a real diff.
- **P2 — signal check.** If P1's compliant runs all pass and noncompliant runs all
  fail (i.e. the graders track the rule), run each at N=3 both arms. First real Δ
  on the frontend surface.
- **P3 — batch to catalog.** If P2's Δs are real, fold prompts into the standard
  `pixi run claude-md-eval` catalog. Rollup grows a "frontend rules" section (via
  a prefix filter on prompt_ids) or just lists them alphabetically alongside the
  backend prompts — decide at render time.
- **P4 — persistence rule.** Add `frontend-persist-viewstate.md` once we have the
  authoring pattern down and can regex around false-positive `ref()` uses.

## Cost model

| Phase | Prompts | Runs | Arms | Est. cost |
|---|---:|---:|---:|---:|
| P1 pilot | 3 | 1 | with only | \$1.50 |
| P2 signal check | 3 | 3 | both | \$18 |
| P3 batch (with backend) | 15 total | 3 | with only | \$8 |
| P3 ablation | 15 total | 3 | both | \$45 |
| P4 persistence add | +1 | 3 | both | \$6 |

**Total to close the loop through P3: ~\$70. Total through P4: ~\$76.** Not cheap;
gate on P1 producing a clean signal before proceeding.

## Open questions

- **`.vue` files and `_capture_diff`.** The `git diff --cached HEAD` already captures
  new `.vue` files the same as `.py` files; nothing to change. Confirmed by inspecting
  the current runner.
- **UI copy canonical detection.** Some canonical strings are prefixed with a namespace
  (`CLAUDE_TERMINAL.action`); regex needs to be careful not to match `CLAUDE_TERMINAL.action = 'Set up my terminal'` in the const file as an anti hit if the agent doesn't touch it (additions-only filter handles this).
- **Cross-file convention consistency.** A rule like "if you moved a button, update every doc that names the old location" is fanout-shaped, which the fanout audit already covers on real PRs — not this eval's job. Skip.
- **Do we test `frontend/CLAUDE.md` itself being loaded?** Analogous to the root canary
  probe. Currently only the root `CLAUDE.md` canary exists. If a P1 pilot fails
  everywhere with tool_order=FAIL on WITH-arm, that's the same failure mode the root
  canary catches — add a `frontend-canary.md` prompt then.

## Follow-ups the plan doc updates

- [`docs/todo/README.md`](README.md) — row for this plan added in the same change.
- [`CLAUDE_MD_EVAL_PLAN.md`](CLAUDE_MD_EVAL_PLAN.md) → *Indirect tier (2026-09-28)*
  → the "Deferred (Sonnet-flagged, not yet)" paragraph now points here rather than
  describing the frontend surface inline.
