> **ARCHIVED — audit findings, not authoritative.** Point-in-time read of drift-prevention mechanisms in this repo as of `main @ 580f87df` (2026-09-26). Answers the audit prompt in [`drift_prevention_mechanism_prompt.md`](drift_prevention_mechanism_prompt.md). Not a spec; nothing here should be acted on without a follow-up plan in `docs/todo/` that turns findings into decisions.

# Drift-Prevention Mechanism Audit — findings

Model: Opus 4.7. Read-only pass. Scope: whether the existing mechanisms — `INVENTORY.md`, the discovery-first `CLAUDE.md` rule, `docs/MAINTAINABILITY.md`, the CSS/UI/backend ratchets — are actually preventing divergent reimplementation or only cataloguing it after the fact.

Ground truth from [`code.claude.com/docs/en/best-practices`](https://code.claude.com/docs/en/best-practices) (fetched this pass):

- **Advisory vs deterministic:** *"[Hooks] run scripts automatically at specific points in Claude's workflow. Unlike CLAUDE.md instructions which are advisory, hooks are deterministic and guarantee the action happens."* Under the "Set up hooks" tip: *"Use hooks for actions that must happen every time with zero exceptions."*
- **Over-specified CLAUDE.md:** *"The over-specified CLAUDE.md. If your CLAUDE.md is too long, Claude ignores half of it because important rules get lost in the noise. Fix: Ruthlessly prune. If Claude already does something correctly without the instruction, delete it or convert it to a hook."*
- **Adversarial review:** *"A reviewer running in a fresh subagent context sees only the diff and the criteria you give it, not the reasoning that produced the change, so it evaluates the result on its own terms."*

---

## Item 1 — Enforcement level of each mechanism

| Mechanism | Enforcement level | Evidence |
|---|---|---|
| `INVENTORY.md` + `docs/inventory/*.md` | **Advisory only** — no hook, no CI check that reads it. | `find /home/dominik/cc-workspace/cecelia/cecelia-drift-archive -maxdepth 3 -name settings*.json` → **zero hits.** `find . -name '.claude*'` → **zero hits.** `~/.claude/settings.json` and `~/.claude/settings.local.json` — grep for `PreToolUse|PostToolUse|SessionStart|Stop|UserPromptSubmit` → **zero hits.** |
| Discovery-first rule in root `CLAUDE.md` | **Advisory only** — loads at session start via CLAUDE.md convention, but there is no gate that verifies it was consulted before a write. | Root `CLAUDE.md:180-197` — the "Before implementing anything — mandatory discovery step" block. No hook backs it. |
| `docs/MAINTAINABILITY.md` | **Mixed** — the doc itself is advisory; a subset of its rules are backed by convention tests that run in the test suite (post-hoc, not write-time). | See item 2. Concrete tests below. |
| Typed-params ratchet | **Enforced** at `pixi run test-pkg` time — `app/test/suite/ratchets.jl` `@testset "typed params ratchet — _run_task reads params through parse_*_params"` (loaded from `app/test/suite.jl:503`). Post-hoc. |
| Cohort-metrics ratchet | **Enforced** at `pixi run test-pkg` time — same file, `@testset "cohort-metrics ratchet"`. Post-hoc. |
| Zarr-access ratchets | **Enforced** — `python/cecelia/tests/test_zarr_access_convention.py` (Python side) + `zarr-access ratchet` testset in `app/test/suite.jl` (Julia side). Post-hoc. |
| Store-compressor / store-staging | **Enforced** — `python/cecelia/tests/test_store_compressor_convention.py`, `python/cecelia/tests/test_store_staging_convention.py`. Post-hoc. |
| Streaming (dask-in-runners) | **Enforced** — `python/cecelia/tests/test_streaming_convention.py`. Post-hoc. |
| Task-JSON section rule (>6 params) | **Enforced** — `python/cecelia/tests/test_task_json_convention.py`. Post-hoc. |
| Doc-index / doc-pointer conventions | **Enforced** — `python/cecelia/tests/test_doc_index_convention.py`, `python/cecelia/tests/test_doc_pointer_convention.py`. Post-hoc. |
| CSS re-implemented-scenario detector | **Enforced** — `frontend/src/utils/cssScenarios.test.ts` (Vitest); the detector code is in `cssScenarios.ts` and explicitly says: *"That rule was only ever enforced by review — and review across fresh context windows is exactly what kept failing (see docs/todo/UX_PRIMITIVES_PLAN.md)."* Post-hoc. |
| Continuous-controls scheduler discipline | **Enforced** — `frontend/src/utils/continuousControls.test.ts`. Post-hoc. |
| Channel-selection → `channel_indices` | **Enforced** — `channelSelection params resolve through channel_indices` testset (referenced in `app/CLAUDE.md:65-66`). Post-hoc. |
| Two mandatory frontend lookups (`ui/PRIMITIVES.md`, `ui/COPY.md`) | **Advisory** — `frontend/CLAUDE.md:8-17` says "enforced by tests, so skipping them fails the build," but those tests fire post-hoc (Vitest) and only cover the *outputs* they can pattern-match (`cssScenarios`, tooltip coverage). Whether the agent read the doc before writing is not checked. |

**Verdict on item 1:** Anthropic's advisory/deterministic distinction lines up bluntly. Every mechanism whose entry point is a Markdown file is advisory. The ratchets are deterministic but they are all **post-hoc test-suite checks**, not write-time gates — and none of them target the reuse-vs-hand-roll axis directly. The nearest thing is the CSS scenario detector, which is exactly what its own header comment says it is: an admission that review failed and detection was needed downstream.

---

## Item 2 — Where in the session lifecycle each mechanism fires

| Mechanism | Lifecycle point | Prevention or detection? |
|---|---|---|
| Root `CLAUDE.md`, `frontend/CLAUDE.md`, `app/CLAUDE.md` | Session start (auto-loaded); nested files load when a session touches that dir | Prevention — but only insofar as the agent reads and applies them |
| `INVENTORY.md` + `docs/inventory/*.md` | Session start (linked from root `CLAUDE.md`); agent must think to grep | Prevention — but requires an explicit action every time |
| `docs/MAINTAINABILITY.md` | On demand (not auto-loaded into every session); referenced from root `CLAUDE.md` | Prevention on paper, detection in practice — see item 7 |
| All convention tests + Julia ratchets | Post-hoc — at `pixi run test-*` time | **Detection.** The code already exists on disk when the check runs. |
| PR review by Dominik | Post-hoc — at PR time | **Detection.** |
| The claim in `docs/MAINTAINABILITY.md:3-4` that this is "the check that runs *before* new code lands" | Post-hoc, in practice | **Detection.** The doc is aspirational about its own lifecycle. |

**One thing that IS session-start deterministic:** the nested-CLAUDE.md loading pattern (`frontend/CLAUDE.md` loads only when a session touches `frontend/`). That is a lifecycle-shaping choice done well — see item 5.

**Flagged:** every ratchet in the repo is post-hoc. There is no write-time drift-prevention gate in this repo, period. The mechanisms called "prevention" in the CLAUDE.md and MAINTAINABILITY.md language are prevention-*by-persuasion*, not prevention-*by-mechanism*.

---

## Item 3 — Real PR-trail failures × proposed layers

Six merged PRs from the last six weeks whose title/body explicitly names hand-rolling a component or path that already existed. `gh pr list --state merged --limit 300` filtered on `consolidat|canonical|duplicate|dedupe|drift|reimplement|unify` (Python regex over titles).

| # | PR | What existed | What was reimplemented | Caught by |
|---|---|---|---|---|
| A | [#1215](https://github.com/schienstockd/cecelia/pull/1215) "Unify viewer capture reshow with the module-page pathway" | Module pages mounted `CaptureViewSurface` for `CaptureEnvelope` rehydration | Viewer used a read-only `MarksOverlay` + `.vw-marks-chip` — a second rehydration path with no pencil affordance | Dominik noticed a specific capture had no re-annotate button |
| B | [#1079](https://github.com/schienstockd/cecelia/pull/1079) "blackboard list gets the canonical draggable divider" | `usePanelResize` composable (used by TasksModule) | Blackboard shipped fixed `width: 22rem` — no drag, no persistence | Follow-up PR after user feedback |
| C | [#1074](https://github.com/schienstockd/cecelia/pull/1074) "blackboard page uses canonical primitives" | `SelectionTable`, `ConfirmDeleteButton`, the `ChainModule` shell pattern | Blackboard hand-rolled `<ul>` list + bespoke layout + non-canonical buttons | User feedback on preceding PR #1070; body explicitly says *"I hand-rolled a list + bespoke layout + non-canonical buttons"* |
| D | [#1076](https://github.com/schienstockd/cecelia/pull/1076) "shared spine for cellCards / motifCards / hmmCards" | (nothing — this is the extraction after the fact) | Three parallel card implementations (cellCards, motifCards, hmmCards) with duplicated filmstrip render | Planned refactor after three variants shipped |
| E | [#1085](https://github.com/schienstockd/cecelia/pull/1085) "unify Share entrypoints" | (nothing — extraction PR) | `ViewerPanel.openShare()` and canvas Share had separate `beginShare` handshakes | Planned refactor |
| F | [#1151](https://github.com/schienstockd/cecelia/pull/1151) "extract FloatingCanvasHost + useFloatingCanvas" | (nothing yet — three hand-rolls) | `SummaryCanvas`, `GatingPlots`, `ClusterPlots` each hand-rolled the floating canvas wrapper | The Share stuck-annotator fix #1101 landed on ONE hand-roll only, silently leaving the other two broken — the divergence *caused a bug* |
| G | [#1013](https://github.com/schienstockd/cecelia/pull/1013) "unify route-body parsing through a typed boundary" | (nothing yet) | 35 setters across 15 files inlined `JSON3.read(body_bytes)` with drifted try/catch shapes — some returned 500 on malformed JSON | Backend-drift sweep; found by scan, not by a specific bug report |

**Whether the canonical thing was in the inventory at the time of the drift** (`git blame` on `docs/inventory/FRONTEND.md`):

- `usePanelResize` — inventory entry added **2026-08-20** (commit `dd1651b23`), drift PR #1079 merged **2026-09-20**. Inventory was current for 31 days before the miss.
- `SelectionTable`, `ConfirmDeleteButton` — same commit, same 31-day gap before #1074's miss.
- `CaptureViewSurface` — inventory entry added **2026-09-21** (commit `ae0a14aa6`), drift PR #1215 merged **2026-09-25**. The drift-writing PR ran *without* the inventory entry (`ae0a14aa6` was authored after the drift was in the tree); once the entry landed, the miss was recognised within days.
- `FloatingCanvasHost` — post-extraction entry, so the F case predates the entry by definition.
- `_parse_body` — no inventory entry at drift time (canonical helper didn't exist yet); the boundary trigger in `docs/MAINTAINABILITY.md` (`Dict{String,Any}` crossing a boundary needs a typed constructor) did exist and would have flagged pattern G.

### Per-instance × per-layer verdict

Legend: **Y** = would have caught it; **N** = would not have caught it; **?** = maybe, depending on match quality.

| # | Write-time PreToolUse gate against INVENTORY.md | SessionStart injection of INVENTORY.md digest | In-session adversarial review subagent |
|---|---|---|---|
| A (#1215) | **N** at the time of the drift write (inventory entry didn't exist); **Y** for a re-attempt today | ? — the reshow-path phrase `MarksOverlay` reads like a legitimate rendering surface, not obviously a duplicate of `CaptureViewSurface`; injection makes both names visible but doesn't compel a comparison | **Y** — a fresh-context reviewer told "two rehydration paths for `CaptureEnvelope`, is that a duplicate?" would find it in the diff |
| B (#1079) | **Y** — `usePanelResize` is in inventory with an exact name match; `.bb-list { width: 22rem }` on a resizable pane is a pattern a naive regex against `docs/inventory/FRONTEND.md` entries could flag | **Y** likely — an agent with `usePanelResize` and its consumers (`TasksModule`) in the session-start context has the pattern in mind | **Y** — reviewer told "does the blackboard list share TasksModule's resize behaviour?" finds it |
| C (#1074) | **Y** for `SelectionTable` (inventory names it), **Y** for `ConfirmDeleteButton` (inventory names it); a `<ul>` on a page that renders a selectable entry list is a strong signal | **Y** likely | **Y** |
| D (#1076) | **N** for the first variant (cellCards) — nothing to match against yet; **?** for the second variant (motifCards) — depends on whether the first variant was itself indexed as canonical; **Y** for the third (hmmCards) if the pattern-shape entry got added after variant 2 | **?** — same problem: an inventory that grows one entry per variant doesn't have the aggregation the gate needs | **Y** for variants 2 and 3 — a reviewer with variants 1 and 2 loaded and told "is this a family?" catches it |
| E (#1085) | **N** for the first Share button, **Y** for the second — same shape as D | **N** for the first, **Y** for the second | **Y** for the second |
| F (#1151) | **?** — inventory entries are per-component; the drift is a wrapper-shape recurrence across three files, which no name-match gate reliably catches | **?** | **Y** — a reviewer given the shell shape of `SummaryCanvas` and asked "is the caller re-implementing this?" catches it |
| G (#1013) | **N** — no inventory entry for a helper that didn't exist; the pattern is a `MAINTAINABILITY.md` trigger, not an inventory hit | **?** — if the MAINTAINABILITY.md digest names `Dict{String,Any}` crossing a boundary, an agent might notice at write time | **Y** — a reviewer against the MAINTAINABILITY.md checklist finds every raw `JSON3.read(body_bytes)` in the diff |

**What this table says:**

- The write-time inventory-match gate is decisive only when a **named canonical entity already exists in the inventory**. That's cases B, C, and a re-attempt of A. It cannot help with "the first variant of what will become a family" (D, E, F) or with pattern-shape drift that doesn't reduce to a single name (G).
- The session-start digest is a weaker cousin of the gate — same coverage window, but relies on the agent still choosing to look.
- The adversarial-review subagent is the only layer that catches **all seven** cases, because it evaluates the diff against a stated criterion rather than trying to match a name.

This is a real finding: the proposed three-layer harness ranks in reverse order of the "cheap → expensive" intuition. The cheapest layer (write-time regex match) has the narrowest coverage; the expensive layer (a whole subagent per change) has the broadest.

---

## Item 4 — INVENTORY.md staleness

The **absence** of an inventory entry was the causal factor in exactly **one** of the seven cases (A, and only for the 4-day window before `CaptureViewSurface` got indexed on 2026-09-21). Every other named canonical thing that was hand-rolled around was already in the inventory when the miss happened — the miss was the agent not consulting it, not the doc being wrong.

`INVENTORY.md:43` last-audited stamp reads `2026-07-16 (against main @ c1ce165)`. That's 10 weeks stale by the terms of the doc itself. But the per-area files under `docs/inventory/*.md` are updated in-line — `docs/inventory/FRONTEND.md` was touched 2026-09-21 (`ae0a14aa6`), and PR #1074's body says *"Rewritten against the frontend inventory"* — so the inventory update discipline is holding on the leaf files. The stale stamp on the parent index overstates the staleness of the actual content.

**Verdict on item 4:** Staleness is real but not the primary bottleneck. `INVENTORY.md:43` should get its date bumped (or the stamp removed) but that would not have prevented cases B–G.

---

## Item 5 — CLAUDE.md size and signal-to-noise

Line counts on the three CLAUDE.md files (`wc -l`):

- Root `CLAUDE.md`: **306 lines** (~14 KB by rough estimate).
- `frontend/CLAUDE.md`: **47 lines** — deliberately terse, area-scoped.
- `app/CLAUDE.md`: **158 lines** — area-scoped.

The root file is dense but not obviously "over-specified" in the Anthropic sense. Its structure is a doc index (a table of ~20 docs, one line each) plus five rule sections. What it does well:

- **Nested CLAUDE.md loading is used correctly.** The two mandatory frontend lookups sit in `frontend/CLAUDE.md`, not in root — a Julia-only session doesn't pay for them (`frontend/CLAUDE.md:3-6` states the reason explicitly). Same for `app/CLAUDE.md`. This directly follows Anthropic's own advice on keeping instructions relevant per-session.
- **The discovery-first rule is not buried.** It has its own H2 header (`## Before implementing anything — mandatory discovery step`) at line ~180, is 20 lines long with five numbered steps, and does not compete with a wall of unrelated rules — the surrounding sections are the four other cross-cutting rules (H5AD, zarr, run_py, Windows).
- **Redundancy the guide would flag has been pruned.** The rule check *"if Claude already does something correctly without the instruction, delete it"* — `wc -l CLAUDE.md` on the git log shows the file was actively slimmed as recently as 2026-08-20 (commit `dd1651b23` = *"docs: cut per-session token cost of the agent-facing docs"*). This is not an untouched drift-y file.

What it does less well:

- The doc-index table is 20+ rows and reads as a wall of paths. It's useful when you know what you're touching but for a fresh session it's a lot of catalog before the actionable rules.
- The "Watch for divergent re-implementation" blockquote at line ~155 says the right thing but sits *between* the doc-index and the discovery-first rule — it names the problem twice (once here, once as the H2 below it). One statement of the rule with a `⇒` to the discovery step would be tighter.

**Verdict on item 5:** Not the primary cause. The over-specified-CLAUDE.md failure mode is a real risk in principle but doesn't fit this repo today. The nested-loading discipline is a genuine strength; the ~300-line root is at the upper edge of "readable" and probably fine. If the doc has to grow, the growth should go into a new `frontend/`- or `app/`-level nested file, not the root.

---

## Item 6 — Session-length effects

**Cannot be verified from this side of the fence.** PR bodies and commit messages don't record which turn of a session a change was made on, and the Claude Code session transcripts under `~/.claude/projects/-home-dominik-cc-workspace-cecelia*` are not indexed against the resulting diffs in a way this pass can join.

**Weak circumstantial evidence:** PRs C (#1074) and F (#1151) both call out that the drift happened in a *branch that was already open on a related feature* — the blackboard drift landed under the Bidir context work, the floating-canvas drift landed under the Share rollout. That's consistent with the "already in flight, adding one more file" pattern, which is a proxy for context-length dilution. But it's a proxy, not a measurement.

**Skipped as unmeasurable in this pass.** A follow-up would need a hook that logs `(session-uuid, turn-index, file-path)` for every Write/Edit so the "when in a session did this drift occur?" question becomes answerable. That's a data-collection change, not an audit finding.

---

## Item 7 — MAINTAINABILITY.md checklist sort — the core new question

`docs/MAINTAINABILITY.md` has a "Checklist before you commit" (lines 285–294) plus the substantive rules in the body. Sort:

| Rule (source) | Bucket | Justification |
|---|---|---|
| `# invariant:` prefix on load-bearing single-line rules (§Comment shape) | **Mechanically hookable** | Regex over source lines: is a comment inside a function body a "rule" (contains `never`, `must`, `exactly once`, `terminal`, `race`, etc.) and does the prefix precede it? Grep-shaped. Low precision on its own, but composable with the protected-comments header check. |
| No cross-repo references in comments (§Comment shape) | **Mechanically hookable** | Regex over `#` and `//` comments for the specific tokens (`R port`, `depmixS4`, `caTools::`, `DescTools::`, `celltrackR`, plus a filename-pattern list). The exception ("bridging code" whole files) can be a per-file allow-list. |
| No dataset IDs / dated authorship / quoted user reports in comments (§Comment shape) | **Mechanically hookable** | Same regex family: six-character uid pattern (`[A-Za-z0-9]{6}` in a comment), ISO dates in a comment (`\d{4}-\d{2}-\d{2}`), `reported:` in a comment. The protected exception (hardcoded-constant provenance) is inline via `pattern 3` marker or a per-file allow-list. |
| No commit-SHA references (`commit 860da24b`) in source (§Comment shape) | **Mechanically hookable** | Regex for `commit [0-9a-f]{7,40}\b` in `#`/`//` comments. |
| Protected-comments header + no-trim discipline (§Correctness-critical comments) | **Hookable with a cheap model call** | Regex detects the header. Enforcing "no trim without re-reading" cannot be done at write time by a rule; a diff-scope check that flags "content deleted from a `# concurrency-critical` file" is a cheap-model classification. |
| Typed constructor at any `Dict{String,Any}` boundary (§Cross-module trigger 1) | **Hookable with a cheap model call** | The boundary detection is regex-shaped (function signature contains `AbstractDict`/`Dict{String,Any}` at a named seam — frontend↔Julia, Julia↔Python, API↔handler). "Would a typed struct fit here?" is a judgment call, but a small classifier can flag *plausible* candidates and defer to human decision. Note: the already-enforced typed-params ratchet in `app/test/suite/ratchets.jl` **is** the write-time version of this rule for the `_run_task`/`parse_*_params` case — the mechanism exists, just not extended. |
| Enforce callee's invariant at the source, not defensively at call sites (§Cross-module trigger 2) | **Needs judgment** | Requires understanding whether a bit of defensive code is "compensating for a callee's quirk" or is legitimate input validation. Fresh-model call would misfire often. Adversarial review only. |
| Structural enforcement / runtime assertion for order-dependent calls (§Cross-module trigger 3) | **Needs judgment** | Same shape — a small model cannot tell "correct today by accident" from "correct by construction" without global context. |
| Typed params — every `_run_task` reads through `parse_<task>_params` (§) | **Mechanically hookable — already hooked** | Enforced by the existing typed-params ratchet. Post-hoc rather than write-time, but exists. |
| Image / OME-ZARR access through `zarr_utils` (§) | **Mechanically hookable — already hooked** | Enforced by `test_zarr_access_convention.py` + the `zarr-access ratchet` in `app/test/suite.jl`. Post-hoc. |
| Cohort-comparable metrics registered or exempted (§) | **Mechanically hookable — already hooked** | Enforced by `cohort-metrics ratchet` in `app/test/suite.jl`. Post-hoc. |
| Task JSON — collapse advanced params behind a `type: "section"` (§) | **Mechanically hookable — already hooked** | Enforced by `test_task_json_convention.py`. Post-hoc. |
| Enums for state machines — `@enum` + typed setter over `Symbol`/`String` (§) | **Hookable with a cheap model call** | Regex-shaped for the *field-declaration* side (a `::Symbol` or `::String` field annotated with `# one of X, Y, Z`). The "does this value have a small closed set?" judgment is close to trivial for a small classifier when the comment is a hint. |
| Sum types over `Union{Nothing, T}` discriminants (§) | **Needs judgment** | The pattern is easy to detect (grep `Union{Nothing,`); *whether the union is a discriminant vs. a legitimate optional* needs code understanding. Adversarial review only. |
| ~200-line / third-responsibility split rule (§File responsibility) | **Hookable with a cheap model call** | Line-count is trivial (`wc -l`); "does this file carry N of {run orchestration, QC scoring, param translation, cohort metric, composite dispatch}?" is a small-model classification against the fixed responsibility list. The lock-owned-monolith carve-out (scheduler.jl) needs a per-file exemption. |
| "For now this just handles X" red flag (§) | **Mechanically hookable** | Regex for the phrase family (`for now`, `just handles`, `just for now`, `TODO: handle other`). Genuinely cheap. |
| Frontend UI-copy rules (§Frontend specifics) | **Already hooked** | Enforced by tests referenced from `frontend/CLAUDE.md` and `frontend/src/utils/cssScenarios.test.ts`. Post-hoc. |

**Counting:** 8 items mechanically hookable, 5 items hookable with a cheap model call, 2 items requiring adversarial-review judgment, plus 5 items already covered by an existing ratchet (mostly post-hoc). The judgment-only items are the smallest bucket — that is a design-relevant finding.

### Has MAINTAINABILITY.md's own catalogued pattern recurred after documentation?

`docs/MAINTAINABILITY.md` was created **2026-09-15** (commit `9da86408`). The six-instance `@enum`/state-machine pattern was catalogued around Phase-4. Status today:

- All six named instances have been converted to `@enum` + typed setter: `ChainScope` / `ChainBarrierPolicy` / `ChainNodeStatus` (`app/src/tasks/chain/types.jl`), `TaskStatus` (`app/src/tasks/scheduler/task_record.jl`), `ImageStatus` (`app/src/model/image.jl`), `PopType` / `BoolMembership` / `FilterFun` (`app/src/gating/popmanager/types.jl`). **That is a real win.**
- However, `git log --diff-filter=A --since='2026-09-15' -- 'app/src/**/*.jl' 'api/src/**/*.jl'` surfaces two files added AFTER the doc that reintroduced the stringly-typed shape while the canonical `@enum` was already sitting one directory over:
  - `app/src/gating/popmanager/pop_df.jl:268` — `pop_type::String # the pop_type it is derived under (e.g. "live")`, when `@enum PopType` already exists at `app/src/gating/popmanager/types.jl:62`.
  - `app/src/tasks/chain/api.jl:205` — `scope::String # "" → inherit`, when `@enum ChainScope` already exists at `app/src/tasks/chain/types.jl:7`.

Both may be intentional (wire-format string on a JSON boundary) rather than sloppy — a follow-up would need to confirm — but the shape *is* what the doc names as the pattern to avoid, and the doc did not prevent it.

**Verdict on item 7:** MAINTAINABILITY.md is a mixed picture. The concrete state-machine sweep worked; the ongoing prevention of the same shape did not. Most of the checklist is either already hooked or hookable — 15 of 17 items sit outside the "needs judgment" bucket. The two items that genuinely need judgment (call-site vs source-side invariant enforcement; sum type vs optional Union) are exactly the sort of thing an adversarial-review subagent handles well and a regex handles badly.

---

## Root-cause verdict

**Primary root cause: no enforcement.**

The evidence: every mechanism that is documented as "the check before code lands" is in fact a Markdown file that an agent may or may not consult. The mechanisms that ARE enforced are all post-hoc test-suite checks that catch specific *shapes* (raw zarr access, missing typed-params, missing cohort registration, CSS scenario duplication) — none of them ask *"does this new component reuse the canonical one?"* at write time. The nearest thing to a write-time drift check in this repo is a `Vitest` file (`cssScenarios.test.ts`) that runs after the code is on disk and whose own header comment says it exists because review across fresh context windows kept failing.

**Rank of contributing causes** (weakest to strongest):

1. **Genuinely-judgment-dependent** — smallest bucket by item 7's sort. Not the story.
2. **Over-specified advisory documents** — the root CLAUDE.md is edge-of-comfort at 306 lines but not obviously the failure mode; the nested-loading discipline is a real strength.
3. **Stale inventory** — real for `INVENTORY.md:43` last-audited stamp; not causal for six of the seven PR-trail cases.
4. **No enforcement** — primary. Six of seven cases involve a canonical helper that was known, documented in inventory or CLAUDE.md, and hand-rolled anyway. Anthropic's own docs frame the situation exactly: "hooks are deterministic and guarantee the action happens" vs advisory instructions that will sometimes be skipped.

This matches the prompt's most stringent test: "A well-written standard that nothing enforces has the same enforcement profile as no standard at all." That is the state today.

---

## What the design should carry from item 3's per-instance table

- **Write-time PreToolUse against INVENTORY.md is worth building** but its coverage window is narrower than the pitch. It catches cases B, C, and re-attempts of A. It does not catch the first variant of a new family (D, E, F) or pattern-shape drift (G).
- **SessionStart injection is redundant with a good write-time gate.** If the gate blocks the write, the injection is unnecessary; if the gate would miss, the injection would miss the same way (both key on name/entry match). Sequence it in only if the write-time gate is not built.
- **In-session adversarial review is the highest-yield layer** — it caught all seven cases on paper. Cost is real (one subagent per change) but the coverage is qualitatively different. This is the layer to build first if only one gets built.
- **Fold MAINTAINABILITY.md's mechanical items (item 7) into ratchets** as post-hoc detectors, sharing shape with the existing typed-params / zarr-access / cohort-metrics ratchets. That is a smaller, incremental change and it's already been done for a subset.
- **The two judgment-only MAINTAINABILITY.md items** (call-site vs source-side, sum type vs optional) are natural targets for the adversarial review — they are the reason to build it at all rather than a fifth ratchet.

---

## Reference-repo takeaways

- **Sorbet gradual-typing ratchet** — no change to design. The baseline-may-shrink-must-not-grow pattern is already how the typed-params and cohort-metrics ratchets work (`app/test/suite/ratchets.jl` — `TYPED_PARAMS_MIGRATION_BASELINE`), and `cssScenarios.ts` describes the same intent for the frontend side.
- **0xwilliamortiz/ratchet** — inaccessible (`https://github.com/0xwilliamortiz/ratchet` → HTTP 404 this pass). Cannot evaluate.
- **leonkacowicz/ratchet** — not fetched. Prompt notes it as "structural-metric snapshot + CI regression gate," which is post-hoc, so it maps onto the existing ratchet layer, not the write-time gate. No change to design.
- **mehmethk88-dot/eval-gate-ratchet** — **one design-relevant point.** The tool fingerprints gate checks and flags "structural weakening" (removed assertions, thresholds becoming more permissive, inverted guards). Our own ratchets carry `_BASELINE` sets that a session CAN edit down — an agent that shrinks a baseline instead of fixing the underlying code has weakened the check. A meta-ratchet that fingerprints the baseline arrays themselves and flags shrinkage without a paired code-side fix would harden this. Small change to the design: don't just build layers; add a fingerprint check on the ratchets themselves.
- **praveenvijayan/Ratchet** — cited in the prompt as "PreToolUse/skill-based hook wiring for Claude Code specifically." On inspection this is a GitHub-native CD framework with a `GATES.md` config for acceptance criteria and PR-time verification, not a per-write inventory check. Still useful as a reference for PR-time gate composition, but it does not resolve the write-time-gate design question — that would need its own build.
- **PyPI drift-analyzer** — not fetched. Prompt notes it as post-hoc detection with a baseline+ratchet CI mode; same shape as existing ratchets, no change to design.

---

## Reservations

- **Item 6 could not be answered from this side of the tools available in this pass.** Session-turn provenance for a diff is not indexed. If it matters, that needs a hook, which is itself the thing being audited — so the answer will only exist after some enforcement work is done.
- **The PR-trail sample is 7 cases, drawn from a keyword filter over titles.** A wider sample (unfiltered, or filtered on review-comment text rather than title) might change the per-layer coverage rates. I did not run that wider sample.
- **The item-7 checklist sort involves judgment calls per row.** Two people could sort several rows differently — in particular, the boundary between "hookable with a cheap model call" and "needs judgment" is not sharp. The 15-of-17-hookable count is the important claim; the exact per-row bucket is less so.
- **I did not attempt to measure the token cost of the proposed layers** (particularly the in-session adversarial-review subagent, which if run per change would add a subagent-per-diff to every agent session). That's a design-viability question — the audit's conclusion that this layer is highest-coverage is separate from whether it's affordable to run continuously.
- **Two reference repos were not fetched** (`leonkacowicz/ratchet`, PyPI `drift-analyzer`) and one 404'd (`0xwilliamortiz/ratchet`). The prompt characterisations were used at face value for those.
- **The two post-doc `@enum` misses may be intentional** (JSON wire format vs internal state); I flagged them by shape but did not read the calling context deeply enough to be certain each is a violation vs a legitimate boundary string. A follow-up should check.
