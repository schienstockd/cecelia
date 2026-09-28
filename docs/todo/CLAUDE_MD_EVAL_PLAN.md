# CLAUDE.md compliance eval — plan

**Status:** P1 (single-prompt runner) + P2 driver + 9-of-10-prompt catalog **SHIPPED** on PR
#1264 (2026-09-27). One prompt (`discovery-first`) originally deferred pending tool-log
inspection — **built via a bespoke transcript-reader + `tool_order` grader** (2026-09-28
via #1273). **P2.5 rollup SHIPPED** (2026-09-28, #1273) — renders
[`docs/ai-assist/CLAUDE_MD_EVAL.md`](../ai-assist/CLAUDE_MD_EVAL.md) at the end of every pass
+ on demand via `pixi run claude-md-eval-rollup`. **Indirect tier on
`feat/indirect-eval-tier`** (2026-09-28, PR #1274): additions-only regex scoring, synthetic
session id, widened `tool_order` matcher (lists), reworked `discovery-first` prompt, one
indirect pilot (`crop-failure`) — see *Indirect tier (2026-09-28)* below. P3 (cron) still
unbuilt.

## Sonnet 2026-09-28 discipline additions

Following the abandoned plugin-eval port (PR #1272, closed — the `claude plugin eval` sandbox
doesn't load CLAUDE.md as system context, which invalidates the whole point of a compliance
eval; see the PR body for the trace and diagnosis), three selective backports to the bespoke
runner:

- **Transcript-reader for `tool_order` grader.** `scripts/claude_md_eval/transcript.py`
  parses `claude -p --output-format=stream-json --verbose` stdout, exposes ordered
  tool_calls list + `TranscriptSignals.tool_order_passes(before_tool, before_arg_match,
  after_tool)`. Prompts declare `tool_order_before_tool` / `tool_order_before_arg_match` /
  `tool_order_after_tool` in frontmatter; scorer combines with regex graders (compliant iff
  ALL declared graders pass).
- **CLAUDE.md ablation via worktree cleanup.** `run_prompt.py --arm {with,without}`;
  `_make_detached_worktree(arm=without)` strips every CLAUDE.md from the throwaway
  worktree before spawn. `pixi run claude-md-eval-ablation` fires suite twice + computes
  per-prompt Δ + emits `claude_md_eval_ablation` row. Same loading mechanism as production
  in both arms — Claude Code loads CLAUDE.md from cwd; we just make it absent in
  without-arm.
- **Cost + turns capture** from the same stream-json parse. Emitted as `cost_usd` +
  `turns` on every `claude_md_eval_run` row. Would have caught the "compliance costs 60%
  more than the bypass" pattern natively.

**Canary rule in CLAUDE.md** (Sonnet-suggested pre-flight). Made-up marker
(`# canary: CLAUDE.md loaded`) that only exists in CLAUDE.md prose. `canary` eval prompt
asks the agent to create `python/cecelia/analysis_scratch/canary_probe.py`; grader checks
for the marker. If a with-arm canary run scores noncompliant, CLAUDE.md isn't reaching
the agent and every other run in the same session is suspect. Run canary once before
paying for a full ablation pass.

**D12 discipline** (formerly "N≥3 before quoting Δ"): **N≥3 AND read at least one trace
per arm before quoting Δ anywhere.** N≥3 alone didn't catch the plugin-eval "CLAUDE.md
never loaded" class — trace inspection would have. The trace is the ground truth; the
scoring output is a derivative signal that can be right for the wrong reason (e.g. the
2026-09-28 h5ad-read Δ=1 claim — retracted, likely artefact).

Baseline behavioural datum: `h5ad-read` scores **3/3 compliant** at CLAUDE.md blob
`2f05fefc…` (via the bespoke runner in a real worktree — production loading semantics).
Full catalog rerun (`pixi run claude-md-eval`) will produce the first cross-rule number.

Follow-up from the enforcement-coverage work in PRs #1258 (recital SHA-anchoring) and #1263
(h5ad / utf-8 / Windows helpers / run_py ratchets). Written to be picked up cold by another
session.

## Indirect tier (2026-09-28)

Sonnet pushed back on scrapping indirect. The direct catalog names the guarded util
(`zarr_utils`, `label_props`, `run_py`) in the prompt, so a without-arm agent can just
grep the name — both arms score high, Δ shrinks by construction, and low Δ reads as
"CLAUDE.md doesn't matter" when it actually reads "the prompt gave away the answer."
Direct-only can't disprove that failure mode.

Adopted shape:

- **`crop-failure` indirect pilot** (`scripts/claude_md_eval/prompts/crop-failure.md`) —
  symptom-first bug report seeded by past init prompt `9fb138d2`. Agent must discover
  and use `zarr_utils` without being told. Graders: `compliant_signal = zarr_utils\.`,
  `anti_signal = bare zarr.open( / da.from_zarr / tifffile.imread`, `tool_order`
  (inventory-touching Grep/Read/Glob before Write/Edit/MultiEdit).
- **Reworked `discovery-first`** — the prior `next_multiple(x, n)` task was too trivial
  to plausibly need discovery, so both arms scored noncompliant and the rule wasn't
  actually being tested. Replaced with a tile-origin helper task where `zarr_utils`
  might genuinely already own the helper, forcing real inventory grep.
- **Widened `tool_order` matcher** — added plural `tool_order_before_tools` /
  `tool_order_after_tools` frontmatter keys that accept comma-separated alternatives.
  `TranscriptSignals.tool_order_passes` accepts str or list. Wanted for indirect
  because a real agent may Read `INVENTORY.md` instead of Grepping it and reach for
  Edit/MultiEdit instead of Write on an existing file — the discovery-first rule is
  about ordering, not tool identity. Singular form stays valid.

**Deferred (Sonnet-flagged, not yet):** widen indirect coverage to the frontend rule
surface (`frontend/CLAUDE.md` — primitive catalog, analysis-board registries,
`InlineNote`). That's where the fanout/convention findings keep flagging real drift
and the suite doesn't cover it at all — but it needs a different scorer than
regex-on-diff (a bug fix has many valid solutions across many files). Land after
the indirect pilot has produced a pass or two of usable data.

## Scoring artefacts fixed (2026-09-28)

- **Additions-only regex scoring.** `_regex_hits` runs the compliant/anti regexes only
  against `+`-prefixed lines of the diff (via `_additions_only`) — scoring the agent's
  CHOICE, not incidental text. Guards against two artefacts:
  - `arm=without` strips every CLAUDE.md before the spawn; without the filter, the
    compliant regex fires on the DELETED CLAUDE.md prose ("use `zarr_utils`…") and
    inflates `compliant_hits`. Surfaced by the `crop-failure` without-arm pilot on
    PR #1274 (`compliant_hits=7` on a diff with zero real refs).
  - Pasted anti-pattern snippets an agent minimally edits — retained lines don't
    appear as `+` and don't count against the agent.
- **CLAUDE.md excluded from `_capture_diff`.** Belt-and-suspenders to the above +
  keeps `diff_bytes` comparable between arms (without-arm otherwise reports 5–10×
  larger diffs from the CLAUDE.md deletion).
- **Synthetic session id.** `_ensure_eval_session()` sets `CLAUDE_CODE_SESSION_ID`
  to `eval-<uuid8>` if unset, so every row of one pass groups under one id. Previously
  every eval row emitted `sess=unknown` because pixi doesn't propagate the env var
  from the launching Claude Code shell.

## Log isolation — considered, dropped

Briefly implemented (`_harvest_sandbox_log` + per-spawn `CECELIA_EFFECTIVENESS_LOG`
sandbox + `source="eval"` filter in the audit rollup) to catch an accidental recital
run inside an eval-spawned `claude -p` session that would inflate the audit rollup
with runs that never touched shipped code. Trimmed same day — the trigger surface is
tiny: `ratchet_hit` fires from commits (eval prompts say "don't commit"), and every
recital emission originates from `pixi run recital` which we don't invoke from prompts.
Zero pollution measured in the log (2026-09-28 audit). ~50 LoC + 4 tests wasn't
earning its keep against a threat we control at prompt-authoring time. Revisit if we
ever ship an eval prompt that legitimately runs recital or commits.

## Goal

Behavioral signal for "does `CLAUDE.md` work?", rerunnable as the file changes. Replaces the
placeholder "500-line trigger" in [`CLAUDE_MD_ENFORCEMENT_PLAN.md`](CLAUDE_MD_ENFORCEMENT_PLAN.md)
→ *Re-audit triggers* with an actual measurement: give a fresh agent a task under a rule, watch
whether the agent's diff follows the rule.

Not a replacement for the ratchets. Ratchets catch anti-patterns *in shipped code*; this eval
measures whether agents *reach for the canonical path in the first place*. Different failure mode,
different signal.

## Non-negotiables (the "rerunnable across CLAUDE.md edits" requirement)

- **Fixed prompt catalog.** Prompts are versioned in the repo. Changing a prompt requires a
  plan-doc edit + a rebaselining note in the rollup ("prompt N reworded on <date>; pre/post
  comparisons are not apples-to-apples").
- **Deterministic scoring.** Convention tests do the compliance scoring. No LLM-judge, no manual
  triage. If a rule doesn't have a ratchet, tool-log inspection scores it (was the expected read
  performed?) — still deterministic.
- **Per-run environment snapshot.** Every eval row records the `CLAUDE.md` SHA it ran under +
  the model id + timestamp, so a trend line across file edits is legible in the rollup.
- **One command to rerun.** `pixi run claude-md-eval` runs the full catalog and updates the
  rollup. Anything less is friction that stops a re-run from happening.

## Prompt catalog (initial)

~10 prompts, one per major rule. Each is a `.md` file under
`scripts/claude_md_eval/prompts/<id>.md` with a frontmatter block:

```yaml
---
id: h5ad-read
rule: "H5AD / cell-data access — always go through the readers/writers"
scored_by: "test_h5ad_access_convention.py"
compliant_signal: "LabelPropsView("
anti_signal: "anndata.read_h5ad("
---
Task (you get this verbatim, no priming):

Add a small helper in `python/cecelia/analysis_scratch/read_track_speed.py` that reads
a `.h5ad` at a given path, filters to a given list of `label` ids, and returns the
`live.cell.speed` column as a `pandas.Series`. It'll be called from a notebook.
```

Initial 10 prompts, one per shipped ratchet + a few that don't have one:

1. **h5ad-read** → `LabelPropsView(...)` — scored by `test_h5ad_access_convention.py`
2. **h5ad-write** → `write_h5ad_atomic(...)` — scored by convention test + inspection
3. **zarr-read** → `zarr_utils.open_as_zarr(...)` — scored by `test_zarr_access_convention.py`
4. **zarr-write** → `zarr_utils.staged_store + store_compressor` — scored by
   `test_zarr_access_convention.py` + `test_store_compressor_convention.py`
5. **spawn-python** → `run_py(...)` — scored by `python spawn ratchet` in `ratchets.jl`
6. **kill-process-tree** → `_kill_tree(...)` — scored by `process-kill helpers ratchet`
7. **dir-size** → `_dir_bytes(...)` — scored by `dir-bytes ratchet`
8. **utf-8-json-write** → `encoding='utf-8'` — scored by `test_utf8_encoding_convention.py`
9. **discovery-first** → agent must grep `docs/inventory/` before writing new code — scored
   by tool-log inspection (was a `docs/inventory/*` Read/Grep issued before the first Write?)
10. **cite-algorithm** → new implementation of a published algorithm must include a paper/URL
    citation in a comment — scored by inspection (regex on the diff for `DOI` / `doi` /
    `arXiv` / `github.com` in a comment near the new function). No ratchet exists for this
    rule; the eval is the enforcement.

Each prompt is task-shaped, in a user's words, and does NOT mention CLAUDE.md, the helper name,
or the anti-pattern. If the agent hits the canonical helper without being told, that's the
signal.

## Runner

`scripts/claude_md_eval/run.py`:

1. Load prompt catalog from `scripts/claude_md_eval/prompts/*.md`.
2. Snapshot the environment: `CLAUDE.md` SHA (`git rev-parse HEAD:CLAUDE.md`), model id
   (`claude --version`), timestamp.
3. For each prompt, spawn `N` fresh agents (default `N=3`) via `claude -p`. Each spawn runs in
   its own `git worktree` off HEAD so the agents can Write freely without touching the primary
   checkout. The eval runner uses the same worktree pattern the rest of the codebase does — no
   temp dirs, no shared state.
4. Capture per spawn: the resulting diff, the full session transcript, the tool-log jsonl.
5. Score:
   - Apply diff in the worktree.
   - Run the relevant convention test(s). Pass ⇔ ratchet clean.
   - Grep the tool log for expected/forbidden reads (for prompts scored by inspection).
   - Grep the diff for `compliant_signal` / `anti_signal` where the rule has neither a
     ratchet nor a natural tool-log check.
6. Emit one `claude_md_eval_run` row per (prompt_id, run_number) to the effectiveness log via
   `cecelia.effectiveness.append_event`. Payload: `{"prompt_id", "rule", "outcome",
   "compliant_hits", "anti_hits", "worktree_sha", "model", "duration_s"}`. `outcome` is one of
   the closed vocabulary: `compliant` / `noncompliant` / `error`.
7. Clean up the eval worktrees (`git worktree remove`) unless `--keep-worktrees` is passed for
   debugging.

Failure recovery: if a spawn times out or the diff doesn't apply cleanly, emit an `error` row
with the reason. Don't crash the run — one bad prompt shouldn't kill the whole pass.

## Rollup

`scripts/claude_md_eval/rollup.py`:

Reads all `claude_md_eval_run` rows from the effectiveness log. Emits
`docs/ai-assist/CLAUDE_MD_EVAL.md`:

- **Latest run summary.** Pass rate per prompt, per rule. `N/N` per prompt, aggregated.
- **Trend.** Pass rate over time, annotated with `CLAUDE.md` SHA changes. A drop in compliance
  after a CLAUDE.md edit is the exact signal this whole design exists to surface.
- **Failing rules called out.** Any prompt with `<100%` pass rate in the latest run gets a
  paragraph — recent runs, recent CLAUDE.md edits touching that section, links to the
  transcripts.

Rollup runs at the end of every eval pass, so `pixi run claude-md-eval` produces the updated
markdown as its side effect.

## Effectiveness-log integration

Two new event types added to `cecelia.effectiveness.log.EVENT_TYPES`:
- `claude_md_eval_run` — one row per (prompt, run).
- `claude_md_eval_pass` — one row per full eval pass, summarising N runs × M prompts.

Both carry `commit` (the CLAUDE.md SHA that was evaluated — for cross-referencing with git
history) and `session` (the CLAUDE_SESSION_ID of the runner, so a scheduled cron pass shows up
distinctly from a manual one).

## Cost model

Rough per-pass cost: 10 prompts × 3 runs × ~2000 output tokens × sonnet rates
(~$3/M in, $15/M out) + input tokens for the prompt priming (~1000 in per spawn = 30k input) →
**~$1–2 per pass.** Cheap enough for weekly runs plus every material CLAUDE.md edit. If a rule
needs longer output (e.g. a whole new module), bump `max_tokens` on that prompt individually.

## Cadence

- **Manual** (`pixi run claude-md-eval`) before/after every material `CLAUDE.md` edit. Doc-doc
  tweaks don't warrant a run; adding/removing/rewording a rule does.
- **Weekly cron** — optional P3, uses the existing scheduling infrastructure. Not built now.

## Non-goals

- Not measuring the fanout audit / convention check reviewers themselves — those catch
  drift; this catches whether the *author* writes drift in the first place.
- Not measuring model quality broadly. This is a project-specific compliance signal.
- Not replacing the ratchets. Ratchets are always-on; the eval is a periodic sanity check on
  the file the ratchets exist for.
- Not an LLM-judged eval — every score is deterministic.

## Open questions (before build)

- **Model pin.** Sonnet (matches the reviewer subagents) or Opus (matches primary agent
  sessions)? Different failure modes on each. Cheap fix: run against both, report separately;
  ~2× cost stays under $5/pass.
- **Multi-turn.** Single-turn `claude -p` invocation, or a scripted multi-turn conversation
  that mimics a real session's back-and-forth? Multi-turn is more realistic but adds complexity
  and non-determinism. P1 = single-turn; upgrade to multi-turn only if the single-turn signal is
  weak.
- **Prompt-wording sensitivity.** Small phrasing changes may swing the outcome. Freeze one
  canonical wording per prompt; document that rewording resets the trend baseline for that
  prompt.
- **Scope of tool-log inspection.** Should the eval also check whether the agent read the
  relevant `docs/inventory/*.md` slice before writing? That's the whole discovery-first rule —
  worth adding as prompt 9's scoring signal explicitly.

## Deliverable phases

- **P1 SHIPPED** — prompt catalog (9 `.md` files, one deferred — see below), single-prompt
  runner, single-run scorer, manual invocation. Emits `claude_md_eval_run` + `_pass` rows.
- **P2 driver SHIPPED** — full runner (`pixi run claude-md-eval`) iterates the catalog, emits
  one `claude_md_eval_suite` summary per invocation, prints per-prompt table to stdout. Per-
  prompt failure records `error` verdict and doesn't stop the suite.
- **P2.5 rollup SHIPPED** (2026-09-28, this branch) — `scripts/claude_md_eval/rollup.py`
  renders [`docs/ai-assist/CLAUDE_MD_EVAL.md`](../ai-assist/CLAUDE_MD_EVAL.md): latest suite
  table, latest ablation delta (when an `_ablation` row exists), failing-rule callouts,
  multi-pass trend annotated by CLAUDE.md blob SHA. Auto-rendered at the end of every
  `pixi run claude-md-eval` pass; standalone regen via `pixi run claude-md-eval-rollup`.
  Not auto-committed — user reviews the diff.
- **P3** — DEFERRED. Optional cron/schedule for weekly `pixi run claude-md-eval`.

### One prompt still deferred: `discovery-first`

The rule ("before implementing anything, mandatory discovery step — grep
`docs/inventory/*.md` before writing new code") is compliance-visible only via **tool-log
inspection** (was a `docs/inventory/*` Read/Grep issued before the first Write?), not diff-
inspect. P1 scores via regex on the diff; the tool-log seam isn't wired. Ship the missing
prompt in the same PR that adds tool-log inspection to the scorer.

### `cite-algorithm` is scored loosely

The rule has no ratchet, so scoring is regex on the diff for citation-shaped tokens (DOI,
arXiv id, `10.NNNN/` prefix, `github.com/<owner>/<repo>` URL) in a comment. Anti-signal is a
sentinel that never matches — a "no citation added" run scores `noncompliant` via the
"neither matches" branch, which is the honest floor for a prose-only rule.

## Follow-ups the plan doc updates

- `docs/todo/CLAUDE_MD_ENFORCEMENT_PLAN.md` → *Re-audit triggers*: replace the "500-line
  trigger" (placeholder) with "compliance drop below <threshold> on the CLAUDE.md eval
  rollup." Threshold TBD once we have baseline data — probably "any rule that drops below
  80% over three consecutive passes."
- `docs/inventory/README.md` (if it exists) → cross-link this eval as the behavioral floor for
  inventory discipline.
