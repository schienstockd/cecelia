# CLAUDE.md compliance eval — plan

**Status:** P1 (single-prompt runner) + P2 driver + 9-of-10-prompt catalog **SHIPPED** on PR
#1264 (2026-09-27). One prompt (`discovery-first`) deferred pending tool-log-inspection
scaffolding. Rollup markdown (`docs/ai-assist/CLAUDE_MD_EVAL.md`) still unbuilt — deferred as
P2.5. P3 (cron) still unbuilt.

Baseline behavioural datum: `h5ad-read` scores **3/3 compliant** at CLAUDE.md blob
`2f05fefc…` — three fresh `claude -p` agents, given only the task text, all reached for
`LabelPropsView` unprompted. Full catalog rerun (`pixi run claude-md-eval`) will produce the
first cross-rule number.

Follow-up from the enforcement-coverage work in PRs #1258 (recital SHA-anchoring) and #1263
(h5ad / utf-8 / Windows helpers / run_py ratchets). Written to be picked up cold by another
session.

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
- **P2.5 rollup** — DEFERRED. Writing to `docs/ai-assist/CLAUDE_MD_EVAL.md` (trend annotated
  by CLAUDE.md blob SHA changes) is still stdout-only. Ship once we have >1 pass's data.
- **P3** — DEFERRED. Optional cron/schedule + trend-annotation in the rollup.

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
