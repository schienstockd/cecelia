# Drift detection — prior art

Scope search done 2026-09-26 so the reviewer stack (see [`FANOUT_AUDIT_PLAN.md`](../todo/FANOUT_AUDIT_PLAN.md), [`CONVENTION_CHECK_PLAN.md`](../todo/CONVENTION_CHECK_PLAN.md), [`EFFECTIVENESS_LOG_PLAN.md`](../todo/EFFECTIVENESS_LOG_PLAN.md), [`EFFECTIVENESS_METHODOLOGY.md`](EFFECTIVENESS_METHODOLOGY.md)) can be presented as a considered choice rather than an unaware one.

## What "drift" means here

Three shapes we have actually hit:

- **Canonical → projection drift.** One source of truth, a copy elsewhere silently diverges. Example: `_OUTCOME_TAGS` in `check_commit_recital.py` hardcoded instead of importing `OUTCOME_VOCABULARY` from `effectiveness/log.py`.
- **Review-fanout drift.** A fix applied at one call site but not the siblings; or an addition that reinvents an existing helper.
- **Reviewer-effectiveness drift.** The reviewer optimises to look good on its own scorecard instead of catching real issues (Goodhart).

## Neighbouring tools reviewed

| Tool | What it is | Why not adopt |
| --- | --- | --- |
| **[drift-analyzer](https://github.com/mick-gsk/drift)** (`pipx install drift-analyzer`) | Repo-level structural-drift scorer, 24 signals, publishes its own score against itself in `drift_self.json`. Claimed 77% strict / 95% lenient precision on 5 wild repos (vendor's number, v0.5). | Different layer — whole-repo score, not per-commit review. The **self-scoring pattern** is the same move as our #1250 self-catch; worth citing as prior art for that idea. |
| **[Revieko](https://synqra.tech/revieko)** ([Show HN](https://news.ycombinator.com/item?id=46430352)) | GitHub App. Encodes repo structure into a multi-channel representation, scores structural deviation vs baseline, flags hotspots at PR time. | Architecture-coherence layer; orthogonal to reviewer-output discipline. Not a substitute. |
| **Conclave AI** (listed in [awesome-ai-code-review](https://github.com/kodustech/awesome-ai-code-review)) | Council of three models (Claude / GPT-5 / Gemini) reviews each PR against an attached PRD to flag scope drift. | Same *goal* as our two-reviewer split, different mechanism: model diversity vs task-scoped prompts + closed vocab. We already get diversity across sessions from the outcome log; we chose task scoping because our failure mode is fanout consistency, not scope creep. |
| **AgentSync** ([dallay](https://github.com/dallay/agentsync), [harryy2510](https://github.com/harryy2510/agentsync)) | Detects drift between a canonical spec and generated projections; `agent status --exit-code` in CI, `apply` hooks post-checkout for auto-repair. | This is exactly the *canonical → projection* pattern we already applied in #1250 (import `OUTCOME_VOCABULARY`, don't copy it). Nothing to adopt — we're doing this pattern in-tree without an extra tool. |
| **[dev.to: "The agent didn't hallucinate, it ignored what the repo already knew"](https://dev.to/tufan_tunc/the-agent-didnt-hallucinate-it-ignored-what-the-repo-already-knew-2m44)** (Tufan Tunç, 2026) | 12-reviewer AI pipeline against 3 real Copilot-authored PRs in .NET orgs. Pre-registered in a GitHub folder (not OSF/arXiv). Documents own false positives: (i) `HEAD~2000000000` flagged as unbounded loop, actually bounded by history depth; (ii) hardcoded literal flagged, actually the file's idiom. | Not a tool — an independent write-up that converges on our `confirmed` vs `plausible` discipline (no finding without located, verified evidence). Cite as external validation of the tagging convention. |

## What we do instead, and why

- **Two reviewers, task-scoped prompts.** One catches fanout drift (`FANOUT_AUDIT.md`), one catches addition drift vs inventory (`CONVENTION_CHECK.md`). Prompts live verbatim in the docs so a reviewer of the reviewers can read them.
- **Closed outcome vocabulary, single source, imported.** `OUTCOME_VOCABULARY` in `python/cecelia/effectiveness/log.py`; the recital hook imports it rather than copying. Regression against copying is `test_vocabulary_matches_effectiveness_log_module`.
- **Effectiveness log the reviewer never reads.** Goodhart-motivated. Log is passive; only the human and the offline rollup read it. Rationale spelled out in `EFFECTIVENESS_METHODOLOGY.md` § Non-goals.
- **One entrypoint, atomic emission** (`pixi run recital`, #1252). Spawns both reviewers via `claude -p` subprocess, emits their `_run` events to the effectiveness log in-process the moment the subprocess returns, prints the formatted recital body. Replaces the agent-remembers-to-emit trust path — that path failed four times consecutively in the session that motivated #1252 (zero rows in `~/.cecelia-effectiveness/events.jsonl` across four reviewer runs). Emission is now Python code, not a step the parent agent has to execute after the fact.
- **Self-catch is the acceptance test.** Two independent proofs on load-bearing infrastructure so far: (i) first draft of the recital hook (#1250) had its own vocab drift — both reviewers caught it pre-commit; (ii) first draft of the recital script (#1252) had a Windows-compat should-reuse + doc-drift finding — convention-check caught them in the same PR, fixed in-diff. Not a one-off — the shape of evidence we treat as validating the loop.

## Notes for write-up

- None of the five tools above run the specific loop we run (two task-scoped reviewers + closed outcome vocab + passive effectiveness log + self-catch acceptance).
- Vendor precision numbers (drift-analyzer's 77/95) are not third-party audited.
