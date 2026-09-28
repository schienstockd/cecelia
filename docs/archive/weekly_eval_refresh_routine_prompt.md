# Audit: A weekly eval-refresh routine, informed by published research

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

**Model:** Opus — this is a design task requiring judgment about tradeoffs
and independent research, not a mechanical check.
**Type:** Research + design audit. Produce a concrete routine, not just
principles. No code changes in this pass.

## Why this audit

A cron job now runs the `claude_md_eval` suite weekly. The open question is
how the challenge set itself should change over time — a fixed catalog run
repeatedly has a known failure mode in the published literature (see seeded
references below): static test/benchmark sets go stale and stop measuring
what they were built to measure. Two data sources already exist that could
drive rotation: the effectiveness log (`events.jsonl` — findings, outcomes,
resolution lag) and PR history (real fanout/convention-check findings from
actual work, like `#1281` errored-run split — currently OPEN, not yet
merged — and the button-move cluster in PR #1214 / commit `51704eef`
"collapse board-close confirm to a single arming button"). Design the
actual routine that turns those into new or retired challenges on a weekly
cadence, grounded in evidence rather than intuition alone.

**Read the existing plan doc BEFORE searching or designing.**
`docs/todo/CLAUDE_MD_EVAL_PLAN.md` already covers roughly 40% of what's
below — cadence, direct/indirect ratio, D12 discipline, ablation shape,
scoring-artifact patterns. Refine and disagree with it explicitly where
warranted; do not restart from zero and produce a parallel design that
silently contradicts it. Cite section names when your routine departs
from the plan doc's stance.

## Do your own literature search — the seeded list below is a starting
point, not the full search

The four references below were found in one prior search pass and are
seeded here so you don't start from nothing. **Before designing anything,
search further yourself** — don't treat this list as complete. Areas worth
searching that weren't fully covered:

- **Regression test selection from production/bug data** — this is a
  mature software-engineering research area (test case prioritization,
  test suite reduction, coverage-guided selection) predating LLM
  benchmarking entirely. It may have directly relevant, more rigorously
  studied answers to "how do you decide what to add/retire from a test
  suite based on real defects found in production" than anything in the
  LLM-benchmark literature — search for this specifically rather than
  assuming the eval-benchmark framing is the only relevant field.
- **Benchmark saturation / retirement criteria** — specific, quantitative
  rules other projects use for when to retire a benchmark item (e.g.,
  ceiling effects, inter-rater agreement collapse), which the seeded
  references gesture at but don't fully specify.
- **Cost-aware or budget-constrained continuous evaluation** — anything
  addressing the same cap/trade-off problem in item 5 below (adding a case
  requires retiring one) in a more principled, previously-studied way than
  an ad hoc rule.
- **Anything specifically about instruction-file or system-prompt
  compliance testing over time**, as opposed to general model-capability
  benchmarking — this is a narrower, newer area and may have little
  published work, but check before assuming so.

**Standards for what you cite, consistent with how prior audits in this
repo have handled sourcing:**
- Prefer primary sources (the paper itself, or its official venue page)
  over secondhand blog summaries. Where you can only find a secondhand
  summary, say so explicitly rather than presenting it as if verified
  against the source.
- Flag and exclude specific unverified statistics you can't trace to a
  primary source — a prior pass here found a "58% degradation per quarter"
  claim attributed to LiveBench that appears to come from a low-quality
  blog, not the paper itself, and was excluded for that reason. Apply the
  same scrutiny to anything new you find.
- For every source you cite, state explicitly what transfers to this
  repo's context (single-maintainer, non-adversarial, CLAUDE.md-compliance
  specific) and what doesn't. None of the seeded references were built for
  this exact use case — treat anything new you find the same way.

## Seeded references — read these too, then search beyond them

1. **White et al., "LiveBench: A Challenging, Contamination-Limited LLM
   Benchmark," ICLR 2025 (arXiv 2406.19314).** Static LLM benchmarks go
   stale because their answers leak into later training data, so the
   benchmark stops measuring capability and starts measuring memorization.
   LiveBench's fix: questions drawn from recent sources, refreshed on a
   fixed cadence, scored against objective ground truth rather than an LLM
   judge. Mechanism doesn't transfer directly (nothing here is being
   trained on your eval prompts), but the structural lesson does: a fixed
   challenge set loses discriminative power once a rule is reliably
   followed and stops producing findings, the same way a saturated
   benchmark question does. Cite for rotation cadence and retirement, not
   for a contamination argument.
2. **"A Survey from Static to Dynamic Evaluation," EMNLP 2025 (ACL
   Anthology 2025.emnlp-main.511).** Frames dynamic evaluation as a
   *sequence* of datasets `D_t`, updated on some schedule, versus a single
   static `D`. Useful for framing item 2 below as literally defining `D_t`
   for this repo.
3. **Wang et al., "Profit is the Red Team: Stress-Testing Agents in
   Strategic Economic Interactions" (arXiv 2603.20925).** Found agents
   robust against a fixed attack set become exploitable once pressure
   adapts, but the useful finding here is the **distillation** step —
   highest-impact failures get turned into concise hardening rules rather
   than the test set escalating forever. Cite for the
   distillation-vs-escalation argument (item 3).
4. **Large-scale AI agent red-teaming competition (arXiv 2507.20526).**
   1.8M submitted prompt-injection attacks across 22 agents, 44 scenarios;
   indirect injection outperformed direct. Relevant to the governance
   audit's prompt-injection item, not to rotation directly —
   cross-reference only.

## What to design

1. **The two data sources, concretely, not just "use the log and PRs."**
   - From `events.jsonl`: which rules show the highest unresolved rate,
     flattest `fixed_pre_commit`-only distribution (possible ceremony), or
     longest resolution lag? Candidates for harder/additional challenges.
   - Which rules show clean, stable compliance with real findings
     dropping? Candidates for rotation out, per Reference 1's retirement
     logic. **But distinguish two cases before retiring**: (a) the
     eval prompt is saturated — agents pass reliably, findings dropped
     because the *rule* is internalised — retire the prompt; (b) the
     eval prompt is trivial — agents pass because the prompt does not
     plausibly exercise the rule — rewrite the prompt, keep the rule.
     Do NOT retire a rule from the catalog based on stable compliance
     alone; verify the prompt is discriminating before retiring.
   - From PR history: pull real findings, check for shapes with no
     matching prompt in the catalog. Walk through whether PR #1214 /
     commit `51704eef` (button-move cluster — three ad-hoc buttons
     collapsed to one arming pattern) would have produced a new
     challenge under the proposed routine.

2. **Define `D_t` for this repo, per Reference 2's framing.** What's in
   this week's set, what rule updates it from last week's, what's the
   smallest unit of change. **Split `D_t` into an anchor set and a
   rotation set**: the anchor set (roughly 4-5 prompts) stays stable
   across the whole year so week-over-week trend in the rollup measures a
   comparable delta, not apples-vs-oranges churn. Only the rotation set
   churns per your routine. Say explicitly which of today's 12 prompts
   are anchors and why.

3. **Escalation vs. distillation — pick one, justify against this repo's
   actual scale.** Argue whether Reference 3's conclusion holds here too,
   given one primary contributor and the review-burden constraint from the
   governance audit.

4. **Direct vs. indirect balance in the rotating set.** Propose a target
   ratio and how it should shift as more real PR-history data accumulates.

5. **Cost and cadence sanity check.** State current $/week and turn count,
   project the rotation routine's cost, propose a cap/trade-off rule —
   check this against anything found in the budget-constrained-evaluation
   search above, not just intuition.

## What NOT to do

- Do not stop at the four seeded references — searching further is part
  of the deliverable, not optional.
- Do not propose permanent, unbounded escalation.
- Do not treat every PR-history finding as challenge-worthy on first
  sight — set an actual repeat-count threshold.
- Do not let the rotating set grow without bound; every addition needs a
  paired removal-or-demotion rule.
- Do not cite any source — seeded or newly found — as if built for this
  exact context without stating what transfers and what doesn't.
- Do not repeat the unverified-statistic mistake — trace every number you
  cite to its primary source or flag it as unverified.

## Output format

- What you found in your own search, beyond the seeded four — list new
  sources with the same primary-source/transfer discipline applied.
- A short verdict on distillation vs. escalation (item 3).
- `D_t` defined concretely (item 2).
- The concrete weekly selection algorithm (item 1), as an executable
  procedure with actual thresholds.
- A walk-through of the algorithm against PR #1214 / `51704eef` (button-move
  cluster, merged) and `#1281` (errored-run split, currently OPEN — note
  the state; the algorithm should still handle OPEN PRs, but say how).
- **Prompt-authoring constraints for any new challenge the routine
  proposes.** New prompts must conform to the scoring machinery already
  in place: plural-key `tool_order_before_tools` / `tool_order_after_tools`
  frontmatter (not the deprecated singular keys), additions-only regex
  matching (a `compliant_signal` or `anti_signal` that would match the
  anti-pattern *pasted in the prompt body* is a scoring artifact, not a
  compliance signal), and the `_ensure_eval_session` synthetic-id
  convention. State these constraints explicitly in the routine's
  "how a new challenge is authored" step, or the auto-generated
  challenges will reproduce the scoring bugs that took last week to
  clear.
- The proposed direct:indirect ratio and how it should shift over time.
- The cost/cadence cap rule and current baseline numbers to check it
  against.
- One closing paragraph: for every source cited (seeded and self-found),
  one sentence on what transferred to this design and what was scoped out.
