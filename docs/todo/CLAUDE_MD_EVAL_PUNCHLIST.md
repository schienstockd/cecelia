# CLAUDE.md eval — reference set + execution punchlist

**Status:** ACTIVE (2026-09-29) — sequel to [`CLAUDE_MD_EVAL_REFRESH_ROUTINE.md`](CLAUDE_MD_EVAL_REFRESH_ROUTINE.md).
That doc designs the rotation; this doc says *what to do next, in order, with costs*.

## Purpose

The parent [`CLAUDE_MD_EVAL_PLAN.md`](CLAUDE_MD_EVAL_PLAN.md) shipped the runner, ablation, canary,
rollup, and weekly cron. The first real weekly pass ran on 2026-09-29 (blob `23634d3e`, 28/36
compliance, $17.09). [`CLAUDE_MD_EVAL_REFRESH_ROUTINE.md`](CLAUDE_MD_EVAL_REFRESH_ROUTINE.md)
designed the rotation routine — anchors, `D_t = A ∪ R_t`, Rule-of-Three algorithm,
distillation-over-escalation. Neither doc gives a *punchlist*, and both are long enough that
"where do I start Monday morning" needs its own single-page answer. That's this doc.

It also consolidates the **verified reference set** from tonight's research pass so future work
doesn't have to re-verify.

## Verified reference set (as of 2026-09-29)

All ten below were verified against a primary source (arXiv API, CrossRef, or DOI resolver) on
2026-09-29. Full transfer analysis — what applies here and what doesn't — lives in
[`CLAUDE_MD_EVAL_REFRESH_ROUTINE.md`](CLAUDE_MD_EVAL_REFRESH_ROUTINE.md) *§ Sources beyond the
seeded four*.

### Seeded (from the research prompt at `docs/archive/weekly_eval_refresh_routine_prompt.md`)

| # | Citation | Where the argument enters here |
|---|---|---|
| R1 | White et al., "LiveBench: A Challenging, Contamination-Limited LLM Benchmark," ICLR 2025 — [arXiv:2406.19314](https://arxiv.org/abs/2406.19314) | Rotation-cadence and retirement logic; **not** the contamination argument (we're not training) |
| R2 | Zhang et al., "A Survey from Static to Dynamic Evaluation," EMNLP 2025 — [ACL Anthology 2025.emnlp-main.511](https://aclanthology.org/2025.emnlp-main.511/) | The `D_t` sequence-of-datasets framing that defines our anchor + rotation split |
| R3 | Wang et al., "Profit is the Red Team: Stress-Testing Agents in Strategic Economic Interactions," [arXiv:2603.20925](https://arxiv.org/abs/2603.20925) | Distillation-over-escalation — the core verdict |
| R4 | Zou et al., large-scale AI agent red-teaming (1.8M attacks, 22 agents), [arXiv:2507.20526](https://arxiv.org/abs/2507.20526) | Cross-ref for future prompt-injection prompts; not driving anything here |

### New (found during the 2026-09-29 research pass, all verified primary)

| # | Citation | Where the argument enters here |
|---|---|---|
| S1 | Yoo, S. & Harman, M. (2010), "Regression testing minimization, selection and prioritization: a survey," *STVR* 22(2):67–120 — [DOI 10.1002/stvr.430](https://doi.org/10.1002/stvr.430) | Minimization/selection/prioritization vocabulary; ~2–3 fault-linked triggers as the repeat-count threshold |
| S2 | Zhou et al. (2023), "Instruction-Following Evaluation for Large Language Models" (IFEval), [arXiv:2311.07911](https://arxiv.org/abs/2311.07911) | Verifiable-instruction pattern — validates our regex + tool_order scorer architecture |
| S3 | Li et al., "ATLAS: Adaptive Testing for LLM Evaluation," ICML 2026 Spotlight — [arXiv:2511.04689](https://arxiv.org/abs/2511.04689) | IRT-derived *discrimination-vs-difficulty* distinction — names why a 3/3 prompt may be trivial rather than internalised |
| S4 | Zhou et al. (2025), "Lost in Benchmarks? Rethinking LLM Benchmarking with Item Response Theory" (PSN-IRT), AAAI 2026 — [arXiv:2505.15055](https://arxiv.org/abs/2505.15055) | Corroborates S3's discrimination argument at population scale; **no concrete threshold extracted** |
| S5 | Śliwerski, Zimmermann & Zeller (2005), "When do changes induce fixes?" (SZZ), MSR 2005 — [DOI 10.1145/1082983.1083147](https://doi.org/10.1145/1082983.1083147) | Fix-to-introducer chain as methodological warrant for walking PR history into prompt candidates |

**Excluded / unverified** — one number the research prompt flagged (LiveBench "58% degradation
per quarter") could not be traced to a primary source and was excluded. No "benchmark saturation
velocity" statistic replaced it; the field lacks a defensible quantitative anchor for that
specific claim.

## Punchlist

Ordered by (payoff / cost). Check off in place as items complete; delete the row when the work
lands (per the *When work is done, delete it* rule in `docs/todo/README.md`).

### P1 — Rewrite the two 0/3 prompts before next Monday (2026-10-05) — **REWRITES LANDED 2026-09-29, awaiting cron verification**

**Cost:** ~1 hour, $0 (no eval runs until Monday). **Blocks:** meaningful trend row.

- [x] `cite-algorithm` — swapped from SSIM to **logicle transform** (2026-09-29). Anchor exists in
      CLAUDE.md's own *Cite sources* example (`app/src/gating/transforms.jl` cites Moore & Parks
      2012). Task wording now says "specific published algorithm; its implementation must be
      traceable to its source" — stronger cue than the previous "should follow a published
      reference". `compliant_signal` unchanged (requires `#`-comment citation, matching CLAUDE.md's
      exact "add a comment with the citation" wording).
- [x] `discovery-first` — task rewritten to a **tile-slice generator** for 3D+time zarr iteration
      (2026-09-29). Territory `slice_utils` genuinely owns (`create_slices_multiscales`,
      `preview_region_bounds`, `crop_slice_tuple` — all in `docs/inventory/PYTHON.md`). A grep
      for `slice`, `tile`, or `zarr` in the inventory now returns a plausible hit, restoring the
      credibility of the "helper might already exist" cue that was missing when the task described
      a `tile_origins` helper with no inventory match.
- [ ] **Cron-verification pending.** Monday 2026-10-05 23:59 AEDT fires the weekly cron against
      the new blob. Delete this P1 section after that trend row lands and shows either
      (a) improved compliance on both prompts, or (b) still-failing prompts with new failure
      mode — either result is a real data point, and the rewrites themselves are complete.
      Do NOT run the eval on-branch — the CLAUDE.md blob SHA is stable, and running mid-week
      pollutes the log with an extra WITH-arm row that distorts the trend at the wrong blob.

### P2 — First paired ablation on the 4 failing prompts

**Cost:** ~$3–4 (4 prompts × 2 arms × N=3 = 24 runs at ~$0.40 avg). **Blocks:** first quotable Δ.

- [ ] Run `pixi run claude-md-eval-ablation --prompts cite-algorithm,discovery-first,dir-size,kill-process-tree`
      (or the equivalent one-off invocation — check `scripts/claude_md_eval/run_ablation.py --help`
      for the exact CLI, may need extension).
- [ ] Read at least one trace per arm per prompt before quoting Δ — D12 discipline.
- [ ] If a prompt's `without`-arm score is ≥ its `with`-arm score, CLAUDE.md is not the mechanism
      keeping compliance up; that prompt is measuring something else. Flag in the rollup as a
      candidate for rewrite (not retire).

### P3 — Frontend P1 pilot

**Cost:** ~$1.50 (3 prompts × N=1 WITH-arm). **Blocks:** frontend coverage in anchor + rotation
selection.

- [ ] Author 3 prompts per [`CLAUDE_MD_EVAL_FRONTEND_PLAN.md`](CLAUDE_MD_EVAL_FRONTEND_PLAN.md)
      *§ P1 pilot* (primitive catalog, UI-copy canonicalisation, coalescing). Use plural-key
      `tool_order_before_tools` / additions-only regex per the artifact-fix constraints in the
      refresh-routine doc's *New prompt authoring constraints* section.
- [ ] Run each at N=1 WITH-arm, read the trace, gate the P2 decision on whether the signal is
      distinguishing anything.

### P4 — Update anchor set based on real weeks of trend data

**Cost:** review-only, $0. **Blocks:** committing to the design doc's anchor set for 2026H2.

- [ ] After 3 weekly cron passes (~2026-10-19), verify the design doc's anchor picks
      (`canary`, `h5ad-read`, `zarr-write`, `utf-8-json-write`, `spawn-python`) are all still
      stable 3/3. If any has drifted, treat as a data point on whether "3/3 for 3 weeks" is the
      right stability threshold, per S3 discrimination logic.
- [ ] Update [`CLAUDE_MD_EVAL_REFRESH_ROUTINE.md`](CLAUDE_MD_EVAL_REFRESH_ROUTINE.md)
      *§ Anchor set A* with the confirmed picks + one line on the observed 3-week stability.

### P5 — Implement the rotation selection script

**Cost:** weeks of engineering, not hours. **Blocks:** rotation actually happening.

- [ ] Build `scripts/claude_md_eval/select_rotation.py` per the algorithm in
      [`CLAUDE_MD_EVAL_REFRESH_ROUTINE.md`](CLAUDE_MD_EVAL_REFRESH_ROUTINE.md)
      *§ Weekly selection algorithm*.
- [ ] Emit a new `claude_md_eval_rotation` event type to `~/.cecelia-effectiveness/events.jsonl`
      when a prompt is added, retired, or rewritten. Effectiveness rollup renders these as a
      rotation-history section under the trend table.
- [ ] Manual rotation is the fallback until this ships; the design doc's decisions are firm
      enough to rotate by hand for 2–3 weeks without drift.

## Not in scope

- **Cost cap enforcement.** Design doc proposes $20/week WITH-arm; enforcement would need a
  pre-fire cost check in `cron_pass.sh`. Deferred until we've seen 4 weeks of cost data —
  tonight's $17.09 may not be representative.
- **Full-frontend suite (P3 in `CLAUDE_MD_EVAL_FRONTEND_PLAN.md`).** Gated on P1 pilot signal.
- **Prompt-injection prompts** from Zou et al. (R4). Cross-referenced only; not in the rotation
  candidate pool for now.
- **IRT scoring.** S3/S4 both use IRT; we have 12 prompts and one model per pass — the machinery
  doesn't apply at this scale. Borrow the discrimination-vs-difficulty *vocabulary*, not the
  algorithm.
