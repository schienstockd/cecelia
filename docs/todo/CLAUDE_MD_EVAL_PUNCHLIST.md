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
| S3 | Li, Tang, Chen, Cheng, Metoyer, Hua & Chawla (2026), "Adaptive Testing for LLM Evaluation: A Psychometric Alternative to Static Benchmarks" (ATLAS), ICML 2026 Spotlight — [arXiv:2511.04689](https://arxiv.org/abs/2511.04689) | IRT-derived *discrimination-vs-difficulty* distinction — names why a 3/3 prompt may be trivial rather than internalised |
| S4 | Zhou et al. (2025), "Lost in Benchmarks? Rethinking Large Language Model Benchmarking with Item Response Theory" (PSN-IRT), AAAI 2026 Oral — [arXiv:2505.15055](https://arxiv.org/abs/2505.15055) | Corroborates S3's discrimination argument at population scale; **no concrete threshold extracted** |
| S5 | Śliwerski, Zimmermann & Zeller (2005), "When do changes induce fixes?" (SZZ), MSR 2005 — [DOI 10.1145/1082983.1083147](https://doi.org/10.1145/1082983.1083147) | Fix-to-introducer chain as methodological warrant for walking PR history into prompt candidates |

**Excluded / unverified** — one number the research prompt flagged (LiveBench "58% degradation
per quarter") could not be traced to a primary source and was excluded. No "benchmark saturation
velocity" statistic replaced it; the field lacks a defensible quantitative anchor for that
specific claim.

## Punchlist

Ordered by (payoff / cost). Check off in place as items complete; delete the row when the work
lands (per the *When work is done, delete it* rule in `docs/todo/README.md`).

### P1 — Rewrites landed 2026-09-29, **verified ineffective same day**

**Cost:** ~1 hour + $6 eval spend on backend P2-ablation WITH-arm. **Blocks:** deciding whether
these two rules stay in the catalog or get retired.

- [x] `cite-algorithm` — swapped from SSIM to **logicle transform**. Anchor exists in CLAUDE.md's
      own *Cite sources* example. Task wording strengthened to "specific published algorithm; its
      implementation must be traceable to its source".
- [x] `discovery-first` — task rewritten to a **tile-slice generator**. Territory `slice_utils`
      genuinely owns (`create_slices_multiscales`, `preview_region_bounds`, `crop_slice_tuple`).
- [x] **Re-tested at N=3 WITH-arm same day** (part of the P2 backend ablation, blob `436f7b6…`).
      Both prompts scored **0/3 compliant** — no change from pre-rewrite. `discovery-first`
      additionally tool_order=FAIL 3/3. See [findings 2026-09-29](CLAUDE_MD_EVAL_REFRESH_ROUTINE.md#findings-from-2026-09-29-first-real-ablation-pilot-pass)
      for the interpretation.

**Diagnostic-frame verdict: escalate out of the rule-rewrite loop.** Two well-formed rewrites
did not move the score. A third rewrite of the same shape is diagnostic-mode churn. Next
action is to pick an intervention layer that isn't "the prompt":

- [ ] Read the actual scored diffs for one WITH-arm run of each (needs `--keep-worktrees`
      re-fire, ~$0.60 each) — is the agent citing the wrong way, or not citing at all?
- [ ] Choose one for each rule: (a) redesign the CLAUDE.md rule *section* (not the prompt),
      (b) add a ratchet that enforces the anti-pattern deterministically, (c) accept the rule
      isn't teachable at this layer, document, and retire the probe.
      **Do not rewrite the prompt again** — the mechanism is upstream of the prompt.

### P2 — First paired ablation on the 4 failing prompts

**Cost:** ~$3–4 (4 prompts × 2 arms × N=3 = 24 runs at ~$0.40 avg). **Blocks:** first quotable Δ.

- [ ] Run `pixi run claude-md-eval-ablation --only cite-algorithm,discovery-first,dir-size,kill-process-tree --runs 3`
      (`--only` is the actual flag name — verified 2026-09-29 by reading
      `scripts/claude_md_eval/run_ablation.py`; no CLI extension needed).
- [ ] Read at least one trace per arm per prompt before quoting Δ — D12 discipline.
- [ ] If a prompt's `without`-arm score is ≥ its `with`-arm score, CLAUDE.md is not the mechanism
      keeping compliance up; that prompt is measuring something else. Flag in the rollup as a
      candidate for rewrite (not retire).

### P3 — Frontend P1 pilot

**Cost:** ~$1.50 (3 prompts × N=1 WITH-arm). **Blocks:** frontend coverage in anchor + rotation
selection.

- [x] Author 3 prompts per [`CLAUDE_MD_EVAL_FRONTEND_PLAN.md`](CLAUDE_MD_EVAL_FRONTEND_PLAN.md)
      *§ P1 pilot* — landed 2026-09-29 as `frontend-inlinenote.md`,
      `frontend-copy-canonical.md`, `frontend-coalesce.md`.
- [x] N=1 WITH-arm pilot, trace-inspected, regex bug on `coalesce` (`\(` didn't allow `<T>(`)
      caught + fixed.
- [x] N=3 WITH-arm signal check at blob `436f7b6…`. Results:
      `frontend-inlinenote` 3/3, `frontend-coalesce` 3/3, `frontend-copy-canonical` 0/3 with
      tool_order=FAIL 3/3. The 0/3 is a **real drift signal** — agents don't reach the
      canonical UI-copy const — not a scoring bug. See [findings 2026-09-29](CLAUDE_MD_EVAL_REFRESH_ROUTINE.md#findings-from-2026-09-29-first-real-ablation-pilot-pass).
- **Cost implication:** landing these 3 as `.md` files under `prompts/` auto-adds them to the
  weekly `pixi run claude-md-eval` catalog (the runner enumerates the directory). Weekly cost
  goes from 12×3=36 to 15×3=45 runs, ~+25% on the current $17/pass baseline. Rotation-set logic
  in P5 will address; not blocking today's landing.

### P4 — Retire the stable-3/3 backend anchors (diagnostic framing)

**Cost:** minutes; frees ~$8/wk of cron spend. **Blocks:** stopping the eval from re-confirming
what ratchets already enforce.

Under the diagnostic frame the seven backend I/O prompts stable at 3/3 have delivered their
result — the weakness they probed does not exist (or is closed by an existing ratchet). Move
them out of the weekly enumeration:

- [ ] Move to `scripts/claude_md_eval/prompts/retired/`: `h5ad-read`, `h5ad-write`,
      `zarr-read`, `zarr-write`, `utf-8-json-write`, `spawn-python`, `crop-failure`.
      Justification per-prompt is the ratchet that already enforces the same rule
      (`test_h5ad_access_convention.py`, `test_zarr_access_convention.py`,
      `test_utf8_encoding_convention.py`, `python spawn ratchet`).
- [ ] Add `prompts/retired/README.md` naming the retirement discipline (why here,
      how to un-retire if a ratchet is removed).
- [ ] After the next Monday cron with the frontend three still enrolled, decide on
      `frontend-inlinenote` + `frontend-coalesce` (no ratchet backstop — needs one more
      pass of confirmation before retirement).

### P4a — Guard rollup against errored-arm rendering

**Cost:** 30 min. **Blocks:** stopping the misleading Δ=+4 table from persisting in
`docs/ai-assist/CLAUDE_MD_EVAL.md` when the WITHOUT arm errors.

The 2026-09-29 post-update transient rendered a bogus Δ=+4 in the ablation section
(WITH=4/12 vs WITHOUT=0/12-all-errored). The rollup should detect the errored-arm case and
suppress or banner the Δ table, not publish a false comparison.

- [ ] `run_ablation.py`: record `with_error` / `without_error` counts per prompt + in totals.
- [ ] `rollup.py._ablation_section`: if ≥50% of runs in either arm errored, replace the Δ
      table with a banner (`⚠ WITHOUT arm errored across ≥50% of runs — Δ suppressed`) and
      link the transient case as prior art.

### P4b — Add first mined indirect probe: `hand-rolled-debounce`

**Cost:** ~$1.50 first pass; ongoing $0.50/wk if kept. **Blocks:** the first
non-copy-canonical indirect probe in the catalog.

From the 2026-09-29 PR-history mining pass (cluster #1, `existing-helper-not-reached-for`).
Same shape as `frontend-copy-canonical`: realistic multi-step task, never names the canonical
helper (`debouncedLatest`), scored by whether the agent grep-and-imports it.

- [ ] Author `scripts/claude_md_eval/prompts/hand-rolled-debounce.md`.
- [ ] Interpret the first pass as diagnostic evidence, not a compliance number: 3/3 → retire +
      inspect whether `continuousControls.test.ts` ratchet can relax; 0/3 → the rule is not
      teaching, fix the inventory pointer or the CLAUDE.md section.

### P5 — Rotation selection script (deferred under diagnostic framing)

**Cost:** weeks of engineering, not hours.

**Deferred indefinitely.** The rotation-set-under-a-cap logic was a compliance-suite
construct. Under the diagnostic frame there's no rotation *set* — there's a small backlog of
weakness probes to author from PR-history mining, and probes retire as their weakness closes.
Manual authoring + retirement is sufficient at this scale. Un-defer if the backlog grows past
the point where hand-management drifts.

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
