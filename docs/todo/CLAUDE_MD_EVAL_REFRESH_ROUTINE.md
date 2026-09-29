# CLAUDE.md eval — weekly refresh routine

**Status:** design; no code. Follow-on to [`CLAUDE_MD_EVAL_PLAN.md`](CLAUDE_MD_EVAL_PLAN.md)
(the plan-doc that shipped P1–P3 + rollup + indirect pilot). This doc defines how the
challenge set `D_t` changes over time on the weekly cron, driven by the effectiveness log
and PR history rather than intuition. Grounded in tonight's first real weekly pass
(2026-09-29, `23634d3e`, N=3, WITH-only, $17.09) and the literature scan below.

Where this doc departs from the plan doc's stance, the plan-doc section is named.

---

## TL;DR

- **`D_t` = anchor set A (5 stable prompts, unchanged for the whole year) ∪ rotation set
  R\_t (5–7 prompts, weekly churn).** `|D_t|` ≤ 12, matching current suite size.
- **Distillation over escalation.** Reference 3 (Wang et al.) transfers as written:
  single-contributor, non-adversarial repo has nobody to escalate against; every added
  prompt spends real dollars against one reader (Dominik).
- **Retire on prompt-triviality, not on rule-compliance.** A prompt at 3/3 for three
  weeks retires only if the *rule* also produced ≥3 real findings in the same period;
  otherwise the prompt is trivial and gets rewritten. Directly extends
  *Sonnet 2026-09-28 discipline additions* (D12) and the *What to design* item 1
  distinction in the prompt.
- **Add on repeat-count ≥3 across distinct PRs**, not on first sight. Directly borrowed
  from Yoo & Harman 2012 selection theory (see Sources).
- **Cost cap: $20/week WITH-arm.** Tonight's $17.09 already sits at 85% of that ceiling
  with no rotation activity — the plan doc's $1–2 estimate (*Cost model*) is ~10× low,
  flag and correct.
- **Direct:indirect target: 3:2 in R\_t, 4:1 in A.** Shift indirect share by +1 prompt
  per quarter until R\_t reaches 1:1.

---

## Sources beyond the seeded four

Applying the primary-source discipline the prompt requires. For each: what transfers to
this repo (single maintainer, non-adversarial, CLAUDE.md-compliance specific) and what
doesn't.

**S1. Yoo, S. & Harman, M. (2012) — "Regression testing minimization, selection and
prioritization: a survey." *Software Testing, Verification and Reliability* 22(2):67–120.
DOI 10.1002/stvr.430.**
The canonical survey the prompt gestures at (the "mature SE research area predating LLM
benchmarking"). Provides the three-way frame: **minimization** (drop redundant test cases
without losing coverage), **selection** (pick tests relevant to the current change),
**prioritization** (order tests to detect faults sooner).
- *Transfers:* the minimization–selection–prioritization vocabulary maps cleanly to our
  problem — retiring a prompt is minimization, adding one from a PR-history cluster is
  selection.
- *Doesn't transfer:* the field assumes coverage instrumentation and per-test cost data
  we don't have; the eval scores don't factor into a coverage matrix. Cite for the
  *framing and repeat-count threshold* (Section 4.3 discusses history-based rules of
  thumb around 2–3 fault-linked triggers), not for a specific algorithm we implement.
  Primary source: Wiley URL above; also mirrored at KAIST institutional repo. Verified.

**S2. Zhou et al. (2023) — "Instruction-Following Evaluation for Large Language Models"
(IFEval), arXiv:2311.07911.**
The closest LLM-eval analog to "did the model follow the written rule." Uses
*verifiable instructions* scored by deterministic checkers rather than an LLM judge.
- *Transfers:* the verifiable-instruction pattern is exactly the shape of a compliance
  prompt: the rule ("write in >400 words", "use `zarr_utils`") is checkable by a
  scorer, not a judge. Validates our regex+tool-order scorer architecture.
- *Doesn't transfer:* IFEval prompts are single-turn synthetic strings with no repo
  context; our prompts run in a real worktree with CLAUDE.md, ratchets, and an
  existing inventory. Their ~500-prompt scale is not our scale (12) and their
  saturation math therefore doesn't apply. Reported average model scores of 70–80% are
  observational only. Primary source verified (arXiv abstract page + PDF).

**S3. ATLAS — "Adaptive Testing for LLM Evaluation: A Psychometric Alternative to
Static Benchmarks," arXiv:2511.04689.**
Applies Item Response Theory (IRT) to LLM benchmark items — each item has a
*discrimination* parameter (how well it separates strong from weak models) alongside
difficulty. Reports up to 90% item reduction while preserving measurement precision
(e.g., "41 items out of 5,600 on HellaSwag reproduce whole-bank ability estimates").
- *Transfers:* the *concept* that low-discrimination items should be retired directly
  supports the "3/3 for 3 weeks ⇒ candidate for retirement" rule below. The stronger
  transfer is the diagnostic that **item difficulty and discrimination are separate
  axes** — a prompt everyone passes is either trivial (low difficulty) or shows the
  rule is universally internalised (high difficulty, low discrimination among the two
  arms). D12 already surfaces the latter via ablation; IRT names it.
- *Doesn't transfer:* IRT needs a matrix of many respondents × items; we have one
  model per pass and 12 items. Don't implement Fisher-information selection; borrow the
  discrimination-vs-difficulty distinction as a decision aid. Reported reductions are
  from other benchmarks, not ours. Primary source: arXiv abstract page verified;
  numerical claim above is stated in the abstract, so cited as authors' claim, not as
  independently validated.

**S4. "Lost in Benchmarks? Rethinking Large Language Model Benchmarking with Item
Response Theory" (PSN-IRT), arXiv:2505.15055 — accepted AAAI 2026 (oral, per the
paper page).**
Analyses 11 LLM benchmarks (41,871 items) with an IRT-derived framework, finds
substantial variation in item quality, and argues for retirement of low-discrimination
items. *No concrete retirement threshold was extractable from the abstract or the
sections available via WebFetch* — flagged and used only for the qualitative claim
that per-item quality varies widely across benchmarks.
- *Transfers:* corroborates S3's discrimination argument at population scale.
- *Doesn't transfer:* same one-respondent limitation; and until I can read the full
  paper I do NOT cite a specific numerical threshold from it.

**S5. Śliwerski, Zimmermann & Zeller (2005) — "When do changes induce fixes?" — the
SZZ algorithm.** MSR 2005. Backwards-walks bug-fix commits to find the introducing
change. Foundational for "learn from real fixes in version control."
- *Transfers:* the pattern of "PR #1214 was a fix — walk it back to the shape of what
  it fixed, and use that shape as a test candidate" mirrors SZZ's fix-to-introducer
  chain. Our version is coarser: we don't need to find the introducing change, we
  need the *symptom shape* the fix addressed (three ad-hoc arming buttons → collapse).
- *Doesn't transfer:* SZZ assumes labelled bug-fix commits and produces a commit-level
  attribution; we're using recital `_finding` rows and merged PR titles, not labelled
  bugs. Cite for the methodological warrant, not the algorithm. Verified: pointer to
  the 2005 paper via multiple survey mirrors; I did not read the primary MSR PDF this
  pass — cite as *methodological pattern in the SE literature*, not as if I re-verified
  their numerical claims.

**Excluded (unverified / low-quality):** the LiveBench "58% degradation per quarter"
figure noted in the prompt has already been excluded. I found no equivalent quantitative
"benchmark saturation velocity" number in a primary source during this pass — the
Stanford AI Index phrase "saturated in months" is qualitative and untraceable to a
primary-source table.

---

## Verdict: distillation over escalation

**Adopt distillation, reject escalation. Rewrite the two failing prompts; do not spawn
harder variants.**

Wang et al. (Reference 3) argue distillation because adversarial escalation stops paying
off once each new attack teaches you the same thing. Here the argument is stronger,
because there is no adversary — the "attacker" is Claude itself, running the same
CLAUDE.md rules the "defender" wrote. Escalation costs money and reader-attention
against a single primary contributor who will already read every finding. Concretely:

- **The two 0/3 prompts** (`cite-algorithm`, `discovery-first`) are rewrite candidates,
  not additional-hard-variant candidates. The plan doc's *Sonnet 2026-09-28 discipline
  additions* already applied this reasoning to the original `discovery-first`
  (`next_multiple` was retired for being non-discovery-shaped) — extend it: the current
  `tile_origins` phrasing still failed 0/3, which suggests the phrasing doesn't make
  the "helper might already exist" claim credible. Real fix is to first plant a
  plausible helper reference in `docs/inventory/PYTHON.md` grep hits, then rewrite the
  prompt to nudge (not name) that path. `cite-algorithm` failed 0/3 because SSIM is
  ordinary-code-shaped in Claude's prior — needs a less-Wikipedia algorithm, e.g.
  logicle transform (already cited in-repo per CLAUDE.md's own example, so the anchor
  exists).
- **Ceremony risk.** Wang et al.'s escalation warning cashes out here as: if we add a
  harder version of a rule that only triggers when the agent already knows to look
  something up, we're measuring rule-recital, not rule-application. Distillation of the
  *rule* (tightening CLAUDE.md prose or splitting one bullet in two) is often the right
  action, not distillation of the *prompt*.

Escalation exception: **indirect coverage is not yet at 3:2 in R\_t** (currently
1/12). Growing indirect share until we hit that ratio is a one-time expansion, not
open-ended escalation — see *Direct:indirect balance* below.

---

## `D_t` — definition and split

**`D_t = A ∪ R_t`** where **A = 5 anchor prompts stable for the year** and
**R\_t = 5–7 rotation prompts that churn weekly**, per Reference 2 (dynamic-eval
framing). `|D_t|` ≤ 12; on the weeks a rotation add exceeds the cap, the algorithm
forces a paired removal.

### Anchor set A (5 prompts, do not change in 2026H2)

Chosen by three criteria: (a) rule has an active ratchet (drift catchable in code, so
the eval anchor is measuring the *authoring* side), (b) prompt scored 3/3 in tonight's
pass (stable enough that the anchor line is a comparable delta week to week — the
purpose of an anchor is trend legibility, not challenge), (c) the top-8 rule-frequency
buckets from `events.jsonl` include this rule (real workload touches it).

| Anchor | Rule | Reason |
|---|---|---|
| `canary` | CLAUDE.md-loaded marker | Precondition for interpreting every other row. Non-negotiable per D12. |
| `h5ad-read` | H5AD readers/writers | #2 in effectiveness-log frequency (16 events). Has ratchet. 3/3 tonight. |
| `zarr-write` | zarr_utils / store_compressor | Ratchet-backed. Rule appears in top-8 (rank 4). 3/3 tonight. |
| `utf-8-json-write` | Windows encoding=utf-8 | #1 in effectiveness-log frequency (20 events). Ratchet-backed. 3/3 tonight. |
| `spawn-python` | run_py | Ratchet-backed. Ranks 7 in effectiveness log (4 events). 3/3 tonight. |

Departs from plan-doc *Prompt catalog (initial)*, which enumerates 10 prompts without
splitting anchor from rotating. The plan doc treats the catalog as monolithic; this
doc splits it so week-over-week trend rows in the rollup are apples-to-apples.

### Rotation set R\_t (5–7 prompts, churns weekly)

At `t = 2026-09-29` this is: `zarr-read`, `h5ad-write`, `kill-process-tree`,
`dir-size`, `crop-failure` (indirect pilot), `cite-algorithm`, `discovery-first`.

**Smallest unit of change:** one prompt added or removed per week, per rule. Rewrites
of an existing rotation prompt count as a change and reset the trend baseline for that
prompt (already the plan doc's *Non-negotiables* rule for the whole catalog — extended
here to per-prompt granularity).

---

## Weekly selection algorithm

Runs as a step of the Monday-night cron, before the eval pass. Reads the effectiveness
log (`~/.cecelia-effectiveness/events.jsonl`), the last four `claude_md_eval_run`
weeks, and merged PR titles + `_finding` rows for the trailing 30 days.

```
Inputs:
  E   = effectiveness-log events, trailing 30 days
  V   = claude_md_eval_run rows, trailing 4 weeks
  PRs = merged PRs, trailing 30 days, with associated _finding_resolved rows
  A   = anchor set (frozen this year)
  R   = current rotation set

Step 1. Anchor health.
  For each a in A:
    pass_rate_a = mean(V rows where prompt_id = a, arm = with)
    If pass_rate_a < 0.80 for 2 consecutive weeks:
      Emit HEALTH_ALERT(a) into rollup.  Do NOT retire anchors.
      Open a docs/todo entry naming the CLAUDE.md section that owns the rule.

Step 2. Rotation retirement candidates.
  For each r in R:
    If pass_rate_r == 1.0 for 3 consecutive weeks:
      finding_count_r = count(E where event in {fanout_audit_finding,
                                                 convention_check_finding}
                                and payload.rule matches rule_of(r),
                                trailing 30 days)
      If finding_count_r >= 3:
        # Rule is exercised in the wild AND the prompt reliably passes.
        # Prompt is measuring successful internalisation.  ==>  RETIRE prompt.
        mark r for removal.
      Else:
        # Prompt is easy and the rule is NOT being tested.
        # Do NOT retire.  Mark r for REWRITE.
        emit REWRITE_TASK(r) into rollup.

Step 3. Rotation add candidates from log.
  rule_counts = top-8 rules by event count in E
  For each rule with no corresponding prompt in D_t:
    Emit ADD_CANDIDATE(rule_id).

Step 4. Rotation add candidates from PR history.
  clusters = group PRs' _finding_resolved rows by (canonical_helper, symptom_shape)
             where outcome in {fixed_pre_commit, confirmed}
  For each cluster c with distinct_pr_count(c) >= 3:
    Emit ADD_CANDIDATE_INDIRECT(c, seed_pr_ids = c.pr_ids).

Step 5. Trim to cap.
  removes = candidates from Step 2 tagged RETIRE
  adds    = candidates from Steps 3–4, ranked by (log_frequency +
            2 * indirect_flag * urgency_needed)
  Apply removes first.
  Apply adds until |R_t+1| == min(7, |R_t| - len(removes) + len(adds)).
  If adds > removes and cap hit: retire the ROTATION prompt with the lowest
    week-over-week variance across the last 4 weeks (== lowest discrimination,
    per Source S3), NOT an anchor.

Step 6. Author the new prompt.
  See "New prompt authoring constraints" below.  Fails-closed: if the
  authoring step cannot produce a compliant frontmatter block on a
  candidate, the candidate is deferred to the next week, not shipped
  half-formed.
```

**Thresholds are explicit and independently justifiable:**

- `< 0.80` anchor alarm — matches the plan doc's *Follow-ups* placeholder ("any rule
  that drops below 80% over three consecutive passes"), tightened to 2 weeks because
  we have only 4 weeks of runway before the next quarterly review.
- `1.0 for 3 weeks` retirement floor — the shortest window that survives one bad-luck
  spawn per week without triggering. Matches Yoo & Harman §4.3's history-window rule
  of thumb (2–3 windows before a decision).
- `finding_count >= 3` real-world use — matches the prompt's "actual repeat-count
  threshold" *What NOT to do* item. 3 is the Rule of Three from the repo's own
  `feedback_rule_of_three_no_deferring` memory.
- `distinct_pr_count >= 3` for indirect adds — same Rule of Three; also matches Yoo &
  Harman's history-based prioritisation heuristic. Distinct PRs (not distinct
  findings) so that a single sprawling PR doesn't inflate a candidate.

---

## Walkthrough: PR #1214 / `51704eef` (merged) and #1281 (open)

### PR #1214 — "collapse board-close confirm to a single arming button"

Three ad-hoc buttons (board-close, chip-remove, analysis-tab-close) each had bespoke
"click twice to confirm" implementations; PR #1214 pulled them behind one arming
primitive. This is a **case-C convention finding** (a new component that duplicated an
existing pattern) — the reviewer subagent shape the convention-check catches.

Trace through the algorithm at week `t`:
- **Step 4**: cluster the 30-day `_finding_resolved` rows by `(canonical_helper,
  symptom_shape)`. The three ad-hoc arming buttons produce three PR entries touching
  the same symptom shape → `distinct_pr_count == 3` → the cluster **passes the Rule of
  Three threshold**.
- **Step 5**: `ADD_CANDIDATE_INDIRECT(button-arming-collapse, seed_pr_ids = [#1214,
  earlier-two])`. Trimmed against cap; if adds ≤ removes, admitted.
- **Step 6**: the authoring step writes an indirect prompt of shape "*I want a 'delete
  region' button in the region-editor sidebar that requires a confirming second click
  before firing. Put the button in `frontend/src/components/regions/RegionEditor.vue`.
  One click = arm, second click within 3s = fire.*" — with `compliant_signal` on the
  arming primitive's component name (e.g. `ArmingButton`), `anti_signal` on
  `if (\s*armed\s*)` / `setTimeout.*disarm` inline patterns, `tool_order_before_tools:
  Grep,Read,Glob`, `tool_order_before_arg_match:
  (docs/inventory/FRONTEND|ArmingButton|arming)`, `tool_order_after_tools:
  Write,Edit,MultiEdit`. Does **not** include the string "ArmingButton" in the prompt
  body (the anti-signal check would fire against the pasted body per the plan doc's
  *Scoring artefacts fixed* rule).

**Verdict:** algorithm generates a *new* indirect prompt for the button-move cluster,
not a variant of an existing prompt. Confirms the routine reaches PR-history-driven
adds, not only log-driven ones.

### PR #1281 — "split errored `_run` events from clean passes" — OPEN

Currently not merged. Under the base algorithm above, **only merged PRs feed Step 4**
(the `distinct_pr_count` reference implies "landed in main"). This deliberately
excludes OPEN PRs on the grounds that an open PR may still be reverted or reshaped —
scoring off a moving target would be Reference 1's contamination problem inverted.

But: excluding OPEN PRs loses signal on the newest, freshest work. Amendment:

- **Step 4 (extended):** OPEN PRs contribute to the cluster count **at weight 0.5**
  and only if the PR has ≥1 `_finding_resolved` row with outcome `fixed_pre_commit`
  or `confirmed`. #1281's title (`split errored _run events from clean passes`) is
  itself a case-F fanout finding shape — the fix separated one code path into two
  because the merged path was masking real errors. Under the extended rule, #1281
  contributes 0.5 to any cluster it fits into. To *promote* to an ADD_CANDIDATE, a
  cluster needs `weighted_pr_count >= 3` — so #1281 alone doesn't trigger, but two
  merged PRs plus one OPEN would.
- **Rollup annotation:** every ADD_CANDIDATE_INDIRECT records `open_pr_contributions:
  [pr_id, ...]` so a later reviewer can see the routine was driven partly by
  not-yet-merged work.

**Verdict:** the algorithm handles OPEN PRs by weighting them down, not by excluding
them. Alternative (exclude entirely) is defensible and simpler; the weighted rule is
preferred because it responds faster to fresh signal at the cost of one extra
attribute per candidate row.

---

## New prompt authoring constraints

Every rotation add must be a `.md` file matching the shipped schema
(`scripts/claude_md_eval/prompts/*.md`). Constraints are **fails-closed** — if the
authoring step can't satisfy them, the candidate defers to next week.

1. **Plural-key frontmatter only for tool_order.** `tool_order_before_tools`,
   `tool_order_after_tools` (comma-separated string OR list). Do **not** emit the
   deprecated singular keys — the plan doc's *Indirect tier* section widened the
   matcher; new prompts must use the widened form.
2. **`_ensure_eval_session()` synthetic id.** No prompt-level opt-out. The synthetic
   id is a runner behavior, not a prompt field — authors don't touch it, but the
   authoring step should refuse to ship a prompt whose scoring would depend on
   `session=unknown` grouping (per the plan doc's *Scoring artefacts fixed*).
3. **Additions-only regex safety.** Both `compliant_signal` and `anti_signal` are
   evaluated **against `+`-prefixed lines only** (per `_additions_only` in the shipped
   runner). Authors must therefore verify that neither regex matches text pasted **in
   the prompt body itself** that the agent will minimally edit. Concretely:
   - If the prompt body pastes a broken snippet using `zarr.open(...)` and asks the
     agent to fix it, the agent's minimal-edit diff will retain some untouched lines
     (which don't appear as `+`) — good. But if the agent DOES touch the offending
     line (e.g., adds a newline), the anti_signal fires against it — **bad, false
     negative**. Fix: the anti_signal must be tight enough (e.g., `(^|[^_])zarr\.open`
     as in `crop-failure`) that the agent's *own* import handling doesn't
     collateral-hit.
   - The `compliant_signal` must not match text the agent didn't type. If the prompt
     body pastes `# TODO: use zarr_utils`, the without-arm agent that leaves the
     comment untouched still gets its diff-additions-only pass thanks to
     `_additions_only` — but if the agent re-flows the file, the comment becomes an
     addition and the regex fires. Rule: **never paste the helper name in a prompt
     body, only its category**.
4. **Anti-signal defaults to a sentinel** for prose-only rules with no ratchet
   (`cite-algorithm` uses `__NEVER_MATCHES_SENTINEL__`). The "no citation added"
   run scores noncompliant via the `neither matches` branch — do not invent a false
   anti-pattern to force asymmetry.
5. **Ratchet ownership declared.** The frontmatter `scored_by` field (used in the
   plan doc's initial catalog examples) should be set when the rule has a ratchet;
   the routine's retirement decision reads this to decide whether a `1.0` streak
   means "the ratchet already catches drift" (retirement candidate) or "the eval IS
   the enforcement" (rewrite-only, never retire — `cite-algorithm`, `discovery-first`
   fall here).
6. **`rule_section:` frontmatter must resolve to a real anchor** in a
   `CLAUDE.md` or `docs/**/*.md` heading. The rollup already links to it; a broken
   anchor turns the failing-rule callout into a dead link.

**Fails-closed check** (add to the rotation runner in the same PR that ships the
routine): a `pixi run claude-md-eval-lint-prompt <path>` command that parses the
frontmatter, runs the regexes against the prompt body itself, and refuses if either
signal fires against pasted text.

---

## Direct vs indirect balance

**Definitions.** *Direct* = the canonical helper is named in the prompt body
(`h5ad-read` names `LabelPropsView` implicitly by mentioning `.h5ad` + `label` +
`speed` col — very close to naming it). *Indirect* = the canonical helper is not
mentioned and must be discovered (`crop-failure`, `discovery-first`, and by design
any button-arming successor). The plan doc's *Indirect tier (2026-09-28)* section
established this split.

**Current state.** A: 4 direct, 1 canary (canary is neither). R\_t: 5 direct, 2
indirect (`crop-failure`, `discovery-first`). Suite total 9 direct : 2 indirect ≈ 4:1.

**Target ratios.**

| Set | Now | End of Q4 2026 | End of Q1 2027 |
|---|---|---|---|
| Anchor A | 4:1 | 4:1 | 4:1 (stable) |
| Rotation R\_t | 5:2 | 4:3 | 1:1 |

**Shift rule.** Each quarter, the first rotation add that clears Step 5 as an
indirect candidate displaces one direct rotation prompt (not an anchor). This keeps
the shift bounded and PR-driven — no forced indirects if the PR history doesn't
produce ≥3-count clusters. If a quarter produces zero indirect candidates, the target
slides one quarter without penalty.

**Why not 100% indirect.** The plan doc's *Indirect tier* section flagged that
direct prompts are gameable ("both arms grep the name and score high"), but the
inverse holds too: 100% indirect prompts are noisy — an agent that discovers a
non-canonical-but-adjacent helper (e.g., writes its own `_dir_size` in a subprocess)
scores `noncompliant` even when its behaviour is correct. Direct prompts are the
signal floor. Anchors stay direct-heavy because their job is trend-comparability, not
maximum-realism.

---

## Cost and cadence

### Baseline numbers

- **Tonight (2026-09-29, N=3, WITH-arm only, 12 prompts):** $17.09.
- **Per-prompt average:** ~$1.42.
- **Per-run-across-arm-and-N (12 prompts × 3 runs):** ~$0.47/run.
- **Plan-doc estimate (*Cost model*):** $1–2/pass. **~10× low.** Flag and correct.
- **Cause of gap:** the plan-doc estimate assumed ~2000 output tokens per run at
  sonnet rates. Actual runs on the indirect tier and the tool-order-scored prompts
  (discovery-first, crop-failure) execute multi-tool trajectories with Grep/Read
  loops that expand both input (cumulative transcript) and output (agent's
  reasoning). Tool-loop cost was not modelled.

### Cost cap

- **WITH-arm weekly suite: $20 hard cap.** Sized as tonight's cost + one rotation-add
  headroom. If a rotation add pushes the projected cost above $20, the algorithm's
  Step 5 forces a paired removal — this is the "every addition needs a paired
  removal" rule from the prompt's *What NOT to do*.
- **Ablation (WITH + WITHOUT) budget: $30/month separate line.** Runs on-demand only,
  not weekly. Trigger: (a) canary is fresh (≤7 days), (b) a HEALTH_ALERT on an anchor
  fires, or (c) CLAUDE.md gained/lost a rule since the last ablation. Rationale: D12
  says "read at least one trace per arm before quoting Δ" — a weekly ablation without
  trace inspection wastes ~$34 (2× the WITH-arm cost) on numbers nobody reads.
- **Budget guard.** The cron pass emits `claude_md_eval_cost` per run; a running
  4-week sum is rendered in the rollup. If the sum crosses $80/mo the routine emits a
  BUDGET_ALERT and defers the next add.

### Cadence

- **Weekly cron: unchanged from plan doc *Cadence*.** Monday 23:59 local, WITH-arm
  only, `pixi run claude-md-eval` under `nice -n 10 ionice -c 3` + lockfile +
  `ConditionACPower=true` + no `Persistent=true`.
- **Rotation step runs immediately before the eval pass** (same cron unit), so a
  weekly `D_t` composition is decided against the same 30-day event window the eval
  measures against. Rotation adds/removes are logged as
  `claude_md_eval_rotation` events for post-hoc audit.
- **Manual triggers** for material CLAUDE.md edits stay as-is.

**Nothing in the budget-constrained-eval literature I searched offers a more
principled rule than the paired add–remove cap. IRT-based item selection (S3) is the
principled alternative in theory but requires many-respondent data we don't have.
Cite the paired-cap as ours; cite Yoo & Harman §4 for the framing.**

---

## Decisions

- **D-R1.** `D_t = A ∪ R_t`, `|D_t| ≤ 12`, `|A| = 5`, `|R_t| ∈ [5,7]`. Anchors frozen
  through 2026.
- **D-R2.** Distillation over escalation. `cite-algorithm` and `discovery-first`
  rewrite, do not retire — both are rule-without-ratchet where the eval IS the
  enforcement.
- **D-R3.** Retire only when a prompt hits `1.0 × 3 weeks` AND its rule shows ≥3
  real findings in the same window. Otherwise, mark for REWRITE.
- **D-R4.** Add only when a rule shows ≥3 findings (log route) OR a PR cluster shows
  ≥3 distinct merged PRs, with OPEN PRs weighted 0.5.
- **D-R5.** Direct:indirect targets 4:3 by end-Q4 2026, 1:1 by end-Q1 2027 in R\_t;
  A stays 4:1.
- **D-R6.** WITH-arm weekly cap $20; ablation $30/month separate. Paired
  add–remove enforced by Step 5.
- **D-R7.** OPEN PRs contribute to cluster count at weight 0.5 with mandatory
  `open_pr_contributions` annotation on the resulting candidate.

## Open questions

- **OQ-R1.** Should the anchor set itself be re-selected at year end, or does
  freezing forever risk anchor-decay? Proposal: year-end review + 1-slot swap
  budget; a full anchor replacement invalidates the year-over-year trend line and
  should be treated as a re-baseline event.
- **OQ-R2.** The `symptom_shape` clustering in Step 4 needs a concrete grouper. First
  cut: exact-match on `payload.desc[:80]` prefix + shared filename token. A better
  version would embed the finding text and cluster by cosine similarity, but that's
  fresh scope. If Step 4 misses obvious clusters in the first month of running, revisit.
- **OQ-R3.** Do we ever want *multi-model* rotation (Sonnet AND Opus) to get an IRT-shaped
  discrimination signal (S3)? Cost: 2×. Payoff: an actual item-discrimination axis. Park
  until we've run the routine on one model for a quarter — premature IRT with two
  respondents is noise.
- **OQ-R4.** `pixi run claude-md-eval-lint-prompt` is proposed above but not built. It
  should ship in the same PR that first triggers a rotation add — building it earlier
  is speculative.
- **OQ-R5.** The plan doc's *Non-goals* excludes measuring the fanout/convention
  reviewers themselves; but this routine reads their `_finding_resolved` rows as
  ground truth. If a reviewer regresses (false-positive-heavy), the routine adds
  bad prompts. Mitigation: the paired-cap keeps the blast radius to one prompt/week,
  and rotation removals happen. But there is no explicit reviewer-quality guard here
  — a proper fix would gate Step 3–4 on a reviewer-precision estimate. Deferred.

## Cadence (mirror of plan doc's structure)

- **Monday 23:59 (cron):** rotation step → WITH-arm suite → rollup. Cost budget
  $20/pass.
- **On-demand (Dominik):** ablation, after canary refresh AND HEALTH_ALERT / CLAUDE.md
  edit. Cost budget $30/mo.
- **Manual `pixi run claude-md-eval`:** unchanged, before/after material CLAUDE.md edit.
- **Quarterly review:** confirm anchors, review direct:indirect target-shift, review
  cost trend.
- **Yearly (2026-12-31 → 2027):** re-baseline event; 1-slot anchor swap budget per
  OQ-R1.

---

## Source-transfer paragraph (closing)

**Reference 1 (LiveBench)** transferred as *structural warrant for retirement on
saturation*; the contamination mechanism did not transfer (nothing is training on our
prompts) — cited only for the discriminative-power argument. **Reference 2 (Static→
Dynamic survey)** transferred as the `D_t` framing itself; specific `D_t` schedules
in the survey did not transfer. **Reference 3 (Wang et al., red-team)** transferred
as the distillation-over-escalation verdict; the strategic-economics setting did not
transfer, and the argument was reinforced by the single-contributor constraint.
**Reference 4 (agent red-teaming competition)** contributed nothing to this design
beyond cross-reference; scoped out per prompt guidance. **S1 (Yoo & Harman 2012)**
transferred as the minimization–selection–prioritization vocabulary + the
history-window rule of thumb; coverage-based selection did not transfer. **S2
(IFEval)** transferred as validation of the verifiable-instruction scorer pattern;
its scale and single-turn synthetic-prompt shape did not transfer. **S3 (ATLAS)**
transferred as the *discrimination vs difficulty* distinction that motivates
per-prompt variance as a retirement tiebreaker; IRT itself did not transfer for lack
of respondent-matrix data. **S4 (PSN-IRT)** transferred only as qualitative
corroboration of S3; no numerical threshold was extracted because the full paper
wasn't read this pass. **S5 (SZZ)** transferred as *methodological warrant* for
mining real PRs to seed test candidates; the specific fix-to-introducer algorithm
did not transfer.
