# Audit: CLAUDE.md compliance and enforcement-coverage

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `CLAUDE.md`, `docs/MAINTAINABILITY.md`, and
> `docs/todo/*_PLAN.md`.

**Model:** Opus.
**Type:** Compliance audit against Anthropic's own stated best practices,
grounded in a specific direct read of `CLAUDE.md` (branch
`feat/findings-emission`, 359 lines / 25.5 KB). Not a hypothesis to weigh —
a directive to check ourselves against, using four concrete findings already
identified as the starting evidence. No code changes in this pass; produce
a change list for a follow-up PR.

## The directive

Anthropic's own published best-practices guide and their team's own public
statements about how they run Claude Code internally say:

1. **CLAUDE.md instructions are advisory; hooks are deterministic.** "Use
   hooks for actions that must happen every time with zero exceptions."
2. **Over-specification actively hurts.** "If your CLAUDE.md is too long,
   Claude ignores half of it because important rules get lost in the
   noise... If Claude already does something correctly without the
   instruction, delete it or convert it to a hook."
3. **Their own system prompt was cut ~80%.** Adding examples to a system
   prompt is reportedly no longer best practice for their newest models,
   and lists of "don't do X and don't do Y" can reduce output quality on
   the latest models.
4. **Even at their scale, the same failure mode is still live for them.**
   Their own reporting names maintaining architectural integrity across
   fast-growing codebases as an unsolved, ongoing problem — not something
   solved by moving to bigger models. They still keep critical changes
   under manual review and only automate the "outer layers."

Take (1)–(3) as the standard we're checking `CLAUDE.md` and
`docs/MAINTAINABILITY.md` against. Take (4) as the reason we shouldn't
assume the fix is simply "delete the rules and trust the model more" —
Anthropic's own experience says drift at scale doesn't go away on its own
even for them.

## Starting evidence: four concrete findings from a direct read

A close read of `CLAUDE.md` found the file is not generically
over-specified — it's mostly single-instruction rules, not
don't-do-X-and-Y lists, and the nested-`CLAUDE.md`-per-area architecture
already keeps out-of-scope rules out of context. The actual finding is
different: **enforcement coverage is uneven** — some rules got the full
hook/test treatment, structurally identical sibling rules didn't. Verify
each of these four before doing the broader inventory below.

1. **The discovery-first rule has no write-time enforcement, and the cost
   calculus around that has never been revisited.** "Before implementing
   anything — mandatory discovery step" is pure prose — no `PreToolUse`
   hook checks it. This is the entire reason `FANOUT_AUDIT.md` and
   `CONVENTION_CHECK.md` exist as a post-hoc catch net. #1233/#1235
   declined a write-time inventory gate on cost grounds at a specific point
   in time. Since then, two reviewer subagents, a closed outcome
   vocabulary, a pre-commit hook, and an effectiveness log have all been
   built as the catch-net workaround. **Re-run the cost comparison from
   #1233 with current numbers**: given what's now been spent building and
   maintaining the catch-net layer, is a write-time gate still not worth
   it, or has the accumulated downstream cost crossed the threshold #1233
   set? Pull actual build/maintain cost of the fanout+convention+log
   apparatus and compare honestly against what a narrow, scoped
   `PreToolUse` gate would have cost.

2. **Windows compatibility is the most list-dense, least-enforced section
   in the file.** Roughly 8 named helpers (`python_bin_path()`,
   `bioformats2raw_bin()`, `expand_user()`, `ensure_config_dir()`,
   `agent_bin_path()`, `_kill_tree`/`free_port`, `_dir_bytes`,
   `joinpath()`, UTF-8 encoding on Python text I/O), each justified with
   "this already caused a real bug," none with a cited test or lint —
   unlike the zarr-access rule immediately above it, which cites
   `test_zarr_access_convention.py` plus a ratchet testset. Each is a
   grep-able anti-pattern. For each helper: confirm whether an enforcement
   test already exists and the doc simply doesn't cite it, or whether none
   exists. For each confirmed gap, propose the specific grep pattern a
   lint/test could check.

3. **Confirm or refute the h5ad/zarr enforcement asymmetry.** The zarr
   section cites `test_zarr_access_convention.py` and a ratchet testset.
   The h5ad section ("Never touch `.h5ad` directly") is phrased with equal
   severity but cites no equivalent test. Search the test suite for
   anything that already enforces the h5ad convention. If one exists, this
   is a doc-currency bug — fix the citation. If none exists, this is a real
   category-(c) gap sitting next to its enforced sibling — propose the
   equivalent test.

4. **The recital mechanism enforces outcome-tag presence, not that a review
   actually ran.** `check_commit_recital.py` validates that commit messages
   carry a matching outcome tag per finding, but (per
   `FINDINGS_EMISSION_PLAN.md`'s own stated non-goal) does not verify a
   slug corresponds to a real `_finding` row from an actual
   `pixi run recital` invocation. Confirm this is still accurate. If a
   cheap check exists (e.g., checking whether an `_run` event was emitted
   to the effectiveness log within a short window before the commit),
   propose it. If no cheap check is feasible, say so explicitly and leave
   this as a named, accepted gap.

## Full inventory — everything else in the file

Beyond the four items above, sort every remaining distinct rule in
`CLAUDE.md` and `docs/MAINTAINABILITY.md` into:

- **(a) Already enforced mechanically** — a hook, CI check, or test
  (`test_vocabulary_matches_effectiveness_log_module` is one). Per
  directive (1), a mechanically-enforced rule doesn't also need to exist
  as prose — flag redundant restatement.
- **(b) Prose-only, no enforcement, model reliably follows it anyway.**
  Per directive (2), this is pure cost with no benefit — candidate for
  deletion. Requires actual evidence (checked PR/commit history for
  whether this rule has ever needed correcting), not an assumption. If no
  evidence either way, say so rather than guessing.
- **(c) Prose-only, no enforcement, direct evidence the model does NOT
  reliably follow it unprompted.** Items 1–4 above are the known instances
  of this category; check for others. For every rule here, the
  compliance-correct move per directive (1) is conversion to a hook, or an
  honest note that no hook is feasible yet and the rule stays as a known,
  accepted gap — not deletion. Deleting a category-(c) rule because "the
  guidance says be less prescriptive" is a misapplication of the
  directive, not compliance with it.

Also check, separately from the categorization above:

- **List/example density.** Directive (3) is about don't-do-X-and-Y lists
  and heavy examples specifically, not instruction count in general. Item
  2 above (Windows compatibility) is the confirmed worst offender — check
  whether anything else in either file has similar list-shaped
  accumulation.
- **Model class the directive was reported for, versus what we run.** Our
  reviewers are deliberately pinned to `sonnet`
  (`FANOUT_AUDIT.md`/`CONVENTION_CHECK.md`, Decision 1), to avoid Opus
  over-reasoning on tight tasks. Anthropic's density finding is about "the
  latest models." State plainly whether you think the finding transfers to
  `sonnet`-run sessions or is likely calibrated to a different model class
  — don't assume either way.

## Produce the change list

For every rule (the four items above plus anything found in the full
inventory), one of:

- `KEEP` — mechanically enforced already, or prose is load-bearing with no
  enforcement path yet (state which, and why no hook is feasible).
- `DELETE` — category (b), with the evidence checked.
- `CONVERT` — category (c) → propose the hook/test that would enforce it,
  or state explicitly that conversion isn't feasible yet and the rule
  stays as an accepted, named gap.

## What NOT to do

- Do not propose deleting or shortening any of the four starting-evidence
  items. All four are enforcement gaps or asymmetries, not verbosity
  problems — the fix is conversion or citation correction, not pruning.
- Do not treat "Anthropic said shorten it" as license to delete any rule
  that feels redundant on a skim. Every `DELETE` needs the evidence check,
  not a vibe.
- Do not delete the discovery-first rule, or any other category-(c) rule,
  without either converting it to something enforced or explicitly
  flagging the resulting gap. Silently dropping a rule with known-bad
  prose-only compliance trades a directive-compliance win for a real
  regression — say so if you find yourself tempted to do that.
- Do not treat "the nested-CLAUDE.md architecture already limits context"
  as a reason to skip per-rule enforcement checking. Structural density
  management and per-rule enforcement coverage are independent — confirm
  both.
- For item 1 specifically: do not re-litigate the fanout/convention-check
  design itself. The question is narrowly whether the *original* declined
  write-time gate is worth reopening given accumulated costs.
- Do not treat this as settled once the change list ships. This is a
  compliance snapshot against guidance reported at one point in time, for
  a different codebase. Name what would prompt a re-audit.

## Output format

- Items 1–4, each resolved per their specific instructions above (cost
  comparison, enforcement table, asymmetry verdict, gap confirmation).
- The (a)/(b)/(c) inventory for everything else, with actual counts, for
  both files.
- The full change list (`KEEP` / `DELETE` / `CONVERT`), one line per rule,
  each with its category and evidence.
- A short summary: net line-count reduction if the change list ships, and
  how many rules move from prose-only to mechanically enforced.
- One paragraph: does closing these gaps materially reduce the odds of a
  repeat of a known incident type (the `_OUTCOME_TAGS` drift, a
  Windows-only bug, a raw h5ad access bug), or are some low-value busywork
  relative to fix cost — say which is which. Also state plainly whether
  following this directive, done correctly, makes the system more or less
  likely to catch what `FANOUT_AUDIT.md`/`CONVENTION_CHECK.md`/the
  effectiveness log already exist to catch — flag immediately if any
  proposed change would trade compliance for drift resistance.