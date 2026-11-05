> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/ai-assist/RESULT_REVIEW.md`](../ai-assist/RESULT_REVIEW.md),
> [`docs/ai-assist/FANOUT_AUDIT.md`](../ai-assist/FANOUT_AUDIT.md) and
> [`docs/todo/RESULT_REVIEW_PLAN.md`](../todo/RESULT_REVIEW_PLAN.md).
>
> **Outcome:** not run. Archived 2026-11-05; no `RECITAL_ORCHESTRATION_AUDIT.md` exists. It asked
> for the report at the repo root; audit records belong in `docs/archive/`. A first read against
> `origin/main` (fe1c91d7) already corrects some of the claims it lists:
>
> - **Claim 5 (it can't fix code):** partly. It runs in a copy of the staged tree with
>   `Write,Edit,Bash` (`recital.py` `RESULT_REVIEW_TOOLS`), so it writes and runs repros there. It
>   can't touch the worktree, and its prompt says not to suggest fixes.
> - **Claim 6 (numbers):** two different runs. The backtest took 85–130 s and 50–80k tokens per
>   Opus review, with no dollar figure. 70 s and $0.87 came from one live run through the real
>   mechanism on the `34c919f6` case. "3 of 3" means the known bugs; the "4 bugs" are those three
>   plus the px/µm default the reviewer found live on main (`RESULT_REVIEW_PLAN.md` → *Backtest*).
> - **Claim 7 (pinned model):** partly. `--model opus` is an alias, not a version
>   (`claude_cli.MEASURED_MODEL`); each run row logs the resolved `models`. Since e02a8cdb **all**
>   recital reviewers run on Opus, not just this one.
> - **Claim 11 (no plan-approval gate):** no gate is documented in `CLAUDE.md`, `docs/DEV.md` or
>   `docs/ai-assist/`. The human gates written down are "ask before every commit / PR" and the
>   outcome-tag hook. Not checked further.
> - Since this was written, repeat rounds review only the change since the last round (#1949).

# RECITAL_ORCHESTRATION_AUDIT_PROMPT

Audit task. Read-only: do not modify code, config or docs. Write findings to `RECITAL_ORCHESTRATION_AUDIT.md` at the repo root.

## Why

We may write a perspective article on AI-assisted development of scientific software, using Cecelia (Feijoa, Julia + Vue) as the worked example. I compared recital with a public "multi-agent workflow" article and wrote down my understanding below. That understanding comes from memory of PR #1921 and from the article, not from reading the code. **Treat every claim as unverified. Check each against the actual repo and say where I am wrong.**

## The article's pattern (external, for comparison)

Source: https://unlearn.dev/blog/build-your-first-multi-agent-workflow (summary only, a course promo)

- Problem it targets: context rot. A long single session fills with files and searches, and key decisions get buried.
- Three agents, each with a fresh context, returning a short summary:
  1. Planner: reads code, writes the plan, never touches code.
  2. Implementer: follows the plan, builds, gets tests passing.
  3. Reviewer: checks the diff against the plan, not allowed to fix anything.
- One command (`/build-feature`) runs all three and pauses for human approval of the plan before any code is written.
- Skip orchestration for trivial changes (typo, rename); extra agents cost tokens.
- No validation data. Only a CLI-vs-MCP token anecdote.

## My understanding of recital (claims to verify)

Mark each: CONFIRMED / WRONG / PARTIAL / NOT FOUND, with `path:line` evidence and the correction if wrong.

1. `result_review` (PR #1921) is a third reviewer in recital, aimed at silent wrong results (code that runs, passes tests, and gives scientifically wrong output).
2. It is a single reviewer per run, not multiple reviewers with consensus.
3. Findings are classed `reproduced` (backed by a failing test) vs `inferred` (no repro).
4. A `reproduced` finding requires an executable failing test as evidence.
5. The reviewer cannot fix code, only report.
6. Backtest: it caught 3 of 3 (or 3 of 4, check the PR text) known silent-wrong-result bugs. Cost about $0.87 and about 70 s per run on Opus. Find the exact numbers and how the backtest was run.
7. The model is pinned.
8. A weekly sweep or judge workflow acts as a second filter on `inferred` findings.
9. Each reviewer runs in a fresh context, not the implementing session.
10. It triggers only on changes touching result paths, not on every commit. Find the actual trigger rule.
11. Recital has no planning stage and no human plan-approval gate. (Uncertain. Check hard.)
12. Reviewers check results and evidence, not conformance to a plan.

## Tasks

1. **Verify claims 1-12** against code, prompts, hooks, scripts, CI, docs and PR #1921. Include the other recital reviewers if they exist and what each checks.
2. **Map recital onto the article's three roles** (planner / implementer / reviewer). Which roles exist, which are missing, which are covered by something else (CLAUDE.md, session-gate, console, worktree flow)?
3. **Identify real differences**, not cosmetic ones. In particular:
   - What failure class does recital catch that a plan-conformance reviewer cannot?
   - What does the article's pattern catch that recital does not (plan drift, context rot, wrong-plan-right-code)?
4. **Context handling:** how does recital keep key decisions from being buried in long sessions? Compare with the article's fresh-context-per-role approach.
5. **Human gates:** list every point where a human must approve. Is there a plan-approval gate before implementation? Should there be, and for which size of change?
6. **Triplicate/consensus question (secondary):** given what the code does today, would running 3 reviewers and taking consensus help? Consider that `reproduced` findings are self-verifying, that voting trades recall for precision, and that same-model reviewers have correlated errors. Is there any existing data on run-to-run variance? If not, propose the minimal experiment (e.g. rerun the backtest 3 times) and what result would change the decision.
7. **Borrow list:** concrete, small changes worth adopting from the article, each with the files it would touch and a size estimate. Also list anything in the article that would be a bad fit for Cecelia.
8. **For the perspective article:** list the claims about recital that are supported by evidence in the repo (backtests, logs, PR text) versus claims that would need new evidence. Note any numbers that could not be reproduced.

## Output format

`RECITAL_ORCHESTRATION_AUDIT.md`:

1. Summary (10 lines max): how recital differs from the pattern, top 3 findings.
2. Claim table: # | claim | verdict | evidence (`path:line` or PR link) | correction.
3. Role mapping table.
4. Differences, context handling, human gates (short prose each).
5. Consensus question: recommendation and minimal experiment.
6. Borrow list (ordered by value/effort).
7. Evidence inventory for the perspective article.
8. Open questions I need to answer.

## Rules

- Cite `path:line` or a PR/commit for every factual statement about our code.
- If you cannot find something, write NOT FOUND. Do not infer it exists.
- Do not trust the PR description over the code; if they disagree, report both.
- Do not trust my claims above. Correct them plainly.
- Do not make recommendations that need new features unless the evidence from the code supports them.
- Keep it terse. No filler.
