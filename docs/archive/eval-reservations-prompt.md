> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/todo/CLAUDE_MD_EVAL_PLAN.md` and its `CLAUDE_MD_EVAL_*` siblings.
>
> **Outcome:** reworked 2026-10-02 into `docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md` →
> Decisions 16–17, against the shipped pipeline and the 2026-10-01 pass. Item 1 became "the timer
> runs from the supervisor's worktree" (a behind-or-dirty check on the dev checkout would fail most
> weeks). Item 2 rescores the previous pass instead of flagging the scorer. Item 3 was dropped: all
> three F4 traces read `slice_utils.py`, so the guard would not have fired. Item 4 shipped as labels
> only; N=10 escalation is deferred. Item 6 landed on #1347.

# Task: harden the CLAUDE.md eval pipeline before the next rehearsal

Context: the first supervised rehearsal (PR #1347, run `2026-09-30`) worked but exposed gaps in how the pipeline is trusted and operated. Fix the items below in order. All changes are to the eval tooling (`scripts/claude_md_eval/`, `cron_pass.sh`, tests, docs). Do not touch `CLAUDE.md`, `docs/DEV.md`, `slice_utils.py`, `jobs.jl` or any probe premise. Setup fixes P1–P4 and the F6 decision are out of scope, so that scorer changes and setup changes stay in separate passes.

Work in a branch off `main`. One commit per item. Follow existing conventions (`clean_env`, `run_py`, `joinpath`, Windows compatibility rules in CLAUDE.md).

## 1. Preflight in the live path (highest priority)

`cron_pass.sh` runs from `~/cc-workspace/cecelia/cecelia-feijoa`, which was behind main with no `supervise.py`, so the timer would silently have run the bare suite.

- Before any pass, fail loudly (non-zero exit, clear message, no record written as a "pass") if:
  - the checkout is behind `origin/main` (after `git fetch`), or
  - `scripts/claude_md_eval/supervise.py` is missing, or
  - the working tree has modified tracked files outside the eval worktree.
- Test with a fixture repo for each failure case.
- Verify: unit tests, plus `cron_pass.sh --preflight-only` against a deliberately stale checkout.

## 2. Attribution: scorer vs setup changes

Today a score change can't be attributed when the scorer, the sandbox, and the setup move together.

- Record in each run record: scorer SHA, CLAUDE.md blob, prompt set hash, sandbox mode, and the pinned SHA. The pinned SHA currently reads "not recorded (replayed from the log)", so fix that for live passes.
- In `delta()`, compare these against the previous record. If more than one of {scorer SHA, CLAUDE.md blob, prompt set hash, sandbox mode} differs, render a visible **"confounded: <list>"** line at the top of the delta and do not label any score change as improvement or regression.
- Tests for: none changed, one changed, several changed.

## 3. Probe-premise guard

F4 showed a probe passing 3/3 on a false premise (discovery alone satisfied it).

- Add an optional per-prompt `target_files` list in the prompt frontmatter.
- When a prompt scores 3/3 and none of its traces read or modify any listed target file, emit a `probe_suspect` finding (class `decision`, owner queue) saying the probe may be passing without exercising its target.
- Backfill `target_files` for existing prompts only where it is unambiguous from the prompt text; leave the rest unset and list them in the PR description.

## 4. Noise labelling for small N

N=3 over 9 prompts cannot show trends.

- In the results table and delta, mark a prompt result of 1/3 or 2/3 as `noisy`.
- Never state an aggregate score change as a trend in rendered markdown. Show the aggregate, but have `delta()` report only per-prompt transitions between 0/3 and 3/3 as changes.
- Add a `--n` override per prompt so a noisy prompt can be re-run at higher N without re-running the whole catalog. Cost is logged per run, so print the estimated cost before running.
- Run-count policy:
  - Default screen is N=3 for every prompt.
  - Any prompt that lands on 1/3 or 2/3 is escalated to N=10 (the extra runs, not a full re-run). Prompts at 0/3 or 3/3 are not escalated.
  - Escalation is a separate step from the screen. In supervised passes, do it automatically but cap total escalation cost per pass (configurable, default $10) and report any prompts skipped because of the cap.
  - A before/after comparison across a setup change is only trusted when the same prompt has N=10 on both sides. Otherwise `delta()` labels it a hint, not evidence.
  - Escalated runs are recorded as part of the same prompt's result (e.g. 7/13), with the screen and escalation counts both visible.
  - Rationale for the PR description: at N=3 a 70% prompt shows 3/3 about a third of the time, so 1/3 or 2/3 can't separate flaky from broken. N=10 on only the noisy prompts costs roughly $4–6 for the most expensive one (`kill-process-tree`) versus about $37 to run everything at N=10.
- Tests: escalation triggers only on 1/3 and 2/3, respects the cost cap, and merges counts into one result.

## 5. Setup size budget

- Add a configurable budget (default: current CLAUDE.md line count + 10%, stored in the eval config) for CLAUDE.md and `frontend/CLAUDE.md`.
- When a pass finds either over budget, add a finding (class `decision`, owner queue). It never blocks the pass.
- The "Setup size" section of the rendered record shows used vs budget.

## 6. Cleanups from PR #1347 review

- Add a test for the backslash branch of `clean_env` (Windows PATH entries like `C:\x\.pixi\envs\y\bin`). The existing test only uses POSIX paths.
- Remove the now-redundant `not args.session` check near `supervise.py:550`.

## Constraints

- Sandbox, `--dangerously-skip-permissions` handling and the 4h unit timeout stay as they are.
- No new dependencies.
- Run `pixi run test-pkg` and the eval tests before each commit.

## Done when

- All six items are committed with tests passing.
- A second rehearsal (isolated store, N=1, no PR) exits 0 and the record shows the new fields, including the confounded line if applicable.
- The PR description lists each item, what was verified, anything left unset (item 3), and any decision you want from the owner.
