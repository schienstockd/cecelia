# Worktree cleanup — `pixi run prune-worktrees`

Status: **in-progress** (2026-10-10) on `prune-worktrees` — P1 built (report, `--remove`, bootstrap nudge, CLAUDE.md reminder); P2 not started.

## Goal

One command that says which sibling worktrees can go, and removes only the ones whose removal
cannot lose work. Everything that needs a judgement call is listed with enough evidence that the
review is short, and is left to Dominik or an agent.

## Why

Dominik asked an agent to clean up worktrees about 14 times between 2026-09-18 and 2026-10-09.
Every session re-derived the criteria, and the same traps came back:

- **Squash merges** — a merged branch is not an ancestor of `origin/main`, so an ancestry check
  alone calls it unmerged. The PR's merged head is the test.
- **Commits pushed after the merge** (#1330) — the PR says merged but the local HEAD has more.
- **Merged and clean is not unused** — Dominik runs `pixi run dev` and the consoles from feature
  worktrees.
- **State shifts mid-audit** — other sessions commit into a worktree between the scan and the
  removal (2026-09-26).
- **Compile junk counted as work** — stray `frontend/src/**/*.vue.js` files made merged trees
  look dirty (2026-09-26).
- **Worktrees nested inside `cecelia-feijoa`** (2026-09-28).
- **The permission classifier blocks `--force`, `branch -D` and `kill`** — so those were always
  going to be Dominik's `!` commands anyway (2026-09-28, 2026-10-09).

The scheduled run of 2026-10-07 wrote the criteria down properly (27 removed, 24 GB freed, nothing
forced). This plan turns that prompt into code.

On 2026-10-10, 135 of 154 registered worktrees were dead entries at `/tmp/…/judge-worktree`, left
by three tests in `test_judge_weekly.py` that ran the live judge pass against the real repo (fixed
separately on branch `fix-judge-worktree-leak`); this tool only prunes the entries.

## Decisions (2026-10-10)

1. **Python, in `scripts/prune_worktrees.py`, run as `pixi run prune-worktrees`.** Branch and PR
   discovery reuse `python/cecelia/effectiveness/git_context.py`, the one canonical answer for
   that job. `scripts/bootstrap_worktree.jl` is Julia, but it shares no logic with this.
2. **Report by default. `--remove` acts on SAFE and DEAD only.** SAFE is defined so that removal
   cannot lose work; nothing else is ever removed by the tool.
3. **Never `--force`, never `git branch -D`, never kill a process.** For anything that needs one,
   the report prints the exact command for Dominik to run with `!`.
4. **SAFE = all of:**
   a. *Merged*: HEAD is an ancestor of `origin/main`, **or** the branch has a merged PR whose
      `headRefOid` equals HEAD (covers squash merges and catches commits pushed after the merge).
   b. *Clean*: `git status --porcelain --untracked-files=all` is empty (ignored files such as
      `.pixi`, `node_modules` don't show there). JUNK = only untracked `.vue.js`/`.vue.d.ts` files,
      or `.js`/`.d.ts` files next to a tracked `.ts`, under `frontend/`.
   c. *Unused*: no process has its cwd, exe or cmdline under the worktree path.
   d. *No stash* whose message names the branch.
5. **Buckets**, one per worktree, first match wins: PRIMARY, DEAD (folder gone), LOCKED
   (`git worktree lock`), OTHER (outside the primary checkout's parent folder — e.g. the judge's
   persistent `~/.cecelia-effectiveness/judge-worktree`, which must survive), FRESH (created under
   24 h ago), IN USE, UNMERGED, STASHED, JUNK (dirty only with compile output), DIRTY, SAFE.
   Nested worktrees (inside `cecelia-feijoa`) are inside the parent folder, so they get the same
   rules as siblings.
   - *Why FRESH:* a just-bootstrapped worktree has no commits or edits yet, so it reads as merged
     and clean. Without the age guard the tool would remove it from under the agent working in it.
6. **Re-check every SAFE worktree immediately before removing it**, and skip it if anything changed.
7. **Branches are deleted with `-d` only.** A squash-merged branch that `-d` refuses stays; a
   branch costs nothing on disk.
8. **The process check reads `/proc`, so it is Linux-only.** Where `/proc` is missing every
   worktree reads "in use: unknown", so nothing is SAFE — the safe failure.
9. **`gh` unavailable ⇒ a non-ancestor branch is UNMERGED ("PR state unknown")**, never SAFE.
10. **Pure classifier.** Gathering facts (git, gh, `/proc`) is separate from deciding the bucket;
    tests drive the classifier with fake facts and never touch the real repo.

## Phases

- **P1 — report + safe removal.** Fact gathering, classifier, table report with `df` before/after,
  `--remove` for SAFE (`worktree remove` + `branch -d`) and DEAD (`git worktree prune`). Unit
  tests on the classifier; one hermetic test against a throwaway repo. `docs/DEV.md` section next
  to *Creating a new worktree*.
- **P2 — shorter review of the rest.** Per DIRTY/UNMERGED worktree: diffstat, and whether each
  change is already on main (`git cherry` for commits, patch comparison for uncommitted work).
  JUNK gets its `rm` + `worktree remove` command printed. IN USE lists the PIDs and commands.
- **P3 — only if still asked weekly.** A scheduled 5 am run. Not built unless the one-command
  version still leaves Dominik asking.
