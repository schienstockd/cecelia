# Worktree cleanup — `pixi run prune-worktrees`

Status: **in-progress** (2026-10-10) on `prune-worktrees` — P1 built (report, `--remove`, bootstrap nudge, CLAUDE.md reminder); P1b planned (2026-10-13: Claude-session gate, actionable nudge); P2 not started.

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

## Decisions (2026-10-13) — P1b

Context: a proposal to grow this tool into a per-worktree status table (PR, CI, recital findings,
parked plans, session state) was audited and dropped — every column is either in the current
report or shown natively by Claude Code. A narrower "list removable, prune on confirm" proposal
turned out to be P1 as built, with one gate that doesn't hold on Dominik's machine.

11. **Claude sessions count as IN USE — read from Claude Code's session files, not `/proc` cwd.**
    *Why:* decision 4c misses them. Dominik launches every session from `~/cc-workspace/cecelia`
    and works in a worktree by `cd`, so the `claude` process's cwd is the workspace root. On
    2026-10-13 all 7 live sessions had that cwd; the `/proc` scan only flagged worktrees where an
    MCP server or `pixi run dev` happened to run. Removal still cannot lose work (merged + clean),
    but it pulls the folder out from under an idle session.
    *How:* for each `~/.claude/sessions/<pid>.json` whose `procStart` equals field 22 of
    `/proc/<pid>/stat` (live, and not a reused pid), find the transcript
    `~/.claude/projects/*/<sessionId>.jsonl` and take the last `"cwd"` from its tail. That cwd
    tracks the session's `cd`s; `sessions/<pid>.json`'s own `cwd` is the launch directory and
    does not. Spike 2026-10-13: 6 of 7 live sessions resolved to a worktree, the 7th to the
    workspace root (correct). The reason line names the session: `session cecelia-56 (idle)`.
12. **The session files are an undocumented Claude Code format, so parsing fails safe per file,
    not open.** Missing `~/.claude/sessions/` = no sessions (Claude Code not used here; `/proc`
    still applies). A session file or transcript that exists but can't be parsed marks no worktree
    but prints one `! couldn't read Claude session <pid>` line under the report, so a format change
    is visible rather than silently dropping the gate. Read only the transcript's last ~64 KB.
13. **The bootstrap nudge says what is removable, not how many worktrees exist.**
    `prune-worktrees --summary` (no fetch, no report) prints one line —
    `3 removable, 2 dead — pixi run prune-worktrees --remove` — or nothing when both are zero.
    `bootstrap_worktree.jl` calls it instead of counting `git worktree list`; `NUDGE_AT` goes.
    *Why:* "48 worktrees" was noise when nothing could go. Cost measured: a full report run took
    4.5 s (17 worktrees, `--no-fetch`), small next to bootstrap's `pixi install` + `npm ci`. If
    the call fails, bootstrap prints nothing — the nudge is a hint, not a gate.
14. **No interactive pick-list.** SAFE is defined so removal cannot lose work, and agents run
    `--remove` non-interactively on Dominik's go-ahead (root `CLAUDE.md`). A y/n prompt would
    only add a step.
15. **Remote branches stay untouched.** `fetch --prune` already drops stale remote-tracking refs;
    deleting `origin/<branch>` is GitHub's job (auto-delete on merge), not this tool's.

## Phases

- **P1 — report + safe removal.** Fact gathering, classifier, table report with `df` before/after,
  `--remove` for SAFE (`worktree remove` + `branch -d`) and DEAD (`git worktree prune`). Unit
  tests on the classifier; one hermetic test against a throwaway repo. `docs/DEV.md` section next
  to *Creating a new worktree*.
- **P1b — session gate + actionable nudge** (decisions 11–15). The session reader is
  `python/cecelia/effectiveness/claude_sessions.py`, which it shares with the recital-console agents
  pane (`AGENTS_PANE_PLAN.md` P1, built first). It gives `(name, status, cwd)` per live session; `Facts` gains
  `sessions: list[tuple[str, str]]` and `classify` puts a non-empty one in IN USE ahead of the
  pid check. Tests: classifier cases with fake sessions; a parser test on a fixture `sessions/`
  dir + transcript in `tmp_path` covering a live match, a reused pid (`procStart` mismatch), a
  `cd` mid-transcript (last cwd wins) and an unparseable file (warning, no match). `--summary`
  gets a test on made-up rows. `bootstrap_worktree.jl`: swap `cleanup_nudge` for the
  `--summary` call; update root `CLAUDE.md` (*remind Dominik about worktree cleanup* → relay
  the removable count) and `docs/DEV.md` → *Removing worktrees*.
- **P2 — shorter review of the rest.** Per DIRTY/UNMERGED worktree: diffstat, and whether each
  change is already on main (`git cherry` for commits, patch comparison for uncommitted work).
  JUNK gets its `rm` + `worktree remove` command printed. IN USE lists the PIDs and commands.
- **P3 — only if still asked weekly.** A scheduled 5 am run. Not built unless the one-command
  version still leaves Dominik asking.
