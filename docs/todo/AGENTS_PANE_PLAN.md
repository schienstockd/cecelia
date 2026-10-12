# Agents pane — `pixi run recital-console` shows the live sessions

Status: **planned** (2026-10-13). Nothing built.

## Goal

One live view of agent work. Each running Claude Code session gets one row: which worktree it is
in, its branch, the PR with its CI state, whether the session is working or waiting for Dominik,
and how many recital rounds and open findings its branch has.

## Why

Dominik runs about 10 foreground sessions at once, one per worktree. The native tools cover part
of this, as checked on 2026-10-13:

| Want | Native |
|---|---|
| Live sessions, idle/busy | Yes: `/list-agents` |
| Worktree | No. Every session shows its launch folder, the workspace root |
| PR / CI | Only in Agent view, and only for background (`/bg`) sessions |
| Waiting for Dominik | Partly: Agent view's "needs input", background only |
| Recital rounds, open findings | No |

The earlier `claude-console` and session-bus briefs (`docs/archive/`) were dropped for their
extras: hooks, a message bus, window control. None of those is needed here.

## Decisions (2026-10-13)

1. **A pane in the existing `recital-console` dashboard, not a new tool.**
   `python/cecelia/effectiveness/console.py` already has the full-screen refresh loop, height
   budget, colours and `--stream` fallback. The new `── agents ──` pane goes at the top, above
   `── by mechanism ──`, because "which session needs me" is the first question.
2. **All read-only, and no hooks.** Every column comes from files that already exist:

   | Column | Source |
   |---|---|
   | session name, idle/busy | `~/.claude/sessions/<pid>.json` (`name`, `status`). Live when `procStart` equals field 22 of `/proc/<pid>/stat` |
   | worktree | the last `"cwd"` in the transcript `~/.claude/projects/*/<sessionId>.jsonl`. The `cwd` in the session file is the launch folder and never changes |
   | branch | `git_context.git_output("branch", "--show-current", cwd=<worktree>)` |
   | PR, CI, merged | **one** `gh pr list --state all --json number,headRefName,state,statusCheckRollup` per refresh, matched by branch, cached for 60 s. Add it as a new helper in `effectiveness/git_context.py`, next to `pr_for_branch` and `merged_pr_heads`, not as an inline `gh` call |
   | recital rounds | distinct `commit` values among the branch's `fanout_audit_run` events in the effectiveness log. `pixi run recital` writes one per round |
   | open findings | `_finding` rows for the branch that have no `_finding_resolved` row. This is the logic the commit hook already has (`.claude/hooks/check_commit_recital.py::_untagged_slugs_on_head`). Move it into `effectiveness/log.py` and call it from both places |

3. **One session reader, shared with prune-worktrees.** `python/cecelia/effectiveness/claude_sessions.py`
   returns `(pid, name, status, cwd)` per live session, reading only the last
   ~64 KB of each transcript. `scripts/prune_worktrees.py` imports it for its in-use check
   (`WORKTREE_CLEANUP_PLAN.md` decision 11), the same way it already imports `git_context`.
4. **The session files are undocumented, so failures show.** If a file can't be parsed, the row
   reads `? (unreadable)` rather than disappearing. A missing `~/.claude/sessions/` hides the pane.
5. **Rows are sorted by what needs Dominik:** idle with a failed CI run, then idle, then
   busy. Idle on a foreground session in practice means waiting for him.
6. **No plan-mode column.** Dominik keeps sessions in auto mode, so it would never change.
7. **`pixi run agents` is an alias** that opens `recital-console` with only the agents pane. It is
   for the second terminal.

## Phases

- **P1: reader and pane.** `claude_sessions.py` and its tests: a fixture `sessions/` dir plus
  transcripts in `tmp_path`, covering a live match, a reused pid, a `cd` partway through a
  transcript, and an unparseable file. Pane rendering is a pure function over fake rows. Wire in
  git and the effectiveness log; leave PR/CI blank.
- **P2: PR and CI.** One cached `gh` call per refresh. A missing `gh` leaves the columns blank.
- The prune-worktrees in-use check (`WORKTREE_CLEANUP_PLAN.md` P1b) uses the P1 reader, so P1 is
  built first.

## Open questions (default in brackets)

1. Should merged-PR sessions stay in the list? [Yes, dimmed. They are prune candidates]
2. Should a branch's open findings count every round or only the latest stamp? [Every round. That
   is what blocks the commit]
