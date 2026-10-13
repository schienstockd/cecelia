# Agents pane — `pixi run recital-console` shows the live sessions

Status: **built** (2026-10-13) — P1 (`claude_sessions.py`, `agents.py`, the pane, `pixi run agents`) and P2 (PR + CI columns).

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
   | recital rounds | the branch's `fanout_audit_run` rows in the effectiveness log. `pixi run recital` writes one per round |
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

## Built (P1 + P2, 2026-10-13)

- Session `status` also takes the value `"shell"` (seen live), so it is shown as written and
  sorted between idle and busy.
- `claude_home()` honours `$CLAUDE_CONFIG_DIR`, which Claude Code also uses; tests point it at a
  temp folder so they never read the machine's real sessions.
- One refresh (sessions + two `git` calls each + a full log read) took ~70 ms with 5 sessions and a
  5 000-row log; the pane refreshes every 5 s.
- The commit hook's `_untagged_slugs_on_head` and the judge scripts now take the finding/resolved
  event names from `effectiveness/log.py`.
- Columns: `agent state worktree branch recitals open pr ci`. `ci` is the CI state while the PR is
  open (`fail`/`running`/`pass`) and `merged`/`closed` after. `git_context.recent_prs` (100 newest
  PRs, any state) took ~3 s, so `agents.PrCache` fetches it in a background thread at most every
  60 s; a failed fetch keeps the last list. A one-shot snapshot (`--no-follow`, or piped) waits for it.
- `pixi run agents`, → (← undoes, PgUp/PgDn scroll): under each session, its outcome counts
  (`open · fixed · shipped · false pos · dropped`) and every recital finding on its branch with
  its outcome, mechanism, `file:line` and description, plus the agent's reason under a shipped /
  dropped / false-positive one. Asked for 2026-10-13: "how much these sessions have to fix, and
  which ones they dropped, shipped". The join is `log.resolved_findings`, shared with the hook.
- The → view draws each finding with the recital console's own `_render_finding_block` (head line +
  description capped at 3 lines, no branch anchor — the session row names it), outcome in front,
  the agent's reason (`why …`) under it. A one-line-per-finding version clipped every description
  and read as clutter; a version listing only non-fixed findings dropped detail Dominik wanted.
- **Which window is which:** each session has a badge — one of 4 shapes in one of 7 Okabe-Ito
  colours, from a hash of its session id (`agents.session_badge`). The pane draws it before the
  name; `scripts/statusline_badge.py` wraps any statusline command and draws the same badge and
  name in that session's own statusline. Opt-in, one line in `~/.claude/settings.json`:
  `"command": "python3 <repo>/scripts/statusline_badge.py <your statusline command>"`. ~60 ms per
  refresh on top of the wrapped command; if it fails the wrapped statusline still shows.
- Headless `claude -p` runs (the recital's reviewers) write session files too, named after their
  folder (`cecelia-prune-session-gate-13`) and marked `kind: "interactive"` like a window; only
  `entrypoint: "sdk-cli"` (windows: `"cli"`) tells them apart, so `sdk*` is skipped. Names longer
  than the column are cut.
- `done` replaces the state when the PR merged and the session isn't busy: close that window,
  then `prune-worktrees` can take the worktree (the session gate keeps it IN USE until then).
  Sorts after idle, before shell/busy.
- A session's shell can be reset to the launch folder (the workspace root, no checkout) while
  its work stays in a worktree, so the pane takes the newest *checkout* among the transcript's recent
  `cwd`s, and the prune gate protects every worktree among them.
- macOS / Windows: no `/proc`, so the pane reads "can't list sessions" and the prune gate falls back
  to IN USE-unknown as before. The statusline wrapper writes UTF-8 bytes (cp1252 has no badge shapes).
- Session `status` meanings, read off live sessions (not documented by Claude Code): `idle` =
  turn over, nothing running; `shell` = turn over, a background command (e.g. a CI watch) still
  running and will wake the agent; `busy` = mid-turn.

## Open questions (default in brackets)

1. Should merged-PR sessions stay in the list? [Yes, dimmed. They are prune candidates]
2. Should a branch's open findings count every round or only the latest stamp? [Every round. That
   is what blocks the commit]
