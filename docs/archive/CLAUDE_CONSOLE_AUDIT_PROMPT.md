# Prompt: audit and plan `pixi run claude-console`

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

> **Outcome: not run; dropped on 2026-10-10.** No `pixi run claude-console` and no
> `docs/todo/CLAUDE_CONSOLE_PLAN.md` exist. Liveness (item 1 of "what the console must answer")
> is now native: Claude Code 2.1.296 `ListAgents` shows each local session as busy, idle or at a
> shell. Focus/jump (item 6) mostly cannot work here — the desktop is Wayland, where an outside
> process cannot raise a window. Two premises had also moved: the eval supervisor it says to read
> was retired on 2026-10-04 (headless agents now come from `scripts/judge/` and
> `scripts/agent_eval/`), and "open reservations" is the pre-commit list in `CLAUDE.md`, which
> holds recital findings *and* judgement-only risks — only the first are logged (a `*_finding`
> row without a matching `*_finding_resolved`). The one part that is not native is the
> lifecycle join — which worktrees are merged, clean and safe to remove. That is a one-shot
> snapshot (`git worktree list` + `gh pr list`, reusing `git_context.py`), not a live console with
> hooks; not built. Companion:
> [`CLAUDE_CONSOLE_BUS_AUDIT_PROMPT.md`](CLAUDE_CONSOLE_BUS_AUDIT_PROMPT.md).

Run this in Claude Code (Opus) at the repo root. **Audit and plan only. Do not implement the console.** Throwaway spike scripts are allowed under `scratch/` and must not be committed.

## Problem

D often runs 10+ Claude Code sessions in parallel (one per git worktree, mostly), on Ubuntu with the standard terminal. He loses track of which window needs him. He wants a read-mostly live console, `pixi run claude-console`, in the family of:

- `pixi run console` → `api/task_console.jl` (scheduler tasks, WebSocket + HTTP snapshot reconcile)
- `pixi run recital-console` → `python/cecelia/effectiveness/console.py` (tails `~/.cecelia-effectiveness/events.jsonl`)

## What the console must answer, per worktree/session

1. **Liveness**: working now, idle, waiting for my input (permission prompt or question), dead/crashed.
2. **Lifecycle stage** (derived; label as inferred where it is):
   working → in recital → recital findings open → committed → PR lodged (CI pending/failed/green, review state) → merged → **ready to clean up worktree**.
3. **Identity**: which worktree path, branch, PR number/URL, commits ahead of base, dirty or clean.
4. **Plan**: whether a plan was parked (and where) during the session.
5. **Attention ordering**: sessions that need D float to the top (needs input > cleanup ready > findings open > CI failed > PR open > committed > recital running > parked > working).
6. **Window locate/highlight**: a way to find the terminal window of a session, or make it visibly stand out. Ideas on the table: terminal title and/or background tint set by a hook when input is needed and reset afterwards; a jump action from the console. Blinking was floated and is probably too much.

"Open reservations" in D's message is assumed to mean unresolved recital findings. Flag it if the code suggests otherwise.

## Constraints and conventions

- Follow `CLAUDE.md` (read it first, including the Windows-compat rules). The target is Ubuntu, but keep POSIX-only terminal code isolated and optional.
- Consoles are reporting-only by convention. Any action (focus window, cleanup) must be explicit, minimal, and never destructive without confirmation. Prefer "print/copy the command" for cleanup.
- Reuse existing pieces before inventing: shared palette (`share/console_palette.json`, `palette.py`), `scroll_window`/key handling, `DashboardState`, `_drain_new_events`/`follow_events`, `--stream` mode for non-TTY, the effectiveness log schema (`log.py::EVENT_TYPES`, `docs/todo/EFFECTIVENESS_LOG_PLAN.md`).
- Event streams can be lossy (hooks missed, crash without `SessionEnd`). `task_console.jl` solves the equivalent problem with snapshot reconcile and retire-after-N-misses. Decide whether the same pattern applies here.

## Tasks

### 1. Read the repo first
At minimum: `CLAUDE.md`, `pixi.toml` (existing console tasks), both consoles above, `palette.py`, `log.py`, `rollup.py`, `recital.py`, `.claude/settings*.json` and any existing hooks, the eval supervisor plan (`CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md`) and how it spawns agents, and any existing worktree tooling/scripts. List what you found and what is missing. Do not assume file contents.

### 2. Verify Claude Code hook facts against current docs
Fetch the current official Claude Code hooks documentation. Do not rely on memory. Establish, with citations to the doc sections:

- Exact event names available, and the payload fields of each (session id, cwd, transcript path, tool name, etc.).
- Precisely when `Notification` fires (permission request vs. idle prompt vs. others) and when `Stop` fires. Do subagents emit their own `Stop`-like events?
- Whether hook stdout/stderr reaches the terminal, and whether a hook can write to the session's tty (`/dev/tty`, or a tty path resolved from the parent process).
- Whether project-level `.claude/settings.json` hooks apply inside git worktrees, and how they merge with user-level settings.
- Behavior for headless runs (`claude -p`, the eval supervisor's agents): should they be tagged or excluded?
- What happens on `/clear`, `--resume`, `--continue` (same or new session id?).
- Whether a status line command could supply any of this more cheaply than hooks.

Anything you cannot confirm from the docs goes in an "Unverified" list. Do not guess.

### 3. Verify terminal control on this machine (Ubuntu)
Write small spike scripts and run them where possible:

- `echo $XDG_SESSION_TYPE` (X11 or Wayland) and identify the terminal emulator and version (likely gnome-terminal/VTE).
- Does OSC 0/2 (title) work, and does OSC 11 (background colour) set and OSC 111 reset in this terminal? Does it survive Claude Code's TUI redraws? Can a separate process write the sequences to the session's `/dev/pts/N` and have them take effect without corrupting the TUI?
- Focus/raise by title: `wmctrl -a` / `xdotool` under X11. State plainly what is impossible under Wayland (windows generally cannot be focused by an outside process) and what the realistic fallbacks are (unique titles, tint, desktop notification via `notify-send`, a GNOME-specific route if one exists). Verify; do not assert.
- Whether tmux would be a worthwhile (opt-in) upgrade path, with the exact commands, but do not require it.

Report which highlight/jump features work on D's actual setup, which need a one-time setting, and which are not feasible.

### 4. Evaluate candidate bases
Compare, in a table with explicit criteria:

| Candidate | Notes to evaluate |
|---|---|
| A. Extend/fork `recital-console` (`console.py`) | event-log tail model fits hook events; shares palette, scroll, dashboard state |
| B. Extend `console` (`task_console.jl`) | WS + snapshot reconcile pattern; Julia; would need a producer/server |
| C. New standalone Python console in the same family | extracting shared TTY/dashboard helpers from `console.py` into a small module, so both Python consoles use them |
| D. Adopt cltop (https://github.com/chrbailey/cltop) as-is or as a dependency | Textual TUI, alpha; process-level states only; evaluate honestly |
| E. Other prior art (recon, claude-deck, claude-dashboard, claude-code-dashboard, claude-code-monitor) | which, if any, could be extended by plugin/hook rather than rebuilt |
| F. Anything else you propose | e.g. a Textual app, a tiny local daemon that owns state and serves both a TUI and a title/tint notifier |

Criteria: fit with the event model, effort, reuse of existing code, testability (the repo tests consoles through pure render/reconcile functions), robustness to missed events, cross-platform cost, maintenance burden, and how well it supports the lifecycle states above (git/gh/recital joins are the hard part, not process discovery).

Give one recommendation and say what would change your mind.

### 5. Design the data model and state machine
- Key: worktree path (a session is an attribute of a worktree). Handle multiple sessions in one worktree and sessions outside any worktree.
- Event schema for the hook log (fields, versioning, file location, rotation, atomic append, size).
- Derived-state function: inputs (hook events, git/gh snapshot, effectiveness-log events by branch/PR) → state + priority. Make it pure and unit-testable. Specify the join keys (branch, PR number, commit) and what happens when they are missing.
- Reconcile strategy: how a session with no `SessionEnd` is retired (pid check, tty check, timeout), and how stale "needs input" is cleared.
- gh/git polling: commands, cadence, caching, rate limits, behavior when `gh` is unauthenticated or offline, cost with 10+ worktrees.
- Where "plan parked" is detected (`plan_logged` events, `ExitPlanMode`, plan files) and what counts.

### 6. Risks and edge cases
Hook failure modes (a broken hook must never block or slow Claude Code), privacy of what is logged (no prompts or file contents in the log unless justified), concurrent writers, clock/timezone handling (the existing consoles render local time from UTC), and Windows-compat implications.

## Deliverable

Write `docs/todo/CLAUDE_CONSOLE_PLAN.md` with:

1. **Recommendation first**: chosen base, one paragraph, plus the main reason.
2. Verified facts (with doc citations) and an **Unverified** list.
3. Terminal-capability findings for this machine, with the spike results.
4. Candidate comparison table and rejected options with reasons.
5. Data model, state machine, reconcile strategy.
6. Phased build plan, each phase independently useful and shippable, with test strategy. Suggested shape (change it if the audit says otherwise): P1 hooks + sessions log + minimal table with needs-input tint; P2 git/gh enrichment and cleanup-ready; P3 recital and plan joins; P4 focus/jump.
7. Open questions for D, numbered, each with your default if he does not answer.

Keep it terse: decisions and evidence, no narrative. Then stop and wait for D's review. Do not start implementing.
