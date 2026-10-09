> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/todo/JUDGE_WORKFLOW_PLAN.md`.
>
> **Outcome:** not run as a brief. Written in a chat on 2026-10-09 while P1 (drain and park) was
> shipping, then read against the plan, `scripts/judge/` and the run records. Accepted concerns are
> folded into `docs/todo/JUDGE_WORKFLOW_PLAN.md`; the rest are listed under its "Rejected".

# Task: test the judge workflow plan against six concerns, then revise it

Repo: `schienstockd/cecelia` (public). Plan: `docs/todo/JUDGE_WORKFLOW_PLAN.md` (PR #1505, open, docs only). Related: #1417 (`judge-review` work list), `scripts/judge/`, `docs/ai-assist/WEEKLY_JUDGE.md`, `docs/FUTURE.md`.

Do your own reasoning. Read the plan and the code it touches first. For each concern below, verify it against the actual code and the plan, then decide: **valid and worth a change**, **valid but already covered** (point to where), or **not valid** (say why). Disagree where I'm wrong. Don't add a change just to answer a concern.

## What the plan already decides (don't reopen)

- The local store `~/.cecelia-effectiveness/judge-runs/*.json` is the only source. No committed records.
- Issues are a write-only mirror. The judge reads back only each issue's number and open/closed state, filtered by label `judge-bug`, creator = owner, and the number in the JSON mapping.
- Identity is the owner's own `gh` auth. No bot or GitHub App.
- $4 sweep budget, `guard` bugs parked, PR only for rule proposals, plus a pinned Judge status issue.

## Concerns

1. **Outsider comments can still be read by a fix session.** The judge never reads comments, but a `judge-review` fix session briefed with `Refs #N` could run `gh issue view N` and pull outsider text into context. Consider locking each judge-filed issue and the status issue on creation (`gh issue lock`), and telling the fix brief not to read the issue. Check what locking does and doesn't prevent, and whether it conflicts with D9's comments (the judge is the owner, so it can still comment).
2. **Finding text in public issue bodies.** Quoting finding text isn't enough. A stray `@user` notifies a stranger, `#123` cross-links unrelated threads, and free-text findings can carry absolute paths or project/image uids. Check what recital and verify actually put in findings. Decide between an allowlist built from structured fields and a regex backstop, or both. Bodies should reach `gh` via `--body-file`, not argv. Specify tests.
3. **The `gh` runner is too powerful.** The pass runs with the owner's full-scope token. Decide whether the injectable runner should allowlist subcommands (issue create/edit/close/reopen/comment/list/lock), and what happens when something else is requested.
4. **Backup of the local store.** Once the record PR stops being committed, the local store is the only copy and issues are never read back. D11 justifies exposure by the public `judge-run/*` branches. Is that push staying? If not, say so, and decide whether the store needs a backup or a recovery path (e.g. rebuild the key to number mapping from issues).
5. **Parked re-check is a proxy.** "File touched since the verify SHA" misses renames and triggers that depend on a different file or call site, and hot files could bounce parked bugs back into verify and eat the $4 budget. Look at what a `guard` bug's `trigger` actually contains today. Decide whether verify should record the files or symbols the trigger depends on, how renames are handled, and whether re-verification is capped per pass.
6. **Backlog warning won't be seen.** D2's console warning fires inside a systemd timer run. Make sure the same signal lands in the status-issue comment, and say what "grew two passes in a row" is measured against (the last two records in the store).

## Deliverable

Edit `docs/todo/JUDGE_WORKFLOW_PLAN.md` on the `judge-plan` branch (or a follow-up commit on #1505) with the changes you accept. Keep its style: decisions numbered, rejected items listed, phases unchanged unless a concern forces a new one. Add rejected concerns to "Rejected" with one line of reasoning. In the commit message or PR comment, give a table of the six concerns with your verdict (valid, covered, not valid) and the plan section affected.

Docs only. Do not change any source in this task. Follow the repo's CLAUDE.md conventions.
