> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/todo/JUDGE_WORKFLOW_PLAN.md`.
>
> **Outcome:** not run as a brief. Written in a chat with Sonnet on 2026-10-07, then read against the
> code and the run records and folded into `docs/todo/JUDGE_WORKFLOW_PLAN.md`. One premise was
> wrong: "committed JSON stays the source of truth". No judge record PR has ever merged and
> `docs/ai-assist/judge-runs/` doesn't exist on main. The records live in the local store
> (`~/.cecelia-effectiveness/judge-runs/`), and the weekly PR is a notification that the next pass
> closes. The plan also takes up a problem this brief didn't raise: the backlog stacking behind the
> per-pass cap.

# Task: let the weekly judge file GitHub issues for bugs

Repo: `schienstockd/cecelia` (public). Judge code: `scripts/judge/` (`bugs.py`, `record.py`, `weekly.py`, `verify.py`). Docs: `docs/ai-assist/WEEKLY_JUDGE.md`. Records: `docs/ai-assist/judge-runs/<date>.json` (+ rendered `.md`).

Do your own reasoning and design. This prompt gives the goal, the decisions already made, and the constraints. Everything else is yours to decide. If you disagree with a decision below, say so and explain before building.

## Context

Today the weekly judge opens a PR per pass with a run record (see #1489). A later Claude Code session is pointed at the record to work the bugs. Rule proposals (P1-P3) also appear in that PR. #1490 (open when this was written; check its state and build on or rebase over it) fixes the "landed fix" wording and judges landed-fix bugs ahead of the per-pass cap.

## Goal

Bugs the judge finds should be filed and tracked as GitHub issues, instead of living only in a weekly record PR.

## Decisions already made

- **Committed JSON stays the source of truth.** Issues are a mirror/view, not state the judge or agents depend on.
- **Rule proposals stay as PRs.** They are diffs to CLAUDE.md files that need review. Issues are the wrong shape for them.
- **The repo is public, so issues are an untrusted input surface.** Outside users can open issues, copy any body text or marker, and comment.

## Constraints

- **Trust by author, not by content.** Markers or hidden comments in the body can be copied by anyone. Labels can only be applied by triage+ users, so they are a useful extra signal, but do not rely on a single signal. Prefer a bot or GitHub App identity, or `github-actions[bot]` via `GITHUB_TOKEN`, over the owner's personal account. Work out what the judge actually runs as (local cron, Claude Code session, CI) and what identity is realistic there.
- **Issue number to bug key mapping is committed** in the judge JSON. An issue counts as judge-filed only if it is in that mapping, authored by the trusted identity, and its body still matches what the judge wrote (detect edits).
- **Agents work from the JSON, not issue text.** Anything an outsider adds (bodies, comments, edits) must never reach an agent prompt as instructions. Decide what, if anything, from an issue is safe to read back.
- **Idempotent and dedupe-safe.** Re-running a pass must not create duplicates. Handle a failed or partial run (the pass can hit the usage limit and retry, as on 2026-10-06).
- **Lifecycle:** decide how open, landed-fix (commit names the bug key), confirmed-gone, and reopened map onto issue state. Keep the #1490 semantics: "fixed", "fix landed, awaiting re-check", confirmed by the judge.
- **Spend and caps:** keep the existing per-pass cap behavior and cost reporting working.
- **No new run-record PR for bugs.** Decide what the weekly PR looks like afterwards (rules only, a summary, or none when there are no proposals).
- Follow the repo's CLAUDE.md conventions, including the discovery step before implementing. Update `docs/ai-assist/WEEKLY_JUDGE.md` and any docstrings that go stale.

## Open for you to decide

- Which identity and auth the judge uses, and how it is provisioned.
- Issue title/body format, labels, and whether to batch or file one per bug.
- How an existing backlog of bugs in the JSON gets migrated or backfilled (or whether it should).
- Whether the rendered `.md` record is still worth generating.
- How a human or a work session finds the current open set (label query, a generated index, the JSON).

## Deliverables

1. A short design note (in the PR description is fine): chosen approach, the trust model, and what you rejected and why.
2. The implementation, with tests. Include tests for forged or edited issues, duplicate prevention on re-run, and the partial-failure retry.
3. Docs updated.

Run `pixi run test-py` before finishing. Do not run the live judge against the real repo as part of verification; use fakes or a dry-run mode.

## Reservations (address these in the design note)

1. **Two stores can drift.** JSON plus mirrored issues means a sync problem the current PR flow doesn't have. The real gains are native closing and no weekly PR. Check whether a generated index over the JSON would get most of that with less machinery. If issues don't clearly win, say so.
2. **Judge identity may be weak.** If the judge runs from local cron or a Claude Code session, it likely runs as the owner, so "trusted author" can't tell the judge from the owner. A bot identity needs a stored credential. If a PAT, scope it to issues only on this repo, and say where it lives.
3. **Merge-triggered auto-close breaks the lifecycle.** `fixes #N` closes the issue when the fix merges, before the judge re-checks it. That contradicts the #1490 state "fix landed, awaiting re-check". Closing should come from the judge's confirmation, not the merge. Avoid closing keywords in fix commits or reopen on re-check failure, and decide which.
4. **Create-then-commit is not atomic.** A crash between creating the issue and committing the JSON mapping leaves an orphan, and the retry files a duplicate. Body-marker search can't fix this alone, because GitHub search is index-lagged. Use an idempotency approach that survives that (e.g. write intent to the JSON first, then reconcile).
5. **Public exposure.** Bug text (file paths, findings quoted from agent sessions) becomes public and indexed. Decide what is safe to publish. Check that nothing sensitive from transcripts, or any local path or credential, ends up in a body.
6. **The judge's own output is derived from untrusted inputs.** Issues don't fix that. Check that finding text is sanitized or clearly delimited before it is filed, and again before it is ever read back.
7. **Legit human edits look like tampering.** The body-hash check will flag the owner editing an issue. Define how a human edit is blessed, or decide that edits are ignored and the JSON wins.
8. **Backfill and volume.** There are 12 open bugs and 64 waiting for the judge. Bulk creation can hit secondary rate limits, flood notifications, and look like spam. Decide whether to backfill at all, and if so throttle it.
9. **History and rollup.** `docs/ai-assist/EFFECTIVENESS.md` (`pixi run audit-rollup`) is built from the run records. Make sure the rollup and per-week trend data keep working when bugs are no longer rendered into a weekly record.

## Also in scope: `pixi run judge-review`

The manual review step was built around the record layout and must be redesigned for issues. Read `scripts/judge/review.py` and #1417 (merged) first. Current behavior, from #1417:

- **Decide:** the verify agent's `decide` bugs. The owner chooses keep open / answer / won't fix.
- **Work:** every other open bug, ordered `fix`, then `decide` bugs kept open or answered, then `guard`, then the rest. `[f] fix now` starts an interactive Claude Code session in the main-checkout workspace (`~/cc-workspace/cecelia`), briefed by `fix_brief` (uses `record._bug_where`, `store_root`). The brief tells it to confirm on `origin/main`, make its own worktree (`pixi run bootstrap-worktree fix-<key>`), fix sibling call sites, add a failing test, run recital, and name the bug key in the commit.
- A bug given a fix session isn't offered again. The next judge pass marks it gone once the fix merges.
- Review events (`bug_work`, `fix_session`, and the decide answers) go through `ANSWERS` / `apply_reviews`, which writes `owner_decision` into the record; `weekly.py` persists it with `_record.write(force=True)`.
- The owner is in every fix session. Nothing runs unattended. The unattended fix agent is deferred in `docs/FUTURE.md`.

Redesign it for the issue layout:

- **Briefs come from the JSON, never issue text.** The brief and the queue are built from committed records plus the verified issue mapping. Raw issue bodies and comments never reach the fix session prompt.
- **Verdicts need a home.** Decide where decide answers, won't-fix, and "fix session started" live. The JSON stays authoritative. Decide whether and how to mirror them to the issue (comment, label, close), keeping reservation 3 in mind: closing must come from judge confirmation, not a merge keyword or the review step.
- **Fix commits and issues.** The commit must still name the bug key (the judge's landed-fix detection relies on it). Decide how the brief references the issue number without a closing keyword.
- **Queue source.** Review runs locally as the owner, so it may use the owner's own `gh` auth. The judge and review then act with different identities. Decide what each is allowed to write, and keep the trust check (author, mapping, body hash) on every issue read. Flag unverified or edited issues in the queue instead of silently using or dropping them.
- **Rule proposals (P1-P3)** remain PRs. Review must not mix them into the bug queue.
- Keep the queue ordering and the one-session-per-bug, owner-present model unless you have a reason to change them. Say why if you do.
- Update the task docs and `WEEKLY_JUDGE.md`. Extend `test_judge_review.py` for the new layout, including a forged or edited issue and an issue missing from the mapping.
