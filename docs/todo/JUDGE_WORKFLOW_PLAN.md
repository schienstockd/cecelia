# Weekly judge — draining the backlog, and bugs as GitHub issues

**Status:** decisions locked (2026-10-07, Dominik); nothing built. Next: P1.
Revises how [`../ai-assist/WEEKLY_JUDGE.md`](../ai-assist/WEEKLY_JUDGE.md) handles volume and where bugs
live. The brief behind the issues half:
[`../archive/JUDGE_GITHUB_ISSUES_PROMPT.md`](../archive/JUDGE_GITHUB_ISSUES_PROMPT.md). This plan
corrects one of that brief's premises (below).

## Goal

1. **Nothing stacks up.** Each pass judges everything new, so the "waiting for the judge" pile can't
   grow week after week.
2. **The owner's list is short and actionable.** It holds only what needs a person: a fix to make or a
   question to answer.
3. **Bugs are tracked where GitHub tracks work.** Each bug has an issue that opens and closes with it.
   Agents never read anything an outsider could write.

## What exists (measured 2026-10-07)

- **Inflow.** Recital logs about 50 fanout findings a day (37–88 on 2026-10-01…06). Most are tagged
  `fixed_pre_commit` at commit time (107 of 131 tagged). `plausible` findings are never tagged, so
  every one reaches the judge. In the 2026-10-06 record, 119 of the 130 candidates were `plausible`.
- **The cap.** The bug sweep judges at most 40 items per pass (`bugs.MAX_ITEMS`), in one tool-less
  call. That call cost **$0.78 for 40 items** (about $0.02 each). The cap is set by the size of one
  prompt, not by money.
- **The backlog.** After the 2026-10-06 pass, 64 were "waiting for the judge" (37 from 2026-10-05,
  27 from 2026-10-06). That is a few days of inflow, and the cap clears 40 a week. The pile grows by
  design.
- **Yield.** On a judged batch of ~40 (2026-10-04, 2026-10-06), 14 were already `gone` and 10–16 were
  `dismissed`. The remaining 7–12 were `open`, and verify split those into 2–5 `fix`, 3–6 `guard` and
  1–2 `decide`. So about **5 bugs a week need a fix**, out of 100+ candidates.
- **Verify isn't the bottleneck.** 24 bugs cost $4.21 of the $10 cap, with 0 waiting.
- **Where records live.** `~/.cecelia-effectiveness/judge-runs/*.json` is the store and the only
  copy that persists. The weekly PR commits a mirror of it, but **no judge record PR has ever
  merged**: each pass closes the previous one, and `docs/ai-assist/judge-runs/` doesn't exist on main.
  The PR is a notification, not a store.
- **Records are public anyway.** Every pass pushes the record to a public `judge-run/<date>` branch.
- **`EFFECTIVENESS.md` is stale on main.** It was last updated 2026-10-03. Only the weekly PR runs
  `pixi run audit-rollup`, and those PRs never merge.

## Part 1 — limits that drain instead of cap

### D1. Batch the sweep until the queue is empty, under a dollar budget

The sweep judges 40 items per call, and calls again until nothing is left or `SWEEP_USD` is spent.
Default: **$4**. A call starts only while what's spent plus its whole $1.50 budget fits, so the cap
holds: four calls, 160 items, against a measured inflow of about 100 a week. Order:
1. bugs with a landed fix (shipped in #1490);
2. carried `open` bugs;
3. oldest first.

The per-call size stays at 40, because it bounds the prompt, not the spend. On 2026-10-06 this costs
about $2 instead of $0.78 and leaves nothing waiting.

### D2. Backlog is a reported number with a trend

The PR, or the status issue (D9), says "backlog N (last pass M)". The recital console warns when the
backlog grew two passes in a row: the budget no longer covers inflow, and a person should decide.
Nothing is silently dropped.

### D3. No age-out for now

Expiring old unjudged findings would throw away signal to hide a capacity problem. With D1 the queue
drains, so expiry isn't needed. Revisit only if D2's warning fires.

### D4. `guard` leaves the work list

Today `guard` ("can't happen yet") counts as `open`: 6 of the 12 open bugs on 2026-10-06. A new
status, `parked`, keeps such a bug and its `trigger`, off the owner's list and unfiled.
- **Free check each pass:** if a commit since the verify SHA touched the bug's file, it goes back to
  verify. Otherwise it stays parked.
- **What remains open:** `open` then means `fix` + `decide` + `stranded` + repeated agent errors
  being fixed. That is about 5–8 a week.

### D5. Keep the weekly cadence and the verify cap

The usage-limit retry (#1465) covers the night the limit is hit. Running nightly would collide with
the overnight agent runs, which share the lock, and gain little.

### D6. Leave inflow alone (rejected for now)

Asking the committing agent to tag every `plausible` finding would cut the inflow. But it adds
friction to every commit to save about $0.02 per item. Judging is cheaper than tagging.

## Part 2 — bugs as GitHub issues

### The premise to correct

The brief says "committed JSON stays the source of truth". There is no committed JSON (above). The
truth is the local store, and it stays that way. Issues don't add a second store next to a committed
one. They replace the weekly PR as the notification and tracking surface. The local JSON is the only
input any agent or tool reads.

### D7. One issue per actionable bug, written one way only

**The one-line model:** the JSON is the only source. Issues give each bug an id and make it
visible, and no agent ever reads one. The judge alone touches issues, and reads back only their
numbers and open/closed state.

**What is filed:** a bug gets an issue once it is `open` after verify, with verdict `fix` or
`decide`. Stranded commits and repeated agent errors count too.

**What is not filed:** `unjudged`, `unmerged`, `parked` (guard), `dismissed` and `gone`-on-arrival
bugs. That is about 5–8 issues a week, not 100.

**Write-only mirror.** The judge writes the issues. Nothing reads issue text back: no bodies, no
comments, no edits. The only things read back are each issue's **number and open/closed state**,
from the REST list endpoint. The list is filtered by label `judge-bug` and creator = owner, and must
match the number in the JSON mapping.

This closes most of the brief's trust questions by construction:
- **Forged issues:** they aren't in the mapping, so they're ignored.
- **Edited bodies:** never read, and rewritten when the bug changes, so the JSON wins.
- **Comments:** never read.

So the brief's body-hash check isn't needed: it only exists to tell whether text that will be read is
trustworthy, and none is read. A human edit to an issue is simply overwritten at the next change. To
change a bug, answer it in `judge-review` (D11).

### D8. Identity: the owner's own `gh`

The pass runs from a local systemd timer with Dominik's `gh` auth, so issues are authored by
`schienstockd`.

A bot or GitHub App identity is **rejected**. It needs a stored, rotated credential and buys nothing
when nothing is read back. The trust check is mapping + label + creator, and an outsider can't author
as the owner.

### D9. Lifecycle

| Bug state (JSON) | Issue |
|---|---|
| `open` (fix / decide) | open, labels `judge-bug` + `fix` or `decide` |
| fix landed (a commit names the key) | stays open; label `fix-landed`; comment naming the commit and PR |
| judge confirms `gone` | closed as completed, with a comment saying which pass confirmed it |
| `gone`, then live again | reopened, with a comment |
| `wont_fix` (owner, in `judge-review`) | closed as not planned |
| `dismissed` by verify after it was filed | closed as not planned, with verify's one-line effect |
| `parked` after it was filed | closed as not planned, label `parked`; reopened if it's verified live again |

**Closing comes only from the judge.** Fix briefs say `Refs #N` and name the bug key, never
`fixes #N`. If a merge keyword closes the issue anyway, the next pass reopens it, unless the judge
confirms `gone` in that same pass. The state comes from the JSON.

### D10. Idempotency: intent first, then reconcile

1. The pass writes `issue: {"status": "pending"}` into the JSON.
2. It creates the issue, with the bug key in the title: `[fanout-81bf20cf] …`.
3. It records the number.

**Recovery:** if the pass dies between steps 2 and 3, the next pass finds the issue by key through the
**list** endpoint (consistent, unlike search, which lags) and adopts it.

**On a usage-limit retry:** the attempt that hit the limit files nothing. Filing happens after verify,
at publish time.

**Throttle:** at most 1 create per second, and at most 20 per pass. More than that is reported, not
filed.

### D11. What an issue body says, and what never goes in

**Allowed:** the bug key, `file:line` on `origin/main`, the finding's description, and the verdict
with its one-line effect and evidence (`file:line` references). Bodies are built from a template;
finding text sits in a quoted block marked as reviewer output.

**Stripped:** absolute paths (`/home/…`, `C:\…`), project and image uids, and any text from agent
transcripts. Agent-run errors carry the tool + HTTP status + the error's template (the repeat key's
form), not the raw message.

This is not new exposure: the same text is on the public `judge-run/*` branches today.

### D12. The weekly PR exists only for rule proposals

- **Rule proposals:** the PR is opened only when the pass has proposals. It holds those diffs and
  `EFFECTIVENESS.md`, so the rollup reaches main when the PR is merged.
- **Every pass:** one pinned **Judge status** issue gets a comment: bugs filed, closed and reopened,
  backlog and trend, and spend. That's one notification a week instead of a PR you close unread.
- **Failed passes:** a FAILED pass comments there too.

The rendered `.md` record stays, written next to the JSON in the store. `judge-review` and fix
sessions read the JSON. It is no longer committed.

### D13. Backfill once, then steady state

Only the bugs open at switch-over with verdict `fix` or `decide` are filed, about 7. They are filed
once and throttled. `unjudged` bugs are never backfilled. D1 judges them, and the ones that come out
`fix` or `decide` are filed then.

## Part 3 — `judge-review` on the new layout

- **Queue:** built from the JSON, as now (#1417's order: `fix`, then answered `decide`, then the
  rest). Parked bugs are left out. Each bug shows its issue link. If its issue isn't in the mapping or
  isn't owner-authored, it is flagged as **issue missing**, never read.
- **Answers:** go to `review.jsonl` as now. The next pass mirrors them, so there is one writer to
  issues:
  - `wont_fix` closes the issue;
  - a decide answer becomes a comment;
  - `fix_session` adds label `in-progress`.
- **Fix brief:** from the JSON, as now, plus `Refs #N` (no closing keyword). It must still name the
  key in the commit, because landed-fix detection relies on it.
- **Rule proposals:** stay PRs, kept out of the bug queue, as now.

## Phases

Each phase ships alone, P1 first, because it's the problem that bites now.

### P1 — drain and park (Part 1)

D1 batched sweep under `SWEEP_USD`, D2 backlog trend + console warning, D4 `parked` with the
touched-file re-check.

**Checkpoint:** one real pass leaves 0 waiting, with spend in the record.

### P2 — the issue mirror (D7–D11, D13)

`scripts/judge/issues.py`: create, update, close and reopen, the key-in-title reconcile, body
template + redaction. All of it goes through an injectable `gh` runner so tests use a fake.

**Tests:**
- a forged issue with a copied key;
- an issue the owner edited;
- a crash between create and record (adopted, not duplicated);
- a rerun (no duplicate);
- a merge-keyword close reopened;
- redaction.

**Checkpoint:** a `--dry-run` that prints what it would file, against the real store. Dominik reads it
before the first real filing.

### P3 — `judge-review` on issues (Part 3)

Extend `test_judge_review.py` with: an issue missing from the mapping, a forged issue, and an edited
issue (ignored).

### P4 — PR only for rules, status issue (D12)

`WEEKLY_JUDGE.md` is rewritten at this phase.

## Rejected

- **A generated index instead of issues.** It would have to be committed to be seen, which needs
  the weekly PR this plan removes. It has no native close or notifications either.
- **Reading issue comments or bodies back** as owner answers: an outsider can write there.
  Answers come only through `judge-review`.
- **Filing every candidate:** 100+ a week, mostly gone or dismissed.
- **A bot identity:** see D8.
- **Age-out, nightly passes, tagging plausible findings:** see D3, D5, D6.

## Answered (Dominik, 2026-10-07)

1. **Sweep budget (D1):** $4 a pass.
2. **Parking `guard` bugs (D4):** yes, off the list with no issue, re-checked when their file changes.
3. **Notifications:** about 5–8 issues a week plus the weekly Judge status comment is fine.
4. **`EFFECTIVENESS.md`:** the judge keeps generating it. It reaches main through the rules-only PR
   when there is one.

## What would change this plan

- **D2's warning fires two weeks running:** inflow outgrew the budget. Raise `SWEEP_USD` or cut
  inflow (D6).
- **Outsiders start filing look-alike issues:** a GitHub App identity becomes worth its credential.
- **`fix` bugs routinely reach 5+ a week:** revisit the unattended fix agent in
  [`../FUTURE.md`](../FUTURE.md). That is already about the 2026-10-06 rate.
