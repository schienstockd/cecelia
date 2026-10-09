# Weekly judge — draining the backlog, and bugs as GitHub issues

**Status:** decisions locked (2026-10-07, Dominik); a concerns review folded in (2026-10-09: D14, D15
and amendments to D2, D4, D9, D11). P1 merged (#1513), P1b merged (#1515). P2–P4 built and on: the
2026-10-09 pass ran in the issue layout, which is now the default (`--no-issues` / `JUDGE_ISSUES=0`
for the old record PR). P5 audited 2026-10-09; its fixes are in (see P5). Rule proposals are answered
in `judge-review` after the bugs (Dominik, 2026-10-09: "I will probably forget otherwise"); the
rules-only PR only carries `EFFECTIVENESS.md`.
Revises how [`../ai-assist/WEEKLY_JUDGE.md`](../ai-assist/WEEKLY_JUDGE.md) handles volume and where bugs
live. The brief behind the issues half:
[`../archive/JUDGE_GITHUB_ISSUES_PROMPT.md`](../archive/JUDGE_GITHUB_ISSUES_PROMPT.md). This plan
corrects one of that brief's premises (below). The concerns review:
[`../archive/JUDGE_PLAN_CONCERNS_PROMPT.md`](../archive/JUDGE_PLAN_CONCERNS_PROMPT.md).

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
- **Finding text links and notifies.** Measured 2026-10-09 across the store: 9 bugs carry
  `@primeuix` (an npm scope, and a GitHub account an issue body would notify) and 12 carry `#2`
  (which GitHub would cross-link to issue #2). No absolute paths turned up.
- **Guard triggers name callers, not the bug's file.** Of the 6 open `guard` bugs on 2026-10-06, 5
  read "a future client, MCP tool or direct API call sends X". The code that would make them live
  doesn't exist yet, so no file or symbol can be watched for it.
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

The PR, or the status issue (D12), says "backlog N (last pass M)". The recital console warns when
the backlog grew two passes in a row: the budget no longer covers inflow, and a person should decide.
Nothing is silently dropped.

**Measured against** the last three *pass* records in the store, by date. A failure record in
between is skipped, not a reset. The console is where you see it at every commit, not in the timer's
log. The status-issue comment (D12) carries the same warning line, from the same function.

### D3. No age-out for now

Expiring old unjudged findings would throw away signal to hide a capacity problem. With D1 the queue
drains, so expiry isn't needed. Revisit only if D2's warning fires.

### D4. `guard` leaves the work list

Today `guard` ("can't happen yet") counts as `open`: 6 of the 12 open bugs on 2026-10-06. A new
status, `parked`, keeps such a bug and its `trigger`, off the owner's list and unfiled.
- **Free check each pass:** if a commit since the verify SHA touched the bug's file, it goes back to
  verify. Otherwise it stays parked.
- **The check is a proxy, on purpose.** It catches a change to the bug's own code. It misses what
  most triggers actually name: a future caller somewhere else (see "What exists"). Recording those
  dependencies in verify is rejected (below), so a parked bug that goes live elsewhere surfaces the
  normal way, as a new finding from the commit that wires the caller. A rename fires the check once:
  the rename commit touches the old path.
- **Re-verify cap (P1b):** at most `PARKED_RECHECK = 3` parked bugs go back to verify per pass,
  oldest parking first. The rest stay parked, with why "file changed; waiting for a re-check slot".
  A hot file (`viewer_api.jl`: 9 commits in a week) can't then eat the verify budget: 3 of the 6
  guard bugs would have come back on the first pass.
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

**Locked on creation.** Every judge-filed issue and the status issue is locked (`gh issue lock`) right after it is
created. Only the owner and collaborators can then comment, so an issue never holds outsider text
that a fix session might pull in with `gh issue view`. The judge posts as the owner, so its own D9 comments still work. Locking
doesn't stop reactions from collaborators or edits by the owner, and it doesn't need to. The fix
brief also says "don't read the issue; the brief is the whole task", as a second guard.

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

**Built from structured fields only:** the bug key, `file:line` on `origin/main`, the verdict, its
one-line effect, and evidence `file:line` references. The template places each; nothing is pasted
whole. The finding's free-text description is the one prose field. It sits in a quoted block marked
as reviewer output.

**Defused in every prose field:** `@name` and `#N` go inside a code span, where GitHub neither
notifies nor cross-links. Home paths (`/home/<user>`, `/Users/<user>`, `C:\Users\<user>`) become
`~`; a system path such as `/opt/homebrew/bin` stays, since it names no one and is often the point
of the finding. Project and image uids (the ones on this machine) become `<uid>`, and text from
agent transcripts is left out. Agent-run errors carry the tool + HTTP status + the error's template
(the repeat key's form), not the raw message.

**Backstop:** after the template is filled, a regex pass looks for a bare `@name`, a bare `#N`, an
home path or a uid. A hit means the bug is **not filed**; the status comment reports it. A
backstop that only logs would let the one it missed through.

**Transport:** bodies reach `gh` through `--body-file`, never argv.

This is new exposure. D12 stops the weekly record push, and the `judge-run/*` branches go with it,
so the issues become the only public copy of finding text. That is why the rules above are
load-bearing rather than tidy.

### D12. The weekly PR exists only for rule proposals

- **Rule proposals:** the PR is opened only when the pass has proposals. It holds those diffs and
  `EFFECTIVENESS.md`, so the rollup reaches main when the PR is merged.
- **Every pass:** one pinned **Judge status** issue gets a comment: bugs filed, closed and reopened,
  bugs held back by the D11 backstop, backlog and trend (with D2's warning line when it fires), and
  spend. That's one notification a week instead of a PR you close unread.
- **Failed passes:** a FAILED pass comments there too.

The rendered `.md` record stays, written next to the JSON in the store. `judge-review` and fix
sessions read the JSON. It is no longer committed.

### D13. Backfill once, then steady state

Only the bugs open at switch-over with verdict `fix` or `decide` are filed, about 7. They are filed
once and throttled. `unjudged` bugs are never backfilled. D1 judges them, and the ones that come out
`fix` or `decide` are filed then.

### D14. The `gh` runner allows a fixed list

The injectable runner (P2) passes through only `issue create`, `issue edit`, `issue close`,
`issue reopen`, `issue comment`, `issue lock`, `label create`, and a bare-path GET of `user` or this
repo's issue list (the REST list D7 reads; `gh issue list` with an author filter goes through
search, which lags). Anything else raises and fails the
pass's issue step, loudly, the same way a failed judge call is reported. The token is the owner's
full-scope one (D8), so the allowlist is what bounds what the pass can do.

### D15. The local store has no backup; this is how it recovers

The store is the only copy once D12 lands. It gets **no backup**:
- the issue mapping is rebuilt from the issues themselves, by key in the title (D10's reconcile);
- verdicts are re-earned: a fresh store judges everything again, at about $4 sweep + $4 verify;
- `review.jsonl` (owner answers) lives in `~/.cecelia-effectiveness/` beside it and has the same
  exposure. It's the one thing that can't be re-earned, and is small enough that a person who cares
  copies it.

Revisit if the store is ever lost for real.

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

Each phase ships alone, P1 first, because it's the problem that bites now. Every phase also checks
that `pixi run judge-review` still offers the right work list on a real record and a `--no-judge`
pass, and extends it in the same PR if not.

### P1 — drain and park (Part 1)

D1 batched sweep under `SWEEP_USD`, D2 backlog trend + console warning, D4 `parked` with the
touched-file re-check.

**Checkpoint:** one real pass leaves 0 waiting, with spend in the record.

### P1b — cap parked re-checks (D4)

`PARKED_RECHECK` in `scripts/judge/bugs.py`, oldest parking first. Test: 5 parked bugs whose files
all changed send 3 to verify, keep 2 parked with the waiting-for-a-slot reason.

### P2 — the issue mirror (D7–D11, D13, D14)

`scripts/judge/issues.py`: create, update, close, reopen and lock, the key-in-title reconcile, body
template + redaction + backstop (D11). All of it goes through an injectable `gh` runner with the D14
allowlist, so tests use a fake.

**Tests:**
- a forged issue with a copied key;
- an issue the owner edited;
- a crash between create and record (adopted, not duplicated);
- a rerun (no duplicate);
- a merge-keyword close reopened;
- redaction: `@primeuix` and `#2` come out in code spans; an absolute path and a uid are stripped;
- the backstop: a template that leaks a bare `@name` files nothing and reports the bug;
- the body goes through `--body-file` (the fake runner sees no body in argv);
- lock: every create is followed by a lock;
- the allowlist: a `repo delete` or `pr merge` through the runner raises.

**Checkpoint:** a `--dry-run` that prints what it would file, against the real store. Dominik reads it
before the first real filing.

### P3 — `judge-review` on issues (Part 3)

Extend `test_judge_review.py` with: an issue missing from the mapping, a forged issue, and an edited
issue (ignored).

### P4 — PR only for rules, status issue (D12)

`WEEKLY_JUDGE.md` is rewritten at this phase.

### P5 — audit the whole loop (Dominik, 2026-10-09)

Once P1–P4 are in: one audit of the pass end to end, against real records and a real pass. It covers
sweep, verify, parking, the issue mirror, `judge-review` and the status issue. It looks for what the
phases broke between them, not inside any one.

**Outcome:** audited 2026-10-09 against the 2026-10-09 pass. Fixed: issue bodies link the bug's own
commit (the run SHA churned every body weekly and pointed 12 of 19 links at the wrong line); closing
follows the open judge issues GitHub lists, so a pass that skipped the mirror leaves nothing open; the
excerpt judge's `not_a_bug` sends a verified `fix`/`decide` bug back to verify instead of dismissing
it; a close comment names who dismissed; bugs held back by verify's cap are said in the status
comment, record and PR; answers given to an older record mid-pass apply; the issue layout is the
default; a failed mirror or status comment warns in the recital console; an over-cap bug says so and
gets its answer comment when filed; `in-progress` lasts one pass; a pass without proposals leaves the
open rules PR alone; a rule's findings count only after its section's last edit.
Left alone: `fix-landed` coming off a week later is intended (a re-verified open bug means the fix
didn't fix it); a key the sweep judge never answers stays `unjudged` with no repeat yet, revisited if
the same key goes unanswered twice.

## Rejected

- **A generated index instead of issues.** It would have to be committed to be seen, which needs
  the weekly PR this plan removes. It has no native close or notifications either.
- **Reading issue comments or bodies back** as owner answers: an outsider can write there.
  Answers come only through `judge-review`.
- **Filing every candidate:** 100+ a week, mostly gone or dismissed.
- **A bot identity:** see D8.
- **Age-out, nightly passes, tagging plausible findings:** see D3, D5, D6.
- **Verify records the files or symbols a trigger depends on** (concern 5): the triggers are prose
  about callers that don't exist yet, so there is nothing to record. The cap (D4) bounds the cost of
  the proxy instead.
- **Watching the bug's function instead of its file:** no trigger names the bug's own function, and
  it would miss callers in the same file.
- **A backup of the store** (concern 4): see D15.

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
