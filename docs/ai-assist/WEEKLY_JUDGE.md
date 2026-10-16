# Weekly judge

Once a week, a pass reads the effectiveness log's reviewer findings and answers two questions:

- **Which findings nobody fixed are still bugs in shipped code?** The work list a session fixes from.
  Duplicates count too: code a convention check said copies an existing helper, still not reused.
- **Which CLAUDE.md rules keep breaking, across sessions?** The input for tightening a rule or making
  it a mechanical check.

It replaced the CLAUDE.md compliance eval (retired 2026-10-04,
[`../archive/CLAUDE_MD_EVAL_PLAN.md`](../archive/CLAUDE_MD_EVAL_PLAN.md)). That eval replayed canned
single-shot tasks. After three passes every prompt passed 3/3 while real sessions kept breaking the
same rules: in one week the discovery step was broken 27 times across 9 sessions, and its prompt
was green. The real signal is in the log. Recital catches a violation at commit time, the judge
counts them across sessions, and the autonomous runs ([`../todo/AGENT_OVERNIGHT_PLAN.md`](../todo/AGENT_OVERNIGHT_PLAN.md))
test whether an agent can actually use the framework.

## What a pass does

`pixi run judge-weekly` (`scripts/judge/weekly.py`), run by a systemd timer every Tuesday at 23:59:

1. **Pin.** Fetch, resolve `origin/main` to a SHA, and reset the persistent worktree
   (`~/.cecelia-effectiveness/judge-worktree`) to it.
2. **Owner answers.** Fold `pixi run judge-review` answers into the earlier records, so a bug
   answered `wont_fix` is not carried. An answer given to an older record after the last pass
   started applies to the last record's bug with the same key: the latest answer per bug wins.
   **Run reviews** (`run_reviews.py`). Read the agent run records: Blackboard entries with meta
   `agentRun` in the projects dir (`CECELIA_AGENT_APP_PROJECTS`, else
   `~/cecelia-feijoa/projects`; `--projects-dir` overrides). Each section a person marked `bad` with
   cause `guide` (the guide didn't say it) or `platform` (the agent couldn't see it) is logged once as
   an `agent_run_finding` with `kind: "review"`, keyed by project + entry + section. It carries the
   run, the section, the cause, the note and the guide when the record names one (`agentRun.guide`).
   A Claude proposal and a `bad` with no cause (marked before causes existed) are skipped. `agent`
   causes stay on the run record; the pass's record lists them per guide under *Agent causes per
   guide*, and flags a guide whose `agent` notes span 2+ runs as a possible guide gap. Whether two
   notes describe the same gap is the owner's read: no judge step compares notes.
3. **Bug sweep** (`bugs.py`). Candidates are fanout findings logged since the last pass that were
   never fixed (`shipped_with_finding`, `dropped_no_action`, `false_positive`, untagged, and every
   `plausible` note not tagged `fixed_pre_commit`), plus the last record's `open` / `unjudged` / `unmerged` / `parked` bugs. Free checks
   come first:
   - findings on frozen paths (a judge record, `docs/archive/`) are dropped;
   - a finding whose branch hasn't reached the SHA is `unmerged` and waits;
   - the excerpt is the whole function around the line (`enclosing.py`), found at the finding's
     own commit, so a moved line still shows the right code;
   - a function that no longer exists is `gone`;
   - findings on one file + function are merged into one bug;
   - a `parked` bug (verify said `guard`, step 4) skips the judge and stays parked, unless a commit
     names its key (*Landed fixes* below: the guard may have gone in elsewhere) or one since its
     verify SHA touched its file (an agent-run error with no file: unless a run hit it again after it
     was verified). Then it goes back to `open` with its verdict cleared, and verify looks at
     it again. At most 3 a pass (`PARKED_RECHECK`), a landed fix first, then the oldest parking:
     the check is a proxy, and a busy file would otherwise send its every parked bug back each
     week. The rest stay parked and go first next pass. A carried `open` bug whose verdict is `guard` (a record from before parking) is parked.

   **Duplicates** (`kind: "duplicate"`, key `conv-…`): convention findings marked `should reuse` that
   were never fixed. New code copying a helper doesn't misbehave, so the judge's question for these is
   whether the copy is still there and does the job of the helper the finding names; when the
   finding gives that helper's `path:line`, the judge sees its code beside the copy's. Same verdicts
   and statuses; verify's `guard` doesn't apply. Issues get the `duplication` label, and the fix
   brief asks for the helper to be used and the copy deleted, not for a failing test. `wrong home`
   findings (comment placement) and the convention advisories stay rules-only (step 5). The pass
   records the sources it swept (`run.swept`); a source the previous pass didn't sweep is read from
   the start of the log, so the first pass that sweeps duplicates judges their whole backlog once.

   Errors the autonomous runs hit (`agent_run_finding`, one per error key, counting the runs that hit
   it) are candidates too. One with a `file:line` from a backend stacktrace goes through the checks
   above. One without has no code to excerpt: it is `open` straight away and goes to verify. A
   `wont_fix` one is carried, so a run that hits it again doesn't raise it as new.

   A **run review** (`kind: "review"`, key `rev-…`) has no code to excerpt either: it is `open`
   straight away and goes to verify. A person set its cause, so verify doesn't ask whether it is
   real: it checks whether the fix is in (the guide's text in `mcp/cecelia_mcp/guides.json`, or the
   tool or surface the note names). `dismiss` means this code already has it. One run's reviews go
   to one agent. The rows are stamped a second before the pass started, so the next pass's window
   doesn't read them again.

   A **repeated agent error** (`kind: "repeat"`, key `rep-…`) is the exception to "a 4xx with a
   reason is the agent's own input". One alone never reaches the judge: the emitter logs it as an
   `agent_run_misuse` observation, which the judge doesn't read. Once the same mistake has been
   hit in **2 separate runs**, each run that hits it again logs a finding carrying the count
   (`runs`). The judge takes that count, not the number of rows, and keeps the newest message,
   because a fix often improves the message. Verify judges the guidance, not the input: does the
   tool's description, its MCP guidance or the error message steer an agent to a call that
   passes? A dismissed one is carried muted and re-opens, to be verified again, once its count
   reaches **twice** what it was dismissed at. The message still fires on every run that makes the
   mistake, even after a fix, so any single new hit would re-open it every week.

   "The same mistake" = tool + HTTP status + the reason's template: its first clause (up to
   ` — `, `; ` or `. `), normalised, cut to its leading plain words (at most 3). Values (quoted
   names, ids, numbers, dotted column names) end the run of words. A hint a fix appends later
   doesn't change the key. With fewer than two leading words (a value comes first), the whole
   normalised clause is the template. Three words, because a plain-word value can follow the
   template with nothing to mark it: `No values for volume / …` and `No values for
   mean_intensity_0 / volume …` are one mistake.

   On the 9 runs of 2026-10-04/05 this raised 4 repeats: the `+` in a chain name (5 runs),
   `get_cohort_qc` with no cohort metrics for a composite (3), `gate_plot` on a column the cell
   table doesn't have (3), and `get_cohort_qc` without a set uid (2, night 1's one-image copies).
   The first three were the ones fixed by hand (#1426–#1428); the fourth came from one-image
   copies with no set to name, which the multi-image copies replaced
   ([`../todo/AGENT_RUN_REVIEW_PLAN.md`](../todo/AGENT_RUN_REVIEW_PLAN.md) Decision 1). The one
   single-run 4xx (an unknown plot name) stayed out.

   **Landed fixes** (no Claude call). For each carried `open` / `unjudged` / `parked` bug, git looks for commits
   since the last pass (`previous run.sha..sha`) whose message names one of its keys (`fanout-…`,
   `rep-…`, `run-…`, `stranded-pr…`). They go on the bug as `fix_landed`, with the PR from `(#N)` in
   the subject or the `Merge pull request #N` that brought them in. That is evidence, not a verdict:
   the judge and verify prompts get it as a hint, and the judge still decides `gone`. The record says
   *Fix landed: … — awaiting re-check* until it does, or verify dismisses it. So a fix you landed with `pixi run
   judge-review` shows up even on a pass whose judge failed. On 2026-10-05, 6 of the 7 carried open
   bugs had a fix commit naming their key on main; the judge never ran, so the record said "0 fixed".
   A bug with a landed fix that is still `open` after the judge (it said `live_bug`, failed, or the
   bug has no code to excerpt) and has a verify verdict goes back to verify this pass (`reverify`):
   that verdict predates the fix. The excerpt judge can't close it alone, because the excerpt can
   miss the fix: on 2026-10-13, 9 bugs whose fixes had merged stayed open, the judge saying the
   fixed code wasn't in its excerpt, and verify skipped them for already having a verdict. The old
   verdict stands until a new one replaces it.

   Then tool-less judge calls read the excerpts as data and mark each bug `live_bug` / `gone` /
   `not_a_bug`, 40 per call (`BATCH_ITEMS`: it bounds the prompt, not the spend). Calls continue until
   nothing is left or the next one could take the sweep past **$4** (`SWEEP_USD`): a call starts only
   while what's spent plus its whole $1.50 budget fits, so $4 is four calls, 160 bugs, against a
   measured inflow of about 100 a week. Bugs with a landed fix go first, then carried `open` ones,
   then the rest oldest first. A failed call ends the sweep; what earlier calls judged stands.
   Commits pushed to a PR's branch after it merged are reported as `stranded`. A bug the judge gives
   no verdict for (a call failed, or the budget ran out before it) waits as `unjudged`, except one the
   last record had `open`: it stays `open`, with its verdict, because a missing check is no evidence
   it was fixed. A `not_a_bug` on a carried bug verify called `fix` or `decide` doesn't dismiss it:
   the excerpt judge has no tools, so the bug stays `open` with its verdict dropped and verify,
   which traced the code, checks it again in the same pass.

   The `unjudged` count is the **backlog**. The record's *Backlog* row and the PR say it against the
   last pass (`N waiting for the judge (last pass M)`), and the recital console warns when it grew
   two passes in a row: the budget no longer covers the inflow, and you decide whether to raise
   `SWEEP_USD`. Nothing ages out.
4. **Verify** (`verify.py`). Each open bug that no agent has checked yet, or checked before its fix
   landed (`reverify`, step 3), goes to a read-only
   `claude -p` agent in a sandboxed checkout at the SHA (`agent_sandbox.py`: no network, `~` is
   write-denied, no MCP). Bugs that share a branch or a file go to the same agent, and so do one
   run's errors. The verdict is
   one of:
   - `fix`: the bug is live;
   - `guard`: it can't happen today, and the agent names what would make it live. The bug becomes
     `parked`: off the work list, listed under *Parked*, and back to verify when its file changes;
   - `decide`: only you can answer it, so it goes on the owner queue;
   - `dismiss`: the bug becomes `dismissed`.
5. **Rules** (`rules.py`). Every fanout + convention finding from the last 30 days is mapped to the
   one CLAUDE.md `##` section it breaks, by one tool-less judge call. Git sorts each finding into
   `agent_made` (the reviewed diff wrote it) or `legacy` (the code was there before). A rule broken
   in **3 or more different sessions** gets a proposal:
   - `tighten`: agents keep missing the rule. Reword it, or turn it into a mechanical check.
   - `ratchet`: older code keeps the shape alive. Add a test that bans it.

   A finding counts only if it came after the section's last edit and after your last answer to a
   proposal for that rule in `judge-review`. So a rule you tightened, or a proposal you rejected,
   comes back only if sessions keep breaking it.

   The count is sessions, not findings: one session's fanout can raise ten findings about a single
   pattern. A finding logged before its section last changed (`git log -L` over the section) doesn't
   count: once a section is tightened, its proposal doesn't recur from the old findings.
6. **Record.** Write `~/.cecelia-effectiveness/judge-runs/<date>.json` and, beside it, its rendered
   `<date>.md`. The JSON is the only source; every reader (`judge-review`, fix sessions, the next
   pass) reads it.
7. **Publish**, one of two layouts:
   - **Issues** (the default). The bugs go to
     GitHub issues (step 8). The pinned **Judge status** issue (label `judge-status`, filed, locked
     and pinned by the first pass) gets one comment: the bugs filed, closed, reopened and held,
     the backlog and its warning, any step whose judge failed, the rules PR, and spend. A PR is
     opened only when there are rule proposals: it carries `EFFECTIVENESS.md` (`pixi run
     audit-rollup`) to main and lists the proposals, and supersedes the last one. A pass without
     proposals closes any record PR left from before the switch, and leaves an open rules PR alone.
     A failed pass comments on the status issue instead of opening a PR. A failed mirror or status
     comment is kept on the record (`run.issues.error`, `run.status_comment.error`) and the recital
     console warns about it.
   - **Record PR** (`--no-issues`, or `JUDGE_ISSUES=0` in the timer's unit: *Issues are the default*
     in [`scripts/judge/systemd/README.md`](../../scripts/judge/systemd/README.md)). Mirror the record with its markdown to
     `docs/ai-assist/judge-runs/`, commit it on `judge-run/<date>` with recital, and open the PR.
     Each new PR closes the previous one. A failure's PR closes none: the last pass's record is
     still the work list.
8. **Issues** (`issues.py`, issue layout only). Each open bug
   verify called `fix` or `decide`, stranded commits and a repeated agent error get a GitHub issue,
   titled `[<key>] <verdict>: <where>` and locked as soon as it's filed. The issue follows its bug:
   - closed as completed when the judge confirms it `gone`, or verify dismisses it after a fix landed;
   - closed as not planned when it's `dismissed` (the comment says by whom: the owner, verify or the
     judge), `wont_fix` or `parked`;
   - closed too when GitHub still lists it open but the record no longer has its bug live, whatever
     pass dropped it: the mirror reads the open judge issues, not just the last record;
   - reopened when it's live again, a `fixes #N` close included;
   - labelled `fix-landed`, with one comment, when a commit names its key.

   It is a write-only mirror. The record is the only source, and the one thing read back is each
   judge issue's number, title and open/closed state: labelled `judge-bug`, opened by the `gh`
   user, and mapped to that number in the record (`bug.issue`). A forged issue with a copied key is
   ignored; an edited one is rewritten at the bug's next change. Bodies come from the record's
   fields; in the prose ones `@name` and `#N` go inside code spans, home paths become `~` and
   project/image uids `<uid>`, and a body that still carries any of those isn't filed (*held*).
   `gh` is allowlisted to the issue subcommands this needs. At most 20 creates a pass, one a
   second; the rest wait. A mirror that fails says so on `run.issues` and leaves the pass standing.
   Design: [`docs/todo/JUDGE_WORKFLOW_PLAN.md`](../todo/JUDGE_WORKFLOW_PLAN.md) D7–D11, D14.
9. **Chart** (`chart.py`, best-effort, last). The README's chart is rendered from the log and the
   store and pushed to the `judge-assets` branch; *The chart* below. Its own timer
   (`weekly-chart.timer`, Monday 00:30) draws it too, the night a week completes. With `--no-pr` it is rendered
   but not pushed. Whatever happens is kept on `run.chart` (`pushed`, `skipped`, or `error`) and
   printed; a chart failure never fails the pass.

The record PR's headline is **Bugs: N open** (K verified, X new): K counts the open bugs a verify agent has
a verdict on, so an agent-run error nobody has traced yet doesn't read like a checked bug (nor does a
verdict from before a landed fix that verify didn't reach). Then the
parked count (and how many of them are due a re-check), what was fixed since the last pass, and the backlog against the last pass.

A crash at any stage still writes a failure record and its PR. So does a **usage limit** (HTTP 429,
`RateLimited`, from the sweep, verify or rules — read by `claude_cli.rate_limit`, the one reader the
autonomous runs share; `judge.py` re-exports it): every call after it would fail too, and on
2026-10-05 a pass that hit one recorded a normal-looking week with nothing judged, none of the
fixes found, and $10 of "spend" for five agents that never started.

**Waiting out the limit.** A pass stopped by the usage limit exits 75 (`EX_TEMPFAIL`) and writes
`~/.cecelia-effectiveness/judge-ratelimit.json` (`reset`, `message`, `retry`, `stage`). The reset is
read from the CLI's message ("resets 1:40am (Australia/Sydney)") as the next time that clock reads
1:40 in that zone, or an hour from now when it can't be read. `cron_pass.sh` sleeps until 5 min
after the reset and reruns the whole pass, at most 3 attempts. weekly.py decides whether a retry
follows (`retry`: attempts left in `JUDGE_RETRY_LEFT`, and the reset at most 8h off), and while one
does it opens no FAILED PR. The failure record is still written; the retry's pass record replaces
it. The attempt that gives up opens the FAILED PR as before. A retry pins the same commit: the timer
runs `--ref HEAD` in the judge worktree, and a pass stopped by the limit never commits there. The
wrapper **keeps the cron lock through the wait**, so the nightly agent run is skipped: it would hit
the same limit. The unit's `TimeoutStartSec=12h` covers one wait. Any other failed judge call
doesn't stop the pass; the record's `run.failed` names the step, and the record and PR say that
step didn't run instead of reporting a quiet week. The recital console warns when the newest record
is more than 9 days old or failed, and when the backlog grew two passes in a row (`judge_staleness.py`).

**Spend.** The record's *Tokens* row has what each step used: output, uncached input, and cache read
and write, taken from each call's `usage`. Its *Spend* row is the CLI's dollar figure, which is list
price, not what a seat is charged. The caps are in dollars, because `--max-budget-usd` is the
CLI's only limit: bug sweep $4 ($1.50 per call), verify $10 ($2 per group), rule mapping $2. Verify's cap counts
a failed agent that never said its cost at its whole budget (`reserved_usd`); its spend (`usd`) is
only what the agents reported.

## Working the record

`pixi run judge-review` is where the record gets worked. It runs full screen, one bug per screen;
a key answers as soon as it is pressed, no Enter (`[z]` undoes a slip). PgUp/PgDn or the mouse wheel scroll a bug taller
than the screen, and a resize repaints it at once:

1. **Decide.** The bugs a verify agent sent to you. `[o] follow recommendation` (stays open; a
   session follows the agent's recommendation), `[a] answer` (in your own words; a session follows yours instead), or
   `[w] won't fix` (dropped, never carried again).
2. **Work.** Every other open bug: `fix` first, then the `decide` bugs you followed or answered,
   then the rest. A `guard` bug is parked, so it isn't offered. `[f] fix now` starts an interactive Claude Code session where you
   start sessions (the folder holding the main checkout and its worktrees, `~/cc-workspace/cecelia`),
   briefed on that bug: where it is, what verify found, your answer or the recommendation, and how to
   work it (confirm on `origin/main`, its own worktree from `pixi run bootstrap-worktree fix-<key>`,
   sibling call sites, a failing test, recital, the bug key in the commit). You're in that session as usual; the queue comes back when
   you exit it. `[w] won't fix` closes a bug you don't want.

   **Bundles.** Work bugs that share a reviewed branch or a file come as one card, up to 8 (verify's
   own grouping, `verify.groups`): each bug's card without its evidence. `[f] fix now` starts one
   session briefed on all of them, in one worktree (`fix-bundle-<first key>`). It confirms each on
   `origin/main`, fixes the real ones, names each fixed key in the commit that fixes it, and ends by
   saying which it left and why. Every bug in the bundle gets its own fix-session answer, and one `[z]`
   takes them all back. `[s] one by one` splits the bundle into single cards for this session. A
   decide bug you answer joins its file's or branch's bundle at once.
3. **Rules.** The pass's rule proposals, one per screen: the section, what to do (`tighten` or
   `ratchet`), and the findings that broke it. `[e] edit the rule` starts a session briefed on them,
   in its own worktree (`rule-<id>-<date>`); it first says what the findings share, and stops if a
   rule can't fix it. `[r] reject` drops the proposal. Either answer resets the count (step 5).

**Issues.** Once the mirror runs (step 8), each card names the bug's issue under its title: the link,
*issue pending* (a pass stopped while filing it; the next adopts or files it), or *issue missing*
(the record maps it to a number that isn't a judge issue you opened). All of it comes from the
record; `judge-review` never calls GitHub. The fix brief names the issue, says not to read it, and
asks for `Refs #N` in the commit, never `fixes` / `closes`: the judge closes it when it confirms the
fix. Your answers reach the issue at the next pass: `[w]` closes it as not planned, `[o]` / `[a]`
add one comment (your words, defused like a body; on a bug filed that pass, right after the
create), and `[f]` labels it `in-progress` for one pass. A bug over the pass's create cap says
*issue not filed yet*.

**Unattended: `--auto`.** `pixi run judge-review --auto [--limit N]` works every `fix` bug (or
the first N sessions) without you, one session per bundle, one after another (`--dry-run` prints the briefs), with a live line
per step: spinner, elapsed time, the agent's tool calls and its latest; `pixi run agents` lists
the agent as `auto-fix^`. On the usage limit it waits for the reset and resumes the same
session (a recital round runs again); a reset more than 6 h off (`MAX_WAIT`) stops the run instead. A bug with a landed fix
(`fix_landed` on the record, or a commit naming its key on origin/main since) gets no session. The agent never
holds a credential: `scripts/judge/auto_fix.py` bootstraps its worktree (`fix-<key>` /
`fix-bundle-<key>`), runs it under the verify sandbox (no network, no `~/.config/gh`) with
`acceptEdits` keeping edits in the worktree, then stages the diff, runs recital, resumes the agent
to act on the findings and tag them, and runs recital again while that changed the diff (up to
`RECITAL_ROUNDS`, then it goes back to you), and commits, pushes and opens the PR itself, its body flagged
when the fix changes what is drawn on screen (look before you merge). Then it waits for the PR's CI and mergeability: red CI goes back to the same
session with the failed jobs' log excerpts (an unrelated failure is re-run once), a conflict with main
as a merge the agent resolves; after `CI_ROUNDS` (2) still red, the bundle goes back to you. A bug
counts as fixed only once its PR is green and mergeable. It never merges. After the agent, nothing of the worktree's
runs outside the sandbox: recital and the commit-msg hook run from your checkout, and git with the
hooks path pointed there. The script stops, worktree left, if the agent changed the worktree's
`.git` pointer, `.githooks`, `.claude` or `.mcp.json` (ignored files included), if the diff touches
`.github/`, the hooks, `pixi.toml`/`.lock`, any `CLAUDE.md` or the fence itself (`PROTECTED`), or
if it holds a credential. A fixed bug gets the same answer `[f]` gives, noting the PR. One the
agent called not real, or left, stays in your queue with its reason on the card
(`~/.cecelia-effectiveness/auto_fix.jsonl`) and isn't tried again; a failed session is removed and
tried once more. A guard, recital, commit or push that stops puts the bundle back in your queue, `left`; its card
names the worktree, and `[f]` opens a session on that work instead of a fresh worktree (not when
the agent changed what runs outside its sandbox: then `[f]` starts fresh). The agent can't run recital itself (its
reviewers are `claude -p` runs, which need the network), so this was deferred until the record
routinely carried 5+ `fix` bugs (14 on 2026-10-09). The `[f]` sessions it was measured against had
you in them; read the first few auto PRs for sibling call sites the agent missed.

A bug you started a fix session for isn't offered again. The next pass marks it `gone` once the fix
has merged; if it hasn't, the bug is still open there and back on the list. The bugs are also in
`~/.cecelia-effectiveness/judge-runs/<date>.md` for any session pointed at the record.

The rules-only PR only carries `EFFECTIVENESS.md` to main: merge it whenever, and answer its
proposals in `judge-review`.

No fix runs unattended: an unattended fix agent is deferred, see
[`../FUTURE.md`](../FUTURE.md) → *Unattended fix agent for the weekly judge*.

## The chart

The README embeds `effectiveness.svg` from the orphan `judge-assets` branch: what it shows and what
it can't is in [`AGENT_RECORD.md`](AGENT_RECORD.md). `chart.py` builds it in three steps:

- **Aggregate.** Per ISO week: changes reviewed and sessions (from the `_run` rows), PRs merged
  (merge and `(#n)` squash subjects on the pinned SHA's first-parent line), findings by
  their latest outcome (the `_finding` / `_finding_resolved` rows, joined on slug the way the rollup
  does), and how many first-parent commits on the pinned SHA touched the reviewer prompts or
  `CLAUDE.md`. Plus the newest pass record's bug counts by status, and per reviewer by week
  raised. Numbers only.
- **Render.** A hand-written SVG, deterministic: same numbers, same bytes. Five panels drawn like a paper
  figure (lettered, y axes with round ticks): changes reviewed and PRs merged; findings per 100
  changes and share fixed before commit, one line per reviewer; and the newest pass's verdicts as
  one stacked bar per week, shares of its judged notes (its n under the date), one panel per reviewer, by the week raised (the judge's own `bugs._key` joins a record's bug to its log
  row). Partial and low-session weeks are hollow points on dashed segments, and a low-session week
  gets no rate (leading ones aren't drawn); the week still running
  is left out of the picture. Okabe-Ito swatches from
  `share/console_palette.json`, mid-grey ink on a transparent background so one file reads on
  GitHub's light and dark themes; the numbers are in its `<desc>` too. Every label is fixed text or a
  number, XML-escaped anyway; an outcome the code doesn't know is drawn as *no outcome logged*.
- **Publish.** A blob per file, a tree and a commit with no parent (`git hash-object` / `mktree` /
  `commit-tree`: no checkout is touched), force-pushed to `refs/heads/judge-assets`, so the branch
  is always one commit. The `git` runner (`chart.Git`) allows only those calls, `ls-remote` of that
  ref, and a push of a bare SHA to it: nothing else runs on the owner's credentials. The push is
  skipped when the SVG is byte-identical to the last one pushed and the branch still holds that
  commit (`~/.cecelia-effectiveness/chart/pushed.json`); a deleted branch is pushed again. A failed
  push writes nothing, so the next pass tries again.

`raw.githubusercontent.com` serves the SVG as `image/svg+xml` with a 5-minute cache, so a push
shows in the README within minutes. If the branch is deleted, the README image breaks until the
next pass (or `pixi run effectiveness-chart -- --push`) recreates it.

## Commands

| | |
|---|---|
| `pixi run judge-weekly` | The pass. `-- --no-pr` writes the record only, `-- --dry-run` prints it, `-- --no-issues` publishes the old record-PR layout |
| `pixi run judge-bugs` | The sweep, printed (`--no-judge` is free) |
| `pixi run judge-verify --date D` | Verify a record's bugs; prints, never writes |
| `pixi run judge-rules` | The rule table + proposals, printed |
| `pixi run judge-run-reviews` | The run reviews a pass would log and the agent causes per guide, printed; logs nothing |
| `pixi run judge-review` | Decide, work the open bugs (`[f] fix now` opens a briefed session), then the rule proposals (`[e] edit the rule`) |
| `pixi run judge-review --auto` | Work the `fix` bugs unattended, one PR per session (`--limit N`, `--dry-run`) |
| `pixi run judge-record D --mirror` | Re-render a stored record |
| `pixi run effectiveness-chart` | Render the README chart to `~/.cecelia-effectiveness/chart/`; `-- --dry-run` prints the push, `-- --push` publishes to `judge-assets` |
| `pixi run judge-issues` | What the issue mirror would do for the newest record, every body printed; reads only. `-- --apply` does it |

On the usage limit, `judge-bugs` / `judge-verify` / `judge-rules` print one line with when it lifts
(`judge-bugs: usage limit — lifts 2026-10-07T01:40+11:00: …`) and exit 75, not a traceback.

Timer install, adjust and uninstall steps: [`scripts/judge/systemd/README.md`](../../scripts/judge/systemd/README.md).
