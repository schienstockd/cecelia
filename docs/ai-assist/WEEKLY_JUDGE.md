# Weekly judge

Once a week, a pass reads the effectiveness log's reviewer findings and answers two questions:

- **Which findings nobody fixed are still bugs in shipped code?** The work list a session fixes from.
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
   answered `wont_fix` is not carried.
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
   never fixed (`shipped_with_finding`, `dropped_no_action`, `false_positive`, untagged, every
   `plausible`), plus the last record's `open` / `unjudged` / `unmerged` bugs. Free checks come first:
   - findings on frozen paths (a judge record, `docs/archive/`) are dropped;
   - a finding whose branch hasn't reached the SHA is `unmerged` and waits;
   - the excerpt is the whole function around the line (`enclosing.py`), found at the finding's
     own commit, so a moved line still shows the right code;
   - a function that no longer exists is `gone`;
   - findings on one file + function are merged into one bug.

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

   **Landed fixes** (no Claude call). For each carried `open` / `unjudged` bug, git looks for commits
   since the last pass (`previous run.sha..sha`) whose message names one of its keys (`fanout-…`,
   `rep-…`, `run-…`, `stranded-pr…`). They go on the bug as `fix_landed`, with the PR from `(#N)` in
   the subject or the `Merge pull request #N` that brought them in. That is evidence, not a verdict:
   the judge and verify prompts get it as a hint, and the judge still decides `gone`. The record says
   *Fix landed: … — awaiting re-check* until it does. So a fix you landed with `pixi run
   judge-review` shows up even on a pass whose judge failed. On 2026-10-05, 6 of the 7 carried open
   bugs had a fix commit naming their key on main; the judge never ran, so the record said "0 fixed".

   Then one tool-less judge call reads the excerpts as data and marks each bug `live_bug` / `gone` /
   `not_a_bug`. Commits pushed to a PR's branch after it merged are reported as `stranded`. A bug the
   judge gives no verdict for (the call failed, or the bug is over the 40-per-pass cap) waits as
   `unjudged`, except one the last record had `open`: it stays `open`, with its verdict, because a
   missing check is no evidence it was fixed.
4. **Verify** (`verify.py`). Each open bug that no agent has checked yet goes to a read-only
   `claude -p` agent in a sandboxed checkout at the SHA (`agent_sandbox.py`: no network, `~` is
   write-denied, no MCP). Bugs that share a branch or a file go to the same agent, and so do one
   run's errors. The verdict is
   one of:
   - `fix`: the bug is live;
   - `guard`: it can't happen today, and the agent names what would make it live;
   - `decide`: only you can answer it, so it goes on the owner queue;
   - `dismiss`: the bug becomes `dismissed`.
5. **Rules** (`rules.py`). Every fanout + convention finding from the last 30 days is mapped to the
   one CLAUDE.md `##` section it breaks, by one tool-less judge call. Git sorts each finding into
   `agent_made` (the reviewed diff wrote it) or `legacy` (the code was there before). A rule broken
   in **3 or more different sessions** gets a proposal:
   - `tighten`: agents keep missing the rule. Reword it, or turn it into a mechanical check.
   - `ratchet`: older code keeps the shape alive. Add a test that bans it.

   The count is sessions, not findings: one session's fanout can raise ten findings about a single
   pattern.
6. **Record + PR.** Write `~/.cecelia-effectiveness/judge-runs/<date>.json`, mirror it with its
   rendered markdown to `docs/ai-assist/judge-runs/`, commit it on `judge-run/<date>` with recital,
   and open the PR. Each new PR closes the previous one. A failure's PR closes none: the last
   pass's record is still the work list.

The PR's headline is **Bugs: N open** (K verified, X new): K counts the open bugs a verify agent has
a verdict on, so an agent-run error nobody has traced yet doesn't read like a checked bug.

A crash at any stage still writes a failure record and its PR. So does a **usage limit** (HTTP 429,
`judge.RateLimited`, from the sweep, verify or rules): every call after it would fail too, and on
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
is more than 9 days old or failed (`judge_staleness.py`).

**Spend.** The record's *Tokens* row has what each step used: output, uncached input, and cache read
and write, taken from each call's `usage`. Its *Spend* row is the CLI's dollar figure, which is list
price, not what a seat is charged. The caps are in dollars, because `--max-budget-usd` is the
CLI's only limit: bug sweep $1.50, verify $10 ($2 per group), rule mapping $2. Verify's cap counts
a failed agent that never said its cost at its whole budget (`reserved_usd`); its spend (`usd`) is
only what the agents reported.

## Working the record

`pixi run judge-review` is where the record gets worked. It runs full screen, one bug per screen;
a key answers as soon as it is pressed, no Enter (`[z]` undoes a slip). PgUp/PgDn or the mouse wheel scroll a bug taller
than the screen, and a resize repaints it at once:

1. **Decide.** The bugs a verify agent sent to you. `[o] keep open` (a session follows the agent's
   recommendation), `[a] answer` (in your own words; a session follows yours instead), or
   `[w] won't fix` (dropped, never carried again).
2. **Work.** Every other open bug: `fix` first, then the `decide` bugs you kept open or answered,
   then `guard`, then the rest. `[f] fix now` starts an interactive Claude Code session where you
   start sessions (the folder holding the main checkout and its worktrees, `~/cc-workspace/cecelia`),
   briefed on that bug: where it is, what verify found, your answer or the recommendation, and how to
   work it (confirm on `origin/main`, its own worktree from `pixi run bootstrap-worktree fix-<key>`,
   sibling call sites, a failing test, recital, the bug key in the commit). You're in that session as usual; the queue comes back when
   you exit it. `[w] won't fix` closes a bug you don't want.

A bug you started a fix session for isn't offered again. The next pass marks it `gone` once the fix
has merged; if it hasn't, the bug is still open there and back on the list. The bugs are also in
`docs/ai-assist/judge-runs/<date>.md` for any session pointed at the record.

**Rules.** Take a `tighten` or `ratchet` proposal like any other change. Every finding it cites is
listed under *Sources*.

No fix runs unattended: an unattended fix agent is deferred, see
[`../FUTURE.md`](../FUTURE.md) → *Unattended fix agent for the weekly judge*.

## Commands

| | |
|---|---|
| `pixi run judge-weekly` | The pass. `-- --no-pr` writes the record only, `-- --dry-run` prints it |
| `pixi run judge-bugs` | The sweep, printed (`--no-judge` is free) |
| `pixi run judge-verify --date D` | Verify a record's bugs; prints, never writes |
| `pixi run judge-rules` | The rule table + proposals, printed |
| `pixi run judge-run-reviews` | The run reviews a pass would log and the agent causes per guide, printed; logs nothing |
| `pixi run judge-review` | Decide, then work the open bugs (`[f] fix now` opens a briefed session) |
| `pixi run judge-record D --mirror` | Re-render a stored record |

Timer install, adjust and uninstall steps: [`scripts/judge/systemd/README.md`](../../scripts/judge/systemd/README.md).
