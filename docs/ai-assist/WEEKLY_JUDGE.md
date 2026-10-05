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

`pixi run judge-weekly` (`scripts/judge/weekly.py`), run by a systemd timer every Monday at 23:59:

1. **Pin.** Fetch, resolve `origin/main` to a SHA, and reset the persistent worktree
   (`~/.cecelia-effectiveness/judge-worktree`) to it.
2. **Owner answers.** Fold `pixi run judge-review` answers into the earlier records, so a bug
   answered `wont_fix` is not carried.
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

   Then one tool-less judge call reads the excerpts as data and marks each bug `live_bug` / `gone` /
   `not_a_bug`. Commits pushed to a PR's branch after it merged are reported as `stranded`.
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
   and open the PR. Each new PR closes the previous one.

A crash at any stage still writes a failure record and its PR. The recital console warns when the
newest record is more than 9 days old or failed (`judge_staleness.py`).

**Spend.** The record's *Tokens* row has what each step used: output, uncached input, and cache read
and write, taken from each call's `usage`. Its *Spend* row is the CLI's dollar figure, which is list
price, not what a seat is charged. The caps are in dollars, because `--max-budget-usd` is the
CLI's only limit: bug sweep $1.50, verify $10 ($2 per group), rule mapping $2.

## Working the record

`pixi run judge-review` is where the record gets worked. It runs full screen, one bug per screen:

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
| `pixi run judge-review` | Decide, then work the open bugs (`[f] fix now` opens a briefed session) |
| `pixi run judge-record D --mirror` | Re-render a stored record |

Timer install, adjust and uninstall steps: [`scripts/judge/systemd/README.md`](../../scripts/judge/systemd/README.md).
