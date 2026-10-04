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
   never fixed (`shipped_with_finding`, `dropped_no_action`, untagged, every `plausible`), plus the
   last record's `open` / `unjudged` / `unmerged` bugs. Free checks come first:
   - findings on frozen paths (a judge record, `docs/archive/`) are dropped;
   - a finding whose branch hasn't reached the SHA is `unmerged` and waits;
   - the excerpt is the whole function around the line (`enclosing.py`), found at the finding's
     own commit, so a moved line still shows the right code;
   - a function that no longer exists is `gone`;
   - findings on one file + function are merged into one bug.

   Then one tool-less judge call reads the excerpts as data and marks each bug `live_bug` / `gone` /
   `not_a_bug`. Commits pushed to a PR's branch after it merged are reported as `stranded`.
4. **Verify** (`verify.py`). Each open bug that no agent has checked yet goes to a read-only
   `claude -p` agent in a sandboxed checkout at the SHA (`agent_sandbox.py`: no network, `~` is
   write-denied, no MCP). Bugs that share a branch or a file go to the same agent. The verdict is
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

**Spend** is capped per pass at about $14: bug sweep $1.50, verify $10 ($2 per group), rule
mapping $2. The record's *Spend* row shows what each step cost.

## Working the record

Point a session at `docs/ai-assist/judge-runs/<date>.md`:

- **Bugs.** For each `open` bug, read the code at `file:line` on `origin/main`, confirm it, and fix
  it on a normal branch, naming the bug's key in the commit. The next pass marks fixed ones `gone`.
- **`decide` bugs** are yours: `pixi run judge-review` (one key each: leave it open, or `wont_fix`).
- **Rules.** Take a `tighten` or `ratchet` proposal like any other change. Every finding it cites is
  listed under *Sources*.

## Commands

| | |
|---|---|
| `pixi run judge-weekly` | The pass. `-- --no-pr` writes the record only, `-- --dry-run` prints it |
| `pixi run judge-bugs` | The sweep, printed (`--no-judge` is free) |
| `pixi run judge-verify --date D` | Verify a record's bugs; prints, never writes |
| `pixi run judge-rules` | The rule table + proposals, printed |
| `pixi run judge-review` | The owner queue |
| `pixi run judge-record D --mirror` | Re-render a stored record |

Timer install, adjust and uninstall steps: [`scripts/judge/systemd/README.md`](../../scripts/judge/systemd/README.md).
