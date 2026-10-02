# CLAUDE.md eval — supervisor, run records, review queue

**Status:** in progress — phases 1–6 built 2026-10-01; open: the first supervised pass, which
measures the judge's cost so the cap can be set (Decision 4), and owner decision F6. Consolidates the two briefs at
`docs/archive/eval-supervisor-prompt.md` and `docs/archive/eval-dev-ui-prompt.md`, corrected against
the shipped eval and its sibling plans. Where this plan and a brief disagree, this plan wins.

Siblings: [`CLAUDE_MD_EVAL_PLAN.md`](CLAUDE_MD_EVAL_PLAN.md) (runner, rollup, cron),
[`CLAUDE_MD_EVAL_REFRESH_ROUTINE.md`](CLAUDE_MD_EVAL_REFRESH_ROUTINE.md) (add/retire rules),
[`CLAUDE_MD_EVAL_PUNCHLIST.md`](CLAUDE_MD_EVAL_PUNCHLIST.md) (diagnostic frame; P5 rotation script
stays deferred).

## Goal

Each Monday pass ends with a **run record** that a fresh session can act on cold: every failure
triaged, open findings carried forward, proposed setup and prompt changes backed by evidence, all in
one PR. Owner decisions go in through a terminal review queue, not by editing markdown. (A
frontend review panel was planned and dropped on 2026-10-01: about 5 minutes a week doesn't justify
Julia endpoints and a Vue page.)

Why: the 2026-09-30 pass scored 3/27. Re-scoring the same traces after #1314 gave 19/27, so most
failures were in the scorer. The findings were written to `~/Downloads/TMP/` and would have been
lost there.

## Decisions (2026-10-01)

1. **Diagnostic frame governs.** A prompt is a hypothesis about a weakness in the setup. Green for
   3 consecutive full passes → propose retiring it. There is no protected core set: the
   refresh-routine anchors were already retired (punchlist P4). Only `canary` is fixed. A prompt
   that needed an `infra` retry inside that 3-pass window isn't eligible to retire: N=3 per prompt
   reduces flakiness but doesn't remove it. Scores are compared per prompt-set version **and**
   sandbox setting, never raw across a change in either.
2. **Records are authoritative outside the repo.** They live at
   `~/.cecelia-effectiveness/eval-runs/<date>.json`, next to `events.jsonl` and `traces/`. The PR
   mirrors them to `docs/ai-assist/eval-runs/`. Delta, recurrence, curation and the review queue all
   read the local store, so an unmerged or superseded PR loses nothing.
3. **JSON is the source; markdown is rendered from it.** One `<date>.json` per run (no `run.json`)
   with a `schema_version` field, validated in a test. Shape it as scores attached to traces plus
   review-queue items, so a later swap to a hosted trace tool (e.g. Langfuse) stays cheap.
4. **Python orchestrates; `claude -p` only judges.** Locking, worktrees, reruns, rollups and the PR
   are deterministic code in `cron_pass.sh` → a new `supervise.py`. `claude -p` is used only for
   triage, curation and writing the record, and those calls get **no tools**. Python inlines the
   trace excerpts and log rows into the prompt and validates the output with `--json-schema`. The
   sandbox doesn't cover Read (F7), so "read-only" isn't a boundary; nothing to call is.
   **Measure before capping:** the first supervised run
   logs supervisor spend (turns + $) apart from the suite's, under only a high safety stop; the real
   cap is set from that number, as the $20 suite cap was set from the first pass.
5. **Pin the ref.** The supervisor pins the `origin/main` SHA and records it. `run_prompt.py` gains
   a `--ref` option, because `_make_detached_worktree` currently branches off the **user's checkout
   HEAD**. Per-prompt isolation already exists; don't add resets between prompts.
6. **One persistent supervisor worktree** at `~/.cecelia-effectiveness/eval-worktree`, running
   `reset --hard <sha>` each run. It keeps its own `.pixi`, reused incrementally. A new worktree per
   run would mean a full `pixi install` every week (see `scripts/bootstrap_worktree.sh` for why the
   env can't be moved).
7. **Triage classes:**
   - `scorer_bug`: fix on a branch and show the rescore delta; never auto-merge.
   - `infra`: rerun, at most 2 retries, each one logged.
   - `genuine`: write it up as a finding; no fix.
   - `decision`: the rule's meaning is ambiguous (e.g. `cite-algorithm`), so it becomes a question
     for the owner.
   - Never edit a scorer or prompt to pass without recording a `scorer_bug` with before/after scores.
8. **Curation proposes, never applies.** Adds need 3 or more findings (refresh routine D-R4) and
   cite their slugs. Retire rule as in Decision 1. At most 1 add per rule per run. Staying under the
   $20/week cap (D-R6) forces a paired removal. Red-team slugs are excluded (`_REDTEAM_SLUGS`). Added
   prompts run as unscored candidates until the PR merges.
9. **Both rollups regenerate:** `docs/ai-assist/CLAUDE_MD_EVAL.md` (`claude-md-eval-rollup`) and
   `docs/ai-assist/EFFECTIVENESS.md` (`effectiveness/rollup.py`).
10. **Commits go through recital** like any session, and its cost counts toward the cap. Every
    spawn sets `CECELIA_OBSERVER_NO_PAIR=1`.
11. **One open PR:** a new run closes the previous unmerged `eval-run/*` PR with a link to the new
    one. This is safe because of Decision 2.
12. **Recurring** means seen in 2 or more consecutive runs; anything else is marked `watch`.
    Setup-change proposals should come from recurring findings.
13. **Staleness guard:** warn in the recital console header when the newest local record is older
    than `cadence_days + 2` (default `cadence_days = 7`). Warn only, never block. A crashed run
    still writes a minimal failure record and opens the PR.
14. **Supervisor scope:** the Python orchestrator writes only to its worktree and the local store,
    pushes only `eval-run/*`, and opens and closes only its own PRs. The judge calls have no tools
    (Decision 4). Everything they're given is data, not instructions.
15. **Owner loop:**
    - Every 4th run, include 3–5 randomly sampled findings for the owner to label
      `real`/`false`/`unclear`. The supervisor never labels them.
    - After 8 runs, add a loop review (proposals merged, score trend, false-positive rate, cost)
      for the owner to decide whether to continue, retune or stop.
    - Each record also stores setup size: CLAUDE.md lines (root and `frontend/`), hook count and
      inventory doc count. Growth over 10% is flagged.
    - Owner decisions are append-only events in `~/.cecelia-effectiveness/review.jsonl`:
      `spot_check_label`, `finding_status`, `proposal_decision`. Corrections are new events. The
      supervisor applies new events at the start of each run. The terminal queue (phase 6) writes
      them, and never touches the repo.
16. **The timer runs from the supervisor's worktree (2026-10-02).** `REPO` in the unit is
    `~/.cecelia-effectiveness/eval-worktree`, and `ExecStartPre` resets it to `origin/main`, so the
    driver (`cron_pass.sh`, `supervise.py`) is always today's. It used to be the `cecelia-feijoa` dev
    checkout, which was 46 commits behind with no `supervise.py`, so the timer would have run the
    bare suite. A preflight check that failed when that checkout was behind or dirty would have
    failed most weeks, and a second driver checkout would need a second 7.7 GB `.pixi`. The setup
    also has a line budget (`SETUP_BUDGET`, then-current size + 10%). Going over it is a
    `decision` finding, never a block.
17. **A delta compares both passes under one scorer (2026-10-02).** The previous pass's traces are
    rescored by this checkout's scorer, so a scorer fix can't pass for a setup change. 09-30 → 10-01
    read 3/27 → 24/27 as logged, but 20/27 → 24/27 under one scorer. If more than one of
    {prompt set, sandbox, CLAUDE.md, scorer} changed, the delta says **confounded** and a hypothesis
    reads "consistent, not proven" instead of "held". The scorer counts only when traces were lost.
    Claude Code is named but not counted, because it can't be pinned. Per prompt, a move between 0/N
    and N/N is a change; one with a 1/3 or 2/3 side is `noisy`, a hint. At N=3 a 70% prompt still
    shows 3/3 a third of the time. Escalating noisy prompts to N=10 is deferred: two passes had one
    noisy prompt each.
18. **Each pass sweeps the log for unfixed bugs (2026-10-02).** `bugs.py`, between triage and
    curation. Candidates are fanout findings logged since the previous pass that nobody marked
    `fixed_pre_commit` or `false_positive`: `**confirmed**` ones shipped, dropped or never tagged, and
    every `**plausible**` advisory. The previous record's `open` and `unmerged` bugs are added to them.
    A finding whose change hasn't reached the pinned SHA (its branch has no commit after the
    finding's commit, or that commit isn't merged) is `unmerged` and waits. For the rest, Python
    inlines ±20 lines at `file:line` from the pinned SHA, and one tool-less judge call (Decision 4,
    $1.50 cap, 40 items) says `live_bug` / `gone` / `not_a_bug`. The record's `bugs` list is the
    work list (`open` / `unmerged` / `gone` / `dismissed` / `wont_fix`): a session pointed at the
    record fixes the `open` ones on a normal branch, and the next pass marks them `gone`. New open
    bugs go on the owner queue, where `wont_fix` stops carrying them. Convention findings are
    code-shape, not bugs, and stay curation's input; a fixed bug still counts there as evidence
    about the setup. The supervisor still opens no fix PRs (Decision 14). First dry run on the
    real log: 24 judged for $0.38, 16 open, and the two confirmed authorship findings mislogged as
    dropped came back `gone`.

## Run record (fields)

- **Run metadata:** date, pinned SHA, prompt-set version (list + hash), sandbox setting (hash of
  `_SANDBOX_SETTINGS`), CLI/model version, cost, retries, trace dir.
- **Results:** score raw → after scorer fixes; per-prompt result + class; candidates (unscored).
- **Findings:** `id, slug, class, status, recurring|watch, evidence (trace path + excerpt),
  diagnosis, proposed fix`.
- **Proposals:** prompt add/change/retire with source slugs; setup changes with a
  `Hypothesis: fixes <id>; expect <prompt> to pass`.
- **Delta since last run:** findings opened/resolved, both scores under this checkout's scorer
  with any version change noted, `confounded` and per-prompt `moves` (Decision 17), and whether each
  earlier hypothesis held.
- **Tracking:** setup-size metrics, plus spot-check and loop-review sections when due.
- **Next actions:** ordered, each self-contained (files, the exact change, the verify command).
  The record's header says how to use it: point a session at this file, do the open Next actions,
  update each finding's Status. No separate README.

## Seed record — 2026-09-30

Source: `~/Downloads/TMP/eval-2026-10-01-findings.md` + #1314 (merged). **Written** as
[`docs/ai-assist/eval-runs/2026-09-30.md`](../ai-assist/eval-runs/2026-09-30.md), each finding
checked against its trace and today's code. F3 and F6 are `recurring` (failing in consecutive full
passes); the rest are `watch`. F7 is resolved by the sandbox.

| Id | Slug | Class | Finding |
|---|---|---|---|
| F1 | `frontend-copy-canonical` | genuine | `CLAUDE_TERMINAL` (`lib/claudeOverview.ts`) is orphaned; `KiwiCockpit.vue` renders its own copy |
| F2 | `frontend-copy-canonical` | scorer_bug (unfixed) | anti-pattern matched inside an HTML comment. Fix in phase 2 as a per-prompt opt-in, not a blanket strip: `cite-algorithm` is scored *on* a comment |
| F3 | `kill-process-tree` | genuine | `_kill_tree` has no grace-then-force mode |
| F4 | `discovery-first` | genuine | `slice_utils.py` docstring promises helpers that are gone; the probe premise is false |
| F5 | `dir-size` | genuine | CLAUDE.md + `docs/DEV.md` name `_dir_bytes`; the inventory names `_path_bytes` |
| F6 | `cite-algorithm` | decision | does a docstring count as the citation comment? |
| F7 | — | decision | eval agents run with `--dangerously-skip-permissions` (`run_prompt.py`); one ran `kill -9`. No tool restriction has existed: #1272's sandbox was plugin-eval (closed — no CLAUDE.md), #1274's was log isolation (trimmed in `c6463720`). Probe result under *Open questions* |

Record and classify these only; the fixes are separate work.

## Phases

Each phase is its own PR.

1. **Record store — built.** `scripts/claude_md_eval/record.py` (`pixi run claude-md-eval-record`):
   `build` / `validate` / `load` / `write` / `render_markdown`, tests in
   `python/cecelia/tests/test_claude_md_eval_record.py`, and `docs/ai-assist/eval-runs/` in
   `_SKIP_DIRS`. Checkpoint met: `replay --date 2026-09-30` rescores the saved traces 3/27 → 19/27,
   the same as #1314's rescore. Choices made while building:
   - A replay can't recover the pinned repo SHA: the log's `commit` is the CLAUDE.md blob. It
     records `sha: null` and the blob. `--ref` says where setup size is measured (for 09-30,
     `358df7f2`, whose CLAUDE.md blob matches the pass).
   - `scored_at` is this checkout's HEAD, since rescoring reads its prompts and scorer.
   - The validator is hand-written: `jsonschema` is only a transitive dependency.
   - A replay without `--annotations` keeps the existing record's findings, so a rescore after a
     scorer fix doesn't drop them.
   - Only *open* `decision` findings go on the owner queue, plus every proposal.
   - `delta` stays `null` until phase 4.
2. **Scorer F2 — built.** A per-prompt `anti_signal_ignore_comments` opt-in in `_regex_hits`
   (`run_prompt.py`), enabled for `frontend-copy-canonical`. It strips `<!-- -->`, `/* */` and `//`
   comments (not `://`) before the anti regex only. Checkpoint met: rescoring all 46 saved traces
   flips `20260930T141134Z-frontend-copy-canonical-with-r2` and nothing else.
3. **Supervisor — built.** `scripts/claude_md_eval/supervise.py` (`pixi run claude-md-eval-supervise`),
   tests in `python/cecelia/tests/test_claude_md_eval_supervise.py`. `cron_pass.sh` runs it by
   default (`--no-supervise` for the bare suite). Checkpoint met: a crash at any stage writes a
   `kind: failure` record (`record.failure_record`). Choices made while building:
   - **No `--ref` in `run_prompt.py`.** The suite runs inside the persistent worktree, reset to the
     pinned SHA and checked, so its eval worktrees already branch from that SHA.
   - The worktree's `git clean -fdx` keeps `.pixi`, `.env` and `frontend/node_modules`; the first
     run pays a full `pixi install` there.
   - The suite runs under a session id the supervisor picks, so `rollup.pass_runs` finds exactly
     its rows. `record.build(suite=…)` takes that row, not just the day's last pass.
   - The judge call is `claude -p --tools "" --safe-mode --strict-mcp-config` in an empty temp
     dir, with `--json-schema`. Its input has the rule, the task, the scorer frontmatter and score,
     anti-signal matches, the first 40 tool calls, the final message and the diff (12 KB cap). A
     quoted `evidence` line that isn't in that input is marked `verified: false`.
   - Errored runs are `infra` without a judge call, and are retried (≤2) through
     `claude-md-eval-one`.
   - One finding per (prompt, class). `recurring` means the previous record in the store has the
     same pair.
   - A failure record never replaces a pass record of the same date.
   - `--session S --dry-run` re-triages a logged pass. On the 09-30 pass that was 8 judge calls,
     $0.58 (~$0.07 each), and the findings matched the hand-written seed.
   - The judge budget is a high safety stop ($10, $0.75 per call), so the first real pass measures
     the real cost. The cap gets set after that.
4. **Close-out — built.** Checkpoint met: `test_eval_staleness.py` fires on an old store and is
   silent on a fresh one.
   - `record.delta` / `with_delta`: findings matched on (slug, class), so each is opened, still
     open or resolved. The score is shown with every version that changed (prompt set, sandbox,
     CLAUDE.md, Claude Code). Each earlier proposal's `expect <prompt> to …` hypothesis is checked
     against this run.
   - `supervise.publish`, in the persistent worktree:
     - writes branch `eval-run/<date>` with the mirrored record and both rollups
       (`claude-md-eval-rollup`, `audit-rollup`);
     - commits with the `pixi run recital` output in the message, then pushes;
     - opens the PR, or reuses it on a same-date rerun, and closes every other open `eval-run/*`
       PR with a link to the new one.
     A failure record gets a PR too, once the worktree exists. `--no-pr` skips publishing, and a
     `--session` re-triage never publishes.
   - `prepare_worktree` detaches before `reset --hard`, so last week's `eval-run/*` branch never
     moves.
   - Staleness: `cecelia.effectiveness.eval_staleness` puts one yellow line in the recital
     console's header (and at the start of `--stream`) when the newest record is older than
     7 + 2 days, or when it is a failure. A machine with no store stays silent.
5. **Curation — built.** `scripts/claude_md_eval/curate.py` (`pixi run claude-md-eval-curate`),
   called by the supervised pass, which puts the proposals in the record and on the owner queue.
   Checkpoint met: on the 2026-10-01 log it proposed one setup change (F3, recurring, so
   `expect kill-process-tree to pass`) and no adds, for $0.19.
   - **retire:** green in each of the last 3 full-catalog records with the same prompt-set hash and
     sandbox, and no infra retry for that prompt in the window. `canary` never retires. Only
     supervised records count, so the first retire proposals come after three of them.
   - **add:** reviewer findings from the last 30 days, red-team slugs excluded. The finding rows
     carry no rule (the routine's `payload.rule` doesn't exist), so one tool-less judge call maps
     each finding onto a CLAUDE.md `##` section and the prompt covering it. Answers naming an
     unknown rule or prompt are dropped. A rule with ≥3 uncovered findings is proposed with its
     slugs (D-R4).
   - **cap:** an add estimated at the mean prompt cost that pushes the week over $20 (D-R6) names a
     paired removal: a retire candidate first, otherwise the prompt whose pass rate moved least
     across the last 4 records.
   - **setup / scorer:** one per open recurring `genuine` / `scorer_bug` finding, with the
     `expect <prompt> to pass` hypothesis the next record's delta checks.
   - Not built: authoring the added prompt, or running candidates unscored. The add proposal stops
     at "which rule, which findings"; writing the prompt stays with whoever accepts it.
   - `judge.py` holds the one tool-less `claude -p` call, shared by triage and curation.
6. **Owner loop — built.** `scripts/claude_md_eval/review.py` (`pixi run recital-review`), tests
   in `python/cecelia/tests/test_claude_md_eval_review.py`. Checkpoint met: a missing record, an
   other-schema record or an empty store each give a one-line error.
   - **Queue.** It walks the record's queue items that have no live answer, one key each:
     - decision: resolved / dropped / open (resolved asks for the answer as a note);
     - proposal: accept / reject / defer;
     - spot check: real / false / unclear;
     - loop review: continue / retune / stop.
     It also has skip, undo and quit keys, an `[n/total]` progress count, and an empty state. It
     is line-based, not raw-key, so it runs anywhere `input()` does, scripted tests included.
     Colours come from the shared console palette.
   - **Events.** Answers go to `~/.cecelia-effectiveness/review.jsonl`: `finding_status`,
     `proposal_decision`, `spot_check_label`, plus `loop_review_decision` for the loop review.
     An undo is a new event with `value: null` and `corrects: <id>`. The latest event per
     (record, event, ref) wins.
   - **Applying.** At the start of a pass the supervisor folds the events into every earlier
     record and rewrites it, unless it's a dry run. So recurrence, the delta and curation see the
     owner's statuses and decisions.
   - **Spot check.** On every 4th supervised run, 3–5 of that run's findings are sampled, seeded
     by date so a rerun picks the same ones. They become `spot_check` queue items.
   - **Loop review.** On every 8th run it shows scores per run, proposals accepted (of those
     made), the spot-check false-positive rate and total cost. It becomes a `loop_review` item.
   - **Setup size.** Growth over 10% in any setup-size metric is flagged in "Since last run".

## Open questions

- F7 — eval-agent containment (**resolved**, below). Probed 2026-10-01 (`claude` 2.1.286).
  Tool restriction alone can't contain it: agents do all discovery through Bash, and Bash can run
  anything. The sandbox needed an AppArmor profile for `/usr/bin/bwrap` first: Ubuntu 24.04 sets
  `apparmor_restrict_unprivileged_userns=1`. The owner installed `/etc/apparmor.d/bwrap` the same
  day (10:02). It's a machine prerequisite for every sandboxed run.
  With `sandbox.enabled` + `allowUnsandboxedCommands: false`:

  | Check | Result |
  |---|---|
  | Bash write outside worktree | blocked (read-only FS) |
  | Bash `kill` of a host process | blocked (PID namespace: "No such process"; the target survived) |
  | Bash GET on host `:8080` (listening) | blocked (connection refused) |
  | Bash `curl https://example.com` | **allowed (200)** — needs a network allowlist |
  | Bash read outside worktree | **allowed** — reads are open by default |
  | `dangerouslyDisableSandbox: true` | refused |
  | Write tool to `~` | **allowed** — the sandbox covers Bash only |
  | `permissions.deny` `Write(~/**)` / `Edit(~/**)` under `--dangerously-skip-permissions` | blocked; writes inside the worktree still worked |
  | CLAUDE.md canary | loaded |

  **Wired 2026-10-01** in `run_prompt.py` → `_SANDBOX_SETTINGS`. Re-probed through the real spawn
  path (~$1.9 total for all probes):
  - Bash and Write-tool writes to `~` are blocked; credential reads are blocked.
  - Network: `deniedDomains: ["*"]` blocks everything. `allowedDomains` does **not** restrict under
    skip-permissions (`[]` and `["github.com"]` both let `example.com` through).
  - Julia may write only `~/.julia/{compiled,logs,scratchspaces}`, so
    `julia --project=app -e 'using Cecelia'` works.
  - `pixi run` fails (read-only rattler cache, no network). It barely worked before, since a fresh
    worktree has no env.
  - Eval worktrees moved to `$TMPDIR/cecelia-eval`.
  - Still to do: a full suite pass to compare scores with the last unsandboxed one.
