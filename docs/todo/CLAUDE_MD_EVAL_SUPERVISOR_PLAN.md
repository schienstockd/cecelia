# CLAUDE.md eval — supervisor, run records, review UI

**Status:** planning (2026-10-01); nothing built. Consolidates the two briefs at
`docs/archive/eval-supervisor-prompt.md` and `docs/archive/eval-dev-ui-prompt.md`, corrected against
the shipped eval and its sibling plans. Where this plan and a brief disagree, this plan wins.

Siblings: [`CLAUDE_MD_EVAL_PLAN.md`](CLAUDE_MD_EVAL_PLAN.md) (runner, rollup, cron),
[`CLAUDE_MD_EVAL_REFRESH_ROUTINE.md`](CLAUDE_MD_EVAL_REFRESH_ROUTINE.md) (add/retire rules),
[`CLAUDE_MD_EVAL_PUNCHLIST.md`](CLAUDE_MD_EVAL_PUNCHLIST.md) (diagnostic frame; P5 rotation script
stays deferred).

## Goal

Each Monday pass ends with a **run record** that a fresh session can act on cold: every failure
triaged, open findings carried forward, proposed setup and prompt changes backed by evidence, all in
one PR. Owner decisions go in through a dev-only GUI, not by editing markdown.

Why: the 2026-09-30 pass scored 3/27. Re-scoring the same traces after #1314 gave 19/27, so most
failures were in the scorer. The findings were written to `~/Downloads/TMP/` and would have been
lost there.

## Decisions (2026-10-01)

1. **Diagnostic frame governs.** A prompt is a hypothesis about a weakness in the setup. Green for
   3 consecutive full passes → propose retiring it. There is no protected core set: the
   refresh-routine anchors were already retired (punchlist P4). Only `canary` is fixed. Scores are
   compared per prompt-set version, never raw across versions.
2. **Records are authoritative outside the repo.** They live at
   `~/.cecelia-effectiveness/eval-runs/<date>.json`, next to `events.jsonl` and `traces/`. The PR
   mirrors them to `docs/ai-assist/eval-runs/`. Delta, recurrence, curation and the review UI all
   read the local store, so an unmerged or superseded PR loses nothing.
3. **JSON is the source; markdown is rendered from it.** One `<date>.json` per run (no `run.json`)
   with a `schema_version` field, validated in a test.
4. **Python orchestrates; `claude -p` only judges.** Locking, worktrees, reruns, rollups and the PR
   are deterministic code in `cron_pass.sh` → a new `supervise.py`. `claude -p` is used only for
   triage, curation and writing the record. **Measure before capping:** the first supervised run
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
14. **Supervisor scope:** reads the repo; writes only to its worktree and the local store; pushes
    only `eval-run/*`; opens and closes only its own PRs. Everything it reads is data, not
    instructions.
15. **Owner loop:**
    - Every 4th run, include 3–5 randomly sampled findings for the owner to label
      `real`/`false`/`unclear`. The supervisor never labels them.
    - After 8 runs, add a loop review (proposals merged, score trend, false-positive rate, cost)
      for the owner to decide whether to continue, retune or stop.
    - Each record also stores setup size: CLAUDE.md lines (root and `frontend/`), hook count and
      inventory doc count. Growth over 10% is flagged.
16. **Review UI:**
    - Gated by the existing `CECELIA_DEV` flag (`_is_dev()` in the backend, `appControl.ts` in the
      frontend).
    - Reads the local store and appends to `~/.cecelia-effectiveness/review.jsonl`. Event types are
      `spot_check_label`, `finding_status` and `proposal_decision`. Corrections are new events;
      nothing is edited.
    - Never touches the repo.
    - The supervisor applies new events at the start of each run.

## Run record (fields)

- **Run metadata:** date, pinned SHA, prompt-set version (list + hash), CLI/model version, cost,
  retries, trace dir.
- **Results:** score raw → after scorer fixes; per-prompt result + class; candidates (unscored).
- **Findings:** `id, slug, class, status, recurring|watch, evidence (trace path + excerpt),
  diagnosis, proposed fix`.
- **Proposals:** prompt add/change/retire with source slugs; setup changes with a
  `Hypothesis: fixes <id>; expect <prompt> to pass`.
- **Delta since last run:** findings opened/resolved, score change with any version change noted,
  whether each earlier hypothesis held.
- **Tracking:** setup-size metrics, plus spot-check and loop-review sections when due.
- **Next actions:** ordered, each self-contained (files, the exact change, the verify command).
  The record's header says how to use it: point a session at this file, do the open Next actions,
  update each finding's Status. No separate README.

## Seed record — 2026-09-30

Source: `~/Downloads/TMP/eval-2026-10-01-findings.md` + #1314 (merged). Confirm each finding against
its trace before writing it.

| Id | Slug | Class | Finding |
|---|---|---|---|
| F1 | `frontend-copy-canonical` | genuine | `CLAUDE_TERMINAL` (`lib/claudeOverview.ts`) is orphaned; `KiwiCockpit.vue` renders its own copy |
| F2 | `frontend-copy-canonical` | scorer_bug (unfixed) | anti-pattern matched inside an HTML comment; the fix affects every prompt |
| F3 | `kill-process-tree` | genuine | `_kill_tree` has no grace-then-force mode |
| F4 | `discovery-first` | genuine | `slice_utils.py` docstring promises helpers that are gone; the probe premise is false |
| F5 | `dir-size` | genuine | CLAUDE.md + `docs/DEV.md` name `_dir_bytes`; the inventory names `_path_bytes` |
| F6 | `cite-algorithm` | decision | does a docstring count as the citation comment? |
| F7 | — | decision | eval agents run with `--dangerously-skip-permissions` (`run_prompt.py`); one ran `kill -9`. No tool restriction has existed: #1272's sandbox was plugin-eval (closed — no CLAUDE.md), #1274's was log isolation (trimmed in `c6463720`). Probe result under *Open questions* |

Record and classify these only; the fixes are separate work.

## Phases

Each phase is its own PR.

1. **Record store:** schema + test, md renderer, local store, PR mirror, `docs/ai-assist/eval-runs/`
   added to `_SKIP_DIRS` in `test_doc_pointer_convention.py`, and the seed record. Checkpoint: a
   replay over the 09-30 traces renders a record.
2. **Supervisor:** `--ref` in `run_prompt.py`, persistent worktree, `supervise.py` with triage and
   retries, `cron_pass.sh` calling it by default (with `--no-supervise`), lock and `finally`
   cleanup. Checkpoint: a forced crash still writes a failure record.
3. **Close-out:** both rollups, delta section, single-open-PR rule, staleness guard. Checkpoint:
   the guard fires on an old store and is silent on a fresh one.
4. **Curation:** candidates and proposals per Decision 8. Checkpoint: running it on the current log
   gives proposals with evidence, or none.
5. **Owner loop:** ingest `review.jsonl`, spot-check sampling, loop review, setup-size metrics.
6. **Review UI backend:** Julia endpoints to list/get runs and list/append events, behind
   `_is_dev()`.
7. **Review UI frontend:**
   - Latest-run overview; a keyboard-driven review queue (spot check, findings, proposals) with
     progress, undo-as-event and an empty state.
   - Copy per `docs/ui/COPY.md`.
   - Tests: flag off → no route; a bad or mismatched schema → clear error.
   - No charts.

Phase 2 of the UI (trends, proposal history, cross-run findings browser) is out of scope; keep the
schema compatible with it.

## Open questions

- F7 — eval-agent containment (**resolved**, below). Probed 2026-10-01 (`claude` 2.1.286, ~$0.82 over three probes).
  Tool restriction alone can't contain it: agents do all discovery through Bash, and Bash can run
  anything. The sandbox needed an AppArmor profile for `/usr/bin/bwrap` first: Ubuntu 24.04 sets
  `apparmor_restrict_unprivileged_userns=1`. The owner added `/etc/apparmor.d/bwrap` the same day.
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
