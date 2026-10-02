# CLAUDE.md compliance eval

_Rendered 2026-10-01T09:15:21Z from `~/.cecelia-effectiveness/events.jsonl` — auto-regenerated at the end of every `pixi run claude-md-eval` pass. Standalone regen via `pixi run claude-md-eval-rollup`. Not auto-committed._

Behavioral compliance signal for `CLAUDE.md`: a fresh `claude -p` agent is given a task under a rule, the diff + tool trace are scored deterministically. Design + methodology: [`docs/todo/CLAUDE_MD_EVAL_PLAN.md`](../todo/CLAUDE_MD_EVAL_PLAN.md).

## Latest suite

- **When:** 2026-10-01 09:13 UTC
- **CLAUDE.md blob:** `e5cb42e4`
- **Arm:** `with` · **Runs per prompt:** 3
- **Scope:** full catalog
- **Total spend:** $10.63

| Prompt | Compliant / Total | Errors | Cost | Rule |
|---|---:|---:|---:|---|
| `canary` | 3/3 | 0 | $0.699 | Compliance-eval canary — CLAUDE.md loaded |
| `cite-algorithm` | 0/3 | 0 | $1.347 | Cite sources for non-trivial algorithms |
| `dir-size` | 3/3 | 0 | $0.854 | Windows compatibility — `_path_bytes` for size on disk |
| `discovery-first` | 3/3 | 0 | $1.118 | Before implementing anything — mandatory discovery step |
| `frontend-coalesce` | 3/3 | 0 | $1.731 | Continuous control coalescing — use one of the canonical schedulers at the sink |
| `frontend-copy-canonical` | 3/3 | 0 | $1.266 | UI copy — short, present, sourced from the canonical string when one exists |
| `frontend-inlinenote` | 3/3 | 0 | $1.097 | Rendering UI? The primitive catalog is mandatory |
| `hand-rolled-debounce` | 3/3 | 0 | $1.512 | Continuous controls — coalesce through the canonical scheduler, never a hand-rolled set… |
| `kill-process-tree` | 3/3 | 0 | $1.003 | Windows compatibility — `_kill_tree` / `free_port` for process kill |
| **TOTAL** | **24/27** | 0 | $10.63 | |

## Latest ablation (with vs without CLAUDE.md)

- **When:** 2026-09-29 00:50 UTC · **Blob:** `436f7b69` · **Runs per arm:** 3

> ⚠ **WITHOUT arm errored across ≥50% of runs — Δ suppressed.** An arm-wide error means the numeric delta is not evidence about CLAUDE.md; it is evidence the arm broke. Re-run `pixi run claude-md-eval-ablation` once the underlying cause is fixed. First known case: 2026-09-29 claude 2.1.284 post-update pairing/auth transient (errored 12/12 WITHOUT runs).

## Failing rules (latest suite)

_Each run's diff + tool log is kept under `~/.cecelia-effectiveness/traces/<ts>-<prompt>-<arm>-r<n>/` — read the trace for *why* before changing anything, and fix the dev setup, not the probe._

### `cite-algorithm` — 0/3 compliant
Rule: Cite sources for non-trivial algorithms

## Trend (recent passes)

| When (UTC) | Blob | Arm | `canary` | `cite-algorithm` | `crop-failure` | `dir-size` | `discovery-first` | `frontend-coalesce` | `frontend-copy-canonical` | `frontend-inlinenote` | `h5ad-read` | `h5ad-write` | `hand-rolled-debounce` | `kill-process-tree` | `spawn-python` | `utf-8-json-write` | `zarr-read` | `zarr-write` | Total |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2026-10-01 09:13 | `e5cb42e4` | with | 3/3 | 0/3 | — | 3/3 | 3/3 | 3/3 | 3/3 | 3/3 | — | — | 3/3 | 3/3 | — | — | — | — | 24/27 |
| 2026-09-30 14:23 | `1b549242` → | with | 3/3 | 0/3 | — | 0/3 | 0/3 | 0/3 | 0/3 | 0/3 | — | — | 0/3 | 0/3 | — | — | — | — | 3/27 |
| 2026-09-28 23:35 | `23634d3e` → | with | 3/3 | 0/3 | 3/3 | 2/3 | 0/3 | — | — | — | 3/3 | 3/3 | — | 2/3 | 3/3 | 3/3 | 3/3 | 3/3 | 28/36 |
| 2026-09-27 06:21 | `2f05fefc` → | with | — | 0/3 | — | 3/3 | — | — | — | — | 3/3 | 3/3 | — | 1/3 | 3/3 | 1/3 | 3/3 | 3/3 | 20/27 |

_`→` next to a blob SHA marks a CLAUDE.md edit between passes — the change most likely to have moved the numbers on that row._


## Reading this page

- **Compliance ≠ correctness.** A `compliant` verdict means the diff matched a
  deterministic signal for the rule; it does not mean the code works. Ratchets are the
  correctness backstop; this eval measures whether an agent *reaches for* the canonical
  path in the first place.
- **Δ discipline.** Per plan D12: N≥3 AND read at least one trace per arm before quoting
  a with/without ablation delta anywhere. Regex + tool-order graders can both pass on
  artefacts (e.g. a run where CLAUDE.md never loaded at all — see PR #1272 for the
  abandoned plugin-eval port that surfaced this failure mode).
- **Blob SHA is the CLAUDE.md the eval ran under**, not the current worktree HEAD — so
  a trend line across CLAUDE.md edits is legible even when the runner sits on a branch
  with unrelated changes.
