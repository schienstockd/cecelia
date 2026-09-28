# CLAUDE.md compliance eval

_Rendered 2026-09-28T23:35:55Z from `~/.cecelia-effectiveness/events.jsonl` — auto-regenerated at the end of every `pixi run claude-md-eval` pass. Standalone regen via `pixi run claude-md-eval-rollup`. Not auto-committed._

Behavioral compliance signal for `CLAUDE.md`: a fresh `claude -p` agent is given a task under a rule, the diff + tool trace are scored deterministically. Design + methodology: [`docs/todo/CLAUDE_MD_EVAL_PLAN.md`](../todo/CLAUDE_MD_EVAL_PLAN.md).

## Latest suite

- **When:** 2026-09-28 23:35 UTC
- **CLAUDE.md blob:** `23634d3e`
- **Arm:** `with` · **Runs per prompt:** 3
- **Total spend:** $17.09

| Prompt | Compliant / Total | Errors | Cost | Rule |
|---|---:|---:|---:|---|
| `canary` | 3/3 | 0 | $0.835 | Compliance-eval canary — CLAUDE.md loaded |
| `cite-algorithm` | 0/3 | 0 | $1.149 | Cite sources for non-trivial algorithms |
| `crop-failure` | 3/3 | 0 | $1.521 | Image / OME-ZARR access — always go through zarr_utils |
| `dir-size` | 2/3 | 0 | $1.337 | Windows compatibility — `_dir_bytes` for directory size |
| `discovery-first` | 0/3 | 0 | $0.991 | Before implementing anything — mandatory discovery step |
| `h5ad-read` | 3/3 | 0 | $1.163 | H5AD / cell-data access — always go through the readers/writers |
| `h5ad-write` | 3/3 | 0 | $1.057 | H5AD / cell-data access — always go through the readers/writers |
| `kill-process-tree` | 2/3 | 0 | $2.271 | Windows compatibility — `_kill_tree` / `free_port` for process kill |
| `spawn-python` | 3/3 | 0 | $1.631 | Spawning Python — always go through `run_py` |
| `utf-8-json-write` | 3/3 | 0 | $1.031 | Windows compatibility — always pass `encoding="utf-8"` to Python text I/O |
| `zarr-read` | 3/3 | 0 | $1.612 | Image / OME-ZARR access — always go through `zarr_utils` |
| `zarr-write` | 3/3 | 0 | $2.488 | Image / OME-ZARR access — always go through `zarr_utils` |
| **TOTAL** | **28/36** | 0 | $17.09 | |

## Failing rules (latest suite)

### `cite-algorithm` — 0/3 compliant
Rule: Cite sources for non-trivial algorithms

### `dir-size` — 2/3 compliant
Rule: Windows compatibility — `_dir_bytes` for directory size

### `discovery-first` — 0/3 compliant
Rule: Before implementing anything — mandatory discovery step

### `kill-process-tree` — 2/3 compliant
Rule: Windows compatibility — `_kill_tree` / `free_port` for process kill

## Trend (recent passes)

| When (UTC) | Blob | Arm | `canary` | `cite-algorithm` | `crop-failure` | `dir-size` | `discovery-first` | `h5ad-read` | `h5ad-write` | `kill-process-tree` | `spawn-python` | `utf-8-json-write` | `zarr-read` | `zarr-write` | Total |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2026-09-28 23:35 | `23634d3e` | with | 3/3 | 0/3 | 3/3 | 2/3 | 0/3 | 3/3 | 3/3 | 2/3 | 3/3 | 3/3 | 3/3 | 3/3 | 28/36 |
| 2026-09-27 23:32 | `2f05fefc` → | with | — | — | — | — | — | — | — | — | — | 3/3 | — | — | 3/3 |
| 2026-09-27 23:30 | `2f05fefc` | with | — | — | — | — | — | — | — | — | — | 1/3 | — | — | 1/3 |
| 2026-09-27 23:23 | `2f05fefc` | with | — | — | — | — | — | — | — | — | — | 2/3 | — | — | 2/3 |
| 2026-09-27 06:31 | `2f05fefc` | with | — | 0/3 | — | — | — | — | — | 3/3 | — | 1/3 | — | — | 4/9 |
| 2026-09-27 06:21 | `2f05fefc` | with | — | 0/3 | — | 3/3 | — | 3/3 | 3/3 | 1/3 | 3/3 | 1/3 | 3/3 | 3/3 | 20/27 |

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
