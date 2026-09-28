# CLAUDE.md compliance eval

_Rendered 2026-09-28T01:12:33Z from `~/.cecelia-effectiveness/events.jsonl` — auto-regenerated at the end of every `pixi run claude-md-eval` pass. Standalone regen via `pixi run claude-md-eval-rollup`. Not auto-committed._

Behavioral compliance signal for `CLAUDE.md`: a fresh `claude -p` agent is given a task under a rule, the diff + tool trace are scored deterministically. Design + methodology: [`docs/todo/CLAUDE_MD_EVAL_PLAN.md`](../todo/CLAUDE_MD_EVAL_PLAN.md).

## Latest suite

- **When:** 2026-09-27 23:32 UTC
- **CLAUDE.md blob:** `2f05fefc`
- **Arm:** `with` · **Runs per prompt:** 3

| Prompt | Compliant / Total | Errors | Cost | Rule |
|---|---:|---:|---:|---|
| `utf-8-json-write` | 3/3 | 0 | — | Windows compatibility — always pass `encoding="utf-8"` to Python text I/O |
| **TOTAL** | **3/3** | 0 | — | |

## Trend (recent passes)

| When (UTC) | Blob | Arm | `cite-algorithm` | `dir-size` | `h5ad-read` | `h5ad-write` | `kill-process-tree` | `spawn-python` | `utf-8-json-write` | `zarr-read` | `zarr-write` | Total |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2026-09-27 23:32 | `2f05fefc` | with | — | — | — | — | — | — | 3/3 | — | — | 3/3 |
| 2026-09-27 23:30 | `2f05fefc` | with | — | — | — | — | — | — | 1/3 | — | — | 1/3 |
| 2026-09-27 23:23 | `2f05fefc` | with | — | — | — | — | — | — | 2/3 | — | — | 2/3 |
| 2026-09-27 06:31 | `2f05fefc` | with | 0/3 | — | — | — | 3/3 | — | 1/3 | — | — | 4/9 |
| 2026-09-27 06:21 | `2f05fefc` | with | 0/3 | 3/3 | 3/3 | 3/3 | 1/3 | 3/3 | 1/3 | 3/3 | 3/3 | 20/27 |

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
