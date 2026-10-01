# Agent overnight run — "track everything, give me the behaviours, I'm back tomorrow"

**Status:** planning (2026-10-01) — P0 on `docs/agent-sandbox-audit`; P1–P5 open.
**Audit this builds on:** [`docs/audit/agent-sandbox-value-name.md`](../audit/agent-sandbox-value-name.md).

## Goal

Find out how far an unattended agent gets through a real Cecelia analysis **with the environment as it
is today** — no Kiwi guardrails, no new launch route, no autonomous mode. A headless `claude -p` gets
a project of small synthetic time-lapse images and one sentence: *track every cell and give me the
behaviours*. In the morning there is a scored record of what it did.

Two things are measured separately, because they fail for different reasons:

- **Navigation** — did it find the right tasks and parameters, how many turns and dollars, what errors
  it hit and recovered from, whether it damaged anything that was already there.
- **Performance** — how close its segmentation, tracks and behaviour states are to ground truth.

This is an experiment on the setup, not on the model — same frame as the CLAUDE.md eval
([`CLAUDE_MD_EVAL_PLAN.md`](CLAUDE_MD_EVAL_PLAN.md)): a failure points at a gap in docs, task
interfaces or QC, and the fix goes into the framework.

## What exists (verified 2026-10-01)

- **Headless execution:** `run_task(proj_uid, img_uid; fun_name, params)` / `run_tasks` / `run_chain`
  (`app/src/tasks/scheduler/run.jl:145,171`, `app/src/tasks/chain/api.jl:91`). An agent reaches them
  via Bash → `julia --project=app`. `docs/REPL.md` documents mutation through `run_task`. The observer
  MCP is read-only and stays that way.
- **Pipeline fun_names:** `segment.cellpose` / `segment.coastal` / `segment.measureLabels` (+ the
  `segment.cellposeMeasure` composite), `tracking.bayesian_tracking`, `tracking.track_measures`,
  `behaviour.hmm_states` / `behaviour.hmm` (`app/src/tasks/task_registry.jl`).
- **Isolated projects:** `custom.toml` with `[dirs] projects='<tmp>'` in a temp `CECELIA_DEV_DIR`,
  then `init_cecelia!()` + `create_project!` (`app/test/runtests.jl:22-28`, `scripts/kiwi_eval.jl`).
- **Eval rig to reuse:** `scripts/claude_md_eval/run_prompt.py` — bwrap sandbox settings (network off,
  `~/**` write-denied), `CECELIA_OBSERVER_NO_PAIR=1`, stream-json traces, cost/turns via
  `transcript.py`; `cron_pass.sh` + `systemd/claude-md-eval.{service,timer}` (flock, AC power, nice);
  run records via `record.py` (`schema_version`, delta). Supervisor (#1326) adds pinned worktree +
  tool-less triage.

## What does not exist

- **No time-lapse fixture with moving objects and known tracks.** `test-data/` (1.6 MB, capped at
  8 MB) holds a 3-frame 64×64 zarr and already-tracked h5ads without pixels. The closest synthetic
  pieces are tables (`app/test/suite/tracking.jl:310` random walks; `tracking_hmm.jl:148-175`
  two-state tracks flipping at t=13) and drift movies (`python/cecelia/tests/test_stack_alignment.py:88`).
- **No ground-truth scorer** for segmentation, tracks or states. Reusable: `_compute_iou_matrix`
  (`python/cecelia/utils/segmentation_utils.py:879`).
- **No sandbox enforcement** in the product (audit L1 `_active` leak, L2 no existing-value_name guard).

## Decisions (2026-10-01)

1. **Synthetic fixtures, generated, not committed.** A seeded generator writes the project at run
   time into the temp projects dir; only the generator and its tests are committed (the `test-data/`
   cap stays). 2D + t first — "simpler images" — 3D later only if 2D is solved.
2. **Ground truth is designed in, not annotated.** The generator knows every cell's position, label
   and behavioural state per frame. Behaviour = two motility regimes (e.g. persistent migration vs
   arrested), switching at known frames, so an HMM has something real to recover.
3. **A "prior work" canary in every fixture.** Each image ships with an existing `default`
   segmentation + gating file the user "made". Afterwards the scorer checks they are byte-identical
   and that `_active` is unchanged. This measures L1/L2 without fixing them first.
4. **Phase 1 tests the environment as it is.** The agent gets the repo worktree, Bash, and the normal
   CLAUDE.md / docs. No bespoke agent API, no extra hints beyond the brief. Gaps it hits are findings.
5. **Two brief tiers.** *Vague* ("track everything, give me the behaviours") and *guided* (names the
   task family and the output value_name to use). The difference separates "can't find it" from
   "can't do it".
6. **Python scores, `claude -p` only triages.** Same split as the supervisor plan Decision 4. Scores
   are deterministic and rerunnable over saved outputs.
7. **A human ceiling before any agent run.** We run the pipeline ourselves on the fixture with good
   parameters first. If we can't get a good score, the fixture is wrong, not the agent.
8. **Cost discipline.** First runs are N=1, started by hand, spend reported. Nightly only once a run's
   cost is measured. A hard per-run cap in the runner. Never overlaps the Monday CLAUDE.md eval (shared
   lock).

## Phases

### P0 — audit + latent-bug fix + this plan *(this branch)*
Audit doc; `versioned_entry_overwrite!` replaces the flattening rebuilds in `register_label_files!`
and measureLabels (audit B1), with tests.

### P1 — fixture generator + ground truth + scorer
- `scripts/agent_eval/fixture.py` (or under `python/cecelia/` if it earns reuse): seeded, writes OME-Zarr
  through `zarr_utils` only; per image ~128×128, 1–2 channels, 30–40 frames, 15–30 cells, two motility
  regimes; writes `ground_truth.json` (per-frame centroid + label + state) beside the project.
- Scorer: segmentation (per-frame matched count + mean IoU via `_compute_iou_matrix`); tracking
  (per-GT-track majority-overlap match → fraction of correct links, split/merge counts); behaviour
  (per-cell-frame state agreement, best over label permutation); canary (D3).
- **Checkpoint:** scorer gives 1.0 on GT-as-prediction and drops monotonically on a perturbed copy
  (dropped links, swapped states) — tested.

### P2 — human ceiling + end-to-end isolation check
Run segment → measure → track → track measures → HMM ourselves via REPL into a fresh value_name.
Records the ceiling score and wall-clock, and confirms (or refutes) the audit's "traced, not run"
isolation claim. **Checkpoint:** ceiling score recorded; canary intact.

### P3 — agent runner, by hand
`scripts/agent_eval/run_overnight.py`: build fixture → spawn one sandboxed `claude -p` with a brief
(reusing `run_prompt.py` settings + trace layout) → score → write a record. Navigation metrics from
the trace: turns, cost, tool errors, tasks attempted, value_names written, docs read. Dominik starts
it (auto-mode blocks assistant-side `claude -p` spawns, as in #1264). **Checkpoint:** one vague + one
guided run, N=1 each, spend shown, findings written.

### P4 — midnight timer + run record
Own timer (not Monday), shared flock with the CLAUDE.md eval, `ConditionACPower`. Record reuses
`record.py`'s shape: score per tier, canary, navigation, findings, delta vs previous night, pinned
SHA + Claude Code version.

### P5 — act on findings
Expected first candidates, in order of what P3 shows: close audit L1/L2 behind an explicit autonomous
flag (don't move `_active`; refuse existing value_names); task-interface or doc fixes where the agent
got lost; a second tier on a copy of jFWePN scored against the user's own analysis as reference
(differential testing — no ground truth, but a real-data check).

## Out of scope

Kiwi guardrails and claim checks ([`KIWI_ASSISTANT_PLAN.md`](KIWI_ASSISTANT_PLAN.md)); a product
launch route for the observer; any UI; extending `version_write!` to segmentation/tracking (the audit
shows the outer axis isolates).

## Open questions

- Cellpose vs coastal on the fixture — let the agent choose (it's part of navigation), but P2 must
  confirm at least one gets a good ceiling on CPU.
- How synthetic is too synthetic — blobs on a flat background may be trivially segmentable; add
  noise / touching cells / intensity drift once the baseline works.
- Run cost and duration are unknown until P3; the nightly cap is set from that, not guessed.
