# Agent overnight run — "track everything, give me the behaviours, I'm back tomorrow"

**Status:** in progress (2026-10-02) — P0 shipped (#1335, #1346); P1–P3 shipped (#1350; harness verified with a scripted stand-in, no agent run yet); P4 built, not enabled; P5 L1/L2 design proposed, needs a decision.
**Reframed (2026-10-06):** the app-tier runs (P4b) are guide runs — an agent follows one in-app guide and you review the result; see [`GUIDE_RUNS_PLAN.md`](GUIDE_RUNS_PLAN.md).
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

This is an experiment on the setup, not on the model: a failure points at a gap in docs, task
interfaces or QC, and the fix goes into the framework. It replaced the CLAUDE.md compliance eval
(retired 2026-10-04, [`../archive/CLAUDE_MD_EVAL_PLAN.md`](../archive/CLAUDE_MD_EVAL_PLAN.md)) as the
test of whether an agent can use the framework.

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
- **Agent rig:** `python/cecelia/effectiveness/agent_sandbox.py` — bwrap sandbox settings (network
  off, `~/**` write-denied), the detached checkout, stream-json cost/turns; `CECELIA_OBSERVER_NO_PAIR=1`.
  `scripts/judge/cron_pass.sh` + `systemd/weekly-judge.{service,timer}` are the cron pattern (flock,
  AC power, nice); the overnight run shares that lock.

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
   cost is measured. A hard per-run cap in the runner. Never overlaps the Tuesday 23:59 weekly judge (shared
   lock).

## Phases

### P0 — audit + latent-bug fix + this plan — **shipped**
Audit doc; a versioning-aware overwrite (now `versioned_set_field!`) replaces the flattening rebuilds in
`register_label_files!` and measureLabels (audit B1, #1335); the `filepath` side (B2, #1346).

## How to run it (built)

```bash
pixi run agent-eval-run --root /tmp/agent-night-1 --dry-run             # build only, show the prompt
pixi run agent-eval-run --root /tmp/agent-night-1 --scripted-ceiling    # harness check, $0
pixi run agent-eval-run --root /tmp/agent-night-2 --brief vague         # ONE real agent, ≤ $10
pixi run agent-eval-run --root /tmp/agent-night-3 --brief guided
pixi run agent-eval-crop --project-dir ~/cecelia-feijoa/projects/zolIMa --image fXgbTl \
    --reference-vn flowTom --out /tmp/agent-crop                         # real-data tier source
pixi run agent-eval-run --root /tmp/agent-night-4 --fixture /tmp/agent-crop
```

Each root gets `record.md` / `record.json` (scores, canary, cost, turns, tool errors, tasks run, the
agent's final message) and `trace/` (prompt, command, stream.jsonl, stderr). `--root` must be fresh
and outside `~` (the sandbox write-denies home). The agent works in a detached checkout at HEAD
(CLAUDE.md loads as usual), with `CECELIA_DEV_DIR` = the fixture's isolated dev dir, **no MCP servers**
(`--strict-mcp-config`: the observer would point it at the real app), network off, and a
`--max-budget-usd` cap.

### P1 — fixture generator + ground truth + scorer — **built**
- `scripts/agent_eval/fixture.py`: seeded 2D+t, 192×192 px at 0.8 µm/px, 40 frames at 30 s, 18 cells,
  1 channel (`cells`), two regimes (migrating 5 µm/min persistent / arrested 0.3 µm/min), ≥ 8 frames
  per regime. Writes an **OME-TIFF** (not a zarr): the fixture goes through the real import task.
  ~3 MB per image.
- `setup.py` + `setup_project.jl`: isolated dev dir (projects, bioformats2raw copied read-only from the
  dev config, `python` pinned to the analysis env), import, optional **prior work** = a real
  `segment.cellposeMeasure` into `default`, then the canary snapshot.
- `crop.py`: **real-data tier** — a window of an analysed image (default 128×128 px × 12 z × 20 frames,
  all channels) placed where the user's tracks are densest; the user's tracked rows are the
  *reference*. `fXgbTl` / `flowTom`: 31.5 MB, 220 reference cell-frames, 23 tracks; reference centroids
  are ~3× brighter than random points in nuc-GFP / mem-TOM (offsets verified).
- `score.py`: Hungarian centroid matching per frame (3D when `z_scale` is set) → detection P/R/F1,
  link recall / precision, state accuracy under the best state mapping; canary = file hashes +
  `_active` pointers. Mask IoU deliberately not used — centroids answer the question and need no
  label-store reads. Against a reference, read recall: precision counts untracked cells.
- **Checkpoint met:** 1.0 on ground truth, drops on dropped detections / split / swapped tracks /
  scrambled states (`python/cecelia/tests/test_agent_eval.py`, 16 tests).

### P2 — human ceiling + end-to-end isolation check — **done**
`ceiling.jl`: cellpose (`cpsam_v2`, diameter 10) → measure → btrack (search 15 px, lost 2) → track
measures → 2-state HMM on speed + angle, into a fresh value_name, through `run_task`.

- **Ceiling (synthetic, 2 images, seed 0):** segmentation F1 1.000, link recall / precision
  1.000 / 1.000, state accuracy 0.958 — 3.1 min wall-clock for 2 images on the laptop GPU.
  The fixture is easy for segmentation (open question below) but the behaviour layer is not trivial.
- **Isolation claim confirmed with one exception, and that exception is L1:** no pre-existing file
  changed, but every image's `label_props._active` moved from `default` to the run's value_name —
  the canary reports it.
- **Found on the way and fixed:** `add_image!` / `delete_image!` / `add_set!` / `delete_set!` /
  `move_image!` persisted with a cascading `save!` that wrote back stale siblings — importing image 1
  then adding image 2 wiped image 1's `ccid.json` (#1348; also live in the GUI via the edit tasks);
  and `TaskApplicabilityError` carried an empty message for a scale-only refusal (#1348).

### P3 — agent runner, by hand — **built, not yet run with an agent**
`run_overnight.py` (`pixi run agent-eval-run`): setup → detached checkout → one sandboxed `claude -p`
(the CLAUDE.md eval's `_SANDBOX_SETTINGS` + write access to the run root, `--strict-mcp-config`,
`--max-budget-usd`) with `briefs/vague.md` or `briefs/guided.md` → score every new label set (the one
named in the agent's `RESULT {…}` line is the headline) → canary → `record.md` / `record.json`.
Navigation: cost, turns, tool calls, tool errors, tasks run (`runlog.json`), new value_names.
`--scripted-ceiling` runs `ceiling.jl` as the "agent": the full record path verified at $0 (scores as
P2, canary flags L1). Dominik starts the real runs (auto mode blocks assistant-side `claude -p`
spawns, as in #1264). **Checkpoint (open):** one vague + one guided run, N=1 each, spend shown,
findings written.

Known unknowns for the first real run: whether the GPU is visible inside the bwrap sandbox (cellpose
falls back to CPU — slower, still fine at this size); whether `julia --project=app` precompiles in the
fresh checkout within the timeout (the depot is shared, caches are writable).

### P4 — midnight timer + run record — **built, NOT enabled**
`scripts/agent_eval/cron_night.sh` + `systemd/agent-eval-night.{service,timer}`: nightly 00:30, one
vague + one guided brief on a fresh synthetic fixture, `$CECELIA_AGENT_NIGHT_BUDGET` (default $5) each,
records copied to `~/.cecelia-effectiveness/agent-runs/<stamp>-<brief>.{json,md}`, run roots under
`/tmp/cecelia-agent-night` pruned after 7 days. Shares the CLAUDE.md eval's lock (busy = skip),
`ConditionACPower`, not `Persistent`. Wiring verified with `CECELIA_AGENT_NIGHT_ARGS=--scripted-ceiling`
and the lock-held skip.

**Enable only after one manual vague + guided run has measured the cost (Decision 8):**

```bash
cp scripts/agent_eval/systemd/agent-eval-night.{service,timer} ~/.config/systemd/user/
systemctl --user daemon-reload && systemctl --user enable --now agent-eval-night.timer
```

Open: delta vs the previous night in the record (the CLAUDE.md eval's `record.py` delta is the model);
a real-crop brief in the nightly set once the synthetic one is understood.

### P4b — app tier: the agent drives the RUNNING app — **built, supervised dress rehearsal 2026-10-03**
The synthetic tier hands the agent a repo checkout; a user hands it the APP. `run_app.py` gives the
agent only what any user's install offers: `claude -p --tools ""` (no shell, files or python) with the
observer + the opt-in `cecelia-autonomous` server (`docs/inventory/MCP.md`), locked to a disposable
raw-only copy of the run images in one set (`app_project.py`: `default` store only, fresh uids).
Brief: *"Hey. can you track the cells in these images and analyse their behaviour?"* plus the
one-line open-project context the app would give. Records in `/tmp/cecelia-agent-app/<stamp>/`
(`trace.jsonl` live — read with `trace_view.py`, `record.json`: cost, tool calls/errors, reads of the
source project, per image the copy's label sets / gates / chains next to the source's, cohort QC on
both sides, and a canary over the whole source project). The reviewable record is a blackboard entry
in the SOURCE project, one section per decision (`run_record.py`,
[`AGENT_RUN_REVIEW_PLAN.md`](AGENT_RUN_REVIEW_PLAN.md) P1). `cron_app.sh` = the crontab
entry (checks out origin/main first, skips if the app is down or a run holds the lock).

**No breadcrumbs (Dominik, 2026-10-03).** The run measures autonomous reasoning in this domain, so
the agent gets exactly what any user's install gives it — tools, their API docs, the app's own
error messages and guidance — and NOTHING written for this task: no analysis hints in tool
docstrings or server instructions ("gate in the valley", "check counts after"), no plan rules that
pre-decide a step (a channel-name AF rule was built and withdrawn). A fix the run motivates must help
any user (a clearer error, a missing capability), never steer this agent. The one operating contract
is that pipelines run as whiteboard chains, so the run is recorded.

Smoke findings, fixed in the same change:
- the guard matched the spec's internal `task` id, not `fun_name` (refused `segment.cellpose`);
- `get_module_params("segment")` (~70 KB) is unreadable without file tools → `fun_name=` filter, and
  it now shows a task's fixed output version (`writes: driftCorrected`) so a chain can be wired;
- the gating routes silently swap an unknown value_name for the active one → the autonomous client
  refuses an unmeasured set by name (a gate would otherwise land on another segmentation);
- the observer's "you cannot run a chain" made the agent skip the whiteboard → guidance carves out
  the autonomous session; its instructions now say chain-first;
- the agent never cleaned the image (raw → segment) → `recommend_correction_plan` tool (the
  Cleanup module's metadata-only recommendation). A channel-name AF rule was tried and withdrawn:
  WHETHER and HOW to correct autofluorescence is the agent's call — that is what the run measures;
- the canary failed on the user opening the source project (project.json `lastOpenedAt`, runlog
  rewrite) → app bookkeeping reported separately, analysis files still fail it.

### P5 — act on findings
Expected first candidates, in order of what P3 shows: close audit L1/L2 behind an explicit autonomous
flag; task-interface or doc fixes where the agent got lost; more real-data crops (jFWePN) on the
`crop.py` tier.

**L1/L2 design — PROPOSAL, needs Dominik's call (not built).** L1 is now confirmed by a run (P2).
L2 is harder than "refuse an existing output name": tracking, track measures, HMM, motif and clustering
write *in place* into the value_name (or the pops' value_name) they are given, so the rule has to say
which value_names an unattended run may touch at all.

- *Recommended:* an **autonomous prefix**. The runner sets `CECELIA_AUTONOMOUS_PREFIX=<prefix>` (never
  the GUI). While set: (L1) writers do not move `_active` — `versioned_set_field!`'s `set_active`
  default, plus the explicit flips in measureLabels / composite / `versioned_filepath_write!`; (L2)
  `run_task`'s pre-flight refuses a task whose write target does not start with the prefix. The write
  target needs a per-task declaration in the task JSON (`"writes": "output" | "input" | "pops"`), with a
  ratchet test that every result-producing task declares one — the same pattern as `requires`.
  "Accept the run" = a new `promote_value_name!(img, field, vn)` that flips `_active`; it is also the
  missing rollback lever. Stateless, survives a crashed run, readable in a file listing.
- *Alternative:* a session ledger (value_names created since the run started are writable). No naming
  rule, but needs state that a crash can leave stale, and an in-place write into a pre-existing
  value_name is only caught if the ledger is right.
- Touchpoints (prefix): 3 `_active` sites + `versioned_set_field!`, one pre-flight in `run_task`, a
  `writes` field on every result-producing task JSON + its ratchet, `promote_value_name!` + test.

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
- The real-data tier has no behaviour ground truth; once the agent writes states there, compare them
  to the user's own HMM / motif columns rather than score them.
