# Phase: Claude QC Integration — Scientific Peer Reviewer

Design phase for Opus. Read `OBSERVER.md` and `LAB-LOG.md` alongside this. The three documents form one system. This document defines what Claude does with the data the MCP server provides.

---

## The distinction that matters

**Convenience** (not the goal): Claude tells you what's in your project, summarises what ran, produces plots. The whiteboard, task manager, and analysis canvas already do this.

**Value** (the goal): Claude functions as the person who used to sit at the image analysis computer and notice things. "You've tried this 10 times with the same outcome. Maybe try X." "From my reasoning I wouldn't have done it that way — but the outcome looks great. Why did you do that?" The answer to that second question is exactly what gets lost in every lab and exactly what the lab log captures.

---

## Three operating modes

Stored in project config, not the lab log. User switches at any time.

**Silent**: Claude observes, writes to lab log, never interrupts. For routine runs the user is confident in. Claude is building context for future sessions.

**Advisory**: Claude surfaces observations in the chat window when the pattern is worth naming. Never pauses the pipeline. "Image 7 gated to 23 cells — 8x below cohort mean. Continuing." User acts or ignores.

**Collaborative**: Claude pauses at configurable checkpoints and requires acknowledgement. "3 images flagged at segmentation. Review before tracking?" For overnight runs — user sets checkpoints before leaving, reviews flags in the morning.

---

## What Claude flags

Flags are statistical, not interpretive. Claude says "this gate produced 23 cells, 8x below the cohort mean of 187." Not "this gate is wrong." Interpretation is the user's job.

**Per-stage QC metrics:**

*Segmentation*:
- Cell count per image vs. cohort mean
- Mean Cellpose confidence (if available)
- Fraction of Z-slices with low cell count (Z-drift indicator)
- Label size distribution — flag bimodal when unimodal expected

*Gating*:
- Cells per population as fraction of parent — flag <5% or >95%
- Absolute count <50 cells
- Count >3 SD from cohort mean for same population

*Tracking* (`tracking.bayesian_tracking`, `tracking.track_measures`): implemented — banked per image via `write_qc`.
- Track length distribution — flag bimodal when unimodal expected *(not yet)*
- Fraction of cells tracked vs. segmented — flag <30% *(not yet)*
- `bayesian_tracking` cohort: `nTracks`, `meanTrackLength`, `nTrackedCells`
- `track_measures`: motion dimensionality uncertain (auto + low confidence) → warn (`track_measures_qc_findings`); cohort `nTracks`, `meanSpeed`, `meanDisplacement`

*HMM/behaviour* (`behaviour.hmm_states` / `behaviour.hmm_transitions`, set-scope): implemented — banked per image via `write_qc` in the state/transition write loops (`category_dist_metrics` + `hmm_states_qc_findings` / `hmm_transitions_qc_findings`, `qc.jl`).
- No cells decoded into a state — tracks too short / measurements incomplete (warn)
- All of an image's cells sat in one state — no state switching / too many model states (warn)
- One state holds ≥95% of an image's cells — dominant state (info)
- No transitions found (warn); cohort: per-image `nDecoded` / `nStates` / `dominantStateFrac`, `nTransitions` / `nDistinctTransitions` (`COHORT_METRICS`)

*Clustering* (`clustPops.cluster` / `clustTracks.cluster`, set-scope): implemented — banked per image (how that image's points landed in the set-wide clustering) via `write_cluster_qc!` (`app/src/qc.jl`).
- Run collapsed to ≤1 cluster — resolution too low / features don't separate (warn)
- One image's points all fell into a single cluster while the run found several — batch/normalisation outlier (warn)
- One cluster holds ≥90% of an image's points — under-clustering (info)
- Cohort: per-image `nClusters` / `largestClusterFrac` / point count vs. the set (`COHORT_METRICS`)

**Cohort statistics**: computed after all images complete a stage. Per-image flags that depend on cohort context are deferred until the cohort is complete — Claude needs the full set to know what's an outlier.

**Cross-experiment memory**: if the lab log contains prior experiment records, Claude cross-references. "This pattern (low cell count + bimodal track lengths) appeared in experiment X on 2026-03-12 — root cause was Z-drift. Check Z-stack integrity."

**Repeat attempt pattern**: if the same function has been run >3 times on the same image — regardless of outcome — Claude surfaces it. This is the most reliable signal that something is wrong that the user hasn't named yet.

---

## The hold state

A task can currently run, fail, or complete. There is no "completed but flagged, awaiting decision." This is the most important missing piece.

Design a `flagged` task state:
- Task completed successfully — output is valid
- QC check produced a flag above threshold
- In Collaborative mode: downstream stages for this image pause pending acknowledgement
- In Advisory mode: downstream stages proceed, flag surfaced but not blocking
- Hold is per-image, not per-chain — image 7 held does not block images 1-6

User actions on a flagged image:
- Acknowledge and proceed
- Acknowledge and exclude (logged to lab log)
- Acknowledge and re-run with adjusted params

---

## Parameter adjustment proposals

When an image is flagged, Claude proposes a specific concrete alternative — not a general suggestion:

"Standard threshold 0.3 → 23 cells. Proposed: threshold 0.2 → estimated ~180 cells (based on cohort distribution). Run alternative?"

If accepted: branch run with adjusted params, results presented side by side. User accepts one branch, other discarded. Accepted params and rationale logged.

Parameter search space is bounded — only within the range defined in the module JSON spec.

---

## The why question

When a user does something unexpected — parameter deviation from prior runs, an unusual image note, a decision that contradicts lab log entries — Claude asks why in the chat window. Not to judge. To capture methodology.

"You set the HMM to 4 states here but the lab log notes that 3 states worked well for this cohort. What are you trying to capture with the additional state?"

The user's answer goes in the lab log as a `[User]` entry. This is how domain knowledge stops getting lost.

---

## Morning summary format

For Collaborative/overnight mode. Objective state, no conclusions:

```
COMPLETED CLEANLY (n images)
  → ready for next stage

COMPLETED WITH FLAGS (n images)
  Image 7: gate produced 23 cells (cohort mean 187) — awaiting acknowledgement
  Image 12: track fraction 18% (cohort mean 67%) — awaiting acknowledgement

PAUSED — REQUIRES DECISION (n images)
  Image 3: segmentation attempt 8, consistent low confidence — recommend parameter review

FAILED (n images)
  Image 15: btrack OOM error — see task log
```

No biological conclusions. No "there is a significant difference." The biologist reads the summary and draws the conclusion.

---

## What Claude must NOT do

- Draw biological conclusions
- Recommend exclusion without quantitative justification
- Adjust parameters without user approval in any mode
- Write speculative entries to the lab log
- Flag things inferable from a single image without cohort context
- Surface observations in Silent mode

## Honest reservations

**The why question will misfire early.** The why-question trigger requires Claude to have a model of what's expected — built from the lab log and prior runs. Early in a project, before the lab log is rich, Claude's sense of "unexpected" is poorly calibrated. It may ask obvious questions or miss genuinely unusual decisions. Document this for users: the observer becomes more useful over time, not immediately.

**"Cohort complete" needs a definition.** Per-image flags that depend on cohort statistics are deferred until the cohort is complete — but images get added, excluded, and re-processed. Define what "cohort complete" means: all images that have passed a given stage, or an explicit user declaration. Ambiguity here means cohort flags either fire too early (incomplete cohort) or never fire (waiting for a declaration that never comes). Recommend: cohort stats computed automatically once all currently-included images reach a stage, recomputed when images are added or excluded.

**Overnight calibration.** Before running unsupervised overnight, the user should run a supervised session on a subset of images to calibrate flag thresholds. If too many images hit the hold state overnight, the morning summary becomes a wall of decisions rather than a result — worse than no overnight run. Recommend a "dry run" mode: run the pipeline with all QC checks active but hold states advisory only, so the user can tune thresholds before committing to an unattended run.

---

## Lab log entries from QC

Claude appends when:
- A flag fires and user acknowledges — what was flagged, what action taken
- A parameter adjustment is accepted — original, adjusted, outcome
- An image is excluded — which image, metric, who decided
- A cross-experiment pattern recognised — what matched, prior root cause
- A why question is answered — the user's stated rationale

Claude does NOT append for routine completions with no flags.

---

## Implementation plan

> Grounded against the codebase as of 2026-07. File:line anchors point at what each step builds on.
> **Cecelia already has a QC framework** — `app/src/qc.jl` + `docs/todo/QC_PLAN.md` (Phase 1 landed) —
> that writes advisory per-image *findings* sidecars. This plan **extends** it; it does not add a
> second QC mechanism. Every metric below is written through `write_qc`, and every read through
> `read_qc`/`read_all_qc`. See the ⚠️ locked-decision conflict in step 1 — read it before building.

### 1. The `flagged` task state

- Per-image chain state today is `ImageNodeState.status::Symbol` ∈ `:pending|:running|:done|:failed|:cancelled|:skipped` (`app/src/tasks/chain.jl:73-80`), held in `ChainRun.image_states` and persisted to `<project>/settings/chains/runs/<run_id>/run.json` by `_save_run!` (`chain.jl:241-266`). Every transition passes through one choke point, `_update_node_state!` (`chain.jl:268-328`), which saves under `run._lock` and fires the event bus outside it.
- **Add `:flagged` as a new status symbol.** It round-trips `_save_run!`/`load_chain_run` for free (status stored as `Symbol(string(...))`, `chain.jl:246-250`, `:900-906`).
- **Set it where final status is computed** — after `run_task` returns in `_execute_image_chain!` (`chain.jl:518-525`): read the just-written QC (`read_qc`, step 2); if the worst finding ≥ the mode's threshold, transition to `:flagged` instead of `:done`.
- **Hold (per-image downstream pause).** Add a clause at the top of the per-node loop, beside the fault-isolation gate (`chain.jl:456-463`): if an upstream node *for this uid* is `:flagged` **and** the mode is Collaborative, park this image's thread instead of `:skip`. Because each image is its own thread walking nodes sequentially (`chain.jl:400-527`), a hold is simply "do not advance past the flagged node" — images 1-6 keep running (per-image, not per-chain, exactly as the spec requires).
- Handle `:flagged` in `_reset_stale_nodes!` (`chain.jl:842-878`): on resume treat a `:flagged` node as decision-pending (like `:done` for re-run purposes) until the user acts (acknowledge → `:done`; exclude → image dropped; re-run → `_force_restart_from!`, `chain.jl:823-837`).
- ⚠️ **Locked-decision conflict — ratify before building.** `docs/todo/QC_PLAN.md` decision #4 and `app/src/qc.jl:8` lock QC as **advisory: it never blocks or gates a task** (the `"error"` level is reserved and unused). The hold state is the deliberate opposite. Amend `QC_PLAN.md` first: scope the advisory-only guarantee to Silent/Advisory mode and make Collaborative mode the explicit, opt-in exception. **Barrier caveat:** a held image must still `_barrier_arrive!` at set-scope nodes or it deadlocks the cohort (`chain.jl:365-389`, SCHEDULER.md invariant #4) — the hold must park *after* barrier arrival, not before.

### 2. Per-stage QC metric computation

- **Reuse the existing producer pattern**: a task computes findings in its `_run_task` after the subprocess returns and calls `write_qc(img, fun_name, value_name, findings; source, output)` → sidecar `{proj}/1/{uid}/qc/{funName}/{valueName}.json` (`app/src/qc.jl:32-43`; reference impl `app/src/tasks/segment/drift_correct.jl:95-108`, findings builder `_drift_qc_findings` `:9-31`). Build each finding with `qc_finding(level, code, short, long; detail)` (`qc.jl:20-28`), `level ∈ info|warn`. Keep finding text brief-and-actionable (short = problem, long = the imperative action, numbers in `detail`).
- **Add per-stage metric functions** (Julia, in `app/src/`), computed off the canonical accessors so nothing new touches HDF5:
  - *Segmentation*: cell count = `nrow(label_props(img; value_name) |> as_df)`; label-size distribution off the `area` var column (`app/src/label_props.jl`, `select_cols(["area"])`); bimodality via `_hist_counts` (`app/src/plotting/plot_data.jl:27`).
  - *Gating*: `pop_stats(m, path)` already returns `(count, parent_count, pct_parent)` (`app/src/gating/gating_engine.jl:87`) — flag `pct_parent < 5% || > 95%`, `count < 50`.
  - *Tracking*: fraction tracked = tracked-cell count (obs `track_id > 0` / `is_tracked`, `population_manager.jl:774`) ÷ segmented rows; track-length distribution from `track_props(img).num_cells` (`app/src/tracking/track_props.jl:69-88`) or `live.track.trackLength` in `__tracks.h5ad`.
  - *HMM/behaviour*: state frequency per image from obs `live.cell.hmm.state.<name>` (`app/src/tasks/behaviour/hmm_states.jl:96`) via `_summary_agg(chart_type="frequency", by_image=true)` (`plot_data.jl:262-285`).
- Single-image flags (bimodality, absolute counts, single-state dominance) write immediately. Cohort-relative flags (vs mean/SD) are deferred to step 3 — Claude "needs the full set to know what's an outlier."

### 3. Cohort statistics computation

- **Cohort = the images in a `CciaSet`** — iterate `images(s::CciaSet)` (`app/src/model/set.jl:84`). The per-image → cohort aggregation already exists at the plotting layer: `_population_metric_frame` counts per `(value_name, pop, uID)` with genuine-zero completion (`plot_data.jl:153-190`) and `_summary_agg` reduces to one point per image (`plot_data.jl:197`). Reuse these for mean/SD across the `uID` axis rather than hand-rolling.
- **Timing — the "cohort complete" definition** (an honest reservation in this doc): compute cohort stats automatically once **all currently-included images** have reached a given stage, and **recompute** when an image is added, excluded, or re-processed. Inclusion is the `included` flag on `CciaImage` (ccid.json; surfaced in `_image_payload`, `api/src/routes.jl:1019` region) — "cohort complete for stage S" = every `included` image has a `:done` node for S in the active `ChainRun`. No user declaration required (avoids the "waits forever" failure mode).
- **Storage**: DONE — `cohort_qc!` (`app/src/qc_cohort.jl`) writes cohort-relative findings back through `write_qc` per image under the **`cohort.{fun}` namespace** (so an outlier surfaces on the image identically to a single-image flag, without clobbering the task's own `{fun}` QC doc; every included image is written, empty clears a stale flag), PLUS the per-set summary sidecar `{proj}/1/{set_uid}/qc/cohort/{fun}/{vn}.json` for the morning summary and MCP `get_cohort_qc`.

### 4. Operating mode storage and enforcement

- **Store the mode in per-project config, not the lab log** (the doc's explicit requirement). The established home for per-project non-object settings is `{proj}/settings/` (`api/src/routes.jl:7`, alongside `chains/`, `analysisBoards.json`, `animations.json`) — add `settings/ai-assist.json` = `{ "mode": "silent|advisory|collaborative", "checkpoints": [...], "thresholds": {...} }`.
- **Enforcement is layered:** (a) the `:flagged` hold in step 1 is gated on `mode == collaborative` inside `_execute_image_chain!`; (b) whether Claude *surfaces* an observation vs only logs is enforced in the observer/MCP layer (Silent → append to lab log only, never emit to the chat; Advisory/Collaborative → emit). Mode is read at flag-time and at event-emit time — a mid-run switch takes effect on the next event.

### 5. Parameter adjustment branch run

- **Deferred to Phase 2** (write-capable MCP). Design: when an image is flagged, Claude proposes a concrete alternative bounded by the module JSON spec's declared range (task JSON is the single source of truth, served via `GET /api/tasks/definitions`). On accept, run the alternative as a **new value_name variant** using the existing versioned-variable pattern (`versioned_set_field!`; correction tasks already do this — e.g. `cpCorrected`/`afCorrected`, `app/src/model/image.jl:3-93`), so the original output is preserved and the two are directly comparable. Present side by side via the existing summary/QC plot path; on user accept, `set_active!` the chosen variant, discard the other, and log the accepted params + rationale to the lab log (see `LAB-LOG.md`).
- No new storage mechanism — a branch run is just a second versioned output, which the object model already supports.

### 6. The why-question trigger

- **"Unexpected" = a measurable deviation from an established baseline**, not a heuristic guess: (a) a task param differs from the value used on prior images in the same set/cohort (compare against `runLog`/completed-node params); (b) an image `note` was added that pairs with an unusual decision (`image_note_added` event, step 8); (c) a decision contradicts a lab-log entry (e.g. HMM state count differs from a `[User]` entry recording what worked). Claude asks *why* in the chat; the answer is appended as a `[User]` lab-log entry.
- **Honest reservation (carried from this doc):** the trigger needs a baseline to deviate from, so it misfires early — before the lab log is rich and the cohort is seen. Ship with expectations set: the first few sessions build context, they don't catch errors.

### 7. Morning summary generation

- **Trigger**: fires when a Collaborative-mode run reaches a configured checkpoint or completes overnight. Content is exactly the four-bucket format specified above (COMPLETED CLEANLY / WITH FLAGS / PAUSED / FAILED) — **objective state, no biological conclusions**.
- **Sources**: per-image `:done`/`:flagged`/`:failed` from the `ChainRun`; flag detail from the QC sidecars (steps 2–3); FAILED detail from the task log (`{img._dir}/logs/{fun}.log`, `scheduler.jl:321-335`).
- **Surfaced in two places**: appended once to the lab log (`append_lab_log`, a `[Claude]` entry) and pushed to the frontend as a WS notification (a new `ai:summary` frame via `broadcast_ws`, `server.jl:52-62`) so the user sees it on arrival without opening the panel.

### 8. Event notification design

- The observer is **event-driven, not streaming** (token cost). The MCP server subscribes to the WS feed and translates frames into MCP notifications; the backend already re-broadcasts chain events via the subscribe-and-rebroadcast bridge (`api/src/server.jl:113-164`) built on `subscribe_chain_events!`/`_fire_chain_event!` (`app/src/events.jl`). Extend that surface:
  - `task_completed` / `task_failed` → already exist as `task:status` frames (`api/src/sockets.jl:15`) and chain `node:done`/`node:failed` (`server.jl:139-164`). **Reuse — do not add duplicates.**
  - `qc_flag_fired` → **NEW**. Fire a `node:flagged` (or dedicated `qc:flag`) event from `_update_node_state!` at the `:flagged` branch (step 1), payload `(run_id, project_uid, image_uid, node_id, fn, stage, metric, value, cohort_mean)`; add a bridge subscriber in `server.jl`. Must fire outside `run._lock` (SCHEDULER.md invariant #2).
  - `image_note_added` → **NEW**. `api_images_inclusion_set` (`api/src/routes.jl:891-909`) currently writes the note with **no broadcast** — add `broadcast_ws({type:"image:note", projectUid, imageUid, note})` after the mutate.
  - `lab_log_entry_added` (user-written only) → **NEW**, broadcast from the lab-log append route (`LAB-LOG.md` step 4).
- **Speak-vs-silent** is decided in the MCP/observer layer per event + mode: most events → silence; surface on the ">3 attempts same image" pattern (see the attempt-count gap in step 9), cohort outliers, and unexpected-decision triggers. The doc's token throttle (configurable max events/session) lives here.

### 9. Dependencies on framework components not yet built — and build order

- **Attempt counter is missing.** The run log (`app/src/run_log.jl`, `{proj}/1/{uid}/runlog.json`) records only *successful* runs — no failures, no retry count; `ImageNodeState` has no counter either. The ">3 attempts on the same image" signal — called the *most reliable* value signal in this doc and `OBSERVER.md` — therefore needs a **new per-(uid,node) attempt counter**, persisted on `ImageNodeState` in `run.json` (or derived by counting `node:failed`/`node:running` events in the MCP server's session memory). Build this early; it gates the core observer behaviour.
- **Build order:**
  1. `OBSERVER.md` infra (read-only MCP + read endpoints + event bridge) — no scheduler changes, unblocks everything.
  2. Per-stage QC metric functions (step 2) — pure additions on existing accessors, advisory-only, safe under the current locked decision.
  3. Attempt counter (step 9) + `qc_flag_fired`/`image_note_added` events (step 8) — enables the observer's real signals.
  4. Cohort stats (step 3) — needs the "cohort complete" definition settled.
  5. `:flagged` state + hold (step 1) — **requires the QC_PLAN.md amendment ratified first**; this is Phase 2, gated on the ROADMAP check-in.
  6. Mode storage/enforcement (step 4), morning summary (step 7) — Phase 2/3.
  7. Parameter branch runs (step 5) — Phase 2, write-capable MCP.
