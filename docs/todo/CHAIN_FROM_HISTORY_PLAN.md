# Chain from history — and the manual step it needs

**Status:** built except the optional B-P4 (2026-10-30) · branch `docs/chain-from-history-archive`. Part A built (A-P1–A-P3);
B-P1 + B-P2 + B-P3 built (B-P4, optional Claude on top, not started). Decisions refined in the build, from the two real histories:
- B4/B5: identity and wiring read the spec, not names alone. A param links to an output only when it
  reads that output's namespace (`namespace` / `field` / a model `optionsFrom`): region clusters and
  spatial graphs were both called `immune` in 4kS67f. A composite takes its steps' params, the first
  declaration of a key winning unless a later step writes it (measureLabels re-reads `outputValueName`).
- B8 needs no spec flag: a task with an `imagePicker` param (per-image geometry) is never replayed. A run
  logged before params were recorded is reported, not replayed with `{}`.
- B6: only population pickers that take gated cells (`accepts` has `live`, or none) imply a gate; a path
  segment naming a run (`clusters.immune`) is a produced population, not a gate.
Brief: [`docs/archive/CHAIN_FROM_HISTORY_PROMPT.md`](../archive/CHAIN_FROM_HISTORY_PROMPT.md).

## Goal

"I tried things on a few images; process the other 29 the same way." Build that chain **without
Claude**: from the run history to a whiteboard template, then run it with the existing Run button.
Claude can sit on top later (names, plain-language summary, flagging odd params). Removing Claude must
not break the feature.

It needs one thing the chain runner doesn't have: a **manual step**, a node where the run waits for the
user. Gating is the first use. No automatic way to gate exists, and every real pipeline we checked
reads gated populations. So this plan has two parts:

- **Part A:** a general manual step in chain runs. Useful on its own.
- **Part B:** the history → template builder. It inserts manual steps where gates are needed.

`MICROCLAW_LESSONS_PLAN.md` lesson 6 rated "run → chain" **not needed** ("no evidence anyone rebuilt
a chain by hand"). This plan reopens it because the maintainer asked for it directly on 2026-10-30.

## Evidence: two real projects

Read through the observer on 2026-10-30, checked against the code.

| Finding | 4kS67f (B vs T) | zolIMa (MERTK) |
|---|---|---|
| History coverage | Starts 07-15. The core pipeline (chain `3P`: denoise → AF/drift → 2× seg, runs 07-03/04) and tracking predate the run log (added 07-11; params since 07-20). **0 of 3P's 4 nodes recoverable from history** | Complete since 07-21. But fXgbTl sits at exactly `RUN_LOG_CAP` = 200 rows, so its older segmentations' runs are gone |
| Wiring by names | `graphSuffix: immune` links neighbours → region clusters / neighbour stats; contacts/motifs read `B/qc/_tracked` | seg `outputValueName: Bcells` → tracking `Bcells`; train `flow.small` → seg `model: flow.small.pt` (`.pt` varies) |
| Gates | Every downstream step reads `B/qc`, `T/qc` | Tracking reads `/qc/frags`, `/qc/cells`: **gates sit mid-pipeline**, between seg and track |
| Noise | 11 cellposeMeasure variants on one crop; branching ×7; region k 3→4→3; `customExamples.qcProbe` ×140 | drift ×10 on x4E5HU (input and channel flipping); denoise ×4 failed |
| Latest-done-wins | — | Exact for overwriting outputs: what is on disk *is* the last done run |
| "Same task + same output = rerun" | — | **Wrong**: tracking on `Bcells` ran for `/qc/frags` *and* `/qc/cells`. Two outputs, not a rerun |
| Per-image params | Tuning done on 3 crops of M4c; crop boxes are per-image coordinates | Training is a set-level artefact; replaying it retrains |
| Chains as a source | `3P` is the real pipeline | 10 templates, 1 ever run; not the pipeline |

## Part A — the manual step

### Decisions

- **A1. Manual is a node *kind*, not a scope.** Scope keeps its meaning and decides the grain of
  the wait:
  - `image`: each image parks at the step and continues on its own.
  - `set` (picnic): the existing barrier, then one park and one Continue for all images.
  - `incremental`: rejected by `validate_chain_template` (a manual step must gate progress).
- **A2. A manual step is a task spec with a `"manual"` block.** It gets the palette, param widgets,
  validation, run log and REPL for free. Its `_run_task` is the **prepare** action (may be a no-op).
  The block declares:
  - `prompt`: one short line shown while waiting.
  - `open`: where the user goes to do the work (route + panel).
  - `check` (optional): a Julia function run on Continue. Failing it refuses Continue and says why.

  The chain executor parks after prepare; the task itself knows nothing about chains.
- **A3. New node status `NODE_WAITING`** (`"waiting"`). Non-terminal, persisted in `run.json`. It is
  entered only after prepare succeeds; a failed prepare is `NODE_FAILED` as today.
- **A4. Live wait, durable park.** The image thread (or the set runner) blocks on a continue channel,
  one per node and image for image scope, one per node for set scope. A Julia `Task` blocked on a
  `Channel` costs no OS thread. If the process dies, resume **re-parks**: `_reset_stale_nodes!` leaves
  `NODE_WAITING` alone and prepare does **not** re-run, so the user's work is never redone over.
- **A5. Prepare never destroys user work.** It must be safe to repeat (gate prefill skips an image
  that already has gating for that label set and pop type). This matters because cancel → resume
  *does* re-run prepare.
- **A6. Continue is one API with three transports, mirroring cancel.**
  - `continue_chain_node!(run_id, node_id; image_uids = nothing)` in the package (REPL/tests work
    with no server).
  - WS `chain:continue`.
  - Runner `POST /continue-chain`, beside `/cancel-chain`.

  It runs `check` per image, marks passing ones `NODE_DONE` (with `params_hash`) and releases them.
  If the run is not live (process restarted), it marks done on disk and resumes the run: one call
  either way.
- **A7. Cancel while waiting** releases the wait → `NODE_CANCELLED`, same as any pending node.
- **A8. Unattended runs leave the step waiting.** The autonomous MCP gets no Continue in v1.
  `wait_for_chain` must report `waiting` as settled-not-finished, not time out on it.
- **A9. The step is runnable outside chains.** On a module page it runs prepare only (e.g. "copy
  gates from exemplar"). This comes free from A2.

### First consumer: `gating.manual_gate`

- Params: label set (`valueName`), pop type (`flow` / `track`), optional `copyFrom` (exemplar image —
  a new generic `imageSelection` param type), `requiredPops` (comma-separated paths that must exist
  before Continue). Built as `gate.manual_gate`, `hidden` (a chain step, not a Gating-page task).
- **Prepare:** copy gating from `copyFrom` onto the image unless it already has some. This reuses the
  existing copy rules (`api_gating_copy`: position gates need a pixel size, column warnings #1737).
  **The copy logic moves into the package** as `copy_gating!`, with the route calling it, because a
  chain may run in the detached runner, which never loads `api/src`.
- **Check:** every `requiredPops` path exists on that image. Non-empty is reported, not required.
- **Open:** that image's gating view for the label set.
- QC: not result-producing (the user's gates are the result). Exempt by an explicit comment, as
  `app/CLAUDE.md` asks.

### Phases

- **A-P1 Executor** (`app/src/tasks/chain/{types,execute,api,validate}.jl`, `app/src/runner/server.jl`,
  `api/src/sockets.jl`): `NODE_WAITING`, the `manual` spec block (task loader), park for both scopes,
  `continue_chain_node!` + both transports, resume re-park, validator rule. Tests in
  `app/test/suite.jl` with a no-op manual test task: image-scope continue of one image while another
  waits; set-scope single continue; failed check refuses; cancel releases; crash → resume re-parks
  without re-running prepare. `docs/SCHEDULER.md`: status table, concurrency invariant 3 ("if you add a
  new scope type…" now also covers kinds), new *Manual steps* section.
- **A-P2 Gate step:** `copy_gating!` extracted (route keeps its contract), the
  `gating/manual_gate.json` spec + handler + check, and tests: prefill skips gated images, check names
  the missing pop.
- **A-P3 UI** (`ChainModule.vue`, `ChainTaskNode.vue` / `ChainPicnicNode.vue`, `ChainLiveNode.vue`,
  `stores/ws.ts`): the manual step's own look on the whiteboard. In the Live view, a "waiting N" badge
  and a list of waiting images, each with **Open** and **Continue**, plus **Continue all**. Copy in
  `docs/ui/COPY.md` style; `docs/UI.md` updated.

## Part B — the history → template builder

### Decisions

- **B1. Mechanical. Claude optional.** A pure Julia function plus a route; the whiteboard is the only
  UI it needs.
- **B2. Two sources: the run logs and the chain run records.** Run records (`settings/chains/runs/*/
  run.json`) hold frozen templates with params, and they outlive the run log's start date and its
  200-row cap (4kS67f). Where both cover a step, the run record wins: its params are what the chain
  actually ran.
- **B3. The user picks the exemplars and ticks the outputs to keep.** The builder lists every final
  output on the exemplars (outputs nothing else read). The user ticks the ones that matter, and
  everything upstream of them comes in through name links. That one tick list drops abandoned
  experiments, the qcProbe ×140 and the dead-end clusterings. No heuristic can.
- **B4. Rerun identity = task + output name + output-shaping params.** `task_output_name` (spec
  `namespace`) gives the name. A spec may also mark params that make separate outputs under one name
  (tracking `popsToTrack`). Within one identity the latest **done** run wins; that is exact for
  overwriting outputs, and for versioned ones the active version is checked to match. Failed,
  cancelled, interrupted and running are dropped.
- **B5. Edges from names.** A → B when B's params name A's output: `valueNameSelection` and label
  params, `model` (with `.pt` normalised, as `_chain_produced_names` already does for models), and
  `graphSuffix`-style links. Set-scope runs appear once per image in the run log and are collapsed by
  `taskId`.
- **B6. Gates become manual steps.** A population path read downstream that no task produced (e.g.
  `/qc/frags` under `Bcells`) inserts a `gating.manual_gate` after the producer of that label set,
  `copyFrom` = the exemplar, `requiredPops` = the paths read downstream.
- **B7. Disagreement is a question, never a guess.** Exemplars that ran one identity with different
  params produce a conflict row (param, value per exemplar). The template is not created until each
  conflict is picked.
- **B8. Some steps never replay.** A spec flag (`"replay": false`) marks per-image geometry: crop
  boxes, ROI coordinates. Such steps are left out and listed. An exemplar that is itself a crop
  contributes its params; the crop does not come along.
- **B9. Training defaults to reuse.** A training node whose model already exists is dropped, and its
  consumers keep pointing at that model. A per-node toggle switches it back to retraining.
- **B10. The result is an ordinary template, never a run.** It is saved through the whiteboard's own
  path (`validate_chain_template`) and opened for review. Running it is the existing Run button.

### Pre-flight before the run (B-P3)

Per target image, before anything starts. Every check names the image and the missing thing:

- channel names every node uses resolve on the target (`channel_indices`, case-hint included)
- dimensions the params assume (Z / T present; timepoint params within range)
- pixel size present where a position gate will be copied
- an output the run would overwrite on the target: listed, with "keep previous version" state
  (`keep_previous_version()`) shown, not silently overwritten

### Phases

- **B-P1 Builder backend:** `app/src/tasks/chain/from_history.jl` (pure: run-log rows + run records +
  task specs → `{template, conflicts, notReplayed, gateSteps, finalOutputs}`), and
  `GET /api/chains/suggest?projectUid&imageUids[&keep]`. Fixtures are rows **copied from the real
  4kS67f and zolIMa histories**, never hand-written. Golden expectations: the frags/cells tracking pair
  stays two nodes; x4E5HU drift resolves to the 12:55 run; stack-align drops out; UMq4v3 gets a gate
  step between seg and track.
- **B-P2 Builder UI:** "From history…" in `ChainModule.vue`. Pick exemplars → tick final outputs →
  resolve conflicts → the template opens on the whiteboard.
- **B-P3 Pre-flight** at run start (above), shared by every chain run, not only built ones.
- **B-P4 (optional) Claude:** an observer read tool over `/api/chains/suggest`, and the Kiwi
  explanation. Nothing in B-P1–P3 depends on it.

Order: A-P1 → A-P2 → A-P3 → B-P1 → B-P2 → B-P3. A ships value before B starts.

## Open questions

1. **`requiredPops` by hand or derived?** Recommend: derived by the builder (B6). On a hand-built
   chain the author lists them, and an empty list means "Continue always passes".
2. **A signal when a run starts waiting?** The Live view badge is in scope. A toast or Task Manager
   row for "3 images waiting for you" is not decided.
3. **Output-shaping params (B4):** add a spec field, or derive from the spec (e.g. any `popSelection`
   on a task whose output is a label set)? Needs a pass over the task specs first.
