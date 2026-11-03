# Parameter sweep — run one task over a range of one parameter, compare the runs

**Status:** P0 + P1 built (2026-11-03) on `param-sweep` — the Sweep toggle in the task form is
unverified live. P2–P4 open. Opted in: HMM states, the three clusterings, ridges, branching, neighbour
graphs, ROI discovery, flow training. Task audit below.

## Why

The XCR1 rebuttal asked how the number of HMM states was chosen. Answering it meant running the same
fit at K = 2…5 and comparing them. `behaviour.hmm_states` now records each run's fit and plots the
runs as an elbow (`HMM_FIT_QUALITY_PLAN.md`, PR #1911), but launching the runs is still N trips through
the task form, renaming the output each time. The same shape recurs: clustering resolution, denoise
strength, smoothing window — "run this with a few values of one knob and look at them side by side".

Two halves, and only one of them is generic:

1. **Launching** — N runs of one task, one param varied, each writing its own named output. Generic
   for every task whose output is named by a param and coexists with its siblings.
2. **Comparing** — a number per run that can be lined up across the sweep. Task-specific: HMM has
   log-likelihood; clustering records no quality score; for pixel tasks the honest comparison is
   looking at the images. A sweep without a comparison is N outputs and a guess.

## Decisions

1. **The launcher expands in the frontend into ordinary runs.** One `task:run` per value, exactly what
   pressing Run N times with edited params would send (`TaskRunner.vue` `run()`). No chain: chains run
   by SAVED template name (`handle_chain_run`, `api/src/sockets.jl`), so a sweep would have to persist
   a throwaway template; and the scheduler's pools already queue N runs. Each run is a normal run —
   history, run log, delete, QC, re-run all work unchanged.
2. **Pure expansion helper** `expandSweep` in `frontend/src/utils/paramSweep.ts` (unit-tested), called
   by `run()`. Values must be valid AS TYPED — outside `[min, max]` or a fraction for an `int` is
   refused, not clamped: a clamped value would run under a name that says something else.
3. **Output name = template with `{v}`**, default `"{name}_{v}"` from the form's current output name
   (`k3`-style naming for HMM is the user's choice of template). Names that already exist on the
   selected images — read from the output param's `namespace` list, the same one its name field
   suggests from — are refused before dispatch. An output with no such list (HMM's plain `colName`)
   cannot be checked from the payload; the form says the sweep replaces runs of the same name, which
   is what a single Run does there too.
4. **Opt-in per task via the spec**: `"sweep": { "output": "<param key>", "params": ["<key>", …] }`.
   `output` names the param that keeps runs apart; `params` lists the knobs offered for sweeping. A
   task without the block shows no sweep control. Not inferred from param types: whether two runs
   coexist is a fact about the `.jl` (audit below), not about the JSON.
5. **One varied param.** Grids (2 params) multiply cost and have no comparison view; add when a real
   question needs one.
6. **A sweep carries a `sweep` tag in its run params** (`{id, param, values}`), so history can group the
   runs and a comparison readout can find its siblings. Comparison readouts may still group by their
   own key (the HMM elbow groups by everything-but-K and needs no tag).
7. **Comparison readouts stay per task**, built only when a task has a question behind it. HMM elbow
   exists. Next candidate: clustering (see P3).
8. **Cost shown before launch**: the run names and "Run 4× on 8 images" on the button; at most 20
   runs. GPU tasks (flow training) say "GPU: runs one after another" instead of a confirm dialog — the
   pool (limit 1) serialises them; the warning is the cost, the user is already pressing Run.
9. **Opt in only tasks whose runs truly coexist** — the audit below, not the param types. Tasks whose
   output target is fixed (cleanup, tracking, motifs, contacts, aggregates) stay out until their output
   is nameable; that is a per-task change, listed as a blocker, not something the launcher works around.

## Phases

### P0 — blockers on the first customers
- `behaviour.hmm_states` banks QC under the segmentation vn (`write_qc(img, "behaviour.hmm_states",
  string(vn), …)`, `hmm_states.jl` ~135), so the K=3 run's QC replaces K=2's. Key it by vn + colName.
- HMM comparison needs the fit record from `HMM_FIT_QUALITY_PLAN.md` (PR #1911) merged first.

### P1 — launcher
- `sweep` block in the spec schema + ratchet test (every `sweep.output`/`sweep.params` key exists and is
  int/float/select; `output` is a name param).
- `expandSweep` + tests (clamping, dedupe, name template, collision refusal).
- `TaskRunner.vue`: a sweep toggle on a sweepable param → values field (list `2,3,4,5` or range
  `2:5`, step for floats), name template field, the cost line. `run()` loops `expandSweep`.
- `_remember_fun_params` is written per dispatch, so the remembered "last used" becomes the LAST sweep
  value; the form keeps its draft (unchanged behaviour). Acceptable; noted in the tip.
- Enable for the tasks the audit marks SWEEP.

### P1 — what shipped differently
- No `sweep` tag in run params yet (decision 6) — nothing reads it until P2.
- Segmentation (cellpose/coastal) is NOT opted in: its knobs (diameter, thresholds, seed size) live in
  the repeatable per-model `group`, which a one-param sweep cannot address. P4.
- A composite serves its first step's `sweep` block (`api/src/routes.jl`), so `behaviour.hmm` sweeps.

### P2 — history groups a sweep
- Task list / history shows a sweep as one collapsible row (`sweep.id`), with per-run status.

### P3 — clustering comparison
- Clustering already banks `nClusters` and `largestClusterFrac` per `{vn}.{suffix}` in QC — enough
  for a first curve (clusters vs resolution). Add a quality score (silhouette on the clustering
  embedding) to the run's `{props}.clustfeatures.json` entry only if that curve doesn't answer the
  question.
- Same readout shape as the HMM elbow: score vs resolution, one curve per family (same pops, measures,
  normalisation). Generalise `hmm_fit_curve.jl` to a `run_curve` readout when this second customer
  lands (two users — don't abstract before).

### P4 — group-entry params (segmentation)
- Sweep `models[i].cellDiameter` etc.: the sweep names one group entry and one of its keys. Needs a
  trigger (someone tuning cellpose diameter by hand), and GPU cost per run.

## Task audit (2026-11-03, read-only, against main)

What a sweep needs per task: an output named by a PARAM (so `{name}_{v}` can keep runs apart), runs that
really coexist on disk (columns, QC, sidecars), and a number to compare. Two findings corrected the
starting assumptions:
- **Segmentation sweeps.** `segment.cellpose/coastal/ridges/branching` write under the
  `outputValueName` PARAM (`valueName` is the input), and `assert_labels_free` (`segmentation.jl:194`)
  refuses a taken name unless Overwrite is ticked — distinct names coexist, `nCells` is recorded.
- **Cleanup doesn't.** `cleanupImages.*`, `editImages.flip/dtype` have a FIXED top-level spec
  `outputValueName` (`"smoothed"`, …) read by `_spec_output_value_name` (`task/spec.jl:440`); their
  `valueName` is the input. N runs write one target (overwrite, or unnameable vN with keep-previous on).

| Task | Output name | Coexist? | Scope / cost | Sweep params | Compare signal | Verdict |
|---|---|---|---|---|---|---|
| behaviour.hmm_states | `colName` | columns yes; QC keyed by vn → clobbered | set / cpu | numStates | fit record (#1911): logLik, BIC | **SWEEP** after P0 |
| behaviour.hmm_transitions | `colName` | yes | set / cpu | — | counts | no — follows each states run |
| clustPops / clustTracks / clustRegions .cluster | `valueNameSuffix` | yes (`clusters.{suffix}`, QC per `{vn}.{suffix}`) | set / cpu | resolution (regions: numClusters) | nClusters, largestClusterFrac (`qc.jl`); no silhouette | **SWEEP** |
| segment.cellpose / coastal | `outputValueName` param | yes (guarded) | image / **GPU** | cellDiameter, flow/cellprob threshold; probThreshold, seedSize | nCells, byType | **SWEEP** (confirm) |
| segment.ridges / branching | `outputValueName` param | yes | image / cpu | sigmaMaxPx, threshold; dilation sizes | ridge metrics; nBranches, anisotropy | **SWEEP** |
| spatialAnalysis.cellNeighbours | `graphSuffix` | yes | image / cpu | neighbourRadius, nNeighbours | nEdges, meanDegree | **SWEEP** |
| spatialAnalysis.roiDiscovery | `roisSuffix` | yes | image / cpu | minObjects, maxRois | nObjects, nRois | **SWEEP** |
| opticalFlow.train / trainSupportDenoise | `modelName` | yes (guarded) | set / **GPU, hours** | epochs, loss weights, learningRate | finalLoss, lossDrop | **SWEEP** (confirm) |
| editImages.bin | new image per run | yes | image / io | factorX/Y | none (visual) | SWEEP, low value |
| cleanupImages.* (6) | fixed spec key | no | image / cpu (denoise gpu) | smooth sigma/frames, drift lag/sigma, … | QC keyed by fixed name | after fix: name param + QC per name |
| tracking.bayesian_tracking (+measures) | none — writes into the input's h5ad | no | image / cpu | maxSearchRadius, maxLost | nTracks, keyed by input vn | after fix: track-set name (vn-versioning gap) |
| behaviour.motif_discovery | none — unsuffixed columns | no | set / cpu | windowSize, numClasses | nClasses, keyed by vn | after fix: suffix columns + QC per run |
| spatialAnalysis.cellContacts / contactsMeshes / detectAggregates / aggregatesMeshes | none — fixed columns | no | image / cpu | maxContactDist; clustDiameter, minCells | nContacts, nAggregates | after fix: key output by run |
| neighbourStats, measureLabels, importLabels, other edit/crop/project/register, flip, dtype, export, import, remove, gate, correct / carry-over / staleness steps | — | — | — | none sensible, or one-off / destructive | — | **no** |

Chain machinery (relevant if a sweep is ever expressed as a chain, e.g. a saved recipe): N nodes of the
same fn are allowed (only ids must be unique, `chain/validate.jl`); sibling nodes fail independently
(`execute.jl` ~259); run order among siblings is arbitrary (`_topo_sort` seeds from a Dict); preflight
does not catch name collisions between siblings, only with outputs already on the image. Params out of
range are clamped silently at run time — the launcher clamps up front so the run name matches the run.
Run logs and `analysis_runs.jl` delete work per output name, so N sibling runs are fine there.
