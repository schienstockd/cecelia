# Cecelia structure — what it is (P1 of `CECELIA_STRUCTURE_PLAN.md`)

**Status:** draft (2026-11-07). Code read at `origin/main` c7f0af6d; nothing was run. Three read-only
passes (objects and record; stages and contracts; QC, chains and AI). The load-bearing claims were
re-checked by hand against the code, and those are marked ✔. Anything not traced is marked
UNVERIFIED. To be promoted to `docs/STRUCTURE.md` after review (plan Decision 5).

## In one paragraph

Every task goes through one scheduler. The scheduler applies spec defaults, validates params and
checks applicability, then writes a per-image run log and task log. That part comes free to any new
task, custom modules included. The project → set → image → output → population hierarchy lives in
`ccid.json` and sidecars. QC is written per run and per set, read by the GUI, the briefing and MCP,
and **never blocks**. Chains replay a frozen template over many images. A claim can be checked
mechanically (S0–S2) with no model. AI reads all of this through the same readers the GUI uses, and
anything it writes lands in shared, stamped files.

The weak spots are in the **record**. The run log covers params and outcome, but not versions,
devices, the input that was read, or the outputs a re-run destroyed. Segmentation, measures and gates
are overwritten in place. Gating and statistics have no QC.

## 1. Objects and the record

| Object | Storage | Owner | Records about its origin |
|---|---|---|---|
| Project | `{proj}/project.json` | `app/src/model/project.jl:19` | uid, name, set uids |
| Set | `{proj}/1/{set}/ccid.json` | `model/project.jl`; `image.jl:698` | member uids, last-used params. **No run log of its own** |
| Image | `0/{uid}/` zarr + `1/{uid}/ccid.json` | `save!(img)` `image.jl:581` | `ori_path` (path only, no checksum), calibration + units, acquisition, partial-timepoint flag |
| Output (value name) | versioned fields `{_active, vN, _latest}` | `helpers.jl:68,113,243` | which version is active / latest. **Not which run wrote it** |
| Labels, label props, branching | `labels/{vn}.zarr`, `labelProps/{vn}.h5ad` | ✔ `app/src/segmentation.jl:111`, `measure_labels.jl:123`, `branching.jl:257` | **Never versioned. A re-run overwrites** |
| Populations + gates | `1/{uid}/gating/{vn}[__type].json` | `gating/popmanager/persistence.jl:150` | gate tree; `createdBy` (Claude-made gates); `authored_labels_version` (always `v1`, because labels never version). ✔ **Overwritten in place** (atomic write, no history, `:160`) |
| Run log | `1/{uid}/runlog.json` | `app/src/run_log.jl:159,184`; `scheduler/jobs.jl:95,151` | fun, output name, params after defaults, start/end, status, task id, pool, profile. ✔ **Capped at 200** (`:39`). ✔ **No Cecelia or engine version** (`cecelia_version()` is read only by a test). No input version, no device |
| Remembered params | `meta.funParams`, `funParamsByName` | ✔ `api/src/sockets.jl:798-804` | written **at dispatch**, whatever the outcome; GUI launches only |
| Chains | `settings/chains/{name}.json`; runs `settings/chains/runs/{id}/run.json` + `.cache/{sha}.json` | `tasks/chain/persistence.jl:107,146,174` | template hash (frozen), image uids, per image × node: status, task id, result, **params hash only** |
| Blackboard | `{proj}/blackboard/{id}/…` + `.snapshots/` | `api/src/blackboard_api.jl` | full revision history, author stamps |
| Figures, movies | `{proj}/figures/`, `settings/movies.json` | `figures_api.jl`, `app/src/figures.jl:111` | `producedBy`, source output + version + store mtime; staleness check `figures.jl:128` |
| Author stamps | `createdBy`/`updatedBy` = {profile, via, model?, session?} | `config/profile_settings.jl:268` | on gates, chains, boards, figures, blackboard, notebooks, lab log. **Not on run logs** |

**Tracing a number on a plot back to the raw file:**

| Hop | Recorded? |
|---|---|
| Board CSV row → image, cell/track, output, population | yes (`docs/ANALYSIS.md:430`) |
| Board PDF/SVG → anything | **no**: no embedded record (`frontend/src/plots/pdf.ts`, `boardSvg.ts`) |
| Population → gate as it was when plotted | **breaks**: only today's gate exists |
| Gate → labels it was drawn on | nominal: the field is always `v1` |
| Labels → params | from the run log's latest done run, rebuilt at export time (`chain/history_params.jl`), not frozen with the plot |
| Params → input image version | only if pinned; otherwise it follows `_latest`, unrecorded |
| Image → raw file | path only, no checksum |

**Sets don't record their members' differences.** An excluded image is dropped before dispatch,
leaving only a live log line (`sockets.jl:626,673`). A chain's skipped image gets `NODE_SKIPPED`, but
the reason goes only to the log (`chain/execute.jl:316-321`; `persistence.jl:174-185`). Missing
outputs are inferred from disk at read time.

**Silent defaults versus gaps shown as gaps:**
- Shown as gaps: author stamps leave model/session absent, interrupted runs are reaped as
  `interrupted`, and `channel_indices` refuses an unknown channel.
- Silent defaults:
  - ✔ calibration falls back to **1.0 µm/px and 1 min/frame** in `img_physical_sizes`
    (`model/image.jl:743-757`). Tasks that declare `requires.scale` are blocked; others aren't.
  - a legacy run entry with no status reads as `done` (`run_log.jl:148`).
  - a gating file with no unit reads as `px` (`persistence.jl:53`).
  - a figure with no recorded source is never flagged stale (`figures_api.jl:72-78`).

## 2. Stages and contracts

Every stage gets the run log, the task log, param validation and the applicability check from the
scheduler (`tasks/scheduler/run.jl:58-108`). The stage-specific parts:

| Stage | Main tasks | Adds to the record |
|---|---|---|
| Import | `importImages.omezarr`, `migrateLegacy` | calibration + units, acquisition, saturation, partial timepoint into `ccid.json` meta |
| Preprocessing | `cleanupImages.*`, `editImages.*` | a new *image version* (pixel writers do version, `keep_previous_version()` opt-in) |
| Segmentation | `segment.cellpose` / `coastal` / `importLabels`, `measureLabels` | labels + h5ad (overwritten); auto tile size only in the task log |
| Tracking | `bayesian_tracking`, `track_measures`, `correct` | track ids into the h5ad, trackProps, `motionDims` in QC |
| Correction | `segment.correct`, `tracking.correct` | a staleness list of downstream files (`correction_staleness.jl`) |
| Populations + gating | gating sidecars; `gate.manual_gate` as a chain pause | gate tree. **GUI gate edits are not tasks, so no run-log entry** |
| Analysis + plots | spatial, clustering, behaviour; plots via `pop_df` | run log + QC for tasks. **Plots and statistics record nothing** |
| Export | `exportImages.ome_tiff` | run log + QC |

| Contract | Status | Evidence |
|---|---|---|
| Physical units (xy, t) | enforced where declared | `requires.scale` blocks the run (`task/validate.jl:438-466`); units converted in `img_physical_sizes` |
| z spacing | **not enforced** | no task requires z; a missing value becomes 1.0 µm with an advisory warning (`qc.jl:271`) |
| 2D vs 3D | auto per image, warned | `track_measures.jl:446-452`; cohort flag `qc_cohort.jl:75-81` (advisory) |
| Axis order | enforced by name | `label_props.jl:436-442`, `image.jl:833-839` |
| Channel identity | enforced | `channel_indices` refuses unknown names (`image.jl:517`), ratchet-tested |
| Output names | syntax only | `value_name_problem` (`utils.jl:111-116`); same name ≠ same processing |
| Same populations across a set | convention | gate copy writes the same gates, then each image is editable; only the claim check compares them, and only the leaf gate (`claim_checks.jl:120-139`) |

**Where images in one set can silently differ:**
1. Auto tile size depends on free RAM/VRAM (`segmentation_utils.py:371`).
2. `dims = auto` is chosen per image.
3. Missing z spacing becomes 1 µm.
4. A chain fits set-scope steps on the subset that wasn't skipped.
5. Unpinned inputs follow `_latest`, and re-running segmentation flags nothing downstream (staleness
   exists only after corrections).
6. Defaults changed between releases (`CHANGELOG.md` *Read this first*) and no version is stored.
7. The tracking search radius is per frame (`bayesian_tracking.jl:119`). That is standard for
   trackers, not a flaw. It only matters if a set mixes frame intervals, and S1 already compares
   calibration across images.

**Extension points.** A new task is a struct + `_run_task` + JSON spec + registry entry
(`task_registry.jl`); custom modules use `register_task!` (`docs/CUSTOM_MODULES.md`). A new task gets
the run log and validation free. It must **opt in** to QC ("required" in `docs/MODULES.md:199`, but
no test enforces it) and to scale gating. **Population types have no registry**: the type lists are
hard-coded in at least five places and already disagree (`storage.jl:550`, `mixed_resolution.jl:200`,
`observer_summary.jl:27` lacks `branch`, `persistence.jl:143`, plus the frontend).

## 3. QC and checks along the pipeline

| Stage | Per-run QC | Cohort QC (set) | Cleanup facts | Acquisition panels | Claim checks |
|---|---|---|---|---|---|
| Acquisition | saturation, photon-limited, depth/time profile (`qc.jl:338,398`, `qc/profile.jl`) | saturation, sparsity, fade | zero %, clipped %, noise, floor | light path, data path, settings (numbers, no verdicts) | S1 calibration, S2 uncalibrated |
| Import | calibration, partial timepoint, pyramid (`qc.jl:678`) | nChannels/Z/T, pyramid | — | metadata dialog | S0 excluded image |
| Preprocessing | drift, denoise, smooth, align, flow, AF | drift, flow, AF | drift px/µm | — | — |
| Segmentation | counts, objects, seams (`segmentation.jl:393`) | nCells | — | — | S0 no data / params not recorded; S1 param diffs + confounds |
| Tracking | `bayesian_tracking.jl:170`, `track_measures.jl:570` | nTracks, MSD slope, motionDims | — | — | S0 dims not recorded |
| Correction | edits + staleness | fraction edited | — | — | — |
| Populations + gating | ✔ **none** (clustering only) | cluster metrics | — | — | S0 has gate; S1 leaf gate compared |
| Analysis + stats | spatial tasks; ✔ **statistics none** (`plotting/stats.jl:8`) | spatial metrics | — | — | S2 unit, few units, images per unit |
| Export | `ome_tiff.jl:199` | — | — | — | — |

- ✔ **Nothing blocks.** QC is info/warn only (`qc.jl:27`), cohort QC is advisory, and the claim
  checks are read-only.
- **Cohort QC runs only from a button** (`api/src/routes/image.jl:769`). ✔ Chains don't call it.
- **Only the claim review turn needs a model** (`api/src/claim_review.jl`). S0–S2 are rules.

## 4. Chains and replay

- Templates are authored on the whiteboard, from the REPL, or via MCP (create-only, validated at
  `chain/validate.jl:51`).
- A run starts only via `chain:run` (`sockets.jl:639`), which the autonomous MCP uses too.
- **Resume** skips nodes that are done with an unchanged params hash; `start_node` forces a re-run.
- **New images:** a fresh run works, but it loads the *current* template by name. No route starts a
  run from an old run's frozen template (UNVERIFIED for the GUI).
- **Determinism:**
  - clustering seed is ✔ hard-coded to 0, so it is not in the record.
  - device (CPU/GPU) is picked at run time and not recorded.
  - ✔ btrack's optimiser has a 10-minute wall-clock limit (`tracking_utils.py:225`), so a slow
    machine can give a different result.
  - no package or app versions are stored.
- **Replaying on the same images destroys the previous segmentation** (labels overwrite).

## 5. Where AI sits

| AI surface | Reads | Writes |
|---|---|---|
| Image, project, QC, lineage reads (MCP) | same readers as the GUI (`all_qc_docs`, run log, `ai/lineage.jl`) | nothing |
| Claims (`check_claim`, `state_claim`) | S0–S2 over the record | a Blackboard claim entry |
| Gates (`add/set/delete_gate`) | gating files | Claude-only routes, ✦-marked, stamped with model + session |
| Chains (`suggest_chain`, `create_chain`) | run log, templates | a template (create-only); the user runs it |
| Blackboard, lab log, boards, notebooks, figures | registries | create/revise with snapshots; lab log append-only |
| Marks, seek, navigation | — | ephemeral, nothing on disk |
| Kiwi turn | read-only allow-list (`kiwi_turn.jl:24`) | its own feed, capped at 50 |
| Autonomous `run_task`/`run_chain` | — | normal scheduler, so normal run log + QC |

**AI sits on top of the structure.** Every read goes through the same readers the GUI uses, and every
write lands in a shared, stamped artefact. The only private state is the Kiwi feed, the LabArchives
context file and in-memory marks. The "AI off" switch is `AI_TOGGLE_PLAN.md` (#2008).

## Not verified

- Whether the h5ad stores the producing params or labels version.
- Whether anything outside corrections flags tracks or gates as stale after re-segmentation.
- Whether the bioformats2raw version is recorded at import.
- Whether a GUI path replays a chain from its frozen template.
- Whether every task without `write_qc` is a justified exemption.
