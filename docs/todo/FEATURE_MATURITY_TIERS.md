# Feature maturity — starting tiers

**Status:** **draft** (2026-11-02) — provisional starting tiers for [`FEATURE_MATURITY_PLAN.md`](FEATURE_MATURITY_PLAN.md) P1; drafted by an agent, corrected by the maintainer the same day (decisions below).

**Maintainer decisions (2026-11-02):** features with no scientific result are **exempt** from the scale (no badge, no maturity row); the result-tested features listed under *Tested* are proposed for Tested, each pending the maintainer's review of its fails-if line; cluster-cells stays Beta and needs one PROVENANCE.md line from the maintainer; cluster-tracks is Beta (checked by hand, matches the old R version); train-flow-model, spatial-analysis, Phenotype, branching and legacy migration stay Experimental (not checked extensively).

**Every tier below is provisional, for the maintainer to confirm.** Drafted 2026-11-02 for `docs/todo/FEATURE_MATURITY_PLAN.md` P1.

- Repo: worktree `cecelia-hardening-audit-archive`, branch `hardening-surfaces`, HEAD `5883851517600ff4a7e7fd4eecb2099e4d1187f9`.
- Sources: `FEATURE_MATURITY_PLAN.md` (scale), `HARDENING_SURFACES.md` (inventory, "HS"), `HARDENING_RESULT_CORRECTNESS.md` (step 2a, "2a": ledger, §5 test-set needs, §6 ranked gaps), `docs/PROVENANCE.md` ("PROV"), `frontend/src/lib/guides/*` (27 guides incl. the tour), `mapData.ts`, `callForDatasets.ts` (3 ask ids).
- Every test name cited under evidence was grepped in this tree and exists. "Fails if…" lines come from reading the test name or its body. Tests were **not run**, and none was mutation-checked.

## How the rules were applied (read this first)

1. **Applied as a ladder.** Experimental is the floor. **Beta** needs documented evidence that someone checked results by hand on real data, from PROV, a docs/ section, a test comment, or (marked *inferred*) a 2a ledger row whose found-by is "visual/measured on real data" for that feature. **Tested** needs Beta plus a test that fails on a wrong result. A result test with no recorded hand-check does **not** jump a feature to Tested. Those rows stay at Experimental and say "result test exists" in the notes.
2. **Gating precedent.** Gating already has result tests (logicle golden, membership, a real-fixture count), yet the maintainer holds it at Beta until #1901 closes its known gap. I applied the same rule everywhere: **nothing is seeded at Tested.** Features that meet Tested's letter are marked **"Tested-eligible"** in the notes, with the gap that holds them back.
3. **Features with no scientific result** (profiles, blackboard, tour, Tasks page, …): the scale talks about results and fits these badly. I put them at Beta, which is the unbadged default per the plan's "an unbadged page is Beta" ("n/a-result" in the notes). That is a structural question for the maintainer, not a finding.
4. Abbreviations:
   - **IV2P** = intravital 2-photon live movies, one lab (zolIMa/fXgbTl, UMq4v3, yDfwP7, c91ICQ, MERTK…).
   - **KDIeEm** = the committed real measured+tracked fixture.
   - **ZARRFMT** = a real 64×64 crop with real calibration.
   - **syn** = synthetic test input.
5. "Proposed ask" = a new call-for-datasets entry that does not exist yet (from 2a §5). Since wired: each proposed ask is a `test` entry in `callForDatasets.ts` (CALL_FOR_DATASETS_PLAN Decision 9); the HPC site is a `noAsk`.

## Tiers by sidebar group

### Data

| id | title | tier | testedOn | evidence (path::testset — fails if…) | next (1–3) | needs / datasetAsk | notes |
|---|---|---|---|---|---|---|---|
| `import-images` | Import images | **Tested** (proposed — maintainer reviews the fails-if lines) | IV2P LIF/ND2/IMS/TIF; ZARRFMT; syn OME-XML | `app/test/suite/ome_qc.jl::"_delta_t_fallback"` — fails if the frame interval is not the median DeltaT converted to s (incl. series subdir, warm-up outlier). `omezarr_bf2raw.jl` v2/v3 meta read on ZARRFMT — fails if real 0.5964 µm / 3 µm / 30 s fall back to 1. Hand-check: 2a ledger 8936b2a7 "real LIF (c91ICQ)" (*inferred*) | ☐ Normalise non-s/µm units at `img_physical_sizes`, plus a QC finding for units it can't convert (2a gap 3 / S2). ☐ Tiny vendor-file fixtures with known calibration, incl. ms/min time and nm pixels | proposed ask: "vendor files with known pixel size + frame interval, non-second units" (2a §5 Import) | Tested-eligible for calibration *reading*. Held back by S2 (units read but ignored → speeds 60–1000× off) |
| `fix-metadata` | Fix pixel size and channel names | **Tested** (proposed — maintainer reviews the fails-if lines) | IV2P; syn stores | `ome_qc.jl::"sync_zarr_calibration!"` — fails if a corrected Z/DeltaT doesn't reach the NGFF the viewer reads. `ome_qc.jl::"resync_ome_meta! fill-only"` — fails if resync overwrites a human correction. Hand-check: ledger c6a4f3ca "visual (napari 1 s/frame)" (*inferred*) | ☐ A calibration edit marks downstream µm/speed outputs stale (2a S6, gap 4). ☐ Unit check, as in import | proposed ask: same as import | Tested-eligible (metadata writes, not science). The gap is staleness, not the write |
| `preprocess-images` | Preprocess an image (teaches crop) | **Experimental** | IV2P (inferred); syn | `python/cecelia/tests/test_crop_image.py::test_slices_xyz_keeps_channel_full` / `test_time_trim` — fails if crop bounds slice wrong. `calibration_helpers.jl` "CropImage inherits source calibration" (helper only) | ☐ Backend test that crop cuts the *active* version, not `default` (ledger 8bd0252d, unguarded). ☐ Task-level test of `adopt_store_counts!` wiring (50572d97) | — | Result test exists, but no recorded hand-check of crop pixels. Beta if the maintainer attests crops of drift stores were checked |
| `drift-correct` | Drift correct a time series | **Tested** (proposed — maintainer reviews the fails-if lines) | IV2P; syn known shifts | `python/cecelia/tests/test_drift_estimate_rigid.py::test_recovers_a_known_rotation_and_translation` — fails if the estimated rotation/translation is off. `test_drift_estimate.py::test_recovers_a_known_linear_drift`. Hand-check: PROV → *Ported subsystems* ("believed output-equivalent" to R, **not re-checked**) | ☐ Re-check parity against R on one real movie (PROV names this as the natural check). ☐ Real-pixel fixture with a known applied drift | `sitk-rigid-3d-full` (for the 6-DOF extension) | Tested-eligible by test. **Weak Beta**: PROV's line is a belief, not a check. Could be Experimental |
| `af-correct` | Correct bleedthrough and autofluorescence | **Tested** (proposed — maintainer reviews the fails-if lines) | IV2P; syn injected leak | `python/cecelia/tests/test_correction_utils.py::test_exclusive_recovers_the_WHOLE_leak_when_nothing_is_co_labelled` — fails if the estimator under-recovers alpha (the 0.0248 vs 0.113 bug). `::test_the_leaked_amount_is_subtracted_not_scaled` — fails if a co-positive voxel is zeroed. Hand-check: PROV → *Autofluorescence correction* | ☐ Fixture: 2-channel image with a measured single-stain control (real leak spectrum, 2a §5). ☐ Land the downstream-impact audit that PROV says is "in flight" | proposed ask: "single-stained controls from another microscope" | Tested-eligible. Unreleased channel-roles rework: saved settings won't pre-fill |
| `segment-an-image` | Segment an image (`segment.cellposeMeasure`) | **Beta** | IV2P (CART slice by eye); syn stub-model tiles | `test_tile_seam_stitch.py::test_every_cell_has_one_id_and_no_two_cells_share_one` — fails if a seam-crossing cell keeps two ids. `test_cellpose_stitch_ids.py::test_stitch3d_collision_is_repaired` — fails if an empty plane merges two cells. `test_segmentation_streaming.py::test_2d_timeseries_matches_golden` (stub model). Hand-check: `docs/SEGMENTATION.md` "Which knob first" ("outlines checked by eye on that slice") | ☐ Tiny annotated 3D stack, real weights on CPU in default CI, object count ± IoU golden (2a gap 1). ☐ Ellipticity test for measure (16ead1dd, unguarded) | proposed ask: "fixed/cultured 3D stack with hand-annotated masks" | No real-weights result test (smoke is opt-in, count>0), so it can't go higher |
| `train-denoise-model` | Train a denoise model | **Experimental** | IV2P (inferred) | none checks a result: `test_denoise_trainer.py`, `models_training.jl` (refusals/QC) are runs-only | ☐ Any output-quality check on a known-noise input. ☐ Record a hand-check of denoised output in docs | proposed ask: "paired low/high-SNR acquisition" | 2a §7: denoise/SUPPORT output quality not audited. I could not find a hand-check in repo docs |
| `train-flow-model` | Train a flow model | **Experimental** | IV2P (zolIMa, measured in SEGMENTATION.md) | `test_flow_train_normalisation.py::test_cutting_early_gives_the_reference_numbers_exactly` (normalisation numbers, not model quality). `omezarr_bf2raw.jl` intensityWeight default pin | ☐ Test that the flow metric sheet seeds the model's channel (851908ef, unguarded). ☐ Tiny checkpoint fixture (shared with segment-by-motion) | proposed ask: see segment-by-motion | **Unsure Exp/Beta.** Its result is judged through segment-by-motion's PROV check. CHANGELOG 0.1.1 says "early and still moving" |
| `segment-by-motion` | Segment a movie by motion (`segment.coastalMeasure`) | **Beta** | IV2P only (zolIMa, UMq4v3) | `test_coastal_utils.py::test_renaming_restores_the_training_channel_order` — fails if `mag_{lag}` planes are string-sorted (wrong channel per plane). The #1837 guard counts flow computations, not values. Inference never runs in CI. Hand-check: PROV → *Segmentation on real intravital data* | ☐ Tiny trained checkpoint + 16-frame synthetic movie, two groups on different channels, frame interval ≠ the model's (2a §5). ☐ Fix/test coastal `match_masks_3d` empty-plane collision (S8) | proposed ask: none possible (needs controlled motion; synthetic) | Engine lives in the sibling `coastal` repo |

### Data — features with no guide

| key (no guide) | title | tier | testedOn | evidence | next | needs | notes |
|---|---|---|---|---|---|---|---|
| `editImages.{zProject,tProject,bin,resampleZ,flip,dtype}` | Other preprocessing ops | **Experimental** | syn | `test_flip_runner.py::test_flip_mirrors_content_so_source_labels_no_longer_line_up` (flip only). The rest are calibration helpers only; projected/binned pixels untested (HS) | ☐ Pixel test per op (max-projection, bin mean). ☐ dtype test | — | One row for six ops. Split if the maintainer wants per-task badges |
| `editImages.register` | Staining-cycle registration | **Coming soon** (from `comingSoon` flag) | — | — | — | `static-3d-registration` | Derived from the flag, not tiered (plan: one source per fact) |
| `cleanupImages.stackAlign` | Intra-stack XY alignment | **Experimental** | syn | `test_stack_alignment.py::test_recovers_a_known_planar_shift` — fails if the shift is not recovered | ☐ Hand-check on a real stack, recorded | — | Result test exists, no hand-check |
| `cleanupImages.flowRegister` | Dense flow registration | **Experimental** | — | none (no runner test, HS) | ☐ Known-warp recovery test | — | |
| `cleanupImages.smooth` | Smoothing | **Experimental** | syn | `test_smooth_runner.py::test_gated_keeps_the_moving_feature_the_median_flattens` — fails if gated smoothing erases motion | ☐ Hand-check recorded. ☐ Guide or recipe mention | — | Result test exists, no hand-check |
| `cleanupImages.denoise` | Denoise with a trained model | **Experimental** | — | runs-only (`test_denoise_runner.py`) | see train-denoise-model | — | Map line names it; guide covers training only |
| `segment.importLabels` | Import external labels | **Experimental** | syn | `test_label_import.py` id uniqueness/cast (helper) | ☐ Round-trip: imported labels → measure equals the source | — | |
| `segment.branching` | Branching / skeleton | **Experimental** | syn aniso fixture | `test_branching_anisotropy.py` (synthetic known-angle) | ☐ Hand-check on SHG recorded in docs | — | **Unsure.** The maintainer has used it on real SHG (not documented in the repo) |
| `segment.ridges` | Ridge / fibre segmentation | **Experimental** | — | runs only (`test_ridges_runner.py`) | ☐ Known-line synthetic test | — | |
| `importImages.migrateLegacy` | Migrate legacy R projects | **Experimental** | the maintainer's own legacy projects (inferred) | `test_legacy_migrate.py` obsm/centroid relabel | ☐ Test that migrate fills TimeIncrement (f51fea6b, unguarded) | — | **Unsure Exp/Beta.** It was clearly run on real legacy data (ledger f51fea6b "visual") |
| `exportImages.ome_tiff` | Export as OME-TIFF | **Experimental** | syn | `test_ome_tiff_export.py` voxel + calibration round-trip — fails if voxels/calibration change | — | — | Result test exists; nobody recorded opening an export in Fiji |

### Populations

| id | title | tier | testedOn | evidence | next | needs / datasetAsk | notes |
|---|---|---|---|---|---|---|---|
| `gate-populations` | Create and gate populations | **Beta** (given) | IV2P; KDIeEm; syn | `app/test/suite/gating.jl::"Transforms"` — fails if logicle deviates from FlowUtils golden. `gating.jl::"Gates"` — fails if membership is wrong (incl. a logicle gate selecting by raw value). `pop_df.jl::"pop_df integration (KDIeEm)"` — fails if a real-fixture median gate's count ≠ the independent count. Hand-check: PROV → *Gating — population plausibility* | ☑ Channel-name match on gate copy/pool (#1901). ☑ Swapped-channel copy test (`gate_manual.jl`). ☐ Maintainer review → Tested. ☐ (open) a nested child ⊆ parent on real data | proposed ask: "2-image set with swapped channel order" (2a §5) | First Tested candidate, per the maintainer |
| `track-cells` | Track cells (`tracking.bayesian_track_measures`) | **Beta** | IV2P | `test_tracking_units.py::test_anisotropic_z_is_no_longer_under_counted` — fails if btrack links in pixels (units only). No test runs btrack and scores links. Hand-check: `docs/TRACKING.md` btrack two-phase note (steps measured on zolIMa/fXgbTl) + PROV → btrack "invoked identically" | ☐ Simulated tracks with known links (constant velocity + diffusion, anisotropic voxel, a gap, a division); score link accuracy (2a §5). ☐ Non-second frame-interval fixture | proposed ask: "ground-truth tracks (e.g. a Cell Tracking Challenge-style movie)" | The measures half is much stronger; see the next row |
| `correct-a-mask` | Correct a mask or a track | **Experimental** (given) | IV2P (manual eyeballing per test header) | `correction.jl::"track correction — join and split"` (op results). `test_correct_split_runner.py` pure helpers; its header says "end-to-end by manual eyeballing on a real project" | ☐ Test that the correct+re-measure composite forwards the corrected valueName (860da24b, unguarded). ☐ Op loop on ZTYX labels, not just the detector (56aaedb2) | — | Covers label correction **and** track correction (both given Experimental). user-docs: *In progress* |
| `cluster-cells` | Cluster cells into phenotypes | **Beta** (given) | IV2P, few markers; KDIeEm (B + copy as T) | No Leiden result test. `test_clustering_utils.py` checks stats helpers only. `overlay_3d_and_rail_3d.jl` "co-clustered segmentations stay apart" (isolation, on a copy). Hand-check: per the maintainer (no doc found) | ☐ Synthetic table with 3 planted clusters + two genuinely different segmentations (2a §5). ☐ Channel names compared at set-scope pooling (S1, gap 2) | `cluster-grouping-highplex` | Given Beta. **No written attestation in the repo.** Worth one line in PROV Needs one PROVENANCE.md line from the maintainer: what was checked, on which data. |
| `cluster-tracks` | Cluster tracks into behaviours | **Beta** | IV2P (yDfwP7, inferred) | runs-only (`meta_and_branching.jl` spec, `pop_types.jl`). `plots.jl::"track-grained plots return the aggregates they name"` (heatmap rows) | ☐ Test that track_measures/measureLabels call `prune_stale_clustfeatures!` (19b3bea7). ☐ Guard for the trackclust vn preference (76fa75df, N) | proposed ask: see behaviour-states | **Unsure Exp/Beta.** The maintainer uses it on real data, but no hand-check is recorded. Behaviour axes / cluster map (#1821–1823) are folded in, absent from CHANGELOG Checked by hand: the maintainer, 2026-11, matches the old R version on their intravital data. |
| `cluster-regions` | Cluster spatial regions | **Experimental** | IV2P; syn | `test_spatial_utils.py::test_two_populations_fractions` — fails if the composition formula is wrong (helper). Sparse-graph QC warns (`spatial_neighbours.jl`) | ☐ Planted-region synthetic fixture. ☐ CONTMESH extended to 2+ frames | proposed ask: "multiplex tissue section with annotated niches" | Few-marker intravital is a poor fit for regions |

### Populations — features with no guide

| key | title | tier | testedOn | evidence | next | needs | notes |
|---|---|---|---|---|---|---|---|
| `tracking.track_measures` + track diagnostics | Track measures / motility metrics | **Tested** (proposed — maintainer reviews the fails-if lines) | hand-built + analytic tracks; IV2P (MERTK, spleen) | `app/test/suite/labelprops.jl` celltrackR golden (L611+) — fails if speed/straightness etc. deviate from celltrackR 1.2.2. `tracking.jl::"diagnostics golden — MSD matches celltrackR squareDisplacement"` — fails if MSD drifts. Hand-check: ledger 6153985b "running on real data (MERTK + spleen)" (*inferred*) + PROV → *celltrackR track measures* | ☐ Run `celltrackr_golden.R` in a scheduled job (2a §7). ☐ Fixture with a non-second frame interval (speed units end to end) | — | Taught inside `track-cells` (composite). If the maintainer wants it badged on its own, it needs its own row or guide |
| `gate.manual_gate` (copy gating) | Copy gating to other images | **Beta** (with gating) | KDIeEm | `gate_manual.jl` runs only | gating's next | swapped-channel set | Gap 2 bites here first |
| MCP gating tools / ✦ proposed gate | Claude proposes a gate | **Experimental** | IV2P | `api/test/suite/gating_views.jl` data-derived transform params (checks-result); add-only guards | ☐ A proposed gate on a fixture vs a hand gate | — | Merged after v0.2.11, not in CHANGELOG |

### Explore

| id / key | title | tier | testedOn | evidence | next | needs | notes |
|---|---|---|---|---|---|---|---|
| `/phenotype` (no guide) | Phenotype page | **Experimental** | KDIeEm; syn | `app/test/suite/plots.jl::"plot count (raw + proportion normalize) — population summary"` — fails if counts/proportions are wrong | ☐ Record a hand-check (counts vs a manual tally on one image) | — | **Unsure.** Result test exists, but no recorded hand-check. Likely Beta if the maintainer attests. **Write a guide** |
| `behaviour-states` | Classify behaviour states (`behaviour.hmm`) | **Experimental** | KDIeEm | `tracking_hmm.jl::"HMM states + transitions"` — runs only; no known-answer fit | ☐ Simulated tracks switching between two speed states on a known schedule (2a §5). ☐ Interleaved-track-order fixture | proposed ask: "tracks with known behaviour switches" (likely synthetic) | Behaviour page plots: `summary_motion.jl` partial ≈ asserts |
| `spatial-analysis` | Score spatial interactions (`neighbourStats`) | **Experimental** | KDIeEm; syn | `pop_types.jl::"contact_matrix — CODEX log-odds heatmap matrix"` renders a sidecar (does not compute log-odds). `test_neighbour_stats_runner.py` QC contract only | ☐ Known-answer log-odds on a planted arrangement. ☐ CONTMESH over 2+ frames | proposed ask: as cluster-regions | **Unsure.** The neighbour graph's per-frame pairing *is* result-tested (see below). The log-odds the guide teaches is not |
| `spatialAnalysis.{cellContacts,contactsMeshes}` (no guide) | Cell contacts | **Tested** (proposed — maintainer reviews the fails-if lines) | KDIeEm; CONTMESH; IV2P movie | `spatial_neighbours.jl::"cellContacts nearest B is per frame"` — fails if pairing crosses frames. `test_mesh_containment.py::test_fragment_in_an_open_vacuole_is_mostly_enclosed`. Hand-check: ledger 9c45408a "visual on real movie" | ☐ Stale flag for `contained_by#` written under the old 0/1 meaning (gap 4) | — | Tested-eligible except for the staleness gap |
| `spatialAnalysis.detectAggregates` / `aggregatesMeshes` (no guide) | Aggregates | **Experimental** | syn; KDIeEm | `spatial_neighbours.jl::"aggregate DBSCAN is per frame"` — fails if a slow track reads as an aggregate. aggregatesMeshes: no test | ☐ aggregatesMeshes result test | — | |
| `spatialAnalysis.roiDiscovery` (no guide) | Find ROIs | **Experimental** | syn | `test_roi_utils.py::test_groups_at_any_density_need_no_radius` (synthetic groups) | ☐ The maintainer looks at the viewer panel (unseen per memory). ☐ Guide | — | Merged 2026-10-31, not in CHANGELOG |
| `behaviour.motif_discovery` (no guide) | Motif discovery | **Experimental** | KDIeEm | `tracking_hmm.jl::"motif discovery resolves the written HMM state column"` — runs only | ☐ Regression test for motif labels landing on unrelated tracks (41809852, **N**). ☐ Run-time multi-seg refusal test (1855256b) | as behaviour-states | |
| Behaviour + cell cards (no guide) | Cards | **Experimental** | KDIeEm | `api/test/suite/validate_and_cards.jl` "pick_card_example + motif/HMM example ranking" | ☐ Footer stats on two different segmentations (ab3d4c45) | — | |

### Analysis

| id / key | title | tier | testedOn | evidence | next | needs | notes |
|---|---|---|---|---|---|---|---|
| `build-plots` | Build plots (Analysis board) | **Tested** (proposed — maintainer reviews the fails-if lines) | KDIeEm; IV2P | `plots.jl::"plot percent (% positive of a 0/1 measure)"` — fails if Wilson CI ≠ published values or percent aggregation is wrong. `plots.jl::"plot statUnit=image"`. Hand-check: ledger cad78b0a / 534014ef "user on real data" (*inferred*) | ☐ pooled `pointUids` test (cad78b0a, **N**). ☐ CSV attributes toggle covers gating/UMAP/track/HMM CSVs (CHANGELOG 0.2.9 says it doesn't) | — | Tested-eligible on aggregation arithmetic |
| `record-a-movie` | Record a movie | **Beta** | IV2P (zolIMa/fXgbTl) | `overlay_3d_and_rail_3d.jl` "co-clustered segmentations stay apart" — fails if one segmentation's overlay draws another's cells. `movie_frames_and_overlays.jl` "label_geometry_mismatch". Hand-check: PROV → *Offline renderer* + *WebGPU viewer* (cross-check) | ☐ Z-window overlay visual check on real data (69488407, pending per commit) | — | |
| `build-an-animation` | Build an animation | **Beta** (*inferred*) | IV2P | `api/test/suite/movie_rail_translators.jl::"API: interpolate_keyframes reaches the incoming keyframe exactly"`. Hand-check: shares the offline renderer validated in PROV (*inferred*, not animation-specific) | ☐ Record one hand-checked fly-through | — | **Unsure Exp/Beta** |
| `create-a-notebook` | Create a notebook | **Experimental** | — | runs-only (`notebooks_rw.jl`); no CeceliaNb package tests found (HS) | ☐ CeceliaNb `pop_df` result equals the app's | — | Notebooks Phase 3 still pending |
| `use-the-blackboard` | Use the blackboard | **Exempt** (no result) | the maintainer's own use (inferred) | `api/test/suite/bidir_blackboard.jl` CRUD/versioning | — | — | No scientific result |
| `/movies` (no guide) | Movies & figures page | **Exempt** (no result) | — | `utils/movies.test.ts` | — | — | A player. **Not worth a guide** |
| Still figures (no guide) | Figure editor | **Experimental** | — | `test_plane_render.py` colormap vs matplotlib (checks-result) | ☐ Guide once P1–P5 settle | — | #1880–1887 merged after v0.2.11, not in CHANGELOG |

### Pipeline

| id / key | title | tier | testedOn | evidence | next | needs | notes |
|---|---|---|---|---|---|---|---|
| `run-a-chain` | Run a pipeline over a whole set | **Beta** | IV2P "common shapes" | `app/test/suite/task_machinery.jl` `_flatten_sections` (L69–83) — fails if nested section params don't reach the run (c1576eba). `chain.jl` end-to-end/resume: runs. Hand-check: PROV → *Chain executor* | ☐ A 3-task chain on the cellpose fixture whose outputs equal the same tasks run by hand (2a §5). ☐ Arbitrary shapes (parallel, resume-retry) | — | PROV states the gap itself |
| `/tasks` (no guide) | Tasks page / Task Manager | **Exempt** (no result) | — | frontend logic tests | — | — | **Write a short guide?** Probably not needed |
| Chain from history (no guide) | Chain from history | **Experimental** | real run logs (zolIMa UMq4v3) | `chain_from_history.jl::"Chain from history — zolIMa UMq4v3: segment → gate → tracking ×2"` | ☐ Guide/CHANGELOG mention | — | Code + tests only (HS) |
| `cohort QC` (no guide) | Cohort outlier check | **Experimental** | — | runs only | ☐ Known-outlier fixture | — | 2a §7: not audited |

### AI

| id / key | title | tier | testedOn | evidence | next | needs | notes |
|---|---|---|---|---|---|---|---|
| `assist-with-kiwi` | Assist with Kiwi | **Experimental** | testpr fixture, fake engine | `api/test/suite/kiwi_refs.jl` resolver (checks-result, *inferred* from name); `kiwi_turn.jl` runs | ☐ `scripts/kiwi_eval.jl` in CI or recorded results | — | **Unsure.** Kiwi pivoted recently (structured duck) |
| No-GUI chat (`pixi run chat`) (no guide) | Headless Claude | **Experimental** | — | `mcp/tests/test_autonomous.py` refusals | — | — | user-docs: "new and experimental" |
| HPC dispatch (no guide) | Run segmentation on a cluster | **Experimental** | one lab's SLURM | `hpc_remote.jl` sacct parsing (checks-result on strings). CI `verify-hpc.yml` container | ☐ Second site. ☐ HPC run output equals the local run (2a §7) | proposed ask: "a second HPC site" | user-docs: "Built for one lab's cluster so far". Sidebar group is a guess (it's Settings) |

### Install + settings (guide group "Start")

| id / key | title | tier | testedOn | evidence | next | needs | notes |
|---|---|---|---|---|---|---|---|
| `find-your-way-around` | Tour | **Exempt** (no result) | — | `guides.test.ts` anchors | — | — | |
| `use-a-user-profile` | Use a user profile | **Exempt** (no result) | — | `project_ownership.jl`, `profileApi.test.ts` | — | — | |
| `use-a-view-profile` | Use a sidebar layout | **Exempt** (no result) | — | `viewProfiles.test.ts` | — | — | |
| Browser viewer (no guide) | Viewer window + controls | **Beta** | IV2P | `api/test/suite/viewer_slab_and_meta.jl` byte-exact slab; `version_and_render.jl` "sRGB encode matches the browser viewer's canvas gamma". Hand-check: PROV → *WebGPU viewer / shader rendering* | ☐ Pan-sign picking test derived from the shader, not by hand (650db4bf). ☐ Check that viewer region selection filters by t (7ec4915a sibling) | — | Every task guide opens it, but nothing teaches the panel |
| Project export / import (no guide) | .ccbundle | **Experimental** | — | `ome_qc.jl` export runs | ☐ Import round-trip asserts identical data | — | |
| Shared / system install (no guide) | Admin install | **Experimental** | CI Linux/Win matrix | `verify-system-install.yml` (runs) | ☐ Wizard system-scope path verified | — | |
| Custom modules + plugins (no guide) | Drop-in tasks | **Experimental** | — | `plugins.jl` runs | — | — | |

## Tier counts (58 rows: 27 guides + 31 no-guide)

| tier | total |
|---|---|
| Tested (proposed) | 7 — import-images, fix-metadata, drift-correct, af-correct, build-plots, cell contacts, track measures |
| Beta | 11 |
| Experimental | 33 |
| Coming soon | 1 (`editImages.register`, from its flag) |
| Exempt (no result) | 6 — blackboard, Movies, Tasks, tour, user profile, view profile |

## Features with no guide

**Write a guide** (usable, and users meet it):
- Phenotype page: the only Explore page with no guide.
- Browser viewer panel.
- Cell contacts / contacts via meshes: on the Spatial page; the guide only teaches neighbourStats.
- Track measures / diagnostics: or fold explicitly into track-cells.
- Smoothing.
- Import external labels.
- Export OME-TIFF.
- Project export/import.
- HPC dispatch: a user guide, separate from docs/HPC.md.

**Not ready for one** (Experimental, or the surface is still moving):
- Find ROIs.
- Motif discovery: unguarded wrong-result bug.
- Aggregates via meshes.
- Ridges.
- Branching.
- flowRegister, stackAlign.
- Denoise (inference).
- Still figures (P1–P5 just landed).
- Chain from history.
- Cohort QC.
- Behaviour/cell cards.
- MCP proposed gate.
- No-GUI chat.
- Custom modules/plugins.
- Legacy migration.
- Other preprocessing ops.
- Staining-cycle registration (Coming soon).

**Not worth a guide:**
- Movies & figures player.
- Tasks page (no result; self-explanatory).

## Rows I was least sure about

1. **drift-correct (Beta vs Experimental):** the only hand-check record is PROV's "believed output-equivalent" to R, which is a belief, not a check.
2. **cluster-cells (given Beta):** there is no written attestation anywhere in the repo. Add one line to PROV so the Beta has evidence.
3. **train-flow-model (Exp vs Beta):** its quality is judged through segment-by-motion's PROV check.
4. **cluster-tracks (Exp vs Beta):** used on real data, but no recorded check.
5. **spatial-analysis (Exp vs Beta):** the graph pairing is result-tested; the log-odds the guide teaches are not.
6. **Phenotype (Exp vs Beta):** has a count result test, but no recorded hand-check.
7. **build-an-animation, build-plots, cell contacts:** Beta rests on *inferred* ledger found-by lines, not on PROV.
8. **segment.branching, migrateLegacy:** real use is likely, but none is documented.
9. **assist-with-kiwi:** I don't know how its claim validation should be judged on this scale.
10. **Structural:** the n/a-result features (profiles, blackboard, tour, Tasks, Movies). Should the scale apply at all, or should they be exempt and unbadged?
