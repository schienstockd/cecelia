# Phase 1b — Decisions and evidence, correction functions

Scope: `app/src/tasks/cleanupImages/*` (`afCorrect`, `driftCorrect`, `smooth`, `stackAlign`, `flowRegister`, `denoise`, `flip`, `dtype`, and the composite `afDriftCorrect`). Sources: `git log`, merged PR bodies (`gh pr view`), and the parked plan docs under `docs/todo/`. Every claim below is either a direct quote or a citation to a file+line.

`docs/todo/CORRECTION_PLAN.md` is about MANUAL mask/track correction in napari; **out of scope** for this audit (image corrections only).

Search discipline: `gh pr list --state merged --limit 500 --json number,title,mergedAt` was the enumeration; the keyword filter used was `\b(af|autofluor|bleed|spill|drift|denoise|smooth|cellpose|stackAlign|flowRegister|correction|temporalSmooth|qc)\b`. Every PR that survived that filter is either cited below or was ancillary (progress bars, message wording, cohort-QC test wiring) and not a decision.

---

## Chronology

### 2026-07-07 — PR unknown, commit `43d41774` — QC framework born with drift as the first producer
> "General 'we processed this, but the output looks off' layer (advisory, never blocking), with drift correction as the first producer. […] `drift.jump` (a per-frame step dwarfing the trajectory's median — the actual cause, e.g. fHqhyb at T15->T16) and `drift.canvas_expansion` (XY grew >25%; Z growth is fine)."

Impact: establishes the QC sidecar contract (`1/{uid}/qc/{funName}/{valueName}.json`) that every later correction reports through. Sets the split *short = problem, long = action* — enforced later in PR #458.

### 2026-07-24 — PR #323 — AF correction rewritten to stream per timepoint
> "`af_correct_channel` loaded a whole channel (all timepoints) and cast it to float64 — ~47 GB for one channel of your 181×4×31×1024×1024 movie […] Rewrites the whole AF path to process one timepoint at a time into the output store."

Impact: no rule change, but establishes that AF-correction memory characteristics scale with `SizeC × SizeZ × SizeY × SizeX × sizeof(float64)`, not with `SizeT`.

### 2026-07-31 — PR #427 — Every task must declare its QC position
> commit `dd1013ea`: "Five tasks had neither `write_qc` nor the exemption comment CLAUDE.md requires. Four are genuinely exempt and now say so with a reason — `remove` […] `cropImage` (deterministic geometric subset, no measurement output), `cellpose_correct` (perceptual denoise, the named example), and `af_correct`, whose comment records honestly that this is the weakest of the four: over-subtraction has a plausible objective signal (fraction of pixels…"

Impact: introduces a convention that every correction either writes QC or carries an inline "QC-EXEMPT" comment justifying itself. Directly relevant to Phase 2 — enumerates who is scoreable.

### 2026-08-01 — commit `95894bed` — AF: percentile-based ceiling is the wrong estimator family
> "**Why the percentile window had to go, measured.** At the only setting that gave a usable image (max = 100) the window was the ratio's *maximum*. On a real 181-frame movie the top six occupied bins held exactly ONE voxel each, so a single voxel in 5.88 billion set the output scale of the whole image […] Tightening to p99.99 was unusable for the opposite reason: the signal lives in the tail (p99.99 = 52 against a max of 256), so any percentile near 100 cuts into real structure. […] the ceiling is now `robust_hist_max` — the highest value at least K voxels attain."

Impact: deletes 18 legacy AF params. Backgrounds now derived via Zack's triangle threshold over the non-zero population.

### 2026-08-01 — commit `92295f35` — Ceiling banked as a cohort-only metric
> "AF's two existing metrics are blind to the failure that actually invalidates a comparison. […] a 1.86x ceiling difference moved `clippedFrac` and `levelsUsedFrac` by 0.000. […] `ceiling` becomes a cohort metric with NO finding attached: one image's ceiling is neither right nor wrong, and `qc_cohort.jl`'s outlier detector is what can judge it."

Impact: establishes the *per-image finding vs cohort metric* split — some things can only be judged relative to peers. Ceiling ranges by **1.71×** across 9 identical-setting movies of one experiment.

### 2026-08-01 — commit `5131d794` — Composite tasks declare which steps they DON'T preview
> "A composite now says what it does not preview. `preview_params` delegates to the first previewable step, so `afDriftCorrect` previews AF and silently skips drift correction — which expands the canvas and shifts every frame, so the geometry on screen is not the geometry the run produces."

Impact: composite semantics are "run the child specs in list order"; there is no shared plan/ordering engine — the order is *literally the JSON array* (`app/src/tasks/cleanupImages/af_drift_correct.json` → `"composite": ["cleanupImages.afCorrect", "cleanupImages.driftCorrect"]`).

### 2026-08-03 — PR #448 — AF's "neutral ratio" was a pedestal, not zero
> "a voxel with no signal and no autofluorescence lands on `ratio == 1`, 'no excess over the reference'. That is exactly what AF correction removes, so it should come out as 0. It came out as `rescale / c_max`. Measured on `kSUFux/Or1L8a` (uint8, CH1÷CH4, derived ceiling 15.06): every background voxel was 17 of 255."

Impact: scientific-behaviour change; downstream measurements had been on a pedestal for the whole ratio era.

### 2026-08-03 — PR #450 — Ratio replaced by channel-dominance weight
> "The ratio goes to zero wherever the target isn't brighter than its reference, so a cell carrying two reporters was hollowed into a dim rim […] the centre, where both channels are bright and the ratio sits at 1, pushed to zero. A hollow cell doesn't segment, and segmentation runs next."
> Formula: `out_t = b_t · b_t^p / Σ b_i^p over {target} ∪ competingChannels`, "Output is in input counts (fidelity slope 1.001/1.000 on clean cells) and can never exceed its input — no ceiling to derive, nothing to clip."

Impact: AF's output units change from "ratio × ceiling" to "input counts". Retires derived-ceiling QC as previously interpreted. Explicitly asks for scientific sign-off before merge.

### 2026-08-03 — PR #458 — `af-low-range` warning DELETED, and re-tuning would have been wrong
> "It warned when the output used under 20% of the dtype's levels — a real signal under the RATIO, whose output was stretched to fill the range through a derived ceiling, so using little of it meant the ceiling had been derived too high. #450 replaced that with the power weight, whose output is in INPUT COUNTS: a 16-bit channel carrying signal in the low thousands legitimately occupies a sliver. […] The threshold survived the mechanism change with its premise inverted, so it fired on everything and meant nothing."

Impact: pattern to codify in Phase 2 — a threshold outliving the mechanism it was calibrated for is worse than no threshold.

### 2026-08-04 — PR #462 — Clipping measured against signal voxels, not all voxels
> "`topFrac` divides clipped voxels by every voxel in the channel. These images are ~95% background, so the figure is diluted by empty frame — it says as much about how much blank space an acquisition contains as about the clipping. […] `clippedSignalFrac` divides by voxels above the derived background."

Impact: metric-scope rule — per-image, per-channel; **denominator must be the signal population, not the whole tensor.**

### 2026-08-05 — commit `95cb553f` (temporalSmooth landed) — new correction, born because AF fails on photon-limited data
> "AF correction produced almost nothing on resonance-scanner movies, and the correction is not at fault — its background derivation is. `af_weight_stats` finds the background with a triangle threshold, which assumes the channel HAS a background population. Resonance dwell times give single-digit photon counts, so each channel is a delta at zero plus a thin tail and the threshold lands INSIDE the signal."

Impact: **the clearest scanner-type applicability rule in the corpus.** Resonance-scanner data → AF alone insufficient → smooth as a prerequisite (spatial gaussian + temporal median). SMOOTHING_PLAN.md L111 pins the same claim.

Also from the same commit — order rules:
> "MEDIAN for time, never for space. A spatial median rejects sparse photon counts as outliers. Mean over time inflates masks ~34% by averaging in a cell that moved through the window."
> "`restoreDynamicRange` applies ONE gain across all smoothed channels, so cross-channel ratios hold."

### 2026-08-06 — commit `9a2353d5` — task rename `temporalSmooth → smooth`
> "The name overstated the temporal term. It is one optional parameter (`temporalFrames=1` disables it) and the spatial gaussian is what does the work on photon-limited data. […] BREAKING for saved chains: a template calling `cleanupImages.temporalSmooth` no longer resolves […] Existing stores are NOT migrated."

Impact: canonical example of a rename that broke saved chains without a migration. Value-name legacy: stores written before 2026-08-06 carry `temporalSmoothed`; new runs write `smoothed`.

### 2026-08-08 — PR #493 — AF audit rescued from an unmerged worktree
> "`docs/todo/AF_CORRECTION_AUDIT.md` was written on 2026-08-06 in the `af-correction` worktree and never committed to any branch. It existed only as an untracked file, so it would have been destroyed when that worktree was pruned."

Impact: pointer to `docs/todo/AF_CORRECTION_AUDIT.md`, which the later bleedthrough decisions cite as their ground truth.

### 2026-08-11 — PR #523 — Valid-box provenance survives the pipeline; several correctors dropped it silently
> "`smooth` carried it via `read_valid_box(path)` — which on a per-frame box returns the union over frames, nearly the whole canvas once the window drifts. `af_correct`, `cellpose_correct` — dropped it silently."

Impact: earliest documented case of downstream corrections silently invalidating an upstream provenance artefact — directly relevant to Phase 1d (provenance/versioning). Fixed by `zarr_utils.carry_valid_box(src, dst)`, "self-refuses when the geometry moved".

### 2026-08-11 — PR #524 — Drift estimator becomes redundancy-based; reliability is a QC output
> "The estimator chained neighbour measurements and integrated them in order, with nothing to check them against. `estimate_drift` now measures every pair up to `driftMaxLag` apart and solves the whole trajectory in one robust least squares with a second-difference prior […] `drift.unreliable` QC finding — the cycle residual is a reliability number for free."

Impact: introduces `driftMaxLag`, `multiLag` estimator, and cycle-consistency as the drift reliability metric. Old `chain` estimator retained under the same task's `driftEstimator` param.

### 2026-08-12 — PR #526 — Padding figures are a property of THIS MACHINE's data
> "The doc now says these are a property of this machine's data rather than of the feature, so the next estimator change doesn't silently invalidate them again."

Impact: convention. Cite for Phase 1d (ground-truth availability) — cecelia has no reference dataset; numbers come from `dev/` movies.

### 2026-08-14 — PR #555 — AF has two jobs (leak + co-presence), only one mechanism
> "AF correction has two jobs and had one mechanism: removing intensity present in several channels because the tissue is autofluorescent, and removing it because the filter set leaks. The dominance weight only ever did the first. On WIaUjL/p6t4mC, CH3 leaked 2.3% into CH2 and was ~7× brighter above background. The weight scales, so it read every co-positive voxel as CH3's: corrected CH2 came out 98–99% zero, and segmenting it found the residue at the bright CH3 spots — i.e. it found CH3."

Impact: adds `af_bleedthrough_alphas` (per ordered channel pair) subtracted BEFORE the weight. Explicit ordering constraint inside AF.

### 2026-08-14 — PR #559 — Bleedthrough estimator choice is a specimen question (`exclusive` flag)
> "Which estimator is right is a question about the SPECIMEN, not the pixels, so it is asked. Each combination carries `exclusive` — 'Different cell types', default on. On → `tls_slope`, because with nothing legitimately co-located the whole proportional relationship is leak. Off → `envelope_slope`, the floor."
> "**NOT resolved:** on synthetic data with no co-labelling the two estimators agree to within 3%, so the 5x divergence on real data is not explained by the definitions."

Impact: **canonical example of a "not derivable" precondition** — Phase 2's wizard question set will lean on this pattern. Default = "on" (exclusive), rationale documented.

### 2026-08-21 — PR #610 — Cellpose v4 migration; `cleanupImages.cellposeCorrect` RETIRED
> Dominik quoted verbatim: *"cellpose denoise is not that useful. we will scratch that. coastal denoise and smooth will replace that. we have to migrate to cellpose v4. v3 is outdated. and doesn't perform as well on static images."*
> "Breaking. `cyto3`/`cyto2`/`cyto`/`nuclei` and `cleanupImages.cellposeCorrect` are gone, and custom v3 checkpoints cannot load. Existing outputs on disk are untouched and stay readable; what cannot be done any more is *re-running* an old cellpose configuration."

Impact: a whole correction task deleted; `RETIRED_FUN_NAMES` machinery introduced (`app/src/tasks/task.jl:1615`). Chain templates naming the retired task now error with a migration message.

### 2026-09-02 — PR #764 — Axis-guard becomes per-param, not per-task
> "Smooth is the only task in the tree with a mixed axis-dependency: spatial sigma applies to a still, the temporal window does not. Gating the whole task on `requires.axes: ['T']` refused a run that works."

Impact: `requires.axes` moves from task-level to param-level; `smooth` becomes usable on static (IBEX / single-frame) images. Sets a precedent — corrections can be *partially* applicable to an image and the task should say so per-param.

### 2026-09-03 — PR #777 — Smooth gains bilateral (VST) engine, and other candidates were tried and rejected
> "Coastal denoise net (`denoise_cyto3`) and Noise2Void/N2V2 hallucinate on this data — dropped. Wavelet BayesShrink over-shrinks (blocky). SimpleITK curvature/patch-based barely move the needle at these params. Anscombe-VST → cv2 bilateral → unbiased inverse was the clear winner."

Impact: `spatialMethod` param — gaussian | bilateral_vst — mutually exclusive.

### 2026-09-04 — PR #785 — Third `driftCorrect` estimator: `sitkRigid`
> "Default stays `multiLag`. Author confirmed on 2026-09-04: real datasets are translation-only; rigid is an opt-in for movies where the stage was bumped. Never quietly promote it — an existing project re-run on a new build must produce the same numbers."

Impact: adds `driftMaxAngle` (default 5°), `showIf` gating `driftMaxLag` → multiLag only, and a `drift.rotation.capped` finding when any frame exceeds the cap. Sets precedent: **algorithm choice is a param, not a new task; new default requires an explicit reason.**

### 2026-09-04 — PR #791 — `driftSmoothSigma` post-solve trajectory low-pass
> "The writer places each frame at `round(cumsum(shifts)[t])` per axis, so a trajectory that sits at the phase-correlation noise floor (~0.4 px std) around a small true drift produces per-frame integer-pixel *jumps* in the corrected zarr — what reads as 'jittering' on movies where the sample barely moved."
> "**Smooths *positions* not deltas** because a per-delta threshold amplifies cumulative drift when small noise deltas partially cancel real spikes."

Impact: default σ=6 frames; a QC metric change (residuals still measured on unsmoothed solution, so reliability isn't masked by the smoother).

### 2026-09-04 — PR #793 — New task `stackAlign`, **sits BEFORE `driftCorrect` in the pipeline**
> "New task `cleanupImages.stackAlign` sitting BEFORE `driftCorrect` in the pipeline. […] Fixes issue #3 from the ttRMjQ audit (adjacent Z planes offset during breathing). Does NOT fix issue #4 (within-plane line-scan smear) — genuinely unrecoverable, QC-flag-only."
> "New task, not a param on `driftCorrect` — different semantics (per-plane per-timepoint XY vs per-timepoint rigid XY(Z))."

Impact: **first explicit ordering rule in the code corpus** (although "before" here is prose in a PR/plan, not code — there is no dispatch layer that enforces it). Two QC findings: `stack_align.unreliable`, `stack_align.large_shifts`.

### 2026-09-04 — PR #795 — `driftNormalisation` (none|phase) restored; gated to PCC estimators
> "Restores the GUI knob removed in `fe20cc8e` (PR #785). The rationale at removal — 'never materially changed the estimate' — held for the movies of that cohort but fails on movies with large per-frame drift or low-SNR reference frames."
> "Unverified: whether `phase` actually helps on the specific movie that prompted this — that requires a real-data run."

Impact: canonical example of a param removal / restoration ping-pong driven by "never materially changed the estimate" being **overgeneralised from one cohort**.

### 2026-09-05 — PR #796 — `cleanupImages.denoise` (SUPPORT); saturation-gate MANDATORY
> "Refuses saturated channels via the import-time saturation metadata."
> DENOISE_INTEGRATION_PLAN.md D6: "Saturation gate is mandatory. Refuse or warn when the input's `rescale8bit.trueMax` is at (or very near) the dtype ceiling. […] Rationale — SUPPORT and other supervised denoisers learn a mapping from noisy → clean. Saturated pixels have no ground truth: everything above the ceiling reads identical."

Impact: introduces the *saturation precondition*, computable from import metadata. Concrete: threshold ≥ `dtype_max * 0.98`. Also codifies the "kind" of applicability rule where the metric is per-channel binary (saturated / not), not continuous.

### 2026-09-05 — PR #798 — `flowRegister` — third rigidness tier, sits BETWEEN stackAlign and smooth
> "Fills the visible gap surfaced by the drift-3d follow-up brief: intravital movies where the sample deforms non-rigidly during a single frame's resonant/galvo scan produce frame-to-frame flexing that varies across (x, y) — no rigid alignment can fix it, dense flow can."
> PR pipeline diagram:
> `driftCorrect — per-frame rigid (bulk translation) / stackAlign — per-plane rigid (breathing offset) / flowRegister — per-pixel non-rigid ← NEW / smooth — temporal denoise`

Impact: the **most explicit pipeline-order statement in the corpus** — but again, this is prose in a PR body, not code. Nothing in `app/src/task_registry.jl` or `chain.jl` enforces the order.

### 2026-09-05 — PR #802 — `smooth` gains `stat='farneback'`; pipeline order re-stated
> "Fusion sits DOWNSTREAM of them in the cleanup pipeline: driftCorrect → stackAlign → flowRegister → smooth (now with `stat='farneback'`)."

Impact: **contradicts PR #798's order** in one respect (see Contradictions §1).

### 2026-09-05 — PR #804 — SUPPORT vendored code deleted; algorithm moves to coastal
> "Algorithms live in coastal, cecelia is a thin orchestration layer."

Impact: no rule change; boundary discipline.

### 2026-09-05 — PR #805 — `denoise` training refuses T-too-short movies upfront
> "Julia handler now pre-checks `img.meta['SizeT']` per selected image before launching Python. When no image is long enough, it refuses with the actual T seen and the largest `inputFrames` that would fit."

Impact: another precondition computable from metadata: `SizeT ≥ inputFrames`. Establishes "check what you can in Julia before launching Python" as an early-fail pattern.

---

## Contradictions and unresolved arguments

### C1 — What is the "correct" pipeline order? Prose says two different things.
- **PR #793 (2026-09-04)**: `stackAlign` "sitting BEFORE `driftCorrect` in the pipeline". `STACK_ALIGN_PLAN.md` L14: "Handled BEFORE `driftCorrect` in the pipeline (planes aligned first, then the whole-timepoint drift is corrected across time)."
- **PR #798 (2026-09-05)**: `driftCorrect / stackAlign / flowRegister / smooth`. Here `driftCorrect` is listed FIRST, `stackAlign` SECOND.
- **PR #802 (2026-09-05)**: `driftCorrect → stackAlign → flowRegister → smooth` — matches #798, contradicts #793 / STACK_ALIGN_PLAN.
- **Code**: nothing enforces either order. `chain.jl` builds user-authored chains; the sole hard-coded ordering is inside `af_drift_correct.json` (`["cleanupImages.afCorrect", "cleanupImages.driftCorrect"]`).

Flag for maintainer resolution.

### C2 — Which bleedthrough estimator to use is a specimen question, and the two disagree on real data
`AF_CORRECTION_AUDIT.md` L27-29: "On synthetic data with no co-labelling the two estimators agree to within 3%, so the 5× divergence on real data is not explained by the definitions — recorded, not resolved." The choice is exposed as `exclusive` (default on). The disagreement between synthetic and real data is documented but never diagnosed.

### C3 — `driftNormalisation` param removed then restored on adjacent days
- PR #785 (2026-09-04, commit `fe20cc8e`): "Drop `driftNormalisation` from the GUI. It was a straight leak of skimage's `phase_cross_correlation(normalization=...)` argument — always left at `none` in real use, materially no effect on the estimate."
- PR #795 (2026-09-04): "Restores the GUI knob […] The rationale at removal — 'never materially changed the estimate' — held for the movies of that cohort but fails on movies with large per-frame drift or low-SNR reference frames."
Same day, opposite decisions. The restoring PR itself flags: "Unverified: whether `phase` actually helps on the specific movie that prompted this — that requires a real-data run."

### C4 — AF's derived ceiling: cohort metric with no finding, but cross-image comparison is fragile
PR commit `92295f35`: ceiling banked as a cohort-only metric with "NO finding attached". The 1.71× range across nine identical-setting movies is real, but the code cannot judge whether that is "acquisition drift or real depth-dependent biology". If Phase 2 wants to auto-flag correction runs whose ceiling is an outlier, the current design deliberately leaves that judgement to the human.

### C5 — `smooth`'s temporal-mean fell out of the shipped default set with two conflicting justifications
- Commit `95cb553f` (SMOOTHING_PLAN L111 vintage): "Mean over time inflates masks ~34% by averaging in a cell that moved through the window."
- SMOOTHING_PLAN.md L119-130 (2026-08-24 addendum): probes `gated` and finds it non-degenerate on sparse data, reversing an earlier assumption that block-matching would have no texture to work with — the fear was "the option help in the form had turned that into 'Not for photon-limited data' — a prohibition where the evidence was an absence."

Not strictly a contradiction — the second entry is a self-correction — but the pattern of a plan-doc warning outliving the measurement it was built on has now happened twice (also PR #458's `af-low-range`).

### C6 — Smooth composite (`smoothAfDriftCorrect`) was designed and NOT built
`SMOOTHING_PLAN.md` L379-394 designs a `cleanupImages.smoothAfDriftCorrect` composite (`["cleanupImages.smooth", "cleanupImages.afCorrect", "cleanupImages.driftCorrect"]`). L417: "The composite (`smoothAfDriftCorrect`) was not built." Nor were "the two drift follow-ups below" (estimate drift on the denoised store; smooth the shift trajectory — the second one is now done differently). The AF-first order the plan proposed is not represented in code, so Phase 2 will need to state whether it is still the intent.

### C7 — Composite `afDriftCorrect` runs AF *without* first-run drift estimate; the smooth-first order in SMOOTHING_PLAN would invert that
`af_drift_correct.json`: `["cleanupImages.afCorrect", "cleanupImages.driftCorrect"]`. SMOOTHING_PLAN's proposed composite puts smooth first, then AF, then drift — implying drift should be estimated on the denoised, AF-corrected store. Both survive in the corpus; only the two-step AF→drift form runs today.

---

## Clearest applicability rules found in the corpus

1. **Scanner regime → smooth is prerequisite for AF.** Resonance-scanner / photon-limited data → AF alone insufficient because triangle threshold lands inside the signal (SMOOTHING_PLAN.md L111, commit `95cb553f`).
2. **`sitkRigid` estimator is opt-in, `multiLag` stays default.** "Real datasets are translation-only; rigid is an opt-in for movies where the stage was bumped. Never quietly promote it" (DRIFT_RIGID_PLAN.md decision 2 / PR #785).
3. **Denoise refuses saturated channels.** Precondition = `meta.saturation.channels[i].saturated == true`; threshold = `dtype_max * 0.98`; error = `denoise.channel_saturated` finding (DENOISE_INTEGRATION_PLAN.md D6 / PR #796).
4. **Denoise-training refuses too-short movies.** `SizeT ≥ inputFrames`; check in Julia handler before launching Python (PR #805).
5. **AF bleedthrough estimator selection is a SPECIMEN question.** `exclusive=on` (different cell types) → `tls_slope`; off (co-labelled) → `envelope_slope`. Default on (PR #559).
6. **`stackAlign` "does NOT fix" within-plane line-scan smear.** QC-flag-only, deferred (STACK_ALIGN_PLAN.md L14-19).
7. **Existing corrected stores are never migrated on task rename/retire.** `temporalSmoothed` stores survive; `cpCorrected` stores survive after cellpose retirement (`RETIRED_FUN_NAMES` in `app/src/tasks/task.jl:1615`, commit `9a2353d5`).
8. **Composite skips previews from non-first steps.** `afDriftCorrect` preview shows AF only; drift is silently skipped in the preview (commit `5131d794`).
9. **Metric denominators = the signal population, not the whole tensor.** `clippedSignalFrac` over `topFrac` (PR #462). Applies to any correction whose output is measured against background-vs-signal.

---

## Notes on search discipline

- `gh pr list --state merged --search '<term>'` returned `[]` for every keyword tried — the `--search` flag on this repo's `gh` build ignores body text. The enumeration used `gh pr list --state merged --limit 500 --json number,title,mergedAt` and grepped locally on the title stream.
- No PR touched a "correction dispatch" or "correction order" module — none exists. The correction order is emergent (user-authored chain + composite JSON arrays).
- `git log --follow --oneline` per file was used for every file in `app/src/tasks/cleanupImages/`. `dtype.jl` and `flip.jl` have only their birth commit (`a8f393c4`, preprocessing group) and one refactor (`19c70498`); nothing about applicability rules for them.
