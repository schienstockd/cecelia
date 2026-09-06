# Phase 1c + 1d — QC signal sources, provenance, conflicts, scope, ground truth

Reads `audit_phase1a_catalog.md` (catalog) and `audit_phase1b_history.md` (history) as authoritative — this doc does not restate their findings, only cites them.

---

## Phase 1c — QC signal source per correction

| Correction | Precondition | Class | Field or metric | Cite | Notes |
|---|---|---|---|---|---|
| `cleanupImages.afCorrect` | User must declare which channels share signal (AF and/or leak); the mechanism has no free threshold to gate on. | **user** | `params.afCombinations` (per-target list of competing channels) + `params.afCombinations[*].exclusive` (specimen question: "different cell types") | `app/src/tasks/cleanupImages/af_correct.jl:184-193`; `python/cecelia/utils/correction_utils.py:1527`, `:1975` (`exclusive` estimator selection) | The **canonical "not derivable" case** per PR #559 (`audit_phase1b_history.md` §2026-08-14 / §Applicability rule 5). Default `exclusive=on`. Saturation is not a precondition here — AF derives its own `saturatedFrac` per-channel at runtime from pixels (`af_correct.jl:124`) and only warns; it does not read `meta.saturation` (see `audit_phase1a_catalog.md` inferred assumption 4). |
| `cleanupImages.afCorrect` — bleedthrough sub-decision | Whether a channel-pair actually leaks in this movie. | **compute** | `af_bleedthrough_alphas` — per ordered channel pair α (`tls_slope` or `envelope_slope` per `exclusive` flag). Floored at `AF_ALPHA_MIN`. | `python/cecelia/utils/correction_utils.py:1489-1527` (per-pair scalar); `af_correct.jl:82-91` (docstring). | Computed inside the task, not surfaced upfront. Could be turned into a plan-time signal only by running a cheap probe. |
| `cleanupImages.afDriftCorrect` (composite) | Inherits `afCorrect` (user) + `driftCorrect` (compute). | — | — | `af_drift_correct.json` (hardcoded order `[afCorrect, driftCorrect]`) | Composite has no precondition of its own; order is JSON-array, not a rule engine (`audit_phase1a_catalog.md` inferred assumption 7). |
| `cleanupImages.denoise` | (a) at least one selected channel is NOT saturated; (b) `SizeT ≥ inputFrames` for training. | **auto** | (a) `meta.saturation.channels[i].saturated` (written at import); (b) `meta.SizeT`. | (a) `app/src/tasks/cleanupImages/denoise.jl:24-93`; import writer `app/src/tasks/importImages/omezarr.jl:1003`. (b) PR #805 (`audit_phase1b_history.md` §2026-09-05). | `saturated == true` uses a **structural** rule (pile-up in the brightest occupied bin — `python/cecelia/utils/intensity_utils.py:82,135`), not `dtype_max * 0.98` as documented in the plan; the plan quote in 1b §2026-09-05 PR #796 describes the intent, the code uses structural detection. Refuses run when ALL selected channels are saturated; drops saturated channels and fires `denoise.channel_saturated` otherwise. |
| `cleanupImages.denoise` | Guidance: temporal denoise "compares the same pixel across frames" → drift-corrected input helps. | **user** | `params.valueName` (soft tip in JSON `requires: {axes: ["T"]}`) | `denoise.json:17` | No structural enforcement — the tip is `showIf T-axis`. |
| `cleanupImages.driftCorrect` | Timelapse (`T`-axis) present; reference channel picked; estimator choice appropriate. | **auto** (axis gate) + **user** (channel + estimator) | `requires.axes: ["T"]`; `params.driftChannel` (name); `params.driftEstimator ∈ {multiLag, chain, sitkRigid}` | `drift_correct.json:9`; `drift_correct.jl:112` (channel resolution); PR #785 (`audit_phase1b_history.md` §2026-09-04). | Rule 2 from 1b: `sitkRigid` is opt-in, never quietly promote. `driftMaxLag` only for `multiLag`; `driftMaxAngle` only for `sitkRigid` — the finding vocabulary differs per estimator (`residualRms`/`unreliable` present only for PCC estimators, `drift.rotation.capped` only for `sitkRigid`). Reliability = `residualRms` (2 px warn, calibrated on 18 movies; `drift_correct.jl:12`). |
| `cleanupImages.driftCorrect` | Should `driftNormalisation` be `none` or `phase`? | **user** (undecidable) | `params.driftNormalisation` (restored PR #795 after removal PR #785) | `audit_phase1b_history.md` §C3 | Contradiction C3 — no computable signal today; the restoring PR admits "unverified whether `phase` actually helps". |
| `cleanupImages.stackAlign` | `Z`-axis stack present; reference plane picked; a majority of planes survive the confidence gate. | **auto** (axis gate) + **compute** (post-hoc) | `requires.axes: ["Z"]`; `params.alignChannel`; `nPlanesApplied / nPlanesTotal` (advisory, warn < 0.35). | `stack_align.json:9`; `stack_align.jl:10, 24-33` (`STACK_ALIGN_APPLIED_FRAC_WARN`). | The applied-fraction is a **post-hoc reliability metric**, not a precondition. A plan can only know "should I run stackAlign?" from user knowledge (breathing during Z acquisition, `STACK_ALIGN_PLAN.md`) — no `meta.breathing`, no scanner tag. |
| `cleanupImages.flowRegister` | Non-rigid intra-frame deformation (galvo/resonant line-scan warp within one frame). | **user** (would-need-metadata) | none — no metadata field describes intra-frame scan behaviour | 1a inferred assumption 6 (`flowRegister` has no `T`-axis `requires` even though it needs one); `flow_register_run.py:10` (docstring: resonant/galvo scan warp). | Peak flow magnitude vs `maxShiftPx` (`flow_register.jl:20-40`) is a **post-hoc** reliability metric; the trigger for running the task is currently user-only. |
| `cleanupImages.smooth` (spatial) | Photon-limited data — a channel is close to per-frame Poisson floor. | **compute** | `zeroFracIn` per channel (post-hoc, banked; NOT in COHORT_METRICS deliberately). | `smooth.jl:25-46`; docstring at :43-46 explains why it is NOT cohort-comparable — confounded by drift-correction canvas padding. | The "photon-limited" signal DOES exist as a per-channel fraction after running, but there is **no cheap plan-time probe** — cecelia would need to compute a channel-histogram at plan time (analogous to the import-time `saturation_run.py`, which cost ~3 s/GB). |
| `cleanupImages.smooth` | Prerequisite: input has been drift-corrected. | **name-heuristic** (fragile) | `occursin("rift", string(value_name)) \|\| occursin("rift", string(filename))` — WARN-only, does not block. | `smooth.jl:91-94` (1a inferred assumption 6). | Defeated by any rename or bespoke pipeline value_name. |
| `cleanupImages.smooth` (spatial=0 + temporalFrames>1 on photon-limited data) | Do not run this configuration on photon-limited data. | **compute** | Same `zeroFracIn` — WARN-guarded in the handler for the gaussian arm; measured 8.5% vs 15.4% signal kept (`smooth.jl` handler). | `smooth.jl:80-90` region (per catalog 1a row). | Second post-hoc-only signal. |
| `cleanupImages.flip` | User just picked the wrong-side mount. | **user-choice** (no precondition) | — | — | QC-EXEMPT per header (1a). |
| `cleanupImages.dtype` | User storage decision. | **user-choice** (no precondition) | — | — | QC-EXEMPT per header (1a). |
| `cleanupImages.cellposeCorrect` (RETIRED, still readable) | Refuses run (retired). | **hard-error** | `RETIRED_FUN_NAMES` in `app/src/tasks/task.jl:1614-1629` | 1a row + `audit_phase1b_history.md` §2026-08-21 PR #610. | Existing `cpCorrected` stores stay readable via versioned fallback but the writer is gone — see Phase 1d §1 below for the plan-engine implication. |

### Class distribution — plan implications

- **Auto-detectable, on existing metadata** (2 rules): the axis gates (`T` for drift/denoise/smooth-temporal, `Z` for stackAlign — `requires.axes`) and the denoise saturation gate (`meta.saturation.channels[i].saturated`). These are the ONLY two things a Phase 2 rule engine can trigger on without running a probe or asking the user.
- **Computable from pixels — pre-run** (0 rules today): no correction has a pre-run pixel probe. `saturation_run.py` is the closest thing, and it runs at IMPORT, not at plan time.
- **Computable from pixels — post-hoc reliability** (5 metrics): `residualRms` (drift PCC estimators only), `nPlanesApplied/nPlanesTotal` (stackAlign), `flowMax` peak/cap ratio (flowRegister), `zeroFracIn` (smooth), `saturatedFrac` per-channel (afCorrect run-time). Useful as post-hoc score bands; not usable as pre-run triggers unless the task becomes cheap enough to probe.
- **Not derivable** (1 canonical): AF `exclusive` (specimen question) — plus every "did the microscope do X?" question below.

### Signal gaps — things Phase 2 will WANT to trigger on that don't exist today

1. **Scanner type / acquisition mode** — no metadata field. Applicability rule 1 in 1b ("resonance-scanner → smooth is prerequisite for AF", commit `95894bed` / SMOOTHING_PLAN L111) has no field to test. `resonance`/`galvo`/`resonant` appear only in docstrings and code comments (`app/src/model/image.jl:373`, `flow_register_run.py:10`, `smooth_run.py:7`). Nothing in `omezarr.jl`'s `read_ome_metadata` extracts a scan-mode from OME-XML `<DetectorSettings>` or `<Microscope>` even when Bio-Formats has it.
2. **Intra-frame deformation** (`flowRegister` trigger) — no metadata field; no cheap pixel probe. Would need a per-frame local-warp metric.
3. **Breathing / intra-stack Z offset** (`stackAlign` trigger) — no metadata field; the reliability metric (`nPlanesApplied`) only fires AFTER the task runs.
4. **Photon-limited plan-time flag** — `zeroFracIn` exists post-hoc; the plan-time equivalent would be a `sparsity_run.py` mirror of `saturation_run.py` (`~3 s/GB` at that cost).
5. **Filter-set fingerprint** (which pairs bleed) — computable per-image inside `afCorrect` (`af_bleedthrough_alphas`) but not cached anywhere on the image or on the set as a cohort property. A set-level average would let a plan pre-configure `afCombinations` on new images of the same experiment.
6. **Corrected-store staleness marker** — see §1 below. Even the fields that DO exist (e.g. `meta.saturation`) are only written for `default`; nothing writes them for corrected versions.

---

## Phase 1d — extra questions

### 1. Provenance / versioning — corrected stores go silently stale

- **How versions are tagged**: each correction writes to a distinct `filepath[<name>]` entry and re-points `_active` (`docs/OBJECTMODEL.md` L247-263 + `versioned_set_field!`; the version names per task are in 1a's rightmost column). `runlog.json` (`app/src/run_log.jl:6-37`, cap 200) records every run's params. `meta.funParams` (last params flat) and `meta.funParamsByName` (last params per output name) also survive on the image; both are per-task, not per-plan.
- **`resync_ome_meta!` is FILL-ONLY and reads only `default`** (`app/src/tasks/importImages/omezarr.jl:689-707`, docstring L660-687): "adds fields that are genuinely absent and never overwrites one already on disk". So editing acquisition metadata — including the case where a maintainer corrects `PhysicalSizeZ`, `TimeIncrement`, or channel names — **never touches any corrected store's own `.zattrs`** and **never invalidates a stored correction plan** (there is no correction plan today; but even the per-image reliability sidecars in `1/{uid}/qc/{fun}/{value_name}.json` are not touched). It also does not re-run `saturation_run.py`, so `meta.saturation` never changes after import.
- **`meta.saturation` is written for the default store only** (`omezarr.jl:1003`). If a corrected store has a materially different saturation profile (e.g. `dtype`-rescaled), nothing writes back — the denoise gate on a corrected input still reads the default's saturation flags.
- **On corrected versions**: `imChannelNames` for a corrected variant is `nothing` by design — reads fall back to `default` (`channel_names(img; value_name)` in `app/src/model/image.jl:207-228`, `docs/OBJECTMODEL.md` L273-284 documents why). A rename of the default channels therefore RE-BINDS every downstream correction's stored `funParams` that reference the old name, silently.
- **Retired-writer case (`cpCorrected`)** (worked example, from 1a): the store is on disk, `filepath["cpCorrected"]` is in ccid.json, no `imChannelNames["cpCorrected"]`, and `RETIRED_FUN_NAMES` (`task.jl:1614-1629`) refuses any re-run. **Nothing tags the store as retired-writer output.** `SMOOTHING_PLAN.md:228` (quoted in 1a) warns that `2h06xA/cpCorrected` was run with `modelChannels: [0,1,2,3]` and has a flat SHG channel — the store still reads clean, no on-disk flag says the writer is gone or that the result is suspect on low-dynamic-range channels.
- **Plan-engine implication**: any Phase 2 correction plan will need its OWN sidecar (call it `plan.json`), tagged with (a) the versions of the writers it assumed, (b) which upstream `value_name` fed each step, and (c) the input's `meta.saturation` fingerprint at plan time. Without these three, an acquisition-metadata edit or a rename silently keeps the plan valid on paper.

### 2. Conflict resolution today — no runtime engine, so no runtime handling

- **The three prose contradictions** from 1b (§C1 pipeline order between PR #793 vs #798/#802; §C3 `driftNormalisation` remove/restore; §C6/C7 `smoothAfDriftCorrect` composite designed-not-built and its order-invert with `afDriftCorrect`) live in PR bodies and plan docs, not in code. **Nothing in `chain.jl` or `task_registry.jl` enforces or checks correction order**; the sole hard-coded ordering is the JSON array in `af_drift_correct.json`. Composites' preview delegates to the first previewable step (`5131d794`) — silently skips the rest, per 1b §2026-08-01.
- **Inside a single task — one real runtime conflict**: `driftCorrect`'s finding vocabulary is estimator-conditional (`residualRms`/`unreliable` only on PCC estimators; `drift.rotation.capped` only on `sitkRigid`). The code silently picks by `driftEstimator` param and the user is not told which findings CAN fire (1a inferred assumption 4). No error, no prompt — just a different QC vocabulary depending on which of three code paths ran.
- **A second runtime near-conflict**: `denoise` reads `meta.saturation` from the DEFAULT store even when the user selected a corrected `valueName` as input (`denoise.jl:24-93` — read is `img.meta["saturation"]`, not resolved through `value_name`). So on a `dtype`-rescaled or `afCorrected` upstream where saturation is meaningfully different from `default`, the gate can either refuse a run that would be fine, or approve a run that would not. Silent — no warn, no prompt.
- **User-facing handling of the three prose contradictions today**: nothing. The user picks `valueName` per task from a picker that offers whatever versions exist. There is no lint saying "you chose smooth's input as raw, but denoise usually wants drift-corrected first".

### 3. Metric scope

| Metric | Scope | Where scope is set |
|---|---|---|
| `saturated` / `topFrac` / `clippedSignalFrac` (import) | **per-channel** (per image, one row per C index) | `app/src/tasks/importImages/saturation_run.py:51-57` (`channel_histograms(level0, c_idx)`); ccid meta shape `saturation.channels[]` in `omezarr.jl:1003-1005`. |
| `saturatedFrac` (afCorrect QC) | **per-channel, worst over channels** for the metric; **per-channel** for findings. | `af_correct.jl:120-158` (`af_qc_findings`, `worst.saturated`), `:227` writes worst-channel scalar to metrics. |
| `bleedthrough α` (afCorrect QC) | **per ordered channel pair** (target × source); worst over pairs is banked as one cohort metric | `af_correct.jl:141-158` (one finding per (target, source)); worst = `worst.leak`; `python/cecelia/utils/correction_utils.py:1527`. |
| `residualRms`, `residualP90`, `nPairs`, `nRejected` (drift) | **per-image**, over the whole cycle-consistency residual set | `drift_correct.jl:28-37`, `_drift_qc_metrics`; runner side `drift_correct_run.py:185`. |
| `interpolated frames` / `drift.jump` (drift) | **per-frame** (list of `T` indices) | `drift_correct.jl:39-71`. |
| `nPlanesApplied` / `nPlanesTotal` / `appliedFraction` (stackAlign) | **per-image aggregate over per-(t,z)-plane decisions** | `stack_align.jl:18-34`. |
| `peakShiftPx` (stackAlign) | **per-image scalar** collapsed from a per-`(t,z)` field | `stack_align.jl:36-59`. |
| `flowMax`, `flowMean` (flowRegister) | **per-frame** (one scalar per `T`); metrics reduce to per-image max/mean | `flow_register.jl:20-53`. |
| `zeroFracIn` / `zeroFracOut` (smooth) | **per-channel** (`{str(c): float(...)}`) | `smooth_run.py:438-439`; consumer `smooth.jl:25-33`. |
| `meta.saturation.channels[i].saturated` (denoise gate) | **per-channel** binary | `denoise.jl:24-93`. |
| `driftNormalisation` decision | **per-image** (single param) | 1b §C3. |
| `afCombinations` `exclusive` | **per-target-channel** (declared per row) | `af_correct.jl:184-193`. |

Composition implications: any Phase 2 QC score that mixes drift (per-image) with saturation (per-channel) or bleedthrough (per-pair) must state its reduction rule. The precedent set by 1b §PR #462 — "denominator = signal population, not whole tensor" — is per-image; it does not address per-channel vs per-pair composition. New territory.

### 4. Ground truth — none

- `test-data/projects/` has **two fixtures**:
  - `testpr/1/KDIeEm/labelProps/B.h5ad` — a 1377-cell, 4-channel, 20-timepoint `.h5ad` (`test-data/README.md`). NO image store, NO OME-XML, NO known acquisition scanner type. `ccid.json` records `SizeC: 4` and channel names `ch0`..`ch3` — nothing about scanner, sample, saturation, or expected drift.
  - `ZARRFMT/{ZV2img, ZV3img}` — zarr v2 vs v3 format equivalence fixtures; empty of biological signal, no acquisition properties.
- The fixture cap is 1 MB per file, 8 MB total tree (`test-data/README.md`) — a real correction fixture (a few frames of a real 4-channel movie with a known scanner) blows through this.
- **No repo has reference images with verified acquisition properties for correction auto-detection.** Every calibration number in the corpus is a "property of THIS MACHINE's data" (PR #526 quoted in 1b §2026-08-12): the 2 px `residualRms` threshold (18 movies), the 0.35 stack-align applied-fraction floor (`d5vw7z/c91ICQ`), the AF ceiling range (nine `kSUFux` movies), the SUPPORT saturation gate (`2h06xA` vs `c91ICQ`).
- **Consequence for Phase 2**: every QC score band and every applicability-rule threshold in the plan is defensible only as "measured on Dominik's dev data". Ranges cannot be validated against a labelled reference set. Any `AcquisitionPreset` card the design proposes (galvo / resonant / spinning-disk / …) needs the maintainer to attest that a representative image exists in `dev/`; there is no fixture to test the card-classifier against.

---

## Newly-discovered contradictions

Beyond 1b §C1–C7:

- **C8 — Denoise saturation gate reads DEFAULT meta regardless of chosen input version.** `denoise.jl:24-93` reads `img.meta["saturation"]` — not versioned. If the user selects `valueName = "dtype"` or `"afCorrected"`, the gate still uses `default`'s saturation profile. Silent mismatch, no warn.
- **C9 — Saturation detection mechanism disagrees with the plan doc.** DENOISE_INTEGRATION_PLAN.md D6 (quoted in 1b §2026-09-05 PR #796) states "threshold ≥ `dtype_max * 0.98`". The shipped detector is `intensity_utils.is_saturated`: a structural pile-up count in the brightest occupied bin (`saturation_run.py:8-11` docstring: "structural anyway: it needs no metadata"). The plan's `0.98 * dtype_max` rationale doesn't match what runs; both may agree on obvious cases and diverge on 12-bit-in-16-bit sensors — which is exactly the case the docstring names.
- **C10 — Composite `afDriftCorrect` and the `run_log`/`funParamsByName` do not align on what "an output" is.** `funParamsByName` (`docs/OBJECTMODEL.md` L213-244) is keyed by output name (`afCorrected`, `driftCorrected`), but a composite writes BOTH intermediate versions en route (`afCorrected` lands, then `driftCorrected` overwrites `_active`). The banked per-name params are the composite's params for each; the per-step params are only in `runlog.json`. A Phase 2 plan-engine that assumes "one output name = one recipe" will mis-attribute the intermediate.

---

## Biggest signal gaps (plan-relevant TL;DR)

1. **No scanner-type metadata field** — the loudest applicability rule in the corpus (resonance → smooth-before-AF) has nothing structural to trigger on.
2. **No plan-time pixel probe** — every "compute" precondition is post-hoc, ran INSIDE the task. Import-time `saturation_run.py` is the only cheap probe today, and it feeds one gate.
3. **No `meta.acquisitionMode` / `meta.filterSet` / `meta.expectedDrift` fields** — the entire preset-card design (Phase 2.5) has no structural inputs; it can only ask the user or infer from the same post-hoc metrics.
4. **Corrected stores carry no self-tag for writer version or upstream input** — retired-writer output stays readable and silent; a metadata edit never invalidates a downstream plan; a channel rename silently re-binds saved params.
5. **Post-hoc reliability metrics are estimator-conditional** (drift) or confounded by upstream corrections (smooth's `zeroFracIn` by drift-padding) — a QC score band that treats them as image-invariant will be wrong for at least these two.

Delivered: `docs/archive/audit_phase1cd_qc_and_provenance.md`.
