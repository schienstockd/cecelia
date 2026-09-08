# SUPPORT — per-channel training escape hatch + low-SNR precheck

**Status:** shipped as manual toggle (2026-09-08); `auto` deferred (see below). Worktree
`cecelia-support-audit`, branch `audit/support-denoise`.

**Origin:** 2026-09-07 audit of `supp.MERTK` on `x4E5HU` (project `zolIMa`). Pooled model handles
nuc-GFP (raw ~0–88) and mem-TOM (~0–126) cleanly; CD169-Kat (~0–12) collapses — the network is
doing the right thing under the shared prior, and CD169-Kat's real structure sits at ~2–3σ of shot
noise. Amends [`DENOISE_INTEGRATION_PLAN.md`](DENOISE_INTEGRATION_PLAN.md) D3 (2026-09-05 pool-
channels amendment): pooling stays the default, but a weak channel needs its own model.

## Goal

Keep the "user picks channels once, one training run" ergonomics — but under the hood, when a
channel is too weak to survive the shared prior, train a separate model for it. Same user-facing
model name, same inference command. Warn (and default to auto-switching) when the precheck sees
a channel that would collapse.

## Locked decisions

### D1 — `trainMode: pooled | perChannel`, default `pooled`

New param on `opticalFlow.trainSupportDenoise`. `auto` was in the original design but is deferred
(see below) — the reliable signal is post-run collapse QC, not a training-time precheck.
- `pooled` (default): today's behaviour, one model over the pooled channels.
- `perChannel`: one model per channel — user opts in after the QC flags a collapsed channel, or
  when they already know a channel is weak.

Rejected: (a) `perChannel` default — makes the common good-SNR case ~N× slower for no reason;
(b) hiding the knob and doing auto silently only — power users need to override when the
precheck disagrees with what they can see in the movie.

### D2 — Bundle on disk: folder mirrors the single-file name

Pooled model stays a single `<config_dir>/models/denoiseModels/<name>.pt` + `<name>.json` (no
migration). Per-channel model writes a **folder** `<config_dir>/models/denoiseModels/<name>/`
containing:

```
<name>/
  manifest.json          # top-level: mode="perChannel", channels: [...], per-channel training stats
  <channelSlug>.pt       # one .pt per trained channel, filename == slug of channel name
  <channelSlug>.json     # per-channel arch + loss curve, same schema as the pooled manifest
```

Rejected: (a) a single `.pt` with per-channel weight blobs — awkward for `torch.save` and hides
the fact that these are independent networks; (b) suffixing files (`<name>.<channel>.pt`) —
`list_*_models` would need to reassemble bundles from filename patterns, fragile.

### D3 — Resolver in `cleanupImages.denoise` is by path type

The Julia handler resolves `modelPath`. If it points at a `.pt` file → hand it to the Python
runner unchanged (pooled). If it points at a **folder** → read `manifest.json`, and for each
requested channel look up the sub-model, then hand the runner `{modelPaths: {channelIdx →
subPath}, manifests: {...}}`. A requested channel not in the bundle is a **hard error** with
the actionable message (which channels the bundle covers, offer retraining with the missing one).

Rejected: (a) auto-training a missing channel at inference time — hides real user intent, mixes
train and inference tasks; (b) falling back to a pooled model — silent divergence from what
the user thought they were running.

### D4 — Two-sided low-SNR detection: cheap precheck + reliable post-run QC

**Design choice 2026-09-07 (Option C):** DR alone is the wrong precheck — the ground-truth
collapse case (`x4E5HU`, CD169-Kat) has raw DR ≈ 235 driven by a handful of bright pixels,
while typical signal sits in the shot-noise floor. Two checks together, cheap + reliable:

**(a) Pre-training precheck — Poisson SNR of typical signal.** Runs at the start of
`trainSupportDenoise` on the same frames the trainer is about to consume (post `midZOnly`
slicing). Per selected channel, on raw counts pooled across images:

- `p50`, `p99` on the whole channel.
- **SNR proxy** = `(p99 − p50) / sqrt(max(p50, 1))` — signal head-room above the median
  floor, in units of the shot-noise σ at the floor.

**Starting threshold (tunable — locked by Phase 0):** `SNR < 3` fails a channel.

**(b) Post-run collapse QC — within-run ratio.** Runs at the end of `cleanupImages.denoise`
(pooled runs only; skipped for `perChannel` bundles since each channel has its own model).
For each channel: `collapseRatio = outMax / inMax` (already in the sidecar). Compare the
lowest ratio in the run against the median of the others:

- If `min_ratio < 0.5 * median_of_others` → bank `warn` finding *"CD169-Kat range collapsed
  Nx more than the pooled cohort — consider retraining with `trainMode: perChannel`"*.

No calibration needed — within-run comparison is self-normalising. This is the reliable
backstop; it fires even when the precheck missed the case.

**Auto-switch policy:** parked with the precheck (see Deferred). Effective flow today:
- `pooled` → post-run collapse QC (D4(b)) fires if a channel collapses → user retrains manually
  with `perChannel`.
- `perChannel` → post-run collapse QC skipped (per-channel bundle trains each channel
  independently, so a within-run cohort ratio is meaningless).

### D5 — No engine change

`coastal/support/_support.py` and `_dataset.py` are untouched. `train_support` already trains
on a single channel-worth of `vols` at a time — `perChannel` is just calling it N times with
the per-channel slice of `vols`. Same for inference: `denoise_stack` already runs one channel
through one model. The whole change lives in the two `_run.py` callers + the two `.jl`
handlers + one JSON spec + the docs.

## Validation

**2026-09-08 — perChannel `supp.small` on `x4E5HU` (visual, per `CLAUDE.md` → *Real-data visual
validation*):** nuc-GFP and mem-TOM as clean as the pooled run; CD169-Kat improved to the input's
own ceiling (channel is genuinely photon-limited on this movie — the residual dimness is a signal
ceiling, not a network one). Chapter closed here.

## Phases

### Phase 0 — calibrate the precheck threshold (before writing the switch)

Done in-flight 2026-09-07: sidecar for `supp.MERTK` on `x4E5HU` gives `outMax/inMax` per
channel = 0.39 (nuc-GFP), 0.36 (mem-TOM), **0.13 (CD169-Kat)**. Collapse ratio confirms
CD169-Kat is 3× worse than the pooled cohort — the post-run QC in D4(b) fires on this data
by construction.

The precheck threshold (`SNR < 3`) is still a placeholder — pin it after the first pooled
run of the new pipeline computes p50/p99 for each channel on the MERTK set. If the numbers
say a different cutoff, edit this line and D4(a) in the same PR.

### Phase 1 — precheck helper + tests

- New pure helper `_support_snr_precheck(vols_by_channel) -> {channel: {dr, snr, verdict}}` in
  a testable spot (Python side — computed where the volumes are opened).
- Unit test: golden values on a synthetic photon-limited channel (should fail) vs a
  well-illuminated channel (should pass), matching the numbers from Phase 0.
- QC finding string tested per convention (finding_short_len, action_starts_with_verb).

### Phase 2 — `perChannel` training path

- Add `trainMode` to `train_support_denoise.jl` param spec (default `auto`), tooltip.
- Julia branch: when the runner reports "auto → perChannel" back, write the bundle at
  `<name>/`; when `pooled`, keep the flat file.
- Python `train_support_denoise_run.py`: run precheck → resolve effective mode → loop
  `train_support` per channel (perChannel) or once (pooled) → return the effective mode + QC
  numbers.
- Test: fresh unit test for the run-loop param contract; the existing `_support_train_qc_findings`
  suite is extended with the low-SNR finding shape.

### Phase 3 — bundle-aware inference

- `denoise.jl`: `is_bundle(modelPath) = isdir(modelPath)`; when true, load manifest, split
  requested channels into (sub-path, sub-manifest) pairs.
- `denoise_run.py`: accepts `{modelPaths: {channelIdx: path}, manifests: {channelIdx: obj}}`
  in addition to the current pooled `{modelPath, manifest, channels}`. Loops per channel.
- `list_denoise_models`: reports bundle vs file, and for a bundle reports the covered channel
  list (the frontend can then grey-out inference channels not in the bundle).

### Phase 4 — docs sweep + inventory

- Amend [`DENOISE_INTEGRATION_PLAN.md`](DENOISE_INTEGRATION_PLAN.md) D3 with a 2026-09-07 note
  pointing here.
- Add one line to `docs/inventory/JULIA_APP.md` (task spec convention) if `trainMode` needs
  it; probably not — it's just another param.
- Update the vault-management page docs if `list_denoise_models` output shape changes.

## Deferred — `trainMode = auto` and its precheck metric

**Status:** `auto` chip removed 2026-09-08; the precheck helper (`channel_snr_proxy`,
`_resolve_mode`, `SNR_FAIL_THRESHOLD`) and its unit tests stay in
`train_support_denoise_run.py`. Not called at runtime.

**Why v1 was pulled.** The Poisson head-room proxy (`(p99 − p50) / √max(p50, 1)` on the
nonzero-median floor) can't distinguish "sparse and weak" from "narrow-DR but clean". Measured
on fXgbTl + x4E5HU (2026-09-08):

| channel   | nonzero-p50 | p99 | worstSnr | collapsed? |
|-----------|-------------|-----|----------|------------|
| nuc-GFP   | 28          | 34  | **0.94** | no (0.39 out/in) |
| mem-TOM   | 34          | 74  | 5.92     | no (0.36 out/in) |
| CD169-Kat | 31          | 36  | **0.36** | **yes (0.13 out/in)** |

Both nuc-GFP and CD169-Kat fail at threshold 3.0 — but nuc-GFP renders cleanly under a pooled
prior. Auto would over-fire perChannel training on any normal intravital cohort with one
narrow-DR fluorescent channel, for ~N× training time and no benefit.

**Backstop.** Post-run collapse QC in `cleanupImages.denoise` (D4(b)) is the reliable signal — it
fires on CD169-Kat (0.13 vs 0.36/0.39) and does not fire on nuc-GFP. That path is untouched.

**What metric v2 needs.**
- A signal-vs-background axis (candidate: sparsity × p99, or nonzero-p99 vs total-p50), not raw
  head-room.
- Calibration against ≥ 2 cohorts with labelled ground truth (collapsed vs clean), not a single
  session — a threshold pinned to x4E5HU alone will just overfit.
- Validation against the post-run collapse QC as ground truth (that call is already reliable).

Land any v2 metric behind `_resolve_mode` — the tests already pin its contract — and add the
`auto` chip back to the JSON in the same PR.

## Out of scope (do not build here)

- **Reusing a per-channel model across bundles / a shared per-channel vault.** A tempting
  optimisation; the current one-model-per-set premise (D3 in DENOISE_INTEGRATION_PLAN) covers
  it. If someone wants to reuse a CD169-Kat model, they retrain the bundle with the same
  training set — cheap because engines stay unchanged.
- **Attribution UI for the switch** — same reasoning as D5 in DENOISE_INTEGRATION_PLAN.
- **Auto-suggesting an acquisition change** ("your CD169-Kat channel is photon-starved, dial
  the PMT up"). Different feature.

## Cross-references

- Original pooling decision: [`DENOISE_INTEGRATION_PLAN.md`](DENOISE_INTEGRATION_PLAN.md) → D3
  (2026-09-05 amendment).
- Prior collapse observation: 2026-09-07 audit fork, `cecelia-support-audit` worktree.
- Engine paths: `app/src/tasks/opticalFlow/train_support_denoise_run.py` (calls into
  `coastal.support.train_support`), `app/src/tasks/cleanupImages/denoise_run.py` (calls into
  `coastal.support.denoise_stack`). Both live upstream in the sibling `coastal-support-migration`
  repo — no changes expected there.
