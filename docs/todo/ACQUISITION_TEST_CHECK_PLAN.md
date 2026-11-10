# Test-acquisition check — plan

**Status:** **IN PROGRESS** (2026-11-09). Measures built (#2072); slice 1 — rules, storage, API, set modal — built
(*Slice 1 built*, below), awaiting the maintainer's visual pass; slice 2 — one review turn — built, not yet run on a real model. Written from one set used as a stand-in (below).
Part of [`ACQUISITION_ADVISOR_PLAN.md`](ACQUISITION_ADVISOR_PLAN.md) — that plan's Decision 1/2 ("test
acquisition imported into the user's project") given a concrete shape. Reuses the mechanism of
[`IMAGE_ANALYSIS_RECITAL_PLAN.md`](IMAGE_ANALYSIS_RECITAL_PLAN.md).

## Goal

Before the user processes anything, and before they acquire the rest of the experiment:

> "Here is my image. I'm planning to track the uptake of beads by B cells. Is that OK, or should I
> change anything before acquiring the other images?"

The answer is a short list. Each item names a problem in this image that gets in the way of that
plan, the change to make at the microscope, and what the change costs. Changes for the next
acquisitions come first. What it means for this image comes second.

## Route — the recital mechanism, with a plan instead of a claim (the maintainer, 2026-11-09)

The plan sentence plays the role a claim plays in recital: it scopes what matters.

| Recital | Test-acquisition check |
|---|---|
| Claim: "T faster than B" | Plan: "track bead uptake by B cells" |
| Translator (parked there) → populations, measure, replicate | **Needed here**: plan → objects (B cells, beads), the channel for each, the event (uptake), context channels. The user confirms |
| Checks: record gaps, cohort consistency, replicate floors | Checks: the six below, read per channel against the plan |
| Review: one Kiwi turn, advisory, Good/Bad/Unsure | Review: one Kiwi turn reading `get_acquisition_knowledge`; "consistent with" until the cause is separated |
| A Blackboard entry, re-checked each session | Same: the plan is an entry, re-checked when the next test image lands |
| Popover: facts grouped by category, "what, where → what to do" | Same layout |

**Before analysis vs. after (the maintainer, 2026-11-09).** This plan covers *before*: a test image and a
plan, and advice for the next acquisitions. Asking which acquisition problems affect a claimed result is
*after*, and belongs to claim recital: a recital check that reads the import findings on the channels the
claim uses. Don't build a per-set "affects your analysis" report here.

**Not the import QC badge.** The badge reports everything on every image. Every m5uLPh image is flagged,
and the most common warning is on a channel the plan doesn't use.

## The checks (the maintainer, 2026-11-09)

All six run at import, in seconds. Anything that needs the user's pipeline has to wait for that pipeline to be chosen.

| Check | What it measures | Exists today |
|---|---|---|
| **Noise** | Signal over noise *on the objects* | `noise.snr` (P4b-1) has the wrong denominator for this (below) |
| **Dynamic range on the objects** | How much of the detector range the cells and beads use, against what sets the ceiling | not built; `range` and `hist` are over all non-zero voxels, mostly background in a sparse movie |
| **Bleed-through** | Signal from one channel in another | not at import. A rough version can use existing pieces: `coloc_utils.envelope_slope` (what AF correction uses to tell leak from autofluorescence; `tls_slope` beside it) and the paired sampling in `correction_utils.af_bleedthrough_alphas`, applied to the bead channel against GFP and collagen at non-bead voxels. AF correction's `maxBleedthrough` comes only after processing |
| **Signal fading** | Loss over the movie per channel | `import.signal_fades` (`byT`) |
| **Drift** | Sample movement between frames | only after drift correction runs. At import: a shift estimate between a few frames (drift correction's estimator on a subset), not built |
| **Uneven z intensity** | Signal change with depth | `import.brighter_with_depth` / `darker_with_depth` / `clipped_deep` (`byZ`) |

Read every check per channel against the plan. Tracked and measured channels count. A context channel
(collagen here) gets one line: "not used by your plan".

**No pilot (the maintainer, 2026-11-09).** A pilot is a chain run on a crop, and the user or Claude can
already do that with the existing crop task and chains. Nothing to build here. Advisor P6's mapping from
task QC codes to stages still applies if someone does run one.

## Stand-in case — Immunemap XbeT2l, set m5uLPh (Obino 2018, Exp 63)

These are already-acquired public movies, treated as if one were the test image. Channels:
`Microspheres (Rhodamine)`, `B cells (GFP)`, `Collagen (Autofluorescent)`.

**The maintainer's diagnosis (2026-11-09), written before any check exists:** the main problems are
**noise**, **low dynamic range on the actual cells and beads**, and **bleed-through between channels**.
This is the expected answer the first slice is scored against (advisor P7). For now the public set is
the test bed (the maintainer, 2026-11-09). Score on several of its movies, not one. One judge is a known
limit; a second (facility staff) and the lab's own bad acquisitions come later.

### Settings (re-imported 2026-11-09 with the `.ims` reader, #2064: J1ha0v, JW7Qr0, JIThPI, niHekF)

- Zeiss LSM 510/710, two-photon, **one 890 nm laser for all three channels**. Emission filters are
  BP 612/69 (beads), BP 520/35 (GFP) and BP 472/30 (collagen). So the channels are separated only by
  emission filters, and excitation can't be done sequentially with one line.
- Not recorded: laser power, detector HV, gain, offset. Advice can give a direction, not a value.
- 9 planes at 4 µm, 30 s frames, 12-bit.
- **J1ha0v used a different objective** (Plan-Neofluar 10x/0.3, 1.8 µm pixels) from the other three
  (Achroplan 40x/0.8 W, 0.45 µm). For a test image this becomes a consistency check: acquire the rest
  with the same objective and zoom.

### Numbers from the import probe (`meta.saturation.channels`, the 3 Achroplan images)

| Channel | Background median / spread | p50 non-zero | Ceiling 4095 hit | Voxels in the bottom 6 % of range (0–255) | `snr` |
|---|---|---|---|---|---|
| Beads | 56 / 9–11 | 60–62 | yes, 0.1–2.7 % of signal | 74–79 % | 39–42 |
| B cells (GFP) | 117–118 / 11–12 | 133–159 | yes, 0.9–1.3 % | 57–62 % | 45–51 |
| Collagen | 142–143 / 11 | 147 | yes, 1.4–1.9 % | 72–75 % | 61–63 |

What this does and doesn't show (reasoning; review on PR #2066):
- **These numbers don't show low range on the objects.** The movies are sparse, so the median non-zero
  voxel and the bottom-6 % share are mostly background (beads: median 60–62 against a background of 56).
  A few clipped bright objects can also mean correctly exposed sparse objects. It needs an object mask
  first.
- **The noise number has the wrong denominator.** `snr` = (median of voxels above `bgLevel` − background
  median) / background spread (`intensity_utils.noise_stats`). The numerator does describe the typical
  above-background voxel, but the spread is background-only, and its own docstring says this understates
  the Poisson noise on brighter structure. Noise has to be measured at signal level, e.g. variance against
  mean (photon transfer) or single-image gain estimation (Heintzmann 2016, `ACQUISITION_ADVISOR_PRIOR_ART.md`).
  Validate on synthetic Poisson–Gaussian images with a known gain (golden values) as well as against the
  maintainer's call.
- **Bleed-through matters most for this plan.** Uptake means bead signal inside B cells. RSQxFI's AF
  correction found 41 % collagen and 10 % GFP signal in the bead channel; that makes false beads inside
  cells and inflates uptake.

### Next steps the check should be able to give (to confirm with the maintainer)

| Finding | Next acquisition | Cost / caveat |
|---|---|---|
| Low range on cells; a few bright objects clip | If the clipping objects are beads, raise power or HV for the cells and let the beads clip. A clipped bead still has a centre, so tracking and object counts survive | Uptake measured by bead intensity is lost; count bead objects |
| Noise on dim cells | Frame or line averaging, or more power | Longer frames (30 s now) or more bleaching; check fading after |
| Bleed-through, one excitation line | Narrower emission filter on the receiving channel, or a second excitation wavelength for one dye | Hardware-dependent; otherwise correct afterwards (AF correction) |
| Fading over the movie | If it's bleaching: less power on that dye, fewer planes or a longer interval | Bead counts per frame first: beads leaving or being taken up look the same in intensity |
| Drift | Stabilise the preparation; a larger z-range as margin | Correctable afterwards up to a point; unregistered frames break tracks |
| Uneven z intensity | Depth power ramp, if the scope has one | Beads collecting at a depth looks the same; "consistent with" only |
| Different objective in the set | Use the test image's objective and zoom for the rest | — |

## Open decisions

1. **Plan → structure: a small form** (proposed on PR #2066 review): objects, channel per object, event;
   the sentence is shown back, not parsed. Not a config file.
2. **Rules vs. review.** "Let beads clip if you only track and count them" is a rule once the plan says
   beads are counted objects. "Brighter with depth is bead placement" is a judgement for the review.
3. **Where it lives.** A step in the import flow ("what will you do with this?"), or an action on an
   imported image. The metadata dialog's light path stays the echo-back.
4. **Finding-text fixes — done (branch `fix/import-finding-text`):** clipping at an 8-bit file's 255 with
   no 8-bit detector on record points at the original file, not the gain (`_clipped_at_export_ceiling`);
   the depth findings name Bright Z only when `.oir` settings record it ON (`_depth_ramp_key`), else
   generic text.

## Built so far (2026-11-09, branch `feat/acq-test-checks`)

The import probe gained a sampled second pass, `data_path.measure_signal` (4 timepoints × up to 16
planes; ~16 s per 1.4 GB movie including drift). It writes `meta.saturation.channels[i].photons`,
`meta.saturation.sharedStructure` and `meta.saturation.drift`; `get_image_info` shows them in
`cleanupFacts` (`photons`, `sharedStructure`, `importDrift`). Refresh metadata backfills older images
(`photons` is in `SATURATION_CURRENT_KEYS`).

- **Noise and range, one measure: photons** (`photon_utils`). Grey levels per photon come from the
  image's own variance against its mean (after Foi 2008), so noise is read at signal level and range is
  counted in photons. Golden-tested on synthetic Poisson–Gaussian images (gain 1–300, within 10 %).
  Independent real check: on a photon-counted FV4000 movie, the histogram's own photon step reads
  26–32 grey levels per photon and the fit reads 30–35.
- **m5uLPh (all 15 movies):** ~600 / ~900 / ~1350 grey levels per photon (beads / GFP / collagen),
  fit R² ≥ 0.99, the same in every movie. The 12-bit ceiling holds **3–7 photons**; objects carry a
  median of **~1 photon** per voxel (p99 ~3). Noise, low range and clipping are one cause: detector
  gain far too high for the light collected. The old `snr` (39–75) counted single-photon spikes above
  the triangle level as signal.
- **Bleed-through: not identifiable from one image; shared structure instead.** Every channel pair
  shares structure (Pearson r 0.17–0.71 on band-passed planes, 15 movies), and both leak directions fit equally well
  (one 890 nm line excites all three). The envelope and TLS slopes swung 0.13–0.93 on RSQxFI with the
  threshold and estimator. What does show it: the steady-state movies (no beads injected) have B-cell
  shapes and granules in the bead channel. So the check reports shared structure per pair and, for an
  uptake plan, asks for a no-bead control — this set already has one.
- **Shared structure is correlated on band-passed planes** (cell-scale: σ1.5 smooth minus σ25 background,
  each plane centred). Pooled raw, two independent channels that both dim with depth read r 0.99
  (`test_signal_pass_shared_structure.py`); centred only, a shared vignette alone read r 0.97
  (`test_signal_pass_vignette.py`).
- **Second case, RFtn9f set lfGUDg** (naive P14 / OT-I / gBT, two label designs, FVMPE-RS; the
  maintainer's own data): the two designs were acquired with different lasers and with Bright Z on (Nov)
  vs off (Dec, signal −3 to −28 % with depth); reporters differ up to 4× in photons, OT-I dimmest in 6 of
  9. uGFP shows in 570–616: band-passed r 0.34–0.86 in the CTDR design (7 movies), where that channel
  holds no label; in the ubTomato design it is the P14 channel and r splits 0.32–0.51 (jcRERt, TKufLV,
  7b1jqm, VNvvA8) vs 0.09–0.16 (the other 5). The GUI
  moved to a set-level modal (mockup kept outside the repo); the image metadata modal links to it.
- **Drift:** drift correction's estimator on the whole movie (10–14 s), a stopped acquisition's last
  frame left out. RSQxFI is the only one whose
  own measurements disagree (3.7 px RMS, over the 2 px limit); several others move 40–57 px in X while
  registering cleanly.

## Slice 1 built (2026-11-09, branch `feat/acq-set-check`, stacked on #2072)

- **Rules** `app/src/acq_check.jl`, pure over facts read from image meta: settings that differ (within a design,
  and between designs), Bright Z off with a saved ramp, a detector
  range under 20 photons, photon-starved objects (median p50 < 3; points exempt), reporter balance
  (p99 ratio ≥ 2), an object's channel sharing structure with another object's (warn) or a reference
  (info) (r ≥ 0.5), drift that doesn't register (drift correction's 2 px), movies not measured. All
  thresholds are named `ACQ_*` placeholders. Without a plan only the plan-free checks run.
- **Settings** are read within a design (movies compared directly — a warning naming the movies per
  setting) and between designs (info when every compared object is in each design, else a warning).
- **Depth is settings-only:** Bright Z off with a ramp saved is a warning naming the ramp; off with none
  is a note. Whole-plane p99 per Z mixed cell count with brightness on sparse cells, so it was dropped;
  whether real cells dim with depth is a post-segmentation check (claim recital side), not the advisor's.
- **Measure N movies** runs the probe as background tasks (`importImages.measureSignal`, io pool) —
  `POST /api/images/meta/resync` with `measure: true` probes inside the request, fine for one image only.
- **Label designs** are the movies that share a channel-name list, in channel order — never read off file
  names. Default names (`CH1`…) put every movie in one design and raise one finding asking for names.
  The plan picks each object's channel by name, per design.
- **Storage**: the set's plan is a Blackboard entry carrying `acqCheck` meta (a passthrough, like
  `claim`); saving a changed plan is a new round on it. `GET /api/acqcheck` runs the check live and
  writes nothing; `POST /api/acqcheck/plan` saves and records.
- **GUI**: an "Acquisition" button in the image panel's action bar (open-warning count) opens the set
  modal — plan echo + form, across the set, per movie grouped by design; a row shows its findings, the
  name opens its metadata. The image metadata dialog has one line linking to it. Needs the
  maintainer's visual pass.
- **On lfGUDg** (16 movies, read-only; channel names as set by the maintainer — `P14-CTDR · AF · OTI-uGFP ·
  gBT-CTV`, 7 movies; `AF · P14-ubiTom · gBT-uGFP · OTI-CTV`, 9 — probe numbers from #2072 overlaid where not
  yet stored): laser settings differ within the ubTomato design (8/15 % on 4 movies, 10/10 % on 5);
  lasers differ between the designs (info: every compared cell type is in each design); Bright Z off with
  the saved 12→15 % ramp in all 7 CTDR movies; P14 / OT-I / gBT median 1.6 / 1.6 / 2.1 photons per voxel;
  objects up to 4.2× apart, OT-I dimmest in 10 of 16; OT-I ~ gBT and P14 ~ gBT shared structure in one movie
  each; OT-I ~ AF in 6, P14 ~ AF in 9; all register.
- **Not in this slice**: the Kiwi review turn; the briefing's open acquisition checks.

## Slice 2 built (2026-11-09, branch `feat/acq-check-review`, stacked on slice 1)

- **One review turn**, the claim review's mechanism (`api/src/acq_review.jl`; the pieces both share moved
  to `api/src/review_turn.jl`): "Review" in the set modal → `POST /api/acqcheck/review` → one Kiwi turn on
  the saved plan, told the rule findings so it doesn't repeat them. Reads the record and
  `get_acquisition_knowledge` only (`ACQ_REVIEW_TOOLS`); notes, Blackboard, captures, briefing, ELN, lab
  log and the analysis record are withheld. Points file under the check's own categories (`ACQ_CATEGORIES`,
  plus `fading` — no rule reads it yet) as `### sNN` sections on the set's entry, marked Good / Bad /
  Unsure there; a new plan round drops the review. Run on lfGUDg (2026-11-10); scoring on m5uLPh against
  the diagnosis above is still to do.
- **The review closes with a verdict on the plan as written** (the maintainer, 2026-11-10): "Ready to
  acquire as planned." / "Change before acquiring: …", weighing the rule findings too, heading *Before
  acquiring the rest*. The first run's "what these movies would support" (the claim review's closing
  question) narrowed the plan, ignored the rule findings and leaned on a Mouse attribute the set lacks.
  Info findings go to the model as "expected, not a problem" — the next run acted on one.
- **The verdict's rule part is the app's** (the maintainer, 2026-11-10): two runs ignored "start from the
  rule warnings", so `Cecelia.acq_verdict` writes it — one step per warning code per stage
  (`ACQ_VERDICT_STEPS`: "Denoise OTI, P14 and gBT; correct bleed-through between …; drift-correct before
  tracking.") — shown even without a review. The model is shown that sentence and adds ONE step it
  doesn't cover ("Model adds: …"), or "Nothing to add.".
- **Stage: test acquisition or final data** (the maintainer, 2026-11-10 — "I have no more samples, what
  should I watch for when I analyse these"): a toggle on the plan (`plan.stage`, default `test`; the
  record can't tell whether the experiment is finished). On final data each finding leads with its
  analysis action (its `.data` catalog line; a finding without one has a `long` that holds at either
  stage), the review asks what the analysis must do and never suggests the scope, and the verdict is
  "Analyse as planned." / "Before comparing: …" — or, when the movies can't answer the plan, "These
  movies can answer: …" (the narrowed question fits finished data). Heading *Before analysing*.

## Thresholds: relative to the set, not fixed (the maintainer, 2026-11-10)

The `ACQ_*` constants in `app/src/acq_check.jl` were read off m5uLPh and lfGUDg by eye. Rather than
calibrate them as fixed cuts (which would take many sets with a known verdict), the question is asked
inside the set or comes from physics:

| Check | Now | Replacement |
|---|---|---|
| Shared structure | r ≥ 0.5 (`ACQ_SHARED_R`) | the label's r with the reference/empty channel against the set's own baseline — the other labels' r with it: "this label tracks AF more than your other labels do" |
| Balance | p99 ratio ≥ 2 (`ACQ_BALANCE_RATIO`) | only when the plan compares those objects by intensity, or when the dim one is short of photons to segment — the ratio alone is not a problem |
| Few photons | median < 3 (`ACQ_FEW_PHOTONS`) | shot-noise SNR (√photons), stated with what it means for segmentation — graded, not pass/fail |
| Gain ceiling | < 20 photons (`ACQ_CEILING_PHOTONS`) | the photon levels the range holds — graded the same way |
| Drift, fading, settings | partly relative already | a movie out of line with the set's other movies, not past an absolute value |

`ACQ_REL_TOLERANCE` (pixel size 1 %) is float tolerance and stays.

Movie-against-set checks reuse `_cohort_outliers` (`app/src/qc_cohort.jl`, robust z, already reused by
`qc_gating.jl`) rather than a new outlier test. It needs ≥ 3 values, so the label-against-label baseline
within one movie (2–4 labels) is a plain comparison, not that test.

The verdict still needs a cut — the rule part is written by the app (the model ignored the warnings
twice). The cut is soft: the value is always shown, and three bands decide its place — fine, borderline
(an info note), a problem (enters the verdict) — set relative to the set wherever the check allows.
More sets with the maintainer's verdict then confirm the bands; they don't have to find the numbers.

**Built (2026-11-10, branch `feat/acq-relative-bands`).** `ACQ_SNR_BANDS` (problem < 1.5, borderline
< 2.5), `ACQ_HEADROOM_BANDS` (< 2×, < 4×), `ACQ_SHARED_EXCESS` (0.25, 0.1 above the movie's median
pair; needs three pairs, else `acq.shares_no_baseline`), `ACQ_SHARED_BASELINE_HIGH` 0.4
(`acq.shares_all`). Balance fires only when the dimmest compared object sits in a worse SNR band than
the brightest. New per-movie notes: `acq.dim_movie` (an object dimmer than in its design's other
movies) and `acq.drift_far`. The table's *Shared r* shows the highest pair · the movie's baseline.
On real data:
- **m5uLPh** (B cells, beads as points, collagen as reference): warnings for headroom (bright
  objects within 1.3× of a ~5-photon range) and B cells at SNR 1.0, the maintainer's noise + low
  range; every channel shares structure in 12 movies (r ~0.48), a note asking for a control rather
  than a guessed leak.
- **lfGUDg** (his final-data plan): every object at SNR 1.3–1.5 (warn), headroom 2.7× (note);
  OTI ~ gBT (uGFP next to CTV) stands out of its movie's baseline in 11 of 16 movies (warn in 6),
  the one bleed-through step in the verdict.

## Resume when

The measures exist; what is left is the slice, which needs the maintainer first:
1. **His look at the photon numbers** — does "~1 photon per voxel on cells, ceiling at 3–7 photons"
   match what he sees in these movies?
2. **Finding text and levels** for few photons, shared structure and drift — now the relative bands
   above, not fixed cuts.
3. Then the form + these checks + one review turn, scored on several m5uLPh movies against the
   diagnosis above.

Superseded build order (kept for the record):

In this order:
1. **Object mask at import:** above-background voxels per channel. Everything below depends on it.
2. **Bleed-through proxy:** bead channel against GFP and collagen at non-bead voxels, from
   `coloc_utils`. Compare it with AF correction's `maxBleedthrough` on RSQxFI once.
3. **Range on the objects:** mask voxels, clipped ones excluded, against the ceiling.
4. **Noise at signal level**, golden-tested on synthetic Poisson–Gaussian images, then checked against
   the maintainer's call.
5. **Drift shift estimate** on a few frames.
6. Then the smallest slice: the form + these checks + one review turn, scored on several m5uLPh movies
   against the diagnosis above.
