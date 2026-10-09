# Cleanup facts — show what was measured, drop the plan recommender

**Status:** built (2026-10-09) — P0 answered, P1–P3 built; open checkpoint: P2's look at one
photon-limited and one bright movie. Was gated on the night guide run (intravital, `--discovery on`
vs `off`), which ran 2026-10-09. Replaces the recommending half of
[`CORRECTION_QC_PLAN.md`](CORRECTION_QC_PLAN.md) (cards, wizard, plan.json, mount, order buckets). Its
measuring half (Q-M4 sparsity probes, `meta.saturation`) stays. Related:
[`TASK_DISCOVERY_PLAN.md`](TASK_DISCOVERY_PLAN.md) (task text, `--discovery` arm),
[`TASK_PREVIEW_PLAN.md`](TASK_PREVIEW_PLAN.md) (preview seam).

## Why

- **The plan panel did not help** (Dominik, 2026-10-07). The Cleanup page's `CorrectionPlanPanel`
  (acquisition cards, a three-question wizard, a recommended ordered plan, save as `plan.json`, mount as a
  chain) is ~2,300 lines of Julia + Vue (panel, cards, routes, engine) built 2026-09-06. Nobody used it to decide a cleanup.
- **Its cutoffs are guesses.** `correction_plan.jl`'s header: "All bands are unvalidated placeholders".
  That includes the photon-limited cutoff added 2026-10-06 (`_NOT_PHOTON_LIMITED_ZERO_FRAC = 0.50`).
  Calibrating them needs ground truth nobody collects. Cleanup choices are judged by looking at the
  image.
- **Its order is a code constant.** `_ORDER_WEIGHTS` fixes stackAlign → drift → smooth → AF/denoise.
  The two order questions #1483 left unwritten are settled there silently, and the intravital
  recipe (`frontend/src/lib/guides/recipes.ts`) already states an order with a reason per step.
- **The measurements are the useful part.** Per channel, import records `zeroFrac`, `signalFrac`,
  `topFrac` and `clippedSignalFrac` in `meta.saturation` (`ensure_saturation_meta!` backfills older
  images, #1486). Nobody sees them per channel. They surface only as `import.channel_saturated` and
  `import.photon_limited`, and the latter fires only above its own cutoff.

## Locked decisions

1. **Facts, not verdicts.** For the selected image, the Cleanup page shows one line per channel with
   what was measured (zero voxels %, clipped %) and, once drift correction has run, how far the image
   drifted. It shows no "do this" text and no cutoff. The task text (`useWhen` / `notWhen`, e.g.
   "Photon-limited channels: sparse, low photon counts") says what each method needs; the reader
   matches the two. This is the AF decision (user's pick) applied to the rest of Cleanup.
2. **Look before running.** Smoothing becomes a task-preview consumer, as cellpose and AF already
   are (#437): a crop before and after. Its compute moves out of `smooth_run.py` into a helper, the
   extraction TASK_PREVIEW_PLAN already names.
3. **Remove the recommender, keep the probes.**
   - Remove: `CorrectionPlanPanel.vue`, `CorrectionCardPicker.vue`, `cardVis.ts`,
     `useCorrectionPlan.ts`, `types/correctionPlan.ts`, `correction_presets.jl`,
     `correction_plan_chain.jl`, the rule/plan half of `correction_plan.jl`, the
     `/api/correction-plan/*` routes, and the MCP `recommend_correction_plan`.
   - Keep: `meta.saturation` and its probe (`omezarr/saturation.jl`, `ensure_saturation_meta!`), the
     cohort QC sparsity fields (`qc.jl`, `qc_cohort.jl`), and the drift QC findings.
   - `plan.json` files already on disk are left alone and become unread. Nothing deletes user data.
4. **Order lives in the recipes.** No order constant in code. A recipe step's `why` carries the
   reason; a step outside a recipe has no implied order.
5. **The agent gets the same facts the user gets.** The per-channel line comes from one Julia
   accessor on the image (not a gating or plan helper). The GUI and the MCP's `get_image_info` both
   read it. There is no separate agent tool.
6. **Verdict-shaped finding text goes.** `import.photon_limited`'s long text ("Run Cleanup → Denoise
   before segmentation") is a verdict resting on a placeholder cutoff. It is reworded to state the
   measurement, or dropped in favour of the per-channel line. The `seg.*` findings (#1479) stay: they
   point at a step to check, not a method to run.
7. **The `--discovery` arm follows.** `off` currently switches the recommender to `evidence=metadata`.
   After the removal, `off` hides the per-channel facts instead. It is recorded as a changed arm, and
   runs before and after are never pooled.

## Phases

### P0 — read the night run (checkpoint)
- Did the `on` agent read and act on "not photon-limited — denoising would remove signal"? Did the
  `off` agent denoise or smooth? Did either go back to Cleanup after segmenting?
- Either way, the panel UI goes (Dominik's call). The result decides only how loudly the fact is
  shown: if the agent used it, the per-channel line is the replacement; if it did not, P1 is still
  for people and the agent check waits for a later pair.

**Answered (2026-10-09, one run per arm).** `on` acted on "not photon-limited": no denoising, cellpose
per cell type, gates on marker intensity. `off` smoothed inside cellpose and merged the cell types.
Neither went back to Cleanup after segmenting. So the per-channel line is the replacement.

### P1 — per-channel facts — **built**
- `img_cleanup_facts` (`app/src/qc.jl`) on the image payload as `cleanupFacts`; `CleanupFacts.vue` at
  the top of the Cleanup page's task column; `get_image_info` carries it, `--discovery off` drops it.
  Clipped % is of SIGNAL voxels and 0 unless the structural detector saw a pile-up. An image imported
  before the sparsity fields shows no channel line until a denoise run backfills it.
- Was: the image accessor, a row on the Cleanup page for the selected image, and `get_image_info` carrying
  it. Copy per `docs/ui/COPY.md`.

### P2 — smoothing preview — **built, checkpoint open**
- `python/cecelia/utils/smooth_utils.py` (shared by `smooth_run.py` and the worker's `_preview_smooth`);
  `task_previewable(::Smooth)`; protocol 17. The viewer's compare badge reads "Smooth".
- Was: extract the smooth compute and declare the preview trait. Checkpoint: Dominik looks at one
  photon-limited and one bright movie in the preview.

### P3 — removal — **built**
- Removed as listed, plus the §2.1 score layer (`QCResult` and friends — nothing else read it) and the
  autonomous MCP's `recommend_correction_plan`. `import.photon_limited` now reads "N mostly-zero
  channels" and points at the facts; the key is unchanged so banked findings re-render.
- Was: the files and routes in Decision 3, the MCP tool and its guidance line, and the
  `import.photon_limited` copy (Decision 6). Update `docs/inventory/*`, `docs/API.md`,
  `docs/inventory/MCP.md`, and mark CORRECTION_QC_PLAN's recommender sections superseded.

## Out of scope

- New cutoffs or calibration.
- An automatic cleanup chain.
- AF in any recommendation (stays the user's pick).
- A dependency schematic (not yet; see the 2026-10-07 discussion of the review brief in
  `docs/archive/task-discovery-review-prompt.md`).
