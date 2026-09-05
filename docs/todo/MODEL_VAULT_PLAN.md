# Public model vault for coastal flow models — schema first

**Status:** planning (2026-08-21). Branch `feat/coastal-model-vault`, worktree `cecelia-vault`.
**Stop point: the schema below is for review before anything is scaffolded.**

## Origin

Task 4 of the archived brief (`docs/archive/coastal-default-cellpose-v4-prompt.md`), the one part of
it still open after the cellpose 4 migration (`CELLPOSE_V4_PLAN.md`):

> a public model vault for pretrained coastal models, mirroring the existing plugin system's shape
> (discover → fetch → register → select) … a manifest per model — tissue type, imaging modality,
> channels used, training data description, metrics, checksum, download URL … *"Train your own"
> stays first-class … Use the existing `flow.cyto` model to validate the manifest schema against a
> real model.*

The brief's own instruction was to check in on the schema before building the picker, because the
schema is load-bearing for anyone who later contributes a model. That is where this stops.

## What already exists — most of the mechanism

| piece | where |
|---|---|
| Vault directory, per-user not per-project | `<config_dir>/models/coastalModels/`, `coastal_models_dir()` |
| Enumeration (the ONE listing) | `list_coastal_models()` (`app/src/config.jl`) — `.pt` files, label from the manifest's `channelName` |
| Name resolution | `coastal_model_path()`, `flow_model_names()`, `flow_model_filename()` |
| The picker | `optionsFrom: "coastalModels"` in `coastal.json`, resolved by `_OPTION_SOURCES` (`task.jl`) and re-run per request by `/api/tasks/definitions` |
| Vault manager UI (list, rename, delete, details) | `modules/modelTraining/ModelVault.vue` (+ `FlowModelDetails.vue` / `DenoiseModelDetails.vue`). ONE manager, kind chip switches which vault — endpoint dispatch table in `utils/modelVaultKinds.ts`. Renamed from `modules/opticalFlow/FlowModelVault.vue` in Phase C of `DENOISE_INTEGRATION_PLAN.md`. |
| Manifest type + rendering rule | `utils/flowManifest.ts` — **unknown keys are SHOWN, not dropped**, which is already forward-compatible with new fields |
| A manifest writer | `opticalFlow.train` (`train_run.py`) writes `<name>.json` **and** embeds the same dict in the checkpoint via `save_model(metadata=…)` |
| The inference contract | `coastal_utils.temporal_config()` reads `temporalScales`, `cumulativeWindow`, `droppedMetrics` — and coastal fails **silently** on a mismatch |
| An install-time model fetch to mirror | `scripts/models_fetch.py` + `pixi run models-fetch` (built for cellpose; now unused, see `CELLPOSE_V4_PLAN.md`) |

So "discover → fetch → register → select" is three-quarters built. What is missing is the *public*
half: a place to publish to, integrity, and the metadata that answers **"does this model suit my
data?"**

## The finding that decides the schema

**Nothing records the physical scale a model was trained at, so the vault's central question is
currently unanswerable.** Coastal's entire feature set is expressed in pixels and frames:

- `temporalScales: [1, 2, 4, 8]` — **frames.** A movie at 5 s/frame and one at 30 s/frame present
  completely different displacements at scale 1.
- `cumulativeWindow: 5` — frames.
- `cropSize: 256`, `zSpacing: 2`, `foregroundBlurSigma: 1.0` — **pixels/planes.**
- The metrics themselves (`cumulative_mag`, `acceleration`, `strain`, `normal_flow`) are px/frame
  quantities.

None of `pixelSizeUm`, `zSpacingUm` or `frameIntervalS` appears in any of the five manifests on this
machine. `train_run.py` reads `dim_utils.im_physical_size('z')` — for a **log line** only (`:357`) —
and `dim_utils.im_time_increment()` exists and is never called there.

I tried to fill the catalogue entry below for `flow.cyto` from `flow.cyto.json` and **could not**: the
two fields that decide whether it transfers to someone else's movie are the two that are not written
down. For a local vault that is invisible — you trained it on your own data. For a public one it is
the whole product. **So the training-side fix comes before the vault, not with it.**

This is the same failure mode the manifest was invented to prevent. `list_coastal_models`' own comment
says it: *"Inference must use the metric set the model was trained on, and coastal fails SILENTLY when
it does not."* A resolution mismatch is silent in exactly the same way, and a downloaded model is the
case where it is most likely.

## Decisions

### D1 — Two documents, and only one of them is hand-authored

- **The training manifest** (`<name>.json`) stays machine-written and is **never** hand-edited. It is
  the inference contract (`temporal_config` reads it) and it also travels inside the checkpoint. Any
  human editing it can silently change what a model does.
- **The catalogue entry** (in the vault repo) is hand-authored, reviewed in a PR, and describes what
  the model is *for*. It carries integrity and discovery metadata, and it **quotes** rather than
  duplicates the training manifest.

A single merged document was the obvious first draft and is wrong for that reason alone: it would put
a reviewer's prose in the file inference configures itself from.

### D2 — The catalogue is an index of small files; weights are release assets

`catalogue/index.json` (ids + versions + titles, a few kB) plus one `catalogue/<id>.json` per model.
Listing the vault is one small fetch; a contribution is one new file in a PR.

Weights go to **GitHub release assets** on the models repo, not into git. This is a real difference
from the cellpose fetch we already have (`models_fetch.py` downloads a zip of a whole branch): 7 MB
per model in git history compounds, and the coastal case wants **one model** downloaded, not the set.

### D3 — Versions are separate entries, never mutable files

A revised model is `flow.cyto` v2 as its own catalogue entry and its own asset, not a new file at the
same URL. Then "version-pinned in the pipeline config" is free — the config already records the model
filename, and the file it names can never change under it. No `@version` suffixes in filenames, no
mutable "latest".

### D4 — Downloaded and locally-trained models live in the same directory, distinguished by the manifest

One vault, one enumeration (`list_coastal_models` stays the only listing). A downloaded model's
manifest gains a `vault` block — `{id, version, sha256, fetchedAt}` — which is also how the UI can
mark it as "from the vault" rather than trained here, and how a re-fetch can be idempotent.

### D5 — "Train your own" is first-class by construction, not by ordering

The picker's first option is already `None`, and the vault is already empty until you train something.
Downloading is an *addition* to that surface, not a replacement, and the vault section says out loud
that a pretrained model is a starting point. Nothing needs re-ordering to make this true — it just
must not be undone.

### D6 — Fetching is explicit, never implicit

No download at install time and none on first use. A model appears because someone opened the vault
and chose it. An unattended chain that names a missing model keeps failing loudly, exactly as it does
today (`COASTAL_SEGMENTATION_PLAN.md` decision 7) — silently fetching 7 MB mid-run is worse.

## The schema

### Part 1 — new fields in the training manifest — **BUILT** (P0)

Two keys, per movie, matching the manifest's existing style for anything that varies per source
(`frameWindows`, `zPlanesUsed`, `cropWindows` are all `{uid: …}` already):

```jsonc
{
  "physicalScales": {
    "VJy1Nx": { "x": 0.62, "xUnit": "um",        // "y"/"yUnit" only when Y differs from X
                "z": 2.0,  "zUnit": "um",        // the gap between the planes TRAINED ON
                "t": 30.0, "tUnit": "s" }
  },
  "physicalScaleSource": "ome"                   // "ome" | "partial" | "none"
}
```

Three things this shape gets right, each learned from the code it sits in:

- **Per movie, not pooled.** Training pools several movies and they may come from two objectives or
  two microscopes; a single averaged number would hide exactly the case the field exists to expose.
  This is the argument `zPlanesUsed`'s own comment already makes about depth.
- **`z` is the gap between the planes trained on**, not the stack's step. Training takes every
  `zSpacing`-th plane, so a 1 µm stack at spacing 2 means the model saw 2 µm.
- **Values are recorded as read, with their unit, and never converted.** There is no unit converter
  in this codebase; inventing one to normalise metadata that is already µm/s in practice is a silent
  numeric error waiting for the first file that isn't. The catalogue entry is where a human states a
  range in µm, and that is a reviewed step.

`physicalScaleSource: "none"` is the honest case and the reason it is a separate key: an image with no
OME scale gives `im_physical_size` its own fallback of **1.0**, so a bare call writes "1 µm/px" as
though it were a measurement. The vault UI renders `none` as *"unknown — the source images carried no
physical size"*, which is the one absent field worth saying is absent. **Every model already in a
vault predates this and shows nothing at all**, exactly as it did before — correct, not a migration
failure.

### Part 1b — `flowFingerprint`, the engine check — **BUILT**

`coastalBuild` says which code produced a model. It cannot say whether a given change touched the
**feature recipe**, so as a check it fires on every commit — which is why point 5 below concedes it
"makes a discrepancy explainable; it does not prevent one". This is the part that prevents one.

```jsonc
{
  "flowFingerprint": {
    "version": 1,                                // `flow_probe.VERSION`; a different probe is not compared
    "metrics": { "mag_1": [0.148, 0.611], … }    // [spread, 99th percentile of |value|] per metric plane
  }
}
```

Measured, not declared: `flow_probe.fingerprint()` runs the real inference entry point
(`coastal.flow.flow_metrics_for_frame`, asserted elementwise-equal to the training path's
`prepare_data_for_unet`, so one probe covers both) on a fixed analytic window, and summarises every
metric plane it returns. Inference re-measures and compares (`CoastalUtils._check_flow_engine`).
~4 ms, and the coastal import it needs is paid by the run anyway.

Four choices worth keeping:

- **Numeric, not a source hash.** A hash over coastal's flow module would fire on a comment edit —
  a false positive for the one reason a user cannot act on. The probe is sensitive to exactly what
  matters: anything that moves the numbers.
- **The probe's config is FIXED** (`scales (1, 2)`, `cumulative 3`), not the model's. This measures
  the engine; the configuration is recorded and checked separately by `temporal_config`. Mixing them
  would mean two models trained at different scales could never be compared against one engine.
- **`spread` and `p99(|v|)`, not the mean.** Several metrics are signed and near-symmetric, so their
  mean sits at ~0 — where a relative tolerance carries no information and an absolute one fires on
  noise. Both recorded statistics are strictly positive for any non-constant plane, so one tolerance
  works for the whole stack.
- **A WARN, not a refusal.** `RTOL` is 1e-3, orders of magnitude above cross-machine SIMD drift and
  orders of magnitude below a recipe change — but "almost certainly real" is not the standard for
  refusing to run somebody's segmentation. Absent on either side reads as *cannot be checked*, which
  is a third answer and says so.

What it catches, measured on `zolIMa/fXgbTl` mem-TOM: swapping Farneback for `cv2.DISOpticalFlow`
gives a magnitude field correlated **0.00** with the current one and moves cell/background separation
from **3.68 to 0.84** — background flowing faster than cells — while `temporalScales`, `metricKeys`,
`droppedMetrics` and `cumulativeWindow` all still match. DIS only accepts 8-bit input
(`I0.depth() == CV_8U`), so adopting it would reintroduce the cast coastal removed in PR #19, which
is what put every velocity metric at chance. `test_flow_probe.py` performs that swap and asserts the
fingerprint disagrees.

**Models trained before this shows nothing**, like `physicalScales` before it, and inference says so
once per model rather than guessing.

### Part 2 — the catalogue entry

```jsonc
{
  "schemaVersion": 1,
  "id": "flow.cyto",                        // vault id; also the filename stem when fetched
  "version": 1,                             // bump = a NEW entry (D3)
  "title": "Cytoplasmic reporter, germinal-centre B cells",
  "summary": "Two-photon intravital lymph node; cytoplasmic membrane reporter in a photon-limited movie.",

  // The recognition test — what a chooser reads. Every field optional, and an ABSENT field is
  // honest where an invented one is not.
  "suits": {
    "modality": "two-photon intravital",
    "tissue": "lymph node, germinal centre",
    "labelling": "cytoplasmic reporter",
    "dimensionality": "3D timelapse",
    "pixelSizeUmRange":    [null, null],     // ← CANNOT BE FILLED TODAY (see the finding)
    "frameIntervalSRange": [null, null],     // ← ditto
    "notSuitedTo": "nuclear labels; fixed tissue; anything without motion"
  },

  // Quoted from the training manifest, so the entry is readable without downloading it. NOT a second
  // source of truth: the manifest that ships with the weights is authoritative.
  "trainedOn": {
    "movies": 6, "frames": 2880, "channel": "mem-TOM",
    "temporalScales": [1, 2, 4, 8], "cumulativeWindow": 5,
    "droppedMetrics": ["divergence", "flow_structure_alignment", "vorticity"],
    "epochs": 100, "trainedAt": "2026-08-19 17:44"
  },

  // Numbers only where they were measured, each with a pointer. Same rule as the guide copy
  // (WORKFLOW_RECIPES_PLAN D5): no plausible-looking figures.
  "quality": [
    { "metric": "objects vs scipy intensity baseline", "value": "33 vs 30",
      "on": "fXgbTl mem-TOM", "source": "docs/todo/SEGMENTATION_OPEN_PROBLEM.md" }
  ],

  "files": {
    "weights":  { "url": "https://github.com/schienstockd/ceceliaModels/releases/download/coastal-flow.cyto-v1/flow.cyto.pt",
                  "sha256": "…", "bytes": 7268160 },
    "manifest": { "url": "…/flow.cyto.json", "sha256": "…", "bytes": 23570 }
  },

  // The knob values this model was VALIDATED with, and the engine that produced the validation.
  // Not decoration: the scale-bearing knobs are in µm and self-adjust per image, but their VALUES are
  // still a choice, and the app's defaults for them move (see the leeway section — `embeddingBlurSigma`
  // changed under us mid-branch). The vault offers to apply these rather than leaving a recipient on
  // whatever today's defaults are.
  "inference": {
    "coastalBuild": { "version": "0.1.0", "commit": "49d6380" },
    "params": { "seedSize": 4.0, "seedBlurSigma": 2.5, "probThreshold": 0.3,
                "affinityThreshold": 0.5, "embeddingBlurSigma": 1.5, "minComponentSize": 2.0,
                "stitchThreshold": 0.2, "labelSmoothing": 0.5 }
  },

  "license": "CC-BY-4.0",
  "author": "Schienstock lab",
  "citation": "",                            // DOI once there is one
  "notes": ""
}
```

`catalogue/index.json` is `{schemaVersion, entries: [{id, version, title, summary}]}` — enough to
render a gallery, small enough to fetch on opening the vault.

### Why these fields and not the brief's list verbatim

The brief asked for "tissue type, imaging modality, channels used, training data description, metrics,
checksum, download URL". All present, with three changes:

- **`channels used` is a name, not an index.** `trainChannels: [2]` in the manifest is an index into
  *his* image; it means nothing in another lab's file. `channel: "mem-TOM"` is the transferable half,
  and it is already what the picker labels with.
- **Scale ranges added**, per the finding — the fields that actually predict transfer.
- **`notSuitedTo` added.** Every model in this vault will be trained on one lab's data, and the useful
  sentence is often the negative one. It is also the field a reviewer can check.

## Does a recipient have to reproduce our parameters, or is there leeway?

Dominik, 2026-08-21: *"we have not shown that models can transfer to other data. this is more of a
test. and it needs testing whether they would have to reproduce the exact parameters that we used or
whether there is some leeway."*

The question splits in two, and only one half is an experiment.

### The knobs already travel — they are in µm, not pixels

Every user-facing inference parameter in `coastal.json` that has a scale is declared in **microns** and
converted per image through `px_from_um` / `px_area_from_um2`: `seedSize`, `seedBlurSigma`,
`embeddingBlurSigma`, `probBlurSigma`, `mergeMaxDistance`, `minComponentSize`, `labelSmoothing`. So
"seed window 4 µm" means the same biology on a recipient's image and resolves to a different number of
pixels by itself. That half of the transfer problem was solved before it was asked.

What does **not** self-adjust is the model's own learned features, which are in pixels and frames:
`temporalScales`, `cumulativeWindow`, the crop size it was trained at, and every flow metric
(px/frame quantities). Nothing converts those, and nothing can — they are baked into the weights.

**So the leeway question is specifically about acquisition scale, not about the knobs.** That is also
how the experiment below has to be set up: hold every µm-valued knob fixed, vary only the acquisition,
or the two effects are conflated.

### The half that is bookkeeping, not research

The inference parameters a recipient *could* reproduce exactly are **not recorded anywhere**. The
manifest is a record of TRAINING; the segmentation knobs are chosen later, on the Segment page, and
live only in that run's params. So a published model arrives with no statement of the settings it was
validated with.

And the defaults it would otherwise inherit are moving. On `perf/coastal-speed` right now,
`d3006e2e` changes the group default `embeddingBlurSigma` from **0.5 to 1.5** — a real fix (the spec
carried two values, so a second model entry silently ran at a different blur from the first), but
`docs/SEGMENTATION.md`'s own measurement has that parameter moving median object diameter
**10.3 → 8.9 µm**. A recipient running "the defaults" before and after that lands gets different
objects from the same weights.

Two consequences, both cheap:

- **The catalogue entry carries an `inference` block** — the exact knob values the model was validated
  with — and the vault offers to apply them rather than leaving the user on whatever the app's current
  defaults are. This is the answer to "must they reproduce our parameters": for these, yes, and the
  vault should hand them over rather than asking.
- **The manifest records the engine.** `coastalBuild: {version, commit}` — done, see P0. Without it a
  model's output cannot be tied to the code that produced it, and coastal's inference is under active
  change.

### Would you train in physical units instead?

Dominik, 2026-08-21: *"would you somehow train on physical size and actual timescale"*. Yes — but the
useful version is not feeding the numbers to the network, and one thing has to be corrected first.

**Coastal already throws the absolute scale away.** Every metric plane goes through
`normalize_metric` (percentile 0.02/99.98 → [0, 1]) — `mag_*`, `acceleration`, `strain`,
`normal_flow`, all of them — at training *and* at inference (`flow_metrics_for_frame` is asserted
elementwise-equal to `prepare_data_for_unet`). So a uniform change in displacement, which is exactly
what a different frame interval or a different magnification produces, is largely normalised out
already. **Converting px/frame to µm/s before that would be a no-op.** That is worth knowing before
building anything: the obvious version of "train in physical units" buys nothing here.

What normalisation cannot touch, and therefore what actually fails to transfer:

1. **Which time spans the feature stack covers.** `temporalScales: [1, 2, 4, 8]` are frame offsets:
   5–40 s on a 5 s/frame movie, 30–240 s on a 30 s/frame one. Those are different features, not the
   same feature at a different gain.
2. **Structure size in pixels.** An 11 µm cell is 18 px at 0.6 µm/px and 37 px at 0.3. The UNet's
   receptive field is in pixels and does not adapt.
3. **Aliasing.** Past Farneback's window the flow is simply wrong — and per-plane normalisation then
   makes wrong-but-in-range look exactly like signal. This is the failure mode that would be hardest
   to notice in someone else's hands.

So "physical units" means **choosing the feature geometry in physical units**, in two independent
pieces:

- **Temporal — DONE, both halves (inference 2026-08-21, training 2026-08-24).** Declare the scales in
  SECONDS and resolve them per movie to frame offsets, at training and at inference, recording the
  seconds in the manifest. A recipient's movie adapts itself instead of the recipient matching a frame
  count. Coarser-than-declared data cannot be fixed (you cannot interpolate frames you did not
  acquire), so a model declares a maximum usable frame interval.

  `segment.coastal`'s **Temporal scale** (`frames` default / `seconds`) plus `opticalFlow.train`'s own,
  where `seconds` takes the spans and resolves each training movie onto its own offsets. Full account
  in `docs/SEGMENTATION.md` → *Temporal scale*; the three things worth carrying back here:

  **It did not need the retrain, and the reason is the design that made it cheap.** A seconds model
  still records ordinary frame offsets in `temporalScales` — the spans at the FINEST training interval
  — plus `temporalReferenceInterval` saying which rate they belong to. So it IS a frames model with a
  statement attached, every path that predates the mode reads it unchanged, and no model in any vault
  is invalidated. "Declaring the seconds primarily, so a model has no frame offsets at all" was the
  wrong target: the offsets are what the channels are NAMED after, and removing them would have forced
  a second manifest shape on every reader for no gain.

  **The inference-only half shipped broken, and the bug was in the layer this plan did not name.**
  Coastal names its per-scale planes `mag_{offset}` and stacks the metric dict by `sorted(keys)` — a
  string sort. So `[1,2,4,8]` resolved to `[2,4,8,16]` fed `mag_16` into the channel the model reads
  as `mag_1`, at the right channel count and therefore silently. Every ratio ≥ 2 was affected, i.e.
  the common case. Fixed by renaming the resolved planes onto the trained names by position
  (`mag_rename`), which is now also what lets a mixed-rate training set pool at all. The lesson for
  the spatial half below: the arithmetic was never the risk, the CHANNEL IDENTITY was.

  **Collapse and clamp are refusals, not notes.** The old code deduped a collapsed scale set and
  reported it; that shifts every channel after the mag block and zero-fills the tail. Both now raise,
  naming `maxFrameInterval` — the shortest span and the smallest gap between two spans, whichever is
  smaller. A training movie that trips either is skipped with a warning rather than clamped onto its
  closest frames (Dominik, 2026-08-24).
- **Spatial — a choice between two known approaches.** Either resample XY to a canonical µm/px
  (exactly what cellpose 1–3 did with diameter → 30 px, and what v4 gave up), or train across scales
  by resampling as augmentation and let the net absorb it. The first is predictable and costs a
  resample per frame; the second needs more training and gives no guarantee.

**The option that sounds best is the weakest one here.** Feeding µm/px and s/frame to the network as
conditioning inputs cannot work yet: you cannot learn to condition on a variable that is constant in
your training set, and every movie in the archive is one microscope at one setting. The only way to
get multi-scale training data today is to synthesise it by resampling — which is the same machinery
P0.5 needs. One implementation, two uses, and P0.5 is what says whether the spatial half needs
canonicalising at all.

**Consequences to accept before starting.** Written of both halves together, and the temporal one
turned out cheaper than this: it invalidated nothing, because a seconds model still carries frame
offsets (above). What remains true is the SPATIAL half — resampling XY changes what every pixel-valued
knob means, so that one does invalidate the vault and still sequences behind flowperf.

### The half that is an experiment — and it needs no retraining

**Design.** One movie, one model already trained on it (`flow.cytoFg` / `fXgbTl` is the cheapest honest
case: 240 frames, a single source image). Perturb the INPUT along one axis at a time, hold every
µm-valued knob fixed, re-run `segment.coastal`, and score:

| axis | perturbation | simulates |
|---|---|---|
| XY scale | resample ×0.5, ×0.71, ×1.41, ×2 **and correct the OME physical size to match** | another objective / another microscope |
| Frame interval | keep every 2nd, 3rd, 4th frame | a slower acquisition (only coarser is simulable) |
| Z spacing | every 2nd plane | a coarser stack |

Correcting the physical size is what keeps the µm knobs meaning the same thing after a resample —
without it the run changes for two reasons at once and the result says nothing.

**Score with what exists**: object count, median diameter against the expected ~11 µm, merge count
(the metrics `docs/SEGMENTATION.md` already uses for this segmenter), plus the QC-gate pass-yield
harness — which `SEG_QUALITY_PLAN.md` already nominated as the yardstick for coastal.

**What it produces** is exactly the two fields the catalogue cannot fill today: the factor at which
quality falls away becomes `pixelSizeUmRange` and `frameIntervalSRange`, measured rather than guessed.

**Three caveats, all of which limit what the answer means:**

1. **Resampling is not acquiring.** Downsampling keeps the photons and loses the sampling; a real
   lower-resolution acquisition has different noise. So this bounds the answer optimistically. The
   honest version needs a second acquisition of the same specimen at a different objective — which
   may already exist in the archive, and is worth looking for before building a synthetic sweep.
2. **It must run against a FIXED coastal commit**, and coastal's inference is changing on
   `perf/coastal-speed` right now. Numbers taken against a moving engine expire. So: after that lands,
   or on that branch, and the `coastalBuild` field is recorded with them.
3. **One movie, one model, one labelling.** It measures the tolerance of THIS model, not of the
   method. Which is the right first answer — "is there any leeway at all" is the question, and one
   curve answers it.

**Sequencing consequence:** nothing gets published until this runs. The vault's discovery value *is*
those ranges; an entry that says "suits: two-photon intravital" with no scale bounds is a guess in a
schema field, which D5 exists to forbid. P0 stands on its own regardless — it improves the local
vault today.

## Phases

**P0 — record what a model was trained at. DONE (2026-08-21).** `physicalScales` +
`physicalScaleSource` (per movie, unconverted, `z` measured between the planes trained on) and
`coastalBuild` (version + git commit), written by `opticalFlow.train` and rendered in the vault's
details modal. 8 python tests + 7 frontend ones; the "invented 1.0" case was validated by
re-introducing the bug. Improves the LOCAL vault immediately — nothing previously told you two of
your own models were trained at different magnifications, or against different engine builds.

**P0.5 — the leeway experiment** (see the section above). Blocks publishing, not the schema. Runs
after `perf/coastal-speed` lands, needs no retraining, and produces the two catalogue fields that
cannot honestly be filled without it.

**P1 — the catalogue, as data only.** `catalogue/index.json` + `catalogue/flow.cyto.json` in
`schienstockd/ceceliaModels`, the first entry filled in for real, and a validator (a test, run against
the checked-in example) so a malformed contribution fails rather than silently disappearing from the
gallery.

**P2 — fetch + verify.** A `coastal_models_fetch` alongside `models_fetch.py`: read the index, download
one entry's two files, verify sha256, write the `vault` block into the manifest. CLI first, because it
is testable without any UI.

**P3 — the picker.** The vault manager gains a "Browse published models" section over the same
`list_coastal_models` enumeration. **Not started until P1's schema is reviewed** — the schema is what
the UI renders, so building the UI first fixes the schema by accident.

## Reservations

1. **Nobody has shown that a coastal model transfers to other data at all** (Dominik, 2026-08-21:
   *"this is more of a test"*). Every model here was trained on the movie it was then used on. So the
   vault is an experiment before it is a feature, and P0.5 is the experiment. Until it runs, the honest
   first entry is a *reference* — this is what a converged model looks like, here is what it was
   trained on — rather than something anyone is told to run on their own data.
2. **The scale fields are necessary, not sufficient.** Matching µm/px and s/frame does not mean
   matching labelling, depth, SNR or motility. The schema can only make the mismatch visible, and
   P0.5 measures only the axes it perturbs.
3. **`flow.cyto` itself still has no recorded scale** — it was trained before P0 existed, so its
   manifest shows nothing. Either retrain it (100 epochs) or read the values back from the six source
   images and put them in the CATALOGUE entry rather than the manifest — the catalogue is
   hand-authored by design (D1), so that is the honest place for a number a human looked up. Worth a
   decision at P1, and it is the first real test of D1's split.
4. **A GitHub release-asset URL is not archival.** Fine for now; a DOI (Zenodo) is the answer if these
   are ever cited, and `citation` is in the schema for that reason.
5. **A published model is only as stable as coastal's inference.** `coastalBuild` records which
   build produced it, but nothing pins a recipient to that build — and `perf/coastal-speed` is
   changing inference now, including one default that moves object size. Recording the engine makes a
   discrepancy explainable; it does not prevent one. **Partly closed by `flowFingerprint`** (Part 1b):
   a change to the flow recipe is now DETECTED and reported. The inference *parameters* are still
   unpinned — that is what the catalogue's `inference` block is for.

6. **The vault still does not travel with a `.ccbundle`.** A shared project naming a vault model now at
   least gives the recipient something they can fetch — which is an improvement on today, but it is not
   the same as the export carrying it.
