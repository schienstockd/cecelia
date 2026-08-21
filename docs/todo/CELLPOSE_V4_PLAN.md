# Cellpose v4 migration + retire `cellposeCorrect` (coastal denoise/smooth takes over)

**Status:** Phases 0-5 built (2026-08-21), unreviewed and uncommitted. Branch `feat/cellpose-v4`,
worktree `cecelia-cellpose4`. Suites green: `test-py` 901, `test-pkg` 5931, `test-api`, frontend
typecheck + 2066 vitest.

Dominik, 2026-08-21: *"scrap the cellpose pin. cellpose denoise is not that useful. we will scratch
that. coastal denoise and smooth will replace that. we have to migrate to cellpose v4. v3 is
outdated. and doesn't perform as well on static images."*

**This reverses two written decisions** — both must be rewritten, not left to drift:

| decision | where it is written | new position |
|---|---|---|
| "Cellpose is PINNED to v3 on purpose … Do NOT bump to 4.x" | `pixi.toml:70-73` | pin becomes `>=4.2` |
| "`DenoiseModel` removed in v4; we need it for cleanup/denoise tasks. Do NOT upgrade." | `docs/SHIPPING.md:555` | the cleanup task goes; the need goes with it |
| "This does NOT retire coastal's port. `cleanupImages.cellposeCorrect` still exists and still wants the cellpose pin dropped" | `docs/todo/SMOOTHING_PLAN.md:158` | correct, and now acted on |

The pin rationale was **factually right** (v4 has no `DenoiseModel` — verified below) and is now
simply outranked: the capability is not worth a frozen segmenter.

---

## What v4 actually is — measured, not assumed

Read out of the real `cellpose-4.2.1.1` wheel (`models.py`, `transforms.py`, `denoise.py`,
`METADATA`), not from release notes. Latest 4.x is **4.2.1.1** — the same number `INSTALL.md`'s old
version table carried and `pixi.toml` calls "wrong".

**One model class, one model zoo.** `MODEL_NAMES = ["cpsam_v2", "cpdino", "cpdino-vitb", "cpsam"]`.
`cyto3`/`cyto2`/`cyto`/`nuclei` **do not exist**. Weights come from
`huggingface.co/mouseland/cellpose-sam` into `~/.cellpose/models` on first use
(`CELLPOSE_LOCAL_MODELS_PATH` overrides). `cpdino` additionally needs `dinov3` from git — out of
scope, we ship `cpsam_v2` only.

**Both silent-fallback paths land on `cpsam_v2`, with only a log warning:**

```python
if model_type is not None:                      # our current call passes model_type=
    models_logger.warning("model_type argument is not used in v4.0.1+. Ignoring this argument...")
...
if pretrained_model not in all_models:          # e.g. "cyto3"
    models_logger.warning(f"pretrained model {pretrained_model} not found, using default model")
    pretrained_model = cache_model_path("cpsam_v2")
```

So a straight version bump does **not** fail loudly — every existing `cyto3` run would quietly
become a `cpsam_v2` run. Any migration that leaves the model names in place is a silent-result
change, which is worse than an error. This is the reason model names must be handled explicitly in
the same commit as the pin.

**What survives in `eval()`** (signature verified): `diameter` (still real — `rescale = 30./diameter`),
`stitch_threshold`, `do_3D`, `z_axis`, `normalize`, `min_size`, `batch_size`, `augment`,
`tile_overlap`, `anisotropy`, `flow3D_smooth`, `max_size_fraction`. Return is still
`(masks, flows, styles)`, so `masks, _, _ = model.eval(...)` keeps working.

**What breaks in `eval()`:**

- `channels=` → warning, **ignored**. v4 takes arbitrary channel order; `channel_axis` is the only
  channel control. Our `cp_channels = [1,2]` / `[0,0]` plumbing is dead weight.
- Input is coerced to **exactly 3 channels**: `<3` is zero-padded, `>3` keeps the first 3 with a
  warning. Our 1-channel (cyto-only) and 2-channel (cyto+nuc) stacks are both fine, unchanged.
- **`bsize != 256` raises** for the `cpsam` backbone.
- **`z_axis` with `do_3D=False` and `stitch_threshold=0` raises**
  `ValueError("2D image processing selected, but z_axis is not None")`. Our 3D branch passes exactly
  that combination, and `stitchThreshold` is user-settable down to `0.0` with the tip *"0 = independent
  2D slices"*. **A 3D run with the stitch threshold at 0 is a hard crash on v4 unless we restructure
  the call** — see Phase 2.

**No `DenoiseModel`.** v4's `denoise.py` is 188 lines of *training augmentation* (`add_noise`,
`img_norm`, `deterministic`). The restoration class is gone.

**Custom v3 checkpoints cannot load.** `get_backbone()` does `torch.load(path)` and branches on
`"encoder.cls_token"`; a v3 `CPnet` state dict has no such key, so it is treated as `sam_vitl` and
handed to `CPSAM.load_model` — mismatched keys. This kills `ccia.fluo` (shipped by
`install.sh:207`/`install.ps1:186`, which **hard-fail** if it is absent) and any user-trained
checkpoint in either `cellposeModels/` slot. **Confirmed by running it** — cellpose raises its own
clear error, `ValueError: This model does not appear to be a CP4 model. CP3 models are not compatible
with CP4.`, so `_get_model` only has to name the file in the re-raise.

**Measured on his GPU** (RTX 2000 Ada Laptop, 8 GB, `cpsam_v2`, `diameter=20`): model load 2.1 s,
then 0.28 s / 1.03 s / 4.14 s for a 256² / 512² / 1024² tile, ~1.5 GiB peak VRAM. Cost is linear in
PIXELS (~4 µs/px) where cyto* tracked cells. VRAM is not a problem at the default `bsize=256`.

**New deps**, all resolvable on PyPI (checked): `segment_anything`, `fastremap`, `fill-voids`,
`imagecodecs`, `roifile`, `natsort`, `opencv-python-headless`, `torchvision`. `torch>=2.6` (cu124)
already satisfies v4's `torch>=1.6`.

---

## Discovery inventory

### Pin / environment

| file | what it does |
|---|---|
| `pixi.toml:70-73` | the pin + the "do NOT bump" rationale comment |
| `pixi.lock` | 5 `cellpose-3.1.1.2` wheel entries — regenerated by `pixi install` |
| `docs/SHIPPING.md:555` | pin table row, "Do NOT upgrade" |
| `docs/INSTALL.md` | version table (already said 4.2.1.1; becomes true) |
| `THIRD_PARTY.md:44` | attribution names Cellpose 3 `DenoiseModel` explicitly |

### Model names — 4 independent copies of the same 4 strings

| file | what it does |
|---|---|
| `app/src/tasks/segment/cellpose.jl:24` | `BUILTIN_CELLPOSE_MODELS` — the custom-vs-builtin fork for `cellpose_model_path` |
| `app/src/config.jl:166` | `_BUILTIN_CELLPOSE_MODELS` — **a second copy**, feeds `list_cellpose_models` |
| `app/src/tasks/segment/cellpose.json:33,52-70` | group default `"cyto3"` + the hardcoded `options` array, alongside `optionsFrom: cellposeModels` |
| `python/cecelia/utils/cellpose_utils.py:87` | `model_params.get('model', 'cyto3')` fallback |
| `app/src/tasks/chain.jl:1387`, `docs/SCHEDULER.md:880`, `docs/SEGMENTATION.md:324`, `app/src/tasks/segment/cellpose_run.py:13` | docstring/doc examples |
| `app/test/suite.jl:329,737,12484-12560` | tests that pin the four names and their order |

Two hardcoded copies of one list is a rule-of-three miss already; the bump is the moment to make it
one constant.

### Segmentation call path

| file | what it does |
|---|---|
| `python/cecelia/utils/cellpose_utils.py` | `_get_model` (both `model_type=` and `pretrained_model=` branches), `predict_slice` (`channels`, `channel_axis`, `z_axis`, `stitch_threshold`, `do_3D`) |
| `app/src/tasks/segment/cellpose.jl` | `cellpose_models_for_python` — name→path resolution, custom-checkpoint error message |
| `app/src/config.jl:123-200` | `cellpose_models_dir`, `cellpose_model_path`, `list_cellpose_models` |
| `api/src/routes.jl:418` | injects the model list into the served spec |
| `preview/preview_worker.py` | calls the same `CellposeUtils.predict_slice`; carries a **time budget** tuned to cyto3 |
| `scripts/models_fetch.py`, `install.sh:200-221`, `install.ps1:182-204` | fetch/verify `cellposeModels/` incl. the `ccia.fluo` hard-fail |

### `cleanupImages.cellposeCorrect` — the task being retired

| file | what it does |
|---|---|
| `app/src/tasks/cleanupImages/cellpose_correct.{jl,json}`, `cellpose_correct_run.py` | the task itself (`from cellpose import denoise`, `DenoiseModel`, 7 model options incl. deblur/upsample) |
| `app/src/Cecelia.jl:276`, `app/src/tasks/task_registry.jl:17,155` | include + registry entries |
| `app/src/tasks/task.jl:86-92` | uses it as the worked example of "hardcodes its model list with no hook" |
| `app/test/suite.jl:4320,4624-4627,5713,6827` | pool, param-inheritance, `_spec_output_value_name`, registry tests |
| `frontend/src/utils/runLog.test.ts` (11 sites), `paramValues.ts:349` | fixtures + a doc comment naming the task |
| `docs/`: `MODULES.md:127,851,1400`, `SCHEDULER.md:437-444,879-886`, `TASK_PREVIEW_PLAN.md:34,208`, `ARCHITECTURE.md:119,145,260`, `NAPARI.md:551`, `ZARR_STREAMING_PLAN.md:48-77`, `SMOOTHING_PLAN.md:158-171`, `QC_OBSERVER_PLAN.md:112`, `TASKS.md:86`, `THIRD_PARTY.md:44` | prose, examples, and two plan docs whose reasoning depends on it |

**`cpCorrected` is a different thing and stays.** It is a value_name in `ccid.json`, and the
versioned-variable machinery (`OBJECTMODEL.md`, `STORAGE_RECLAIM_PLAN.md`, `imageDelete.test.ts`,
`api/test/runtests.jl:1444`) must keep reading it — existing projects have those stores on disk.
Retiring the task must not touch the format.

### The replacement already exists

- `cleanupImages.smooth` (`smooth.{jl,json}`, `smooth_run.py`, `resource_pool: "cpu"`) — gaussian +
  temporal median via coastal's model-free restorers. `SMOOTHING_PLAN.md:95-175` measured it
  **beating** the repaired Cellpose-3 net on the metric that matters (24 objects / 0 merges vs
  18 / 2 merges) at **2.8 s vs 88 s**.
- `coastal.denoise` — the Cellpose-3 restoration net **re-implemented, no `cellpose` import**,
  loading the public `denoise_cyto3`/`_cyto2`/`_nuclei` weights. Its own header states the purpose:
  *"it lets a downstream env run Cellpose 4 for segmentation while still getting Cellpose-3-quality
  restoration (the two are the same PyPI distribution and cannot coexist)."* `deblur_*`/`upsample_*`
  were deliberately not ported (upsample changes output size — incompatible with a same-size
  correction).

So the *capability* survives the pin drop either way; the open question is only whether cecelia still
exposes a net-denoise task. See Decision 1.

---

## Decisions (Dominik, 2026-08-21)

1. **No net-denoise task survives.** `cleanupImages.cellposeCorrect` is deleted; `cleanupImages.smooth`
   is the cleanup path. `coastal.denoise` stays available in the library, uncalled by cecelia.
   Deblur and upsample go with it and have no replacement.
2. **Stop shipping `ccia.fluo`.** The installers no longer fetch `ceceliaModels`; the drop-in slot is
   unchanged and takes v4 checkpoints. Retraining on v4 is separate work — see the reservations.
3. **No v3-vs-v4 A/B on his data.** The v4 quality claim for static images is taken as given.
4. **The original prompt's Task 1 is not a default at all.** Dominik reframed it: *"we could come up
   with a sort of.. builder i guess. to say. ok. what do you want to do? intravital? then do these
   steps. do you need behaviour or tracks or interactions? then follow these steps. do you have large
   multiplex images. then do these steps."* That is a guided workflow builder, not a default-selection
   change — parked in `WORKFLOW_RECIPES_PLAN.md`, deliberately out of this branch.

## Phases — all built

Kept in build order; each line is what actually landed.

**Phase 0 — env. DONE.** `cellpose = ">=4.2"` resolved to `4.2.1.1` against torch 2.6.0+cu124, CUDA
available; `segment_anything` and the rest of v4's new deps solved from PyPI without pinning anything.
`cpsam_v2` weights were already cached on this machine from an earlier experiment, so nothing
downloaded. v3-checkpoint rejection and the timings above were measured here.

**Phase 1 — model names, one constant. DONE.** `BUILTIN_CELLPOSE_MODELS` now lives once, in
`config.jl`, as `(name, label)` pairs — `cpsam_v2` / `cpsam` — and `cellpose.jl` reads it instead of
keeping its own tuple. `RETIRED_CELLPOSE_MODELS` beside it is rejected in `cellpose_models_for_python`
with a migration message. `cellpose.json`'s default and `options` rewritten; the v3-checkpoint case
re-raises cellpose's own error with the file path attached.

**Phase 2 — the `predict_slice` call. DONE.** `channels=` and the `model_type=` branch are gone;
`channel_axis` carries the channel layout and v4 zero-pads 1 or 2 channels to its 3 itself. The 3D
branch splits on `stitchThreshold`: `> 0` keeps `z_axis=0` + `stitch_threshold`, `== 0` passes a LIST
of planes (verified to return per-plane independent labels, which is what "0 = independent 2D slices"
always meant). `bsize` is left unset — v4 only rejects a non-256 value, so not passing it is the safe
form. `test_cellpose_v4_callpath.py` pins the kwargs with a fake model that reproduces v4's own
rejection rule; reverting the 3D branch makes it fail, which was checked.

**Phase 3 — retire the denoise task. DONE.** The three files, the `Cecelia.jl` include, the export
and both registry entries are gone. `RETIRED_FUN_NAMES` in `task.jl` gives a saved param set or chain
node naming it a real sentence instead of "Unknown fun_name" plus a list of everything. `cpCorrected`
is untouched everywhere — it is a value_name, not a task. Docs and fixtures that used the task as
their worked example now use `smooth`/`afCorrect`.

**Phase 4 — installers + shipping. DONE.** Both installers' model-fetch blocks are replaced by a
comment saying why there is nothing to fetch; `pixi run models-fetch` and `models_fetch.py` are kept
(the mechanism is still how a v4 checkpoint would be distributed). `pixi.toml`, `SHIPPING.md`,
`INSTALL.md`, `ROADMAP.md`, `THIRD_PARTY.md` and `SEGMENTATION.md` rewritten, including the
`CELLPOSE_LOCAL_MODELS_PATH` pre-seeding note for offline installs.

**Phase 5 — migration note. DONE.** A ⚠️ breaking section in `CHANGELOG.md` `[Unreleased]`, which is
the release body (`docs/RELEASING.md`), so it reaches users through the release and the What's New
modal rather than only through the docs tree.

---

## Reservations

Ranked. 1 and 2 are the ones that change what a user can do.

1. **`segment.branching` has lost its model, and two plan docs regress with it.** `ccia.fluo` was the
   segmenter for fibrous signal (dendritic cells, SHG collagen, FRC networks) upstream of branching.
   `BRANCHING_PLAN.md` and `SPATIAL_ANISOTROPY_PLAN.md` both recorded "`ccia.fluo` reaches a new user"
   as **RESOLVED (2026-08-05)**; both are now marked RE-OPENED. "Segment SHG → branch → quiver" from
   scratch needs either `cpsam_v2` shown to be adequate on fibrous signal (untested — and SAM's
   training distribution is cells, not fibres) or `ccia.fluo` retrained on cellpose 4. Retraining is
   `cellpose.train` on cpsam and its own project.
2. **On intravital data, cellpose is now measurably worse than what it replaced, with no fallback.**
   `SEG_QUALITY_PLAN.md` Phase 2 measured cpsam at **0.0% QC-pass (65 objects)** against tuned
   `cyto2`'s **13.4% (11,070 objects)** on `EaMaVq`. That case is `segment.coastal`'s now, which is
   why the migration is still the right call — but nothing in the GUI says so, and `cyto2` is no
   longer selectable. This is the strongest argument for the guided builder in
   `WORKFLOW_RECIPES_PLAN.md`.
3. **Every existing cellpose run is irreproducible.** No v4 path to a cyto3 result. The migration
   error is honest, but old label stores can no longer be regenerated — and `EaMaVq`'s tuned
   configuration, the one with a measured number attached, is among them.
4. **Quality on static data is unverified.** "v4 is stronger on static images" is taken as given
   (Dominik's call, A/B declined). v3 and v4 cannot coexist in one env, so a comparison would need
   the old env kept around.
5. **Deblur and upsample are gone with no replacement.** Neither exists in coastal, neither is in
   `smooth`. `upsample_*` could not be ported in any case — it changes the output size, which the
   same-size correction contract forbids.
6. **cpsam's cost curve is different, not worse.** 0.28/1.03/4.14 s for 256²/512²/1024², ~1.5 GiB
   peak VRAM on an 8 GB card — fine for one tile, but a large tiled 3D run now scales with pixels
   rather than cells, which is a different planning model for the GPU pool and for batch runs. The
   preview's "downsampling doesn't help" note is now false and says so; nothing was re-tuned, because
   the preview's budget is a normalisation-frames budget, not a time budget.
7. **The GUI still lets you point cellpose at a timelapse.** Nothing gates or hints. Deliberately out
   of scope here (Decision 4), but it is the state this branch leaves behind.
