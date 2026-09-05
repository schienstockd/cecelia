# SUPPORT denoise as a cleanup task, on a generalised model-training page

**Status:** planning (2026-09-05). Worktree `cecelia-denoising-methods`, branch `denoising-methods`.
**Origin:** `docs/archive/denoising-methods-prompt.md` (companion to the drift/registration prompt),
and the 2026-09-05 evaluation of SUPPORT + DeepCAD-RT on drift-corrected crops of `2h06xA` (MERTK
mem-TOM, photon-limited) and `c91ICQ` (CAR-T, saturated). Result: SUPPORT is a clean win on
photon-limited intravital and useless on saturated data; Dominik: *"that's great"* on 2h06xA v2, *"we
can scrap this for movies like c91ICQ. not photon-starved so it probably doesnt work"*. Full context:
`[[project_denoising_methods_eval]]` auto-memory.

## What we are shipping

A user-visible **standalone cleanup task** that runs a **self-supervised denoiser (SUPPORT)** on a
per-image movie, using a **model picked from a vault**, with the **model trained on a set of images**
via a **generalised Model Training page**. Verbatim third-party method — Dominik's framing:
*"this is not our method. but you can use it for denoising"*.

## Locked decisions

### D1 — Standalone cleanup task, NOT a replacement for median-of-3 smooth
`cleanupImages.denoise` is a sibling to `cleanupImages.smooth` / `driftCorrect` / `stackAlign` /
`afDriftCorrect`. Writes `ccidDenoised.ome.zarr`. The archive prompt's second framing (denoiser as
fusion step replacing median-of-3) is **out of scope for this plan** — Dominik was clear the intent
mirrors the old cellpose-denoise slot.

### D2 — Per-kind vault directories
`<config_dir>/models/denoiseModels/` alongside `<config_dir>/models/coastalModels/`. Each with its own
`list_*_models` and its own manifest schema. Rejected: single `models/` with a `kind` field in the
manifest — mixes schemas, forces if-kind branching in every consumer, blocks per-kind evolution.

### D3 — Set-scope training, like `opticalFlow.train`
The training task trains one model from N images in an experimental set, model reused across similar
acquisitions. Matches `opticalFlow.train` and the "vault is for sharing" premise. Per-image training
was rejected — contradicts the vault.

### D4 — Rename `OpticalFlowModule.vue` → `ModelTrainingModule.vue` NOW
One module page hosts multiple training scenarios. The vault manager on that page gains a **kind
selector** (Optical flow / Denoise / …). Route, sidebar entry, docs sweep. Rejected: two separate
pages (divergent-reimplementation trap), or keep name and add denoise under Optical Flow (dishonest).

### D5 — Attribution UI is PARKED
Dominik: *"maybe leave it for now. and wait for my colleague. otherwise we would also need to cite
scanpy and anything else"*. When a colleague contributes an algorithm, the question is forced and this
gets designed. Belongs in `docs/FUTURE.md` at that point. **Do not build attribution UI in this plan.**

### D6 — Saturation gate is mandatory
Refuse or warn when the input's `rescale8bit.trueMax` is at (or very near) the dtype ceiling. c91ICQ
is the ground-truth case for "don't offer to denoise this". Cheap check on `ccid.json.meta`.

### D7 — Vendor SUPPORT under `python/cecelia/vendor/support/`
SUPPORT is not on PyPI (the `deepcad`-style empty wheel trap was hit 2026-09-05 for DeepCAD; the
SUPPORT PyPI presence was not verified but the repo is not `pip install`-ready). Vendor
`model/SUPPORT.py`, `model/convhole.py`, `src/utils/dataset.py` (only the classes we consume:
`DatasetSUPPORT`, `DatasetSUPPORT_test_stitch`, `random_transform`, `normalize`, `gen_train_dataloader`
if it stays). Upstream: <https://github.com/NICALab/SUPPORT>. License: GPL-2 — compatible with
cecelia's GPL-3-or-later. Vendored version + commit pinned in a `VENDORED.md`.

### D8 — Mirror-pad the temporal input at inference
SUPPORT is a temporal blind-spot model; without padding, the first/last `input_frames//2` frames
come back as init-zeros (verified 2026-09-05). Pad by `input_frames//2` on both ends, strip after.
This is a runner requirement, not a user choice.

### D9 — Training and inference are both `run_py`-launched
No new spawn path. Standard `[PROGRESS] n/total` streaming. See `app/CLAUDE.md` → *Spawning Python*.

## Explicitly out of scope

- **Attribution UI** — parked, see D5.
- **Cross-Z fusion / true 3D denoising** — SUPPORT and DeepCAD-RT are 2D+time. Per-Z inference from a
  once-trained model is the default. Cross-Z is a distinct research question.
- **Denoiser-as-fusion-step framing** (archive prompt Q3) — deferred, see D1.
- **DeepCAD-RT** — evaluated and passed over for SUPPORT on our target dynamic range.
- **The public model catalogue side of the vault** — that is `[MODEL_VAULT_PLAN.md](MODEL_VAULT_PLAN.md)`.
  This plan extends the *local* vault mechanism to a second kind; it does not touch publishing.

## Phased build

Every phase is a self-contained PR; test with the matching category in the same change.

### Phase A — end-to-end inference from a hand-placed model (no training UI)
Purpose: prove the plumbing before writing the training loop.

- Vendor SUPPORT into `python/cecelia/vendor/support/` (D7). Add `VENDORED.md` with upstream commit +
  license note. Verify `import` works from `run_py` with `PYTHONPATH=python/`.
- Create the denoise vault: `<config_dir>/models/denoiseModels/`, `denoise_models_dir()`,
  `list_denoise_models()`, `denoise_model_path(name)`, `denoise_model_manifest(name)` (mirror the
  five coastal helpers 1:1 — see `app/src/config.jl`).
- API: `GET /api/denoise/models` in `api/src/denoise_api.jl` (mirror `optical_flow_api.jl`).
- `_OPTION_SOURCES`: register `denoiseModels` so `optionsFrom: "denoiseModels"` resolves.
- Task registration: `cleanupImages.denoise` in `app/src/task_defs/cleanup_images.jl`, params —
  `model` (select, `optionsFrom: "denoiseModels"`), `channels` (multi-select), `mirrorPadT` (bool,
  default true, hidden), `saturationGate` (bool, default true, hidden), `patchSizeXY` (int, default
  from manifest).
- Python runner: `python/cecelia/analysis/{package}/cleanup_images/denoise.py`. Reads the movie via
  `zarr_utils.open_as_zarr`, per-channel per-Z inference against the picked model, staged write via
  `zarr_utils.staged_store` + `create_multiscales`, native-endian dtype. Mirror-pad per D8.
- **Saturation gate (D6):** if any selected channel's `rescale8bit.trueMax >= (dtype_max * 0.98)`,
  refuse the run with an actionable QC message: *"channel X is at sensor ceiling; denoise won't help.
  Uncheck it or re-import."* Enforce in the Python runner, mirror in the Julia dispatch as an
  early-fail so the frontend surfaces it before a run starts.
- Manifest schema (`<name>.json`) — first cut, machine-written by phase B:
  - `kind: "denoise-support"`
  - `arch: {inputFrames, patchXY, blindConvChannels, midChannels[], depth, bsSize, bp}`
  - `training: {imageUids[], setUid, epochs, batchSize, lr, framesPerImage}`
  - `imaging: {pixelSizeUm, zSpacingUm, frameIntervalS, modality, channels[]}` — per-image, list-of
  - `checksum: sha256`
  - `createdAt`, `ceceliaVersion`
- Tests:
  - Julia — `test-pkg` — round-trip a fake manifest through `list_denoise_models`; picker `optionsFrom`
    resolution.
  - Python — `test-py` — smoke: load a 4-frame fake movie, call the inference wrapper, assert output
    shape and non-zero interior. Fixture stays under the size cap.

### Phase B — training task on the existing OpticalFlow page (temporary home)
Purpose: prove the training loop wired to the vault; don't touch the rename yet.

- Task: `modelTraining.trainSupportDenoise` (or the naming Dominik prefers when we get there — this
  is the fresh module namespace) in `app/src/task_defs/`. Set-scope input: one experimental set's
  images. Params: `imageUids[]`, `modelName`, `channel`, `inputFrames`, `patchXY`, `epochs`,
  `unetSize` (small / medium / large), `midZOnly` (bool, default true — matches per-Z inference).
- Python runner: `python/cecelia/analysis/{package}/model_training/train_support_denoise.py`. Loads
  images via `zarr_utils`, extracts training crops (mid-Z per image by default), trains, writes
  `<name>.pt` **and** `<name>.json` atomically via `write_json_atomic` + a `.pt.partial` → rename.
- Progress: `[PROGRESS] batch/N` streamed per epoch.
- **Interim housing:** register the training task under the existing OpticalFlow module (Optical Flow
  page shows both training tasks). Ugly but honest — this is the C-slice invitation.
- Tests: skipped for training itself (needs GPU + wall time). Add a smoke test that instantiates the
  Python trainer, does two batches on a fake 8-frame movie, and confirms the manifest is written.

### Phase C — rename OpticalFlow → Model Training, kind selector in the vault
Purpose: honest naming; no divergent-reimplementation. Big rename PR.

- Frontend: `OpticalFlowModule.vue` → `ModelTrainingModule.vue`; route `/optical-flow` → `/model-training`
  (redirect the old path); sidebar entry; canvas category `opticalFlow` → decide **rename to
  `modelTraining`** OR keep as a legacy id and layer a display name — measure the docs cost of each.
- `FlowModelVault.vue` grows a kind selector chip row (Optical flow / Denoise). One vault manager, one
  refresh path via `useDataRefresh`, one delete/rename path via `useInlineEdit`. `FlowPlots.vue`
  becomes `ModelPlots.vue` or splits per kind — decision goes here at build time.
- Analysis-board dock: `rail: 'flowModels'` — either rename to `models` and hold both kinds, or keep
  the flow rail and add a `denoiseModels` rail. Rail scoping and `shared.flowModel` in the canvas
  scope determine which is cheaper; MODEL_VAULT_PLAN is silent on this so a small design decision
  goes here.
- Docs sweep — every one of these files mentions the OpticalFlow name today (grepped 2026-09-05):
  `docs/MODULES.md`, `docs/API.md`, `docs/UI.md`, `docs/ARCHITECTURE.md`, `docs/SEGMENTATION.md`,
  `docs/inventory/JULIA_APP.md`, `docs/inventory/FRONTEND.md`, `MODEL_VAULT_PLAN.md`.
- Tests: `test-frontend` — route redirect, sidebar entry, vault manager renders with kind selector.

### Phase D — chain wiring + intensity gate polish + docs entries
Purpose: composability and the durable references.

- Extend `withChainProducedModels` (`frontend/src/utils/chainModelOptions.ts`) + backend
  `_chain_produced_names` (see `docs/SCHEDULER.md`) so a denoise model trained upstream in a chain is
  offered to a downstream `cleanupImages.denoise` node.
- Verify saturation gate surfaces in the frontend early (not only on run) — reuse whatever QC display
  path the existing cleanup tasks use.
- `docs/inventory/JULIA_APP.md`: entry for **Denoise model vault (drop-in)** mirroring the
  Coastal-model-vault entry. Line noting `list_denoise_models` is the ONE listing.
- `docs/inventory/FRONTEND.md`: entry for the Model Training page and its kind selector.
- `docs/MODULES.md`: `cleanupImages.denoise` and `modelTraining.trainSupportDenoise` task specs.
- `docs/ARCHITECTURE.md`: one paragraph on per-kind vault dirs as the pattern.
- Remove or update the interim entry from Phase B in the OpticalFlow section.
- Add a `docs/FUTURE.md` entry: *Attribution UI for verbatim third-party algorithms — deferred until a
  colleague contributes an algorithm (D5)*.
- Update `docs/todo/README.md`: mark this plan as **built** with the phases that landed.

## Runtime cost — the honest number

Measured 2026-09-05 on RTX 2000 Ada Laptop 8 GB (~5 GB free after other processes):

- SUPPORT v2 on 2h06xA (100 T × 1040 × 1051, mid-Z, mem-TOM channel):
  4.2 M-param UNet [64,128,256,512], input_frames=61, patch 128, batch 2, 20 epochs → **~8 min train**,
  ~1 min inference per channel per Z-plane at that size. 4-channel 31-Z inference = ~2 h without any
  reuse. **Train once on mid-Z, infer per Z from that model** is the default (D3 + out-of-scope note).
- Vault worth is exactly this: train on one dataset, reuse across a cohort.

## What can go wrong that this plan does not fix

- **Motion between frames** — SUPPORT assumes registered input. `2h06xA` was drift-corrected; that is
  the honest input contract. The archive prompt's registration companion (`drift-3d-followup-prompt`)
  is where motion is solved.
- **Cross-channel bleed / SHG dominance** — untested. Recommend running on immunolabel channels only
  (SHG is static structure, uninformative for a temporal denoiser).
- **A model transferring across microscopes** — the manifest records `imaging` per source image (see
  Phase A schema) but there is no UI-level "does this fit?" check. That is the MODEL_VAULT_PLAN
  problem, not this plan's.
- **The Python `deepcad`/PyPI trap** — vendor SUPPORT (D7). Do not `pip install support` blind.

## Handoff pointers (for a fresh context after `/compact`)

- This plan file, and `[[project_denoising_methods_eval]]` auto-memory.
- Scratch results from the eval, still on disk: `~/Downloads/TMP/denoise_v2_2h06xA_memTOM.mp4` (the
  "that's great" one), `denoise_compare_c91ICQ_ch2.mp4` (the "scrap" case),
  `denoise_compare_2h06xA_memTOM.mp4` (v1). Scratch code at
  `/tmp/claude-1000/-home-dominik-cc-workspace-cecelia/d901e51f-a11f-4a23-8073-f8202d869370/scratchpad/denoise/`
  (`run_support_v2.py` is the reference training + mirror-padded inference loop; reuse its call shape).
- Read before touching the vault: `MODEL_VAULT_PLAN.md` in this directory.
- Read before touching the training UI: `docs/inventory/JULIA_APP.md` → *Coastal model vault (drop-in)*
  and *Optical Flow page + vault manager*.
- Start on **Phase A**; do not do Phase C before Phase B is merged.
