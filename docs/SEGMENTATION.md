# Segmentation

Design reference for the cellpose segmentation pipeline and the extension points for future algorithms (Stardist, etc.).

---

## Data flow

```
Julia (cellpose.jl)
  → resolves input zarr path from ccid.json "filepath"
  → converts channel names → 0-based indices
  → writes JSON params to {task_dir}/tasks/cellposeSegment.{id}.params.json
  → spawns  app/src/tasks/segment/cellpose.py as subprocess
  → reads [PROGRESS] n/total from stdout
  → on success: writes label file list to ccid.json "labels" field

Python (cellpose_run.py)
  → opens OME-ZARR via zarr_utils.open_as_zarr
  → constructs DimUtils from OME-XML metadata
  → constructs CellposeUtils(params, dim_utils)
  → calls predict_from_zarr(im_dat)

SegmentationUtils.predict_from_zarr
  → computes global norm params from im_dat[-1] (lowest-res level) if normaliseToWhole=true
  → outer loop: T timepoints × XY tiles
  → inner loop: one model per tile → predict_slice → crop → accumulate into label arrays
  → post-processing per label type
  → base-nuc matching (if both present)
  → writes zarr outputs
```

---

## Label type convention

| matchAs value | Meaning | Output file |
|---|---|---|
| `"base"` | Primary segmentation (cytoplasm, membrane, or any single-channel cell body) | `{outputValueName}.zarr` |
| `"nuc"` | Nucleus segmentation, matched to base IDs by IoU | `{outputValueName}_nuc.zarr` |

**'base' is always the primary type.** It is written to `{outputValueName}.zarr` (no suffix). Non-base types are written as `{outputValueName}_{type}.zarr`. There is no `{outputValueName}_base.zarr`.

For a cyto-only run (one model, matchAs="base"):
- Output: `labels.zarr` only

For a cyto+nuc run (two models, matchAs="base" and matchAs="nuc"):
- Output: `labels.zarr`, `labels_nuc.zarr`
- Nuc label IDs are re-mapped to match base IDs by IoU (matchThreshold param)
- `removeUnmatched=true` removes base cells with no matching nucleus

---

## ccid.json `labels` field

Stored as a `Dict{String, Vector{String}}` — value_name → list of zarr filenames:

```json
{
  "labels": {
    "default": ["default.zarr", "default_nuc.zarr"]
  }
}
```

- Set by `cellpose.jl` on task completion (derived from the `matchAs` values in params)
- Read by `_load_image` in `image.jl` as `CciaImage.labels`
- Exposed to the frontend via `_image_payload` in `routes.jl` as `labels: Record<string, string[]>`
- Backward-compatible: bare string values in old ccid.json are wrapped in a 1-element list

---

## Output zarr layout

```
{task_dir}/labels/
  {outputValueName}.zarr/       ← base (primary) labels, multiscale OME-ZARR v2
    .zattrs                     ← multiscales metadata, axes=[t,z,y,x] (no C), coordinateTransformations
    0/                          ← full resolution
    1/                          ← 2× downsampled (if im_dat has > 1 level)
  {outputValueName}_nuc.zarr/   ← nucleus labels (same layout, only written if nuc model present)
```

Shape = image shape without the C axis. dtype = uint32.

The OME-ZARR metadata includes `axes` (lowercase, no C) and per-level `coordinateTransformations` with physical scale from OME-XML (Y/X scale doubled at each pyramid level).

### Stores are written staged, never in place

A run does not write `{outputValueName}.zarr` directly. It streams into a `{outputValueName}.zarr.partial`
sibling and that is renamed onto the final path only once the pyramid is complete — the
`zarr_utils.staged_store` idiom, which every store writer in the codebase uses (see the *Image /
OME-ZARR access* rule in `CLAUDE.md`).

A label store is also written with a **different compressor from an image store** — plain zstd rather
than the image default of blosc+byte-shuffle. Not an oversight: label planes are >99% zero, so what
compresses them is a long-range match across the whole plane, and blosc's small blocks plus the byte
shuffle both break those runs up (measured 1273-2019x against 794-1145x on two real stores). Both
label writers therefore pass `kind='labels'` to `zarr_utils.store_compressor`; see the constants there
before "unifying" the two.

The reason is specific to re-running a value_name that is **already registered**. The writer used to
`rmtree` its target and then fill it over minutes, so cancelling that re-run left `ccid.json`
advertising a store with most of its frames missing. On a multi-level image the next read raised
`KeyError: '1'`; on a **single-level** one — drift/AF/cellpose-corrected output, i.e. the common case —
there was no error at all: the unwritten frames read as zeros and `segment.measureLabels` and tracking
produced numbers from a partial segmentation. Staging makes a cancelled run a no-op on the previous
data.

Cancellation is a SIGKILL, so nothing cleans up: the `.partial` directory survives. That is the
intended trade (the alternative is deleting the user's data to tidy up) and it is invisible — nothing
in `ccid.json` names it. Settings → Data patches → **Remove leftover stores** is the broom
(`cecelia.utils.store_sweep`), and Settings → Storage reports the bytes on Scan so it announces itself
rather than waiting to be found.

**The sweep detects structurally, not by name**, because a name list only ever covers the writers
someone remembered. `*.partial`/`*.superseded` catches everything that opted into `staged_store` — AF,
drift, cellpose-correct, crop. **Import does not opt in**: bioformats2raw writes straight to the
FINAL name and a cancel leaves a
half-written store called `ccidImage.ome.zarr` — which a name-based sweep actively *skipped* as a real
store — plus `_stage_src`, a full local copy of the source that is often larger than the store itself.
So on top of the name fast path:

| `why` | Test |
|---|---|
| `unregistered` | a store in a store location no `ccid.json` entry names (a cancelled import is unregistered *by construction* — registration is the last thing a successful run does) |
| `incomplete` | `.zattrs` declares levels 0..N, fewer exist on disk — catches a truncated store even at a registered path (the `KeyError: '1'` case) |
| `scratch` | import scratch (`_stage_src`) — neither a store nor suffixed |

Two guard rails, because a sweep that deletes the wrong directory is worse than the disk it reclaims:
the orphan check is scoped to **store locations only** (`0/`, `labels/`, `branchLabels/` — `data/`,
`qc/`, `gating/` and `labelProps/` are legitimately unregistered and deleting them would take the
user's analysis), and an unreadable `ccid.json` reports **nothing** rather than everything.

---

## Python class hierarchy

```
python/cecelia/utils/segmentation_utils.py        SegmentationUtils (base)
python/cecelia/utils/cellpose_utils.py            CellposeUtils(SegmentationUtils)
python/cecelia/utils/coastal_utils.py             CoastalUtils(SegmentationUtils)
app/src/tasks/segment/cellpose_run.py      entry point (named _run to avoid shadowing the cellpose package)
app/src/tasks/segment/coastal_run.py       entry point
```

Further algorithms (Stardist, …) subclass `SegmentationUtils` and implement `predict_slice`.

### The Julia half — `app/src/segmentation.jl`

Swapping the algorithm is a Python concern (one subclass, one `predict_slice`), but a new backend also
needs a Julia task handler, and most of what that handler does with its *output* is not specific to any
algorithm. Those pieces live in `app/src/segmentation.jl` so a second backend adds a `_run_task` and
its own param translation, and nothing else:

| Helper | What it owns |
|---|---|
| `segment_label_files(vn, models)` | the output filename convention — mirrors `_store_path` in `segmentation_utils.py` |
| `register_label_files!(img, vn, files)` | recording the finished set in `ccid.json` `labels` |
| `segment_live_outputs(params)` | the live-preview declaration (below) |
| `segment_qc_findings(counts)` | per-type counts → advisory QC findings |

What stays in the task's own `.jl`: resolving the input image, translating params for the backend, and
any model/checkpoint lookup (for cellpose: `BUILTIN_CELLPOSE_MODELS` + `cellpose_model_path`).

### `SegmentationUtils` responsibilities
- XY tiling with overlap (`blockSize`, `overlap`)
- Global normalisation params from lowest-res zarr level (`normaliseToWhole`)
- Global label ID tracking — `max_labels[match_as]` incremented per tile so IDs are unique across tiles and timepoints
- Tile merge via `np.maximum`
- Tile seam stitching (`labelOverlap > 0`): after tiling, labels split at tile boundaries are matched by IoU and remapped to a single ID
- Post-processing: erosion, expansion, min/max size filter, XY border clearing, Z depth clearing (per timepoint)
- Base-nuc IoU matching
- Writing multiscale OME-ZARR output

### `CellposeUtils` responsibilities
- GPU detection (CUDA → MPS → CPU)
- Model loading and caching (`_get_model`)
- Channel preparation: threshold → median/gaussian filter → percentile normalise
- Calling `cellpose.model.eval` with correct channel stacking and 3D stitch mode
- Physical diameter conversion: `cell_diam_um / phys_size_x` → pixels
- Returns a single `uint32` label array per call

### `CoastalUtils` responsibilities — the temporal segmenter
- Declaring `TEMPORAL_RADIUS` so the base supplies a window of frames around each tile
- Reading the model's manifest for the metric set it was trained on
- Projecting the window to one channel at the *global* photometric scale
- Per-Z 2D + `match_masks_3d` for 3D stacks
- Returns a single `uint32` label array per call

### `predict_slice` signature (current implementation)
```python
def predict_slice(self, tile: np.ndarray, model_params: dict, norm_params: dict | None,
                  context: np.ndarray | None = None, context_index: int | None = None) -> np.ndarray:
    """
    tile: [C, Z, Y, X] for 3D images, [C, Y, X] for 2D
    model_params: one entry from the 'models' JSON dict (with 0-based channel indices)
    norm_params: per-channel (norm_min, norm_max) from normaliseToWhole, or None
    context: [W, ...tile axes] — the same tile through time; ONLY when TEMPORAL_RADIUS > 0
    context_index: index of this timepoint within `context` (not always the middle)
    Returns: uint32 label array [Z, Y, X] or [Y, X]
    """
```

Called once per model per tile in the outer loop. The outer loop in `segmentation_utils.py` handles multi-model iteration and accumulates results by `matchAs`.

---

## Tiling

`_create_xy_tiles(H, W)` generates `(read_yx, write_yx, crop_yx)` tuples:
- `read_yx` — slice with overlap padding (what we read from the zarr)
- `write_yx` — slice without overlap (where we write to the label array)
- `crop_yx` — `(pad_top, pad_bottom, pad_left, pad_right)` amounts to trim from the prediction

After prediction, `_crop_masks` trims the overlap, then `_write_tile_to_arr` merges via `np.maximum`.

Z dimension is handled by cellpose's built-in `stitch_threshold` (2D-per-slice + inter-slice stitch). No explicit Z tiling is done.

---

### Skipping the padding a drift correction added

A drift-corrected canvas holds each frame at its own offset and zeroes the rest, and the whole
z-stack goes to cellpose in **one** call (it stitches across z internally) — so the padding costs
real GPU time and produces nothing. Measured 2026-08-12 across the stores on this machine that carry
a valid box: **24.0% fewer plane-frames** handed to cellpose overall, **55.6%** on the worst image
(8 valid planes in an 18-plane canvas, `kSUFux/PsD5Xc`).

Two qualifications on those numbers, both of which cost more than the difference between them:

- **They are per-store, and 8 of the 25 corrected stores here have no valid box** — every `4kS67f`
  one, all corrected before the box existed. `read_valid_box` returns `None` for them, so the skip
  is a no-op and 20,493 plane-frames still go to cellpose as padding. Across *all* corrected stores
  the saving is therefore **21.6%**, not 24.0%. A store gets the box by being re-corrected; there is
  no backfill (only one of the eight still has its `drift_shifts.json`, and recovering the rest
  would mean deriving the box from pixels).
- **They moved when the drift estimator improved, and will move again.** The figures above were
  28.2% / 63.6% before `multiLag` landed (#524) — a better trajectory needs less canvas, so the
  padding it leaves behind shrinks and the skip has less to skip. Padding fell on 14 of the 17
  boxed stores, held on 3, and rose on one 7-frame image. **Treat these as a snapshot of this
  machine's data, not a property of the feature.**

Per timepoint the frame is narrowed to that frame's valid z span (`docs/ARCHITECTURE.md` → *The valid
box*); tiling, cellpose, post-processing and nuc/cyto matching all run unchanged on the reduced
stack. Only the **write** puts it back at its z offset, so the label store keeps its full shape with
the skipped planes zero.

- **A skip, never a crop.** Each frame sits at its own offset *because* the correction aligned them
  in a shared canvas; cropping per frame would put them back out of register.
- **Safe by construction.** A valid box is a contiguous `[start, stop)`, so narrowing z to it can only
  drop LEADING/TRAILING planes. Interior planes inside the span survive, and the dropped ones are
  all-zero — no labels for `stitch_threshold` to link across, so the stitching semantics inside the
  span are unchanged rather than assumed to be.
- **Ambiguity widens, never narrows** (`_valid_z_span`): no box, no Z, a degenerate range, or a span
  under two planes all fall back to the whole stack. One plane is not meaningfully 3D. Missing cells
  is a real cost; doing the work anyway is only the status quo.
- **The label store records the span it segmented**, so a consumer knows those planes are zero
  because nothing *ran* there, not because nothing was *found*.
- **`clearDepth` changes meaning — deliberately.** It clears labels touching the first and last z
  slice of the array it is given. Before the skip that array was the whole padded canvas, so those
  faces were padding: all zero, nothing to clear, and `clearDepth` was a silent **no-op** on every
  drift-corrected image. With the skip it acts on the real top and bottom of the *acquired* stack,
  which is what the option means. Results therefore change for anyone running `clearDepth` on
  drift-corrected data — cells the padding used to shield are now cleared. Pinned by
  `ClearDepthMeetsTheSkipTest`.
- **Normalisation is unaffected.** `normaliseToWhole` computes its percentile once from the full
  store *before* the timepoint loop, and it already excludes background zeros — so the padding never
  contributed to it and narrowing the stack cannot move it.

Nothing changes for a store that never padded — `read_valid_box` returns `None` and the whole stack
is segmented, which is most images.

## Parameters (cellpose.json → cellpose.jl → cellpose.py)

| Param | Type | Default | Notes |
|---|---|---|---|
| `valueName` | valueNameSelection | "default" | Which image version to segment |
| `outputValueName` | text | "default" | Output subdirectory name |
| `models` | group (repeatable) | see below | One entry per model |
| `models[].model` | select | "cyto3" | Cellpose model type or path to custom model |
| `models[].matchAs` | select | "base" | "base" = primary, "nuc" = nucleus |
| `models[].cellChannels` | channelSelection | [] | Channels for cell signal; merged via np.maximum |
| `models[].nucChannels` | channelSelection | [] | Channels for nucleus signal (passed to cellpose as second channel) |
| `models[].cellDiameter` | int (µm) | 10 | Converted to pixels using OME-XML physical size |
| `models[].normalise` | float | 99.9 | Upper percentile for intensity clipping |
| `models[].medianFilter` | int | 0 | Median filter kernel (0=off) |
| `models[].gaussianFilter` | float | 0.0 | Gaussian sigma (0=off) |
| `models[].threshold` | int | 0 | Absolute intensity gate; pixels below set to 0 |
| `models[].stitchThreshold` | float | 0.0 | Z-stitch threshold (0=2D per slice, no stitch) |
| `blockSize` | int (px) | 512 | XY tile size |
| `overlap` | int (px) | 64 | XY tile overlap; provides border context and seam zone for stitching |
| `labelOverlap` | float | 0.0 | IoU threshold for tile seam stitching; 0 = simple np.maximum merge |
| `blockSizeZ` | int | 0 | Z tile size in slices (0 = whole stack; Z tiling not yet active) |
| `overlapZ` | int | 0 | Z tile overlap in slices (future use) |
| `matchThreshold` | float | 0.3 | IoU threshold for base-nuc label matching |
| `removeUnmatched` | bool | false | Remove base cells with no matching nucleus |
| `minCellSize` | int (px) | 0 | Remove labels smaller than N pixels |
| `cellSizeMax` | int (px) | 0 | Remove labels larger than N pixels (0 = off) |
| `labelExpansion` | int (px) | 0 | Expand label boundaries outward |
| `labelErosion` | int (px) | 0 | Erode label boundaries inward |
| `clearTouchingBorder` | bool | false | Remove cells touching XY image border |
| `clearDepth` | bool | false | Remove cells touching first/last Z slice (3D only) |
| `normaliseToWhole` | bool | true | Use lowest-res level for global percentile (timecourse-safe) |
| `useDask` | bool | false | Load image as Dask array |

The Julia handler converts channel names → 0-based indices before writing params JSON.

### Custom cellpose checkpoints

Cellpose ships four built-in models (`cyto3` / `cyto2` / `cyto` / `nuclei`). Custom checkpoints —
e.g. **`ccia.fluo`**, the fluorescence model that segments dendritic / SHG stroma (upstream of
`segment.branching`) — live outside the code. There are two slots, in **override → bundled**
precedence (matches `bioformats2raw_bin()`):

| Slot | Path | Populated by | Purpose |
|---|---|---|---|
| User drop-in | `<config_dir>/models/cellposeModels/<name>` | You | Drop your own checkpoint here (same convention as [custom modules](CUSTOM_MODULES.md)) |
| Bundled | `<install root>/models/cellposeModels/<name>` | `install.sh` / `install.ps1` / `pixi run models-fetch` | The shared set fetched from [`schienstockd/ceceliaModels`](https://github.com/schienstockd/ceceliaModels) |

A user-drop of the same filename **wins** over the bundled copy — so you can fine-tune
`ccia.fluo` and replace it locally without touching the repo/install.

#### Drop-in convention (no rebuild)

1. Copy the file into either slot. `<config_dir>` is `~/.cecelia` on an installed app; in dev
   it's whatever `.env`'s `CECELIA_DEV_DIR` points at.
2. Open the Segment page's Cellpose form. The **Model** dropdown enumerates the built-ins plus
   every file it finds in both slots — no restart, no code, no register call.
3. Select the model, hit Run. The task passes.

Under the hood: `list_cellpose_models()` (in `app/src/config.jl`) enumerates every option;
`_inject_dynamic_options!(::CellposeSegment)` (in `app/src/tasks/segment/cellpose.jl`)
rewrites `cellpose.json`'s Model select's `options` list at spec-load time (via a `_task_spec`
dispatch hook in `app/src/tasks/task.jl`). Because `validate_params` reads through the same
`_task_spec`, and `/api/tasks/definitions` re-runs the same hook on each request, **the picker
and the validator agree** — a dropped-in file is selectable AND accepted.

At run time the Julia handler resolves the selected name to an absolute path via
`cellpose_model_path(name)`, and `cellpose_utils.py::_get_model` picks it up through cellpose's
`pretrained_model=<path>` branch. Missing file → clear `[ERROR]` before dispatch.

#### Fetching the shared set

```bash
pixi run models-fetch     # schienstockd/ceceliaModels master → <repo>/models/cellposeModels/
```

Override the ref with `--ref v1.2` or the destination with `--dest /some/path`. The installers
do the same fetch at install time (`CECELIA_MODELS_REF` env var to pin a ref there). Only
`cellposeModels/` is installed — the feijoa btrack task uses a vendored config beside its
runner, so `btrackModels/` from upstream is skipped.

#### Design notes

- **~26 MB per checkpoint** is too large for the app tarball (which is ~6 MB), so we borrow
  bioformats2raw's install-time-fetch shape. See `docs/SHIPPING.md`.
- The dropdown is **enumerated live** — adding a checkpoint after the server is running only
  takes a page reload of the Segment page; no restart needed. If you swap the file (same name,
  different bytes), the task picks up the new file on the next Run (cellpose caches per model
  path within a task-runner process, but that process is short-lived).
- Names are the **filename verbatim** — no `.pt` implied. Whatever cellpose can load
  (`.pt` weights, no-extension checkpoints, etc.) works.
- Dotfiles (`.DS_Store`) and subdirectories are ignored during enumeration.

---

## Optical flow (coastal) — `segment.coastal`

Segments by **motion** rather than appearance: a UNet is fed optical-flow metric planes derived from
a window of frames around the timepoint, and its embedding head drives region growing. Built for
intravital data where cellpose does not work — a cytoplasmic reporter in a photon-limited movie has
no consistent outline to find, but a cell that moves as a unit is visible in the flow field.

Everything structural is shared with cellpose: the same base class, tiling, streaming, seam
stitching, post-processing, label store and QC. Three things are specific.

**A window, not a frame.** `CoastalUtils` sets `TEMPORAL_RADIUS`, and the base reads that window per
TILE rather than per frame — widening the per-timepoint read would hold `2r+1` full frames in RAM
(~300 MB each on a 1036×1055×35×4ch uint16 movie) and destroy the property that peak memory is one
frame. Windows are **truncated** at the ends of a movie, never reflected: a mirrored frame invents
motion that was not imaged.

The radius is `max(temporal_scales)`, one MORE than the largest lag actually indexed. That extra
frame is not slack — coastal drops `mag_{scale}` when the window is shorter than `scale+1`, and a
missing plane does not leave a hole at its own channel: `predict_frame` stacks in `sorted(key)` order
and zero-fills the remainder, so every later metric shifts down a slot and the model silently reads
misaligned inputs. At `r = max(scales) - 1` the truncated window at `t=0` is exactly one frame short.
An image with fewer than `r+1` timepoints raises rather than segmenting.

**The metric set comes from the model, not the params.** A model is a pair — `<name>.pt` plus a
`<name>.json` manifest recording `temporalScales`, `cumulativeWindow`, `droppedMetrics`, the source
image and channel. Inference MUST use the set the model was trained on, and the failure above is why
that cannot be left to the user re-entering it. A checkpoint with no manifest still loads, with a
`[WARN]`, and falls back to coastal's training defaults.

**Global photometric scaling is required, not optional.** Training normalises by the whole movie's
min/max. `normaliseToWhole` supplies that statistic; the projection then pins coastal's own scaling
to the same range. With it off, every tile gets its own scale — the patchiness the option exists to
prevent, plus a train/inference mismatch on the structure-tensor planes.

**Training reads `zPlanes` planes per movie, and each is its own sequence.** Motion exists within one
recording of one plane; flow between plane *k* and plane *k+1* of the same timepoint is not motion,
so a (movie × plane) pair is a sequence and metrics are computed per sequence before pooling. The
planes are the centres of `zPlanes` equal bins, which puts `zPlanes = 1` on `n_z // 2` (the old
single-plane rule, unchanged) and keeps larger counts off plane 0 and `n_z - 1` — the top and bottom
of an intravital stack are usually outside the tissue, and `linspace` would spend two of five planes
there. Indices are resolved per movie, since depth is: the manifest records the count as `zPlanes`
and the actual indices per image as `zPlanesUsed`, because "3 planes" of a 31-deep stack and of a
9-deep one are different tissue.

Pooled frames are movies × planes × timepoints and the metric stack is ~11–15 float32 planes per
frame, so the plane count is a straight memory multiplier.

**`maxFrames` caps what each movie contributes, because pooling is otherwise weighted by recording
length.** A 200-frame movie contributes ~7× what a 30-frame one does, so without a cap the model is
mostly fitted to whichever image the microscope was left on longest — and nothing showed it, since
the frame count is a single pooled number. The window is **contiguous** (the metrics are temporal;
a random subset of frames is not a shorter movie, it is a movie with the motion taken out) and its
start is **derived from the seed and the movie index** — reproducible from the manifest, different
across runs, and stable when images are added or reordered. Always starting at frame 0 would sample
one part of every experiment, as often as not before the interesting event.

Two ordering constraints, both silent when broken:

- **Normalise over the whole movie, then cut.** The percentiles are the global statistic
  `normaliseToWhole` reproduces at inference. Cutting first would scale a 50-frame window by its own
  percentiles while inference scales the 200-frame movie by the movie's — the same structure at a
  different brightness.
- **Check `max(scales) + 1` against the CAPPED length.** A 200-frame movie capped to 5 produces no
  `mag_8` plane, which corrupts the pooled channel layout exactly as a genuinely short movie would.

The manifest records `maxFrames` and `frameWindows` (uID → `[start, stop)`, only for the movies
actually cut).

**`trainRatio` holds part of every sequence back, and without it the loss curve cannot be read.** A
training loss is measured on the frames the weights were just fitted to, so it goes down whether the
model learned what a cell looks like or memorised these frames. With a split, coastal evaluates every
term on the held-out frames each epoch (no grad, no augmentation) and the manifest's `lossCurves`
gains a `val_<term>` beside each one; the **gap** between the pair is the reading.

The split is `train_test_split_per_movie`, which cuts *within* each sequence — so every movie and
every Z plane appears on both sides. Holding whole movies out would ask "does this transfer to
another recording", a different and much harder question. The held-out frames are the tail of each
sequence, a stretch the optimiser never saw.

One honest caveat: the metrics are computed over the full sequence *before* the split, so a held-out
frame's flow metrics derive partly from frames that ended up in training — roughly `max(scales)`
frames of overlap at the seam. That is not label leakage (coastal is unsupervised; there are no
labels) and the held-out frames themselves never reach the optimiser, which is what the comparison
rests on. Computing the metrics per side instead would change the metrics at *both* seams, which is
worse.

**Two canvas plots, one route.** `POST /api/optical-flow/inspect` answers with the metric planes
when no model is given and with the model's probability map when one is. Same window, same
projection, same metric build — one forward pass is the only difference — so they share the route and
the request machinery (`useFlowPlanes`) rather than getting a second copy of the geometry that could
drift from what a run is fed.

| Plot | Question | Model |
|---|---|---|
| **Flow metrics** | which of these look like cells, i.e. what should I train on | none, deliberately — the question predates any checkpoint |
| **Model probability** | did it learn to tell cell from background | the vault's selection, honouring global/local scope |

Both carry an **image-version selector**, and its default differs on purpose. The metric sheet has no
model, so it defaults to the image's ACTIVE version — what a task form resolves to and what the viewer
shows. The probability plot defaults to the version the MODEL was trained on, read from its manifest's
`sourceValueName`, because that is by definition the right input; pick another and the panel says
"not trained input". Both hardcoded `default` until 2026-08-07, which fed a model trained on a denoised
movie the raw import — a different photometric world, with nothing on screen saying so.

Neither shows instances: those are segmentation output, the Segment page previews them through the
normal path, and a threshold plus a growing step hides exactly what the probability map is for.
`predict_frame` returns `(prob_map, instances, props)` and a run discards the first
(`CoastalUtils._predict_plane`), so the worker's `opticalFlow.probability` backend is the only place
that value is looked at.

Progress is reported over one monotonic scale — a tick per movie prepared, one for the flow metrics,
then one per epoch through coastal's `on_epoch`. The phases are wildly unequal in wall-clock (metrics
is a single tick and minutes long) so the bar does not move smoothly; weighting them would be a guess
dressed up as a measurement. Before this the task emitted no `[PROGRESS]` at all, so a run of tens of
minutes was indistinguishable from a wedged one.

The convergence plot draws each `val_` curve dashed in the same colour as its term — the only thing
read off a validation curve is its distance from its own training line, and a second colour would
make that a legend lookup. QC banks `valFinalLoss`/`valLossDrop` and warns when the held-out loss
does not come down even though the training loss did.

**The vault.** `<config_dir>/models/coastalModels/`, same drop-in convention as `cellposeModels/`
above and the same live enumeration, with two differences: there is nothing built in and nothing
bundled (an empty vault means a picker with only "None"), and only `.pt` files are entries — the
`.json` manifests sit beside them. Config-dir models do **not** travel with a `.ccbundle` export, so
a shared project can name a model this machine lacks; the task fails with that message rather than
falling back to another model.

**The border is a coastline, and `labelSmoothing` is the right answer to it.** Region growing makes
per-pixel decisions on the affinity field, so the frontier comes out fractal — measured on real
output: median roughness 1.47, p90 1.96, worst 3.31, where roughness is perimeter over the perimeter
of a circle of equal area and a perfect disc on this pixel grid scores 1.01.

Two source-level fixes were tried and rejected on measurements, so do not re-try them without new
evidence:

* `probBlurSigma` does **nothing** to the border (1.44 → 1.43 → 1.49 → 1.55 at σ 0/1/2/3 — it gets
  worse). The coastline is not the thresholded probability mask being ragged.
* `embeddingBlurSigma` genuinely cleans it, by eating the dim periphery. Median object diameter goes
  10.3 µm → 8.9 → 7.7 at σ 1.5 → 3.0 → 5.0, against an expected cell of ~11 µm, and the whole-frame
  area roughly halves. A clean border on a cell too small to be a cell is a worse answer than an ugly
  border on the right one. Confirmed by eye in napari across four label versions.

So coastal defaults to `labelSmoothing` **0.5** — cosmetic, honest about being cosmetic, and it keeps
the size. (This paragraph read 1.5 until 2026-08-07. The shipped spec has always been 0.5 and 0.5 is
the intended value: the doc was wrong, not the spec.)

`embeddingBlurSigma` defaults to the calibrated **1.5** (2026-08-07) — the lowest σ in the table above
and the one holding cell size closest to the expected ~11 µm, and independently coastal's own tuned
`embedding_blur_sigma` for both passes. `probBlurSigma` stays at **0**: the same measurement shows it
makes the border *worse*, so its calibrated value is off.

Cellpose keeps `labelSmoothing` 0.0: it has no growing frontier and therefore not this failure mode,
and changing a shipped task's default would alter existing pipelines.

**Coastal's spatial params are in MICRONS, not pixels.** A seed window or a blur radius describes a
CELL, so the same number has to mean the same biology on every image of a set — a px value silently
means something different the moment the zoom changes, which defeats the one-value-per-set rule.
`SegmentationUtils.px_from_um` / `px_area_from_um2` are the single conversion point (they also back
cellpose's `cellDiameter`), and coastal's own API stays in pixels, correctly: it is an array library
and knows nothing about calibration.

`minCellSize`, `cellSizeMax`, `labelExpansion` and `labelErosion` followed (2026-08-07) — they are
base params shared with cellpose, so this **reinterprets any value an existing pipeline had saved**:
a `minCellSize` of 50 used to mean 50 px and now means 50 µm². Done deliberately rather than left as
a mixed-unit form. Sizes are AREAS (µm², converted by pixel area — dividing by the pixel *size*
instead is silently 2× off at 0.5 µm/px), and a radius the user set never rounds down to "off".

Still px, and correctly so: `blockSize` and `overlap`. Those describe the TILING, which is about
memory and array layout, not about cells.

**Run `segment.coastalMeasure`, not `segment.coastal`.** Same as cellpose: the bare segmenter writes
label stores and nothing else, so there is no `.h5ad` and therefore no gating, tracking or analysis
downstream. The composite (`segment.coastal` → `segment.measureLabels`) is what the Segment page is
for, and it is the entry a user should normally pick.

**Two passes = two model groups.** Not a coastal feature. The repeatable `models` group already is
the stacking UI, and `_write_tile_to_arr` fills only unlabelled pixels, so a second group picks up
what the first missed without overwriting it. Splitting cells from apoptotic bodies afterwards is a
**gating** decision, not a segmentation parameter.

**Measured, first end-to-end run** (`zolIMa/fXgbTl`, 31 T × 32 Z × 420 × 441, mem-TOM, one tile):
training 67 s; segmentation **689 s**; 6220 objects, 175–222 per timepoint with no drift. On the
mid-plane that is 37/28/34 objects against an intensity watershed's 33/29/29 — within ~10%, with
visibly better separation of touching cells. Cost scales with pixels × planes, so the uncropped
`Dml3RG` (~5.9× the XY area) is over an hour for the same frame count: this is an overnight task,
which is what the preview is for. The size distribution is bimodal — mode 4–5 µm against an 11 µm
cell — i.e. the small-particle population dominates by COUNT; splitting those off is gating, not a
segmentation parameter. Reproduced by `docs/todo/flow-seg-experiments/first_task_run.py`.

Design record: [`docs/todo/COASTAL_SEGMENTATION_PLAN.md`](todo/COASTAL_SEGMENTATION_PLAN.md);
evidence and dead ends: [`docs/todo/SEGMENTATION_OPEN_PROBLEM.md`](todo/SEGMENTATION_OPEN_PROBLEM.md).

---

## Napari integration

Labels are shown via `show_labels!` (Julia) → `show_labels` (napari_bridge.py):
- `value_name` — the output value name (used as filename stem)
- `label_files` — list of zarr filenames to load (e.g. `["default.zarr", "default_nuc.zarr"]`)
- Each file is loaded from `{task_dir}/labels/{filename}` and becomes a napari Labels layer named `({filename}) Labels`
- Missing files are silently skipped (e.g. single-model run has no `_nuc.zarr`)

The toggle in ViewerPanel:
- `showingLabels` defaults to `true` and is a sticky preference (not reset on image switch)
- On `POST /api/napari/open`, if `showLabels=true` is included, labels are shown as part of the same request so WS messages are ordered: `set_task_dir` → `open_image` → `show_labels`
- The standalone toggle uses `POST /api/napari/show-labels`

### Previewing a running run

A segmentation's label store can be watched **while it is still being written**. That works because of
how `predict_from_zarr` streams: the store is created at its full final shape before the first frame,
then filled one timepoint at a time, and every per-frame step (seam stitching, size filters, nuc/base
matching) runs *before* the frame is written. So a completed frame in the preview is the **final** label
data, not a provisional pass.

What makes it discoverable is the task, not the image: `ccid.json` registers a label set only when the
run **succeeds**, so mid-run there is no `labels` entry to put in a picker. Instead the task declares
its in-flight output via the `live_outputs` trait (`app/src/tasks/task.jl`), the scheduler records that
on the `TaskRecord` at submit time, and `GET /api/tasks` publishes it. The frontend reads that
snapshot on every task lifecycle event (`liveLabelPreviews` in `utils/napariAutoShow.ts`) and offers a
⚡ toggle per in-flight store in the ViewerPanel segmentations list.

Declaring it is opt-in per task, because writing-as-you-go is a property of the backend:
`segment.cellpose` streams (`live_outputs(::CellposeSegment, params) = segment_live_outputs(params)`),
while `segment.branching` assembles its store in RAM and writes it once at the end, so it correctly
declares nothing.

The preview reads the run's **staging** store, not the final path — see *Stores are written staged*
above. This matters on a re-run: until the run completes the final path still holds the PREVIOUS
segmentation, so a preview aimed there would quietly show the old labels while the new ones are being
computed. `live_outputs` therefore declares the `.partial` filenames while carrying the plain
`value_name` alongside them, because the viewer names the layer from the value_name (`({vn})` is what
the recolour and layer-eviction logic match on), not from the file.

Three things differ from a normal labels layer, all forced bridge-side rather than trusted to the caller:

- **Level 0 only.** A label store declares its whole pyramid in `.zattrs` when created, but levels 1…N
  only exist after `_finalize_label_pyramid` runs at the end. Asking for the image's level count
  therefore raises `KeyError: '1'`. The preview renders full-resolution at every zoom — the honest cost
  of watching an unfinished store, and why it is a manual toggle rather than automatic.
- **Caching off.** The point is to see bytes that changed, and napari's `cachey` would serve the old
  ones (see `napari_utils.add_labels` on why dask task names make that cache dangerous for re-run
  labels specifically).
- **A refresh may find the store gone.** The finishing run renames the staging store onto the final
  path, so a throttled refresh tick can lose the race. `refresh_labels` treats that as benign and skips
  — the run has just finished and the task-finished handler is about to swap in the real layer.

The preview layer is namespaced `({vn}) Labels (live)` and a store holds at most one layer of its family
at a time: adding the finished set evicts its own preview, and vice versa (`_LABEL_SUFFIXES` in
`napari_bridge.py`). Progress ticks drive `POST /api/napari/refresh-labels`, which reassigns
`layer.data` from a fresh view in place — throttled to one read per 2 s, since cellpose emits a tick per
XY tile. The toggle is deliberately **not** persisted: it describes a store that exists only while one
task runs, so restoring it later would produce a dead toggle for a layer the bridge can only skip.

### Previewing params BEFORE a run (the task preview)

A different thing from the section above, and the two are easy to confuse. *That* preview watches a run
that is already going. **This one runs no task at all**: it executes the segmentation's own compute over
the one region napari is showing, so params can be judged in under a second instead of by waiting out a
full run and looking at the result. Full design + every measured number:
[`docs/todo/TASK_PREVIEW_PLAN.md`](todo/TASK_PREVIEW_PLAN.md).

| | Live preview (above) | Task preview (here) |
|---|---|---|
| What it shows | a run in progress | what a run *would* produce |
| Runs when | a task is running | on demand, no task submitted |
| Layer | `({vn}) Labels (live)` | `({vn}) Preview` |
| Backed by | the run's staging store | nothing — an in-memory block |
| Scope | whole image, as it fills | ONE z-plane of the visible region |

**Where the compute happens.** `preview/preview_worker.py`, a resident process on **:7656** (like the
napari bridge and Pluto, on the un-pooled `jobs.jl` rail — a preview that queued behind a full
segmentation would not be a preview). Resident because a process that can segment costs **17.7 s** of
imports before it can answer: fatal per preview, irrelevant once. It calls `CellposeUtils.predict_slice`
— the same method the full run uses — so a preview cannot drift from the thing it previews.

**Nothing is written to disk.** The worker returns the mask block (`cecelia.utils.block_transfer`) and
the bridge builds a full-label-extent lazy array with that block placed in it, so the layer aligns with
the image by shape alone with no `translate`. An earlier design wrote a never-promoted scratch store;
it needed its own staging lifecycle and left debris the sweep's active-window heuristic then refused to
collect. A preview is a picture, not data.

**The params are prepared exactly as a RUN prepares them — `preview_params_for_run` is the one entry
point.** Two steps, and each has been a live bug:

1. **`section` sub-params are lifted flat** (`_flatten_sections`, what `run_task` does). A section is a
   UI grouping, so the form holds its params nested and every `_run_task` reads them flat.
2. **Then the task translates its own params** (`preview_params`) — cellpose resolves channel NAMES to
   0-based indices and a custom model name to a checkpoint path.

Sharing `predict_slice` does NOT make the params shared. Skipping (2) produced `ValueError: invalid
literal for int() with base 10: 'CH3'`. Skipping (1) produced nothing at all, which is worse: Python's
`params.get(k, default)` filled every gap silently, so `blockSize` fell back to 512 and the preview
reported a tile seam on a run configured for 4096, while `normaliseToWhole=false` was ignored and the
preview normalised differently from the run. **Design against the silent default** — one entry point,
not a fix per param. The frontend flattens too (`TaskRunner.previewParams`), because its own warnings
read the same params; the backend still flattens because the chain path persists them nested.

**Opt-in per task**, the `live_outputs` shape: `task_previewable(::CciaTask) = false` is the base, a task
overloads it beside its struct, and there is a `CompositeTask` overload because the module page runs
`segment.cellposeMeasure`. `GET /api/tasks/definitions` stamps it onto each spec as `previewable`.

**What it does NOT tell you** — all three surfaced in the UI rather than left to be discovered:
- **One z-plane.** A visible z-stack costs ~89 s with no shortcut (cellpose rescales to a canonical
  diameter, so cost tracks CELLS, not pixels — a coarser pyramid level buys only 2.5× for 16× fewer
  voxels). In 3D display mode it previews the current plane and says so: *"2D preview only — diameter,
  boundaries and splitting match the run; counts and z-extents will not (no z-stitching)"*.
- **Base model only.** The nucleus pass and `_match_nuc_cyto` don't run. With `removeUnmatched` the
  matching *deletes* base labels that found no nucleus, so the run finds **fewer** cells than the
  preview shows — the warning says which case you're in. Not run because `_compute_iou_matrix` is
  quadratic: 1.8 s at 100×100 labels, **26.9 s at 400×400**, against a 0.14–0.38 s preview (see
  `docs/TODO.md` → *`_compute_iou_matrix` is quadratic in cell count* — the real pipeline pays this per
  timepoint too).
- **Not tiled like the run.** `SegmentationUtils` tiles at `blockSize` and re-stitches labels split
  across each seam; the preview segments the visible region as ONE tile. Where a seam would cross the
  region the run's mask is two inferences plus an IoU re-join and the preview's is one, so counts and
  boundaries near it differ — flagged as *"Run would tile this"*. The test is positional
  (`_run_tile_seams`): the grid is anchored at the image origin, so a 600 px region inside one 1024 px
  tile has no seam while a 300 px one straddling y=512 does.
- **"0 cells" is qualified.** A drift-padded plane returns 0 and otherwise looks exactly like too large
  a diameter, so the worker checks `zarr_utils.read_valid_box` and reports `hasSignal`/`noSignalWhy` →
  *"No image data here"* (padding) or *"Region is blank"*.

**The label modifications DO run** — `post_process` (erosion, expansion, the size filter, border
clearing) is applied to the previewed plane, so tuning `minCellSize` or `labelExpansion` changes what
you see. It is the run's own method, called with `la_t=None, T=1` (the whole-array branch the run uses
per frame), and the count is taken *after* it so the readout matches the mask.

It is **crop-aware**, which is the part that needed designing rather than plumbing. Two steps read the
array edge as the image edge, and on a visible region it usually isn't — both errors showing fewer
cells than the run produces: `clearTouchingBorder` would clear every cell at the crop edge (worse the
more you zoom in), and the size filter would judge a cell on its *clipped* pixel count. The worker
passes `real_border` (derived from the region bounds vs the axis lengths), so clearing happens only at
genuine image edges and clipped labels are exempt from the size filter. `real_border=None` — what the
run passes — is exactly the old behaviour, pinned by tests on both sides. Residuals: `labelExpansion`
stays approximate at a crop edge (fixing it needs a halo, which would change what the preview reads for
an edge-only cosmetic difference), and `clearDepth` needs a stack so a one-plane preview can never
apply it — covered by the 2D warning.

**It never guesses which image it is looking at.** `GET /api/preview/status` exposes the open image, and
`/api/preview/run` *checks* the caller's `imageUid` rather than using it to select — mismatches are 409s.
The region and the pixels come from the same store by construction, because a drift-corrected store is
padded larger than its source and pairing one's region with the other's pixels would silently preview
the wrong area.

A **mismatch refusal is amber, not muted text**: `version-mismatch` and `image-mismatch` are the two
states that look exactly like a working preview of the wrong pixels, so they go through the severity
model (`frontend/src/utils/taskPreview.ts` → `previewNotice`) alongside the four warnings above. The
backend owns the explanation — it knows which version is open and which the task reads, so its message
is the tooltip detail and the frontend supplies only the short label, keyed on `code`. What stays quiet
is setup the user can already see: no image open, no model chosen. Amber for those too would just teach
people to ignore amber.

**Two things the preview must never do: keep previewing forever, and look busy forever.** The re-preview
trigger is deduped at its source (`docs/NAPARI.md` → `viewChanged`), the pin drops the queue the moment
it is set (`dropPending`, not `cancel` — the run in flight is the freshest and its mask is the one on
screen), and `/api/preview/run` is deadlined at `PREVIEW_RUN_TIMEOUT_MS` so a wedged worker or viewer
surfaces as *"Preview timed out"* instead of a permanent "Previewing…".

---

## Measure labels

`segment.measureLabels` reads label zarrs and the intensity image and writes per-cell measurements as an AnnData `.h5ad` file. It is a standalone task and also the second step of the `segment.cellposeMeasure` composite.

```
Julia (measure_labels.jl)
  → resolves intensity image path from ccid.json "filepath"
  → reads label zarr paths from ccid.json "labels[outputValueName]"
  → writes JSON params to {task_dir}/tasks/measureLabels.{id}.params.json
  → spawns  app/src/tasks/segment/measure_run.py as subprocess
  → on success: writes {outputValueName} → "{outputValueName}.h5ad" to ccid.json "label_props"

Python (measure_run.py)
  → opens each label zarr; derives label type from filename suffix
  → constructs MeasureUtils(params, dim_utils)
  → calls measure_from_zarr(label_zarrs, im_dat, log)

MeasureUtils.measure_from_zarr
  → outer loop: T timepoints
  → per timepoint: load base label volume + image
  → optional gaussian pre-smooth
  → regionprops_table for morphology (2D: full set; 3D: skimage subset)
  → per-channel mean or median intensities (base label, then secondary types)
  → if extendedMeasures=True: trimesh marching-cubes mesh per cell
      → surface area, volume, convex hull, sphericity, ellipsoid axes
      → optionally save .stl meshes to {task_dir}/meshes/{valueName}/t{NNNN}/
  → build DataFrame → AnnData → write .h5ad
```

### Output

| File | Contents |
|---|---|
| `{task_dir}/labelProps/{outputValueName}.h5ad` | AnnData: obs=cell labels, X=all features, obsm["spatial"]=centroids, obsm["temporal"]=timepoints |
| `{task_dir}/meshes/{outputValueName}/t{NNNN}/{label}.stl` | Per-cell trimesh (only when `saveMeshes=true`) |

### ccid.json `label_props` field

```json
{
  "label_props": {
    "default": "default.h5ad"
  }
}
```

Flat `Dict{String,String}` — value_name → h5ad filename. Written by `measure_labels.jl` on completion; read by `_load_image` into `CciaImage.label_props`.

### Morphology properties

The derived shape descriptors are ported from the old R `measure_utils.py` so the two versions agree
(`oblate`/`prolate` are **axis-length ratios**, not a circularity proxy). `bbox` is **not** saved — it's a
structural extent, not a QC measure, and nothing reads it.

**2D**: area, perimeter, eccentricity, orientation, major/minor axis, solidity, feret diameter, convex area, equivalent diameter, extent → derived: `oblate` (minor/major), `prolate` (major/minor), `aspect_ratio` (major/equivalent_diameter), `perimeter_to_area` (perimeter²/area), `fill` ((convex−area)/convex).

**3D basic** (skimage, no mesh): area (voxel count = volume), extent, `equivalent_diameter_area`, `euler_number`, inertia tensor eigenvalues → derived: major/interm/minor axis lengths **and** the ellipticity ratios `ellipticity_oblate` (minor/major), `ellipticity_prolate` (major/minor), `ellipticity_interm_oblate` (minor/interm), `ellipticity_interm_prolate` (interm/minor). Axis ratios come from the moments — no mesh — so a plain 3D segmentation gets oblate/prolate too. `solidity` is **not** available here (needs a convex hull → extended).

**3D extended** (`extendedMeasures=true`, requires trimesh): `surface_area`, `volume_mesh`, convex hull area/volume, `solidity` (mesh/hull volume), `sphericity`, `compactness`, `surface_to_volume`, `feret_diameter_max_mesh`, ellipsoid axis lengths from convex-hull vertex PCA, and the same `ellipticity_*` ratios (computed from those axis lengths — one formula for both paths). Overrides skimage axis lengths with ellipsoid-fit values.

### $include template system

The `imageTiling` param section in all task JSONs under `segment/` is shared via `{"$include": "imageTiling"}` which splices in `app/src/tasks/fragments/imageTiling.json` at spec load time (resolved in `_task_spec` via `_resolve_spec_includes`). Cellpose and measureLabels share the same `blockSize`, `overlap`, `blockSizeZ`, `overlapZ` definitions with no duplication.

---

## Branching (skeleton analysis)

`segment.branching` skeletonises an existing segmentation into a **branch/path network** for fibrous
non-cell structures (SHG collagen, FRC networks, dendritic stroma, nerves). Task files:
`app/src/tasks/segment/branching.{jl,json,_run.py}`.

Pipeline:
1. Load the input labels zarr (from `img.labels[value_name]`).
2. **Optional** `refPops` mask: Julia resolves the selected population to a label-ID list via
   `resolve_pop_type` + `cells_in_pop` (Decision 7) and hands it to Python — no gate evaluation
   crosses the language boundary.
3. **Optional** T-collapse (`integrateTime` + `integrateTimeMode`), Z-MIP (`flattenBranching`)
   and/or label-boundary conversion (`useBorders`). `integrateTime` gives **one network for the
   whole movie** instead of one per frame — much cheaper, and the right input when you want a
   single spatial summary to correlate against tracks pooled over time. Neither mode is a default;
   both are scientifically valid. Note the label stack always collapses by MAX (a union of where
   structure existed) — the *average* of a label image is meaningless, so `integrateTimeMode`
   applies only to the raw channel.
4. Per timepoint: binary closing (`preDilationSize`) → `skimage.morphology.skeletonize` → optional
   dilation (`postDilationSize`) → `skan.Skeleton` + `skan.summarize(separator='-')`.
5. Write a skeleton labels zarr at `{proj}/1/{uid}/branchLabels/{value_name}.zarr` (a **separate**
   registry from `labels/` — see below).
6. Write a per-branch labelProps sidecar at `labelProps/{value_name}__branch.h5ad`: one row per
   skeleton path, `X` = skan's measurements (`branch-distance`, `branch-type`, endpoint indices,
   Euclidean distance, etc.), `obsm['spatial']` = median of the branch's two endpoint coordinates
   (Decision 8), `obsm['temporal']` = `centroid_t` on timeseries.
7. **Optional** anisotropy (`calcAnisotropy`): local structure tensor via
   `skimage.feature.structure_tensor` at the `Smoothing scale`, aggregated over `Grid spacing`
   boxes, eigendecomposed (both params are µm — see below). All the maths lives in
   `cecelia.utils.anisotropy_utils`. Algorithmic ancestry: Li et al. 2023 (ILEE_CSK).

   Written into the branch sidecar's `uns`, all under one `orientation_` prefix (2D shapes shown;
   3D is the same family with an extra leading spatial axis):

   | key | shape | what |
   |---|---|---|
   | `orientation_coords` | `(T, ny, nx, 2)` | box centre `(y, x)`, in PIXELS |
   | `orientation_eigval` | `(T, ny, nx, 2)` | eigenvalues, **ascending** `[λmin, λmax]` |
   | `orientation_eigvec` | `(T, ny, nx, 2, 2)` | eigenvectors as ROWS — `[…, i, :]` ↔ `eigval[…, i]` |
   | `orientation_box_length` | `(T, ny, nx)` | skeleton pixels in the box (the length weight) |
   | `orientation_box_coherence` | `(T, ny, nx)` | `(λmax − λmin)/(λmax + λmin)` ∈ [0,1] |
   | `orientation_summary` | dataframe, one row per frame | occupancy, cv, skewness, `MF_full_length`, `branching_act`, `anisotropy` |
   | `orientation_meta` | dict | `box_size_px`, `sigma_px`, `source`, `scale_um_per_px`, `flattened`, `t_index`, `eigvec_layout`, `eigval_order`, `fibre_direction` |

   > **Renamed from `ilee_*` (2026-07-29).** The old names claimed an ILEE lineage the arrays do
   > not have — see the box below — and `ilee_box_anisotropy` was per-box *coherence*, not the
   > per-image `anisotropy` scalar, which made the two easy to confuse. Runs banked before the
   > rename keep the old keys; re-run the task to read them.

   `t_index` maps stack position → real timepoint (`[-1]` when `integrateTime` collapsed it).
   **Never infer the timepoint from the position** — the pass used to skip empty frames, which
   silently shifted the whole T axis.

   > **The fibre direction is the MINOR eigenvector.** The structure tensor measures intensity
   > *gradients*, which are largest ACROSS a fibre, so its dominant eigenvector points
   > perpendicular to the structure. Always read the direction through
   > `anisotropy_utils.fibre_orientation`; never index `orientation_eigvec` by hand. The old ILEE
   > tangent tensor used the OPPOSITE convention (major = fibre), so these arrays are shape-alike
   > but **not** index-compatible with the old R vignettes — `uns['orientation_meta']` records
   > `eigval_order`, `eigvec_layout` and `fibre_direction` so a reader never has to guess. A 90°
   > error here is silent: the arrows still look like a plausible field.

   `anisotropySource` picks what the tensor reads — `skeleton` (default), `mask`, or `channel`
   (needs `fibreChannels`). The segmentation-derived sources are denoised and measure closest to
   the legacy skeleton-only estimator; `channel` is the only one that survives a bad segmentation.

   **The two scales are in µm, and that is what you set.** `Smoothing scale` and `Grid spacing` are
   physical, converted to pixels by the Julia handler using the image's own `PhysicalSizeX`. A fibre
   is ~2 µm thick whatever objective took the picture, whereas "12 px" means something different on
   every image — and a cohort with mixed calibration was silently incomparable before. The pixel
   values actually used, the µm asked for, and the µm/px used to convert are all recorded in
   `uns['orientation_meta']` (`sigma_px` / `sigma_um` / `um_per_px`, likewise for the box).

   > On an image with **no pixel size**, `PhysicalSizeX` resolves to 1.0, so the µm numbers land as
   > pixels. That is a QC `warn` (`branching.uncalibrated`), not a silent substitution.

   **What to actually put there.** This is the question the params look easy to get wrong on:

   | | what it controls | how to pick it | what going too low does |
   |---|---|---|---|
   | **Smoothing scale** (σ) | the neighbourhood the orientation is measured over | start near the **fibre thickness**, raise for thicker or noisier structures | reads pixel noise as structure — the field goes incoherent and the arrows stop agreeing with their neighbours |
   | **Grid spacing** (box) | how finely the field is **sampled** | as fine as you want to *look* at, then check the logged size | nothing statistical, but the stored grid explodes: boxes scale as **1/box²**, so halving the spacing **quadruples** the file |

   Those two failure modes are different, which is why the params are separate. Measured on EaMaVq
   (0.596 µm/px, 201 frames), alignment is the median angle between the field and each branch's own
   direction — lower is better:

   | grid spacing | grid | alignment | stored grid |
   |---|---|---|---|
   | 27 µm | 12×12 | 24.3° | 1.2 MB |
   | 9 µm | 36×36 | 16.7° | 10 MB |
   | **5 µm** | **68×68** | **14.9°** | **37 MB** |
   | 3 µm | 108×109 | 14.8° | 95 MB |
   | 1.8 µm | 181×182 | 14.5° | 265 MB |

   So finer genuinely *is* better here, with no noise penalty — σ already did the smoothing, and the
   only cost is file size. Alignment flattens out below ~5 µm, which is why that is the default.
   **The run logs the resulting grid and its size**, and warns past 100 MB
   (`branching.aniso_grid_large`) — read that rather than guessing.

   **Defaults are measured, not guessed** (EaMaVq SHG, scan in
   `docs/todo/SPATIAL_ANISOTROPY_PLAN.md` Decision 4): `skeleton`, σ = 7 µm, box = 5 µm. Tune σ on
   *direction contrast* (`anisotropy_utils.direction_contrast` — near-vs-far agreement), never on
   coherence and never on neighbour agreement alone: both are trivially "improved" by blurring the
   field into uselessness.
8. QC: `nBranches` / `nSkeletons` / `meanBranchLength`, plus `anisotropy` when the pass ran;
   `branching.no_branches` warn for empty output. Cohort metrics: `nBranches`,
   `meanBranchLength`, `anisotropy`.

   `anisotropy` is the per-image structure readout — a **length-weighted** mean of per-box
   coherence (ILEE's `by_length`), 0 = uniform, 1 = non-uniform. Real fibrous tissue sits around
   **0.1–0.4**, so a low number is not a defect. Weighting matters: an unweighted mean counts
   empty background boxes equally and drifts with how much blank field an image contains.

**Reading it back.** There is **no anisotropy plot in the app** — it is figure-shaped, so it lives
in a notebook. Three package accessors (`app/src/anisotropy.jl`) return tidy DataFrames:
`quiver_df` (the arrows), `branch_segments` (the network), `anisotropy_df` (the per-image scalar,
across images). Tracks come from `pop_df` — there is no separate accessor. Recipe, and the three
traps: `docs/NOTEBOOKS.md` → *Structure anisotropy*.
9. Auto-create one filter pop per unique `branch-type` (`ensure_filter_pop!` under the branch pop
   map's root) — semantic names (`endpoint-to-endpoint`, `endpoint-to-junction`,
   `junction-to-junction`, `isolated-cycle`) so pickers show meaningful populations, not integer codes.

**Skeleton labels are NOT registered in `img.labels`.** They live in a dedicated
`img.branch_labels` field (parallel shape: `Dict{String,Vector{String}}`) with
`img_branch_labels_dir` / `img_branch_labels_path` accessors, so the generic labels picker
(measure / track / segment dropdowns) never lists branch label sets. This is deliberate: skeleton
paths are a different granularity from cell regions (see `docs/POPULATION.md` → *Branch pop type*).

**The `__branch` suffix is reserved** — `is_reserved_value_name(name)` rejects user-created
segmentations ending in `__branch` (same rule as `__tracks`).

## Future: tracking and gating

**Gating** (FlowJo-style)
- Hierarchical population gating on measured features
- Populations stored in `ccid.json` or as separate AnnData metadata

**Tracking** (timecourse)
- Link cell IDs across timepoints
- Store track IDs in AnnData `obs["track_id"]` and spatial coordinates per timepoint

See `docs/DATAMODEL.md` for AnnData conventions.
