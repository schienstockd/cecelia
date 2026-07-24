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

---

## Python class hierarchy

```
python/cecelia/utils/segmentation_utils.py        SegmentationUtils (base)
python/cecelia/utils/cellpose_utils.py            CellposeUtils(SegmentationUtils)
app/src/tasks/segment/cellpose_run.py      entry point (named _run to avoid shadowing the cellpose package)
```

Future algorithms (Stardist, etc.) subclass `SegmentationUtils` and implement `predict_slice`.

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

### `predict_slice` signature (current implementation)
```python
def predict_slice(self, tile: np.ndarray, model_params: dict, norm_params: dict | None) -> np.ndarray:
    """
    tile: [C, Z, Y, X] for 3D images, [C, Y, X] for 2D
    model_params: one entry from the 'models' JSON dict (with 0-based channel indices)
    norm_params: per-channel (norm_min, norm_max) from normaliseToWhole, or None
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

## Future: tracking and gating

**Gating** (FlowJo-style)
- Hierarchical population gating on measured features
- Populations stored in `ccid.json` or as separate AnnData metadata

**Tracking** (timecourse)
- Link cell IDs across timepoints
- Store track IDs in AnnData `obs["track_id"]` and spatial coordinates per timepoint

See `docs/DATAMODEL.md` for AnnData conventions.
