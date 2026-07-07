# OBJECTMODEL.md — Image and set object model

The object hierarchy, disk layout, and persistence conventions for `CciaProject`, `CciaSet`, and `CciaImage`.

> Update this file in the same change whenever you modify object fields, ccid.json shape, versioned field conventions, or disk layout.

---

## Object hierarchy

```
CciaProject
  └── CciaSet  (one or more)
        └── CciaImage  (one or more)
```

Sets group images for processing. An image belongs to one set but lives independently on disk — the set holds only a UID reference list, not a nested directory.

---

## Disk layout

```
{projects_dir}/{proj_uid}/
  project.json              — project manifest (set_uids list)
  .cecelia.lock             — naive write lock (see Transactions)
  settings/                 — per-project UI config (persisted on Save project)
    chains/{name}.json      — chain/whiteboard templates (migrated from the legacy top-level chains/)
    analysisBoards.json     — Analysis-canvas tabs + grid layouts + captured screenshots
  0/
    {image_uid}/            — image data (OME-ZARR, written by bioformats2raw or tasks)
  1/
    {set_uid}/
      ccid.json             — CciaSet fields + image_uids list
    {image_uid}/
      ccid.json             — CciaImage fields
      data/                 — task outputs: labels.zarr, label props, layer-props .pkl
```

Sets and images live in the same flat `1/` namespace. They are distinguished only by the `"class"` field in their `ccid.json` (`"CciaSet"` vs `"CciaImage"`). `init_object(proj_uid, uid)` dispatches on that field — no need to know the type in advance.

Image data (`0/`) and metadata (`1/`) are always parallel: `0/{uid}/` holds the zarr, `1/{uid}/` holds everything else. Tasks write zarr outputs into `0/{uid}/` and update `1/{uid}/ccid.json`.

---

## project.json

```json
{
  "uid":      "NRUBxU",
  "name":     "My project",
  "kind":     "static",
  "set_uids": ["AbCdEf", "GhIjKl"],
  "meta":     {}
}
```

Written by `save!(proj::CciaProject)`. Runtime fields (`root`, `_sets`) are not serialised. Sets are loaded in `set_uids` order.

---

## ccid.json — CciaSet

```json
{
  "class":      "CciaSet",
  "uid":        "AbCdEf",
  "name":       "Experiment 1",
  "kind":       "static",
  "image_uids": ["KDIeEm", "XyZwVu"],
  "meta":       {}
}
```

`image_uids` is the ordered membership list. `_images` (the loaded `CciaImage` vector) is runtime-only and not serialised. `save!(set)` also calls `save!` on every member image.

---

## ccid.json — CciaImage

```json
{
  "class":  "CciaImage",
  "uid":    "KDIeEm",
  "name":   "sample_01.czi",
  "kind":   "static",
  "status": "done",

  "filepath": {
    "default":     "ccidImage.ome.zarr",
    "_active":     "default",
    "cpCorrected": "ccidCpCorrected.ome.zarr"
  },

  "imChannelNames": {
    "default":  ["DAPI", "CD4", "CD8"],
    "_active":  "default"
  },

  "labels":      {},
  "label_props": {},
  "attr":        { "condition": "treated" },
  "included":    true,
  "note":        "",
  "meta":        { "SizeC": "3", "SizeZ": "12", "SizeX": "1024", "SizeY": "1024" }
}
```

### Fields

| Field | Type | Notes |
|---|---|---|
| `uid` | String | Random 6-char alphanumeric, globally unique within the project |
| `name` | String | Original filename or user label |
| `kind` | String | Always `"static"` for now |
| `status` | String | `pending` → `converting` → `done` \| `failed` |
| `filepath` | versioned dict | Relative filenames inside `0/{uid}/` |
| `imChannelNames` | versioned dict | Lists of channel name strings |
| `labels` | versioned dict | Paths to label zarrs inside `1/{uid}/data/` |
| `label_props` | versioned dict | Paths to label property files |
| `attr` | flat dict | User-defined string metadata (used for filtering) |
| `included` | Bool | Whether the image is used in further processing/analysis (default `true`). Excluded images (`false`) are greyed — not hidden — in the GUI, can't be checkbox-selected for a run, and are hard-skipped by the task/chain runners even if selected (`_drop_excluded` in `api/src/sockets.jl`). The systematic successor to the old R app's `Include=Y/N` keyword. Ask the model via `image_included(img)`. Set through `POST /api/images/inclusion/set`. Absent in legacy files ⇒ included. |
| `note` | String | Optional free-text reason the user attaches to an image (e.g. why it was excluded); shown in the exclusion badge/tooltip. Set alongside `included` via `POST /api/images/inclusion/set`. |
| `meta` | free dict | OME metadata extracted at import: `SizeC`, `SizeZ`, `SizeX`, `SizeY`, `PhysicalSizeX/Y/Z` (µm/px), `PhysicalSizeUnit`, `TimeIncrement` (seconds/frame), `TimeIncrementUnit`, etc. Physical-size/timing keys are absent (not defaulted) when the source file carried no usable value — the API surfaces them raw/nullable so the frontend can tell "genuinely missing" apart from "confirmed 0/1"; see the metadata-editor warning icon in `docs/UI.md`. `PhysicalSizeZ_raw` is set alongside a corrected `PhysicalSizeZ` when the importer's ImageJ-TIFF Z-spacing auto-fix overrides bioformats2raw's value (see `app/src/tasks/importImages/omezarr.jl`). Images imported before these keys existed can be backfilled without a re-import via `resync_ome_meta!`/`POST /api/images/meta/resync`, which re-reads them from the `"default"` (original bioformats2raw) zarr — deliberately not whichever version is currently `active`, since processed variants (drift/cellpose-correct) carry no OME calibration metadata at all; see `CLAUDE.md` → *OME-ZARR dual-format*. Resync is **fill-only** — it adds missing keys but never overwrites one already present, so it can't revert a human correction or the ImageJ Z-spacing auto-fix (both ccid.json-only, not reproducible by re-reading the zarr). The metadata editor (`POST /api/images/meta/set`) keeps the default zarr's `.zattrs` scale + axis units and OME-XML `<Pixels>` in sync, so an *edited* value does survive a later resync. Import does the same for its own auto-corrections (ImageJ Z-spacing fix, per-plane DeltaT interval) — both the editor and the importer funnel through one translator, `sync_zarr_calibration!`, so napari renders the same calibration `img_physical_sizes` (analysis) computes with rather than the raw zarr value. |

`_dir` (the absolute path to `1/{uid}/`) is runtime-only and never written to disk.

### `meta["funParams"]` — remembered task params

`meta` also holds `funParams`: a `{ "<fun_name>": { …params… } }` map of the **last-used parameters
for each task**, mirroring the old R `moduleFunParams`. Stored on both the **image** (a record of
what params produced it) and the **set** (the shared last-used default). The module-page form
populates from image → set → task-defaults (see `docs/MODULES.md` → *Remembering task params*).
Written via the dir-based `write_module_fun_params!(ccid_dir, fun, params)` / read via
`read_module_fun_params(ccid_dir, fun)` (`app/src/model/image.jl`) — a targeted `ccid.json`
read-modify-write (same idiom a task uses to register its output `filepath`), **dir-based** so
remembering a param blob on the set never has to load all its images. `CciaSet.meta` carries the
same `funParams` key.

---

## Versioned fields

Several fields (`filepath`, `imChannelNames`, `labels`, `label_props`) follow the versioned-variable pattern:

```json
{
  "default":     <value>,
  "_active":     "default",
  "otherVersion": <value>
}
```

- `_active` points to the name of the currently active entry.
- `"default"` is always the raw/imported version.
- Correction tasks add named entries (e.g. `"afCorrected"`, `"cpCorrected"`).
- `_active` is updated to the new name after each correction.

### Julia API

```julia
# Read (returns the active entry when value_name is nothing)
versioned_get_field(raw_dict, "filepath", nothing)       # → active filename
versioned_get_field(raw_dict, "filepath", "cpCorrected") # → specific filename

# Write (sets value and updates _active)
versioned_set_field!(raw_dict, "filepath", "ccidCpCorrected.ome.zarr", "cpCorrected")

# On the in-memory struct (filepath is Dict{String,String})
active(img.filepath)                          # → active filename string
set_active!(img.filepath, "file.zarr", "v1") # mutates in place
versioned_keys(img.filepath)                  # → ["default", "cpCorrected"] (no "_active")
```

### JSON3 gotchas

JSON3 yields `Symbol` keys (`:default`, `:_active`), not `String` keys. When building a working `Dict` from a JSON3 object always convert:

```julia
Dict{String,Any}(String(k) => v for (k, v) in json3_obj)
```

Without this, `get(dict, "default", nothing)` returns `nothing` even when the key exists.

`JSON3.Object <: AbstractDict` but `JSON3.Object isa Dict` is `false`. All type guards must use `isa AbstractDict`, not `isa Dict`. The versioned helpers already do this — don't add new `isa Dict` checks.

---

## `init_object` — type-agnostic loader

```julia
obj = init_object(proj_uid, uid)
# returns CciaSet or CciaImage depending on "class" in ccid.json
```

Used by the API when it knows a UID but not whether it's a set or an image.

---

## Transactions and locking

`with_transaction(f, proj)` holds a naive lockfile at `{proj}/.cecelia.lock` for the duration of `f()`. It polls up to 30 seconds for an existing lock to clear, then errors with the lockfile path so it can be deleted manually.

This is intentionally minimal — a single file existence check, no PID or timestamp. It prevents two concurrent HTTP handlers from clobbering the same `project.json`, but it is not a distributed lock and does not protect per-image writes. See `TODO.md` for the planned move to per-image lockfiles.

---

## Absolute path helpers

```julia
img_zero_dir(img)          # → "{proj}/0/{uid}/"   (image data root)
img_filepath(img)          # → "{proj}/0/{uid}/{active_filename}"
img_filepath(img, "cpCorrected")  # → "{proj}/0/{uid}/{filename_for_cpCorrected}"
```

These derive the path from `img._dir` (`1/{uid}/`) by walking up two levels and crossing to `0/`. Never construct these paths manually.
