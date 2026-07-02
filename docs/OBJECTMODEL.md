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
| `meta` | free dict | OME metadata extracted at import: `SizeC`, `SizeZ`, `SizeX`, `SizeY`, etc. |

`_dir` (the absolute path to `1/{uid}/`) is runtime-only and never written to disk.

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
