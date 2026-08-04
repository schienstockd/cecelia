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

Sets group images for processing. An image belongs to one set but lives independently on disk — the set holds only a UID reference list, not a nested directory. Because of this, **moving an image between sets is a manifest-only operation** — `move_image!(proj, image_uid, from_set_uid, to_set_uid)` (`project.jl`) just edits the two sets' `image_uids` lists; the image's `0/{uid}` and `1/{uid}` dirs are UID-keyed and never move on disk. (Contrast `delete_image!`, which *does* `rm` those dirs.)

---

## Disk layout

```
{projects_dir}/{proj_uid}/
  project.json              — project manifest (set_uids list)
  lab-log.md                — append-only AI+human analysis memory (see docs/ai-assist/LAB-LOG.md)
  project.json.lock         — naive write lock, derived from the state file (see Transactions)
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

### Project identity = the directory name (never bake the uid into internal files)

A project's identity is its **directory name** under `projects_dir()`. `load_project(uid)` and
`init_object(proj_uid, uid)` build paths as `projects_dir()/<uid>/…` — they never read the path out of
`project.json`, so `project.json.uid` is a redundant copy that must equal the dirname. The invariant:

> **A project-internal file must not bake in its own project's uid — identity comes from location.**

This is what lets a project be imported under a new uid, copied, or renamed (`reidentify_project!` in
`project_io.jl`) with no stale references. Enforced in code:
- **Analysis boards** (`settings/analysisBoards.json`) store layout keys **project-relative**
  (`tab:<id>`, no uid); the frontend re-applies the current uid on load (`frontend/src/utils/boardKeys.ts`).
- **Chain runs** (`chains/runs/*/run.json`) resume against the project they're loaded from
  (`load_chain_run` uses `proj.uid`); the persisted `project_uid` is advisory.
- The one unavoidable exception is a **notebook's hardcoded `load_project("<uid>")`** (arbitrary user
  Julia) — `reidentify_project!` / import-copy best-effort rewrites that literal form; fancier user
  references are the user's to fix.

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
  "starred":     false,
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
| `starred` | Bool | A plain user bookmark ("I like this one"), default `false`. **Any number of images can be starred** — it is not a nomination. Nothing downstream reads it: it drives the Starred row filter in the image table (`frontend/src/utils/rowFilters.ts`) and has no effect on selection, runs, or processing. Set via `POST /api/images/inclusion/set`. Absent in legacy files ⇒ not starred. |
| `meta` | free dict | OME metadata extracted at import: `SizeC`, `SizeZ`, `SizeX`, `SizeY`, `PhysicalSizeX/Y/Z` (µm/px), `PhysicalSizeUnit`, `TimeIncrement` (seconds/frame), `TimeIncrementUnit`, plus `ori_path` (the original source file location, before OME-Zarr conversion — surfaced to the frontend as `oriPath` and shown in the per-row image-metadata dialog; any other scalar meta not mapped to a first-class payload field is surfaced generically as `extraMeta`), etc. Physical-size/timing keys are absent (not defaulted) when the source file carried no usable value — the API surfaces them raw/nullable so the frontend can tell "genuinely missing" apart from "confirmed 0/1"; see the metadata-editor warning icon in `docs/UI.md`. `PhysicalSizeZ_raw` is set alongside a corrected `PhysicalSizeZ` when the importer's ImageJ-TIFF Z-spacing auto-fix overrides bioformats2raw's value (see `app/src/tasks/importImages/omezarr.jl`). Images imported before these keys existed can be backfilled without a re-import via `resync_ome_meta!`/`POST /api/images/meta/resync`, which re-reads them from the `"default"` (original bioformats2raw) zarr — deliberately not whichever version is currently `active`, since processed variants (drift/cellpose-correct) carry no OME calibration metadata at all; see `CLAUDE.md` → *OME-ZARR dual-format*. Resync is **fill-only** — it adds missing keys but never overwrites one already present, so it can't revert a human correction or the ImageJ Z-spacing auto-fix (both ccid.json-only, not reproducible by re-reading the zarr). The metadata editor (`POST /api/images/meta/set`) keeps the default zarr's `.zattrs` scale + axis units and OME-XML `<Pixels>` in sync, so an *edited* value does survive a later resync. Import does the same for its own auto-corrections (ImageJ Z-spacing fix, per-plane DeltaT interval) — both the editor and the importer funnel through one translator, `sync_zarr_calibration!`, so napari renders the same calibration `img_physical_sizes` (analysis) computes with rather than the raw zarr value. |

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

**Removing a version** — `remove_image_version!` (`storage.jl`) is the ONE path (used by the
`RemoveImage` task and the storage-reclaim API): it deletes the version's store, clears its
`filepath` entry and re-points `_active`. It only **un-imports** the image (also clears
`imChannelNames` + `SizeC/T/Z`, `status="pending"`) when the primary `default` is removed **and no
other version remains**. Removing `default` while a corrected variant is still present keeps the
channel names/dims that variant inherits from `default` via versioned fallback — this is what makes
"reclaim the original to free space, keep the corrected one" safe.

**Dropping the analysis instead** — `reset_image_analysis!` (`storage.jl`) is the mirror operation and
the two are strictly orthogonal: it deletes every child of `1/{uid}` **except `ANALYSIS_KEEP`**
(`ccid.json` + `runlog.json`) and clears the `labels` / `label_props` / `branch_labels` registrations,
while touching **no image store at all** — `filepath`, `imChannelNames`, `meta`, `attr`, the inclusion
flags and `status` are left exactly as they were. So an image can be re-run from clean without
re-importing or duplicating its zarr.

Two details worth knowing:

- It is a **keep-list, not a delete-list**. `ccid.json` is the only non-derived file in the dir, so a
  delete-list would silently leak whatever analysis dir is added next. The `analysis keep-list`
  assertion in `app/test/suite.jl` fails until a new sibling is deliberately classified.
- `runlog.json` **survives**, which means the image table's run tag then describes *history, not current
  state* — an image whose outputs are gone still shows its last successful run. Deliberate: losing the
  record of what was done is worse than a stale-looking tag. `qc/` does not survive, since its findings
  score outputs that no longer exist.

`analysis_bytes_of(img)` reports what a reset would free, and is what the Settings storage box shows as
**Analysis** — one accounting, so the box can't promise bytes the reset doesn't deliver.

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

`with_transaction(f, obj)` serialises writes to ONE object's state file, and `commit_state!(f, obj)` is what tasks actually call: read `ccid.json`, mutate the raw dict, write it back — the whole read-modify-write inside the transaction.

The lock path is **derived from the object's state file**, as in the old R `reactivePersistentObject.R` (`lockFile = paste0(getStateFile(), ".lock")`). So it works for any persisted object: `with_transaction(f, img)` locks that one image and nothing else. (It used to hardcode one `{proj}/.cecelia.lock`, which could only ever be project-scoped — too coarse to be worth calling, which is why nothing called it.)

**Two layers, because there are two different collisions:**

| Layer | Guards against | Mechanism |
|---|---|---|
| in-process | two scheduler threads (`Threads.@spawn`) on one image — the collision we actually have | a `ReentrantLock` per state-file path |
| cross-process | a REPL session writing to a project the server also has open | the `.lock` file beside the state file |

The lockfile alone was not merely incomplete for the in-process case, it was **wrong**: `isfile` then `touch` is a time-of-check/time-of-use race, so two threads both see no file, both create it, and both proceed. The `ReentrantLock` closes that; a lockfile cannot, without an atomic create.

Reentrancy matters now that task commits take a transaction: only the **outermost** call touches the lockfile, so a commit reached from inside another transaction on the same object doesn't sit waiting on its own outer call's file. (It did, until the reentrancy test caught it.)

**Abandoned lockfiles are reclaimed** after `_LOCK_STALE_AFTER` (120s). That's safe only because a transaction wraps the short commit and never the computation — no legitimate holder is ever that old — and the alternative is worse: a process dying mid-commit would otherwise brick every later task on that image behind a hidden file. Locking the commit rather than the whole load→compute→save span is a deliberate departure from the R original, which could sit on a stale lock for minutes.

**Lock the commit, not the computation.** Long work (bf2raw, cellpose, a multi-GB delete) runs unlocked; only the final registration is serialised:

```julia
ok = run_py(...)            # unlocked: minutes
ok || return nothing
commit_state!(img) do raw   # locked: milliseconds
    versioned_set_field!(raw, "filepath", out_filename, out_value_name)
end
```

`write_json_atomic` and the transaction solve *different* halves and both are needed: atomicity stops a **torn file**, the transaction stops a **lost update** (two tasks both read the old dict; the second write drops the first's field — each write individually intact, just built on stale data).

## Writing state — always atomic

Every state file is written by `write_atomic` / `write_json_atomic` (`app/src/utils.jl`): serialise into a sibling temp file, then rename into place. Never `open(path, "w")` a state file — `open(..., "w")` truncates before the new bytes land, so a process death in that window (the Quit button SIGKILLs the tree, a task cancel, a crash) left a half-written file. That was unrecoverable for `ccid.json` and not confined to the image concerned: `_load_set` has no per-image guard, so **one** truncated image `ccid.json` made the whole project fail to open, every other image intact but unreachable. The `no hand-rolled state writes` testset fails on a new bare-open site.

Locate the file with `state_file` rather than building the path — `state_file(obj)` for a loaded image/set/project, `state_file(meta_dir)` for a directory in hand, `state_file(proj_dir, obj_uid)` for the API layer answering from raw uids without loading the object (`obj_meta_dir` owns the `1/` segment, mirroring `img_zero_dir` for `0/`).

---

## Absolute path helpers

```julia
img_zero_dir(img)          # → "{proj}/0/{uid}/"   (image data root)
img_filepath(img)          # → "{proj}/0/{uid}/{active_filename}"
img_filepath(img, "cpCorrected")  # → "{proj}/0/{uid}/{filename_for_cpCorrected}"
```

These derive the path from `img._dir` (`1/{uid}/`) by walking up two levels and crossing to `0/`. Never construct these paths manually.
