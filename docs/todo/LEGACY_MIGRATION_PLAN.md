# Legacy (R/Shiny cecelia) → Pineapple data migration — PLAN

**Status:** parked plan / in progress. Goal: bring an *old R-version* cecelia project into the new
Julia stack **without recomputing** the expensive steps.

## Scope (LOCKED with Dominik)

**Migrate: images · segmentation · tracking.** These are the expensive/irreplaceable outputs.

**Exclude: clustering · gating · HMM.** These are *trivial to redo in the new app* on top of the
migrated segmentation+tracking, and the old formats diverged (old flat filter-`popMap` gating; old
separate `*.sc.h5ad`/`tracks.clusters.*` clustering; `live.cell.hmm.*` columns). Not worth migrating.

## Locked design decisions
1. **Do NOT add any old-format fallback to the new readers.** The new `label_props.jl` /
   `label_props_utils.py` stay clean (obsm-only centroids, index=label). The **migrator upgrades the
   old data onto the new format** instead.
2. Migrate **images + labels zarr by reference/copy** (no pixel rewrite / no re-pyramid).
3. Rewrite only the **primary segmentation `labelProps/*.h5ad`**; ignore clustering/HMM/gating files.

## Why it's feasible (verified against a real old project: `…/QUACK/CECELIA/projects/8BR53W`)
- Old images = **OME-ZARR (zarr v2)** → the new `zarr_utils.open_as_zarr` opens them fine.
- Old labels = **zarr v2** bare `uint32` (`t,z,y,x`) → readable.
- Old cell data = **AnnData `.h5ad`** → reads fine; `label/track_id/cell_id/live.cell.speed/angle` all
  present. No re-segmentation / re-tracking needed.

## Structural deltas

### 1. Directory layout
`{proj}/ANALYSIS/0/{uid}` → `{proj}/0/{uid}` (image) · `{proj}/ANALYSIS/1/{uid}` → `{proj}/1/{uid}`
(metadata). Add `{proj}/project.json` + `{proj}/settings/`. UID scheme is identical (6-char alnum).

### 2. Object metadata: `ccid.rds` → `ccid.json`
Old `ccid.rds` = gzipped **R-serialized list** (not R6) → read with `Rscript` (R 4.5.2 present). Fields
`UID, CciaClass, CciaName, CciaType, CciaAttr, CciaMeta, CciaLogfile`; sibling `ccid.type`.
**`CciaMeta` is APP metadata** (task params, versioned filepaths, channel names) — NOT image dims.

| New `ccid.json` | Source |
|---|---|
| `uid`, `class`, `name`, `attr` | `UID`, `CciaClass`, `CciaName`, `CciaAttr` |
| `kind` | derive from `CciaType`/attrs |
| `imChannelNames` `{<vn>:[…],"_active":…}` | `CciaMeta$imChannelNames` (**custom names live here** — OME-XML only has generic `CH2/CH3`) |
| `filepath` `{<vn>:…,"_active":…}` | `CciaMeta$imFilepath` |
| `labels` `{<vn>:[…]}` | `CciaMeta$imLabelsFilepath` (keep only migrated value_names) |
| `label_props` `{<vn>:…,"_active":…}` | `CciaMeta$imLabelPropsFilepath` (ditto) |
| `meta` (Size*, PhysicalSize*, TimeIncrement, ori_path) | **OME-XML** (see §3) |
| `status` | `"done"` |

**Versioned conversion:** old R marks the active variant with `attr(list,"default")="<vn>"`; new uses
a `"_active"` key. Map each named element → JSON key; `attr default` → `"_active"`.

### 3. Image dims/calibration: from the OME-XML (gotcha)
Old zarr ships `…zarr/OME/METADATA.ome.xml` with full calibration, but its **NGFF `.zattrs` is a v0.2
stub** (no axes/scale/omero). The Julia `read_ome_metadata` reads NGFF only → empty on old data. The
migrator reads dims from the **OME-XML** via `python/cecelia/utils/ome_xml_utils.py`
(`load_ome_xml`/`read_scale_from_ome_xml`/`read_pixel_unit`/`read_time_increment`). (napari is fine —
`zarr_utils.read_scale` already OME-XML-falls-back; only the Julia NGFF extractor is affected.)

### 4. `labelProps/*.h5ad` rewrite (segmentation + tracking) — verified layout
**New format** (`measure_utils.py`, `label_props.jl`): `obs.index` = **integer label**;
`obsm['spatial']`+`uns['spatial_cols']` centroids (`centroid-0..N`); `obsm['temporal']`+
`uns['temporal_cols']` time; features in `X`/`var`.

**Old is heterogeneous** (the R version was upgraded mid-life; its util branches on
`has_spatial_obsm()`):
- Every old file: positional index (`0,1,2…`) + explicit **`label` obs column** with real **1-based**
  IDs → `index ≠ label`.
- Newer files: already have `obsm['spatial'/'temporal']` + `uns['*_cols']`.
- Older files (`manual.h5ad`, `default.h5ad`, early `dcs.TRITC`): centroids in `var` as
  `centroid_z/y/x/t`.

**Rewrite rules (per file, idempotent):**
1. **`index ← label` column** (drop the column). MANDATORY — labels are 1-based, index is 0-based
   positional; the new reader keys off `/obs/_index`. Skip if already index-keyed.
2. **Only if `obsm['spatial']` absent:** move `var` `centroid_*` → `obsm['spatial']`/`['temporal']` +
   `uns['spatial_cols'/'temporal_cols']` (underscore→`centroid-N`). Files that already have obsm: untouched.
3. Keep `X` features, `track_id`, `cell_id`, `live.cell.speed`, `live.cell.angle`.
4. **Drop** excluded columns: `live.cell.hmm.*`, `live.cell.*clust*`, `live.cell.track.clusters.*`
   (re-derivable in-app; avoids dangling refs).
5. Track lineage: new expects `track_parent/root/state/generation`; old has only `track_id`.
   Synthesize defaults (root=parent=track_id, generation=0, state 0) — verify the new app is happy;
   else leave absent.

**Skip entirely:** `*.sc.h5ad`, `tracks.clusters.*.h5ad`, `populations/`, gating.

### 5. Set + project level
Old set `ccid.rds` (`CciaClass=CciaSet`, image UID list) → new set `ccid.json`
(`class:CciaSet`, `image_uids:[…]`) + `{proj}/project.json`.

## Implementation — GUI, in the Import module (BUILT)

Delivered as an **in-app import flow**, not a CLI. Still adds **ZERO new package deps** (no edits to
`pyproject.toml`/`pixi.toml`/`Project.toml`): R stays external (like bioformats2raw/Java), `anndata`
is already an app dep, OME-XML is stdlib, zarr is file-copied. Confined to the import task dir.

**Dependency isolation**
- **rds read → base R only** (`read_ccid_rds.R`, no packages — system R *or* the old cecelia **renv**
  R via the task's `rscript` param). Batch-reads all images in one process for a fast scan.
- **OME-XML → Python stdlib `xml.etree`**; **h5ad rewrite → `anndata`/`numpy`/`pandas`** (existing).

**Files**
```
python/cecelia/tasks/importImages/
  legacy_migrate.py       # engine: read_rds, read_ome_meta, scan_project, migrate_h5ad, migrate_image
  read_ccid_rds.R         # base-R, uid-prefixed tab records
  scan_legacy_run.py      # run_py entry: read-only preview manifest
  migrate_legacy_run.py   # run_py entry: migrate one image
app/src/tasks/importImages/migrateLegacy.{jl,json}   # per-image task (registered)
api/src/routes.jl         # api_import_scan_legacy, api_import_register_legacy (+ server.jl dispatch)
app/src/model/set.jl      # add_image!(; uid=…) — preserve legacy UID
frontend/src/components/LegacyMigrateDialog.vue      # scan → preview table → register
frontend/src/modules/ImportModule.vue                # "Migrate legacy project" button
```

**Flow** — Import page → *Migrate legacy project* → enter legacy project path → **scan** (read-only)
→ preview table stating per image what **transfers** (image · segmentation · tracking) and what does
**not** (clustering · gating · HMM) + warnings → tick images → **register** (UIDs preserved, source
stashed in `meta`) → run the **Migrate legacy image** task (TaskRunner) per image, which copies the
zarr + rewrites the h5ad and applies the ccid fields. Migrates into the **currently-open set**.

**Safety:** strictly read-only on the source; `migrate_image` refuses if the destination is inside
the source tree; the only deletes are on the destination (idempotent re-run).

**Attributes:** `CciaAttr` carried verbatim; the legacy `Include=Y/N` keyword maps to the new
top-level `included` bool.

## Validation
1. Migrate **one** image from `8BR53W` into a scratch project; assert `ccid.json` parses,
   `label_props(...)|>as_df` reads with **index=label**, `obsm['spatial']` present, zarr opens, dims
   match OME-XML. (Use both a tracked obsm file and an older var-centroid file.)
2. Dominik opens it in the running app → image + labels + tracks render.
3. Then run over a full user dataset.

## Open decisions (need Dominik)
1. **Copy vs symlink** the image/label zarr (disk cost vs coupling to the source drive).
2. **`migration/` top-level dir** OK?
3. Track-lineage: synthesize defaults, or leave absent and let the app treat every cell as its own
   track root?

## Caveats
- Requires R on the migrating machine (for the `.rds` read).
- Windows: same R requirement; paths via the tooling, not hardcoded.
