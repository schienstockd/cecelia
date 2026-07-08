# Data Model

AnnData conventions and cell-level data storage for cecelia-pineapple.

---

## AnnData output (`.h5ad`)

Each measured label set produces one AnnData file:

```
{task_dir}/labelProps/{outputValueName}.h5ad
```

### Structure

| Slot | Content |
|---|---|
| `obs` | One row per cell. Index = cell label ID (string). No extra columns by default. The `tracking.bayesian_tracking` task adds lineage columns: `track_id`, `track_parent`, `track_root`, `track_state`, `track_generation`, `cell_id` (`NaN` for untracked cells). `tracking.track_measures` adds the **per-cell** motility columns `live.cell.speed`, `live.cell.angle`. See `docs/TRACKING.md`. |
| `var` | One row per feature. Index = feature name (see below). |
| `X` | Float32 feature matrix (cells × features). Contains all morphology and intensity columns. |
| `obsm["spatial"]` | Float32 `(n_cells, n_spatial_dims)` — centroid coordinates in array order (Z, Y, X for 3D; Y, X for 2D). |
| `obsm["temporal"]` | Float32 `(n_cells, 1)` — timepoint index (0-based). Present only for timecourse images. |
| `uns["spatial_cols"]` | List of feature names that appear in `obsm["spatial"]` (e.g. `["centroid-0","centroid-1","centroid-2"]`). |
| `uns["temporal_cols"]` | List of feature names that appear in `obsm["temporal"]` (always `["t"]`). |
| `uns["intensity_measure"]` | `"mean"` or `"median"` — which statistic was used for channel intensities. |

### Companion per-track table — `{value_name}__tracks.h5ad`

A tracked segmentation gets a second AnnData file alongside the per-cell one, holding **one row
per track** (written by `tracking.track_measures`, see `docs/TRACKING.md`). Double-underscore
suffix keeps it distinct from a segmentation named `{x}_tracks` and is reserved
(`is_reserved_value_name`); path helper `img_track_props_path(img, value_name)`.

| Slot | Content |
|---|---|
| `obs` | One row per track. Index = `track_id` (string). Per-track lineage: `track_root`, `track_parent`, `track_state`, `track_generation`. |
| `var` | The per-track measures (gateable): `live.track.speed`, `.duration`, `.trackLength`, `.displacement`, `.straightness`, `.displacementRatio`, `.outreachRatio`, `.meanTurningAngle`, `.overallAngle`, `.asphericity`. |
| `X` | Float32 `(n_tracks, n_measures)`. |
| `uns["cecelia_table"]` | `"tracks"` — marks the file as a per-track table. |

Read it through the same `LabelProps` chain (the reader is grain-agnostic — `label` = `track_id`
here). The unified accessor surfaces it as `pop_df(img, "live", pops; granularity=:track)` — see
`docs/POPULATION.md`.

### Clustering output — `clusters.{suffix}` obs + `obsm['X_umap.{suffix}']`

The Leiden clustering tasks write their result back through the `LabelProps` writer (no new files):

| Producer | Writes into | `clusters.{suffix}` | `obsm['X_umap.{suffix}']` |
|---|---|---|---|
| `clustPops.cluster` | the per-**cell** `{value_name}.h5ad` | one code per cell | per-cell 2-D embedding |
| `clustTracks.cluster` | the per-**track** `{value_name}__tracks.h5ad` | one code per track (`label`=`track_id`) | per-track 2-D embedding |

- **`clusters.{suffix}`** holds **integer cluster codes** (not strings) — the stack auto-detects
  integer obs as a categorical code set. A `clusters.*` **name-rule** in `_is_categorical_col`
  (`track_props.jl`) pins it categorical even when a high-resolution run exceeds the integer-level
  cap (codes are never a count). The `{suffix}` lets multiple resolutions / re-clusterings coexist.
- Both run **set-scope** (clustered jointly across the selected images), so codes are comparable
  across the set. They feed the `clust` / `trackclust` pop types (cluster membership = a `clusters.{suffix}`
  filter) — see `docs/POPULATION.md`.
- `obsm['X_umap.{suffix}']` is the plotting embedding (the UMAP chart reads it); written only when
  *Calculate UMAP* is on.

### Feature names

**Morphology (2D)**

| Column | Meaning |
|---|---|
| `area` | Pixel count |
| `perimeter` | Perimeter length |
| `eccentricity` | 0 = circle, 1 = line |
| `orientation` | Angle of major axis (radians) |
| `major_axis_length`, `minor_axis_length` | Ellipse axis lengths |
| `solidity` | area / convex_area |
| `extent` | area / bbox area |
| `convex_area` | Convex hull pixel count |
| `equivalent_diameter` | Diameter of circle with same area |
| `feret_diameter_max` | Maximum Feret diameter |
| `aspect_ratio` | minor / major axis |
| `perimeter_to_area` | Compactness proxy |
| `oblate` | Circularity: 4π·area / perimeter² |
| `centroid-0`, `centroid-1` | Y, X centroids |
| `bbox-0` … `bbox-3` | Bounding box rows/columns |

**Morphology (3D basic)**

| Column | Meaning |
|---|---|
| `area` | Voxel count (= volume in voxels) |
| `extent` | area / bbox volume |
| `solidity` | area / convex hull volume |
| `equivalent_diameter_area` | Diameter of sphere with same volume |
| `euler_number` | Topological Euler number |
| `feret_diameter_max` | Maximum Feret diameter |
| `major_axis_length`, `interm_axis_length`, `minor_axis_length` | Derived from inertia tensor eigenvalues |
| `centroid-0` … `centroid-2` | Z, Y, X centroids |
| `bbox-0` … `bbox-5` | Bounding box |

**Morphology (3D extended, `extendedMeasures=true`)**

Additional columns (trimesh-based):

| Column | Meaning |
|---|---|
| `surface_area` | Mesh surface area (marching cubes) |
| `volume_mesh` | Mesh volume |
| `convex_hull_area`, `convex_hull_volume` | Convex hull metrics |
| `euler_number_mesh` | Mesh Euler number |
| `solidity_mesh` | `volume_mesh / convex_hull_volume` |
| `surface_to_volume` | `surface_area / volume_mesh` |
| `sphericity` | Sphere area (same volume) / actual surface area |
| `compactness` | (36π·V²)^(1/3) / surface_area |
| `ellipticity_oblate`, `ellipticity_prolate` | Axis ratios from convex hull PCA |
| `major_axis_length`, `interm_axis_length`, `minor_axis_length` | Ellipsoid fit (overrides basic values) |

**Intensity**

| Column | Meaning |
|---|---|
| `mean_intensity_{c}` or `median_intensity_{c}` | Per-channel intensity for channel index `c` (base labels) |
| `{ltype}_mean_intensity_{c}` | Intensity within secondary label type (e.g. `nuc_mean_intensity_0`) |

---

## ccid.json `label_props` field

```json
{
  "label_props": {
    "default": "default.h5ad"
  }
}
```

Flat `Dict{String, String}` — value_name → h5ad filename (relative to `{task_dir}/labelProps/`).

Written by `measure_labels.jl` on task completion. Read by `_load_image` into `CciaImage.label_props`.

---

## Reading `.h5ad` — the `LabelProps` chain (the standard data-access idiom)

**All cell-level reads go through the `LabelProps` chain — use it everywhere, don't hand-roll
HDF5 access or read the whole table and filter afterwards.** Keeping one idiom means the access
pattern is consistent across the app and the read/transform logic can change in one place.

H5AD reads go through the **Julia-native** `LabelProps` reader (`app/src/label_props.jl`), which
reads HDF5 directly via `HDF5.jl` — pure Julia, no Python on the read path (writes go through a Python
subprocess; see `ARCHITECTURE.md`). It is
**lazy**: column selection, row filter, and sort are recorded as pending state; HDF5 I/O happens
only inside the terminal `as_df` (opened in a `do` block — no leaked handles), and only the
requested columns are read from `/X`.

Build a view with `label_props(...)`, refine it with chain verbs, finish with `as_df`. Use the
left-to-right `|>` pipe form; verbs that take an argument have a **curried one-argument form**
(below) so they compose without a `v -> verb(v, arg)` lambda:

```julia
# intensities + label
df = label_props(img; value_name="B") |> view_channel_cols |> as_df

# specific columns only (reads just these from /X)
df = label_props(img; value_name="B") |> select_cols(["area", "mean_intensity_0"]) |> as_df

# centroids from obsm/spatial + temporal
df = label_props(img; value_name="B") |> view_centroid_cols |> as_df

# row filter by label — push it INTO the reader, don't post-filter the DataFrame
df = label_props(img; value_name="B") |> select_cols(["x","y"]) |>
     filter_rows(label_ids; by=:label) |> as_df
```

**Chain verbs** (each takes the view first and returns it, so they compose):

| Verb | Effect |
|---|---|
| `select_cols(lp, cols)` (alias `view_cols`) | read only these var/obs/centroid columns |
| `view_channel_cols(lp)` | add the per-channel intensity columns |
| `view_centroid_cols(lp; order=…)` | add centroid columns from `obsm/spatial`+`temporal` |
| `filter_rows(lp, ids; by=:label)` | restrict rows to these label IDs (read-time; **intersection** — IDs absent from the file are silently skipped, result is `≤ len(ids)` rows in file order, never `NaN`/error). Python: `filter_by_label(ids)`. |
| `sort_by(lp, col; rev=false)` | sort the output |
| `rename_channels!(lp, on)` | map intensity var names → image channel names in the output |
| `add_obs(lp, df)` / `drop_obs(lp, names)` | stage obs columns to write / delete (numeric); flushed by `save!` |
| `as_df(lp; include_x, include_obs)` / `as_matrix(lp)` | **terminal (read)** — materialise the `DataFrame` |
| `save!(lp)` | **terminal (write)** — flush staged `add_obs`/`drop_obs` into the `.h5ad` |

**Curried one-argument forms.** The verbs that take a second argument also have a single-argument
form that captures the argument and returns `lp -> verb(lp, …)`, so they drop into a `|>` pipe
without a lambda (used in the examples above):

```julia
select_cols(cols)            = lp -> select_cols(lp, cols)
filter_rows(ids; by=:label)  = lp -> filter_rows(lp, ids; by=by)
sort_by(col; rev=false)      = lp -> sort_by(lp, col; rev=rev)
add_obs(df::DataFrame)       = lp -> add_obs(lp, df)
drop_obs(names)              = lp -> drop_obs(lp, names)
```

Arity disambiguates these from the two-argument methods (Julia never confuses `f(x)` with
`f(lp, x)`), and the captured argument is typed to exclude `LabelProps`. Terminal verbs (`as_df`,
`as_matrix`, `save!`) and the no-argument views (`view_channel_cols`, `view_centroid_cols`,
`view_label_col`) already pipe directly. **`rename_channels!` has no curried form** — it takes a
boolean toggle, and a one-argument `rename_channels!(true)` returning a closure reads ambiguously to
a human (it is not a *dispatch* ambiguity — `Bool` and `LabelProps` are disjoint — just a confusing
one); use the lambda form: `… |> v -> rename_channels!(v, !raw_channel_names)`.

Output is a `DataFrame` with a `label` column (obs `_index`, parsed to `Int`), plus the selected
var columns, centroid columns (`centroid-0..2`, `t`), and obs columns. The reader dispatches on
the AnnData `encoding-type` attribute: dense `array` + `string`/`string-array` (+ `categorical`
for obs) are handled; sparse (`csr/csc`) raises rather than misreads.

**Writes.** Numeric obs columns are written **natively** from Julia via the chain
(`label_props(path) |> add_obs(df) |> save!` — `save!` writes float64 `/obs` datasets directly through
`HDF5.jl`). Categorical/string obs columns (HMM states are written numeric, but transitions like
`"1_2"` and cluster ids are strings) need anndata's categorical encoding, so they go through
`write_categorical_obs(path, columns; drop=…)` — a Python subprocess (`py/tasks/labels/`), the same
"new encodings are Python's job" split as new-file creation. The reader decodes categoricals
transparently, so either write round-trips like any obs column.

**Python mirror**: the same surface exists for Python consumers (napari bridge, task modules) as
`python/cecelia/utils/label_props_utils.py` — `LabelPropsView(path).view_centroid_cols().filter_by_label(ids).as_df()`.
Use it rather than reading the H5AD ad hoc (see `POPULATION.md`). The Python file keeps the `_utils`
suffix (the convention for every file in `python/cecelia/utils/`); the Julia file is `label_props.jl` (Julia
has no such suffix). Different file names, same interface — the type/verb names are what match.

> **Two implementations, one spec — keep them in lock-step.** The Julia reader/writer
> (`app/src/label_props.jl`) and the Python `LabelPropsView` exist because both runtimes need cell
> data directly (the napari bridge and Python task modules can't proxy everything through Julia
> without worse coupling). They are **two implementations of one contract**, not one plus a copy, so
> they can drift — e.g. a new AnnData `encoding-type` handled on one side only. Rules:
> - **Any change to one reader/writer (new encoding type, new chain verb, changed semantics) must be
>   made on the other in the same change.**
> - The shared safety net is the **`LabelProps Julia/Python parity` testset** in `app/test/runtests.jl`:
>   it runs *both* readers against the same fixture (`test-data/.../B.h5ad`) and compares columns +
>   values. Extend the fixture and that test whenever you add an encoding type or column kind so drift
>   is caught in CI. (The test self-skips when the napari venv / `anndata` is unavailable.)
> - The contract both must satisfy: the encoding types listed below (dense `array`, `string`/
>   `string-array`, `categorical` for obs; sparse raises), the `label` column (obs `_index` → `Int`),
>   centroid/temporal split from `uns`, channel-name resolution, and `filter_rows` intersection
>   semantics.

**Deviation rule (read this before reaching for `h5open`/`h5py`).** The reader chain is the
default for *every* cell-level read; raw `HDF5.jl`/`h5py` access on `.h5ad` is a smell. Deviate
only when it is measurably more efficient to hit HDF5 directly — e.g. a single cheap metadata
attribute peek where spinning up the reader would be wasteful — and when you do, **add an inline
comment on that exact line** stating why the reader was bypassed. No silent raw access. The only
sanctioned home for raw `HDF5.jl` is `label_props.jl` itself; convenience metadata reads
(`col_names(lp; data_type=:obs)`, `centroid_columns(lp)`, `channel_columns(lp)`) already exist on
the reader, so prefer those over re-reading `obs`/`uns` by hand.

This is also a hard rule in `CLAUDE.md` (*H5AD / cell-data access*) so it survives outside this doc.

---

## Writing `.h5ad` — the same chain, terminal `save!`/`save()`

Writing is the mirror image of reading: you read a label-keyed DataFrame (`as_df`), you write a
label-keyed DataFrame (`add_obs` then `save!`). One idiom, both languages, so there is a single
way to touch H5AD storage and nothing to guess. Mirrors the old R `cciaImage` idiom
(`labels$add_obs(...)` then `labels$save()`).

```julia
# Julia — append obs columns to an existing file (in place, batched, AnnData-correct)
df = DataFrame("label" => label_ids, "live.cell.speed" => speeds, "live.cell.angle" => angles)
label_props(path) |> add_obs(v, df) |> save!
```
```python
# Python — identical shape
LabelPropsView(path).add_obs(df).save()       # df has a `label` column + value columns
```

- **`add_obs(lp, df)`** stages obs columns; `df` needs a `label` column plus one column per obs
  field. Values are aligned to the file's obs index **by label** (labels absent from `df` get
  `NaN`). Repeated `add_obs` calls accumulate. Float columns (measures, lineage, …).
- **`drop_obs(lp, names)`** stages obs columns to delete (idempotent — names absent from the file
  are ignored). Use to invalidate derived columns whose source changed — the tracking task drops
  stale `live.cell.*` / `live.track.*` measures when it writes new `track_id`s, so a re-track
  doesn't leave measures computed against the previous tracking (`docs/TODO.md` #00028). Combine
  with `add_obs` in one chain; `save!` applies drops and adds together.
- **`save!(lp)`** (Julia) / **`save()`** (Python) is the terminal write — flush staged columns in
  one open/write/close. Julia writes each as a `/obs/<name>` float64 dataset with
  `encoding-type="array"`, `encoding-version="0.2.0"` and updates the `/obs` `column-order`
  attribute; **no whole-file rewrite**. Python flushes via `anndata.write_h5ad`. Verified
  bidirectional: Julia-written columns read cleanly under `anndata.read_h5ad` (no warnings), and
  vice-versa.
- **Crash-safety write order (Julia):** all column datasets are written *first*, the `column-order`
  attribute *last*. A crash mid-write leaves a readable file (data present, `column-order` stale or
  briefly absent — both the reader and `anndata` tolerate that) rather than a `column-order` naming
  a dataset that was never written. Don't reorder the writes in `save!`.

**Scope — obs append vs file creation.** This path appends 1-D `obs` columns to an **existing**
file. It does **not** create files or change `X`/`var` structure (appending a feature to the `X`
matrix is a full rewrite, not a cheap append). Building a new `.h5ad` from measurement arrays
(`X` + `var` + `obsm`) is the producing task's job and is done in Python via `anndata`
(`python/cecelia/utils/measure_utils.py`) — the "Python owns the files it produces" boundary. Gating
needs only `:vars` columns to be gateable today, so measures that must be gateable are deferred to
the phase that settles per-track storage (`docs/TODO.md` #00021), not force-written to `X` here.

This is also a hard rule in `CLAUDE.md` (*H5AD / cell-data access*).

---

## Mesh files

When `saveMeshes=true`, per-cell triangular surface meshes are written to:

```
{task_dir}/meshes/{outputValueName}/t{NNNN}/{label_id}.stl
```

These are watertight STL meshes generated by trimesh marching cubes from the binary label mask. They are the input to downstream spatial analysis (cell contact detection — equivalent of the original `cellContactsMeshes.R`).

---

## Populations / gating

Population definitions are **not** stored in the AnnData (membership is derived, never
duplicated). They live in a per-segmentation sidecar `{task_dir}/gating/{value_name}.json`,
name-matched to `labelProps/{value_name}.h5ad`. See [`POPULATION.md`](POPULATION.md) for the
population manager, the `pop_df` unified accessor (which pools across value_names), the
gating engine, and gate↔track composition.

## Future

**Tracking**: `obs["track_id"]` column linking cells across timepoints; spatial coordinates per timepoint in `obsm["spatial"]` with `t` in `obsm["temporal"]`. A tracking task will write `track_id` for the cells of a (gated) population — see gate↔track composition in [`POPULATION.md`](POPULATION.md).
