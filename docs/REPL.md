# Cecelia from the REPL / a notebook — data-access reference

The canonical way to read a Cecelia project's data from Julia: `using Cecelia`, load the project,
navigate to an image, pull a DataFrame. This is what a Pluto notebook (or an interactive REPL, or
Claude generating a notebook for you) should use — **no manual path building, no raw HDF5/zarr**.

> **How this file stays honest.** The `## API reference` section at the bottom is **generated from the
> live docstrings** of a curated allow-list (`NOTEBOOK_API` in `app/src/ai/repl_api.jl`). A golden test
> (`app/test/runtests.jl`) fails if it drifts, so the reference can never fall behind a signature or
> behaviour change. After editing a listed function's docstring, run
> `cd app && julia --project -e 'using Cecelia; Cecelia.write_repl_doc()'`. Everything *outside* the
> markers is hand-written and safe to edit.

---

## The shape of it

```julia
using Cecelia
init_cecelia!()                          # read .env / config, register tasks

proj = load_project("NRUBxU")            # by project uid (or an absolute project path)

# navigate
setlist = sets(proj)                     # Vector{CciaSet}
imgs    = images(proj)                   # every image across all sets
img     = image_by_uid(proj; uid = "KDIeEm")   # one image by uid (nothing if absent)
vns     = img_value_names(img)           # the segmentation / value_names on this image
is_tracked(img)                          # does this image have tracks?
```

## Access routes — one accessor per job

| You want… | Use | Notes |
|---|---|---|
| gated / derived population cells or tracks | `pop_df` | THE main accessor — pools across pops, value_names **and** images in one call (see *Common idioms*) |
| raw label props (measures / centroids / channels), your own column set | `label_props(img) \|> … \|> as_df` | lazy view; push the column/row selectors *in*, never read-all-then-filter |
| per-track motility table (speed, straightness, displacement, …) | `track_props(img)` | one row per track |
| the numbers behind an analysis-board summary plot | `plot_summary_data` | plot-ready series, not a figure |

**Project-specific names are NOT in this doc — get them from the observer, don't guess.** Population
paths/types come from `get_populations`; the column/measure names available on a pop from
`get_measure_summary` (or a one-row `pop_df` — read `names(df)`); segmentation value_names from
`img_value_names(img)`. Guessing a column name with a candidate-list fallback in the notebook is an
anti-pattern — confirm the real names first, then use them directly.

### Cell / population data — `pop_df`

`pop_df` is the one unified accessor for population + cell/track tables (the same data the gating and
analysis-board plots use). Population **paths and types are project-specific** — enumerate them via the
observer MCP tool `get_populations` (or `load_pop_map(img, value_name, pop_type) |> pop_paths` in the
REPL).

```julia
# gated cells for a population (pop_type "flow"/"live"/"clust"/"labels"; paths from pop_paths)
df = pop_df(img, "flow", ["/nonDebris/CD4"])

# per-TRACK rows instead of per-cell (tracked populations)
df = pop_df(img, "live", ["/tcells/tracked"]; granularity = :track)

# intensity columns come back under channel names by default (mean_intensity_0 → "CD4");
# pass raw_channel_names=true to keep mean_intensity_i
df = pop_df(img, "flow", ["/nonDebris/CD4"]; raw_channel_names = true)
```

### Raw label props — the fluent view

Build a lazy view, refine it with column/row selectors, finish with the terminal verb `as_df`. Push
the selection *into* the view — never read the whole table and filter in memory.

```julia
df = label_props(img) |> select_cols(["area", "mean_intensity_0"]) |> as_df
df = label_props(img) |> view_centroid_cols |> as_df
df = label_props(img) |> view_channel_cols  |> as_df      # all intensity channels
```

### Tracks

```julia
tdf = track_props(img)                    # track-level measures (speed, displacement, …)
```

### Summary-plot data

```julia
pd = plot_summary_data(img, …)            # the data behind an analysis-board summary plot
```

---

## Common idioms — and the anti-patterns

The API is built to do the pooling and column selection for you. The most common mistake is
reimplementing that in user code — a nest of loops, per-image/per-value_name calls, and a final
`vcat`. Reach for the built-in first.

### Pool in ONE `pop_df` call — don't loop-and-vcat

`pop_df` pools across **populations**, across **value_names**, and (vector form) across **images** in
a single call, tagging every row with `pop`, `value_name`, and `uID`. Let it do the work.

```julia
# ✅ one call: two segmentations × many images, pooled and tagged
raw = pop_df(imgs, image_uids, "live", ["T/_tracked", "B/_tracked"];
             pop_cols = ["live.cell.speed", "live.cell.angle", "centroid_t", "track_id"])
# `raw` already carries :value_name (T / B) and :uID (source image) to group / colour by
```

```julia
# ❌ reinventing pop_df's pooling in user code
frames = DataFrame[]
for vn in value_names, img in imgs
    push!(frames, pop_df(img, "live", ["$(vn)/_tracked"]; value_name = vn))  # + redundant value_name=
end
raw = vcat(frames...; cols = :union)   # the :union vcat is the tell you split what pop_df already unions
```

The per-image / per-value_name loop and the `vcat(...; cols = :union)` are the smell: `pop_df`
already returns one unioned, tagged frame.

### The path prefix *is* the value_name selector

`"T/_tracked"` selects value_name `T`; a leading slash (`"/_tracked"`) stays within the active/given
value_name. Don't pass both a `"T/…"` prefix **and** `value_name = "T"` — pick one.

### Push selection into the view; request only the columns you need

`label_props(img) |> select_cols([...]) |> as_df`, and pass `pop_cols` to `pop_df`. Never read the
whole table and filter in memory — the view reads only the columns/rows you ask for.

### Confirm project-specifics, don't auto-detect

Pop paths and column names are project-specific. Read them from `get_populations` /
`get_measure_summary` (or a one-row `pop_df`) **up front** and name them directly. A `pickcol`-style
candidate-list guesser in the notebook is a sign the names were never confirmed — confirm instead.

---

## Notebook write rules — figures and CSV ONLY

A notebook is exploration, not a data mutation. It may write **figures (PNG/SVG/PDF) and CSV**, and
**nothing else**.

```julia
# ✅ allowed from a notebook
save("figures/speed_over_time.png", fig)
using CSV; CSV.write("output/per_cell_speed.csv", df)

# ❌ NEVER from a notebook
#   label_props(path) |> v -> add_obs(v, df) |> save!    # no cell-data writes
#   write_qc(...)                                         # no QC store writes
#   append_lab_log!(...)                                  # lab-log writes go via Chat to Claude, not code
#   any ccid.json / gating / project mutation
```

Read anything; write only figures and CSVs. If you need a derived value persisted back into the
project, that's a task, not a notebook.

---

## Generating a notebook (`create_notebook`)

The observer can create a Pluto notebook for the user via the `create_notebook(project_uid, name,
cells, description)` MCP tool — to turn a "give me the data / plot this" request into a **runnable,
editable artifact the user then owns**. The flow: you create it → the user opens it in the Notebooks
page and edits/iterates in Pluto (you guide them in chat, reading errors and suggesting corrected
cells to paste) → once happy, they run it without you.

- **`cells`** is a list of Julia cell sources (one string per cell). The **env-activation cell is
  prepended automatically — do NOT include it**. Your first cell is typically
  `using Cecelia, DataFrames, AlgebraOfGraphics, CairoMakie, CSV` (+ `CeceliaNb` for plot shortcuts),
  then `Cecelia.init_cecelia!()`, then the analysis.
- Load with `load_project` / `image_by_uid` / `pop_df` / `track_props` / `label_props |> as_df`
  (see the API reference below — read it first; don't guess names). **Follow the *Common idioms*
  above**: pool in one `pop_df` call rather than looping+`vcat`, and confirm pop paths/column names
  via `get_populations` / `get_measure_summary` instead of a candidate-list guesser. Plot with
  AlgebraOfGraphics + CairoMakie; export with `CSV.write`. Obey the **write rules above** (figures/CSV only).
- **Create-only**: a name that already exists returns 409 — it never overwrites a notebook the user
  may have edited. Pick a new name, or suggest cell edits for them to apply in Pluto.
- Suggest first, create on the user's ask — don't spam notebooks.

---

## API reference

Signatures + docstrings for the notebook-safe accessors, generated from the live package.

<!-- BEGIN GENERATED API — regenerate via Cecelia.write_repl_doc(); do not edit by hand -->

*This section is generated from the live docstrings of the `NOTEBOOK_API` accessors (`app/src/ai/repl_api.jl`). Do not edit between the markers — after changing a listed function's docstring, run `Cecelia.write_repl_doc()`; a golden test fails if it drifts.*

### `load_project`

Load a project by its UID.

### `sets`

sets(proj::CciaProject) -> Vector{CciaSet}

All sets (image cohorts) in the project.

### `images`

images(proj::CciaProject) -> Vector{CciaImage}
    images(s::CciaSet) -> Vector{CciaImage}

Every image in the project (flattened across its sets), or the images of one set.

### `image_by_uid`

image_by_uid(s::CciaSet; uid) -> Union{CciaImage,Nothing}

Look up one image by `uid` within the set (nothing if absent).

image_by_uid(proj::CciaProject; uid) -> Union{CciaImage,Nothing}

Look up one image by `uid` anywhere in the project (nothing if absent). Convenience over
`images(proj)` + filter for REPL/notebook use. (Named `image_by_uid`, not `image`, to avoid clashing
with Makie's exported `image` in a plotting notebook — same reason we export `apply_transform`.)

### `img_value_names`

img_value_names(img::CciaImage; field = :label_props) -> Vector{String}

The value_names on an image for a versioned field — by default the segmentation names (the keys of the
`label_props` field). These are the `value_name`s you pass to `pop_df` / `label_props` / `track_props`.

### `is_tracked`

is_tracked(img; value_name) -> Bool

Whether segmentation `value_name` has tracks — i.e. its label props carry a `track_id` obs column.
Cheap: reads only the obs column list, not the data. A track-grained view (`pop_type`
"track"/"trackclust") of an untracked segmentation has no data, so callers use this to say "track
first" instead of erroring or showing an empty plot.

### `pop_df`

pop_df(img, pop_type, pops; value_name=nothing, pop_cols=nothing, include_x=false,
           include_obs=true, unique_labels=true, drop_na=false, flush_cache=false,
           raw_channel_names=false) -> DataFrame

Unified population accessor. Returns the cells of `pops` with a `pop` + `value_name`
column and the requested `pop_cols` (read from the H5AD via `label_props`). Pools across
populations and across value_names: a pop path may name its value_name as a **prefix**
(`"A/qc"` → value_name `A`, path `/qc`), while a **leading-slash** path stays within the
given/active value_name (`pop_df(img, "flow", ["/qc"]; value_name="A")`). So a single call
can pool one population from several segmentations — `pop_df(img, "live",
["A/qc/_tracked", "B/qc/_tracked", "C/qc/_tracked"])` — but a leading-slash path cannot reach a
different value_name than the one passed.

- For `flow`, membership comes from gate `recompute!`. For derived pop_types (`live`, later
  `clust`), the gates still live in the `flow` map; a **derived population** is layered on top
  and is *not* stored in any gating file. Derived pops use a reserved namespace — **leaf names
  beginning with `_`** (e.g. `"_tracked"` → `track_id > 0`, see `_DERIVED_POPS`); the path's leaf
  becomes a transient filtered child of its parent and `recompute!` composes parent ∩ filter.
  Hand-drawn gates may not use the `_` prefix, so a derived name can never be shadowed by a gate.
- `granularity=:cell` (default) returns one row per cell. `granularity=:track` returns one row
  per **track**: for cell-level pop_types (`live`) cell membership still drives selection, but the
  member cells' `track_id`s pick rows from the companion per-track table `{vn}__tracks.h5ad` (track
  measures in X/var, lineage in obs, written by `tracking.track_measures`). A track belongs to a
  pop if any of its cells are in that pop; the row key is `(value_name, track_id)`.
- `pop_type="track"` gates **directly on per-track properties** (one point per track): the gate
  map loads from `gating/{vn}__tracks.json` and is evaluated over the `track_props` table (motility
  from `{vn}__tracks.h5ad` + on-read aggregates of `cell_measures`/`categorical`, keyed by
  track_id). `granularity=:track` returns the gated track rows; `granularity=:cell` expands them to
  member cells (label/track_id/pop/value_name) — selecting a track pulls in all its cells.
  `cell_measures`/`categorical` are the *base* cell columns to aggregate into track properties
  (numeric → `.mean/.median/.sum/.qUp/.qLow/.sd`, categorical → per-category frequency `{m}.{cat}`);
  pass the base names being gated/plotted (mirrors R `tracksInfo`'s `trackStatsNames`).
- `value_name=nothing` resolves to the image's **active** segmentation (parity with
  `label_props(img)`); pass a name to override the default value_name for unprefixed pops.
- `drop_na=true` drops cells that are NA/NaN in any requested `pop_cols` (mirrors R popDT
  `dropNA`).
- Results are cached per image keyed by the request signature **plus the on-disk mtimes of the
  involved gating maps + h5ads**, so a saved gate edit or a re-tracked h5ad **auto-invalidates**
  the cache. `flush_cache=true` is a manual override for in-memory edits never written to disk.

By default intensity columns are returned under their **channel names** (e.g.
`mean_intensity_0` → `"CD4"`), mirroring R `popDT` / Python `change_channel_names` — pass
`raw_channel_names=true` to keep the raw `{measure}_intensity_{i}` column names instead.

pop_df(imgs::Vector{CciaImage}, uids, pop_type, pops; kwargs...) -> DataFrame

Set-level pooling: run `pop_df` on each image and stack the results with a **`uID` column** tagging
each row's source image. This is the cross-image analogue of pooling across value_names — it lets a
summary plot compare the *same* population across images (e.g. mean `live.track.speed` of `A/_tracked`
in images X/Y/Z). `uids` is parallel to `imgs` (the API passes a `CciaSet`'s `_images` + `image_uids`).
Per-image rows are already deduped within their image; the `uID` column keeps same-label cells from
different images distinct (the `uID` is part of `pop_df`'s dedup key — see `_pop_df`). All `kwargs`
(`value_name`/`pop_cols`/`granularity`/…) pass straight through to the per-image `pop_df`.

### `label_props`

label_props(img::CciaImage; value_name=nothing)
    label_props(h5ad_path::AbstractString; channel_names=nothing, value_name="default")

Construct a lazy `LabelProps` view. The image form resolves the `.h5ad` under
`{task_dir}/labelProps/` via the versioned `label_props` field; the path form is for
tests / REPL use.

### `select_cols`

Select explicit columns (var, obs, or centroid names). Only these are read.

### `view_channel_cols`

Add the per-channel intensity columns to the selection.

### `view_centroid_cols`

Add centroid columns (optionally ordered, e.g. order=["x","y","z"]).

### `filter_rows`

Filter rows to the given label IDs (applied at read time).

**Intersection semantics:** the result contains exactly the file rows whose label is in `vals`.
Requested IDs that are absent from the file are silently skipped — no row, no `NaN`, no error —
so the output has `≤ length(vals)` rows and order follows the file, not `vals`. Callers that need
to know which IDs were missing should diff `vals` against the returned `label` column. (The gating
engine / population manager rely on this skip-missing behaviour when filtering to pop members.)

### `as_df`

as_df(lp; include_x=lp.include_x, include_obs=lp.include_obs) -> DataFrame

Reads only the requested columns from disk. Always includes a `label` column.

### `channel_columns`

Per-channel intensity var names (e.g. `mean_intensity_0`). Renamed to channel names if requested.

### `centroid_columns`

Spatial centroid column names from `uns/spatial_cols` (skimage order: z?, y, x),
optionally restricted to `order` length. Mirrors the Python `LabelPropsView.centroid_columns`.

### `col_names`

Feature names (var_names) or obs column names.

### `track_props`

track_props(img; value_name=nothing, cell_measures=String[],
                categorical=String[], numeric=String[],
                include_motility=true, quantiles=(0.95, 0.05)) -> DataFrame

Per-track property table keyed by `track_id` (one row per track; `track_id > 0` only). Columns:
`track_id`, `label` (= `track_id`, so the gating engine's by-`label` membership works unchanged),
`num_cells`, the motility measures from `{vn}__tracks.h5ad` (when `include_motility`), and per-track
aggregates of each requested **cell** column joined by `track_id`:
- numeric → `{m}.mean / .median / .sum / .qUp / .qLow / .sd`
- categorical (e.g. `live.cell.hmm.state.*`) → per-category within-track frequency `{m}.{cat}`

The numeric/categorical split is detected **automatically** from each column's decoded type +
values (see `_is_categorical_col`): strings (e.g. `hmm.transitions = "1.3"`) and integer code sets
(e.g. `hmm.state ∈ {1,2,3}`) → categorical; continuous floats (e.g. `speed = 10.12`) → numeric. No
config map (replaces the old R `config.yml` `parameters.labelStats`). `categorical`/`numeric` are
escape-hatch overrides that force a column's classification when the heuristic misreads it.

Compute-on-read; nothing persisted. Ports R `tracksInfo`.

### `track_cell_measures`

track_cell_measures(cols, motility_cols) -> Vector{String}

Inverse of `track_props`'s column naming: given desired **track-property** column names (a gating
axis or a gate's channels) and the set of motility columns (free from `{vn}__tracks.h5ad`), return
the base **cell** measures that must be aggregated to produce them — i.e. the `cell_measures` to hand
`track_props`/`pop_df(…, "track")`. A motility column (or `track_id`/`label`/`num_cells`
bookkeeping) needs no aggregation; `{base}.mean|median|sum|qUp|qLow|sd` → `base`; any other
`{base}.{cat}` (a categorical within-track frequency) → `base`. Lets the gating API request exactly
the per-track aggregates an axis needs without enumerating every measure×aggregate.

### `plot_summary_data`

plot_summary_data(img, pop_type, pops, chart_type; measure, granularity, …) -> NamedTuple
    plot_summary_data(imgs, uids, pop_type, pops, chart_type; …)                 -> NamedTuple

Compute the data behind an analysis-board **summary plot** (histogram/box/violin/bar/…) for `pops` and
a `measure`, from `pop_df`. Returns the plot-ready series (not a figure). The vector form pools the
same population across images (tagged by `uID`). Use it to reproduce a board plot's numbers in a
notebook; pick `measure` from the population's columns (`pop_df` / `get_measure_summary`).

<!-- END GENERATED API -->
