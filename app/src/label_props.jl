using HDF5
using DataFrames
using JSON3

# HDF5.jl / the underlying libhdf5 we link is NOT thread-safe: concurrent `h5open` (even on different
# files) from parallel tasks can crash or corrupt. This is the single Julia HDF5 chokepoint — every
# read/write here goes through `_with_h5`, which serialises access under one process-wide lock. It's a
# no-op under `-t 1`, and the safety net once the server runs multithreaded (parallel API handlers AND
# the scheduler's per-image task functions, both of which land here). ReentrantLock so a task that
# nests HDF5 calls doesn't self-deadlock.
const _HDF5_LOCK = ReentrantLock()
_with_h5(f::Function, path::AbstractString, mode::AbstractString="r") =
    lock(() -> h5open(f, path, mode), _HDF5_LOCK)

# ── LabelProps — Julia-native lazy reader for AnnData .h5ad ──────────────────────
#
# Mirrors the old Python `LabelPropsView` (inst/py/label_props_view.py) but reads
# H5AD directly via HDF5.jl — pure Julia, no Python on the read path (ARCHITECTURE.md).
# Lazy: column selection / row filter / sort are recorded as pending state; HDF5 I/O
# happens only inside `as_df` (opened in a `do` block, so no leaked handles).
#
# H5AD layout (see docs/DATAMODEL.md):
#   /X                 float32 (n_obs × n_var) — morphology + intensity matrix
#   /obs/_index        string-array — cell label IDs
#   /var/_index        string-array — feature names (= columns of X)
#   /obsm/spatial      float32 (n_obs × n_spatial) — centroids
#   /obsm/temporal     float32 (n_obs × 1) — timepoint (timecourse only)
#   /uns/spatial_cols  string-array — names for obsm/spatial columns
#   /uns/temporal_cols string-array — names for obsm/temporal columns
#   /uns/intensity_measure  scalar string — "mean" | "median"
#
# AnnData encodes some columns non-trivially (categorical = categories+codes group;
# sparse = csr/csc with data/indices/indptr). This reader dispatches on the
# `encoding-type` attribute and handles dense `array` + `string-array`/`string`
# (+ `categorical` for obs); it raises on sparse/unknown rather than misreading.

# @kwdef so the lazy-state / flag / write-staging fields carry their defaults here — callers
# construct with just `path`/`value_name`/`channel_names` (see `label_props` below) and adding a
# field is a one-line change with no positional call-site edits.
@kwdef mutable struct LabelProps
    path::String                              # absolute path to the .h5ad
    value_name::String = "default"
    channel_names::Union{Vector{String},Nothing} = nothing  # from the image, for channel renaming
    sel_cols::Union{Vector{String},Nothing} = nothing   # requested columns (nothing ⇒ all)
    filter_labels::Union{Vector{Int},Nothing} = nothing # row filter by label (nothing ⇒ all rows)
    sort_col::Union{String,Nothing} = nothing
    sort_rev::Bool = false
    include_x::Bool = true                     # include var (X) columns when no explicit selection
    include_obs::Bool = true                   # include obs columns
    rename_channels::Bool = false              # map intensity var names → channel names
    pending_obs::Union{DataFrame,Nothing} = nothing   # staged obs columns to write (label + cols); flushed by save!
    pending_drop::Union{Vector{String},Nothing} = nothing # staged obs column names to delete; flushed by save!
end

"""
    label_props(img::CciaImage; value_name=nothing)
    label_props(h5ad_path::AbstractString; channel_names=nothing, value_name="default")

Construct a lazy `LabelProps` view. The image form resolves the `.h5ad` under
`{task_dir}/labelProps/` via the versioned `label_props` field; the path form is for
tests / REPL use.
"""
function label_props(img::CciaImage; value_name=nothing)
    filename = isnothing(value_name) ? versioned_get(img.label_props) :
               get(img.label_props, string(value_name), nothing)
    isnothing(filename) && error("No labelProps for value_name=$(value_name) on image $(img.uid)")
    path = joinpath(img_label_props_dir(img), filename)
    cn = channel_names(img; value_name=value_name)
    LabelProps(; path=path, value_name=string(something(value_name, "default")), channel_names=cn)
end

function label_props(h5ad_path::AbstractString; channel_names=nothing, value_name="default")
    LabelProps(; path=String(h5ad_path), value_name=String(value_name), channel_names=channel_names)
end

# ── Fluent, lazy column/row selection (each returns the view) ────────────────────

_add_cols!(lp::LabelProps, cols) = begin
    isnothing(lp.sel_cols) && (lp.sel_cols = String[])
    for c in cols
        c in lp.sel_cols || push!(lp.sel_cols, String(c))
    end
    lp
end

"""Select explicit columns (var, obs, or centroid names). Only these are read."""
select_cols(lp::LabelProps, cols) = _add_cols!(lp, cols)
const view_cols = select_cols

"""Add the per-channel intensity columns to the selection."""
view_channel_cols(lp::LabelProps) = _add_cols!(lp, channel_columns(lp))

"""Add the centroid columns to the selection — the explicit spatial axes (`centroid_x`/`_y`/`_z`,
present only) plus the temporal `centroid_t`. `order` selects the spatial axes BY AXIS
(`order=[:x,:y,:z]`), never positionally."""
view_centroid_cols(lp::LabelProps; order=nothing) =
    _add_cols!(lp, vcat(centroid_columns(lp; order=order), temporal_columns(lp)))

"""Ensure the label column is selected (it is always returned regardless)."""
view_label_col(lp::LabelProps) = lp

"""
Filter rows to the given label IDs (applied at read time).

**Intersection semantics:** the result contains exactly the file rows whose label is in `vals`.
Requested IDs that are absent from the file are silently skipped — no row, no `NaN`, no error —
so the output has `≤ length(vals)` rows and order follows the file, not `vals`. Callers that need
to know which IDs were missing should diff `vals` against the returned `label` column. (The gating
engine / population manager rely on this skip-missing behaviour when filtering to pop members.)
"""
function filter_rows(lp::LabelProps, vals; by::Symbol=:label)
    by === :label || error("filter_rows: only by=:label is supported (got $by)")
    lp.filter_labels = collect(Int, vals)
    lp
end

"""Sort the resulting DataFrame by a column."""
function sort_by(lp::LabelProps, col; rev::Bool=false)
    lp.sort_col = String(col)
    lp.sort_rev = rev
    lp
end

"""Map intensity var names to image channel names in the output (if available)."""
rename_channels!(lp::LabelProps, on::Bool=true) = (lp.rename_channels = on; lp)

# ── AnnData decode helpers ───────────────────────────────────────────────────────

_enc(obj) = haskey(HDF5.attrs(obj), "encoding-type") ?
            String(HDF5.read_attribute(obj, "encoding-type")) : ""

# Read a 1-D dataset/group as a Julia vector, dispatching on AnnData encoding-type.
function _read_anndata_vector(parent, name)
    obj = parent[name]
    enc = _enc(obj)
    if obj isa HDF5.Group
        if enc == "categorical"
            codes = read(obj["codes"])                    # 0-based integer codes
            cats  = _as_strings(read(obj["categories"]))
            return [c < 0 ? missing : cats[c + 1] for c in codes]
        else
            error("LabelProps: unsupported group encoding '$enc' for '$name'")
        end
    end
    data = read(obj)
    if enc in ("csr_matrix", "csc_matrix")
        error("LabelProps: sparse '$enc' columns are not supported yet ('$name')")
    end
    data isa AbstractString ? data : data   # dense array or string-array
end

# always Vector{String} — `collect(String, …)` keeps an empty array typed as String[]
# (a broadcast `String.(x)` over an empty Union{}-eltype HDF5 read yields Vector{Union{}})
_as_strings(x) = x isa AbstractString ? [x] : collect(String, x)

# Orient X so we can slice a single feature column lazily. AnnData writes (n_obs, n_var)
# C-order; HDF5.jl may report dims reversed, so detect which axis is `var`.
function _var_axis(dset, n_obs::Int, n_var::Int)::Int
    sz = size(dset)
    sz[1] == n_var && sz[2] == n_obs && return 1   # (n_var, n_obs)
    sz[1] == n_obs && sz[2] == n_var && return 2   # (n_obs, n_var)
    error("LabelProps: /X dims $sz do not match (n_obs=$n_obs, n_var=$n_var)")
end

_read_feature(dset, j::Int, var_axis::Int) = var_axis == 1 ? dset[j, :] : dset[:, j]

function _read_obsm_col(fid, group::String, idx::Int, n_obs::Int, n_dim::Int)
    dset = fid["obsm/$group"]
    sz = size(dset)
    if sz == (n_dim, n_obs)
        return dset[idx, :]
    elseif sz == (n_obs, n_dim)
        return dset[:, idx]
    else
        error("LabelProps: obsm/$group dims $sz do not match (n_obs=$n_obs, n_dim=$n_dim)")
    end
end

# ── Metadata helpers (cheap; open the file briefly) ──────────────────────────────

"""Feature names (var_names) or obs column names."""
function col_names(lp::LabelProps; data_type::Symbol=:vars)::Vector{String}
    _with_h5(lp.path, "r") do fid
        if data_type === :vars
            return _as_strings(read(fid["var/_index"]))
        elseif data_type === :obs
            cols = haskey(HDF5.attrs(fid["obs"]), "column-order") ?
                   _as_strings(HDF5.read_attribute(fid["obs"], "column-order")) : String[]
            return cols
        else
            error("col_names: data_type must be :vars or :obs")
        end
    end
end

"""
    n_obs(lp) -> Int

Number of observations (cells) — the length of the obs index. Reads the dataset **dims only**
(no column materialised), so it's a cheap count for QC/banking. `0` if the file has no obs index.
"""
function n_obs(lp::LabelProps)::Int
    _with_h5(lp.path, "r") do fid
        haskey(fid, "obs/_index") || return 0
        Int(first(size(fid["obs/_index"])))
    end
end

_intensity_measure(fid) = haskey(fid, "uns/intensity_measure") ?
                          String(read(fid["uns/intensity_measure"])) : "mean"

"""Per-channel intensity var names (e.g. `mean_intensity_0`). Renamed to channel names if requested."""
function channel_columns(lp::LabelProps; as_channel_names::Bool=false)::Vector{String}
    _with_h5(lp.path, "r") do fid
        vars = _as_strings(read(fid["var/_index"]))
        measure = _intensity_measure(fid)
        pat = Regex("(^|_)$(measure)_intensity_\\d+\$")
        cols = filter(v -> occursin(pat, v), vars)
        if as_channel_names && !isnothing(lp.channel_names)
            return [_channel_label(c, lp.channel_names) for c in cols]
        end
        return cols
    end
end

# map "mean_intensity_3" → channel_names[4]; "nuc_mean_intensity_0" → "nuc_<ch0>"
function _channel_label(varname::String, chans::Vector{String})
    m = match(r"^(?:(?<prefix>[a-z]+)_)?(?:mean|median)_intensity_(?<idx>\d+)$", varname)
    isnothing(m) && return varname
    idx = parse(Int, m[:idx])
    idx + 1 > length(chans) && return varname
    isnothing(m[:prefix]) ? chans[idx + 1] : "$(m[:prefix])_$(chans[idx + 1])"
end

# Centroids are stored in obsm and labelled explicitly: `centroid_x`/`centroid_y`/`centroid_z` (present
# axes only) in `uns/spatial_cols`, and `centroid_t` in `uns/temporal_cols`. Names are written explicitly
# at measurement time and by the migration (docs/todo/CENTROID_AXES_PLAN.md); the reader takes them
# verbatim — there is NO positional `centroid-N` fallback (run the migration on pre-existing data).
# Every consumer selects by axis name, never by position (the celltrackR x,y,z-vs-skimage z,y,x mismatch
# makes positional reads a silent 2D bug). `axis_of` is the one place that parses an axis from a name.
axis_of(col::AbstractString)::Symbol = Symbol(match(r"^centroid_([xyzt])$", col)[1])

_read_uns_strings(fid, key) = haskey(fid, key) ? _as_strings(read(fid[key])) : String[]

# `centroid-N` (skimage's positional names) and a bare `t` are NOT acceptable anywhere — measurement
# and the migration write explicit axis names. Fail loudly the moment a stale/un-migrated file is read,
# rather than silently mis-mapping axes (docs/todo/CENTROID_AXES_PLAN.md).
function _check_centroid_names(names::Vector{String}, kind::Symbol)::Vector{String}
    pat = kind === :spatial ? r"^centroid_[xyz]$" : r"^centroid_t$"
    for n in names
        occursin(pat, n) || error(
            "LabelProps: non-explicit $kind centroid name '$n' — expected " *
            (kind === :spatial ? "centroid_x/_y/_z" : "centroid_t") *
            ". This file predates the centroid-axis rename; run the migration " *
            "(docs/todo/CENTROID_AXES_PLAN.md).")
    end
    names
end
_spatial_uns(fid)  = _check_centroid_names(_read_uns_strings(fid, "uns/spatial_cols"), :spatial)
_temporal_uns(fid) = _check_centroid_names(_read_uns_strings(fid, "uns/temporal_cols"), :temporal)

"""Spatial centroid column names (`centroid_x`/`_y`/`_z`, present axes only, verbatim from
`uns/spatial_cols`). `order` selects BY AXIS — `order=[:x,:y,:z]` returns the present axes in that
order (z dropped for 2D), never a positional slice. Mirrors the old R `LabelPropsView.centroid_columns`.
Errors on a pre-migration `centroid-N` file."""
function centroid_columns(lp::LabelProps; order=nothing)::Vector{String}
    cols = _with_h5(lp.path, "r") do fid
        _spatial_uns(fid)
    end
    isnothing(order) && return cols
    want = ["centroid_$(lowercase(string(a)))" for a in order]
    filter(in(cols), want)
end

"""Temporal column name (`["centroid_t"]`; empty for non-timecourse) — verbatim from `uns/temporal_cols`."""
function temporal_columns(lp::LabelProps)::Vector{String}
    _with_h5(lp.path, "r") do fid
        _temporal_uns(fid)
    end
end

# ── Terminal: materialise the DataFrame (the only place HDF5 I/O happens) ─────────

"""
    as_df(lp; include_x=lp.include_x, include_obs=lp.include_obs) -> DataFrame

Reads only the requested columns from disk. Always includes a `label` column.
"""
function as_df(lp::LabelProps; include_x::Bool=lp.include_x, include_obs::Bool=lp.include_obs)::DataFrame
    _with_h5(lp.path, "r") do fid
        # ── index + sizes ──
        labels_raw = _as_strings(read(fid["obs/_index"]))
        n_obs = length(labels_raw)
        labels = _maybe_int.(labels_raw)
        var_names = _as_strings(read(fid["var/_index"]))
        n_var = length(var_names)
        var_idx = Dict(v => i for (i, v) in enumerate(var_names))

        # centroid name → (group, position, ndim). Map BOTH the on-disk name and its explicit
        # `centroid_{axis}` alias to the same obsm column, so a consumer can select "centroid_x" even
        # on a legacy file that stores "centroid-0" (normalisation lives in _explicit_*_names). This is
        # the legacy bridge that lets the code deploy before the data migration (CENTROID_AXES_PLAN.md).
        # Guard on read: a file carrying `centroid-N`/bare-`t` is rejected outright (errors), so those
        # names can NEVER reach a returned DataFrame. Convert the file first (CENTROID_AXES_PLAN.md).
        cent_map = Dict{String,Tuple{String,Int,Int}}()
        raw_spatial  = _spatial_uns(fid)
        raw_temporal = _temporal_uns(fid)
        n_sp = length(raw_spatial); n_tp = length(raw_temporal)
        for (i, c) in enumerate(raw_spatial);  cent_map[c] = ("spatial", i, n_sp);  end
        for (i, c) in enumerate(raw_temporal); cent_map[c] = ("temporal", i, n_tp); end
        all_centroid_cols = vcat(raw_spatial, raw_temporal)   # set returned when no explicit selection
        obs_cols = haskey(HDF5.attrs(fid["obs"]), "column-order") ?
                   _as_strings(HDF5.read_attribute(fid["obs"], "column-order")) : String[]

        # ── resolve which columns to read ──
        local var_cols::Vector{String}, cent_cols::Vector{String}, ocols::Vector{String}
        if isnothing(lp.sel_cols)
            var_cols  = include_x ? var_names : String[]
            cent_cols = all_centroid_cols
            ocols     = include_obs ? obs_cols : String[]
        else
            sel = lp.sel_cols
            var_cols  = [c for c in sel if haskey(var_idx, c)]
            cent_cols = [c for c in sel if haskey(cent_map, c)]
            ocols     = [c for c in sel if c in obs_cols]
            unknown = setdiff(sel, vcat(var_cols, cent_cols, ocols, ["label"]))
            isempty(unknown) || @warn "LabelProps: ignoring unknown columns $unknown"
        end

        # ── row filter (compute kept indices once) ──
        keep = isnothing(lp.filter_labels) ? Colon() :
               findall(in(Set(lp.filter_labels)), labels)

        df = DataFrame()
        df.label = keep === Colon() ? labels : labels[keep]

        # var columns (lazy per-column hyperslab read from X)
        if !isempty(var_cols)
            Xds = fid["X"]
            vax = _var_axis(Xds, n_obs, n_var)
            for c in var_cols
                col = _read_feature(Xds, var_idx[c], vax)
                outname = lp.rename_channels ? _channel_label(c, something(lp.channel_names, String[])) : c
                df[!, outname] = keep === Colon() ? col : col[keep]
            end
        end

        # centroid columns (from obsm)
        for c in cent_cols
            grp, pos, n_dim = cent_map[c]
            col = _read_obsm_col(fid, grp, pos, n_obs, n_dim)
            df[!, c] = keep === Colon() ? col : col[keep]
        end

        # obs columns
        for c in ocols
            col = _read_anndata_vector(fid["obs"], c)
            df[!, c] = keep === Colon() ? col : col[keep]
        end

        # ── sort ──
        if !isnothing(lp.sort_col) && lp.sort_col in names(df)
            sort!(df, lp.sort_col; rev=lp.sort_rev)
        end
        return df
    end
end

# label IDs are integers stored as strings; keep Int when possible
_maybe_int(s::AbstractString) = something(tryparse(Int, s), s)
_maybe_int(x::Integer) = Int(x)
_maybe_int(x::Real) = Int(round(x))

"""Read the full feature matrix as a DataFrame (convenience for analysis)."""
as_matrix(lp::LabelProps) = as_df(lp; include_x=true, include_obs=false)

"""
    obsm_keys(lp) -> Vector{String}

Names of the `obsm` matrices present in the file (e.g. `["spatial", "temporal", "X_umap"]`).
"""
function obsm_keys(lp::LabelProps)::Vector{String}
    _with_h5(lp.path, "r") do fid
        haskey(fid, "obsm") ? collect(String, keys(fid["obsm"])) : String[]
    end
end

"""
    obsm(lp, key) -> Matrix{Float64}   (n_obs × k, obs order)

Read a full `obsm` matrix (e.g. an embedding `"X_umap"`) in obs order — pair with the `label`
column from `as_df` for alignment. Returns a `0×0` matrix if the key is absent. The Python
`LabelPropsView.obsm` is the mirror; both write via `add_obsm`. Orientation is auto-detected
(AnnData writes `(n_obs, k)` C-order; HDF5.jl may report it transposed).
"""
function obsm(lp::LabelProps, key::AbstractString)::Matrix{Float64}
    _with_h5(lp.path, "r") do fid
        haskey(fid, "obsm/$key") || return Matrix{Float64}(undef, 0, 0)
        n_obs = length(_as_strings(read(fid["obs/_index"])))
        dset  = fid["obsm/$key"]
        data  = read(dset)
        m = Matrix{Float64}(data)
        # orient to (n_obs × k): AnnData stores (n_obs, k); HDF5.jl may report (k, n_obs)
        return size(m, 1) == n_obs ? m : permutedims(m)
    end
end

# ── Write path: stage obs columns, flush with save! ──────────────────────────────
#
# The reader's mirror image: you read a label-keyed DataFrame (`as_df`), you write a
# label-keyed DataFrame (`add_obs` then `save!`). Same chain idiom in Julia and Python
# (`LabelPropsView.add_obs(...).save()`), so there is one way to touch H5AD storage and
# no guessing (docs/DATAMODEL.md). Julia writes the 1-D obs columns it computes directly
# via HDF5.jl with the correct AnnData encoding; structural changes (X/var, new files)
# remain Python-anndata's job (the modules that own those files).

"""
    add_obs(lp, df::DataFrame) -> lp

Stage obs columns to write. `df` must have a `label` column plus one column per obs field;
values are aligned to the file's obs index **by label** at `save!` time (labels absent from
`df` get `NaN`). Repeated `add_obs` calls accumulate (later columns win on name clash).
Floating-point columns only (track measures, etc.); terminal verb is `save!`.
"""
function add_obs(lp::LabelProps, df::DataFrame)
    "label" in names(df) || error("add_obs: DataFrame needs a `label` column")
    if isnothing(lp.pending_obs)
        lp.pending_obs = copy(df)
    else
        lp.pending_obs = outerjoin(lp.pending_obs, df; on=:label, makeunique=false)
    end
    lp
end

"""
    drop_obs(lp, names) -> lp

Stage obs columns to delete (by name). Names absent from the file are ignored (idempotent).
Flushed by `save!`. Use to invalidate derived columns whose source changed — e.g. the
tracking task drops stale `live.cell.*` / `live.track.*` measures when it writes new tracks.
Combine with `add_obs` in one chain; drops and adds are applied in the same `save!`.
"""
function drop_obs(lp::LabelProps, names)
    isnothing(lp.pending_drop) && (lp.pending_drop = String[])
    for nm in names
        nm = String(nm)
        nm in lp.pending_drop || push!(lp.pending_drop, nm)
    end
    lp
end

# ── Curried chain forms ──────────────────────────────────────────────────────────
# Single-argument variants of the pipe verbs that capture the second argument and return
# `lp -> verb(lp, …)`, so chains read without the `v -> verb(v, …)` lambda:
#
#     label_props(path) |> select_cols(cols) |> add_obs(df) |> save!
#
# Arity disambiguates these from the two-argument methods above (Julia never confuses `f(x)` with
# `f(lp, x)`); the argument is typed to exclude `LabelProps` so a stray one-arg call can't silently
# return a closure. Terminal verbs (`as_df`, `save!`) and the no-arg views already pipe directly.
select_cols(cols::Union{AbstractString,AbstractVector}) = lp -> select_cols(lp, cols)
filter_rows(vals::AbstractVector; by::Symbol=:label)    = lp -> filter_rows(lp, vals; by=by)
sort_by(col::Union{AbstractString,Symbol}; rev::Bool=false) = lp -> sort_by(lp, col; rev=rev)
add_obs(df::DataFrame)                                  = lp -> add_obs(lp, df)
drop_obs(names::AbstractVector)                         = lp -> drop_obs(lp, names)

"""
    save!(lp) -> lp

Terminal write: flush the obs columns staged by `add_obs`/`drop_obs` into the `.h5ad`, in
place. Added columns become `/obs/<name>` float64 datasets with AnnData attrs
`encoding-type="array"`, `encoding-version="0.2.0"`; adds align by label to the existing obs
index (unset labels → `NaN`). One open/write/close (batched) — no whole-file rewrite. No-op if
nothing is staged.

Write order is deliberate for **crash safety**, reconciling the opposite needs of adds and drops:
1. write all added datasets (a new dataset must exist *before* `column-order` lists it);
2. write the final `column-order` = old + adds − drops (this both lists the adds and de-lists
   the drops — a dropped name is removed from the listing *before* its dataset is deleted);
3. delete the dropped datasets last.
A crash at any point leaves a readable file: orphan datasets (written-but-unlisted, or
dropped-but-undeleted) are simply ignored by the reader and by `anndata`. Do not reorder.
"""
function save!(lp::LabelProps)
    (isnothing(lp.pending_obs) && isnothing(lp.pending_drop)) && return lp
    drops   = something(lp.pending_drop, String[])
    pend    = lp.pending_obs
    valcols = isnothing(pend) ? String[] : filter(!=("label"), names(pend))
    plabels = isnothing(pend) ? Int[] : _maybe_int.(pend.label)
    # A column staged in BOTH drop and add is a re-add — the add wins (matches the Python writer's
    # drop-before-add contract). Without this, the drop would de-list and delete the just-written
    # dataset (e.g. overwriting a categorical hmm.state with a numeric one in one chain).
    drops = filter(c -> c ∉ valcols, drops)

    _with_h5(lp.path, "r+") do fid
        obs        = fid["obs"]
        idx_labels = _maybe_int.(_as_strings(read(obs["_index"])))
        rowof      = Dict(l => i for (i, l) in enumerate(idx_labels))
        n          = length(idx_labels)

        colorder = haskey(HDF5.attrs(obs), "column-order") ?
                   _as_strings(HDF5.read_attribute(obs, "column-order")) : String[]

        # (1) write added datasets first — a dataset must exist before column-order names it.
        for c in valcols
            arr = fill(NaN, n)
            src = pend[!, c]
            for (k, lab) in enumerate(plabels)
                r = get(rowof, lab, 0)
                r == 0 && continue
                v = src[k]
                arr[r] = (ismissing(v) || v === nothing) ? NaN : Float64(v)
            end
            haskey(obs, c) && HDF5.delete_object(obs, c)   # overwrite existing
            obs[c] = arr
            attrs(obs[c])["encoding-type"]    = "array"
            attrs(obs[c])["encoding-version"] = "0.2.0"
            c in colorder || push!(colorder, c)
        end

        # (2) write final column-order = old + adds − drops (de-lists drops before deletion).
        filter!(c -> c ∉ drops, colorder)
        haskey(HDF5.attrs(obs), "column-order") && HDF5.delete_attribute(obs, "column-order")
        attrs(obs)["column-order"] = colorder

        # (3) delete dropped datasets last (now unreferenced by column-order).
        for c in drops
            haskey(obs, c) && HDF5.delete_object(obs, c)
        end
    end

    lp.pending_obs  = nothing
    lp.pending_drop = nothing
    lp
end

# ── Categorical / string obs write (via Python anndata) ──────────────────────────
# `save!` above writes NUMERIC obs columns only (plain float64 arrays via HDF5.jl). Categorical
# columns (HMM states, cluster ids) and string columns (HMM transitions like "1_2") need anndata's
# categorical encoding (categories + codes), so they are written through a Python subprocess
# (py/tasks/labels/write_categorical_obs_run.py) — the same "new encodings are Python's job" split
# as the per-track table (docs/DATAMODEL.md, docs/ARCHITECTURE.md). The reader above already decodes
# categoricals, so once written these round-trip like any obs column.

"""
    write_categorical_obs(props_path, columns; drop=String[], on_log, on_process) -> Bool

Write/replace categorical obs columns in an existing labelProps `.h5ad`. `columns` is a vector of
`(name, labels, values)` named tuples — `values` aligned to `labels`; a `missing`/`nothing` value
or a label absent from the column is left unset (category code -1 → NaN). `drop` removes obs
columns first. Runs the Python writer as a subprocess via `python_bin_path()`; returns success.
"""
function write_categorical_obs(props_path::AbstractString, columns::AbstractVector;
                               drop::AbstractVector=String[],
                               on_log::Function     = line -> println(line),
                               on_process::Function = _ -> nothing)::Bool
    isfile(props_path) || (on_log("[ERROR] No labelProps: $props_path"); return false)
    cols = [(; name   = String(c.name),
               labels = collect(Int, c.labels),
               values = Any[v === missing ? nothing : (v isa AbstractString ? String(v) : v)
                            for v in c.values])
            for c in columns]
    # props_path = {img._dir}/labelProps/{vn}.h5ad → up two dirs reaches img._dir
    run_py("writers/write_categorical_obs_run.py",
        (; filepath = String(props_path), columns = cols, drop = String.(collect(drop))),
        task_run_dir(dirname(dirname(props_path)));
        on_log = on_log, on_process = on_process)
end
