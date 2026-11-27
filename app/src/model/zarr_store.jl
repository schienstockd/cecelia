# ── OME-ZARR store structure (read-only) ──────────────────────────────────────
#
# The Julia side's structural reads of a store, for either layout (bioformats2raw series wrapper or
# flat root) and either zarr format (v2 `.zattrs`/`.zarray`, v3 `zarr.json`): `series_base`,
# `ngff_attrs`, `ngff_multiscales`, `ngff_version`, `zarr_array_meta`, `store_axis_sizes`, and the
# stat-only change fingerprint `store_rev` / `store_written_at`. Metadata JSON only — never pixels
# (pixels are `api/src/image_geometry.jl`'s `open_level`). Model tier, because QC, the task framework
# and the spatial readers ask these questions too; the import task's readers and writers
# (`importImages/omezarr/`) build on them. This file and that family are the only places that may
# name the raw metadata files (app/test/suite/task_validation.jl checks).
using JSON3

"""
Directory whose `.zattrs` carries the NGFF `multiscales` — the bioformats2raw series wrapper
(`zarr/0`) or, for a flat `create_multiscales` store, `zarr` itself. Julia mirror of Python's
`zarr_utils.series_base`; the ONE place on this side that decides the layout, so a reader or a
writer can't quietly understand only one of the two (docs/ARCHITECTURE.md → **OME-ZARR dual-format**).

Detection is STRUCTURAL — does `0/.zattrs` actually carry a `multiscales` attr — not the path
suffix, because both layouts have a `0/` child: a group in the series layout, the level-0 ARRAY
(whose `.zattrs` is `{}`) in the flat one.
"""
function series_base(zarr_path::AbstractString)::String
    series = joinpath(zarr_path, "0")
    ms = ngff_multiscales(series)
    (isnothing(ms) || isempty(ms)) || return series
    String(zarr_path)
end

"""
NGFF attributes of a zarr GROUP directory, for **either** zarr format. `nothing` when the directory
carries no readable group metadata.

This is the Julia half of the v2-vs-v3 question, and it lives next to `series_base` on purpose: one
resolver per question per language (docs/ARCHITECTURE.md → **OME-ZARR dual-format**). Do NOT add a parallel set
of v3 readers — route through here.

Name matches the Python twin (`zarr_utils.ngff_attrs`) on purpose — the cross-language contract on
this question is by name.

* zarr v2 / NGFF 0.4 — `.zattrs`, attributes at the **top level**
* zarr v3 / NGFF 0.5 — `zarr.json` → `attributes`, everything nested one level down under **`ome`**

The *content* is identical in both (same axes, datasets, coordinateTransformations), which is why
unwrapping is the whole difference. Python gets this cheaper because `zarr-python`'s `Group.attrs`
already hides the file-level half (its `ngff_attrs` takes an `attrs` object); Julia reads the JSON
itself, so it handles both halves here and takes a directory path.
"""
function ngff_attrs(group_dir::AbstractString)
    zattrs = joinpath(group_dir, ".zattrs")
    if isfile(zattrs)
        try
            return JSON3.read(read(zattrs, String))
        catch
            return nothing
        end
    end
    zjson = joinpath(group_dir, "zarr.json")
    isfile(zjson) || return nothing
    try
        attrs = get(JSON3.read(read(zjson, String)), :attributes, nothing)
        isnothing(attrs) && return nothing
        inner = get(attrs, :ome, nothing)      # NGFF 0.5 nests under `ome`; 0.4-in-v3 would not
        isnothing(inner) ? attrs : inner
    catch
        nothing
    end
end

"""
`multiscales` list of a zarr group directory, or `nothing` — version-agnostic (see `ngff_attrs`).
"""
function ngff_multiscales(group_dir::AbstractString)
    attrs = ngff_attrs(group_dir)
    isnothing(attrs) && return nothing
    ms = get(attrs, :multiscales, nothing)
    (isnothing(ms) || isempty(ms)) ? nothing : ms
end

"""
OME-NGFF spec version a store declares (e.g. `"0.4"`, `"0.5"`), or `nothing` when it declares none.

Not the same question as the ZARR format, and the two are not interchangeable even though they move
together in practice: the zarr format is how the bytes and metadata files are laid out, the NGFF
version is which image-metadata spec those attributes follow. Reported side by side in the image
metadata modal so "what is this store?" is answerable without opening a terminal.

0.5 carries it on the `ome` group attribute (which `ngff_attrs` has already unwrapped by the
time we see it); 0.4 and earlier carry it per-multiscales-entry.
"""
function ngff_version(zarr_path::AbstractString)
    base  = series_base(zarr_path)
    attrs = ngff_attrs(base)
    isnothing(attrs) && return nothing
    v = get(attrs, :version, nothing)                     # NGFF 0.5 (on the `ome` attribute)
    isnothing(v) || return string(v)
    ms = get(attrs, :multiscales, nothing)                # NGFF 0.4 and earlier (per entry)
    (isnothing(ms) || isempty(ms)) && return nothing
    mv = get(first(ms), :version, nothing)
    isnothing(mv) ? nothing : string(mv)
end

"""
Metadata of a zarr ARRAY directory (`shape`, `chunks`, dtype/codecs), for either format — `.zarray`
(v2) or `zarr.json` (v3). `nothing` when unreadable. Both carry `shape`, so a caller that only needs
the extent can treat them alike; anything format-specific must branch explicitly.
"""
function zarr_array_meta(array_dir::AbstractString)
    zarray = joinpath(array_dir, ".zarray")
    if isfile(zarray)
        try
            return JSON3.read(read(zarray, String))   # a `.zarray` IS an array — nothing to discriminate
        catch
            return nothing
        end
    end
    # v3 puts groups AND arrays in the same filename, so here the node type has to be checked —
    # otherwise a group's `zarr.json` would be handed back as if it described an array.
    zjson = joinpath(array_dir, "zarr.json")
    isfile(zjson) || return nothing
    try
        m = JSON3.read(read(zjson, String))
        string(get(m, :node_type, "")) == "array" ? m : nothing
    catch
        nothing
    end
end


"""Level-0 extent of ONE store by lower-case NGFF axis name (only the axes it has; a label store has no
`"c"`), either layout and zarr format; empty when unreadable. Unlike `read_ome_metadata` it has X/Y: it
describes the store passed (`img_filepath(img, vn)` / `img_labels_path(img, vn)`), not the image."""
function store_axis_sizes(zarr_path::AbstractString)::Dict{String,Int}
    base = series_base(zarr_path); ms = ngff_multiscales(base)
    isnothing(ms) && return Dict{String,Int}()
    axes = [lowercase(string(get(ax, :name, ""))) for ax in get(first(ms), :axes, [])]
    ds   = get(first(ms), :datasets, [])
    lvl  = isempty(ds) ? "0" : string(get(first(ds), :path, "0"))
    path_inside(base, joinpath(base, lvl)) || (lvl = "0")   # the attrs are the file author's text
    meta = zarr_array_meta(joinpath(base, lvl))
    shape = isnothing(meta) ? Int[] : collect(Int, get(meta, :shape, []))
    length(shape) == length(axes) ? Dict(zip(axes, shape)) : Dict{String,Int}()
end

"""
A cheap "did this store's bytes change" fingerprint: inode + mtime + ctime of the store root and
its root/level-0 metadata files — a handful of `stat`s, never a walk (a drift-corrected store on
zolIMa is 300k chunk files). Every rewrite path either recreates the root (rmtree + `mode='w'`,
staging dir + rename swap) or rewrites the metadata, so one of these moves; the viewer compares it
across a task-done meta refetch and reloads only when a store it SHOWS changed. "" when absent.
A bioformats2raw import keeps its multiscales one level down (`<store>/0/`); `series_base` is the
canonical resolver, so both layouts fingerprint their real metadata — a calibration rewrite
(`set_ngff_axes`) moves it too.
"""
function store_rev(path::AbstractString)
    isdir(path) || return ""
    parts = String[]
    for p in _store_meta_paths(path)
        st = stat(p)
        ispath(st) && push!(parts, "$(st.inode):$(st.mtime):$(st.ctime)")
    end
    join(parts, "|")
end

# The store root plus its root/level-0 metadata files, in both layouts — what `store_rev` and
# `store_written_at` stat.
function _store_meta_paths(path::AbstractString)
    base = try series_base(path) catch; String(path) end
    unique([String(path); [joinpath(b, rel) for b in unique([String(path), base])
                           for rel in (".zattrs", ".zgroup", "zarr.json",
                                       joinpath("0", ".zarray"), joinpath("0", "zarr.json"))]])
end

"""
    store_written_at(path) -> Float64

The latest mtime (unix seconds) among the same stat-only set `store_rev` fingerprints — a lower bound
on when the store's current contents were written, and `0.0` when the store is absent. Every writer
fills a fresh staging dir and renames it into place, so these files are written by the run that
produced the store; the rename itself is not relied on, since a directory rename does not move the
directory's own mtime on every filesystem. Answers "is this store newer than that run's start?".
"""
function store_written_at(path::AbstractString)::Float64
    isdir(path) || return 0.0
    maximum(p -> (st = stat(p); ispath(st) ? Float64(st.mtime) : 0.0), _store_meta_paths(path))
end
