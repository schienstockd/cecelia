# ── Image geometry (Julia-side, metadata only) ────────────────────────────────────────────────────
#
# "How big is this image version, on which axes?" — resolved from the store itself, for a SPECIFIC
# image version. Everything here is metadata + array shape: no pixels are read.
#
# WHY IT IS PER-VERSION, and why a stored per-image size would be wrong: `filepath` is a versioned
# field (`default` / drift-corrected / AF-corrected / cropped …), and those versions do not share a
# frame extent. Drift correction EXPANDS the canvas (there is a whole QC finding for it,
# `output.canvas_expansion`) and a crop shrinks it. EaMaVq: 512×512 as imported, 544×548 once
# drift-corrected. So a consumer that needs the extent must name a version — or accept the ACTIVE
# one, which is what the tasks themselves run against.
#
# HISTORY: this was `_crop_*` inside the old `crop_render.jl`, private to the crop panel because that
# was the first consumer. None of it is crop-specific (Dominik, 2026-07-30) — the crop panel just
# happened to ask first. Pulled out here so the second consumer (the anisotropy grid-size advisory)
# reuses it instead of growing a parallel reader. The same audit then found the RENDERER wasn't
# crop-specific either, so that became `image_render.jl` and `crop_api.jl` is now just the two routes.
#
# This inherits `image_render.jl`'s SANCTIONED, NARROW carve-out of the "one canonical image reader"
# rule: Julia touches the zarr directly (Zarr.jl) only for lightweight metadata/preview work; Python
# `zarr_utils` stays canonical for anything that processes data. Do not grow this into a general
# image reader.

"""
    open_level0(zarr_path) -> (array, c_order_axis_names)

Level-0 array of a cecelia OME-ZARR plus its NGFF axis names (C-order, e.g. `["t","c","z","y","x"]`).
Handles both on-disk layouts: flat (root group, array at `"0"`) and the bioformats2raw series (group
at `"0"`, array at `"0/0"`). Axes come from the multiscales `.zattrs` on whichever group carries it.
"""
function open_level0(zarr_path::AbstractString)
    g = zopen(zarr_path)
    node = g["0"]
    if node isa Zarr.ZArray                    # flat: root .zattrs has the multiscales; array is "0"
        arr, attrs_dir = node, zarr_path
    else                                       # series: "0" is a group, level-0 array is "0/0"
        arr, attrs_dir = node["0"], joinpath(zarr_path, "0")
    end
    arr, read_ngff_axes(attrs_dir)
end

"""
    read_native(arr, idx...) -> Array

Read a block out of a Zarr.jl array **with the stored byte order applied**. ALWAYS read pixels through
this — never `arr[idx...]` directly.

`bioformats2raw` writes big-endian arrays (`>u2` for uint16), and every raw imported `default` version
in a cecelia project is one. Zarr.jl parses that descriptor for the *element type* but hands back the
bytes **unswapped**, so `eltype(arr) === UInt16` while the values are byte-swapped garbage: a true 63
reads as 16128, and 98% of a real frame lands above a contrast ceiling that should have clipped none of
it. There is no error — the preview just renders saturated white noise. Python is immune because numpy
honours the `>u2` descriptor on read, which is why this only ever bit the Julia preview path.

`ntoh`/`ltoh` are no-ops when the store's order already matches the host, so this is correct on a
big-endian machine too, and a no-op for 1-byte dtypes (`|u1`).

See `docs/NAPARI.md` → *Byte order (big-endian zarr)* for the other half of this trap (the writers
force native order via `zarr_utils.native_dtype`, so corrected/cropped versions are little-endian).
"""
function read_native(arr, idx...)
    blk = arr[idx...]
    order = _zarr_byte_order(arr)
    order == '>' && return ntoh.(blk)
    order == '<' && return ltoh.(blk)
    blk                                          # '|' (not applicable, 1-byte) or unknown → as-is
end

# Leading character of the numpy dtype descriptor in the array's zarr metadata: '>' big, '<' little,
# '|' not-applicable. Zarr.jl v2 metadata keeps it as the raw string (e.g. ">u2"); anything else
# (a v3 store, a future metadata shape) answers '|' so we never swap on a guess.
function _zarr_byte_order(arr)::Char
    dt = try
        getfield(arr.metadata, :dtype)
    catch
        return '|'
    end
    (dt isa AbstractString && !isempty(dt)) ? first(dt) : '|'
end
# Why a zarr v3 store correctly answers '|' (never swap) rather than needing the codec parsed:
# v3 does not put byte order in the dtype at all — `metadata.dtype` is a plain `"uint16"`, and the
# order lives in the `bytes` codec INSIDE the codec pipeline. That pipeline is Zarr.jl's to execute,
# and it does: a v3 store and a v2 store written from the same source decode to IDENTICAL pixels
# (asserted in the *"API: zarr byte order"* testset). The v2 case is the odd one out — there the
# order is metadata Zarr.jl parses for the eltype and then ignores, which is the whole bug this
# function exists for. So v3 needs no branch here; adding one would double-swap.


"""
    read_ngff_axes(attrs_dir) -> Vector{String}

NGFF axis names from a group's `.zattrs` (`multiscales[0].axes[].name`), lowercased, C-order.
Metadata only (JSON), never pixels. Falls back to a sensible order by ndims when absent.
"""
function read_ngff_axes(attrs_dir::AbstractString)
    # `ngff_multiscales` (app/src/tasks/importImages/omezarr.jl) is the ONE Julia resolver for both
    # zarr formats: v2 keeps NGFF attrs top-level in `.zattrs`, v3/NGFF 0.5 nests them under
    # `attributes.ome` in `zarr.json`. Reading `.zattrs` directly made every v3 store answer EMPTY
    # here, and `axis_dims` then silently guessed the order by rank — right for a standard 5D stack,
    # wrong (with no error) for anything else. See docs/todo/ZARR_V3_PLAN.md.
    try
        ms = ngff_multiscales(attrs_dir)
        if !isnothing(ms)
            ax = get(first(ms), :axes, nothing)
            !isnothing(ax) && return [lowercase(string(get(a, :name, ""))) for a in ax]
        end
    catch
        # fall through to the by-rank default
    end
    String[]
end

"""
    axis_dims(c_axes, ndims) -> Dict{String,Int}

Julia array dimension (1-based) for each named axis. Zarr.jl is column-major and so presents the
array in REVERSED axis order: the C-order axis at position `i` sits at Julia dim `ndims - i + 1`.
Falls back to the conventional order for the rank when `.zattrs` carried no axes.
"""
function axis_dims(c_axes::Vector{String}, nd::Int)
    axes = isempty(c_axes) ?
        (nd == 5 ? ["t", "c", "z", "y", "x"] :
         nd == 4 ? ["t", "c", "y", "x"] :
         nd == 3 ? ["c", "y", "x"] : ["y", "x"]) : c_axes
    Dict(a => nd - i + 1 for (i, a) in enumerate(axes) if !isempty(a))
end

"""
    resolve_image_version(project_uid, image_uid, value_name) -> (zarr_path, meta_dir, error)

On-disk path of ONE image version. `value_name === nothing` resolves the ACTIVE version (what a task
would run against). Returns `(nothing, nothing, message)` on any failure — the caller shapes the
HTTP status, so this stays usable off the request path.
"""
function resolve_image_version(project_uid::AbstractString, image_uid::AbstractString, value_name)
    (isempty(project_uid) || isempty(image_uid)) &&
        return (nothing, nothing, "projectUid + imageUid required")
    proj_dir = joinpath(projects_dir(), project_uid)
    meta = state_file(proj_dir, image_uid)
    (isdir(proj_dir) && isfile(meta)) || return (nothing, nothing, "Image not found")
    raw = read_ccid_raw(meta)
    fn  = versioned_get_field(raw, "filepath", value_name)
    fn === nothing &&
        return (nothing, nothing, "No filepath registered — run a conversion task first")
    zp = joinpath(proj_dir, "0", image_uid, string(fn))
    isdir(zp) || return (nothing, nothing, "Zarr not found on disk")
    (zp, joinpath(proj_dir, "1", image_uid), nothing)
end

"""
    image_geometry(zarr_path) -> NamedTuple

`(sizeX, sizeY, sizeZ, sizeT)` of a version's level-0 array. Missing axes report 1, so a 2D still and
a 5D movie answer the same shape of question.
"""
function image_geometry(zarr_path::AbstractString)
    arr, caxes = open_level0(zarr_path)
    d = axis_dims(caxes, ndims(arr))
    dim(name) = haskey(d, name) ? size(arr, d[name]) : 1
    (sizeX = dim("x"), sizeY = dim("y"), sizeZ = dim("z"), sizeT = dim("t"))
end

"""
    store_compression(zarr_path) -> Union{NamedTuple,Nothing}

How a version's pixels are ENCODED on disk, from its level-0 `.zarray`. `nothing` when the store or
its `.zarray` is unreadable — a display-only answer must never throw at a caller listing versions.

Returns `(label, codec, level, shuffle)`. `label` is the name Settings → Storage uses for that codec
(`Cecelia.IMAGE_COMPRESSOR_CHOICES`) so the two surfaces agree; a store written before that choice
existed carries a codec that is not in the table and gets a plain descriptive label instead
(`"blosc/lz4-5"`), which is the honest answer — those stores predate the decision rather than
disagreeing with it.

Reads the JSON, deliberately: `open_level0` would instantiate a Zarr array to answer a question that
is one small file read. Layout is detected STRUCTURALLY, not by suffix (CLAUDE.md → OME-ZARR
dual-format): a flat store's level-0 array is `<store>/0`, a bioformats2raw series' is `<store>/0/0`,
and both have a `0/` child so the path tells you nothing.
"""
function store_compression(zarr_path::AbstractString)
    meta = nothing
    for candidate in (joinpath(zarr_path, "0"),        # flat: level 0 IS "0"
                      joinpath(zarr_path, "0", "0"))   # series: "0" is the group wrapper
        m = zarr_array_meta(candidate)          # discriminates array-vs-group itself, per format
        if !isnothing(m)
            meta = m; break
        end
    end
    isnothing(meta) && return nothing
    try
        fmt    = Int(get(meta, :zarr_format, 2))
        chunks = _chunk_shape(meta)
        if fmt >= 3
            # For a SHARDED array the two shapes mean different things and must not be swapped: the
            # outer `chunk_grid` is the SHARD (one file on disk), and the sharding codec's own
            # `chunk_shape` is the CHUNK (the unit of compression and of a random read). Unsharded, the
            # outer grid IS the chunk.
            comp, inner_chunk = _v3_codecs(meta)
            desc = _describe_compressor(comp)
            sharded = !isnothing(inner_chunk) && !isempty(inner_chunk)
            return (; desc..., zarrFormat = fmt,
                    chunks = sharded ? inner_chunk : chunks,
                    shard  = sharded ? chunks : nothing,
                    separator = _chunk_separator(meta, fmt))
        end
        desc = _describe_compressor(get(meta, :compressor, nothing))
        return (; desc..., zarrFormat = fmt, chunks = chunks, shard = nothing,
                separator = _chunk_separator(meta, fmt))
    catch
        return nothing
    end
end

# How chunk keys are spelled on disk: "/" nests them into a directory tree (`0/0/36/0/8/0/0`), "."
# keeps them flat (`56.2.15.1.1`). NOT cosmetic — it decides how many DIRECTORIES a store costs, which
# is most of its filesystem footprint and all of its cost on a network share. Measured on a real
# 1.7 GB import: nested 20 933 directories, flat 4. bioformats2raw nests by default in BOTH formats
# (`--no-nested` flips it); zarr-python defaults `.` for v2 and `/` for v3, which is why the stores WE
# write pin it (`_V3_FLAT_CHUNK_KEY`).
#
# The defaults differ per format, so an ABSENT key means different things: v2 defaults to ".", v3 to "/".
function _chunk_separator(meta, fmt)::String
    if fmt >= 3
        cke = get(meta, :chunk_key_encoding, nothing)
        isnothing(cke) && return "/"
        cfg = get(cke, :configuration, nothing)
        isnothing(cfg) && return "/"
        return string(get(cfg, :separator, "/"))
    end
    string(get(meta, :dimension_separator, "."))
end

# Chunk shape as a plain Vector{Int}. v2 spells it `chunks`; v3 `chunk_grid.configuration.chunk_shape`.
# For a SHARDED v3 array that is the SHARD shape — the inner chunk shape comes from the sharding
# codec's own `chunk_shape` (see `_v3_codecs`), which is the number a user comparing chunk sizes
# across formats actually wants.
function _chunk_shape(meta)
    haskey(meta, :chunks) && return collect(Int, meta[:chunks])
    grid = get(meta, :chunk_grid, nothing)
    isnothing(grid) && return Int[]
    cfg = get(grid, :configuration, nothing)
    isnothing(cfg) && return Int[]
    collect(Int, get(cfg, :chunk_shape, Int[]))
end

# Flatten a v3 codec pipeline to `(compressor_like, inner_chunk_shape)`, where `compressor_like` is
# shaped like a v2 `compressor` object so ONE describer serves both formats. `sharding_indexed` wraps
# the real codecs, so the compression lives one level in, and its `chunk_shape` is the INNER chunk
# (the caller pairs that with the outer grid, which is the shard). `nothing` inner ⇒ not sharded.
function _v3_codecs(meta)
    codecs = get(meta, :codecs, nothing)
    isnothing(codecs) && return (nothing, nothing)
    shard = nothing
    for c in codecs
        name = string(get(c, :name, ""))
        cfg  = get(c, :configuration, nothing)
        if name == "sharding_indexed" && !isnothing(cfg)
            shard  = collect(Int, get(cfg, :chunk_shape, Int[]))   # the INNER chunk shape
            codecs = get(cfg, :codecs, [])                          # descend into the wrapped codecs
            break
        end
    end
    for c in codecs
        name = string(get(c, :name, ""))
        name in ("bytes", "crc32c", "transpose") && continue        # not compression
        cfg = get(c, :configuration, nothing)
        # present it in the v2 shape (`id`, plus the codec's own keys) for `_describe_compressor`
        merged = Dict{Symbol,Any}(:id => name)
        isnothing(cfg) || for (k, v) in pairs(cfg); merged[Symbol(k)] = v; end
        return (merged, shard)
    end
    (nothing, shard)
end

# numcodecs config → what to show. Two shapes reach here: blosc (`cname`/`clevel`/`shuffle`) and a
# bare codec like zstd (`level`). `compressor: null` is a real, valid value — an uncompressed store.
# blosc's `shuffle` is an INT in zarr v2 metadata (0/1/2) and a NAME in v3
# ("noshuffle"/"shuffle"/"byteshuffle"/"bitshuffle"). One normaliser, because `_describe_compressor`
# serves both formats — `Int("shuffle")` would have thrown, and the caller's catch-all would have
# reported the whole store as having no compression.
function _shuffle_on(v)::Bool
    v isa AbstractString && return !(lowercase(String(v)) in ("noshuffle", "none", "0"))
    v isa Real && return Int(v) != 0
    false
end

function _describe_compressor(comp)
    isnothing(comp) && return (label = "none", codec = "none", level = 0, shuffle = false)
    comp isa AbstractDict || return nothing
    id      = string(get(comp, :id, "?"))
    cname   = haskey(comp, :cname) ? string(comp[:cname]) : id
    level   = Int(get(comp, :clevel, get(comp, :level, 0)))
    shuffle = _shuffle_on(get(comp, :shuffle, 0))
    # zstd treats level 0 as "the library default", which is 3 — so a store written at 0 must not read
    # as a different setting from one written at 3. Same normalisation the rechunk sweep uses.
    (cname == "zstd" && level == 0) && (level = 3)
    # Match the WRAPPER too, not just (cname, level, shuffle). `store_compressor` writes a bare
    # numcodecs Zstd for the unshuffled-zstd choice and a Blosc for everything else, so a
    # blosc-WRAPPED unshuffled zstd is a different encoding on disk from the `zstd` choice even though
    # those three fields agree — labelling it as that choice would misreport what is in the file.
    wrapper(c) = (!c.shuffle && c.cname == "zstd") ? "zstd" : "blosc"
    known = findfirst(c -> c.cname == cname && c.clevel == level && c.shuffle == shuffle &&
                           wrapper(c) == id,
                      Cecelia.IMAGE_COMPRESSOR_CHOICES)
    label = isnothing(known) ?
        "$(id == "blosc" ? "blosc/" : "")$cname-$level$(shuffle ? " + shuffle" : "")" :
        Cecelia.IMAGE_COMPRESSOR_CHOICES[known].label
    (label = label, codec = cname, level = level, shuffle = shuffle)
end

# GET /api/images/stores?projectUid=&imageUid=
#   → {versions: {valueName: {bytes, label?, codec?, level?, shuffle?} | null},
#      labels:   {valueName: {bytes}}}
#
# What each of this image's stored things IS on disk: how its pixels are encoded, and how much space
# it takes. EVERY version at once, because the only consumer (the image-metadata modal) lists them
# all — one call rather than one per version. A version with no registered filepath is reported as
# `null` rather than omitted, so the modal can say "—" against it instead of silently dropping a row;
# a version whose store is missing or unreadable keeps its entry but omits the codec fields (and
# reports 0 bytes), which is the same "—" in the modal without losing the size of the ones that read.
#
# `bytes` is the expensive half — a directory walk per store (`_path_bytes`). MEASURED on a real image
# (3 versions of ~4 GB / 10k chunks each + 3 label sets): 0.24 s for the whole call warm, and ~2 s per
# store on a cold cache. That is affordable HERE and only here: the modal is opened deliberately, one
# image at a time, and fills this section in after it is already on screen.
# Do NOT fold it into `/api/images` (the listing) — that would walk every store in the project on
# every project open. Settings → Storage does the project-wide walk, on demand, for that reason.
function api_image_stores(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    project_uid = get(q, "projectUid", "")
    image_uid   = get(q, "imageUid", "")
    (isempty(project_uid) || isempty(image_uid)) &&
        return 400, JSON3.write((; error = "projectUid + imageUid required"))
    proj_dir = joinpath(projects_dir(), project_uid)
    meta     = state_file(proj_dir, image_uid)
    (isdir(proj_dir) && isfile(meta)) || return 404, JSON3.write((; error = "Image not found"))

    raw = read_ccid_raw(meta)
    out = Dict{String,Any}()
    fp  = get(raw, "filepath", nothing)
    if fp isa AbstractDict
        for vn in versioned_keys(fp)
            fn = versioned_get_field(raw, "filepath", vn)
            isnothing(fn) && (out[vn] = nothing; continue)
            zp    = joinpath(proj_dir, "0", image_uid, string(fn))
            entry = Dict{String,Any}("bytes" => Cecelia._path_bytes(zp))
            c     = isdir(zp) ? store_compression(zp) : nothing
            if !isnothing(c)
                entry["label"]   = c.label;  entry["codec"]   = c.codec
                entry["level"]   = c.level;  entry["shuffle"] = c.shuffle
                # What FORMAT this store is, alongside how it is compressed. Both zarr v2 and v3 will
                # coexist on disk indefinitely (no converter — ZARR_V3_PLAN D7), so "which is this?"
                # has to be answerable from the UI rather than by reading zarr.json in a terminal.
                # `shard` is null for an unsharded store, which is every v2 one.
                entry["zarrFormat"] = c.zarrFormat
                entry["chunks"]     = c.chunks
                entry["shard"]      = c.shard
                entry["separator"]  = c.separator
            end
            isdir(zp) && (entry["ngffVersion"] = ngff_version(zp))
            out[vn] = entry
        end
    end

    # Label stores live in the metadata dir (`1/<uid>/labels/`) and one value_name can register
    # several files (base + nuc), so a row's size is their sum. No codec: these are written by the
    # segmentation writer with the `labels` compressor, which the store itself already reports and
    # nothing in the modal asks for.
    lbl  = get(raw, "labels", nothing)
    lout = Dict{String,Any}()
    if lbl isa AbstractDict
        for (k, v) in lbl
            isnothing(v) && continue
            fns = v isa AbstractVector ? v : [v]
            lout[String(k)] = Dict{String,Any}("bytes" => sum(
                Cecelia._path_bytes(joinpath(proj_dir, "1", image_uid, "labels", string(fn)))
                for fn in fns; init = 0))
        end
    end
    200, JSON3.write((; versions = out, labels = lout))
end

# GET /api/images/geometry?projectUid=&imageUid=&valueName= → {sizeX,sizeY,sizeZ,sizeT,valueName}
#
# The frame extent of ONE image version. `valueName` omitted ⇒ the ACTIVE version, which is what a
# task will run against — the reason this exists rather than a stored per-image size (see the note at
# the top of this file).
function api_image_geometry(req::HTTP.Request)
    q  = HTTP.queryparams(HTTP.URI(req.target))
    vn = get(q, "valueName", "")
    zp, _, err = resolve_image_version(get(q, "projectUid", ""), get(q, "imageUid", ""),
                                       isempty(vn) ? nothing : vn)
    err === nothing || return 404, JSON3.write((; error = err))
    try
        g = image_geometry(zp)
        200, JSON3.write((; g.sizeX, g.sizeY, g.sizeZ, g.sizeT, valueName = vn))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end
