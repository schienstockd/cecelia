# ── Movie / legend / view helpers (formerly napari_api.jl) ─────────────────────
#
# The shared, view-agnostic helpers that survived the napari retirement (P9). They compute movie
# output paths + suffixes, resolve label/branch value names for a movie config, build the compare
# grid (rows/columns) that the offline renderer walks, resolve overlay pop colours, and render the
# tidy legend content the /api/viewer/overlay-legend route serves.
#
# Consumers: movie_rail.jl (offline renderer), viewer_api.jl (props / pick-rect / overlay-legend),
# sockets.jl (movie:record and compare-grid dispatch), movies_api.jl (file naming), gating_api.jl
# (colour overrides), tracking_api.jl (track-scheme colour resolution), api/test/runtests.jl.


# coerce a JSON value (Int/Float/String/null) to a non-negative Int; blank/garbage → 0.
# Used for the z-window dial, which can arrive as null (empty input) or a float.
function _to_int(x)::Int
    x === nothing && return 0
    x isa Integer && return max(0, Int(x))
    x isa Real    && return max(0, round(Int, x))
    x isa AbstractString && return (n = tryparse(Int, x); n === nothing ? 0 : max(0, n))
    0
end

# Layer props are stored as JSON (the single canonical format — see docs/todo/CROP_PANEL_PLAN.md Phase 0).
# The bridge reads/writes this; the in-app crop render (Julia) reads it too. A legacy `.pkl` from before
# the switch is migrated to `.json` by the bridge on first load (see `_legacy_props_path`).
function _props_path(task_dir::String, zarr_path::String)::String
    joinpath(task_dir, "data", basename(zarr_path) * ".json")
end

# ── Label helpers ────────────────────────────────────────────────────────────

# Parse allLabels dict from a request body: {valueName → [file, ...]}
function _parse_all_labels(data)::Dict{String,Vector{String}}
    raw = get(data, :allLabels, nothing)
    raw isa AbstractDict || return Dict{String,Vector{String}}()
    Dict{String,Vector{String}}(
        String(k) => (v isa AbstractVector ? collect(String, v) : String[string(v)])
        for (k, v) in raw
    )
end

# Skeleton-labels equivalent (branchLabels/ store, `({vn}) Branches` layer). Parallel to
# _parse_all_labels; kept separate so the branch payload never mixes into the generic labels
# picker (BRANCHING_PLAN Decision 6).
function _parse_all_branch_labels(data)::Dict{String,Vector{String}}
    raw = get(data, :allBranchLabels, nothing)
    raw isa AbstractDict || return Dict{String,Vector{String}}()
    Dict{String,Vector{String}}(
        String(k) => (v isa AbstractVector ? collect(String, v) : String[string(v)])
        for (k, v) in raw
    )
end

# Requested movie output size from a request body: `(size_x, size_y)`, each `nothing` when absent, blank
# or non-positive — which means "record at the napari canvas size" (the default, and what every movie was
# before the size fields existed). ONE reader for all three surfaces (single record, animation, batch) so
# "blank = canvas" is defined once; the pixel-level validation (clamp, even axes) lives in Python's
# `movie_io.coerce_movie_size`.
function _movie_size_params(data)
    read_axis(key) = begin
        raw = get(data, key, nothing)
        v = raw === nothing ? nothing : tryparse(Int, string(raw))
        (v === nothing || v <= 0) ? nothing : v
    end
    read_axis(:sizeX), read_axis(:sizeY)
end

# {proj}/movies/ for an image (img._dir = {proj}/1/{uid}); created if missing. One place the movies
# dir is derived — the single-image recorders and the batch path all go through here.
function _movies_dir(img)::String
    d = joinpath(dirname(dirname(img._dir)), "movies")
    mkpath(d)
    d
end

# The filename rule now lives in the package (`Cecelia.safe_name_part`) so tasks that name their
# own output files — the OME-TIFF export — and the movie recorders cannot disagree about what a
# safe name is. Kept as a local alias so the movie call sites read unchanged.
_safe_name_part(raw)::String = safe_name_part(raw)

# Movie output path named by the IMAGE (not attrs) — used by the single-image recorders. Sanitises
# img.name, falls back to the uid when blank/unsafe. `suffix` distinguishes timelapse ("") from
# animation ("_animation").
function _movie_named_path(img, uid::AbstractString; suffix::AbstractString = "")::String
    safe = _safe_name_part(img.name)
    joinpath(_movies_dir(img), (isempty(safe) ? String(uid) : safe) * suffix * ".mp4")
end

# A user-supplied filename addition → a safe `_suffix` fragment, or "" for none. Same character rule as
# the image name above, so one movie name can't be sanitised two ways.
#
# It exists because a movie is named after the IMAGE: record the AF-corrected version and then the raw
# import and the second overwrites the first, with nothing in the name to say which is which. The
# frontend prefills it with the open image VERSION (the usual reason two movies of one image differ),
# but it is free text — the comparison someone wants to label is not always a version.
const MOVIE_SUFFIX_MAX = 40
function _movie_suffix(raw)::String
    safe = _safe_name_part(raw)                         # shared rule: no leading/trailing separators
    isempty(safe) && return ""
    "_" * first(safe, MOVIE_SUFFIX_MAX)
end

# Sentinel token in `file_attrs` meaning "the shown channel names joined by '-'" — mirrors the
# frontend MOVIE_CHANNELS_TOKEN (utils/batchMovie.ts); keep the two in sync.
const MOVIE_CHANNELS_TOKEN = "__channels__"

# Attr-named output filename: <attr1>_<attr2>_..._<uid|image name>[_suffix].mp4 (mirrors the R
# `paste(fileAttrs...) _ uid`).
# `file_attrs` is the ordered list of attribute keys and/or the channels token; `channel_names` are the
# channels shown in the movie (used only where the token appears, joined by '-'). Blank/missing attrs
# are dropped. Pure (attr dict + uid + channels) → testable.
#
# WHAT TERMINATES THE NAME is a choice, because the two recorders had made it differently and neither
# was wrong (Dominik, 2026-08-10): a single viewer recording is named after the IMAGE
# (`_movie_named_path`), a batch after the uid — so regenerating a viewer movie from its restored config
# produced a differently-named file beside the original. Pass `name` (the image's) to terminate with it
# instead; blank keeps the uid.
#
# The uid is the SAFE default and stays the default: it is unique by construction, while two images in a
# set may well share a name and would then write over each other — the same collision the single-image
# recorder has always had, which is what its `suffix` exists to break.
function _movie_basename(attr::AbstractDict, uid::AbstractString, file_attrs::Vector{String},
                         channel_names::Vector{String} = String[];
                         suffix::AbstractString = "", name::AbstractString = "")::String
    parts = String[]
    for a in file_attrs
        if a == MOVIE_CHANNELS_TOKEN
            chans = join(filter(!isempty, strip.(channel_names)), "-")
            isempty(chans) || push!(parts, chans)
        else
            val = strip(String(get(attr, a, "")))
            isempty(val) || push!(parts, val)
        end
    end
    # an image with an unnameable name (punctuation only) still has to produce a file, so fall back
    tail = _safe_name_part(name)
    push!(parts, isempty(tail) ? String(uid) : tail)
    # the user's filename addition goes BEFORE the extension (it arrives already sanitised + `_`-led
    # from `_movie_suffix`), so the file is still an .mp4 to every listing that filters on the suffix
    _safe_name_part(join(parts, "_")) * suffix * ".mp4"
end

# Channels shown in the movie for `img` = the `config.channels` keys (the ones given a colormap),
# ordered by the image's channel list so the filename is stable. `config` may be missing/empty.
function _shown_channel_names(img, config, vn)::Vector{String}
    chans = get(config, :channels, nothing)
    (chans isa AbstractDict && !isempty(chans)) || return String[]
    wanted = Set(String(k) for k in keys(chans))
    ch_all = channel_names(img; value_name = vn)
    ch_all === nothing ? collect(wanted) : String[c for c in ch_all if c in wanted]
end

# ── Label layers in a movie ───────────────────────────────────────────────────
# TWO registries, one contract. Cell segmentation masks live in `img.labels` and render as
# `({vn}) Labels`; skeletons from `segment.branching` live in `img.branch_labels` and render as
# `({vn}) Branches` — deliberately separate stores and a separate picker (BRANCHING_PLAN Decision 6).
# They are NOT unified here either; what is shared is the machinery, because they had the identical
# bug for the identical reason: `open_image` clears the canvas and the movie path never asked for
# them back.
#
# What a movie config asks for is THREE-valued, for both:
#
#   * `nothing` — the config says nothing (it predates the setting, or the caller records the live
#     view). Leave the canvas alone.
#   * `String[]` — an explicit "none". Not the same as `nothing`: a user who cleared the picker must
#     get a movie without them, even though the viewer still shows them.
#   * a list — show exactly these, hide every other registered set.
#
# Unregistered names are dropped rather than passed on to the bridge, which would log a skip per
# frameless store; the frontend's `normaliseItems` drops them too, so both ends agree on a deleted set.

# The registries, read defensively: `img.labels`/`img.branch_labels` exist on a CciaImage, but these
# helpers are also exercised against light NamedTuple stand-ins in the tests.
_label_registry(img)::Dict{String,Vector{String}} =
    hasproperty(img, :labels) && img.labels isa AbstractDict ?
        Dict{String,Vector{String}}(String(k) => collect(String, v) for (k, v) in img.labels) :
        Dict{String,Vector{String}}()
_branch_registry(img)::Dict{String,Vector{String}} =
    hasproperty(img, :branch_labels) && img.branch_labels isa AbstractDict ?
        Dict{String,Vector{String}}(String(k) => collect(String, v) for (k, v) in img.branch_labels) :
        Dict{String,Vector{String}}()

# One reader for both keys — `:labelValueNames` against the mask registry, `:branchValueNames`
# against the skeleton one.
function _config_set_names(config, key::Symbol, known::AbstractDict)::Union{Nothing,Vector{String}}
    raw = get(config, key, nothing)
    raw === nothing && return nothing
    seen = Set{String}()
    String[v for v in (String(x) for x in raw)
           if haskey(known, v) && !(v in seen) && (push!(seen, v); true)]
end

_config_label_value_names(config, img)  = _config_set_names(config, :labelValueNames,  _label_registry(img))
_config_branch_value_names(config, img) = _config_set_names(config, :branchValueNames, _branch_registry(img))

# Label OUTLINE width in pixels: 0 = filled (napari's default), N = an N-px contour, which is what lets
# the channel signal under a mask stay readable. Read from a request body or a movie config through the
# one accessor so the routes and the recorder cannot disagree about the key or its floor. Clamped
# rather than validated — a negative contour is meaningless to napari, and failing a whole batch over a
# display nicety would be the wrong trade.
const LABEL_CONTOUR_MAX = 10
_label_contour(src)::Int = clamp(_to_int(get(src, :labelContour, 0)), 0, LABEL_CONTOUR_MAX)

# How much of the z stack a movie shows: the whole thing as a 3D render, or one slice in 2D.
# `show3D` wins — a z index alongside it is a leftover from the last time 2D was chosen, and dropping
# it silently would be worse than ignoring it. `nothing` for the slice means "whatever is showing",
# which is what every recording did before the setting existed.
_show_3d(src)::Bool = Bool(get(src, :show3D, false))
# 3D detail level from an authored config: a multiscale LEVEL index (0 = full resolution, higher =
# coarser), or `nothing` for napari's own choice. Absent means 0, not "auto" — a config written before
# the control existed still wants visible masks.
function _detail_3d(src)::Union{Int,Nothing}
    raw = get(src, :detail3d, 0)
    raw === nothing ? nothing : max(0, _to_int(raw))
end

function _z_slice(src)::Union{Int,Nothing}
    _show_3d(src) && return nothing
    raw = get(src, :zSlice, nothing)
    raw === nothing ? nothing : max(0, _to_int(raw))
end

# Which stretch of the timelapse a movie sweeps, as FRAME INDICES — `(t_start, t_end)`, with `nothing`
# for the end meaning "the last frame". Absent is the whole thing, which is what every recording did
# before the control existed.
#
# Indices rather than a percentage (the 3D crop's choice) because that is the recorders' own contract
# all the way down — `_t_sweep_frames` here, `record_timelapse` in the bridge, `napari_utils` in Python
# — and every one of them CLAMPS to the image's own length. So one range applied across a batch of
# unequal timelapses records to the end of each rather than failing on the short ones.
#
# One reader for both entry points: the viewer's recorder puts these on the request, the batch page
# puts them in its authored config, and they mean the same thing in both.
function _t_range(src)::Tuple{Int,Union{Int,Nothing}}
    t0  = max(0, _to_int(get(src, :tStart, 0)))
    raw = get(src, :tEnd, nothing)
    t1  = raw === nothing ? nothing : max(t0, _to_int(raw))
    (t0, t1)
end

# {valueName => [store files]} for `vns`, the shape `_parse_all_labels`/`_show_all_labels!` consume.
# `nothing` (an agnostic config) and an empty list both give an empty map — the caller pairs it with a
# `showLabels` flag, which is what distinguishes them on the wire.
_files_for(known::AbstractDict, vns::Union{Nothing,AbstractVector})::Dict{String,Vector{String}} =
    vns === nothing ? Dict{String,Vector{String}}() :
        Dict{String,Vector{String}}(String(v) => known[String(v)] for v in vns
                                    if haskey(known, String(v)))

_label_files_for(img, vns)  = _files_for(_label_registry(img), vns)
_branch_files_for(img, vns) = _files_for(_branch_registry(img), vns)

# Full attr-named output path under {proj}/movies/.
function _movie_out_path(img, file_attrs::Vector{String}, channel_names::Vector{String} = String[];
                         suffix::AbstractString = "", by_image::Bool = false)::String
    joinpath(_movies_dir(img),
             _movie_basename(img.attr, img.uid, file_attrs, channel_names;
                             suffix = suffix, name = by_image ? String(img.name) : ""))
end

# Append every non-transient, shown pop of `(vn, pt)` to `out` as {valueName, popType, path} — the shape
# overlay_legend_content consumes. Uses the SAME pop-map primitives the show-tracks handler does, so the
# card can't drift from what's actually rendered.
function _append_config_pop_paths!(out::Vector{Dict{String,Any}}, img, vn::AbstractString, pt::AbstractString)
    try
        m = _live_map(img, vn, pt)
        for path in pop_paths(m)
            p = pop_at(m, path)
            (p.transient || !p.show) && continue
            push!(out, Dict{String,Any}("valueName" => vn, "popType" => pt, "path" => path))
        end
    catch
        # pop map for this (vn, pt) unavailable → nothing to contribute
    end
end

# The overlay pops a movie config would SHOW, as one list of {valueName, popType, path} (the shape
# overlay_legend_content turns into {name, colour}) — point pops AND tracks together, matching the ONE
# "Populations" section the analysis-board strip / single-record card use. Reuses the CANONICAL
# resolvers the show-handlers use — `resolve_pops` for point pops, the pop maps for track gates &
# clusters, "/_tracked" for a segmentation's whole-track overlay — so it can't drift from what renders.
function _config_overlay_pops(img, config)
    out = Vector{Dict{String,Any}}()
    _has_label_props(img) || return out
    segs = String[v for v in versioned_keys(img.label_props) if !is_reserved_value_name(v)]

    if Bool(get(config, :showPopulations, false))
        pt = String(get(config, :popType, "flow"))
        for vn in segs
            try
                for L in resolve_pops(img, pt; value_name = vn)
                    (L.show && !L.is_track) || continue
                    push!(out, Dict{String,Any}("valueName" => vn, "popType" => pt, "path" => L.path))
                end
            catch
            end
        end
    end

    if Bool(get(config, :showTracks, false))
        for vn in collect(String, get(config, :trackValueNames, String[]))   # whole-seg → generic "tracks" row
            haskey(img.label_props, vn) && push!(out, Dict{String,Any}("valueName" => vn, "popType" => "track", "path" => "/_tracked"))
        end
        for vn in segs
            Bool(get(config, :showGatedTracks, false)) && _append_config_pop_paths!(out, img, vn, "track")
            Bool(get(config, :showTrackclust, false))  && _append_config_pop_paths!(out, img, vn, "trackclust")
        end
    end
    out
end

# Assemble the Julia-side title-card content for an image under a movie config (Phase H). Title = image
# name + its attribute values ("MERTK — mouse 1 — location B"); the Populations / Tracks / Colour-by
# sections all come from the CANONICAL `overlay_legend_content` helper. CHANNELS are NOT added here — the
# recorder adds them from the live viewer (the only place channel colour lives). Returns `nothing` when
# the card is disabled/absent so the recorder skips it.
function _title_card_content(img, config)
    tc = get(config, :titleCard, nothing)
    (tc === nothing || !(tc isa AbstractDict) || !Bool(get(tc, :enabled, false))) && return nothing

    attrs = String[]
    if img.attr isa AbstractDict
        for k in sort(collect(keys(img.attr)))
            v = strip(String(img.attr[k])); isempty(v) || push!(attrs, v)
        end
    end
    title = join(vcat([img.name], attrs), " — ")

    sections = Vector{Dict{String,Any}}()
    # Populations (incl. track pops) — the shown pops through the canonical legend helper → {name, colour}
    # rows. ONE section, matching the strip / single-record card (overlay_legend_content dedups by name).
    plist = _config_overlay_pops(img, config)
    if !isempty(plist)
        rows  = overlay_legend_content(img, "", plist, nothing).populations
        items = [Dict{String,Any}("label" => p["name"], "colour" => p["colour"]) for p in rows]
        isempty(items) || push!(sections, Dict{String,Any}("heading" => "Populations", "items" => items))
    end
    # Colour-by — value → pop colour + name for the colour-by measure.
    column = String(get(config, :colourBy, ""))
    if !isempty(column)
        legend = overlay_legend_content(img, column, nothing, get(config, :colourOverrides, nothing))
        items  = [Dict{String,Any}("label" => it["label"], "colour" => it["colour"]) for it in legend.colourBy["items"]]
        isempty(items) || push!(sections, Dict{String,Any}("heading" => "Colour by", "items" => items))
    end

    Dict{String,Any}(
        "enabled"     => true,
        "durationSec" => Float64(get(tc, :durationSec, 3.0)),
        "title"       => title,
        "note"        => String(get(tc, :note, "")),
        "sections"    => sections,   # the recorder prepends a "Channels" section from the live viewer
    )
end

# ── Side-by-side comparison ───────────────────────────────────────────────────
# docs/todo/MOVIE_COMPARE_PLAN.md. A comparison is N recordings plus one compose, NOT one clever
# render: each column goes through the SAME path a single movie uses, so overlays, staging, cancel and
# the size policy keep working untouched, and the finished files are composed frame-by-frame in the
# bridge (D1; D2 records why the alternative — several versions as layers of one canvas — was rejected).
#
# What differs between cells is TWO dimensions, and a movie is a GRID of them:
#
#   * image VERSIONS run across the columns (raw next to AF-corrected),
#   * segmentation MASKS run down the rows (model A above model B).
#
# Pick two of one and one of the other and the grid degenerates to a single row — a plain side-by-side
# comparison, whichever list it came from. Pick two of BOTH and you get the cross-product, which is the
# only layout that answers "does this correction change what each model segments?" in one file.
#
# Everything downstream of the grid is blind to what made two cells differ, which is what D9 bought:
# the pass loop, the per-frame progress arithmetic, cancel, staging, the caption band and the compose
# all key off `Vector{MovieRow}` and never ask. Adding a third dimension would be a builder here plus a
# picker in the UI — though note the cost is multiplicative, not additive (see `_grid_frame_total`).

# One column: what its caption says, and the movie config that produces it. Config keys are Symbols
# because that is how every reader here (`_apply_movie_config!`, `_title_card_content`,
# `_shown_channel_names`) addresses them.
const MovieColumn = NamedTuple{(:label, :config),Tuple{String,Dict{Symbol,Any}}}
# One row of the grid: the cells across it, and what the caption under the whole strip says. A
# single-row grid captions nothing — there is no outer compose to hang a label on.
const MovieRow = NamedTuple{(:label, :columns),Tuple{String,Vector{MovieColumn}}}

# The config's whole base, as the Symbol-keyed Dict a column carries. Split out so both builders pin
# their own key onto the SAME starting point — a column differs from its config by one entry, never by
# which keys survived being copied.
_column_base(config)::Dict{Symbol,Any} = Dict{Symbol,Any}(Symbol(k) => v for (k, v) in pairs(config))

# The columns of a VERSION comparison: the authored config once per selected version, in the order the
# user put the chips in. `""` means the active version — what the config already meant on its own.
function _version_columns(config, value_names::AbstractVector)::Vector{MovieColumn}
    base = _column_base(config)
    MovieColumn[(; label  = (n = strip(String(vn)); isempty(n) ? "active" : n),
                   config = merge(base, Dict{Symbol,Any}(:valueName => strip(String(vn)))))
                for vn in value_names]
end

# The columns of a SEGMENTATION comparison: one label set per column, every column on the SAME image
# version. Pinning the version is what keeps the two dimensions independent — a mask row varies only
# the mask, so the row reads as one model's answer to one image.
#
# Cheap by construction: with the version fixed, no column after the first re-opens the image
# (`_version_is_open`), so the passes differ only by which mask is on screen — no re-sampled contrast,
# no reloaded pyramid.
function _segmentation_columns(config, label_value_names::AbstractVector,
                               value_name::AbstractString)::Vector{MovieColumn}
    base = _column_base(config)
    MovieColumn[(; label  = (n = strip(String(sn)); isempty(n) ? "none" : n),
                   config = merge(base, Dict{Symbol,Any}(
                       :valueName       => value_name,
                       :labelValueNames => String[strip(String(sn))])))
                for sn in label_value_names]
end

# The ONE grid builder every recorder calls. Versions across, masks down:
#
#   * 2+ of BOTH        → the cross-product. One row per mask; that row's cells are the versions.
#   * 2+ of one only    → a single row of that list, side by side — the plain comparison.
#   * neither           → one cell, which is an ordinary single recording.
#
# The degenerate cases are not special-cased anywhere below: a plain movie is a 1x1 grid, and a
# comparison is a 1xN one, so `_record_grid!` has exactly one shape to reason about.
function _compare_grid(config)::Vector{MovieRow}
    versions = _config_value_names(config)              # never empty ("" = the active version)
    masks    = _config_compare_segmentations(config)
    if length(versions) > 1 && length(masks) > 1
        # A cell draws ONE mask on ONE version; the row is captioned with the mask, so the caption
        # under each cell can stay the version and read as a column header repeated per row.
        return MovieRow[(; label   = m,
                           columns = _version_columns(
                               merge(_column_base(config), Dict{Symbol,Any}(:labelValueNames => String[m])),
                               versions))
                        for m in masks]
    end
    length(masks) > 1 &&
        return MovieRow[(; label = "", columns = _segmentation_columns(config, masks, first(versions)))]
    MovieRow[(; label = "", columns = _version_columns(config, versions))]
end

# Fold ONE row of N cells into the squarest rectangle that holds them — the `grid` layout (4 → 2x2,
# 6 → 3x2, 5 → 3 then 2). Four movies side by side are four times as wide as they are tall, which is a
# strip nobody can read on a slide; wrapping them is a purely cosmetic rearrangement of cells that were
# going to be recorded anyway.
#
# Deliberately NOT a new compositor: it hands `_record_grid!` the same `Vector{MovieRow}` the
# cross-product builds, so the nested compose, the per-cell captions, the frame arithmetic, cancel and
# staging are the machinery that already exists. A short last row is centred on black by
# `movie_io._pad_to`, which is why a non-square count needs no special case.
#
# Only ever applied to a SINGLE row: with both lists comparing something, versions-across/masks-down
# already fixes both directions and there is nothing to rearrange. Two cells wrap to one row of two —
# which is the row layout — so small counts need no guard either.
# Mirrors `wrapShape` (frontend/src/utils/movieCompare.ts).
function _wrap_grid(rows::Vector{MovieRow}, layout::AbstractString)::Vector{MovieRow}
    (layout != "grid" || length(rows) != 1) && return rows
    cols = rows[1].columns
    n    = length(cols)
    n <= 1 && return rows
    ncol = ceil(Int, sqrt(n))
    # No row label: the cells carry their own captions and a wrapped row means nothing on its own —
    # captioning it would put a blank band under every strip.
    MovieRow[(; label = "", columns = cols[i:min(i + ncol - 1, n)]) for i in 1:ncol:n]
end

# The versions a config records, in column order. `valueNames` is the comparison list; a config from
# before it existed (or one the user never touched) carries a single `valueName`, and "" — the active
# version — is a perfectly good single column. Never empty, so callers always have one column.
function _config_value_names(config)::Vector{String}
    raw = get(config, :valueNames, nothing)
    names = raw === nothing ? String[] : [String(v) for v in raw]
    isempty(names) ? [String(get(config, :valueName, ""))] : names
end

# The segmentations a config splits into columns. Deliberately NOT `_config_label_value_names`: that
# one is image-scoped (it drops names this image doesn't have) and three-valued, while the column list
# is authored once for a whole batch and must not vary per image — an image missing one of the sets
# would otherwise get a different number of columns than its neighbours.
function _config_compare_segmentations(config)::Vector{String}
    raw = get(config, :labelValueNames, nothing)
    raw === nothing && return String[]
    seen = Set{String}()
    String[v for v in (strip(String(x)) for x in raw)
           if !isempty(v) && !(v in seen) && (push!(seen, v); true)]
end

# D4: how the columns are contrasted. "reference" (the default) applies column 1's intensity mapping to
# the others, so a correction is judged on one ruler; "version" leaves each column with the saved napari
# settings of its own version. Anything else reads as the default rather than failing a whole batch.
_share_contrast(mode)::Bool = String(mode) != "version"

# Frames one T-sweep writes for `img` over `[t_start, t_end]` — 0 when the image has no usable T axis.
# Mirrors the bridge's own range arithmetic (`napari_utils.record_timelapse`): one frame per timepoint,
# both ends inclusive.
function _t_sweep_frames(img, t_start::Int, t_end)::Int
    n = _to_int(get(img.meta, "SizeT", nothing))
    n <= 1 && return 0
    t0 = max(0, t_start)
    t1 = t_end === nothing ? n - 1 : min(_to_int(t_end), n - 1)
    t1 <= t0 ? 0 : (t1 - t0 + 1)
end

# Frames a whole grid renders, so the passes and the composes drive ONE progress bar instead of
# restarting it per cell: a pass per CELL, plus a compose per row (only when a row has something to
# compose) and one more to stack the rows. An estimate made before anything runs — the bridge clamps it
# if a pass comes out longer.
#
# Note what this makes visible: a grid is rows x cols RENDERS. 2 versions x 2 masks is four full
# recordings, not two — which is why the UI states the pass count on the button before you commit.
function _grid_frame_total(n_rows::Int, n_cols::Int, per_pass::Int)::Int
    cells = n_rows * n_cols
    (cells <= 1 || per_pass <= 0) && return 0
    composes = (n_cols > 1 ? n_rows : 0) + (n_rows > 1 ? 1 : 0)
    (cells + composes) * per_pass
end
# …and the same count read off the ACTUAL grid, which is what `_record_grid!` uses. It mirrors the loop
# unit for unit — a pass per cell, a compose per row that has something to compose (a one-cell row IS
# its own strip), and a stack when there is more than one row. The rectangular form above assumes every
# row is the same width; this one cannot be wrong about a grid it was handed, and the two are asserted
# to agree on rectangular input.
function _grid_frame_total(rows::Vector{MovieRow}, per_pass::Int)::Int
    cells = sum(length(r.columns) for r in rows; init = 0)
    (cells <= 1 || per_pass <= 0) && return 0
    (cells + count(r -> length(r.columns) > 1, rows) + (length(rows) > 1 ? 1 : 0)) * per_pass
end
# The single-row case by its old name — a 1xN grid, which is what every comparison was until masks
# became a second dimension.
_comparison_frame_total(n_columns::Int, per_pass::Int)::Int = _grid_frame_total(1, n_columns, per_pass)

# A captured view WITHOUT its per-layer props — the camera and the timepoint, nothing about intensity.
# What "each version keeps its own saved napari settings" (D4) applies to the later columns.
_camera_only(snapshot) =
    Dict{String,Any}(String(k) => v for (k, v) in pairs(snapshot) if String(k) != "layers")

