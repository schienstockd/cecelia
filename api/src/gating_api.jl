# ── Gating API ────────────────────────────────────────────────────────────────
#
# HTTP routes for the population manager + gating engine (docs/POPULATION.md, docs/API.md).
# Synchronous, in-process — interactive latency, no task pool. Mutations persist the
# per-segmentation sidecar gating/{value_name}.json and broadcast `gating:pop-tree`.
# Plot data is served as raw Float32 (already transformed); membership as label IDs.

# ── helpers ──────────────────────────────────────────────────────────────────

_gerr(status, msg) = (status, JSON3.write((; error = msg)))

const _POP_404_SHOWN = 8

# A population 404 names the paths that DO exist, so the caller (a person or an agent) corrects the
# path in one step instead of guessing again. Those sharing the requested leaf name come first.
function _pop_not_found(m::PopulationTree, pop::AbstractString)
    paths = String[p for p in pop_paths(m) if !pop_at(m, p).transient]
    isempty(paths) && return _gerr(404, "Population not found: $pop — this label set has no populations yet")
    parts = split(String(pop), '/'; keepempty = false)
    leaf  = lowercase(isempty(parts) ? "" : String(parts[end]))
    rank(p) = (q = lowercase(p); endswith(q, "/" * leaf) ? 0 : (!isempty(leaf) && occursin(leaf, q)) ? 1 : 2)
    ranked = sort(paths; by = rank, alg = MergeSort)    # stable: equal ranks keep the map's order
    more   = length(ranked) - _POP_404_SHOWN
    _gerr(404, "Population not found: $pop — existing: " * join(first(ranked, _POP_404_SHOWN), ", ") *
               (more > 0 ? " (+$more more)" : ""))
end

# ── Pick-selection registry (linked brushing) ────────────────────────────────
# A spatial selection made in the viewer (pick-cell / pick-rect) is mirrored onto the
# flow plots as a transient population ("see those XY cells in channel space"). The
# selection is ephemeral server state keyed by (task_dir, value_name) — never persisted
# (docs/POPULATION.md). It is injected as a transient `Population` (explicit-label
# membership) into every map served or broadcast, so it reuses all existing
# plotdata/stats/highlight machinery unchanged.
const PICK_SEL_PATH   = "/" * "Pick selection"
const PICK_SEL_NAME   = "Pick selection"
const PICK_SEL_COLOUR = "#22d3ee"                       # cyan, distinct from gate pops
# A FIXED uid: the pop is rebuilt on every map load, and clients key a pop's highlight / eye on its uid
# (`makePopPathRemap`: a uid that vanished = a delete). A fresh uid per load read every new pick as
# "old pop deleted" and switched its plot highlight off. Not 6 chars, so `gen_uid` never mints it.
const PICK_SEL_UID    = "pick_selection"
const _pick_sel       = Dict{Tuple{String,String},Vector{Int}}()
const _pick_sel_lock  = ReentrantLock()

_set_pick_selection!(task_dir::AbstractString, vn::AbstractString, labels::Vector{Int}) =
    lock(_pick_sel_lock) do
        isempty(labels) ? delete!(_pick_sel, (String(task_dir), String(vn))) :
                           (_pick_sel[(String(task_dir), String(vn))] = labels)
    end
_get_pick_selection(task_dir::AbstractString, vn::AbstractString) =
    lock(_pick_sel_lock) do; get(_pick_sel, (String(task_dir), String(vn)), nothing); end

# add the transient pick-selection pop to a freshly-loaded map (no-op when no selection). Cell-grained
# maps only — its labels are CELL labels, which mean nothing on the per-track table (`label ==
# track_id`) — so a track/trackclust tree never lists a pop no read can resolve.
function _inject_pick_pop!(m::PopulationTree, img::CciaImage)
    is_track_grained(m.pop_type) && return m
    labs = _get_pick_selection(img._dir, m.value_name)
    (labs === nothing || isempty(labs)) && return m
    has_pop(m, PICK_SEL_PATH) && return m
    add_pop!(m, PICK_SEL_NAME; parent = ROOT, colour = PICK_SEL_COLOUR,
             explicit_labels = labs, transient = true, uid = PICK_SEL_UID)
    m
end

# resolve a CciaImage from project/image uids; returns (img, nothing) or (nothing, errtuple)
function _gating_image(project_uid::AbstractString, image_uid::AbstractString)
    isempty(project_uid) && return (nothing, _gerr(400, "projectUid required"))
    isempty(image_uid)   && return (nothing, _gerr(400, "imageUid required"))
    isdir(joinpath(projects_dir(), project_uid)) || return (nothing, _gerr(404, "Project not found: $project_uid"))
    isfile(state_file(joinpath(projects_dir(), project_uid), image_uid)) ||
        return (nothing, _gerr(404, "Image not found: $image_uid"))
    obj = init_object(project_uid, image_uid)
    obj isa CciaImage || return (nothing, _gerr(404, "Not an image: $image_uid"))
    (obj, nothing)
end

# active label_props value name. label_props is a PLAIN map {value_name => file} with no
# "_active" pointer (unlike filepath/labels), so versioned_active would wrongly yield the
# literal "default". Prefer an explicit _active if present, else the first real key.
function _active_vn(img::CciaImage)::String
    isempty(img.label_props) && return "default"
    act = get(img.label_props, VERSIONED_ACTIVE_KEY, nothing)
    (act !== nothing && haskey(img.label_props, act)) && return String(act)
    String(first(versioned_keys(img.label_props)))
end

# resolve a requested value_name to a real label_props key (else the active one) — the
# client may send a stale/blank value_name (e.g. "default") that this image doesn't have.
function _resolve_vn(img::CciaImage, requested::AbstractString)::String
    (!isempty(requested) && haskey(img.label_props, requested)) ? String(requested) : _active_vn(img)
end

# P3b: read the optional `labelsVersion` query/body param. Empty ⇒ `nothing` (follow `_latest`); a
# concrete `vN` string pins reads to that labels version for this request. The gating module sends
# it on every request after the user picks "Use pinned vN" on the drift banner; not sent (or set to
# "") after they pick "Re-eval on _latest" or on a fresh session with no drift.
_labels_version_pin(q::AbstractDict)::Union{String,Nothing} =
    (v = String(get(q, "labelsVersion", "")); isempty(v) ? nothing : v)

# true when the image actually has a labelProps table (i.e. it has been segmented/measured). An
# image that hasn't been segmented yet has none — `versioned_keys` is empty — so gating/membership
# endpoints that read the cell table must degrade gracefully rather than let `label_props` throw a
# 500. (Once segmented — static images included — labelProps exist and the normal path applies.)
_has_label_props(img::CciaImage)::Bool = !isempty(versioned_keys(img.label_props))

# Intersect column lists across a set of value_names, preserving the first VN's order. Used by the
# multi-VN picker scope (`?valueNames=A,B`): under contract B the picker only offers features that
# exist on EVERY selected VN, so `clustTracks.cluster` can't be asked for a column that would
# KeyError in the per-VN `track_props` read. Returns `String[]` for an empty input.
function _intersect_cols_across_vns(vns::Vector{String}, getter)::Vector{String}
    isempty(vns) && return String[]
    first_cols = getter(first(vns))
    length(vns) == 1 && return first_cols
    keep = Set{String}(first_cols)
    for v in vns[2:end]
        intersect!(keep, Set{String}(getter(v)))
    end
    filter(c -> c in keep, first_cols)
end

# Parse `?valueNames=A,B` — comma-sep list, filtered to real label_props keys. Returns [] when the
# query is absent or every entry is stale, in which case the caller keeps its single-VN path.
function _parse_multi_vns(img::CciaImage, raw::AbstractString)::Vector{String}
    isempty(raw) && return String[]
    keys_ok = Set{String}(versioned_keys(img.label_props))
    String[String(strip(x)) for x in split(raw, ',', keepempty=false)
        if !isempty(strip(x)) && String(strip(x)) in keys_ok]
end

# pick the channel-name version to label the intensity columns with: the ACTIVE one when its length
# matches (the names the image table, viewer and project store show — a rename writes there), else
# the first whose length matches (an AF-corrected image adds channels); fall back to the active version.
# Dict order is arbitrary, so without the active preference two same-length versions (a denoised copy)
# could label plots with a different list than the rest of the app shows.
function _matching_channel_version(versions::AbstractDict, n_channels::Int;
                                   active::AbstractString = VERSIONED_DEFAULT_VAL)::String
    isempty(versions) && return ""
    a = get(versions, active, nothing)
    a !== nothing && length(a) == n_channels && return String(active)
    for (v, names) in versions
        names !== nothing && length(names) == n_channels && return v
    end
    haskey(versions, active) ? String(active) : VERSIONED_DEFAULT_VAL
end

# track-table motility (var) columns — gateable per-track axes that need no per-cell aggregation.
_track_motility_cols(img, vn) = (p = img_track_props_path(img, vn);
    isfile(p) ? col_names(label_props(p); data_type = :vars) : String[])

# The ONE place the gating routes differ from the package's population read: the transient
# pick-selection pop (a viewer selection mirrored onto the plots) is UI state held here, so it is
# handed to the package as a `map_hook`. `nothing` on a track-grained pop_type (the pick never applies
# there, see `_inject_pick_pop!`), which keeps those reads cacheable.
_pick_hook(img, pop_type) = is_track_grained(pop_type) ? nothing : (m -> _inject_pick_pop!(m, img))
# a caller's hook first, then the pick selection's
function _with_pick_hook(img, pop_type, hook)
    pick = _pick_hook(img, pop_type)
    hook === nothing ? pick : pick === nothing ? hook : (m -> (hook(m); pick(m)))
end

# load + recompute a map (membership ready) — `computed_pop_tree` plus the pick selection.
# `labels_version` is the P3b session pin (`nothing` = follow `_latest`).
_live_map(img, vn, pop_type; labels_version::Union{String,Nothing}=nothing, map_hook=nothing) =
    computed_pop_tree(img; value_name = vn, pop_type = pop_type, labels_version = labels_version,
                     map_hook = _with_pick_hook(img, pop_type, map_hook))

# The pop_type a read route evaluates `pop` under: the package's `read_pop_type` over the request's
# `popType` (absent = discover from the path). So a derived `_tracked` set — what tracking makes and
# tasks take as input (`P14/_tracked`) — is found by every read, not 404'd under the `flow` default.
_read_pop_type(q::AbstractDict, img, vn, pop) = read_pop_type(img, vn, pop, get(q, "popType", ""))

# build an AxisTransform from query params, prefix "x"/"y" (e.g. xt=logicle&xT=262144)
function _axis_transform(q::AbstractDict, p::AbstractString)
    kind = lowercase(get(q, p * "t", "linear"))
    num(key, d) = parse(Float64, get(q, p * key, string(d)))
    kind == "log"     ? LogTransform(floor = num("floor", 1.0)) :
    kind == "asinh"   ? AsinhTransform(cofactor = num("cof", 150.0)) :
    kind == "logicle" ? LogicleTransform(T = num("T", 262144.0), W = num("W", 0.5),
                                         M = num("M", 4.5), A = num("A", 0.0)) :
    LinearTransform()
end

# A logicle or asinh axis the caller left UNPARAMETERISED takes its parameters from the channel's own
# data (`raw`, the whole segmentation): logicle's top `T` = the channel maximum, asinh's cofactor = 1 %
# of its 99th percentile (linear below that, log above — about two decades of log range under the
# bulk of the data). The fixed defaults are a flow cytometer's (T = 262144, cofactor 150): on imaging
# intensities in the tens they squeeze every cell into a sliver or leave the axis effectively linear.
# A parameter the caller sends wins. plotmeta reports the result (`xSpec`…) and the client sends it
# back verbatim to plotdata and when it stores a gate, so dots, outlines and gates share one transform.
function _auto_params(t::AxisTransform, q::AbstractDict, p::AbstractString, raw::AbstractVector)
    if t isa LogicleTransform && !haskey(q, p * "T")
        f = filter(isfinite, raw)                      # an empty read (a column this table lacks) → keep t
        isempty(f) && return t
        hi = maximum(f)
        hi > 0 || return t
        return LogicleTransform(T = float(hi), W = t.W, M = t.M, A = t.A)
    elseif t isa AsinhTransform && !haskey(q, p * "cof")
        pos = sort!(Float64[v for v in raw if isfinite(v) && v > 0])
        isempty(pos) && return t
        return AsinhTransform(cofactor = 0.01 * pos[clamp(round(Int, 0.99 * length(pos)), 1, length(pos))])
    end
    t
end

_needs_auto(t::AxisTransform, q::AbstractDict, p::AbstractString) =
    (t isa LogicleTransform && !haskey(q, p * "T")) || (t isa AsinhTransform && !haskey(q, p * "cof"))

# `_auto_params` for a route that has not read the whole segmentation itself (summary, density, the
# plot picture): reads the two raw columns over ROOT only when an axis needs it, so every gating read of
# the same query lands in the same coordinates as plotmeta's axes.
function _auto_axes(q::AbstractDict, img, vn, pop_type, x, y, xt, yt; labels_version = nothing)
    (_needs_auto(xt, q, "x") || _needs_auto(yt, q, "y")) || return (xt, yt)
    rx, ry = _xy_pair(_plot_cols_raw(img, vn, pop_type, [x, y], ROOT; labels_version = labels_version))
    (_auto_params(xt, q, "x", rx), _auto_params(yt, q, "y", ry))
end

_fmt_tick(v) = abs(v) >= 1000 || (v != 0 && abs(v) < 0.01) ?
               string(round(v; sigdigits = 3)) : string(round(v; digits = 2))

# evenly-spaced ticks in transformed space, labelled with the raw (inverse) value
function _axis_ticks(t::AxisTransform, rmin::Real, rmax::Real; n::Int = 6)
    tmin = apply_transform(t, float(rmin)); tmax = apply_transform(t, float(rmax))
    [begin
        pos = tmin + (tmax - tmin) * (i - 1) / (n - 1)
        Dict("pos" => pos, "label" => _fmt_tick(invert_transform(t, pos)))
     end for i in 1:n]
end

# Channel display names for a table's intensity columns (`chans`). They are VERSIONED (e.g. AF
# correction adds extra channels), and the label value_name doesn't map 1:1 to a channel-name version:
# pick the version whose length matches the number of channels the columns cover (`n_channels_covered`
# — not the column count). Returns every version (for a client that picks itself) and the matched
# names (`nothing`/empty when none matches), indexed by CHANNEL — resolve a column with
# `_channel_label`, never by its position in `chans`.
function _channel_display_names(img, chans::AbstractVector{<:AbstractString})
    versions = Dict{String,Any}(
        v => channel_names(img; value_name = v) for v in versioned_keys(img.im_channel_names))
    display = get(versions, _matching_channel_version(versions, Cecelia.n_channels_covered(chans);
                                                      active = versioned_active(img.im_channel_names)), String[])
    versions, display
end

# A plot axis's title as the Gate page shows it (`gating` store `colLabel` + `axisLabelWithUnit`): an
# intensity column → its channel name, a centroid → "X position" / "Time", anything else as-is;
# then " (unit)" when the axis has one.
const _CENTROID_TITLES = Dict("centroid_x" => "X position", "centroid_y" => "Y position",
                              "centroid_z" => "Z position", "centroid_t" => "Time")
function _axis_title(img, vn, pop_type, col::AbstractString; labels_version = nothing)::String
    name = get(_CENTROID_TITLES, lowercase(col), String(col))
    try
        chans = channel_columns(label_props(img; value_name = vn, version = labels_version))
        if col in chans
            _, display = _channel_display_names(img, chans)
            label = display === nothing ? col : Cecelia._channel_label(String(col), String.(display))
            (label != col && !isempty(label)) && (name = label)
        end
    catch
    end
    unit = _axis_unit(img, vn, pop_type, col)
    isempty(unit) ? name : "$name ($unit)"
end

# The unit a plot axis is displayed in — served to the client so it can label the axis (and so the
# label can never claim µm while the numbers are pixels). "" for a non-spatial axis (an intensity or
# morphology column has no length unit to state). Decided by `gate_axis_scale`, the package rule the
# plot VALUES are scaled by, so the label and the numbers cannot disagree.
_axis_unit(img, vn, pop_type, col)::String =
    !is_spatial_axis(String(col)) ? "" :
    (gate_axis_scale(load_pop_tree(img; value_name = vn, pop_type = pop_type), String(col)) == 1.0 ?
        SPATIAL_UNIT_PX : "µm")

# x/y in the gates' unit — the point cloud, the whole-dataset extents that drive the ticks, density.
_plot_xy_raw(img, vn, pop_type, x, y, pop; labels_version=nothing, map_hook=nothing) =
    _xy_pair(_plot_cols_raw(img, vn, pop_type, [x, y], pop; labels_version, map_hook))

# x/y are all-or-nothing: ONE of them missing means there is nothing to plot, and returning a full
# vector beside an empty one would index past the end of the short one (a 500) in every consumer.
_xy_pair(v) = (isempty(v[1]) || isempty(v[2])) ? (Float64[], Float64[]) : (v[1], v[2])

# Any number of plot columns (x/y, the colour-by z, the point labels) from ONE aligned read —
# `pop_plot_cols` (docs/POPULATION.md → *The gate-plot read*): membership, gate units, per-column
# empty for a column the table lacks, empty for an unknown/vanished pop. Only the pick selection is
# added here. An image with no cell table yet (not segmented, or a segmentation still running while
# the gate plot polls) reads as every column empty — the same answer as a column the table lacks —
# rather than letting `label_props` throw a 500 on every plot request.
# `map_hook` edits the loaded map before the pick selection is added (the rule preview swaps in the map a
# copy would write).
function _plot_cols_raw(img, vn, pop_type, cols, pop; labels_version::Union{String,Nothing}=nothing,
                        map_hook=nothing)
    _has_label_props(img) || return [Float64[] for _ in cols]
    pop_plot_cols(img, pop_type, pop, cols; value_name = vn, labels_version = labels_version,
                  map_hook = _with_pick_hook(img, pop_type, map_hook))
end

# transformed Float32 vectors for plotdata/density/plotmeta
function _plot_xy(img, vn, pop_type, x, y, pop, xt, yt; labels_version=nothing, map_hook=nothing)
    xv, yv = _plot_xy_raw(img, vn, pop_type, x, y, pop; labels_version, map_hook)
    Float32.(transform_column(xt, xv)), Float32.(transform_column(yt, yv))
end

# x/y PLUS the colour-by measure, from the one aligned read (see `_plot_cols_raw`)
function _plot_xyz(img, vn, pop_type, x, y, z, pop, xt, yt, zt; labels_version=nothing)
    v = _plot_cols_raw(img, vn, pop_type, [x, y, z], pop; labels_version = labels_version)
    xv, yv = _xy_pair(v)
    # an unreadable colour measure (not a column of THIS table) is "no value for any cell" — NaN, which
    # the client paints in the dim ink — not a short vector, and never a reason to drop the dots.
    zv = length(v[3]) == length(xv) ? Float32.(transform_column(zt, v[3])) : fill(NaN32, length(xv))
    (Float32.(transform_column(xt, xv)), Float32.(transform_column(yt, yv)), zv)
end

# `{path → [column]}`: for each population that is EMPTY on this image because a gate/filter column
# its membership reads (its own or an ancestor's) is not in the image's table — typically
# `clusters.<run>` after copying gates from an image that was in a clustering run this one wasn't
# (#1769). Rides beside every `tree` so the Population Manager can say why such a pop reads 0.
# Column NAMES only (`missing_gate_columns` → `pop_membership_cols`), no data read, so it is cheap
# enough for every tree payload. Never fails the request: an unreadable table reports nothing.
function _missing_columns_by_pop(m::PopulationTree, img::CciaImage, vn;
                                 labels_version=nothing)::Dict{String,Vector{String}}
    out = Dict{String,Vector{String}}()
    (isempty(m.order) || !_has_label_props(img)) && return out
    miss = try
        missing_gate_columns(m, img; value_name = vn, labels_version = labels_version)
    catch e
        @warn "gating: could not check gate columns on $(img.uid)" exception = e
        return out
    end
    for r in miss, p in r.pops
        push!(get!(out, p, String[]), r.column)
    end
    out
end

# broadcast the updated tree to all clients
function _broadcast_pop_tree(project_uid, image_uid, vn, pop_type, m::PopulationTree, img::CciaImage;
                           missing_cols = _missing_columns_by_pop(m, img, vn))
    broadcast_ws(Dict{String,Any}(
        "type" => "gating:pop-tree", "projectUid" => project_uid, "imageUid" => image_uid,
        "valueName" => vn, "popType" => pop_type, "tree" => to_tree(m),
        "missingColumns" => missing_cols,
        # ride along with the tree so the undo/redo buttons settle in the same frame the edit lands
        # in — for THIS document, so a copy's target images report their own history, not the
        # source's (see the undo/redo block below)
        "canUndo" => _can_undo(project_uid, image_uid, vn, pop_type),
        "canRedo" => _can_redo(project_uid, image_uid, vn, pop_type)))
end

# Serialise gating pop CRUD. Each handler does load_pop_tree → mutate → save_pop_tree!; under
# `-t auto` two concurrent edits to the same map (rapid clicks / two tabs) would otherwise both
# load the same tree and the second save would clobber the first (lost update). One process-wide
# ReentrantLock held across the whole load→save is enough — gating edits are user-paced and cheap,
# so contention across images/value_names is negligible. (save_pop_tree! also writes atomically.)
# The lock lives in the package (`with_pop_tree_lock`, gating/popmanager/copy.jl) so a task that writes
# gating — the manual gate chain step — serialises with these handlers.
_with_pop_tree_lock(f) = with_pop_tree_lock(f)

# ── Undo / redo for HAND-DRAWN gating ─────────────────────────────────────────
#
# The whole document is one serialisable value: every mutation here is load → mutate →
# `save_pop_tree!` of the ENTIRE tree, so "the state before this edit" is just `to_tree` of what is
# still on disk when the handler is about to save. That is what makes an undo stack a ring of
# snapshots rather than a set of hand-written inverse operations — there is no diff protocol to
# invert, and nothing to keep in sync as new mutations are added.
#
# Scope is `flow` + `track` (`is_gating_pop_type`): the hand-drawn pop types, where a wrong drag
# destroys work you cannot get back by re-ticking a box. `clust`/`trackclust`/`region` are filter
# pops whose edit IS the tick, and they mirror set-wide through the client's `mirrorUids` — one
# user action, N image sidecars — so undoing them is a different problem and deliberately not this
# one.
#
# In-process and session-scoped on purpose: history dies with the backend rather than growing a
# user-facing sidecar with an edit log nobody asked to keep.
const _GATING_HISTORY_LIMIT = 50
# One entry = the state to restore, as (image uid → tree). A vector because an entry may span
# images; today every entry holds exactly one, and `api_gating_copy` is the case that would not.
const _GatingSnapshot = Vector{Tuple{String,Dict{String,Any}}}
const _GATING_HISTORY = Dict{String,NamedTuple{(:undo, :redo),Tuple{Vector{_GatingSnapshot},Vector{_GatingSnapshot}}}}()

_history_key(proj, image_uid, vn, pop_type) = join((proj, image_uid, vn, pop_type), '\u0000')
_history_for(key) = get!(() -> (undo = _GatingSnapshot[], redo = _GatingSnapshot[]), _GATING_HISTORY, key)

# The tree as it would be READ BACK from disk. `include_transient=false` keeps the pick selection
# out of history — it is ephemeral server state that is re-injected on every read, and restoring a
# stale one would resurrect a selection the user has since cleared.
_gating_snapshot(img::CciaImage, vn, pop_type)::Dict{String,Any} =
    to_tree(load_pop_tree(img; value_name = vn, pop_type = pop_type); include_transient = false)

# Record the PRE-edit state. Called from `_persist_and_broadcast!` — the one choke point every
# mutating handler already goes through, so a new mutation cannot forget to be undoable. Runs under
# the gating lock (`with_pop_tree_lock`; the handler holds it across load→save), so what is on disk here is exactly what
# this edit is about to replace. A new edit invalidates the redo branch, as everywhere else.
function _history_record!(proj, image_uid, vn, pop_type, img::CciaImage)
    is_gating_pop_type(pop_type) || return
    h = _history_for(_history_key(proj, image_uid, vn, pop_type))
    push!(h.undo, [(String(image_uid), _gating_snapshot(img, vn, pop_type))])
    length(h.undo) > _GATING_HISTORY_LIMIT && popfirst!(h.undo)
    empty!(h.redo)
    nothing
end

_can_undo(proj, image_uid, vn, pop_type) = is_gating_pop_type(pop_type) &&
    !isempty(_history_for(_history_key(proj, image_uid, vn, pop_type)).undo)
_can_redo(proj, image_uid, vn, pop_type) = is_gating_pop_type(pop_type) &&
    !isempty(_history_for(_history_key(proj, image_uid, vn, pop_type)).redo)

# persist a mutated map then broadcast — re-injecting the transient napari pop AFTER save
# (so it never hits disk) but BEFORE broadcast (so the client keeps showing it). Returns the whole
# HTTP response body: the tree (incl. the transient pop) plus the undo/redo flags, so the POST that
# made the edit is self-sufficient. The broadcast carries them too, but a client must not need its
# WS to be up to know whether it can undo what it just did.
function _persist_and_broadcast!(m::PopulationTree, img::CciaImage, body, vn, pop_type)
    proj, image_uid, err = _require_ids(body); err === nothing || return err
    _anchor_follow_rules!(m, img, vn, pop_type)
    _history_record!(proj, image_uid, vn, pop_type, img)
    save_pop_tree!(m, img)
    _inject_pick_pop!(m, img)
    # the broadcast reaches every client, so it answers at `_latest`; the reply answers at THIS
    # client's labels-version pin (the store sends it in the body), the same frame its counts use
    pin = _labels_version_pin(body)
    miss = _missing_columns_by_pop(m, img, vn)
    _broadcast_pop_tree(proj, image_uid, vn, pop_type, m, img; missing_cols = miss)
    (; tree = to_tree(m),
       missingColumns = pin === nothing ? miss : _missing_columns_by_pop(m, img, vn; labels_version = pin),
       canUndo = _can_undo(proj, image_uid, vn, pop_type),
       canRedo = _can_redo(proj, image_uid, vn, pop_type))
end

# A rectangle / polygon saved with a follow rule and no anchor (just drawn, toggled on, or edited on the
# plot — the plot never sends the anchor back) is anchored on THIS image's parent cells before it is
# written: the cloud it was drawn on (app/src/gating/follow_rule.jl). One with no cloud under it is written
# unanchored and tried again at each later save of the tree (one extra evaluation per save, only while one
# is unanchored). Every gate write passes here, and Claude's dry run calls it too, so the counts it reports
# are the saved gate's.
function _anchor_follow_rules!(m::PopulationTree, img, vn, pop_type)
    any(p -> p.gate !== nothing && Cecelia.needs_anchor(p.gate), values(m.pops)) || return m
    recompute!(m, pop_membership_fetch(img, vn, pop_type); anchor = true)
end

# POST /api/gating/undo · /api/gating/redo — pop one snapshot off the stack, put the CURRENT state
# on the other one, write it back through the same save+broadcast path a normal edit uses (so every
# open client converges the same way). Deliberately does NOT go through `_persist_and_broadcast!`:
# stepping through history must not record itself as a new edit.
api_gating_undo(body_bytes::Vector{UInt8}) = _history_step(body_bytes, :undo)
api_gating_redo(body_bytes::Vector{UInt8}) = _history_step(body_bytes, :redo)

function _history_step(body_bytes::Vector{UInt8}, dir::Symbol)
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    is_gating_pop_type(pt) || return _gerr(400, "Undo is for hand-drawn gating only (flow/track), not $pt")
    proj, image_uid, err = _require_ids(body); err === nothing || return err
    _with_pop_tree_lock() do
        h = _history_for(_history_key(proj, image_uid, vn, pt))
        from, to = dir === :undo ? (h.undo, h.redo) : (h.redo, h.undo)
        isempty(from) && return _gerr(409, dir === :undo ? "Nothing to undo" : "Nothing to redo")
        entry = pop!(from)
        counter = _GatingSnapshot()
        for (uid, tree) in entry
            timg, _ = _gating_image(proj, uid)
            timg === nothing && continue                      # image gone — skip, don't fail the step
            push!(counter, (uid, _gating_snapshot(timg, vn, pt)))
            save_pop_tree!(from_tree(tree), timg)
            m = load_pop_tree(timg; value_name = vn, pop_type = pt)
            _inject_pick_pop!(m, timg)
            _broadcast_pop_tree(proj, uid, vn, pt, m, timg)
        end
        push!(to, counter)
        m = load_pop_tree(img; value_name = vn, pop_type = pt)
        _inject_pick_pop!(m, img)
        200, JSON3.write((; tree = to_tree(m),
                            missingColumns = _missing_columns_by_pop(m, img, vn;
                                                                     labels_version = _labels_version_pin(body)),
                            canUndo = !isempty(h.undo), canRedo = !isempty(h.redo)))
    end
end

# Run suffixes available in a table's obs, for the pop_type's own column FAMILY: `clusters.{suffix}`
# for clust/trackclust (clustPops/clustTracks), `regions.{suffix}` for region (clustRegions). The prefix
# is never spelled here — it comes from the single package-side decision
# (`Cecelia._cluster_measure_prefix`, docs/todo/SPATIAL_REGIONS_PLAN.md Decision 5), which is why the
# region page saw an EMPTY run list while `regions.immune` sat in obs: this helper used to hardcode
# "clusters.". Drives the cluster/region pages' page-level run dropdown.
_cluster_suffixes(obscols, pop_type = "clust") =
    let prefix = Cecelia._cluster_measure_prefix(pop_type)
        String[c[ncodeunits(prefix)+1:end] for c in obscols if startswith(c, prefix)]
    end

# per-suffix run manifest ({props}.clustfeatures.json; clustPops/clustTracks/clustRegions write it).
# `features` → the heatmap offers exactly those columns; `partOf` → the uIDs clustered together (mirrors
# old R `valuePartOf`), so cluster/region pops are only defined for images in the run; `labels` →
# optional column → display-label map (region composition columns are machine names). Reads go through
# the package's shared sidecar reader, which owns the family-qualified + legacy key layouts — the API
# never indexes that JSON itself.
# Features by channel NAME — a run recorded before docs/todo/CHANNEL_NAMES_PLAN.md is named against the
# segment it was picked on (`chans_of` = `run_channel_names`), so the frontend shows them as they come.
_clust_features(props_path::AbstractString, suffixes, family; chans_of = nothing, own_uid = nothing) =
    Dict{String,Any}(s => Cecelia._clustfeatures_features(props_path, s; family = family,
                                                          chans_of = chans_of, own_uid = own_uid)
                     for s in suffixes)
# JSON3 hands back Symbol OR String keys depending on how the object was parsed, so every field read
# goes through this. NEVER wrap it in `something(..., nothing)`: `something()` with all-nothing
# arguments THROWS `ArgumentError("No value arguments present")`, which is exactly how
# /api/gating/channels?popType=region started 500ing on any sidecar entry written before the `labels`
# field existed — i.e. every image that hadn't been re-run.
_entry_field(e, key) = e === nothing ? nothing :
    let v = get(e, String(key), nothing)
        v === nothing ? get(e, Symbol(key), nothing) : v
    end

function _clust_members(props_path::AbstractString, suffixes, family)
    out = Dict{String,Any}()
    for s in suffixes
        out[s] = Cecelia._clustfeatures_part_of(Cecelia._clustfeatures_entry(props_path, s; family = family))
    end
    out
end
# per-suffix run stamp (`cluster_run_at`) — a cluster population whose `runAt` differs was picked on an
# earlier run under the same name (the manager marks it stale). Runs from before the stamp are absent.
function _clust_run_at(props_path::AbstractString, suffixes, family)
    out = Dict{String,Any}()
    for s in suffixes
        at = cluster_run_at(props_path, s; family = family)
        at === nothing || (out[s] = at)
    end
    out
end

# Per run: the params it was made with and, for a refine run, what it refines and why it is stale —
# `{suffix => {params, refines?: {pop, run}, stale?: reason}}`. The cluster page draws the run tree and
# each level's resolution from it (docs/todo/CLUSTER_REFINE_PLAN.md). Lineage is cell clusters only.
function _clust_runs(img::CciaImage, vn::AbstractString, props_path::AbstractString, suffixes, family, pop_type)
    out = Dict{String,Any}()
    m = pop_type == "clust" ? load_pop_tree(img; value_name = vn, pop_type = "clust", backfill_save = false) : nothing
    for s in suffixes
        run = cluster_run_info(img, vn, props_path, s; family = family, m = m)
        run === nothing || (out[s] = run)
    end
    out
end

function _clust_feature_labels(props_path::AbstractString, suffixes, family)
    out = Dict{String,Any}()
    for s in suffixes
        l = _entry_field(Cecelia._clustfeatures_entry(props_path, s; family = family), "labels")
        l isa AbstractDict && !isempty(l) &&
            (out[s] = Dict{String,Any}(String(k) => string(v) for (k, v) in l))
    end
    out
end

# distinct cluster IDs per suffix — the universe the pop-manager offers as tickable clusters. Clustering
# is SET-scope, so a single image's `clusters.{suffix}` column carries only the subset of the shared
# 0..k-1 codes present in ITS objects; scanning one image under-reports (e.g. 2 of 4). Pool across the
# run's MEMBER images (`partOf`, from _clust_members) so the universe matches the pooled UMAP/heatmap.
# A suffix with no recorded members (pre-partOf run) falls back to the primary image's own column.
# per-suffix behaviour axes (track runs: axis names + path root, written by clustTracks as
# `behaviourAxes`). Suffixes without them are absent, so the cluster map offers its diffusion layout only
# for runs that have one.
function _clust_axes(props_path::AbstractString, suffixes, family; chans_of = nothing, own_uid = nothing)
    out = Dict{String,Any}()
    for s in suffixes
        a = Cecelia._clustfeatures_axes(props_path, s; family = family, chans_of = chans_of, own_uid = own_uid)
        a isa AbstractDict && (out[s] = a)
    end
    out
end

function _cluster_ids(project_uid::AbstractString, primary_path::AbstractString, vn::AbstractString,
                      suffixes::Vector{String}, members::AbstractDict, track::Bool,
                      pop_type::PopTypeArg = "clust")
    pathcache = Dict{String,Union{String,Nothing}}()   # init_object per uID isn't free; a member may back several suffixes
    member_path(u) = get!(pathcache, u) do
        mi = try init_object(project_uid, u) catch; nothing end
        mi isa CciaImage || return nothing
        p = track ? img_track_props_path(mi, vn) : img_label_props_path(mi, vn)
        isfile(p) ? p : nothing
    end
    out = Dict{String,Any}()
    prefix = Cecelia._cluster_measure_prefix(pop_type)   # "clusters." | "regions." — never hardcoded
    for s in suffixes
        col = "$(prefix)$(s)"
        paths = String[]
        for u in String.(get(members, s, String[]))
            p = member_path(u); p === nothing || push!(paths, p)
        end
        isempty(paths) && push!(paths, primary_path)
        ids = Set{Int}()
        for p in paths
            df = try label_props(p) |> select_cols([col]) |> as_df catch; continue end
            hasproperty(df, Symbol(col)) || continue
            for x in df[!, col]
                (isfinite(x) && x >= 0) && push!(ids, Int(round(x)))
            end
        end
        out[s] = sort!(collect(ids))
    end
    out
end

# ── GET /api/gating/channels — gateable columns + channel display names ───────
function api_gating_channels(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    # Multi-VN scope (`?valueNames=A,B`) — used by track-clustering pickers when the selected pops
    # span more than one segmentation. Only the picker-relevant column lists intersect;
    # metadata (versions, cluster IDs, channel display) stays scoped to the first VN.
    multi_vns = _parse_multi_vns(img, get(q, "valueNames", ""))
    scope_vn = length(multi_vns) >= 2 ? first(multi_vns) : vn
    do_intersect = length(multi_vns) >= 2
    # image not segmented yet: no labelProps → nothing is gateable. Return empty rather than 500ing
    # when the gating UI probes channels for it (label_props would throw "No labelProps …"). After
    # segmentation the image has labelProps and the normal path below runs.
    if !_has_label_props(img)
        return 200, JSON3.write((;
            columns = String[], channels = String[], channelNames = String[],
            channelNameVersions = Dict{String,Any}(), obsColumns = String[], temporalColumns = String[],
            cellMeasures = String[], trackAggregates = String[], clusterSuffixes = String[],
            clusterFeatures = Dict{String,Any}(), clusterMembers = Dict{String,Any}(), clusterRuns = Dict{String,Any}(),
            valueNames = String[], valueName = vn, popType = get(q, "popType", "flow")))
    end
    # track gating: the gateable axes are the per-track properties — motility (the track table's
    # var columns, directly gateable) plus on-read aggregates of any cell measure. We return the
    # motility columns + the aggregatable cell measures + the aggregate suffixes so the client can
    # offer e.g. "mean CD4 per track" → axis `mean_intensity_0.mean` (track_cell_measures inverts it).
    if is_track_grained(get(q, "popType", "flow"))
        # For track/trackclust, `scope_vn` MUST be a tracked segmentation — the branch below reads
        # `img_track_props_path(img, scope_vn)` and everything downstream (clusterSuffixes,
        # clusterIds, cellMeasures aggregation) needs that file to exist. `_resolve_vn` picks the
        # active LABEL_PROPS vn, which may be an UNTRACKED segmentation (e.g. fXgbTl's active is
        # `default` but tracks live on `flowKat`) — and until now the client got an empty
        # clusterIds back, showing "no clusters at this suffix" in the pop manager while UMAP +
        # heatmap (which resolve their own vn per-request) showed the clusters just fine
        # (reported on fXgbTl 2026-09-08). When the request didn't name a valueName, prefer a
        # tracked one; an explicit request is honoured unchanged.
        if isempty(get(q, "valueName", ""))
            tracked_vns = String[v for v in versioned_keys(img.label_props) if is_tracked(img; value_name = v)]
            if !isempty(tracked_vns)
                # 1) if the resolved vn isn't tracked, coerce to the first tracked vn — otherwise
                #    the track_props path below doesn't exist.
                if !(scope_vn in tracked_vns)
                    scope_vn = first(tracked_vns)
                    vn = scope_vn
                end
                # 2) for `trackclust` with no explicit valueName, additionally prefer a tracked vn
                #    that HAS clustering runs — the resolved-tracked pick can still be a peer of the
                #    one the run lives on (e.g. a tracked `default` where the clustering was written
                #    to a differently-suffixed vn), so both cases must be guarded.
                if get(q, "popType", "") == "trackclust"
                    has_trackclust_runs = v -> begin
                        tp = img_track_props_path(img, v)
                        isfile(tp) && !isempty(_cluster_suffixes(
                            col_names(label_props(tp); data_type = :obs), "trackclust"))
                    end
                    if !has_trackclust_runs(scope_vn)
                        with_runs = filter(has_trackclust_runs, tracked_vns)
                        if !isempty(with_runs)
                            scope_vn = first(with_runs)
                            vn = scope_vn
                        end
                    end
                end
            end
        end
        # No labels pin here, deliberately: a track pop_type's gates evaluate over the per-track table,
        # which has no labels-version axis (`pop_membership_fetch`), so `_latest` is the frame they read.
        motility = _track_motility_cols(img, scope_vn)
        lpc = label_props(img; value_name = scope_vn)
        cellmeas = col_names(lpc; data_type = :vars)         # aggregatable cell vars
        cellobs  = col_names(lpc; data_type = :obs)          # aggregatable cell obs (HMM state/transitions)
        if do_intersect
            motility = _intersect_cols_across_vns(multi_vns, v -> _track_motility_cols(img, v))
            cellmeas = _intersect_cols_across_vns(multi_vns,
                v -> col_names(label_props(img; value_name = v); data_type = :vars))
            cellobs  = _intersect_cols_across_vns(multi_vns,
                v -> col_names(label_props(img; value_name = v); data_type = :obs))
        end
        _, display = _channel_display_names(img, channel_columns(lpc))
        tpath = img_track_props_path(img, scope_vn)
        tobs  = isfile(tpath) ? col_names(label_props(tpath); data_type = :obs) : String[]
        pt    = get(q, "popType", "track")
        tsfx  = _cluster_suffixes(tobs, pt)                      # trackclust runs in the track table
        tfam  = Cecelia._cluster_measure_family(pt)
        tmem  = _same_run_members(get(q, "projectUid", ""), img, scope_vn, pt, _clust_members(tpath, tsfx, tfam))
        # WHICH of this image's segmentations actually have tracks. Every track surface needs this and
        # none of them could get it: a track view defaulting to "default" or to the ACTIVE segmentation
        # picks an UNTRACKED one on any image where tracking ran on a named label set — on the
        # reference image (zolIMa/1/fXgbTl) `default` and the active `three` are both untracked while
        # `memTom` holds 374 tracks, so the correction worklist reported "nothing to review" for an
        # image with 31 candidates. `is_tracked` reads only the obs column list, so this is cheap.
        tracked = String[v for v in versioned_keys(img.label_props) if is_tracked(img; value_name = v)]
        # Per-VN channel names — populated only under multi-VN scope, so the pop-picker advisor can
        # decide whether the selected VNs measure the same channels (the "compatibility" contract
        # docs/POPULATION.md — clustTracks value-name intersection).
        chansPerVn = do_intersect ? Dict{String,Any}(
            v => something(channel_names(img; value_name = v), String[]) for v in multi_vns
        ) : Dict{String,Any}()
        return 200, JSON3.write((;
            columns = motility,                                  # whole-track motility (directly gateable)
            # per-track obs (clusters.{run}, track_state, …) — the categorical measures a track-grained
            # PLOT discovers. Own key, not `obsColumns`: the gating pickers read that as cell obs.
            trackObsColumns = tobs,
            trackedValueNames = tracked,                          # the ones a track view may default to
            cellMeasures = cellmeas,                             # cell vars → per-track numeric aggregates
            cellObsMeasures = cellobs,                           # cell obs → per-track aggregates (HMM, …)
            channelNames = display === nothing ? String[] : display,  # relabel intensity aggregates
            channelNamesPerVn = chansPerVn,                       # multi-VN advisor input (empty in single-VN mode)
            trackAggregates = ["mean", "median", "sum", "qUp", "qLow", "sd"],  # see track_props
            clusterSuffixes = tsfx,
            clusterFeatures = _clust_features(tpath, tsfx, tfam; chans_of = Cecelia.run_channel_names(img, scope_vn), own_uid = img.uid),
            clusterMembers  = tmem,         # uIDs clustered together (partOf) that still carry the run
            clusterRunAt    = _clust_run_at(tpath, tsfx, tfam),   # when each run ran — a cluster pop's staleness
            clusterRuns     = _clust_runs(img, scope_vn, tpath, tsfx, tfam, pt),   # params per run (no lineage)
            clusterFeatureLabels = _clust_feature_labels(tpath, tsfx, tfam),
            clusterAxes     = _clust_axes(tpath, tsfx, tfam; chans_of = Cecelia.run_channel_names(img, scope_vn),
                                          own_uid = img.uid),     # diffusion axis names (track runs)
            clusterIds      = isfile(tpath) ? _cluster_ids(get(q, "projectUid", ""), tpath, scope_vn,
                                tsfx, tmem, true, pt) : Dict{String,Any}(),
            valueNames = versioned_keys(img.label_props),
            valueName = scope_vn,
            popType = pt,
        ))
    end
    # The session's labels pin (P3b): the pickers must offer the PINNED table's columns, or a column only
    # `_latest` has can be chosen and every pinned plotdata/gate read of it comes back empty. A pin names
    # one segmentation's version, so it can't scope a multi-VN intersection — refuse that rather than
    # apply it to the first VN only. A version the entry doesn't have is refused too (label_props throws).
    pin = _labels_version_pin(q)
    if pin !== nothing
        do_intersect && return _gerr(400, "labelsVersion can't be combined with valueNames (a pin names one segmentation's version)")
        entry = get(img.label_props, scope_vn, nothing)
        # a bare (unversioned) entry IS v1 — `unversion_value` returns it for any pin, so check the name
        known = is_versioned_entry(entry) ? unversion_value(entry, pin) !== nothing : pin == Cecelia.LATEST_DEFAULT_VAL
        known || return _gerr(400, "No labels version $pin for $scope_vn")
    end
    lp = label_props(img; value_name = scope_vn, version = pin)
    cols = col_names(lp; data_type = :vars)        # all gateable feature columns
    chans = channel_columns(lp)                     # intensity columns specifically
    if do_intersect
        cols  = _intersect_cols_across_vns(multi_vns,
            v -> col_names(label_props(img; value_name = v); data_type = :vars))
        chans = _intersect_cols_across_vns(multi_vns,
            v -> channel_columns(label_props(img; value_name = v)))
    end
    versions, display = _channel_display_names(img, chans)
    # TRACK-level cluster columns (clusters.* in `{vn}__tracks.h5ad`, written by clustTracks). These
    # aren't in the cell obs, but the viewer colour-by broadcasts them to cells via track_id so you
    # can colour tracks by their cluster/population. Offered alongside cell obs columns.
    tpath = img_track_props_path(img, scope_vn)
    trackObs = isfile(tpath) ? col_names(label_props(tpath); data_type = :obs) : String[]
    trackColourColumns = String[c for c in trackObs if startswith(c, "clusters.")]
    # cell-table run family for THIS pop_type: `clusters.*` for clust, `regions.*` for region. Computed
    # once — the obs scan, the sidecar lookups and the ID universe must all agree on the family.
    cpt   = get(q, "popType", "flow")
    cobs  = col_names(lp; data_type = :obs)
    if do_intersect
        cobs = _intersect_cols_across_vns(multi_vns,
            v -> col_names(label_props(img; value_name = v); data_type = :obs))
    end
    csfx  = _cluster_suffixes(cobs, cpt)
    cfam  = Cecelia._cluster_measure_family(cpt)
    cpath = img_label_props_path(img, scope_vn; version = pin)
    cmem  = _same_run_members(get(q, "projectUid", ""), img, scope_vn, cpt, _clust_members(cpath, csfx, cfam))
    chansPerVnF = do_intersect ? Dict{String,Any}(
        v => something(channel_names(img; value_name = v), String[]) for v in multi_vns
    ) : Dict{String,Any}()
    200, JSON3.write((;
        columns = cols,
        channels = chans,
        channelNames = display === nothing ? String[] : display,
        channelNameVersions = versions,
        channelNamesPerVn = chansPerVnF,  # multi-VN advisor input (empty in single-VN mode)
        obsColumns = cobs,              # per-cell obs measures (live.cell.*, hmm.state, …) for labelPropsColsSelection
        trackColourColumns = trackColourColumns,         # track-level clusters.* — colour-by broadcasts to cells
        # spatial/temporal centroid axes (obsm) — offered as gating scatter axes you can visualise AND
        # gate on (like the old R flow frame). Read/gate-eval already materialise them via as_df.
        spatialColumns = centroid_columns(lp; order = [:x, :y, :z]),   # centroid_x/_y[/_z], present axes
        temporalColumns = temporal_columns(lp),          # ["centroid_t"] — groupable + a gateable time axis
        clusterSuffixes = csfx,         # clust runs (`clusters.*`) / region runs (`regions.*`) in the cell table
        clusterFeatures = _clust_features(cpath, csfx, cfam; chans_of = Cecelia.run_channel_names(img, scope_vn), own_uid = img.uid),
        clusterMembers  = cmem,         # uIDs clustered together (partOf) that still carry the run
        clusterRunAt    = _clust_run_at(cpath, csfx, cfam),   # when each run ran — a cluster pop's staleness
        clusterRuns     = _clust_runs(img, scope_vn, cpath, csfx, cfam, cpt),   # params + refine lineage per run
        clusterFeatureLabels = _clust_feature_labels(cpath, csfx, cfam),
        clusterIds      = _cluster_ids(get(q, "projectUid", ""), cpath, scope_vn, csfx, cmem, false, cpt),
        valueNames = versioned_keys(img.label_props),
        valueName = scope_vn,                 # the server-resolved value_name these columns belong to
        popType = cpt,
    ))
end

# ── GET /api/gating/pop-tree ────────────────────────────────────────────────────
# Returns the tree PLUS P3b breadcrumb fields:
#   • `authoredLabelsVersion`     — the labels vN this map was last saved against (`nothing` on
#     legacy pre-breadcrumb files and on maps whose value_name has no label_props yet).
#   • `currentLatestLabelsVersion` — the image's current `_latest` labels version for this
#     value_name (`nothing` when no label_props entry exists yet).
# The frontend renders the drift banner when both are non-null AND they differ.
function api_gating_pop_tree(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    pt = get(q, "popType", "flow")
    m = load_pop_tree(img; value_name = vn, pop_type = pt)
    _inject_pick_pop!(m, img)
    proj, image_uid = get(q, "projectUid", ""), get(q, "imageUid", "")
    current_latest = haskey(img.label_props, vn) ? resolve_version(img, :label_props, vn) : nothing
    200, JSON3.write((; tree = to_tree(m),
                        missingColumns = _missing_columns_by_pop(m, img, vn;
                                                                 labels_version = _labels_version_pin(q)),
                        canUndo = _can_undo(proj, image_uid, vn, pt),
                        canRedo = _can_redo(proj, image_uid, vn, pt),
                        authoredLabelsVersion = m.authored_labels_version,
                        currentLatestLabelsVersion = current_latest))
end

# ── GET /api/gating/stats?...&pop=/cd4 ────────────────────────────────────────
function api_gating_stats(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", "")); pop = get(q, "pop", ROOT)
    m = _live_map(img, vn, _read_pop_type(q, img, vn, pop); labels_version = _labels_version_pin(q))
    (is_root(pop) || has_pop(m, pop)) || return _pop_not_found(m, pop)
    s = pop_stats(m, pop)
    200, JSON3.write((; count = s.count, parentCount = s.parent_count, pctParent = s.pct_parent))
end

# ── GET /api/gating/membership?...&pops=/a,/b[&binary=1] ──────────────────────
function api_gating_membership(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    pops = _query_list(q, "pops")
    isempty(pops) && return _gerr(400, "pops required")
    # one map serves every pop: `live` is `flow` plus the derived sets, so a mix of the two reads as live
    pts = unique(_read_pop_type(q, img, vn, p) for p in pops)
    pt = length(pts) == 1 ? only(pts) : all(p -> pop_tree_type(p) == "flow", pts) ? "live" : get(q, "popType", "flow")
    m = _live_map(img, vn, pt; labels_version = _labels_version_pin(q))
    for p in pops
        (is_root(p) || has_pop(m, p)) || return _pop_not_found(m, p)
    end
    if get(q, "binary", "") == "1"
        length(pops) == 1 || return _gerr(400, "binary=1 requires exactly one pop")
        return 200, collect(reinterpret(UInt8, Int32.(cells_in_pop(m, pops[1]))))
    end
    200, JSON3.write((; membership = Dict(String(p) => cells_in_pop(m, p) for p in pops)))
end

# Extrema over FINITE values only. Object/morphology measurements (volume, convex_hull_area,
# surface_to_volume, …) carry NaN/Inf for degenerate or border objects; plain `extrema` would
# propagate a single NaN into the extent, and JSON3 refuses to serialise NaN (→ 500, so the plot
# never renders). Falls back when the column is empty or all-non-finite.
function _finite_extrema(v, fallback::Tuple{Float64,Float64} = (0.0, 1.0))::Tuple{Float64,Float64}
    lo = Inf; hi = -Inf
    for x in v
        xf = float(x)
        isfinite(xf) || continue
        xf < lo && (lo = xf)
        xf > hi && (hi = xf)
    end
    lo <= hi ? (lo, hi) : fallback
end

# The COLOUR ramp's range: a robust 2–98 percentile of the (transformed) values, not min…max.
#
# Measured on a real 3P spleen dataset (4471 cells, `Bcells-ubiTom` on logicle): p2–p98 covered **28%**
# of the full min…max range, so a full-range ramp spent ~70% of the colour scale on a handful of outlying
# cells and the whole cloud came out one flat orange. The same clip is what a viewer does for contrast
# limits, and it is not hidden: the colour bar is labelled with THESE numbers, and values outside clamp
# to the ends of the ramp (they stay visible, at the extreme colour).
#
# An axis is different and keeps min…max — a gate is drawn against it, so it may not lie about where a
# cell sits. Nothing is gated on a colour.
const RAMP_CLIP = 0.02
function _ramp_range(v::AbstractVector)
    f = Float32[x for x in v if isfinite(x)]
    isempty(f) && return nothing
    n = length(f)
    lo_i = clamp(floor(Int, RAMP_CLIP * (n - 1)) + 1, 1, n)
    hi_i = clamp(ceil(Int, (1 - RAMP_CLIP) * (n - 1)) + 1, 1, n)
    # partialsort! reorders `f` but never changes WHICH values it holds, so the second k-th-smallest
    # query is still correct — two O(n) selections instead of an O(n log n) sort of a million cells.
    lo = partialsort!(f, lo_i)
    hi = partialsort!(f, hi_i)
    hi > lo && return (Float64(lo), Float64(hi))
    e = _finite_extrema(f)                      # degenerate clip (near-constant measure) → full range
    e[2] > e[1] ? e : nothing
end

# Grow a display extent `(lo, hi)` to also enclose the gate edges `edges` (display coordinates on that
# axis), plus a `margin` fraction of the span so an edge lands inside the axes (grabbable), not flush
# on the border. Only the side an edge exceeds moves. `reach` (one data span) covers any edge a user
# drags off the plot — the overlay tracks the drag on the window, so a release can land beyond the
# axes and must come back grabbable. An edge farther than that beyond the data is an OPEN-ENDED threshold (a one-sided gate written as "up to 5000"), not a boundary
# someone placed out there: following it would shrink the cells into a corner of the plot, so it is
# left off-axis and the renderer clips the gate at the border instead.
function _include_edges((lo, hi)::Tuple, edges; margin::Float64 = 0.05, reach::Float64 = 1.0)
    span = hi > lo ? hi - lo : 1.0
    lim_lo = lo - reach * span; lim_hi = hi + reach * span
    # an edge drawn ON the border (the overlay clamps a drag that leaves the plot to it) comes back
    # through the stored transform's invert/apply round trip a hair outside — that is still on-axis,
    # and growing for it would rescale the plot under a gate the user just put exactly there
    tol = 1e-9 * span
    glo = minimum((e for e in edges if isfinite(e) && lim_lo <= e < lo - tol); init = Inf)
    ghi = maximum((e for e in edges if isfinite(e) && hi + tol < e <= lim_hi); init = -Inf)
    nlo = isfinite(glo) ? glo - margin * span : float(lo)
    nhi = isfinite(ghi) ? ghi + margin * span : float(hi)
    (nlo, nhi)
end

# The gating plot's DISPLAY extent for one axis: the data extent `(lo, hi)` widened by `AXIS_PAD` of
# its span on each side (FlowJo's breathing room — a cloud piled against an axis, e.g. every cell at
# 0, otherwise sits ON the border where a gate cannot be started around it), then grown to the
# child-gate `edges` (`_include_edges`). Padding first, so a gate drawn to the padded border is
# already on-axis and the plot does not rescale after it is drawn.
#
# Also returns, per side, whether a gate grew it: the ticks keep labelling the DATA range on an
# unmoved side (the padding is margin, not data — no "-2.9" tick under a cloud that starts at 0), and
# span to the grown edge on a moved one, as before.
const AXIS_PAD = 0.03
function _display_extent((lo, hi)::Tuple, edges; pad::Float64 = AXIS_PAD)
    span = hi > lo ? hi - lo : 1.0
    padded = (lo - pad * span, hi + pad * span)
    ext = _include_edges(padded, edges)
    ext, (ext[1] != padded[1], ext[2] != padded[2])
end

# Every edge coordinate of a plotmeta `gates` list (projected child-gate outlines), per axis:
# rectangles give x_min/x_max/y_min/y_max, polygons and slices their vertices, a quadrant its centre.
# Display coordinates.
function _gate_edges(gates)
    xs = Float64[]; ys = Float64[]
    for pj in gates
        if get(pj, "kind", "") == "rectangle"
            push!(xs, pj["x_min"], pj["x_max"]); push!(ys, pj["y_min"], pj["y_max"])
        elseif get(pj, "kind", "") == "quadrant"
            # the centre only: an arm's stored point just gives its direction, it is not an edge
            push!(xs, pj["centre"][1]); push!(ys, pj["centre"][2])
        elseif get(pj, "kind", "") == "range"
            push!(xs, pj["lo"], pj["hi"])          # 1D: the histogram's y is counts, not a gate axis
        elseif get(pj, "kind", "") == "bisector"
            push!(xs, pj["cut"])
        else
            for v in get(pj, "vertices", ())
                push!(xs, v[1]); push!(ys, v[2])
            end
        end
    end
    xs, ys
end

# The child gates of `pop` drawn on this (x, y) pair, re-projected into the display transforms — each
# a `project_gate` outline plus its `path` and `colour`. Gates on another channel pair are left out.
# Shared by plotmeta (the browser plot) and the gate-plot image (`gating_views_api.jl`).
#
# `parent_raw` = the displayed population's RAW values on the plot's (x, y) channels, when the caller has
# them: a rule gate (a valley-following bisector or range end, a cloud-following shape) is shown where its
# rule puts it on those cells — the same cells `recompute!` resolves it on — and says whether each rule
# found its valley / cloud (`rule_flags`), and a following shape where its peak is (`rule_peak`, display
# coordinates). A gate drawn with the axes swapped reads them swapped.
function _child_gate_outlines(m::PopulationTree, pop, x, y, xt, yt; parent_raw = nothing)
    gates = Dict{String,Any}[]
    for cpath in direct_children(m, pop)
        p = m.pops[cpath]
        p.gate === nothing && continue
        pj = _gate_outline(p.gate, x, y, xt, yt; parent_raw)
        pj === nothing && continue
        pj["path"] = cpath; pj["colour"] = p.colour
        push!(gates, pj)
    end
    gates
end

# One gate's outline on this (x, y) plot, resolved on `parent_raw` (see above); `nothing` off these axes.
function _gate_outline(g::Gate, x, y, xt, yt; parent_raw = nothing)
    flags = nothing
    if Cecelia.needs_parent_data(g) && parent_raw !== nothing
        px, py = parent_raw
        gxy = (g.x_channel, g.y_channel) == (x, y) ? (px, py) :
              (g.x_channel, g.y_channel) == (y, x) ? (py, px) : nothing
        gxy === nothing || ((g, flags) = Cecelia.resolve_with_flags(g, gxy...))
        if flags !== nothing && haskey(flags, "rule_peak")   # gate coordinates → the plot's
            pk = project_point(g, x, y, xt, yt, flags["rule_peak"])
            pk === nothing ? delete!(flags, "rule_peak") : (flags["rule_peak"] = pk)
        end
    end
    pj = project_gate(g, x, y, xt, yt)
    pj === nothing && return nothing
    flags === nothing || merge!(pj, flags)
    pj
end

# ── GET /api/gating/plotmeta ──────────────────────────────────────────────────
# returns n + mode (scatter|density) + transformed extents + axis ticks
function api_gating_plotmeta(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    x = get(q, "x", ""); y = get(q, "y", "")
    (isempty(x) || isempty(y)) && return _gerr(400, "x and y required")
    pop = get(q, "pop", ROOT); pop_type = _read_pop_type(q, img, vn, pop)
    # A track-grained plot (popType track/trackclust) of an untracked segmentation has no data — tell
    # the client to track first (it shows a message) and skip the empty data reads. `tracked` rides on
    # every plotmeta response so the client can distinguish "not tracked" from "genuinely no points".
    if is_track_grained(pop_type) && !is_tracked(img; value_name = vn)
        return 200, JSON3.write((; n = 0, mode = "scatter", tracked = false,
            xExtent = [0.0, 1.0], yExtent = [0.0, 1.0], xLabel = x, yLabel = y,
            xUnit = "", yUnit = "",              # same response shape as the real path below
            xTicks = Dict{String,Any}[], yTicks = Dict{String,Any}[],
            usedX = "linear", usedY = "linear", gates = Dict{String,Any}[],
            xSpec = transform_spec(LinearTransform()), ySpec = transform_spec(LinearTransform()),
            zExtent = nothing, zTicks = Dict{String,Any}[], zUnit = "", usedZ = "linear",
            zSpec = transform_spec(LinearTransform())))
    end
    xt0 = _axis_transform(q, "x"); yt0 = _axis_transform(q, "y")
    # "colour by" (optional): a THIRD measure painted onto the 2D plot as the dot colour. It has no
    # axis — only a range for the colour ramp — so all it needs from here is its transform + extent.
    z = get(q, "z", "")
    # raw extents over the WHOLE dataset (root): used both for ticks (labels read in data units, and
    # selecting a population doesn't rescale the axis) AND to decide auto-linearisation — so the
    # transform choice is stable across populations, not re-decided per subset. Track-aware via _raw.
    pin = _labels_version_pin(q)
    rv = _plot_cols_raw(img, vn, pop_type, isempty(z) ? [x, y] : [x, y, z], ROOT; labels_version = pin)
    rxv, ryv = _xy_pair(rv)
    rxext = _finite_extrema(rxv); ryext = _finite_extrema(ryv)
    xt0 = _auto_params(xt0, q, "x", rxv); yt0 = _auto_params(yt0, q, "y", ryv)
    # A non-linear transform that would collapse a bounded/small-range measure (morphology like
    # solidity ∈ [0,1]) into a sliver is auto-swapped to linear; usedX/usedY tell the client so it can
    # flag the axis. Both plotmeta and plotdata transform with these EFFECTIVE transforms, so the
    # extent and the point cloud always agree. (The client sends its PREFERRED transform; the server
    # decides what's usable — see docs/POPULATION.md.)
    # Opt-in via autoLinear=1 (sent by the single flow plot AND the channel-pairs / gating-strategy
    # montage; other callers keep the requested transform verbatim). usedX/usedY report what was used.
    auto = get(q, "autoLinear", "") == "1"
    xt = auto ? effective_transform(xt0, rxext[1], rxext[2]) : xt0
    yt = auto ? effective_transform(yt0, ryext[1], ryext[2]) : yt0
    # The colour ramp's range is taken over the WHOLE dataset (root), like the ticks: selecting a
    # child population must not re-map the colours, or the same dot changes colour as you walk the
    # tree and two plots of the same measure can't be compared. `nothing` when no colour-by measure
    # (or when it isn't a column on this table — an empty read).
    zt = LinearTransform(); zext = nothing; zticks = Dict{String,Any}[]
    if !isempty(z) && !isempty(rv[3])
        zt0 = _auto_params(_axis_transform(q, "z"), q, "z", rv[3])
        rzext = _finite_extrema(rv[3])
        zt = auto ? effective_transform(zt0, rzext[1], rzext[2]) : zt0
        # robust range (see `_ramp_range`): the ramp is a contrast setting, not an axis
        e = _ramp_range(Float32.(transform_column(zt, rv[3])))
        if e !== nothing
            zext = [e[1], e[2]]
            # the legend's labels are RAW values (inverted through the transform) — served from here for
            # the same reason the axis ticks are: the client has no transform math, so it cannot label a
            # logicle-scaled ramp itself. They describe the CLIPPED range, so the bar states what it
            # actually shows. Three is what a thin colour bar fits.
            zticks = _axis_ticks(zt, invert_transform(zt, e[1]), invert_transform(zt, e[2]); n = 3)
        end
    end
    rxp, ryp = _plot_xy_raw(img, vn, pop_type, x, y, pop; labels_version = pin)
    # `transform_column`, not the scalar map: typed and threaded — a logicle axis solves per value, and
    # this point cloud is the plot's whole population (0.2 s of a 600k-cell plotmeta, single-threaded)
    xv, yv = Float32.(transform_column(xt, rxp)), Float32.(transform_column(yt, ryp))
    n = length(xv)
    density_threshold = parse(Int, get(q, "densityThreshold", "200000"))
    mode = n > density_threshold ? "density" : "scatter"
    xext = _finite_extrema(xv)
    yext = _finite_extrema(yv)
    # x0/y0=1 → "whole dataset" axis: origin at raw 0, upper bound = the FULL dataset max (rxext
    # is computed from all cells, not the pop subset), so selecting a population doesn't rescale
    # the axis (FlowJo behaviour). Autoscale (x0=0) fits the displayed population's extent.
    if get(q, "x0", "") == "1"
        xext = (apply_transform(xt, 0.0), apply_transform(xt, float(rxext[2]))); rxext = (0.0, rxext[2])
    end
    if get(q, "y0", "") == "1"
        yext = (apply_transform(yt, 0.0), apply_transform(yt, float(ryext[2]))); ryext = (0.0, ryext[2])
    end
    # Child gates of the displayed population, re-projected into the EFFECTIVE display transform so
    # their outlines land on the dots even when a gate was drawn under a different transform (the client
    # has no transform math). Colour/path travel so the client renders directly. Membership is untouched.
    gates = _child_gate_outlines(load_pop_tree(img; value_name = vn, pop_type = pop_type), pop, x, y, xt, yt;
                                 parent_raw = (rxp, ryp))
    # Autoscale to the child gates: a gate edge drawn just beyond the data cloud would otherwise fall
    # outside the axes and be un-grabbable, so grow the display extent to it and re-derive the raw tick
    # range so ticks span the whole axis. Only the side an edge exceeds moves, and only for an edge
    # near the data — a far one is an open-ended threshold and is clipped (see `_include_edges`).
    # The axes are padded past the data (`_display_extent`) so edge cells are inside the plot.
    gx, gy = _gate_edges(gates)
    xext, xgrew = _display_extent(xext, gx); yext, ygrew = _display_extent(yext, gy)
    rxext = (xgrew[1] ? invert_transform(xt, xext[1]) : rxext[1], xgrew[2] ? invert_transform(xt, xext[2]) : rxext[2])
    ryext = (ygrew[1] ? invert_transform(yt, yext[1]) : ryext[1], ygrew[2] ? invert_transform(yt, yext[2]) : ryext[2])
    200, JSON3.write((;
        n = n, mode = mode, tracked = true,
        xExtent = [xext[1], xext[2]], yExtent = [yext[1], yext[2]],
        xLabel = x, yLabel = y,
        xUnit = _axis_unit(img, vn, pop_type, x), yUnit = _axis_unit(img, vn, pop_type, y),
        xTicks = _axis_ticks(xt, rxext[1], rxext[2]),
        yTicks = _axis_ticks(yt, ryext[1], ryext[2]),
        usedX = transform_kind(xt), usedY = transform_kind(yt),
        # the transforms with every parameter (incl. the data-derived ones, `_auto_params`) — what the
        # client sends to plotdata and stores in a gate drawn on these axes
        xSpec = transform_spec(xt), ySpec = transform_spec(yt), zSpec = transform_spec(zt),
        gates = gates,
        # colour-by: the ramp's [lo, hi] in TRANSFORMED space (plotdata sends transformed values), the
        # unit for the legend, and the transform actually used — same auto-linearisation as an axis.
        zExtent = zext, zTicks = zticks,
        zUnit = isempty(z) ? "" : _axis_unit(img, vn, pop_type, z),
        usedZ = transform_kind(zt),
    ))
end

# ── GET /api/gating/plotdata → binary Float32 [x0,y0,x1,y1,...] (transformed) ──
#
# BIDIR PR #4b Slice B — opt-in `withLabels=1` appends the point's label column (cell label_id for
# `flow`/`live`, track_id for `track`) to every record so a per-point subscription to
# trackHighlight/pickHighlight (Decision 19) can find the highlighted labels without a second
# round-trip. Layout with labels: pairs → [x, y, label] (3 f32); triples → [x, y, z, label] (4 f32).
# Old callers get the untouched 2/3-float layout. Small ints stay exact in Float32; the frontend
# knows the grain from the popType it sent.
function api_gating_plotdata(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    x = get(q, "x", ""); y = get(q, "y", "")
    (isempty(x) || isempty(y)) && return _gerr(400, "x and y required")
    pop = get(q, "pop", ROOT); pop_type = _read_pop_type(q, img, vn, pop)
    # the GUI echoes plotmeta's full spec (no read here); a caller sending a bare kind gets the same
    # data-derived parameters plotmeta's axes used, not dots in a different transform under them
    xt, yt = _auto_axes(q, img, vn, pop_type, x, y, _axis_transform(q, "x"), _axis_transform(q, "y");
                        labels_version = _labels_version_pin(q))
    # `z` (optional) = the colour-by measure: the response becomes TRIPLES [x,y,z,…] instead of pairs,
    # read in the SAME pass as x/y so each z belongs to its own dot (`_plot_cols_raw`). The client
    # switches stride on whether it asked for z, so old callers are untouched.
    z = get(q, "z", "")
    pin = _labels_version_pin(q)
    with_labels = get(q, "withLabels", "") == "1"
    # Companion label read — same read as x/y (`_plot_cols_raw`, table order), so labels align 1:1
    # with the x/y vectors.
    labs = with_labels ? first(_plot_cols_raw(img, vn, pop_type, ["label"], pop; labels_version = pin)) :
                         Float64[]
    if isempty(z)
        xv, yv = _plot_xy(img, vn, pop_type, x, y, pop, xt, yt; labels_version = pin)
        n = length(xv)
        stride = with_labels ? 3 : 2
        buf = Vector{Float32}(undef, stride*n)
        @inbounds for i in 1:n
            buf[stride*(i-1) + 1] = xv[i]; buf[stride*(i-1) + 2] = yv[i]
            with_labels && (buf[stride*(i-1) + 3] = Float32(labs[i]))
        end
        return 200, collect(reinterpret(UInt8, buf))
    end
    zt = _axis_transform(q, "z")
    _needs_auto(zt, q, "z") &&
        (zt = _auto_params(zt, q, "z", first(_plot_cols_raw(img, vn, pop_type, [z], ROOT; labels_version = pin))))
    xv, yv, zv = _plot_xyz(img, vn, pop_type, x, y, z, pop, xt, yt, zt; labels_version = pin)
    n = length(xv)
    stride = with_labels ? 4 : 3
    buf = Vector{Float32}(undef, stride*n)
    @inbounds for i in 1:n
        buf[stride*(i-1) + 1] = xv[i]; buf[stride*(i-1) + 2] = yv[i]; buf[stride*(i-1) + 3] = zv[i]
        with_labels && (buf[stride*(i-1) + 4] = Float32(labs[i]))
    end
    200, collect(reinterpret(UInt8, buf))
end

# ── GET /api/plots/cluster-map → binary Float32 [x, y, clusterCode, popIdx] per point ─
# `popIdx` = index (0-based) into the requested `colourPops` list that this point belongs to (-1 = in
# none / not requested), so the frontend can colour/facet the embedding by the individually-tracked
# populations (docs/todo/UMAP_COLOUR_FACET_PLAN.md). `code` (cluster) is always present too.
# The UMAP scatter for a clustering pop_type: the `obsm['X_umap.{suffix}']` embedding + the
# `clusters.{suffix}` code per point (frontend colours by code). `clust` reads the cell table;
# `trackclust` reads the per-track table (one point per track). Optionally subset to a population's
# membership (`pop`). Cluster codes are small ints → exact in Float32; an unclustered row → -1.
#
# BIDIR PR #4b Slice B — opt-in `withLabels=1` extends each record to 5 floats
# [x, y, code, popIdx, label] so a per-point subscription to trackHighlight/pickHighlight (Decision
# 19) can find the labels its bag names without a second round-trip. `label` is `cdf.label` (the
# same column pop-membership resolution reads): for `trackclust` = track_id, for `clust` = cell
# label_id. Old callers get the 4-float layout untouched. Small ints stay exact in Float32.
#
# Opt-in `colourFeature=<column>` appends one more float per point: that column's value (NaN where the
# point has none) — a feature the run clustered on (`live.cell.speed`, a marker by channel NAME, an HMM
# frequency), so the UMAP can be coloured by it. Read per segmentation through the label_props view
# with the image's channel names, so a marker name resolves to this segmentation's own column.
# The obsm key a cluster-map layout reads, or `nothing` for an unknown basis (`CLUSTER_MAP_BASES`).
_cluster_map_key(basis, suffix) = basis in Cecelia.CLUSTER_MAP_BASES ? "X_$(basis).$(suffix)" : nothing

# One `colourFeature` value per obs row of `path` (obs order, like the embedding), NaN where a row has
# none — or for every row when this segmentation lacks the column or its name is ambiguous here.
function _cluster_map_values(img, vn, path, feature, n)::Vector{Float32}
    df = try
        label_props(path; channel_names = channel_names(img; value_name = vn), value_name = vn) |>
            v -> select_cols(v, [feature]) |> as_df
    catch
        return fill(NaN32, n)
    end
    feature in names(df) || return fill(NaN32, n)
    Float32[(v isa Real && !ismissing(v)) ? Float32(v) : NaN32 for v in df[!, feature]]
end

# Append one segmentation's records to `out`. A function barrier: the DataFrame columns arrive untyped
# in the handler, and packing 600k rows through them there cost ~0.3 s of dynamic dispatch.
function _pack_cluster_map!(out::Vector{Float32}, stride::Int, idx::Vector{Int}, xy::Matrix{Float64},
                            codes::AbstractVector, labels::AbstractVector, vn_pop::Dict{Int,Int},
                            vals::Vector{Float32}, with_labels::Bool, with_value::Bool)
    base = length(out); resize!(out, base + stride * length(idx))
    @inbounds for (j, i) in enumerate(idx)
        o = base + stride * (j - 1)
        out[o + 1] = Float32(xy[i, 1])
        out[o + 2] = Float32(xy[i, 2])
        c = codes[i]
        out[o + 3] = (c isa Number && !ismissing(c)) ? Float32(c) : -1f0
        out[o + 4] = isempty(vn_pop) ? -1f0 : Float32(get(vn_pop, Int(labels[i]), -1))
        with_labels && (out[o + 5] = Float32(labels[i]))
        with_value && (out[o + stride] = vals[i])
    end
    out
end

function api_plots_cluster_map(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    # The embedding, the codes and the `pop`/`colourPops` membership below all read `_latest`; a
    # labels pin can't be honoured without reading every one of them at the pinned version. Refuse it
    # rather than return a map whose membership silently ignores the pin the caller asked for.
    _labels_version_pin(q) === nothing ||
        return _gerr(400, "labelsVersion is not supported on /api/plots/cluster-map (it reads _latest)")
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    pop_type = get(q, "popType", "clust")
    suffix   = get(q, "suffix", "default")
    pop      = get(q, "pop", ROOT)
    # the code column is the pop_type's OWN family (`clusters.{suffix}` | `regions.{suffix}`) — see
    # `_cluster_suffixes`. Hardcoding "clusters." here left every region UMAP point uncoloured.
    # `basis` picks the layout: the UMAP, or a track run's diffusion axes (DC1, DC2 —
    # docs/todo/TRACK_BEHAVIOUR_AXES_PLAN.md). Both are (n_obs, 2) obsm matrices in obs order.
    basis = get(q, "basis", "umap")
    emb_key = _cluster_map_key(basis, suffix)
    emb_key === nothing && return _gerr(400, "basis must be one of $(join(Cecelia.CLUSTER_MAP_BASES, ", "))")
    clust_col = "$(Cecelia._cluster_measure_prefix(pop_type))$(suffix)"
    track    = is_track_grained(pop_type)
    # POOL across every segmentation that took part in this clustering run (co-clustered value_names),
    # so the UMAP shows all segments' points in the ONE shared embedding — not just the active one
    # (docs/todo/CLUSTER_POOLING_PLAN.md). An explicit `valueName` restricts to that single segmentation.
    req_vn = get(q, "valueName", "")
    vns = (!isempty(req_vn) && haskey(img.label_props, req_vn)) ? String[String(req_vn)] :
          co_clustered_value_names(img, suffix; granularity = track ? :track : :cell,
                                   family = Cecelia._cluster_measure_family(pop_type))

    # Optional per-point POPULATION membership for colour/facet-by-population: resolve the picked pops
    # through pop_df — the canonical accessor already handles GRAIN (granularity=:track rolls a `live`
    # cell pop up to its track_ids so it aligns with a trackclust embedding), value_name pooling, and
    # derived pops. Each token is "popType~valueNamePrefixedPath" (e.g. "live~B/qc/_tracked"); popIdx =
    # its 0-based index in the ordered list, so a pop keeps one colour across segments. A point → the
    # FIRST picked pop it belongs to, else -1 (input pops are normally disjoint). See
    # docs/todo/UMAP_COLOUR_FACET_PLAN.md.
    colour_toks = _query_list(q, "colourPops")
    grain = track ? :track : :cell
    pop_of = pop_index_by_label(img, colour_toks, pop_type; granularity = grain)   # (value_name, label) → popIdx

    with_labels = get(q, "withLabels", "") == "1"
    feature = String(get(q, "colourFeature", ""))
    with_value = !isempty(feature)
    stride = 4 + with_labels + with_value
    out = Float32[]   # [x, y, code, popIdx (, label) (, value)] tuples, concatenated across the co-clustered segmentations
    for vn in vns
        path = track ? img_track_props_path(img, vn) : img_label_props_path(img, vn)
        isfile(path) || continue
        xy = obsm(label_props(path), emb_key)          # (n_obs, 2), obs order
        size(xy, 1) == 0 && continue
        # cluster codes aligned to obs order (label + the code column, same obs order as obsm)
        cdf = label_props(path) |> select_cols([clust_col]) |> as_df
        # `size(cdf, 1)`, not `nrow` — DataFrames is NOT imported in the api scripts, so the fallback
        # branch used to throw `UndefVarError: nrow`. It fires exactly when the run's code column is
        # absent (an embedding with no codes), which is how it stayed hidden until region runs hit it.
        codes = clust_col in names(cdf) ? cdf[!, clust_col] : fill(missing, size(cdf, 1))
        keep = trues(size(xy, 1))
        if !is_root(pop)
            m = _live_map(img, vn, pop_type)
            has_pop(m, pop) || continue                 # this segment lacks the pop → contributes nothing
            sel  = Set(cells_in_pop(m, pop))
            keep = BitVector(l in sel for l in cdf.label)
        end
        # drop rows with no embedding — cells/tracks outside the clustered set have NaN UMAP coords
        # (and NaN code); they'd otherwise render as a stray "NaN" cluster.
        @inbounds for i in 1:size(xy, 1)
            (isfinite(xy[i, 1]) && isfinite(xy[i, 2])) || (keep[i] = false)
        end
        vals = with_value ? _cluster_map_values(img, vn, path, feature, size(xy, 1)) : Float32[]
        # per-point popIdx from the resolved (value_name, label) → popIdx map (colour/facet-by-population),
        # narrowed to this segmentation once so the per-row lookup is a plain Int key
        vn_pop = Dict{Int,Int}(l => k for ((v, l), k) in pop_of if v == vn)
        _pack_cluster_map!(out, stride, findall(keep), xy, codes, cdf.label, vn_pop, vals, with_labels, with_value)
    end
    isempty(out) && return _gerr(404, basis == "diffmap" ? "No $emb_key — a track run writes it; re-run clustering" :
                                                         "No $emb_key — run clustering with UMAP enabled")
    200, collect(reinterpret(UInt8, out))
end

# ── GET /api/plots/pop-positions → every cell at its position, tagged by population ──────────────────
# ?projectUid&imageUid&valueName&pops=popType~vn/path,…[&popType=<default for a bare path>][&colourBy=<col>][&meta=1]
# The read behind the `popPositions` plot (`pop_positions`, app/src/plotting/pop_positions.jl).
#   meta=1 → JSON { n, unit, extents{xMin,xMax,yMin,yMax}, xTicks, yTicks, frames:[t0,t1], valueNames,
#            valueRange, valueTicks } — the axes, over the same cells as the data (the populations decide
#            the segmentations); `valueRange` the colourBy scale (a column's own, `fixed_colour_range`,
#            else its finite min–max here; null without colourBy or values)
#   else   → binary Float32 [x, y, t, popIdx, value] per cell; popIdx = index into `pops`, -1 = in none;
#            value = the colourBy column, NaN = none
function api_plots_pop_positions(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    # the segmentations come from the populations (`pop_positions`); `valueName` only names the one to
    # draw when none are picked, and is NOT swapped for the active one when this image lacks it — that
    # would draw another segmentation's cells under populations that are not on it
    cby = get(q, "colourBy", "")
    r = pop_positions(img, get(q, "valueName", ""), _query_list(q, "pops");
                      default_pop_type = get(q, "popType", "flow"), colour_by = cby)
    if get(q, "meta", "") == "1"
        xr, yr = _finite_extrema(r.x), _finite_extrema(r.y)
        tr = isempty(r.t) ? (0.0, 0.0) : _finite_extrema(r.t, (0.0, 0.0))
        lin = Cecelia.LinearTransform()
        vr = isempty(cby) || !any(isfinite, r.value) ? nothing :
             something(Cecelia.fixed_colour_range(cby), _finite_extrema(r.value))
        return 200, JSON3.write((; n = length(r.x), unit = r.unit == SPATIAL_UNIT_UM ? "µm" : r.unit,
            extents = (; xMin = xr[1], xMax = xr[2], yMin = yr[1], yMax = yr[2]),
            xTicks = _axis_ticks(lin, xr...), yTicks = _axis_ticks(lin, yr...),
            frames = [tr[1], tr[2]], valueNames = unique(r.value_name),
            valueRange = vr === nothing ? nothing : collect(vr),
            valueTicks = vr === nothing ? [] : _axis_ticks(lin, vr...)))
    end
    n = length(r.x)
    buf = Vector{Float32}(undef, 5n)
    @inbounds for i in 1:n
        buf[5i - 4] = r.x[i]; buf[5i - 3] = r.y[i]; buf[5i - 2] = r.t[i]; buf[5i - 1] = Float32(r.pop[i])
        buf[5i] = r.value[i]
    end
    200, collect(reinterpret(UInt8, buf))
end

# ── GET /api/gating/density → binary Float32 grid (bins×bins, row-major) ──────
function api_gating_density(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    x = get(q, "x", ""); y = get(q, "y", "")
    (isempty(x) || isempty(y)) && return _gerr(400, "x and y required")
    bins = parse(Int, get(q, "bins", "256"))
    pop = get(q, "pop", ROOT)
    xt, yt = _auto_axes(q, img, vn, _read_pop_type(q, img, vn, pop), x, y, _axis_transform(q, "x"), _axis_transform(q, "y");
                        labels_version = _labels_version_pin(q))
    xv, yv = _plot_xy(img, vn, _read_pop_type(q, img, vn, pop), x, y, pop, xt, yt;
                       labels_version = _labels_version_pin(q))
    d = density_2d(xv, yv; bins = bins)
    200, collect(reinterpret(UInt8, Float32.(vec(d.counts))))
end

# ── GET /api/gating/summary → the distribution a gate is chosen from (JSON) ───
# ?projectUid&imageUid&valueName&x&y[&pop&popType&bins&xt…&yt…] → { n, x:{channel, …axis_summary},
# y:{…}, grid?:grid_summary } over the SAME read as the gate plot (`_plot_xy`), so the numbers
# describe the cloud the plot shows, in the coordinates a gate is written in. `grid` only when x ≠ y.
function api_gating_summary(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    x = get(q, "x", ""); y = get(q, "y", "")
    (isempty(x) || isempty(y)) && return _gerr(400, "x and y required")
    pop = get(q, "pop", ROOT); pop_type = _read_pop_type(q, img, vn, pop)
    bins = clamp(something(tryparse(Int, get(q, "bins", "30")), 30), 5, 80)
    pin = _labels_version_pin(q)           # the session pin, threaded like plotdata/density
    xt, yt = _auto_axes(q, img, vn, pop_type, x, y, _axis_transform(q, "x"), _axis_transform(q, "y");
                        labels_version = pin)
    xv, yv = _plot_xy(img, vn, pop_type, x, y, pop, xt, yt; labels_version = pin)
    if isempty(xv)
        # nothing read: a column the table lacks, a population that doesn't exist, or an empty one
        # (which is an answer, n = 0). The whole-segmentation read tells the first apart.
        columns_ok = !is_root(pop) && !isempty(_plot_xy(img, vn, pop_type, x, y, ROOT, xt, yt; labels_version = pin)[1])
        columns_ok || return _gerr(400, "No values for $x / $y on $vn — " * _missing_column_hint(img, vn, (x, y); labels_version = pin))
        (pm = _live_map(img, vn, pop_type; labels_version = pin); has_pop(pm, pop)) || return _pop_not_found(pm, pop)
    end
    out = Dict{String,Any}(
        "valueName" => vn, "pop" => pop, "n" => count(isfinite, xv),
        "x" => merge(Dict{String,Any}("channel" => x, "transform" => transform_spec(xt)), axis_summary(xv; bins = bins)),
        "y" => merge(Dict{String,Any}("channel" => y, "transform" => transform_spec(yt)), axis_summary(yv; bins = bins)))
    x != y && (out["grid"] = grid_summary(xv, yv))
    200, JSON3.write(out)
end

# ── POST mutations (add / set-gate / delete / rename) ─────────────────────────

# Extract (projectUid, imageUid) from a parsed body without raising `KeyError`. Every gating
# handler in this file went through `_gating_post` first, which already errors 400 when they are
# missing — so the sites that read them again (`_persist_and_broadcast!`, `_history_step`,
# `api_gating_copy`) were `body["projectUid"]` on the belief that the earlier check made a raw
# access safe. It IS safe today, but two different access shapes for the same fields is exactly
# the drift risk the audit flagged: a hand-rolled call site that skips `_gating_post` gets a bare
# stack-traced `KeyError` instead of the 400 the rest of the API returns. One helper for both.
function _require_ids(body::AbstractDict)
    pu = _wstr(body, "projectUid")
    iu = _wstr(body, "imageUid")
    isempty(pu) && return (nothing, nothing, _gerr(400, "projectUid required"))
    isempty(iu) && return (nothing, nothing, _gerr(400, "imageUid required"))
    (pu, iu, nothing)
end

# parse + resolve common fields from a POST body
function _gating_post(body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes), Dict{String,Any})
    catch
        return (nothing, nothing, nothing, nothing, _gerr(400, "Invalid JSON body"))
    end
    img, err = _gating_image(_wstr(body, "projectUid"), _wstr(body, "imageUid"))
    err === nothing || return (nothing, nothing, nothing, body, err)
    vn = _resolve_vn(img, _wstr(body, "valueName"))
    pt = _wstr(body, "popType", "flow")
    (img, vn, pt, body, nothing)
end

# Where a population added with no `parent` goes → (parent, nothing) or (nothing, the refusal): under the
# population a refine run refines when the new population picks from that run (CLUSTER_REFINE_PLAN
# Decision 4), else the root. 404 when this image lacks that population — the GUI replays one add to every
# image of the run, and a root-level copy here would leave the paths out of step (as add-cluster-pops).
function _default_pop_parent(img, vn, pt, flt)
    fm = flt isa AbstractDict ? get(flt, "measure", nothing) : nothing
    prefix = Cecelia._cluster_measure_prefix(pt)
    (pt == "clust" && fm isa AbstractString && startswith(fm, prefix)) || return (ROOT, nothing)
    run = fm[ncodeunits(prefix)+1:end]
    src = refine_source_pop(img, vn, run)
    src === nothing && return (ROOT, nothing)
    has_pop(load_pop_tree(img; value_name = vn, pop_type = pt, backfill_save = false), src) && return (src, nothing)
    (nothing, _gerr(404, "Run $run refines $src, which this image does not have — re-run the refine on images that do"))
end

function api_gating_pop_add(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    haskey(body, "name") || return _gerr(400, "name required")
    # name-uniqueness guard: reject a name already used by a DIFFERENT pop type in this segmentation, so
    # the mixed-type module picker (which resolves a pop by path alone) is never ambiguous.
    _name = String(body["name"])
    # no parent sent: a refine run's cluster population goes under the population the run refines
    _parent, perr = haskey(body, "parent") ? (_wstr(body, "parent", ROOT), nothing) :
                    _default_pop_parent(img, vn, pt, get(body, "filter", nothing))
    perr === nothing || return perr
    _intended = pop_path(is_root(_parent) ? ROOT : _parent, _name)
    _conflict = pop_name_conflict(img, vn, _intended; pop_type = pt)
    _conflict === nothing || return _gerr(400,
        "A $_conflict population named \"$_name\" already exists in this segmentation — names must be " *
        "unique across population types so a population can be selected unambiguously. Choose another name.")
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = pt)
        # a spec that is no gate (a 1D gate on two channels, crossed quadrant arms) is the caller's 400
        gate = try
            haskey(body, "gate") && body["gate"] !== nothing ? gate_from_spec(_gate_spec_auto(body["gate"], img, vn, pt)) : nothing
        catch e
            return _gerr(400, sprint(showerror, e))
        end
        flt = get(body, "filter", nothing)
        bl = get(body, "boolean", nothing)      # Decision 16: a set operation over other pops
        path = try
            add_pop!(m, String(body["name"]); parent = _parent,
                     gate = gate, colour = _wstr(body, "colour", "#ffffff"),
                     show = _wbool(body, "show", true),
                     filter_measure = flt === nothing ? nothing : get(flt, "measure", nothing),
                     filter_fun     = flt === nothing ? nothing : get(flt, "fun", nothing),
                     filter_values  = flt === nothing ? nothing : get(flt, "values", nothing),
                     filter_default_all = flt === nothing ? false : Bool(get(flt, "default_all", false)),
                     filter_conditions = flt === nothing ? nothing : get(flt, "conditions", nothing),
                     is_track = _wbool(body, "is_track", false),
                     boolean_op   = bl === nothing ? nothing : get(bl, "op", nothing),
                     boolean_pops = bl === nothing ? nothing : get(bl, "pops", nothing),
                     boolean_not  = bl === nothing ? nothing : get(bl, "not", nothing))
        catch e
            return _gerr(400, sprint(showerror, e))
        end
        # a cluster population: its ids are one run's, held by no other population of it (unsaved on refusal)
        taken = _cluster_ids_taken(m, path, m.pops[path].filter_values); taken === nothing || return taken
        stamp_cluster_run!(m.pops[path], img, vn)
        200, JSON3.write(_persist_and_broadcast!(m, img, body, vn, pt))
    end
end

# ── POST /api/gating/pop/add-split — every region of a split gate, as one change ───────────────────
# { projectUid, imageUid, valueName, popType, parent?, gate:{kind: slice|quadrant|bisector, x_channel, y_channel,
#   x_transform, y_transform, vertices | centre + arms | cut + rule?}, pops:[{name, colour, region}, …] }
# → the pop tree, like /pop/add. `region` is the side (slice: 1 left of the line's direction, -1 right;
# bisector: 1 above the cut, -1 at or below) or the quadrant (1–4). Every region is required exactly once, so the populations tile the plane; they share a
# fresh `group` id, which `set_gate!` follows to move them together (app/src/gating/split_gates.jl). One
# request = one undo step; any refusal (a taken name, a bad gate) writes nothing.
function api_gating_pop_add_split(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    spec = get(body, "gate", nothing); pops = get(body, "pops", nothing)
    (spec isa AbstractDict && pops isa AbstractVector) || return _gerr(400, "gate and pops required")
    kind = lowercase(string(get(spec, "kind", "")))
    regions = kind in ("slice", "bisector") ? [1, -1] : kind == "quadrant" ? [1, 2, 3, 4] :
              return _gerr(400, "gate.kind must be \"slice\", \"quadrant\" or \"bisector\"")
    got = Int[]
    for p in pops
        (p isa AbstractDict && haskey(p, "name") && get(p, "region", nothing) isa Integer) ||
            return _gerr(400, "each pop needs a name and an integer region")
        push!(got, Int(p["region"]))
    end
    sort(got) == sort(regions) || return _gerr(400, "a $kind needs one population per region: $(join(regions, ", "))")
    parent = _wstr(body, "parent", ROOT)
    for p in pops
        name = String(p["name"])
        conflict = pop_name_conflict(img, vn, pop_path(is_root(parent) ? ROOT : parent, name); pop_type = pt)
        conflict === nothing || return _gerr(400,
            "A $conflict population named \"$name\" already exists in this segmentation — choose another name.")
    end
    group = gen_uid()
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = pt)
        try
            base = _gate_spec_auto(spec, img, vn, pt)
            for p in pops
                d = Dict{String,Any}(String(k) => v for (k, v) in base)
                d["group"] = group
                d[kind == "quadrant" ? "quadrant" : "side"] = Int(p["region"])
                add_pop!(m, String(p["name"]); parent = parent, gate = gate_from_spec(d),
                         colour = _wstr(p, "colour", "#ffffff"), show = true)
            end
        catch e
            return _gerr(400, sprint(showerror, e))      # nothing persisted: `m` is discarded
        end
        200, JSON3.write(_persist_and_broadcast!(m, img, body, vn, pt))
    end
end

# ── POST /api/gating/pop/add-gate — ONE proposed gate, additive only (observer `add_gate`) ──────
# { projectUid, imageUid, valueName, parent?, name, gate:{kind, x_channel, y_channel, x_transform?,
#   y_transform?, x_min…y_max | vertices}, colour? }
# → { ok, path, valueName, parent, count, parentCount, outside, pctParent, gate, colour, createdBy }
#
# The observer's write into gating, kept apart from /api/gating/pop/add exactly as /api/boards/add is
# kept apart from the boards autosave: that route also takes filters, booleans, track pops and any pop
# type, and answers every refusal with a 400. This one can only ADD one `flow` gate under a parent that
# exists, and refuses with the status the caller acts on — 404 (no such parent / segmentation), 409 (the
# path is taken, here or by another pop type: `pop_name_conflict`), 422 (a column, transform or geometry
# that cannot be a gate) — all BEFORE anything is written. It never edits, moves or deletes a pop.
# Stored through `add_pop!` + `_persist_and_broadcast!`, so the sidecar entry is a GUI gate's (plus
# `createdBy` = `author_stamp()`), the open manager refreshes, and the user's Undo removes it.
function api_gating_pop_add_gate(body_bytes::Vector{UInt8})
    img, vn, body, err = _claude_gate_body(body_bytes); err === nothing || return err
    name, parent, colour, path, err = _claude_new_pop(body); err === nothing || return err
    gate, gerr = _proposed_gate(get(body, "gate", nothing), img, vn)
    gerr === nothing || return _gerr(422, gerr)
    # a split (slice / quadrant / bisector) is one population per region, named `name+` / `name-` (or
    # `name++` … for a quadrant), linked by a fresh group — all added, or none
    split = gate isa Cecelia.SplitGate || gate isa BisectorGate
    regions = split ? [(name * suf, r) for (suf, r) in _proposed_regions(gate)] : [(name, 0)]
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = "flow")
        for (n, _) in regions
            refusal = _claude_place_refusal(m, img, vn, parent, pop_path(parent, n), n)
            refusal === nothing || return refusal
        end
        group = gen_uid()
        for (n, r) in regions
            g = if split
                d = gate_spec(gate); d["group"] = group
                d[gate isa QuadrantGate ? "quadrant" : "side"] = r
                gate_from_spec(d)
            else
                gate
            end
            add_pop!(m, n; parent = parent, gate = g, colour = colour, show = true, created_by = author_stamp())
        end
        split || return _claude_gate_reply(m, img, body, vn, path)
        _claude_gate_reply(m, img, body, vn, pop_path(parent, first(regions)[1]);
                           regions = [pop_path(parent, n) for (n, _) in regions])
    end
end

# The name / parent / colour of a NEW population Claude proposes (add-gate, add-boolean) →
# (name, parent, colour, path, nothing), or four `nothing`s and the 422. Checked before the map is even loaded.
function _claude_new_pop(body)
    name = String(strip(_wstr(body, "name")))
    isempty(name) && return (nothing, nothing, nothing, nothing, _gerr(422, "name required"))
    occursin('/', name) && return (nothing, nothing, nothing, nothing, _gerr(422,
        "A population name cannot contain \"/\" — give the name only, and the parent separately"))
    is_reserved_pop_name(name) && return (nothing, nothing, nothing, nothing, _gerr(422,
        "Names beginning with \"$DERIVED_POP_PREFIX\" are reserved for derived populations — choose another name"))
    parent = let p = strip(_wstr(body, "parent", ROOT)); is_root(p) ? ROOT : String(p) end
    colour = _wstr(body, "colour", "#22c55e")
    _is_hex_colour(colour) || return (nothing, nothing, nothing, nothing, _gerr(422, "colour must be a hex colour like \"#22c55e\""))
    (name, parent, colour, pop_path(parent, name), nothing)
end

# A colour a proposal may carry: "#rrggbb".
_is_hex_colour(c) = c isa AbstractString && occursin(r"^#[0-9a-fA-F]{6}$", c)

# Where a new population would land, on the map in hand: 404 no such parent, 409 the path is taken
# here or by another pop type (`pop_name_conflict`) — an existing population is never replaced.
function _claude_place_refusal(m::PopulationTree, img, vn, parent, path, name)
    (is_root(parent) || has_pop(m, parent)) || return _pop_not_found(m, parent)
    has_pop(m, path) && return 409, JSON3.write((; error = "A population \"$path\" already exists — " *
        "choose another name; an existing population is never replaced", code = "pop_exists"))
    conflict = pop_name_conflict(img, vn, path; pop_type = "flow")
    conflict === nothing || return 409, JSON3.write((; error = "A $conflict population named \"$name\" " *
        "already exists in this segmentation — names must be unique across population types. Choose another name.",
        code = "pop_name_conflict"))
    nothing
end

# ── POST /api/gating/pop/add-boolean — ONE proposed combined population (observer `add_boolean_pop`) ──
# { projectUid, imageUid, valueName, parent?, name, op, pops?, not?, colour?, dryRun? } → add-gate's reply
# with `gate: null` and the stored `boolean: {op, pops, not}`.
#
# add-gate's twin for the third membership kind (POPULATION.md → Boolean populations): "qc but not OTI"
# is `parent=/qc, op=and, not=[/qc/OTI]`, not a polygon with a hole cut in it. Same validation order and
# statuses as add-gate — 404 parent / segmentation / a term that isn't there, 409 a taken path, 422 a
# bad name, operator, or a combination that would depend on itself — all before anything is written,
# and the same `createdBy` stamp, so set-own-gate (422: not a gate) and delete-own treat it as Claude's.
function api_gating_pop_add_boolean(body_bytes::Vector{UInt8})
    img, vn, body, err = _claude_gate_body(body_bytes); err === nothing || return err
    name, parent, colour, path, err = _claude_new_pop(body); err === nothing || return err
    terms(k) = (v = get(body, k, nothing); v === nothing ? String[] :
                v isa AbstractVector ? String[String(x) for x in v] : nothing)
    inc, exc = terms("pops"), terms("not")
    (inc === nothing || exc === nothing) && return _gerr(422, "pops and not are lists of population paths")
    op, inc, exc = try
        Cecelia._normalise_boolean(_wstr(body, "op", "and"), inc, exc; self = path)
    catch e
        return _gerr(422, replace(sprint(showerror, e), r"^boolean pop: " => ""))
    end
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = "flow")
        (is_root(parent) || has_pop(m, parent)) || return _pop_not_found(m, parent)
        for r in [inc; exc]
            has_pop(m, r) || return _pop_not_found(m, r)
        end
        refusal = _claude_place_refusal(m, img, vn, parent, path, name); refusal === nothing || return refusal
        Cecelia.boolean_cycle(m, path, [inc; exc]) && return _gerr(422,
            "That combination would depend on itself — a population cannot combine one of its own descendants")
        add_pop!(m, name; parent = parent, colour = colour, show = true, created_by = author_stamp(),
                 boolean_op = op, boolean_pops = inc, boolean_not = exc)
        _claude_gate_reply(m, img, body, vn, path)
    end
end

# ── POST /api/gating/pop/add-filter — ONE proposed filter population (observer `add_filter_pop`) ──
# { projectUid, imageUid, valueName, parent?, name, conditions:[{measure, fun, values}, …], colour?, dryRun? }
# → add-gate's reply with `gate: null` and the stored `filter` (`filter_spec`).
#
# add-gate's twin for the second membership kind (POPULATION.md → User-defined filter populations): "area
# > 50 and track speed > 5" or "cluster 3 or 7 of the movement run" is AND-ed column rules, not a box drawn
# around the cut. Same validation order and statuses as add-gate — 404 parent / segmentation, 409 a taken
# path, 422 a bad name, operator, value or a column the cells don't have — all before anything is written,
# and the same `createdBy` stamp, so set-own-gate (422: not a gate) and delete-own treat it as Claude's.
function api_gating_pop_add_filter(body_bytes::Vector{UInt8})
    img, vn, body, err = _claude_gate_body(body_bytes); err === nothing || return err
    name, parent, colour, path, err = _claude_new_pop(body); err === nothing || return err
    conds, cerr = _proposed_conditions(get(body, "conditions", nothing), img, vn)
    cerr === nothing || return _gerr(422, cerr)
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = "flow")
        refusal = _claude_place_refusal(m, img, vn, parent, path, name); refusal === nothing || return refusal
        add_pop!(m, name; parent = parent, colour = colour, show = true, created_by = author_stamp(),
                 filter_conditions = conds)
        _claude_gate_reply(m, img, body, vn, path)
    end
end

# The AND-ed conditions of a proposed filter pop → (Vector{FilterCondition}, nothing) or (nothing, the 422
# text). `in` takes a non-empty list; every other operator ONE value, a number for gt/gte/lt/lte. A string
# that parses as a number becomes that number, as the manager's form does (`parseFilterValues`): "3" against
# a numeric cluster column would otherwise never be equal, so `neq` would keep every cell. Each
# column must be one membership reads (`pop_membership_cols`): a misspelt one would otherwise store and
# then evaluate to an empty population with only a server-log warning.
function _proposed_conditions(spec, img, vn)
    (spec isa AbstractVector && !isempty(spec)) ||
        return (nothing, "conditions required: [{measure, fun, values}, …] — AND-ed, at least one")
    scalar(v) = (v isa Real && !(v isa Bool) && isfinite(v)) || (v isa AbstractString && !isempty(v))
    num(v) = v isa AbstractString ? something(tryparse(Float64, strip(v)), String(v)) : v
    funs = join(sort!(collect(values(Cecelia._FILTER_FUN_STR))), ", ")
    out = FilterCondition[]
    for (i, c) in enumerate(spec)
        c isa AbstractDict || return (nothing, "condition $i must be {measure, fun, values}")
        g(k) = get(c, k, nothing)
        ms = g("measure")
        (ms isa AbstractString && !isempty(ms)) || return (nothing, "condition $i: measure required")
        fun = try
            Cecelia.parse_filter_fun(string(something(g("fun"), "")))
        catch
            return (nothing, "condition $i: fun must be one of $funs")
        end
        v = g("values")
        if fun === FILTER_IN
            (v isa AbstractVector && !isempty(v) && all(scalar, v)) ||
                return (nothing, "condition $i: \"in\" takes a non-empty list of values, e.g. [3, 7]")
            v = Any[num(x) for x in v]
        else
            v isa AbstractVector && length(v) == 1 && (v = v[1])
            scalar(v) || return (nothing, "condition $i: \"$fun\" takes one value, not a list")
            v = num(v)
            fun in (FILTER_EQ, FILTER_NEQ) || v isa Real ||
                return (nothing, "condition $i: \"$fun\" compares with a number")
        end
        push!(out, FilterCondition(String(ms), fun, v))
    end
    wanted = unique(String[c.measure for c in out])
    missing_cols = setdiff(wanted, pop_membership_cols(img, vn, "flow", wanted))
    isempty(missing_cols) || return (nothing, _missing_column_hint(img, vn, Tuple(missing_cols)))
    (out, nothing)
end

# ── POST /api/gating/pop/add-cluster-pops — Claude's grouping of a clustering run (observer `add_cluster_pops`) ──
# { projectUid, imageUid, valueName, popType: clust|trackclust|region, run, groups:[{name, clusters:[ids],
#   colour?}, …], dryRun? }
# → { ok, dryRun, popType, run, valueName, images, skipped, populations:[{path, name, clusters, colour, count}],
#     total, unassigned, createdBy }
#
# Grouping is what cluster populations are FOR: a run gives tens of clusters, and a population is several
# of them ("clusters 2, 5 and 9 are all T cells"). So one request carries the whole grouping — every group a
# cluster population (a filter `{prefix}{run} in [ids]`, the shape the manager's ticks write) at the root,
# or under the population the run refines for a refine run (CLUSTER_REFINE_PLAN Decision 4),
# stamped `createdBy` and with the run's `at` (`stamp_cluster_run!`). Written to EVERY image of the run
# (`partOf`) on the same segmentation, as the cluster page's set-wide replay does, so the run's pages agree.
# All or nothing: 404 no such run, 409 a name that is taken (`pop_exists` / `pop_name_conflict`) or a
# cluster another population of the run already holds (`cluster_taken` — a cluster belongs to one
# population), 422 a bad name / colour / id, two groups sharing a cluster or a name — on any image of the
# run, before anything is written. `dryRun` = the same reply, nothing written. `unassigned` = the run's
# clusters no population holds after this, so a partial grouping says what is left.
function api_gating_pop_add_cluster_pops(body_bytes::Vector{UInt8})
    img, vn, body, err = _claude_gate_body(body_bytes); err === nothing || return err
    rq, err = _cluster_run_request(body, img, vn); err === nothing || return err
    (; pt, run, targets, skipped, universe) = rq
    groups, gerr = _proposed_cluster_groups(get(body, "groups", nothing), run, universe)
    gerr === nothing || return gerr
    measure = Cecelia._cluster_measure_prefix(pt) * run
    stamp = author_stamp()
    dry = _wbool(body, "dryRun", false)
    _with_pop_tree_lock() do
        maps = [(u, ti, load_pop_tree(ti; value_name = vn, pop_type = pt)) for (u, ti) in targets]
        # a refine run's groups go under the population it refines (CLUSTER_REFINE_PLAN Decision 4)
        # — its recorded path, so an image (the anchor too) without it is refused below, never rooted
        src = pt == "clust" ? refine_source_pop(maps[1][2], vn, run) : nothing
        parent = something(src, ROOT)
        groups = [merge(gr, (; path = pop_path(parent, gr.name))) for gr in groups]
        # every image's map is checked before any is changed — all or nothing
        for (u, ti, m) in maps
            (is_root(parent) || has_pop(m, parent)) || return _gerr(404,
                "Run $run refines $parent, which image $u does not have — re-run the refine on images that do")
            held = cluster_id_owners(m, run)
            for gr in groups
                refusal = _cluster_name_refusal(m, ti, vn, pt, gr.path, u); refusal === nothing || return refusal
                refusal = _cluster_taken_refusal(held, gr.clusters, u); refusal === nothing || return refusal
            end
        end
        n0 = count(p -> cluster_pop_suffix(p) == run, values(maps[1][3].pops))
        colours = [gr.colour === nothing ? POPULATION_PALETTE[mod1(n0 + i, length(POPULATION_PALETTE))] : gr.colour
                   for (i, gr) in enumerate(groups)]
        for (u, ti, m) in maps
            for (gr, colour) in zip(groups, colours)
                add_pop!(m, gr.name; parent = parent, colour = colour, show = true, created_by = stamp,
                         filter_measure = measure, filter_fun = "in", filter_values = gr.clusters)
                stamp_cluster_run!(m.pops[gr.path], ti, vn)
            end
        end
        saved = _save_cluster_maps!(maps, body, vn, pt); saved === nothing || return saved
        counts, total = _cluster_pop_counts!(maps, [gr.path for gr in groups], vn, pt)
        held = keys(cluster_id_owners(maps[1][3], run))
        200, JSON3.write((; ok = true, dryRun = dry, popType = pt, run, valueName = vn,
            images = [u for (u, _, _) in maps], skipped,
            populations = [(; path = gr.path, name = gr.name, clusters = gr.clusters, colour = c, count = n)
                           for (gr, c, n) in zip(groups, colours, counts)],
            total, unassigned = sort!(collect(setdiff(universe, held))),
            runAt = maps[1][3].pops[groups[1].path].run_at, createdBy = stamp))
    end
end

# The run a cluster-pops request names, resolved on `img`'s `vn` → ((; pt, run, targets, skipped, universe),
# nothing) or (nothing, the refusal): 422 a pop type that is not a cluster one or no run, 404 a run `vn`
# does not have. `targets` / `skipped` = `_cluster_run_images`; `universe` = the run's cluster ids.
function _cluster_run_request(body, img::CciaImage, vn::AbstractString)
    proj = _wstr(body, "projectUid")
    pt = _wstr(body, "popType", "clust")
    Cecelia._is_cluster_pop_type(pt) ||
        return (nothing, _gerr(422, "popType must be a cluster type: $(join(CLUSTER_POP_TYPES, ", "))"))
    run = String(strip(_wstr(body, "run")))
    isempty(run) && return (nothing, _gerr(422, "run required — the clustering run's name (`{run}` in clusters.{run})"))
    fam = Cecelia._cluster_measure_family(pt)
    ppath = cluster_props_path(img, vn, pt)
    runs = sort!(collect(Cecelia._clustfeatures_suffixes(ppath; family = fam)))
    run in runs || return (nothing, _gerr(404, "No $pt run \"$run\" on $vn" *
        (isempty(runs) ? " — it has not been clustered" : " — it has: " * join(runs, ", "))))
    targets, skipped = _cluster_run_images(proj, img, vn, pt, run)
    universe = Set{Int}(get(_cluster_ids(proj, ppath, vn, [run], Dict{String,Any}(run => [u for (u, _) in targets]),
                                         is_track_grained(pt), pt), run, Int[]))
    ((; pt, run, targets, skipped, universe), nothing)
end

# Write each (uid, image, map) of a run — unless the request is a `dryRun` — → nothing, or the refusal
# `_persist_and_broadcast!` gave.
function _save_cluster_maps!(maps, body, vn, pt)
    _wbool(body, "dryRun", false) && return nothing
    for (u, ti, m) in maps
        saved = _persist_and_broadcast!(m, ti, merge(body, Dict{String,Any}("imageUid" => u)), vn, pt)
        saved isa Tuple && return saved
    end
    nothing
end

# Each of `paths`' count summed over the run's maps, and the run's total (cells or tracks), after one
# recompute per map — the numbers a cluster-pops reply gives, written or dry.
function _cluster_pop_counts!(maps, paths::AbstractVector, vn, pt)
    counts = zeros(Int, length(paths)); total = 0
    for (_, ti, m) in maps
        recompute!(m, pop_membership_fetch(ti, vn, pt))
        for (i, path) in enumerate(paths)
            s = pop_stats(m, path)
            counts[i] += s.count
            i == 1 && (total += s.parent_count)
        end
    end
    (counts, total)
end

# ── POST /api/gating/pop/set-cluster-pops — change Claude's own grouping of a run (observer `set_cluster_pops`) ──
# { projectUid, imageUid, valueName, popType, run, groups:[{path, clusters?, name?, colour?}, …], dryRun? }
# → add-cluster-pops' reply, `populations` at their (new) paths, plus `updatedBy`.
#
# set-own-gate's twin for cluster populations: each group names a population of the run Claude made
# (`path`) and what changes — its WHOLE cluster list (re-picked against the run as it is now, so the run
# stamp moves and a stale warning clears), its name, its colour. The grouping is checked as it will be
# AFTER the call, so moving a cluster between two of Claude's populations is one call. On every image of the
# run, before anything is written: 404 a path the map lacks, 422 one that is not a population of this run
# / two groups on one path or one new name / a bad name, colour or id / a group that changes nothing,
# 403 `not_claude_made` / `used_by_runs` (set-own-gate's rules), 409 a taken name or a cluster another
# population holds (`cluster_taken` — ids the population already had are not re-checked).
function api_gating_pop_set_cluster_pops(body_bytes::Vector{UInt8})
    img, vn, body, err = _claude_gate_body(body_bytes); err === nothing || return err
    rq, err = _cluster_run_request(body, img, vn); err === nothing || return err
    (; pt, run, targets, skipped, universe) = rq
    edits, eerr = _cluster_pop_edits(get(body, "groups", nothing), run, universe)
    eerr === nothing || return eerr
    # only a population whose clusters this call replaces gives its old ones up; one renamed or recoloured
    # keeps them, so they stay held
    regrouped = Set(e.path for e in edits if e.clusters !== nothing)
    _with_pop_tree_lock() do
        maps = [(u, ti, load_pop_tree(ti; value_name = vn, pop_type = pt)) for (u, ti) in targets]
        for (u, ti, m) in maps
            for e in edits
                has_pop(m, e.path) || return _pop_not_found(m, e.path)
                cluster_pop_suffix(m.pops[e.path]) == run ||
                    return _gerr(422, "\"$(e.path)\" is not a population of $pt run \"$run\"")
                # a `neq`/`gt` population doesn't hold its values: an id list written into it keeps the fun
                # and would hold every cluster BUT those (Cecelia._claims_values)
                (e.clusters === nothing || Cecelia._claims_values(m.pops[e.path])) ||
                    return _gerr(422, "\"$(e.path)\" is a filter population, not a set of clusters — its clusters can't be regrouped")
                sub, refusal = _own_subtree(m, e.path); refusal === nothing || return refusal
                combos, refusal = _own_combinations(m, e.path, sub); refusal === nothing || return refusal
                refusal = _unread_subtree(ti, vn, [sub; combos]); refusal === nothing || return refusal
                e.name === nothing && continue
                newpath = pop_path(m.pops[e.path].parent, e.name)
                newpath == e.path && continue
                refusal = _cluster_name_refusal(m, ti, vn, pt, newpath, u); refusal === nothing || return refusal
            end
            held = cluster_id_owners(m, run; except = regrouped)
            for e in edits
                e.clusters === nothing && continue
                refusal = _cluster_taken_refusal(held, e.clusters, u; had = _cluster_ids_of(m.pops[e.path]))
                refusal === nothing || return refusal
            end
        end
        paths = String[]
        for (u, ti, m) in maps
            empty!(paths)
            for e in edits
                p = m.pops[e.path]
                if e.clusters !== nothing
                    p.filter_values = e.clusters
                    stamp_cluster_run!(p, ti, vn)        # the whole list re-picked on the run as it is now
                end
                e.colour === nothing || (p.colour = e.colour)
                stamp_edit!(m, e.path)
                push!(paths, e.name === nothing ? e.path : rename_pop!(m, e.path, e.name))
            end
            Cecelia._invalidate!(m)
        end
        saved = _save_cluster_maps!(maps, body, vn, pt); saved === nothing || return saved
        counts, total = _cluster_pop_counts!(maps, paths, vn, pt)
        m1 = maps[1][3]
        pops = [m1.pops[q] for q in paths]
        200, JSON3.write((; ok = true, dryRun = _wbool(body, "dryRun", false), popType = pt, run, valueName = vn,
            images = [u for (u, _, _) in maps], skipped,
            populations = [(; path = q, name = pop_name(q), runAt = p.run_at, colour = p.colour, count = n,
                              clusters = sort!(collect(_cluster_ids_of(p))))
                           for (q, p, n) in zip(paths, pops, counts)],
            total, unassigned = sort!(collect(setdiff(universe, keys(cluster_id_owners(m1, run))))),
            updatedBy = pops[1].updated_by))
    end
end

# The `groups` of a set-cluster-pops request → (edits, nothing) or (nothing, the 422). Each: the `path` of
# a population of the run, and any of `clusters` (the whole new list — ids the run has), `name`, `colour`.
function _cluster_pop_edits(spec, run, universe::Set{Int})
    (spec isa AbstractVector && !isempty(spec)) || return (nothing, _gerr(422,
        "groups required: [{path, clusters?, name?, colour?}, …] — one per population to change"))
    out = NamedTuple{(:path, :clusters, :name, :colour),
                     Tuple{String,Union{Vector{Int},Nothing},Union{String,Nothing},Union{String,Nothing}}}[]
    owner = Dict{Int,String}()
    for (i, g) in enumerate(spec)
        g isa AbstractDict || return (nothing, _gerr(422, "group $i must be {path, clusters?, name?, colour?}"))
        raw_path = strip(string(something(get(g, "path", nothing), "")))
        isempty(raw_path) && return (nothing, _gerr(422, "group $i: path required — the population to change"))
        path = startswith(raw_path, "/") ? String(raw_path) : "/" * raw_path
        any(o -> o.path == path, out) && return (nothing, _gerr(422, "two groups change \"$path\""))
        name = nothing
        if get(g, "name", nothing) !== nothing
            name, _, _, _, err = _claude_new_pop(Dict{String,Any}("name" => g["name"]))
            err === nothing || return (nothing, err)
            any(o -> o.name == name, out) && return (nothing, _gerr(422, "two groups are renamed \"$name\""))
        end
        colour = get(g, "colour", nothing)
        colour === nothing || _is_hex_colour(colour) ||
            return (nothing, _gerr(422, "group \"$path\": colour must be a hex colour like \"#22c55e\""))
        ids = nothing
        raw = get(g, "clusters", nothing)
        if raw !== nothing
            ids, err = _group_cluster_ids(raw, path, run, universe, owner,
                                          " — to empty a population, delete it"); err === nothing || return (nothing, err)
        end
        (ids === nothing && name === nothing && colour === nothing) && return (nothing, _gerr(422,
            "group \"$path\" changes nothing — give clusters, name or colour"))
        push!(out, (; path, clusters = ids, name, colour = colour === nothing ? nothing : String(colour)))
    end
    (out, nothing)
end

# The images a cluster population of run `run` lives on — the run's `partOf` (the anchor first), each with
# the run on the same segmentation `vn` — as (uid, image) pairs, plus the uids of members that lack it
# (moved, deleted, re-segmented) — skipped, as the cluster page skips an image outside the run.
function _cluster_run_images(proj::AbstractString, img::CciaImage, vn::AbstractString, pt, run::AbstractString)
    fam = Cecelia._cluster_measure_family(pt)
    members = Cecelia._clustfeatures_part_of(
        Cecelia._clustfeatures_entry(cluster_props_path(img, vn, pt), run; family = fam), img.uid)
    targets = Tuple{String,CciaImage}[(img.uid, img)]
    skipped = String[]
    for u in members
        u == img.uid && continue
        mi = try init_object(proj, u) catch; nothing end
        _in_same_cluster_run(img, mi, vn, pt, run) ? push!(targets, (u, mi)) : push!(skipped, u)
    end
    (targets, skipped)
end

# Does `other` carry the SAME run `run` as `img` — not just one of that name? Its entry must list `img` in
# `partOf`, and carry the same `at` when both are stamped: an image re-clustered alone under the same suffix
# has renumbered clusters, so a grouping of `img`'s run would mean other cells there.
function _same_cluster_run(img::CciaImage, other::CciaImage, vn::AbstractString, pt, run::AbstractString)::Bool
    fam = Cecelia._cluster_measure_family(pt)
    p_img, p_oth = cluster_props_path(img, vn, pt), cluster_props_path(other, vn, pt)
    run in Cecelia._clustfeatures_suffixes(p_oth; family = fam) || return false
    img.uid in Cecelia._clustfeatures_part_of(Cecelia._clustfeatures_entry(p_oth, run; family = fam), other.uid) ||
        return false
    a, b = cluster_run_at(p_img, run; family = fam), cluster_run_at(p_oth, run; family = fam)
    a === nothing || b === nothing || a == b
end
_in_same_cluster_run(img::CciaImage, mi, vn::AbstractString, pt, run::AbstractString)::Bool =
    mi isa CciaImage && vn in versioned_keys(mi.label_props) && _same_cluster_run(img, mi, vn, pt, run)

# `_clust_members` (the anchor's own `partOf`, per run) narrowed to the members that still carry the SAME
# run — the rule `_cluster_run_images` applies for Claude's routes. The GUI replays every cluster-pop
# tick to these images (`g.mirrorUids`), so a member re-clustered alone under the same suffix must drop
# out here too, or the anchor run's ids land on its renumbered clusters. An empty list (a run from before
# `partOf`) stays empty; without a project uid the members can't be opened and pass unfiltered.
function _same_run_members(project_uid::AbstractString, img::CciaImage, vn::AbstractString, pt,
                           members::AbstractDict)
    isempty(project_uid) && return members
    cache = Dict{String,Any}()
    member(u) = get!(() -> (try init_object(project_uid, u) catch; nothing end), cache, u)
    Dict{String,Any}(s => String[u for u in us if u == img.uid || _in_same_cluster_run(img, member(u), vn, pt, s)]
                     for (s, us) in members)
end

# The `groups` of an add-cluster-pops request → (groups, nothing) or (nothing, the 422). Each group:
# a population name, its cluster ids — known to the run, none in two groups — and an optional colour.
function _proposed_cluster_groups(spec, run, universe::Set{Int})
    (spec isa AbstractVector && !isempty(spec)) || return (nothing, _gerr(422,
        "groups required: [{name, clusters: [ids]}, …] — one per population"))
    out = NamedTuple{(:name, :path, :clusters, :colour),Tuple{String,String,Vector{Int},Union{String,Nothing}}}[]
    owner = Dict{Int,String}()
    for (i, g) in enumerate(spec)
        g isa AbstractDict || return (nothing, _gerr(422, "group $i must be {name, clusters}"))
        name, _, _, path, err = _claude_new_pop(Dict{String,Any}("name" => something(get(g, "name", nothing), "")))
        err === nothing || return (nothing, err)
        any(o -> o.name == name, out) && return (nothing, _gerr(422, "two groups are named \"$name\""))
        colour = get(g, "colour", nothing)
        colour === nothing || _is_hex_colour(colour) ||
            return (nothing, _gerr(422, "group \"$name\": colour must be a hex colour like \"#22c55e\""))
        ids, err = _group_cluster_ids(get(g, "clusters", nothing), name, run, universe, owner, ", e.g. [3, 7]")
        err === nothing || return (nothing, err)
        push!(out, (; name, path, clusters = ids, colour = colour === nothing ? nothing : String(colour)))
    end
    (out, nothing)
end

# The cluster ids a write would ADD to population `path` that another population of the same run holds
# → the 409 naming them, or `nothing`. A cluster belongs to at most one population of a run: the manager's
# tick moves it (off the holder first, then on), so a request that would put it in two is refused here
# rather than silently double-counting its cells. Ids the population already held are not re-checked, so a
# legacy overlap never blocks an unrelated edit. A population that doesn't select its values (`neq 3`:
# Cecelia._claims_values) adds none of them, so it is never refused for them.
function _cluster_ids_taken(m::PopulationTree, path::AbstractString, new_values, old_values = nothing)
    p = m.pops[String(path)]
    sfx = cluster_pop_suffix(p)
    (sfx === nothing || !Cecelia._claims_values(p)) && return nothing
    _cluster_taken_refusal(cluster_id_owners(m, sfx; except = String(path)), _value_cluster_ids(new_values);
                           had = _value_cluster_ids(old_values), hint = " Take it out of there first.")
end

# The cluster ids in a filter's values (3, 3.0, "3" alike; anything else dropped) / the ids a population
# HOLDS — none for one that doesn't claim its values (`neq`/`gt`: Cecelia._claims_values).
_value_cluster_ids(values) = Set{Int}(filter(!isnothing, map(as_cluster_id, Cecelia._as_value_vec(values))))
_cluster_ids_of(p) = Cecelia._claims_values(p) ? _value_cluster_ids(p.filter_values) : Set{Int}()

# The 409 `cluster_taken` for the ids of `ids` that another population holds (`held`, `cluster_id_owners`)
# — ids in `had`, the population's own before this write, are not re-checked — or `nothing`. `image` names
# the image of a set-wide write it was found on.
function _cluster_taken_refusal(held::AbstractDict, ids; had = Set{Int}(), image = nothing,
                                hint = " Leave it out, or ask the user to move it.")
    taken = Dict(string(id) => held[id] for id in ids if !(id in had) && haskey(held, id))
    isempty(taken) && return nothing
    error = _cluster_taken_text(taken) * hint
    409, JSON3.write(image === nothing ? (; code = "cluster_taken", clusters = taken, error) :
                                         (; code = "cluster_taken", clusters = taken, image, error))
end
_cluster_taken_refusal(held, ids, image; kw...) = _cluster_taken_refusal(held, ids; image, kw...)

# A cluster population's new path `path` on image `u`'s map → nothing, or the 409: taken on this map
# (`pop_exists`) or by another pop type of the segmentation (`pop_name_conflict`).
function _cluster_name_refusal(m::PopulationTree, img::CciaImage, vn, pt, path::AbstractString, u)
    has_pop(m, path) && return 409, JSON3.write((; code = "pop_exists", image = u,
        error = "A population \"$path\" already exists — choose another name; an existing population is never replaced"))
    conflict = pop_name_conflict(img, vn, path; pop_type = pt)
    conflict === nothing || return 409, JSON3.write((; code = "pop_name_conflict", image = u,
        error = "A $conflict population named \"$(pop_name(path))\" already exists in this segmentation — " *
                "names must be unique across population types. Choose another name."))
    nothing
end

# One group's `clusters` → (sorted unique ids, nothing) or (nothing, the 422): a non-empty list of ids the run
# has, none already in another group of the request (`owner`: id ⇒ group label, filled in here).
function _group_cluster_ids(raw, label, run, universe::Set{Int}, owner::Dict{Int,String}, empty_hint)
    (raw isa AbstractVector && !isempty(raw)) || return (nothing, _gerr(422,
        "group \"$label\": clusters is a non-empty list of cluster ids$empty_hint"))
    ids = Int[]
    for v in raw
        id = as_cluster_id(v)
        id === nothing && return (nothing, _gerr(422, "group \"$label\": $(repr(v)) is not a cluster id"))
        push!(ids, id)
    end
    ids = sort!(unique(ids))
    unknown = setdiff(ids, universe)
    isempty(unknown) || return (nothing, _gerr(422, "group \"$label\": run \"$run\" has no cluster " *
        join(unknown, ", ") * (isempty(universe) ? "" : " — its clusters are $(join(sort!(collect(universe)), ", "))")))
    twice = [id for id in ids if haskey(owner, id)]
    isempty(twice) || return (nothing, _gerr(422, "cluster $(join(twice, ", ")) is in both " *
        "\"$(owner[twice[1]])\" and \"$label\" — a cluster belongs to one population"))
    for id in ids; owner[id] = String(label); end
    (ids, nothing)
end

# "Clusters 3, 7 already belong to /A — a cluster belongs to one population of a run." — `taken` id ⇒ holder.
function _cluster_taken_text(taken::AbstractDict)
    ids = sort!(parse.(Int, collect(keys(taken))))
    (length(ids) > 1 ? "Clusters $(join(ids, ", ")) already belong" : "Cluster $(ids[1]) already belongs") *
        " to $(join(sort!(unique(collect(values(taken)))), ", ")) — a cluster belongs to one population of a run."
end

# ── Claude's own gates — the MCP servers' whole write surface into gating ─────────────────────────
# Both servers (the observer and the autonomous one) draw through /pop/add-gate (above) and change what
# they drew through these two; neither is allowed /pop/add, /pop/set-gate or /pop/delete, which act on
# anything. "Own" = every population at and below `path` carries a `createdBy` stamped by a Claude
# request (`via: claude`, the ✦ in the population manager): once the user has gated something under
# a proposal, its subtree is theirs too and only they change it (403 `not_claude_made`, naming the
# populations). So is a proposal the user has EDITED (moved, renamed, recoloured, re-parented,
# copied — `updatedBy` from a non-Claude request), and one a task has READ: a clustering or tracking run does not follow its gate, so
# moving the gate would leave that result out of step (403 `used_by_runs`). Each route takes `dryRun: true` — validate, apply to the map in hand, answer with the
# counts, write nothing — so a gate can be tried before it lands.

# The parsed body + the image + its EXACT segmentation, or the refusal. No fallback to the active
# segmentation (`_resolve_vn`): a write lands where it was asked, or nowhere.
function _claude_gate_body(body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes), Dict{String,Any})
    catch
        return (nothing, nothing, nothing, _gerr(400, "Invalid JSON body"))
    end
    img, err = _gating_image(_wstr(body, "projectUid"), _wstr(body, "imageUid"))
    err === nothing || return (nothing, nothing, body, err)
    vn = _wstr(body, "valueName")
    isempty(vn) && return (nothing, nothing, body, _gerr(400, "valueName required"))
    vns = versioned_keys(img.label_props)
    vn in vns || return (nothing, nothing, body, _gerr(404, "No segmentation \"$vn\" on this image" *
        (isempty(vns) ? " — it has not been segmented" : " — it has: " * join(vns, ", "))))
    (img, vn, body, nothing)
end

# Claude drew it and nobody else has edited it since: the last edit (`updatedBy`, `stamp_edit!`) was
# Claude's own or there was none.
_via_claude(s) = s !== nothing && get(s, "via", "") == "claude"
# THE ownership rule, for any record carrying the two stamps — a population here, a figure in
# figures_api.jl (`figure_claude_made`).
claude_made(created_by, updated_by) = _via_claude(created_by) && (updated_by === nothing || _via_claude(updated_by))
claude_made(p) = claude_made(p.created_by, p.updated_by)

# `path` and everything under it must be Claude's; else the 403 that names what is the user's.
function _own_subtree(m::PopulationTree, path::AbstractString)
    sub = [String(path); descendants(m, path)]
    theirs = [q for q in sub if !claude_made(m.pops[q])]
    isempty(theirs) && return (sub, nothing)
    what = theirs[1] == path ? "\"$path\" was not drawn by Claude" :
           "the user's population$(length(theirs) > 1 ? "s" : "") $(join(("\"$q\"" for q in theirs), ", ")) " *
           "sit$(length(theirs) > 1 ? "" : "s") under \"$path\""
    (sub, (403, JSON3.write((; error = what * " — only the user changes it. Propose a new gate instead (add_gate).",
                              code = "not_claude_made", populations = theirs))))
end

# Moving a gate also moves every combination that reads it, wherever it sits ("not G" beside G), and
# everything under those: the populations OUTSIDE `sub` whose membership depends on `path` through a
# boolean term. They must be Claude's too, else the 403 naming the user's — the same rule as a child.
# (delete-own needs none of this: it refuses outright while anything combines what it would remove.)
function _own_combinations(m::PopulationTree, path::AbstractString, sub::AbstractVector)
    inside = Set(sub)
    combos = [q for q in pops_depending_on(m, path) if !(q in inside)]
    theirs = [q for q in combos if !claude_made(m.pops[q])]
    isempty(theirs) && return (combos, nothing)
    (combos, (403, JSON3.write((; error = "the user's population$(length(theirs) > 1 ? "s" : "") " *
        join(("\"$q\"" for q in theirs), ", ") * " combine$(length(theirs) > 1 ? "" : "s") \"$path\" — " *
        "moving it would change their cells; only the user changes it. Propose a new gate instead (add_gate).",
        code = "not_claude_made", populations = theirs))))
end

# None of `sub` may be a population a task has read (`run_pop_reads`), else the 403 naming them and
# the tasks. A clustering, tracking or spatial run on a gate is a snapshot of its cells: moving the gate
# under it would leave that result describing cells the gate no longer holds.
function _unread_subtree(img::CciaImage, vn::AbstractString, sub::AbstractVector)
    reads = run_pop_reads(img)
    hit = [q for q in sub if haskey(reads, (String(vn), q))]
    isempty(hit) && return nothing
    tasks = unique(vcat((reads[(String(vn), q)] for q in hit)...))
    403, JSON3.write((; error = join(("\"$q\"" for q in hit), ", ") * " " *
                         (length(hit) > 1 ? "were" : "was") * " used by " * join(tasks, ", ") *
                         " — changing it would leave that result out of step; only the user changes it. " *
                         "Propose a new gate instead (add_gate).",
                      code = "used_by_runs", populations = hit, tasks))
end

# The reply every own-gate write gives: the population's counts on the map just written — or, for a
# `dryRun`, on the map in hand, which is never saved. One recompute on the edited map either way, so a
# preview and the write it previews report the same numbers.
# `regions`: a split's populations — each with its count, beside the first region's full reply.
function _claude_gate_reply(m::PopulationTree, img::CciaImage, body, vn, path::AbstractString;
                            regions::Vector{String} = String[])
    dry = _wbool(body, "dryRun", false)
    if !dry
        saved = _persist_and_broadcast!(m, img, body, vn, "flow")
        saved isa Tuple && return saved
    else
        _anchor_follow_rules!(m, img, vn, "flow")
    end
    recompute!(m, pop_membership_fetch(img, vn, "flow"))
    p = m.pops[String(path)]
    s = pop_stats(m, path)
    reg = [(; path = r, count = pop_stats(m, r).count, pctParent = round(pop_stats(m, r).pct_parent; digits = 2))
           for r in regions]
    200, JSON3.write((; ok = true, dryRun = dry, path, valueName = vn, regions = reg,
                        parent = is_root(p.parent) ? "root" : p.parent,
                        count = s.count, parentCount = s.parent_count, outside = s.parent_count - s.count,
                        pctParent = round(s.pct_parent; digits = 2),
                        gate = p.gate === nothing ? nothing : gate_spec(p.gate), boolean = boolean_spec(p),
                        filter = filter_spec(p),
                        colour = p.colour,
                        createdBy = p.created_by, updatedBy = p.updated_by))
end

# ── POST /api/gating/pop/set-own-gate — move a gate Claude drew ──
# { projectUid, imageUid, valueName, path, gate, dryRun? } → the add-gate reply. 404 / 403 / 422.
function api_gating_pop_set_own_gate(body_bytes::Vector{UInt8})
    img, vn, body, err = _claude_gate_body(body_bytes); err === nothing || return err
    path = strip(_wstr(body, "path"))
    isempty(path) && return _gerr(422, "path required")
    gate, gerr = _proposed_gate(get(body, "gate", nothing), img, vn)
    gerr === nothing || return _gerr(422, gerr)
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = "flow")
        has_pop(m, path) || return _pop_not_found(m, path)
        sub, refusal = _own_subtree(m, path); refusal === nothing || return refusal
        combos, refusal = _own_combinations(m, path, sub); refusal === nothing || return refusal
        refusal = _unread_subtree(img, vn, [sub; combos]); refusal === nothing || return refusal
        cur = m.pops[String(path)].gate
        cur === nothing && return _gerr(422, "\"$path\" is not a gate")
        gate, refusal = _keep_split_region(cur, gate, path); refusal === nothing || return refusal
        set_gate!(m, String(path), gate)
        stamp_edit!(m, path)
        _claude_gate_reply(m, img, body, vn, path)
    end
end

# A split region (a slice / quadrant / bisector half) moved by set-own-gate keeps its region and group
# (`with_divider`, as `set_gate!` moves its siblings), so the regions still tile the parent. Its names
# (`+`/`-`, `++`…) were worked out from the divider's orientation (`_proposed_regions`), so a divider
# re-sent the other way round (a slice's vertices reversed, a quadrant's arms from another start) is turned
# back first — each name keeps the cells it describes. Another kind, or swapped axes, is refused: delete the
# split and add the new gate. → (gate, nothing) or (nothing, 422).
function _keep_split_region(cur::Gate, gate::Gate, path)
    split(g) = g isa Cecelia.SplitGate || g isa BisectorGate
    split(cur) || split(gate) || return (gate, nothing)
    kind = gate_spec(cur)["kind"]
    refuse(why) = (nothing, _gerr(422, "\"$path\" is one region of a $kind split; $why, or delete the split and add the new gate"))
    gate_spec(gate)["kind"] == kind || return refuse("move it as a $kind")
    (gate.x_channel, gate.y_channel) == (cur.x_channel, cur.y_channel) ||
        return refuse("keep its axes ($(cur.x_channel), $(cur.y_channel))")
    d = gate_spec(gate); d["group"] = Cecelia.gate_group(cur)
    _reorient_divider!(d, gate_spec(cur))
    (Cecelia.with_divider(cur, gate_from_spec(d)), nothing)
end

# A re-sent divider spec `d` turned to run the way the stored one `c` did, so each region index keeps its
# side: a slice reversed when its chord points against the stored chord; a quadrant's arms rotated so arm 1
# is the one nearest the stored arm 1 (by angle about each centre). By geometry, not by the `+`/`-` naming
# rule — a small move across that rule's 45° switch must not swap the halves.
function _reorient_divider!(d::AbstractDict, c::AbstractDict)
    if get(d, "kind", "") == "slice"
        chord(vs) = (vs[end][1] - vs[1][1], vs[end][2] - vs[1][2])
        (u, w) = (chord(d["vertices"]), chord(c["vertices"]))
        u[1] * w[1] + u[2] * w[2] < 0 && (d["vertices"] = reverse(d["vertices"]))
    elseif get(d, "kind", "") == "quadrant"
        ang(spec, a) = atan(a[2] - spec["centre"][2], a[1] - spec["centre"][1])
        gap(t, s) = abs(mod(t - s + π, 2π) - π)
        target = ang(c, c["arms"][1])
        k = argmin([gap(ang(d, d["arms"][i]), target) for i in 1:4])
        d["arms"] = circshift(d["arms"], -(k - 1))
    end
    d
end

# ── POST /api/gating/pop/delete-own — remove a population Claude made, with its subtree ──
# { projectUid, imageUid, valueName, path, popType?, dryRun? } → { ok, dryRun, deleted: [paths], images }.
# 404 / 403 / 409. `popType` (default flow) may be a cluster type: a cluster population lives on every image
# of its run (add-cluster-pops writes it set-wide), so it goes from each of them — after every one passed.
function api_gating_pop_delete_own(body_bytes::Vector{UInt8})
    img, vn, body, err = _claude_gate_body(body_bytes); err === nothing || return err
    path = strip(_wstr(body, "path"))
    isempty(path) && return _gerr(422, "path required")
    pt = _wstr(body, "popType", "flow")
    (pt == "flow" || Cecelia._is_cluster_pop_type(pt)) ||
        return _gerr(422, "popType must be flow or a cluster type: $(join(CLUSTER_POP_TYPES, ", "))")
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = pt)
        has_pop(m, path) || return _pop_not_found(m, path)
        sfx = cluster_pop_suffix(m.pops[String(path)])
        targets = Tuple{CciaImage,PopulationTree}[(img, m)]
        if sfx !== nothing
            for (u, ti) in first(_cluster_run_images(_wstr(body, "projectUid"), img, vn, pt, sfx))
                u == img.uid && continue
                tm = load_pop_tree(ti; value_name = vn, pop_type = pt)
                has_pop(tm, path) && push!(targets, (ti, tm))
            end
        end
        sub = String[]
        for (ti, tm) in targets
            tsub, refusal = _own_subtree(tm, path); refusal === nothing || return refusal
            refusal = _unread_subtree(ti, vn, tsub); refusal === nothing || return refusal
            deps = boolean_dependents(tm, tsub)
            isempty(deps) || return 409, JSON3.write((; code = "boolean_dependents", error = "Can't delete: " *
                join(["\"$(d)\" combines $(join(rs, ", "))" for (d, rs) in deps], "; ") * " — the user's to change"))
            ti === img && (sub = tsub)
        end
        dry = _wbool(body, "dryRun", false)
        if !dry
            for (ti, tm) in targets
                del_pop!(tm, String(path))
                saved = _persist_and_broadcast!(tm, ti, merge(body, Dict{String,Any}("imageUid" => ti.uid)), vn, pt)
                saved isa Tuple && return saved
            end
        end
        200, JSON3.write((; ok = true, dryRun = dry, valueName = vn, deleted = sub,
                            images = [ti.uid for (ti, _) in targets]))
    end
end

# The `gate` of an add-gate request → (Gate, nothing), or (nothing, why it cannot be one). Coordinates
# are TRANSFORMED (the space `/api/gating/plot-image` reports gates and extents in), so only their shape
# is checked here, not their range. A transform is accepted in either key spelling the API speaks —
# the plot routes' `cof` and the stored spec's `cofactor` — so a gate_plot reply can be passed back as is.
function _proposed_gate(spec, img, vn)::Tuple{Union{Gate,Nothing},Union{String,Nothing}}
    spec isa AbstractDict || return (nothing, "gate required: {kind: rectangle|polygon|range|bisector|slice|quadrant, x_channel, y_channel, …}")
    g(k) = get(spec, k, nothing)
    kind = lowercase(string(something(g("kind"), "")))
    kind in _PROPOSED_KINDS ||
        return (nothing, "gate.kind must be one of " * join(("\"$k\"" for k in _PROPOSED_KINDS), ", "))
    one_d = kind in ("range", "bisector")
    xc = g("x_channel"); yc = one_d ? something(g("y_channel"), xc) : g("y_channel")   # 1D: one channel
    (xc isa AbstractString && yc isa AbstractString && !isempty(xc) && !isempty(yc)) ||
        return (nothing, "gate.x_channel and gate.y_channel required")
    lp = label_props(img; value_name = vn)
    cols = Set(vcat(col_names(lp; data_type = :vars), col_names(lp; data_type = :obs)))
    (xc in cols && yc in cols) || return (nothing, _missing_column_hint(img, vn, (xc, yc)))
    xt_spec, terr = _proposed_transform(g("x_transform"), "x"); terr === nothing || return (nothing, terr)
    yt_spec, terr = _proposed_transform(g("y_transform"), "y"); terr === nothing || return (nothing, terr)
    # a logicle / asinh sent without its parameters is stored with the channel's own — the ones gate_plot
    # and gate_histogram drew and summarised it in (`_auto_params`), not the cytometer defaults
    xt_spec, yt_spec = _proposed_auto(xt_spec, yt_spec, img, vn, xc, yc)
    finite_num(v) = v isa Real && !(v isa Bool) && isfinite(v)
    full = Dict{String,Any}("kind" => kind, "x_channel" => xc, "y_channel" => yc,
                            "x_transform" => xt_spec, "y_transform" => yt_spec)
    if kind == "rectangle"
        ks = ("x_min", "x_max", "y_min", "y_max")
        all(k -> finite_num(g(k)), ks) || return (nothing, "a rectangle needs finite x_min, x_max, y_min, y_max")
        (g("x_min") < g("x_max") && g("y_min") < g("y_max")) ||
            return (nothing, "a rectangle needs x_min < x_max and y_min < y_max")
        for k in ks; full[k] = float(g(k)); end
    elseif kind == "polygon"
        vs = g("vertices")
        (vs isa AbstractVector && length(vs) >= 3) || return (nothing, "a polygon needs at least 3 vertices [[x, y], …]")
        all(v -> v isa AbstractVector && length(v) == 2 && all(finite_num, v), vs) ||
            return (nothing, "every polygon vertex must be [x, y] with finite numbers")
        full["vertices"] = [[float(v[1]), float(v[2])] for v in vs]
    else
        err = _proposed_extra_geometry!(full, kind, g, finite_num)
        err === nothing || return (nothing, err)
    end
    if kind in ("rectangle", "polygon") && g("rule") !== nothing
        # follow the cloud: anchored on the parent's cells when it is added (`_anchor_follow_rules!`)
        r = g("rule")
        (r isa AbstractDict && lowercase(string(get(r, "kind", ""))) == "follow") ||
            return (nothing, "a $kind's rule is {\"kind\": \"follow\"} (it moves with the cloud it is drawn on)")
        adj = get(r, "adjust", nothing)
        (adj === nothing || (finite_num(adj) && adj > 0)) || return (nothing, "gate.rule.adjust must be a positive number")
        full["rule"] = Dict{String,Any}("kind" => "follow", "adjust" => float(something(adj, 1.0)))
    end
    try
        (gate_from_spec(full), nothing)
    catch e
        (nothing, "not a valid gate: " * sprint(showerror, e))
    end
end

const _PROPOSED_KINDS = ("rectangle", "polygon", "range", "bisector", "slice", "quadrant")

# The geometry of a proposed 1D or split gate, checked and copied into `full` → nothing, or why not.
# A split proposal describes its divider; the regions are added from it (`api_gating_pop_add_gate`).
function _proposed_extra_geometry!(full, kind, g, finite_num)
    pt(v) = v isa AbstractVector && length(v) == 2 && all(finite_num, v)
    function rule!(key)
        r = g(key); r === nothing && return nothing
        (r isa AbstractDict && finite_num(get(r, "lo", nothing)) && finite_num(get(r, "hi", nothing))) ||
            return "gate.$key must be {\"kind\": \"valley\", \"lo\": a, \"hi\": b} (a search window, transformed)"
        full[key] = Dict{String,Any}("kind" => "valley", "lo" => float(r["lo"]), "hi" => float(r["hi"]),
                                     "adjust" => float(something(get(r, "adjust", nothing), 1.0)))
        nothing
    end
    if kind == "range"
        (finite_num(g("lo")) && finite_num(g("hi")) && g("lo") < g("hi")) || return "a range needs finite lo < hi"
        full["lo"], full["hi"] = float(g("lo")), float(g("hi"))
        e = rule!("lo_rule"); e === nothing || return e
        return rule!("hi_rule")
    elseif kind == "bisector"
        finite_num(g("cut")) || return "a bisector needs a finite cut"
        full["cut"] = float(g("cut"))
        return rule!("rule")
    elseif kind == "slice"
        vs = g("vertices")
        (vs isa AbstractVector && length(vs) >= 2 && all(pt, vs)) ||
            return "a slice needs at least 2 vertices [[x, y], …] with finite numbers"
        full["vertices"] = [[float(v[1]), float(v[2])] for v in vs]
    else                                                    # quadrant
        pt(g("centre")) || return "a quadrant needs a finite centre [x, y]"
        c = float.(g("centre"))
        arms = g("arms")
        arms === nothing && (arms = [[c[1] + 1, c[2]], [c[1], c[2] + 1], [c[1] - 1, c[2]], [c[1], c[2] - 1]])
        (arms isa AbstractVector && length(arms) == 4 && all(pt, arms)) ||
            return "a quadrant's arms are 4 points [[x, y], …], counter-clockwise (omit them for the axes)"
        full["centre"] = c; full["arms"] = [[float(a[1]), float(a[2])] for a in arms]
    end
    nothing
end

# A proposed split's regions: each (name suffix, region). A slice's `+` is the side toward higher values
# (as the GUI names it, `positiveSide` in plots/splitGeometry.ts); a quadrant's sector is named x-sign then
# y-sign by the direction halfway between its two arms (so arms listed from any start name right; axis
# arms give sectors 1–4 = ++, -+, --, +-). Arms that leave two sectors the same signs → `1`…`4`.
function _proposed_regions(gate::Gate)
    gate isa QuadrantGate && return _quadrant_regions(gate)
    pos = 1
    if gate isa SliceGate
        (x0, y0), (x1, y1) = first(gate.vertices), last(gate.vertices)
        dx, dy = x1 - x0, y1 - y0
        pos = abs(dx) >= abs(dy) ? (dx >= 0 ? 1 : -1) : (dy >= 0 ? -1 : 1)
    end
    [("+", pos), ("-", -pos)]
end

function _quadrant_regions(g::QuadrantGate)
    (cx, cy) = g.centre
    θ = [atan(a[2] - cy, a[1] - cx) for a in g.arms]
    sgn(t) = t >= 0 ? "+" : "-"
    names = map(1:4) do q
        mid = θ[q] + mod(θ[mod1(q + 1, 4)] - θ[q], 2π) / 2     # counter-clockwise from arm q to arm q+1
        sgn(cos(mid)) * sgn(sin(mid))
    end
    allunique(names) || (names = string.(1:4))
    [(names[q], q) for q in 1:4]
end

# A whole gate spec with any unparameterised logicle / asinh axis filled from the channel's data — the
# general add / set-gate routes. The GUI always sends every parameter, so for it this changes nothing.
# Cell (`flow`) gates only: a track gate's axes are track aggregates, read differently.
function _gate_spec_auto(spec, img, vn, pop_type)
    (spec isa AbstractDict && pop_type == "flow") || return spec
    d = Dict{String,Any}(String(k) => v for (k, v) in spec)
    (haskey(d, "x_channel") && haskey(d, "y_channel")) || return d
    # the add-gate normaliser (`cof` → `cofactor`, finite params); a bad spec is left for gate_from_spec to refuse
    xs, xerr = _proposed_transform(get(d, "x_transform", nothing), "x")
    ys, yerr = _proposed_transform(get(d, "y_transform", nothing), "y")
    (xerr === nothing && yerr === nothing) || return d
    d["x_transform"], d["y_transform"] = _proposed_auto(xs, ys, img, vn, String(d["x_channel"]), String(d["y_channel"]))
    d
end

function _proposed_auto(xs::Dict{String,Any}, ys::Dict{String,Any}, img, vn, xc, yc)
    q = Dict{String,String}()
    haskey(xs, "T") && (q["xT"] = "1"); haskey(xs, "cofactor") && (q["xcof"] = "1")
    haskey(ys, "T") && (q["yT"] = "1"); haskey(ys, "cofactor") && (q["ycof"] = "1")
    xt, yt = _auto_axes(q, img, vn, "flow", xc, yc, transform_from_spec(xs), transform_from_spec(ys))
    (_needs_auto(transform_from_spec(xs), q, "x") ? transform_spec(xt) : xs,
     _needs_auto(transform_from_spec(ys), q, "y") ? transform_spec(yt) : ys)
end

# One axis transform of a proposed gate → its stored spec, or why not. Absent ⇒ linear.
function _proposed_transform(t, axis::AbstractString)::Tuple{Union{Dict{String,Any},Nothing},Union{String,Nothing}}
    t === nothing && return (Dict{String,Any}("kind" => "linear"), nothing)
    t isa AbstractDict || return (nothing, "gate.$(axis)_transform must be an object like {\"kind\": \"linear\"}")
    d = Dict{String,Any}(String(k) => v for (k, v) in t)
    kind = lowercase(string(get(d, "kind", "linear")))
    params = Dict("linear" => String[], "log" => ["floor"], "asinh" => ["cofactor"], "logicle" => ["T", "W", "M", "A"])
    haskey(params, kind) || return (nothing, "gate.$(axis)_transform.kind must be linear, asinh, log or logicle")
    kind == "asinh" && haskey(d, "cof") && !haskey(d, "cofactor") && (d["cofactor"] = pop!(d, "cof"))
    out = Dict{String,Any}("kind" => kind)
    for k in params[kind]
        haskey(d, k) || continue
        v = d[k]
        (v isa Real && !(v isa Bool) && isfinite(v)) || return (nothing, "gate.$(axis)_transform.$k must be a finite number")
        out[k] = float(v)
    end
    (out, nothing)
end

function api_gating_pop_set_gate(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    (haskey(body, "path") && haskey(body, "gate")) || return _gerr(400, "path and gate required")
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = pt)
        has_pop(m, body["path"]) || return _pop_not_found(m, body["path"])
        try
            set_gate!(m, String(body["path"]), gate_from_spec(_gate_spec_auto(body["gate"], img, vn, pt)))
        catch e
            return _gerr(400, sprint(showerror, e))
        end
        stamp_edit!(m, String(body["path"]))
        200, JSON3.write(_persist_and_broadcast!(m, img, body, vn, pt))
    end
end

# `childrenOnly: true` deletes the subtree UNDER `path` and keeps the population itself — the
# "prune what I gated off this" action in the manager's row menu. One request (so one undo step),
# not one delete per child.
# `paths: [...]` (instead of `path`) deletes a multi-selection the same way: one load, one save, one
# undo step. A selected child of a selected parent is part of the parent's cascade (`del_pops!`).
# `childrenOnly` takes ONE path: "everything below" a selection that holds a parent and its child is
# ambiguous (keep the child, or prune it with the parent's subtree?).
function api_gating_pop_delete(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    _paths, perr = _body_paths(body); perr === nothing || return perr
    _children_only = _wbool(body, "childrenOnly", false)
    (_children_only && length(_paths) > 1) && return _gerr(400, "childrenOnly takes one path")
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = pt)
        for p in _paths
            has_pop(m, p) || return _pop_not_found(m, p)
        end
        # A boolean pop (Decision 16) references pops by PATH, so deleting one it combines would leave
        # it silently pointing at nothing (empty membership + a server warning nobody reads). Refuse
        # and name the dependants instead — rename/move rewrite references, only delete can orphan them.
        _going = _children_only ? descendants(m, _paths[1]) : pops_with_subtrees(m, _paths)
        _deps = boolean_dependents(m, _going)
        isempty(_deps) || return _gerr(400,
            "Can't delete: " * join(["\"$(pop_name(d))\" combines $(join([pop_name(r) for r in rs], ", "))"
                                    for (d, rs) in _deps], "; ") *
            ". Edit or delete the combined population$(length(_deps) > 1 ? "s" : "") first.")
        _children_only ? del_children!(m, _paths[1]) : del_pops!(m, _paths)
        200, JSON3.write(_persist_and_broadcast!(m, img, body, vn, pt))
    end
end

# The populations a mutation acts on: `paths` (a multi-selection) or the single `path` → (paths, err).
function _body_paths(body)
    if haskey(body, "paths")
        ps = body["paths"]
        (ps isa AbstractVector && !isempty(ps) && all(p -> p isa AbstractString, ps)) ||
            return String[], _gerr(400, "paths must be a non-empty list of population paths")
        return unique(String[String(p) for p in ps]), nothing
    end
    haskey(body, "path") || return String[], _gerr(400, "path required")
    String[String(body["path"])], nothing
end

function api_gating_pop_update(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    # `paths` + `show`: the manager's "all" viewer toggle, as ONE write — one reply, one broadcast, one
    # viewer overlay refetch (a whole-movie payload) for any number of populations
    haskey(body, "paths") && return _gating_pops_show(img, vn, pt, body)
    haskey(body, "path") || return _gerr(400, "path required")
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = pt)
        has_pop(m, body["path"]) || return _pop_not_found(m, body["path"])
        p = pop_at(m, String(body["path"]))
        # showing/hiding is looking, not editing; anything else here is the user's edit
        any(haskey(body, k) for k in ("colour", "filter", "boolean", "runChecked")) && stamp_edit!(m, String(body["path"]))
        haskey(body, "colour") && (p.colour = String(body["colour"]))
        haskey(body, "show")   && (p.show = Bool(body["show"]))
        # filter update — the tick-cluster-into-population UX toggles which cluster IDs belong to a
        # `clust`/`trackclust` pop by rewriting its filter (typically filter_values). Mutates only the
        # keys present in the `filter` dict, so a `{values:[…]}` tick leaves measure/fun untouched.
        flt = get(body, "filter", nothing)
        if flt !== nothing
            old_values, old_measure = p.filter_values, p.filter_measure
            haskey(flt, "measure")     && (p.filter_measure = get(flt, "measure", nothing))
            haskey(flt, "fun")         && (fv = get(flt, "fun", nothing);
                                           p.filter_fun = fv === nothing ? nothing : Cecelia.parse_filter_fun(String(fv)))
            haskey(flt, "values")      && (p.filter_values = get(flt, "values", nothing))
            haskey(flt, "default_all") && (p.filter_default_all = Bool(get(flt, "default_all", false)))
            # compound filter (Decision 15): replace the AND-ed conditions, mirroring conditions[1] onto
            # the single fields (same contract as add_pop!). `conditions: []`/null clears back to single.
            if haskey(flt, "conditions")
                conds = Cecelia._normalise_conditions(get(flt, "conditions", nothing))
                p.filter_conditions = conds
                conds === nothing || (p.filter_measure = conds[1].measure;
                                      p.filter_fun = conds[1].fun; p.filter_values = conds[1].values)
            end
            # a cluster population's ids were just re-picked: none another population of the run holds
            # (the tick moves a cluster off its holder FIRST)
            taken = _cluster_ids_taken(m, p.path, p.filter_values, old_values); taken === nothing || return taken
        end
        # The run stamp moves only when the user says the clusters are checked against the run as it is now
        # (`runChecked`, the stale icon's click) — a tick on a stale population leaves its other, old-numbered
        # ids as they were, so it must not clear the warning. A population with no stamp yet, or moved to
        # another run, is stamped: there is no earlier run for it to be stale against.
        moved = flt !== nothing && p.filter_measure != old_measure     # the form re-sends an unchanged one
        (_wbool(body, "runChecked", false) || p.run_at === nothing || moved) && stamp_cluster_run!(p, img, vn)
        # boolean update (Decision 16): the whole definition is replaced, not patched key-by-key — a
        # boolean pop IS its term list, and a partial patch has no meaning ("keep the old excludes but
        # take the new includes" is not something the form can express). `boolean: null` clears it.
        if haskey(body, "boolean")
            bl = body["boolean"]
            try
                set_boolean!(m, String(body["path"]);
                             op   = bl === nothing ? nothing : get(bl, "op", nothing),
                             pops = bl === nothing ? nothing : get(bl, "pops", nothing),
                             nots = bl === nothing ? nothing : get(bl, "not", nothing))
            catch e
                return _gerr(400, sprint(showerror, e))
            end
        end
        200, JSON3.write(_persist_and_broadcast!(m, img, body, vn, pt))
    end
end

function _gating_pops_show(img, vn, pt, body)
    haskey(body, "show") || return _gerr(400, "show required with paths")
    paths = String.(collect(body["paths"]))
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = pt)
        for p in paths
            has_pop(m, p) || return _pop_not_found(m, p)
        end
        for p in paths
            pop_at(m, p).show = Bool(body["show"])       # looking, not editing: no edit stamp
        end
        200, JSON3.write(_persist_and_broadcast!(m, img, body, vn, pt))
    end
end

function api_gating_pop_rename(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    (haskey(body, "path") && haskey(body, "newName")) || return _gerr(400, "path and newName required")
    # name-uniqueness guard: the renamed path must not collide with another pop type's population
    _newName = String(body["newName"])
    _newpath = pop_path(pop_parent(String(body["path"])), _newName)
    _conflict = pop_name_conflict(img, vn, _newpath; pop_type = pt)
    _conflict === nothing || return _gerr(400,
        "A $_conflict population named \"$_newName\" already exists in this segmentation — names must be " *
        "unique across population types so a population can be selected unambiguously. Choose another name.")
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = pt)
        has_pop(m, body["path"]) || return _pop_not_found(m, body["path"])
        stamp_edit!(m, String(body["path"]))
        newpath = try
            rename_pop!(m, String(body["path"]), String(body["newName"]))
        catch e
            return _gerr(400, sprint(showerror, e))
        end
        r = _persist_and_broadcast!(m, img, body, vn, pt)
        200, JSON3.write((; r..., path = newpath))
    end
end

# POST /api/gating/pop/move  { …, path | paths, parent }  → re-parent populations, subtrees and all.
# The gate is unchanged; its MEMBERSHIP is not — a pop is its own gate ∩ its parent's, so this is how
# a population gated under a QC gate is lifted out to all cells (or tucked under another gate) without
# redrawing it and its children. `parent` is "root" for top level. `paths` moves a multi-selection in
# ONE save and one undo step (`move_pops!`: all-or-nothing, a selected child rides with its selected
# parent) → `paths` (each moved root's new path) and `path` (the first, for single-path callers).
function api_gating_pop_move(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    haskey(body, "parent") || return _gerr(400, "path and parent required")
    _paths, perr = _body_paths(body); perr === nothing || return perr
    _parent = String(body["parent"])
    _with_pop_tree_lock() do
        m = load_pop_tree(img; value_name = vn, pop_type = pt)
        for p in _paths
            has_pop(m, p) || return _pop_not_found(m, p)
        end
        roots = selection_roots(m, _paths)
        # the move changes each pop's PATH, so it needs the same cross-pop-type uniqueness guard a
        # rename does (the mixed-type module picker resolves a population by path alone).
        for r in roots
            _newpath = pop_path(is_root(_parent) ? ROOT : _parent, pop_name(r))
            _conflict = pop_name_conflict(img, vn, _newpath; pop_type = pt)
            _conflict === nothing || return _gerr(400,
                "A $_conflict population already exists at \"$_newpath\" in this segmentation — names must be " *
                "unique across population types so a population can be selected unambiguously. Rename it first.")
        end
        foreach(r -> stamp_edit!(m, r), roots)
        newpaths = try
            move_pops!(m, roots, _parent)
        catch e
            return _gerr(400, sprint(showerror, e))
        end
        r = _persist_and_broadcast!(m, img, body, vn, pt)
        200, JSON3.write((; r..., path = first(newpaths), paths = newpaths))
    end
end

# POST /api/gating/copy  { projectUid, imageUid (source), valueName, popType, toImageUids: [...] }
# Copy the gating strategy for ONE gating pop_type (flow = cell gates, or track = track gates) from
# the source image to each target image, by writing the source's gating sidecar to each target —
# REPLACING any existing gating there. Membership recomputes per image on read, so gates alone are
# enough (no per-image recompute step, unlike the old R flowGatingSet copy). Plot layout is copied
# client-side (the canvas store is in-memory + autosaves), not here.
# → { copied: [uid], skipped: {uid: reason}, warnings: {uid: [{column, pops: [path]}]} } — `warnings`
#   names, per COPIED target, each gate/filter column its table lacks and the populations that will
#   therefore be empty there (`missing_gate_columns`); absent key ⇒ nothing missing.
function api_gating_copy(body_bytes::Vector{UInt8})
    img, vn, pt, body, err = _gating_post(body_bytes); err === nothing || return err
    is_gating_pop_type(pt) || return _gerr(400, "Not a gating population type: $pt (only flow/track)")
    tgt_raw = get(body, "toImageUids", nothing)
    (tgt_raw isa AbstractVector && !isempty(tgt_raw)) || return _gerr(400, "toImageUids required")
    proj, src_uid, err = _require_ids(body); err === nothing || return err
    targets = CciaImage[]; absent = Dict{String,String}()
    for t in tgt_raw
        tuid = String(t)
        tuid == src_uid && continue
        timg, _ = _gating_image(proj, tuid)
        timg === nothing ? (absent[tuid] = "not found") : push!(targets, timg)
    end
    r = try
        copy_gating!(img, targets; value_name = vn, pop_type = pt,
                     on_copied = (timg, m) -> _broadcast_pop_tree(proj, timg.uid, vn, pt, m, timg))
    catch e
        e isa ArgumentError || rethrow()
        return _gerr(400, "Source image has no $pt gating to copy")
    end
    200, JSON3.write((; copied = r.copied, skipped = merge(absent, r.skipped), warnings = r.warnings,
                          remapped = r.remapped))
end
