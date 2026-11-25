# Co-clustered segmentations + cluster-pop auto-share (docs/todo/CLUSTER_POOLING_PLAN.md) and the
# categorical colour-by palette (the ONE colour-choice rule). Two adjacent concerns joined in one
# file because they are called from the same read paths (a plot resolving a cluster pop and picking
# its colour) and neither is used from mutations/persistence. The palette is the single source of
# truth for categorical colour resolution — a caller building its own is a bug (docs/UI.md → Colours).

# ── Co-clustered segmentations + cluster-pop auto-share (docs/todo/CLUSTER_POOLING_PLAN.md) ────────
# Clustering (clustPops/clustTracks) runs JOINTLY across the selected populations, which may span
# several segmentations (value_names). The engine writes ONE shared `clusters.{suffix}` column + a
# `{props}.clustfeatures.json` sidecar (keyed by suffix) into EVERY input segment. So the value_names
# that took part in a run = those whose clustfeatures sidecar carries `suffix` — the single source of
# truth for "which segmentations belong to this clustering".
_clustfeatures_path(props_path::AbstractString) = props_sidecar_path(props_path, "clustfeatures")

# The sidecar is keyed by the FULL obs column the run wrote — `{family}.{suffix}` (= the
# `_cluster_measure_prefix` family + the run suffix), so a `clusters.immune` cell clustering and a
# `regions.immune` region clustering coexist on one segmentation instead of clobbering each other.
_clustfeatures_key(suffix::AbstractString, family::AbstractString) = "$(family).$(suffix)"

# Persist a clustering run's per-suffix manifest, so the cluster pages can offer EXACTLY the columns
# the run used (heatmap) and know WHICH images were clustered together (the `partOf` set — mirrors the
# old R `attr(clustPath, "partOf") <- uIDs` + `valuePartOf`). Stored as a `{props}.clustfeatures.json`
# sidecar next to the labelProps (cell table for clust, `__tracks` table for trackclust), keyed by
# suffix as `{features, partOf, family, labels}`; merged so multiple runs/suffixes coexist. Shared by
# clustPops + clustTracks + clustRegions (read by api_gating_channels). Cluster pops can only be
# defined for images in `partOf`.
#
# `family` is the obs-column family the run wrote (`_cluster_measure_prefix` without the dot:
# "clusters" for clustPops/clustTracks, "regions" for clustRegions). Two runs of DIFFERENT families may
# legitimately share a suffix on one segmentation (a `clusters.immune` cell clustering and a
# `regions.immune` region clustering are different columns) — without `family` the second write would
# silently clobber the first's features/partOf. Absent `family` reads back as "clusters" (back-compat
# with sidecars written before this field existed).
#
# `labels` optionally maps column name → display label, for runs whose feature columns are machine
# names but whose meaning is a user-facing string (region composition: `spatial.comp.B_qc__tracked.x`
# → "B/qc/_tracked"). Empty for the cluster tasks, whose columns are already channel/measure names
# relabelled via the channels endpoint's nameMap. `behaviour_axes` (track runs only) is the metadata
# `clustering_utils.behaviour_axes` returns — axis names + path root — stored as `behaviourAxes`
# (docs/todo/TRACK_BEHAVIOUR_AXES_PLAN.md, Decision 4). Untyped until a Julia reader needs it.
# `at` = when the run ran, one value for all its segments — a cluster pop's staleness (`cluster_run_at`).
# `params` = the task params that shaped the run (resolution, normalisation); `refines` = the cluster
# population a refine run was restricted to (`refine_source`). docs/todo/CLUSTER_REFINE_PLAN.md
function _write_clust_features!(props_path::AbstractString, suffix::AbstractString,
                                features::Vector{String}, part_of::Vector{String};
                                family::AbstractString = "clusters",
                                labels::AbstractDict = Dict{String,String}(),
                                behaviour_axes::Union{Nothing,AbstractDict} = nothing,
                                at::AbstractString = string(Dates.now()),
                                params::AbstractDict = Dict{String,Any}(),
                                refines::Union{Nothing,AbstractDict} = nothing)
    entry = Dict{String,Any}("features" => features, "partOf" => part_of,
                             "family" => String(family), "labels" => Dict{String,Any}(labels), "at" => String(at))
    behaviour_axes === nothing || (entry["behaviourAxes"] = behaviour_axes)
    isempty(params) || (entry["params"] = Dict{String,Any}(params))   # CLUSTER_REFINE_PLAN Decision 3
    refines === nothing || (entry["refines"] = refines)               # …and Decision 1 (cluster_refine.jl)
    put_props_sidecar_entry!(_clustfeatures_path(props_path), _clustfeatures_key(suffix, family), entry)
end

# Split a sidecar key back into (suffix, family). A LEGACY key is a bare suffix with no family prefix
# (written before families existed); it returns `nothing` for the family and is then treated as matching
# ANY requested family — preserving the old single-namespace behaviour for existing sidecars, so this
# change is regression-free on data already on disk.
const _CLUSTFEATURES_FAMILIES = ("clusters", "regions")
function _clustfeatures_split_key(key::AbstractString)
    for fam in _CLUSTFEATURES_FAMILIES
        startswith(key, fam * ".") && return (String(key)[ncodeunits(fam)+2:end], fam)
    end
    (String(key), nothing)
end

_clustfeatures_raw(props_path::AbstractString) = read_props_sidecar(_clustfeatures_path(props_path))

"""
    prune_stale_clustfeatures!(props_path; on_log) -> Vector{String}

Drop the sidecar entries whose cluster column is no longer on `props_path`, returning the dropped keys.
A rebuilt table loses its runs' columns — a re-track writes `{vn}__tracks.h5ad` from scratch — and a
sidecar still naming the run keeps this segmentation in it (`co_clustered_value_names`), so its pops
resolve to nothing without a word. Legacy family-less keys stay while either family's column does.
"""
function prune_stale_clustfeatures!(props_path::AbstractString; on_log = _ -> nothing)::Vector{String}
    raw = _clustfeatures_raw(props_path)
    (isempty(raw) || !isfile(props_path)) && return String[]
    cols = Set(col_names(label_props(props_path); data_type = :obs))
    stale = String[]
    for k in keys(raw)
        sfx, fam = _clustfeatures_split_key(String(k))
        any(f -> "$f.$sfx" in cols, fam === nothing ? _CLUSTFEATURES_FAMILIES : (fam,)) || push!(stale, String(k))
    end
    isempty(stale) && return stale
    on_log("[INFO] $(basename(props_path)) no longer carries cluster run(s) $(join(sort(stale), ", ")) — " *
           "re-run the clustering to restore them")
    drop_props_sidecar_keys!(_clustfeatures_path(props_path), raw, stale)
    stale
end

# Suffixes recorded for `family` ("clusters" for clust/trackclust, "regions" for region). Legacy
# family-less entries match every family (see `_clustfeatures_split_key`).
function _clustfeatures_suffixes(props_path::AbstractString;
                                 family::AbstractString = "clusters")::Set{String}
    out = Set{String}()
    for k in keys(_clustfeatures_raw(props_path))
        sfx, fam = _clustfeatures_split_key(String(k))
        (fam === nothing || fam == String(family)) && push!(out, sfx)
    end
    out
end

"""
    _clustfeatures_entry(props_path, suffix; family="clusters") -> AbstractDict | nothing

THE reader for one run's clustfeatures entry — do not index the raw sidecar JSON anywhere else. Prefers
the family-qualified key (`{family}.{suffix}`) and falls back to the legacy bare-suffix key, normalising
the OLDEST format (`{suffix => [features]}`, a bare array with no membership) to the current shape. Every
consumer (the channels endpoint, the observer summaries) goes through this so there is one place that
knows the sidecar's three historical layouts.
"""
function _clustfeatures_entry(props_path::AbstractString, suffix::AbstractString;
                              family::AbstractString = "clusters")
    raw = _clustfeatures_raw(props_path)
    for k in (_clustfeatures_key(suffix, family), String(suffix))
        v = get(raw, k, nothing)
        v isa AbstractDict && return v
        v isa AbstractVector && return Dict{String,Any}(     # oldest format: features only
            "features" => String[string(x) for x in v], "partOf" => String[], "labels" => Dict{String,Any}())
    end
    nothing
end

"""
    cluster_run_at(props_path, suffix; family="clusters") -> String | nothing

When run `suffix` was clustered (its clustfeatures entry's `at`), or `nothing` for a run written before
the stamp existed. A cluster population keeps the `at` it was picked against (`Population.run_at`); a
different one now means the run was re-run under the same name, so its cluster ids may mean other groups.
"""
function cluster_run_at(props_path::AbstractString, suffix::AbstractString;
                        family::AbstractString = "clusters")::Union{String,Nothing}
    e = _clustfeatures_entry(props_path, suffix; family = family)
    v = e === nothing ? nothing : get(e, "at", get(e, :at, nothing))
    v === nothing ? nothing : String(v)
end

# One run's `partOf` — the image uids clustered together — from a `_clustfeatures_entry`. A legacy entry
# records none: with `own_uid` that reads as a single-image run on that image, without it as `String[]`.
function _clustfeatures_part_of(entry, own_uid::Union{AbstractString,Nothing}=nothing)::Vector{String}
    v = entry === nothing ? nothing : get(entry, "partOf", get(entry, :partOf, nothing))
    part = v isa AbstractVector ? String[string(x) for x in v] : String[]
    (isempty(part) && own_uid !== nothing) ? [String(own_uid)] : part
end

"""
    run_channel_names(img, value_name) -> RunChannelNames

The `chans_of` a run's sidecar readers name legacy raw features with (`_clustfeatures_features`,
`_clustfeatures_axes`). Called with a uid it gives that image's channel names under `value_name` (`img`
itself or a sibling image of its project, loaded once; `nothing` when unreadable or none registered).
The readers ask it for the run's REFERENCE segment instead (`run_reference_names`).
"""
struct RunChannelNames
    img::CciaImage
    value_name::String
    sibs::Dict{String,Union{CciaImage,Nothing}}
end
run_channel_names(img::CciaImage, value_name::AbstractString) =
    RunChannelNames(img, String(value_name), Dict{String,Union{CciaImage,Nothing}}())

# A run's image by uid — `img` itself, or the sibling loaded from its state file (`nothing` if none).
function _run_image(r::RunChannelNames, uid::AbstractString)
    String(uid) == r.img.uid && return r.img
    get!(r.sibs, String(uid)) do
        f = state_file(dirname(dirname(r.img._dir)), String(uid))
        isfile(f) || return nothing
        try _load_image(dirname(f)) catch; nothing end
    end
end

_nonempty_names(n) = (n === nothing || isempty(n)) ? nothing : n
function (r::RunChannelNames)(uid::AbstractString)
    im = _run_image(r, uid)
    im === nothing ? nothing : _nonempty_names(channel_names(im; value_name = r.value_name))
end

"""
    run_reference_names(r, part, props_path, suffix; family) -> Vector{String} | nothing

The channel names a LEGACY run's raw features (`mean_intensity_N`) mean: those of the segment the run
picked them on. The task took the first segment of its pooled cells — the first image of `partOf` with
cells in the clustered populations, under ITS segmentation — not `partOf[1]` (the set's first image,
which may have had none). That segment is the first `(uid in partOf, value_name)` whose own sidecar of
the same kind as `props_path` (cell or `__tracks` table) records this run — same `partOf`, same features
and, when stamped, the same `at`. Two unstamped runs with identical features stay indistinguishable. The
sidecar does not record the task's pooled segment order, so these cases stay undecidable: a reference
whose entry was pruned since (`prune_stale_clustfeatures!`), overwritten by a later run under the same
suffix, or whose image is gone (the next match is used, often the reader itself), and a reference image
that took part with two segmentations (`versioned_keys` order). Falls
back to `partOf[1]` under `r.value_name` when no image of the run can be read.
"""
function run_reference_names(r::RunChannelNames, part::AbstractVector, props_path::AbstractString,
                             suffix::AbstractString; family::AbstractString = "clusters")
    track = endswith(basename(props_path), "$(TRACK_PROPS_SUFFIX).h5ad")
    # The same RUN, not just the same suffix: a suffix reused on another segmentation (two `default`
    # runs, one on A pops and one on B pops) or a later re-run over the same images records the same
    # `partOf`, so the candidate must also carry this entry's features and, when stamped, its `at`.
    own_feats = _clustfeatures_features(props_path, suffix; family = family)
    own_at = cluster_run_at(props_path, suffix; family = family)
    function same_run(p)
        e = _clustfeatures_entry(p, suffix; family = family)
        e !== nothing && _clustfeatures_part_of(e) == part &&
            _clustfeatures_features(p, suffix; family = family) == own_feats &&
            (own_at === nothing || cluster_run_at(p, suffix; family = family) == own_at)
    end
    for uid in part
        im = _run_image(r, uid)
        im === nothing && continue
        for vn in versioned_keys(im.label_props)
            v = String(vn)
            p = track ? img_track_props_path(im, v) : img_label_props_path(im, v)
            same_run(p) || continue
            return _nonempty_names(channel_names(im; value_name = v))
        end
    end
    r(first(part))
end

# The names a run's stored features are read against: the run's reference segment for a
# `run_channel_names` (`run_reference_names`); a plain `uid -> names` is asked for `partOf[1]`.
_run_chans(chans_of::RunChannelNames, part, props_path, suffix, family) =
    run_reference_names(chans_of, part, props_path, suffix; family = family)
_run_chans(chans_of, part, props_path, suffix, family) = chans_of(first(part))

"""
    _clustfeatures_axes(props_path, suffix; family, chans_of, own_uid) -> AbstractDict | nothing

A track run's `behaviourAxes` (axis names + top features), with each `top[].feature` named like
`_clustfeatures_features` names the run's features.
"""
function _clustfeatures_axes(props_path::AbstractString, suffix::AbstractString;
                             family::AbstractString = "clusters",
                             chans_of = nothing,
                             own_uid::Union{AbstractString,Nothing} = nothing)
    e = _clustfeatures_entry(props_path, suffix; family = family)
    e === nothing && return nothing
    a = get(e, "behaviourAxes", get(e, :behaviourAxes, nothing))
    a isa AbstractDict || return nothing
    part = _clustfeatures_part_of(e, own_uid)
    (chans_of === nothing || isempty(part)) && return a
    chans = _run_chans(chans_of, part, props_path, suffix, family)
    out = Dict{String,Any}(String(k) => v for (k, v) in a)
    axes = get(out, "axes", nothing)
    axes isa AbstractVector || return out
    out["axes"] = map(axes) do ax
        ax isa AbstractDict || return ax
        d = Dict{String,Any}(String(k) => v for (k, v) in ax)
        top = get(d, "top", nothing)
        top isa AbstractVector && (d["top"] = map(top) do t
            t isa AbstractDict || return t
            tt = Dict{String,Any}(String(k) => v for (k, v) in t)
            haskey(tt, "feature") && (tt["feature"] = only(canonical_channel_columns([string(tt["feature"])], chans)))
            tt
        end)
        d
    end
    out
end

# One run's recorded feature columns (the heatmap's row universe), `String[]` when unrecorded.
# Intensities by channel NAME (docs/todo/CHANNEL_NAMES_PLAN.md): a run recorded before that stored the
# raw columns of its first segment — the one the features were picked on — so `chans_of` (a
# `run_channel_names`, or a plain `uid -> names`) names them against that segment (`run_reference_names`;
# `own_uid` for a run with no `partOf`). Without `chans_of` the features come back as stored.
function _clustfeatures_features(props_path::AbstractString, suffix::AbstractString;
                                 family::AbstractString = "clusters",
                                 chans_of = nothing,
                                 own_uid::Union{AbstractString,Nothing} = nothing)::Vector{String}
    e = _clustfeatures_entry(props_path, suffix; family=family)
    e === nothing && return String[]
    f = get(e, "features", get(e, :features, nothing))
    feats = f isa AbstractVector ? String[string(x) for x in f] : String[]
    chans_of === nothing && return feats
    part = _clustfeatures_part_of(e, own_uid)
    (isempty(part) || !any(c -> _split_channel_column(c) !== nothing, feats)) && return feats
    canonical_channel_columns(feats, _run_chans(chans_of, part, props_path, suffix, family))
end

"""
    co_clustered_value_names(img, suffix; granularity=:cell, family="clusters") -> Vector{String}

The image's segmentations (value_names) that took part in clustering run `suffix` — i.e. whose
clustfeatures sidecar (`{props}.clustfeatures.json`) carries `suffix` for `family`. `granularity=:track`
reads the per-track table's sidecar (`trackclust`), `:cell` the cell table's (`clust`). `family` is the
obs-column family ("clusters" for clust/trackclust, "regions" for region — pass
`_cluster_measure_family(pop_type)`), so a region run and a cell-clustering run that share a suffix
resolve to their own member segmentations. First-appearance order of `versioned_keys(img.label_props)`.
Falls back to the active value_name when nothing is recorded (pre-clustfeatures runs), so callers always
get at least one segmentation.
"""
function co_clustered_value_names(img::CciaImage, suffix::AbstractString;
                                  granularity::Symbol=:cell,
                                  family::AbstractString="clusters")::Vector{String}
    out = String[]
    for vn in versioned_keys(img.label_props)
        v = String(vn)
        p = granularity === :track ? img_track_props_path(img, v) : img_label_props_path(img, v)
        String(suffix) in _clustfeatures_suffixes(p; family=family) && push!(out, v)
    end
    isempty(out) ? String[resolve_value_name(img)] : out
end

# The cluster-style pop types: each is a filter on a per-run `{prefix}{suffix}` obs column written by a
# clustering-family task, with the whole co-clustered-sibling auto-share machinery below applying
# uniformly. `clust`/`trackclust` filter `clusters.{suffix}` (clustPops/clustTracks); `region` filters
# `regions.{suffix}` (clustRegions) — spatial regions are region-clustering output, stored + shared with
# the identical mechanism as cell/track clusters (see docs/todo/SPATIAL_REGIONS_PLAN.md, Decision 5).
# `_is_cluster_pop_type` is the registry's `cluster` flag (registry.jl).

# The obs-column family a cluster-style pop type filters over — the registry's `cluster_prefix`; any
# non-cluster type keeps the `clusters.` answer it always had.
function _cluster_measure_prefix(pop_type)::String
    i = pop_type_info(pop_type)
    (i !== nothing && i.cluster) ? i.cluster_prefix : "clusters."
end
# …and the same decision without the dot, for the clustfeatures sidecar key (`{family}.{suffix}`) and
# any caller that needs the bare family name. Derived, never re-decided.
_cluster_measure_family(pop_type)::String = chopsuffix(_cluster_measure_prefix(pop_type), ".")

# The run a cluster population belongs to — the suffix of its `filter_measure` ("{prefix}{suffix}") — or
# `nothing` for a population that is not one (a gate, a filter on another column).
function cluster_pop_suffix(p::Population)::Union{String,Nothing}
    _is_cluster_pop_type(p.pop_type) || return nothing
    prefix, fm = _cluster_measure_prefix(p.pop_type), p.filter_measure
    (fm === nothing || !startswith(String(fm), prefix)) && return nothing
    String(fm)[ncodeunits(prefix)+1:end]
end

# The table a cluster pop type's runs are recorded on: the per-track table for track-grained types
# (trackclust), else the cell table — its `.clustfeatures.json` holds the run entries.
cluster_props_path(img::CciaImage, value_name::AbstractString, pop_type) =
    is_track_grained(pop_type) ? img_track_props_path(img, value_name) : img_label_props_path(img, value_name)

"""
    stamp_cluster_run!(p, img, value_name) -> Bool

Record on cluster population `p` the `at` of the run its cluster ids are picked against (`cluster_run_at`),
on `img`'s `value_name`. Called wherever the ids are chosen — a new cluster pop, a tick in the manager, a
Claude proposal — so a later re-run under the same name shows the population as stale. `false` (and
nothing changed) when `p` is not a cluster population.
"""
function stamp_cluster_run!(p::Population, img::CciaImage, value_name::AbstractString)::Bool
    sfx = cluster_pop_suffix(p)
    sfx === nothing && return false
    p.run_at = cluster_run_at(cluster_props_path(img, value_name, p.pop_type), sfx;
                              family = _cluster_measure_family(p.pop_type))
    true
end

"""
    cluster_id_owners(m, suffix; except=nothing) -> Dict{Int,String}

Which population of run `suffix` holds each cluster id on map `m`. A cluster id belongs to at most one
population of a run (the manager's tick moves it); the routes that write cluster ids refuse one another
population holds. `except` leaves out the population(s) being edited: one path, or a collection of them.
Only a population that CLAIMS its values (`_claims_values`: `in`/`eq`/legacy) holds them — a `neq 3`
population holds every cluster but 3, so it never holds 3.
"""
function cluster_id_owners(m::PopulationTree, suffix::AbstractString; except = nothing)::Dict{Int,String}
    out = Dict{Int,String}()
    skip(path) = except === nothing ? false : except isa AbstractString ? path == except : path in except
    for path in m.order
        skip(path) && continue
        p = m.pops[path]
        (cluster_pop_suffix(p) == suffix && _claims_values(p)) || continue
        for v in _as_value_vec(p.filter_values)
            id = as_cluster_id(v)
            id === nothing || get!(out, id, path)
        end
    end
    out
end

# A cluster id as written into a filter — 3, 3.0 or "3" — as the Int it names, or `nothing`.
as_cluster_id(::Bool) = nothing
as_cluster_id(v::Integer) = Int(v)
as_cluster_id(v::Real) = (isfinite(v) && isinteger(v)) ? Int(v) : nothing
as_cluster_id(v::AbstractString) = (x = tryparse(Float64, strip(v)); x === nothing ? nothing : as_cluster_id(x))
as_cluster_id(::Any) = nothing

# Suffixes a cluster pop tree's filters reference (each pop's `filter_measure` = "{prefix}{suffix}").
function _referenced_cluster_suffixes(m::PopulationTree)::Set{String}
    out = Set{String}()
    for p in values(m.pops)
        sfx = cluster_pop_suffix(p)
        sfx === nothing && continue
        push!(out, sfx)
    end
    out
end

# AUTO-SHARE: a segmentation that took part in a joint clustering but has no OWN named cluster-pop
# sidecar (the names were authored under a sibling value_name — e.g. under B while T got only the
# shared `clusters.{suffix}` column) borrows the sibling's map, RELABELED to itself so membership
# resolves over ITS own table. Guarded: only borrow when this vn shares the sibling map's referenced
# `clusters.{suffix}` (so we never fabricate cluster pops for a segmentation not in the run). Returns
# nothing when there's nothing to borrow. Read-side only — the SAVE path stays per-vn (editing under a
# borrowing vn materialises its own real sidecar: plain copy semantics).
function _borrow_cluster_pop_tree(img::CciaImage, value_name::AbstractString,
                                 pop_type::PopTypeArg)::Union{PopulationTree,Nothing}
    p = cluster_props_path(img, value_name, pop_type)
    my_suffixes = _clustfeatures_suffixes(p; family=_cluster_measure_family(pop_type))
    isempty(my_suffixes) && return nothing              # this vn wasn't clustered → nothing to share
    for vn in versioned_keys(img.label_props)
        v = String(vn); v == value_name && continue
        sib = load_pop_tree(img._dir, v; pop_type=pop_type)
        isempty(sib.pops) && continue
        ref = _referenced_cluster_suffixes(sib)
        (isempty(ref) || !(ref ⊆ my_suffixes)) && continue
        sib.value_name = value_name
        for pop in values(sib.pops); pop.value_name = value_name; end
        return sib
    end
    nothing
end

# ── Categorical colour-by palette (the ONE colour-choice rule) ─────────────────────
# The population palette — the default categorical colours. Equal to `population` in
# frontend/src/plots/palettes.json (pinned by frontend/src/utils/cvd.test.ts), so a category reads the
# same colour everywhere. The first 6 sit near Paul Tol's 'bright' hues (the agents-pane badges in
# share/console_palette.json), the rest chosen around them at moderate chroma. Each colour is
# >= 3:1 on the dark plot ground and a black image, >= 2:1 on a white export.
# Among the first 8 every pair is >= dE 8 under protan-, deuter- and tritanopia and >= 15 for normal
# vision; the last 4 hold >= 6 / >= 13 against all (a pop's name is the second cue). Past 12 it repeats.
const POPULATION_PALETTE = ["#1271C5", "#F36E9F", "#547606", "#94C56F", "#45C1FB", "#AD4194", "#BC844A", "#A37EF3",
                            "#D1AFB2", "#3AA085", "#FFA21E", "#826463"]

_as_value_vec(x) = x === nothing ? Any[] : (x isa AbstractVector ? collect(x) : Any[x])
# tolerant value equality: exact, string-wise, or numeric (a filter value stored as 2.0 == column 2)
_val_eq(a, b) = a == b || string(a) == string(b) ||
    (a isa Real && b isa Real && Float64(a) == Float64(b))

# Does `p` CLAIM its `filter_values` — i.e. select exactly those values? Only an `in`/`eq` filter does
# (`nothing` = a legacy cluster pop, stored before the fun was recorded, which was always `in`). A
# `neq 3` or `gt 2` population holds everything EXCEPT/ABOVE its values, so naming value 3 after it
# would label exactly the objects it excludes.
_claims_values(p) = p.filter_fun === nothing || p.filter_fun === FILTER_IN || p.filter_fun === FILTER_EQ

# canonical string key for a category value on the wire (2.0 and 2 → "2") so Julia overrides and the
# bridge's column values match regardless of Int/Float encoding.
_val_key(v) = v isa Real ? (isinteger(v) ? string(Int(v)) : string(v)) : string(v)

"""
    pop_label_overrides(m, column) -> Dict{String,String}

The `{value => population name}` a user population gives `column`: for each pop whose
`filter_measure == column`, its `filter_values` → the pop's `name`, keyed by the value's canonical
STRING form. Lets a colour-by legend read the **population name** (e.g. "migratory") instead of the raw
category value (e.g. "2") wherever a population defines that value. First pop in insertion order wins.
"""
function pop_label_overrides(m::PopulationTree, column::AbstractString)::Dict{String,String}
    col = String(column); out = Dict{String,String}()
    for path in m.order
        p = m.pops[path]
        (p.filter_measure === nothing || String(p.filter_measure) != col || !_claims_values(p)) && continue
        for fv in _as_value_vec(p.filter_values)
            k = _val_key(fv)
            haskey(out, k) || (out[k] = p.name)
        end
    end
    out
end

"""
    category_sort_key(v)

The order categories take everywhere — legends, palette slots, the viewer, the UMAP: numbers by value
(so cluster 10 comes after 9, not after 1), then anything else by its text. Mirrored by
`categorySortKey` in frontend/src/plots/categoryColours.ts.
"""
category_sort_key(v) = v isa Real && isfinite(Float64(v)) ? (0, Float64(v), "") : (1, 0.0, string(v))

"""
    category_colours(values; palette=POPULATION_PALETTE) -> Dict{Any,String}

THE category colour rule, pure: every value takes the palette slot of its position among ALL the
distinct values in `category_sort_key` order. A population on a value never repaints it — a cluster
keeps its own colour, so a population and a cluster that happen to share a colour are never mistaken
for one another, and turning a population on or off never repaints a category. General across
clusters, HMM states and any categorical measure. Mirrored by `categoryColours` in
frontend/src/plots/categoryColours.ts; both are checked against share/category_colour_cases.json.
"""
function category_colours(values; palette::AbstractVector{<:AbstractString}=POPULATION_PALETTE)::Dict{Any,String}
    vals = sort!(unique(collect(values)); by = category_sort_key)
    Dict{Any,String}(v => palette[(i - 1) % length(palette) + 1] for (i, v) in enumerate(vals))
end

function load_pop_tree(img::CciaImage; value_name::AbstractString="default", pop_type::PopTypeArg="flow",
                      backfill_save::Bool=true)
    m = load_pop_tree(img._dir, value_name; pop_type=pop_type, backfill_save=backfill_save)
    # Stamp THIS image's µm/px so `recompute!` can put spatial gate axes and the cell data in the same
    # unit. Per image on purpose: the same µm gate copied to another image must be evaluated with that
    # image's own scale (SPATIAL_GATE_UNITS_PLAN.md decision 2). Only for a calibrated image — an
    # uncalibrated one has no µm, and `img_physical_sizes`' 1.0 default would masquerade as one.
    img_is_calibrated(img) && (m.physical_sizes = first(img_physical_sizes(img)))
    # ── adopt µm whenever there is nothing to reinterpret ──
    # The unit stamp only constrains a file that ALREADY holds position coordinates: with no spatial
    # gate in it there are no numbers whose meaning could change, so a calibrated image upgrades the map
    # to µm here — on a brand-new map and equally on a long-standing intensity-only one. That is what
    # makes the migration unnecessary rather than merely unused (SPATIAL_GATE_UNITS_PLAN.md decision 8):
    # every existing gating file adopts µm the next time it is saved, and the first position gate anyone
    # draws is already physical.
    #
    # The two cases it deliberately leaves alone:
    #   • a map that DOES have a position gate — its coordinates were drawn in the stamped unit, and
    #     re-stamping would silently move every one of them;
    #   • an uncalibrated image — there is no µm to adopt, and `img_physical_sizes`' 1.0 default would
    #     masquerade as one, so it stays px until a pixel size is set.
    (img_is_calibrated(img) && !has_spatial_gate(m)) && (m.spatial_unit = SPATIAL_UNIT_UM)
    # cluster pop_types with no own sidecar → try to borrow from a co-clustered sibling (auto-share)
    (_is_cluster_pop_type(pop_type) && isempty(m.pops)) || return m
    borrowed = _borrow_cluster_pop_tree(img, String(value_name), string(pop_type))
    borrowed === nothing && return m
    # a borrowed map came from a sibling segmentation of the SAME image → same pixel sizes
    borrowed.physical_sizes = m.physical_sizes
    borrowed
end

"""
    stored_pop_tree(img, value_name, pop_type) -> Union{PopulationTree,Nothing}

The map holding `value_name`'s stored `pop_type` populations, or `nothing` when it can have none. A
gate-drawn map (flow/track/region…) is per segmentation, so no sidecar file means no pops — the cheap
`isfile` skip. A CLUSTER pop_type often has no sidecar of its own: cluster pops are global to a run and
`load_pop_tree` lends them to every co-clustered sibling (`_borrow_cluster_pop_tree`), so it is always
loaded — skipping on the file hid exactly those borrowers. The one walk every "which pops does this
image have" reader shares (`has_stored_pops`, the observer's population enumerator).
"""
function stored_pop_tree(img::CciaImage, value_name::AbstractString, pop_type::AbstractString)
    (_is_cluster_pop_type(pop_type) || isfile(gating_path(img._dir, value_name; pop_type = pop_type))) ||
        return nothing
    load_pop_tree(img; value_name = value_name, pop_type = pop_type)
end

"""True when `value_name` has at least one stored (non-transient) `pop_type` population. Membership is
not evaluated — an authored pop that matches no cell still counts: the type exists on this image."""
function has_stored_pops(img::CciaImage, value_name::AbstractString, pop_type::AbstractString)::Bool
    m = stored_pop_tree(img, value_name, pop_type)
    m !== nothing && any(path -> !pop_at(m, path).transient, pop_paths(m))
end

# Old-R `popDT(popType="clust", pops=c("A","B","C"))` returned those cluster pops across ALL
# segmentations used in the clustering run — pooled, tagged by value_name. Cluster pops are GLOBAL to a
# run (one shared `clusters.{suffix}` column across its segments), NOT per-segmentation like gates. So a
# BARE cluster-pop reference (root-relative `/A`, no value_name prefix) expands to EVERY co-clustered
# value_name; membership is then evaluated against each segment's own table (own or auto-shared def) and
# the rows pooled + value_name-tagged by the normal pop_df machinery. A value_name-prefixed ref
# ("T/A") is explicit and passes through unchanged (so a single-segmentation request still works). The
# run is identified from the pop's own definition (`filter_measure = clusters.{suffix}`).
function _expand_cluster_pops(img::CciaImage, pops, pop_type::PopTypeArg, default_vn::AbstractString)
    _is_cluster_pop_type(pop_type) || return pops
    granularity = string(pop_type) == "trackclust" ? :track : :cell
    vns = versioned_keys(img.label_props)
    out = String[]
    for p0 in pops
        p = String(p0)
        if !(startswith(p, "/") || is_root(p)); push!(out, p); continue; end   # explicit "vn/path" → keep
        is_root(p) && (push!(out, p); continue)                                # root has no cluster expansion
        # find the run this pop belongs to: a value_name whose OWN sidecar defines it as a cluster filter
        prefix = _cluster_measure_prefix(pop_type)
        suffix = nothing
        for vn in vns
            m = load_pop_tree(img._dir, String(vn); pop_type=pop_type)
            has_pop(m, p) || continue
            fm = m.pops[p].filter_measure
            (fm !== nothing && startswith(String(fm), prefix)) || continue
            suffix = String(fm)[ncodeunits(prefix)+1:end]; break
        end
        suffix === nothing && (push!(out, p); continue)                        # unknown → leave to default_vn
        for vn in co_clustered_value_names(img, suffix; granularity=granularity,
                                           family=_cluster_measure_family(pop_type))
            push!(out, "$(vn)$(p)")                                            # "/A" → "B/A", "T/A", …
        end
    end
    unique(out)
end

