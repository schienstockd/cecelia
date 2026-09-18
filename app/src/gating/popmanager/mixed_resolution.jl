# Mixed-type population resolution — a module-function picker offers ANY pop type, so the plot
# body cannot switch on one shape and needs a uniform resolver. Given a mixed-type row list,
# resolves each row to its DataFrame through the right pop_df variant (leaf, boolean, derived,
# cluster, region, track) and returns a common-shaped result the plot iterates without branching.
# Loaded last because it consumes every earlier resolver in the family.

# ── Mixed-type population resolution (module-function pickers offer ANY pop type) ───────────────────
# A module function's popSelection can offer populations of ANY type at once — cell gates, cell
# clusters, spatial regions, tracked cells, per-track gates/clusters (docs/POPULATION.md → *Module-
# function pop inputs*). But `pop_df` takes ONE `pop_type`, and the selected VALUE is only a
# value-name-prefixed path (`"A/qc/_tracked"`) — it does NOT carry the pop's type. So a consumer that
# wants "these cells, whatever population type the user picked" must DISCOVER each pop's type first.
# `pop_df_multi` is that ONE canonical mixed-type accessor: probe each pop's type, group, call `pop_df`
# per type at the requested granularity, and stack. This replaces the per-task `get(params,"popType",
# "flow")` default — the bug that made every spatial/cluster task assume `flow`, so a region / cluster /
# tracked pop came back empty and tracked cells never appeared in those functions' pickers.

# Split a (possibly value-name-prefixed) pop ref into (value_name, path) — same grammar as
# `_group_pops_by_value_name` (prefix names the vn; a leading "/" or root stays in `default_vn`).
function _split_pop_ref(ref, default_vn::AbstractString)::Tuple{String,String}
    p = String(ref)
    (startswith(p, "/") || is_root(p)) && return (String(default_vn), p)
    idx = findfirst('/', p)
    idx === nothing ? (String(default_vn), "/" * p) : (p[1:idx-1], p[idx:end])
end

# Probe order for pops whose type is unknown from the value alone. A `_tracked` (or any reserved
# derived) leaf resolves to its registered pop_type (`live`) up front; otherwise the first stored map
# that CONTAINS the path wins (name collisions across types are pathological). `flow` is probed first so
# a plain gate resolves to cells, not to a like-named cluster/region pop.
const _POP_TYPE_PROBE_ORDER = ("flow", "clust", "region", "track", "trackclust", "branch")

"""
    resolve_pop_type(img, value_name, path) -> String

The `pop_type` a single selected population must be fetched under, discovered from the stored maps —
because the picker value carries only the path, not the type. A reserved derived leaf (`_tracked`)
→ its registered pop_type (`live`); the all-cells root → `flow`; otherwise the first map in
`_POP_TYPE_PROBE_ORDER` that contains the path. Unknown → `flow` (membership then resolves empty
downstream rather than erroring). This is why `pop_df_multi` can accept a mix of gates, clusters,
regions and tracked cells from one picker.
"""
function resolve_pop_type(img::CciaImage, value_name::AbstractString, path::AbstractString)::String
    for seg in split(String(path), '/'; keepempty=false)         # derived leaf (e.g. _tracked) → live
        haskey(_DERIVED_POPS, String(seg)) && return _DERIVED_POPS[String(seg)].pop_type
    end
    is_root(path) && return "flow"                               # all-cells root → cells
    # Collect EVERY map the path exists in — not just the first. A single path in two maps (a gate and
    # a like-named region, say) is ambiguous: the picker value carries only the path, so discovery can't
    # know which the user meant. Same-name guard: pick by priority but @warn so it's never silent (the
    # user renames to disambiguate). Reusing a name across types is allowed — only a *collision that is
    # actually selected* is flagged, and only where discovery must guess.
    matches = String[]
    for pt in _POP_TYPE_PROBE_ORDER
        m = try; load_pop_map(img; value_name=value_name, pop_type=pt); catch; continue; end
        has_pop(m, String(path)) && push!(matches, pt)
    end
    isempty(matches) && return "flow"
    length(matches) > 1 && @warn "population path resolves to more than one pop type — using the \
        first by priority; rename to disambiguate" value_name path candidates=matches chosen=first(matches) maxlog=3
    first(matches)
end

# Group refs by discovered pop_type, preserving first-appearance order (so dedup below keeps the
# first-selected type on a cell that falls in pops of several types). Returns ordered pop_type => refs.
function _group_pops_by_type(img::CciaImage, pops, default_vn::AbstractString)::Vector{Pair{String,Vector{String}}}
    groups = Pair{String,Vector{String}}[]
    idx = Dict{String,Int}()
    for ref in pops
        vn, path = _split_pop_ref(ref, default_vn)
        pt = resolve_pop_type(img, vn, path)
        if haskey(idx, pt)
            push!(groups[idx[pt]].second, String(ref))
        else
            push!(groups, pt => String[String(ref)]); idx[pt] = length(groups)
        end
    end
    groups
end

"""
    pop_df_multi(img, pops; granularity=:cell, value_name=nothing, unique_labels=true, kwargs...) -> DataFrame

`pop_df` over a MIX of population types — the accessor for a module function whose popSelection offers
all pop types (cell gates + clusters + regions + tracked/per-track pops). Each `pops` ref's type is
discovered (`resolve_pop_type`), refs are grouped by type, `pop_df` is called per type at `granularity`
(track/trackclust pops expand to their member cells when `granularity=:cell`), and the frames are
stacked. With `unique_labels` (default) a cell/track that fell into pops of DIFFERENT types is collapsed
to one row (first-selected type wins). `restrict_to=vn` keeps only rows of segmentation `vn` (the
single-segmentation guard — a task operating on one segmentation drops pops picked from another). All
other `kwargs` (`pop_cols`, `include_x`, `drop_na`, …) pass straight through to each per-type `pop_df`.
Use this — do NOT hand-roll a single-`pop_type` default.
"""
function pop_df_multi(img::CciaImage, pops; granularity::Symbol=:cell,
                      value_name::Union{AbstractString,Nothing}=nothing,
                      unique_labels::Bool=true,
                      restrict_to::Union{AbstractString,Nothing}=nothing, kwargs...)::DataFrame
    resolved_vn = resolve_value_name(img, value_name)
    frames = DataFrame[]
    for (pt, refs) in _group_pops_by_type(img, pops, resolved_vn)
        d = pop_df(img, pt, refs; value_name=resolved_vn, granularity=granularity,
                   unique_labels=unique_labels, kwargs...)
        nrow(d) > 0 && push!(frames, d)
    end
    isempty(frames) && return DataFrame()
    df = reduce((a, b) -> vcat(a, b; cols=:union), frames)
    # SINGLE-segmentation guard: a task that operates on ONE segmentation passes `restrict_to` = its
    # value_name, so a pop picked from ANOTHER segmentation (the across-mode picker lists them all,
    # value-name-prefixed) can't slip in and index the wrong segmentation's centroids/labels. Pooled
    # tasks (clustPops/clustRegions/neighbourStats) leave it unset and keep every segmentation.
    if restrict_to !== nothing && nrow(df) > 0 && "value_name" in names(df)
        df = df[df.value_name .== String(restrict_to), :]
    end
    if unique_labels && nrow(df) > 0
        key = intersect(granularity === :track ? [:uID, :value_name, :track_id] : [:uID, :value_name, :label],
                        propertynames(df))
        isempty(key) || (df = unique(df, key))
    end
    df
end

"""
    pop_df_multi(imgs, uids, pops; kwargs...) -> DataFrame

Set-level mixed-type pooling: run `pop_df_multi` per image (each discovers its OWN pops' types, so a
pop present on only some images is skipped where absent) and stack with a `uID` column — the mixed-type
analogue of the set-level `pop_df`.
"""
function pop_df_multi(imgs::AbstractVector{<:CciaImage}, uids::AbstractVector, pops; kwargs...)::DataFrame
    length(imgs) == length(uids) ||
        error("pop_df_multi: imgs and uids must be parallel (got $(length(imgs)) vs $(length(uids)))")
    frames = DataFrame[]
    for (img, uid) in zip(imgs, uids)
        d = pop_df_multi(img, pops; kwargs...)
        nrow(d) == 0 && continue
        d[!, :uID] = fill(String(uid), nrow(d))
        push!(frames, d)
    end
    isempty(frames) ? DataFrame() : reduce((a, b) -> vcat(a, b; cols=:union), frames)
end

"""
    pop_namespace(img, pops; value_name=nothing) -> String

The measure-namespace `pop_type` a per-cell ANNOTATION task writes its output columns under
(`<ns>.cell.contact#…`, `<ns>.cell.is.aggregate`), derived from the selected populations: any TRACKED
source (a `_tracked` derived pop, or a `track`/`trackclust` pop expanded to cells) → `"live"`;
otherwise `"flow"`. So a contacts/aggregates run over tracked cells now banks `live.cell.*` (R parity)
instead of always `flow.cell.*`. Cluster/region selections are just cell selections → `"flow"`. Empty
selection → `"flow"`.
"""
function pop_namespace(img::CciaImage, pops; value_name::Union{AbstractString,Nothing}=nothing)::String
    isempty(pops) && return "flow"
    resolved_vn = resolve_value_name(img, value_name)
    for ref in pops
        vn, path = _split_pop_ref(ref, resolved_vn)
        occursin("_tracked", path) && return "live"
        resolve_pop_type(img, vn, path) in ("track", "trackclust") && return "live"
    end
    "flow"
end

"""
    pops_value_name(pops; default="default") -> String

The segmentation (value_name) a single-segmentation module task operates on, DERIVED from its picked
populations rather than a separate dropdown — every picker value is value-name-prefixed
(`"B/pos"`, and the all-cells root `"B/"`), so the segmentation is already implied by the selection
(legacy parity: R never had a segmentation dropdown for the point/aggregate spatial tasks, and used
the one on `cellNeighbours` only to name the output). All refs in these pickers share one value_name
(the params are `acrossSegmentations:false`); the first ref decides it. Distinct value_names ⇒ `@warn`
(a mixed pick that shouldn't reach here) and the first still wins. Empty ⇒ `default`.
"""
function pops_value_name(pops; default::AbstractString="default")::String
    isempty(pops) && return String(default)
    vns = unique(String(_split_pop_ref(ref, default)[1]) for ref in pops)
    length(vns) > 1 && @warn "populations span more than one segmentation — using the first; this \
        picker is single-segmentation (acrossSegmentations:false)" value_names=vns chosen=first(vns) maxlog=3
    first(vns)
end

# Pop types that hold user-named populations (the name-guard scans all of them). `live` is not stored
# (it reads the flow gates), so `flow` covers it.
const _NAME_GUARD_POP_TYPES = ("flow", "track", "clust", "trackclust", "region", "branch")

"""
    pop_name_conflict(img, value_name, path; pop_type) -> Union{String,Nothing}

Cross-pop-type NAME-uniqueness guard. The mixed-type module picker resolves a selected population by its
PATH alone (`resolve_pop_type`), so the SAME path under two pop types in ONE segmentation is ambiguous.
This returns the pop_type of an existing population at `path` under a **different** type of `value_name`
(or `nothing`), so a create/rename that would collide can be rejected before it lands (the API guard).
`flow`/`live` share one map (live reads the flow gates) and count as the same type. The all-cells root
and reserved derived leaves (`_tracked`) are exempt. Per-segmentation only — cluster/region names
legitimately repeat across co-clustered segmentations (different value_names).
"""
function pop_name_conflict(img::CciaImage, value_name::AbstractString, path::AbstractString;
                           pop_type::PopTypeArg)::Union{String,Nothing}
    p = String(path)
    (is_root(p) || is_reserved_pop_name(pop_name(p))) && return nothing
    same_as_flow = string(pop_type) in ("flow", "live")
    for pt in _NAME_GUARD_POP_TYPES
        (same_as_flow && pt == "flow") && continue
        pt == string(pop_type) && continue
        m = try; load_pop_map(img; value_name=value_name, pop_type=pt); catch; continue; end
        has_pop(m, p) && return pt
    end
    nothing
end
