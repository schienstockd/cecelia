# Refine runs — gating with clustering (docs/todo/CLUSTER_REFINE_PLAN.md).
#
# A coarse clustering run gives the first level of populations; a run restricted to ONE of those cluster
# populations, usually at a higher resolution, gives its children. Nothing new is stored for the cells:
# the refine run is an ordinary `clusters.{suffix}` column (NaN outside the source population). What this
# file adds is the run's LINEAGE in its clustfeatures entry — `refines: {pop, run, clusters, runAt}` —
# and the readers built on it: where a refine run's populations go (`refine_source_pop` — under it, and
# refused on an image without it, never at the root: Decision 4) and whether the run still matches its
# source (`cluster_refine_stale`, Decision 8).

# A field of a sidecar entry read from JSON (String or Symbol keys).
_entry_get(e, k::AbstractString, default = nothing) =
    e isa AbstractDict ? get(e, k, get(e, Symbol(k), default)) : default

# The sorted cluster ids a cluster population picks — what a refine run records of its source.
_picked_cluster_ids(p::Population)::Vector{Int} =
    sort!(unique!(Int[id for id in as_cluster_id.(_as_value_vec(p.filter_values)) if id !== nothing]))

"""
    refine_source(imgs, pops) -> Dict | nothing

What a `clustPops` run over `pops` refines: one cell cluster population (the same path on every
segmentation picked) → `Dict("pop", "run", "clusters", "runAt")` as found on the first image that has
it — call it per segment (`value_name`), so each (image, segmentation) records its own source; anything else — several populations, a gate, a region — → `nothing`: a plain restricted run with
no lineage (Decision 1). Decision 5: only a cluster population can parent cluster populations.
"""
function refine_source(imgs::AbstractVector{<:CciaImage}, pops;
                       value_name::Union{AbstractString,Nothing} = nothing)::Union{Dict{String,Any},Nothing}
    (isempty(imgs) || isempty(pops)) && return nothing
    refs = [_split_pop_ref(r, resolve_value_name(first(imgs))) for r in pops]
    length(unique(last.(refs))) == 1 || return nothing
    path = last(first(refs))
    vns = unique(first.(refs))
    value_name === nothing || (vns = filter(==(String(value_name)), vns))
    for img in imgs, vn in vns
        # the run reads the population the mixed-type resolver finds — a gate at the same path wins over
        # a cluster population there, and then the run refines nothing
        resolve_pop_type(img, vn, path) == "clust" || continue
        m = try load_pop_tree(img; value_name = vn, pop_type = "clust", backfill_save = false) catch; continue end
        has_pop(m, path) || continue
        p = m.pops[path]
        run = cluster_pop_suffix(p)
        run === nothing && return nothing
        return Dict{String,Any}("pop" => path, "run" => run, "clusters" => _picked_cluster_ids(p),
                                "runAt" => cluster_run_at(img_label_props_path(img, vn), run))
    end
    nothing
end

# A run's `refines` from its clustfeatures entry, or `nothing` for a run that refines nothing.
function _clustfeatures_refines(entry)
    r = _entry_get(entry, "refines")
    r isa AbstractDict || return nothing
    at = _entry_get(r, "runAt")
    (; pop = String(_entry_get(r, "pop", "")), run = String(_entry_get(r, "run", "")),
       clusters = Int[Int(x) for x in _entry_get(r, "clusters", Int[])],
       runAt = at === nothing ? nothing : String(at))
end

# A run's recorded task params (resolution, normalisation, …), or an empty Dict for a run recorded
# before params were (Decision 3 — shown as "res ?").
function _clustfeatures_params(entry)::Dict{String,Any}
    v = _entry_get(entry, "params")
    v isa AbstractDict ? Dict{String,Any}(String(k) => x for (k, x) in v) : Dict{String,Any}()
end

# The population run `suffix` refines (its recorded path), or `nothing` for a run that refines nothing. A
# new population of the run goes under it — `pop/add` with no parent and `add-cluster-pops`, both refusing
# (404) on an image that lacks it, so the run's images keep the same paths.
refine_source_pop(img::CciaImage, value_name::AbstractString, suffix::AbstractString) =
    (r = _clustfeatures_refines(_clustfeatures_entry(img_label_props_path(img, value_name), suffix));
     r === nothing ? nothing : r.pop)

"""
    cluster_run_info(img, value_name, props_path, suffix; family="clusters", m=nothing) -> Dict | nothing

One run as the cluster page and the observer read it: `{params, refines?: {pop, run}, stale?}` — the
params it recorded and, for a cell refine run (`m` = the segmentation's clust pop tree), what it refines
and why it is stale. `nothing` when `props_path` records no such run. The one builder for that shape.
"""
function cluster_run_info(img::CciaImage, value_name::AbstractString, props_path::AbstractString,
                          suffix::AbstractString; family::AbstractString = "clusters",
                          m::Union{PopulationTree,Nothing} = nothing)
    e = _clustfeatures_entry(props_path, suffix; family = family)
    e === nothing && return nothing
    run = Dict{String,Any}("params" => _clustfeatures_params(e))
    r = m === nothing ? nothing : _clustfeatures_refines(e)
    if r !== nothing
        run["refines"] = Dict{String,Any}("pop" => r.pop, "run" => r.run)
        why = cluster_refine_stale(img, value_name, suffix; m = m)
        why === nothing || (run["stale"] = why)
    end
    run
end

"""
    cluster_refine_stale(img, value_name, suffix; m=nothing) -> String | nothing

Why refine run `suffix` no longer matches what it refined, or `nothing` when it still does (or refines
nothing). Stale when the source population is gone, picks other clusters or another run, or its run was
deleted or re-run since — and, up the chain, when the run it refines is itself stale (Decision 8). `m` = the
segmentation's clust pop tree when the caller already holds it.
"""
function cluster_refine_stale(img::CciaImage, value_name::AbstractString, suffix::AbstractString;
                              m::Union{PopulationTree,Nothing} = nothing,
                              seen::Set{String} = Set{String}())::Union{String,Nothing}
    String(suffix) in seen && return nothing           # a cycle cannot be built, but never loop on one
    push!(seen, String(suffix))
    props = img_label_props_path(img, value_name)
    r = _clustfeatures_refines(_clustfeatures_entry(props, suffix))
    r === nothing && return nothing
    m = m === nothing ? load_pop_tree(img; value_name = value_name, pop_type = "clust", backfill_save = false) : m
    has_pop(m, r.pop) || return "$(pop_name(r.pop)) no longer exists"
    p = m.pops[r.pop]
    cluster_pop_suffix(p) == r.run || return "$(pop_name(r.pop)) no longer picks from run $(r.run)"
    _picked_cluster_ids(p) == r.clusters || return "$(pop_name(r.pop)) picks other clusters now"
    _clustfeatures_entry(props, r.run) === nothing && return "run $(r.run) no longer exists"
    r.runAt !== nothing && cluster_run_at(props, r.run) != r.runAt && return "run $(r.run) was re-run"
    up = cluster_refine_stale(img, value_name, r.run; m = m, seen = seen)
    up === nothing ? nothing : "run $(r.run) is stale: $up"
end
