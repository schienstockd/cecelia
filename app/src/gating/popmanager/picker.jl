# Summary-canvas population picker + popScope. Two adjacent concerns: the picker turns a plot's
# authored `popSelection` into a concrete list of {image, value_name, pop_path, label} rows the
# plot iterates (logic lives HERE — `api/plotting_api.jl` is a thin transport), and popScope names
# the two object scopes a MODULE FUNCTION's picker offers (docs/ANALYSIS.md → Scope). Loaded after
# pop_df.jl because the picker resolves each row through pop_df at plot time.

# ── Summary-canvas population picker (logic lives here, NOT in the API — api/plotting_api.jl is a
#    thin wrapper; this is Revise-tracked + headless-testable per docs/ARCHITECTURE.md) ────────────

"""
    flatten_pop_tree(tree) -> Vector{Tuple{String,String,String}}

Flatten a `to_tree` population tree into `(path, name, colour)` in tree (pre-order) order. Accepts
String or Symbol keys (JSON3 round-trips yield Symbols).
"""
function flatten_pop_tree(tree)::Vector{Tuple{String,String,String}}
    out = Tuple{String,String,String}[]
    walk(nodes, parent) = for n in nodes
        name = string(get(n, "name", get(n, :name, "")))
        path = parent == "" ? "/" * name : parent * "/" * name
        push!(out, (path, name, string(get(n, "colour", get(n, :colour, "#7c93b8")))))
        walk(get(n, "children", get(n, :children, [])), path)
    end
    walk(get(tree, "populations", get(tree, :populations, [])), "")
    out
end

"""
    plot_pop_types(pop_type, granularity) -> Vector{String}

Pop types the summary picker surfaces for a plot's granularity. A **track**-granularity plot unions
`live` pops (cell gates + the derived `/_tracked`, aggregated one-point-per-track) with `track` pops
(gated directly on per-track measures, from `{vn}__tracks.json`); a cell plot uses just `pop_type`.
"""
plot_pop_types(pop_type::PopTypeArg, granularity::AbstractString)::Vector{String} =
    granularity == "track" ? unique(String[pop_type, "track"]) : String[pop_type]

"""
    plot_population_groups(imgs, value_names_for, load_map, pop_types) -> Vector{NamedTuple}

Build the summary-canvas picker list: populations available across `imgs`, grouped by segmentation
(value_name). Unioned across images (dedup by `(pop_type, path)`, first image wins name/colour) and
across `pop_types`, with derived pops (`derived_pop_paths`) prepended per pop_type. Each population is
tagged with the `pop_type` it must be fetched under. `value_names_for(img)` and `load_map(img, vn,
pt)` are injected (the API passes `versioned_keys` / `load_pop_map` closures) so this is pure and
headless-testable. Returns `[(value_name, populations=[(path, name, colour, pop_type)])]` in
first-appearance order; `load_map` returning `nothing`/throwing for a missing (vn, pop_type) is
skipped.
"""
function plot_population_groups(imgs, value_names_for::Function, load_map::Function,
                                pop_types::Vector{String};
                                derived_ok::Function = (_v, _pt, _parent, _dpath) -> true)
    # Gateless pop_type (`labels` = ungated all-cells, R parity): there is no gating map to flatten —
    # each segmentation IS its own population, named by its value_name. One selectable entry per vn, so
    # the user overlays whole segmentations (B, T, …) side by side. The path is the fixed "/labels" tag
    # the `labels` pop_df branch stamps (matched by `_series_groups` at plot time); it must start with
    # "/" like any pop path so the manager-form id (`value_name + path`) round-trips through the
    # frontend's `tkey`/`parseTkey` and colour map.
    if all(==("labels"), pop_types)
        vn_order = String[]
        for img in imgs, vn in value_names_for(img)
            v = String(vn); v in vn_order || push!(vn_order, v)
        end
        return [(value_name = v,
                 populations = [(path = "/labels", name = v, colour = "#7c93b8", pop_type = "labels")])
                for v in vn_order]
    end
    vn_order = String[]
    order = Dict{String,Vector{Tuple{String,String}}}()                       # vn → ordered (pt, path)
    meta  = Dict{String,Dict{Tuple{String,String},Tuple{String,String,String}}}()  # vn → key → (name,colour,pt)
    for img in imgs
        for vn in value_names_for(img)
            v = String(vn)
            haskey(order, v) || (push!(vn_order, v); order[v] = Tuple{String,String}[];
                                  meta[v] = Dict{Tuple{String,String},Tuple{String,String,String}}())
            for pt in pop_types
                m = try; load_map(img, v, pt); catch; nothing; end
                m === nothing && continue
                for (path, name, colour) in flatten_pop_tree(to_tree(m))
                    key = (pt, path); haskey(meta[v], key) && continue
                    push!(order[v], key); meta[v][key] = (name, colour, pt)
                end
            end
        end
    end
    # Derived pops (e.g. `_tracked` = track_id>0) are injected at query time, not stored. Offered at
    # root (`/_tracked` = all tracked cells) and under a stored pop (`/qc/_tracked` = qc's tracked
    # subset) — but only where `derived_ok` says the set is real and not a copy of a deeper one, so
    # the picker shows tracking where it happened instead of a `_tracked` per population that exists
    # (`tracked_pop_parents`). Rebuild the order so a derived child directly follows its parent
    # (root-level derived first).
    for v in vn_order
        rebuilt = Tuple{String,String}[]
        for pt in pop_types, dpath in derived_pop_paths(pt)              # root-level derived, at the top
            derived_ok(v, pt, "", dpath) || continue                     # e.g. hide root /_tracked when gated
            key = (pt, dpath)
            haskey(meta[v], key) || (meta[v][key] = (pop_name(dpath), "#7c93b8", pt))
            key in rebuilt || push!(rebuilt, key)
        end
        for (pt, path) in order[v]                                       # each stored pop, then its derived children
            push!(rebuilt, (pt, path))
            parent_colour = meta[v][(pt, path)][2]                       # inherit the stored pop's colour…
            for dpath in derived_pop_paths(pt)
                derived_ok(v, pt, path, dpath) || continue
                cpath = path * dpath                                     # dpath starts with "/" → "/qc" * "/_tracked"
                key = (pt, cpath); haskey(meta[v], key) && continue
                # …so a derived child (e.g. /qc/_tracked) shows in its parent's colour rather than a
                # generic grey. The parent's colour IS editable (gating page) but the derived child's
                # isn't (read-only on the behaviour page), so propagating keeps them visually paired.
                meta[v][key] = (pop_name(dpath), parent_colour, pt); push!(rebuilt, key)
            end
        end
        order[v] = rebuilt
    end
    [(value_name = v,
      populations = [(path = p, name = meta[v][(pt, p)][1], colour = meta[v][(pt, p)][2], pop_type = pt)
                     for (pt, p) in order[v]])
     for v in vn_order]
end

# ── popScope: the two object scopes a MODULE FUNCTION's population picker offers (docs/ANALYSIS.md).
#    A task's popSelection param declares `popScope` = "cells" or "tracks" instead of hand-rolling a
#    raw pop_type per module. This is the Julia parity of the old R `isTrack` pop-map attribute + the
#    `tracksOnly` flag: "cells" = per-cell populations (flow gates [+ clust clusters]); "tracks" =
#    per-track populations (the derived `_tracked` sets [+ per-track gates + trackclust clusters]).
#    Clustering-derived pops are included by default; a picker opts out with `includeClusters=false`.
#    Resolution lives here (package: Revise-tracked + headless-testable per docs/ARCHITECTURE.md);
#    api/plotting_api.jl stays a thin wrapper. New scopes slot in via `scope_pop_types`. ────────────

"""
    is_track_pop(pop_type, path) -> Bool

Whether a population enumerated by `plot_population_groups` is a PER-TRACK population (its members are
tracks) rather than a per-cell one. Track iff its leaf is a track-flagged derived pop (`_tracked`) or
its `pop_type` gates/filters tracks (`track`/`trackclust`); plain `flow`/`live` gates and `clust` pops
are cells. The Julia equivalent of the R `isTrack` pop-map attribute — the sole cell-vs-track test.
"""
function is_track_pop(pop_type::PopTypeArg, path::AbstractString)::Bool
    leaf = String(last(split(String(path), '/')))
    haskey(_DERIVED_POPS, leaf) && return _DERIVED_POPS[leaf].is_track
    string(pop_type) in ("track", "trackclust")
end

"""
    pop_category(pop_type, path) -> String

The DISPLAY category of an enumerated population, for the grouped population picker (Decision 14):
one of `"gated"` (a hand-drawn flow/track gate — or the all-cells root), `"clustered"`
(clust/trackclust), `"region"` (spatial region), `"tracked"` (the derived `_tracked` per-track
subset) or `"aggregated"` (the auto-created spatial-aggregate pop). Pairs with `is_track_pop` (cell
vs track granularity) to place a population under a *"<granularity> · <category>"* header. Derived
from `pop_type` + leaf name only — the same inputs the picker already carries — so the frontend
groups on tags the backend sends, with no second derivation (mirrors how `pop_type` is already sent).
"""
function pop_category(pop_type::PopTypeArg, path::AbstractString)::String
    leaf = String(last(split(String(path), '/')))
    leaf == AGGREGATED_POP_NAME && return "aggregated"
    haskey(_DERIVED_POPS, leaf) && return "tracked"        # e.g. _tracked
    pt = string(pop_type)
    pt == "region" && return "region"
    pt in ("clust", "trackclust") && return "clustered"
    # branch pops: hand-drawn / ensure_filter_pop!-created filters on the branch table (typically
    # `branch-type`) — same category as flow gates. Granularity = "branch" is what distinguishes them.
    "gated"
end

"""
    ensure_filter_pop!(img, pop_type, value_name, parents, name;
                       filter_measure, filter_fun, filter_values, colour) -> Vector{String}

Materialise a CUTOFF as a reusable, persisted FILTER population `name` under each of `parents` in the
`pop_type` gating map of `value_name` — the generalised mechanism behind Decision 14's auto-created
"aggregated" pop. A task that writes a per-cell flag/score (`is.aggregate`, a density, a probability…)
also *defines the population that selects it*, so it flows into any downstream popSelection through the
lazy predicate (`filter_measure`/`filter_fun`/`filter_values`, resolved at read by `pop_df`) rather
than a hand-drawn gate. Deliberately measure-agnostic: the caller supplies the predicate, so a 0/1
flag (`> 0`), a probability (`≥ 0.5`) or a category (`in […]`) all work — the legacy TRUE/FALSE
filter is just the `> 0` case, not baked in.

Idempotent: an existing `<parent>/<name>` is replaced, so re-running the producing task redefines it.
Reserved (`_`-prefixed) names are allowed (system-created). A `parent` absent from this map is skipped
(e.g. an all-cells `/` root maps to `ROOT`, always valid). Loads then saves the map; returns the
created pop paths.
"""
function ensure_filter_pop!(img::CciaImage, pop_type::PopTypeArg, value_name::AbstractString,
                            parents, name::AbstractString;
                            filter_measure::AbstractString, filter_fun::AbstractString,
                            filter_values, colour::AbstractString = "#7c93b8")::Vector{String}
    m = load_pop_map(img; value_name = value_name, pop_type = pop_type)
    created = String[]
    for parent in parents
        par = is_root(parent) ? ROOT : String(parent)
        (par == ROOT || has_pop(m, par)) || continue         # skip parents not in this map
        path = pop_path(par, name)
        has_pop(m, path) && del_pop!(m, path)                # idempotent redefine
        add_pop!(m, name; parent = par, filter_measure = filter_measure, filter_fun = filter_fun,
                 filter_values = filter_values, colour = colour, reserved_ok = true)
        push!(created, path)
    end
    isempty(created) || save_pop_map!(m, img)
    created
end

"""
    scope_pop_types(scope, include_clusters) -> Vector{String}

The pop_types `population_scope_groups` must load to cover a `popScope`. BOTH scopes load `live`: its
stored flow gates are the cell pops AND the parents the derived `/_tracked` children hang off. `tracks`
adds `track` (the per-track gate map) and, unless excluded, `trackclust`; `cells` adds `clust` and
`region` unless excluded. Throws on an unknown scope (a spec typo should fail loudly, not silently
empty the picker).
"""
function scope_pop_types(scope::AbstractString, include_clusters::Bool)::Vector{String}
    if String(scope) == "tracks"
        pts = ["live", "track"]; include_clusters && push!(pts, "trackclust")
    elseif String(scope) == "cells"
        pts = ["live"]; include_clusters && append!(pts, ("clust", "region"))
    else
        error("unknown popScope: $scope (expected \"cells\" or \"tracks\")")
    end
    pts
end

