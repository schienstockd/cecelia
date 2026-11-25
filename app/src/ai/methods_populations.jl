# ── Methods report — how the populations were defined ───────────────────────────────────────────────
#
# The populations defined on the scope's images, as a Methods section states them: the gating tree
# (which pop was gated on which two measures, inside which parent; a combination as what it combines),
# and how cluster and region populations were assembled from a run's clusters. Read from the stored
# gating sidecars (`_observer_each_population`), never from cell data, so nothing is counted here.
#
# A gate stores the raw column of the image it was drawn on (`mean_intensity_1`), and channel order
# differs between images, so each image's axes are named through its own channels
# (`canonical_channel_columns`) — "gBT-uGFP vs OTI-CTV" in 9 images can be "OTI-uGFP vs gBT-CTV" in 7,
# and the text says so rather than naming one image's channels. Claude's stamps on a population
# (`created_by`/`updated_by`) are returned for the AI paragraph.

const _MR_CLUSTER_NOUN = Dict("clust" => "Cell clusters", "trackclust" => "Track clusters", "region" => "Regions")

# One population across the scope's images
mutable struct _MrPop
    pop_type::String; value_name::String; path::String; name::String; parent::String
    kind::Symbol                                # :gate, :filter, :boolean, :cluster, :other
    detail::Dict{String,Int}                    # what defines it, as text → images it reads so
    images::Int
    stamps::Vector{Any}; edited::Bool
end

# a gate axis by name: a channel's intensity column through this image's channels, else the measure
_mr_axis(c, chans) = (n = only(canonical_channel_columns([string(c)], chans));
                      n == string(c) ? methods_measure_phrase(n) : "$n intensity")
# "A intensity vs B intensity" → "A vs B intensity"
_mr_axes(x, y) = endswith(x, " intensity") && endswith(y, " intensity") ? "$(x[1:end-10]) vs $y" : "$x vs $y"

# `names`: path → the name a reader tells apart from the set's other populations (`_mr_pop_names`)
function _mr_pop_detail(p, chans, names::Dict{String,String})::Tuple{Symbol,String}
    pt = string(p.pop_type)
    name_of(path) = get(names, string(path), methods_pop_phrase(string(path)))
    if p.gate !== nothing
        g = gate_spec(p.gate)
        return :gate, _mr_axes(_mr_axis(g["x_channel"], chans), _mr_axis(g["y_channel"], chans))
    end
    b = boolean_spec(p)
    if b !== nothing
        inc = String[name_of(x) for x in b["pops"]]; exc = String[name_of(x) for x in b["not"]]
        base = isempty(inc) ? name_of(p.parent) : join(inc, b["op"] == "and" ? " and " : " or ")
        return :boolean, isempty(exc) ? base : "$base without $(_mr_join(exc))"
    end
    f = filter_spec(p)
    f === nothing && return :other, ""
    vals = something(f["values"], Any[])
    if haskey(_MR_CLUSTER_NOUN, pt)
        prefix = _cluster_measure_prefix(pt)
        run = startswith(string(f["measure"]), prefix) ? string(f["measure"])[ncodeunits(prefix)+1:end] : string(f["measure"])
        word = pt == "region" ? "region" : "cluster"
        ids = join(string.(vals), ", ")
        return :cluster, "$run|$(word)$(length(vals) == 1 ? "" : "s") $ids"
    end
    :filter, "$(methods_measure_phrase(f["measure"])) $(something(f["fun"], "")) $(join(string.(vals), ", "))"
end

# Each label set's population paths, across all the images, as names a reader tells apart
# (`methods_pop_phrases`): two `OTI`s under different parents read "qc/OTI" and "other/OTI" in every
# clause and combination, not "OTI without OTI".
function _mr_pop_names(per_image)::Dict{String,Dict{String,String}}
    paths = Dict{String,Set{String}}()
    for (_, pops) in per_image, p in pops
        push!(get!(Set{String}, paths, p.value_name), p.path)
    end
    Dict(vn => (ps = sort!(collect(ps)); Dict(zip(ps, methods_pop_phrases(ps)))) for (vn, ps) in paths)
end

# Every population on `uids`, grouped across images by (pop type, label set, path)
function _mr_population_index(proj::CciaProject, uids)::Vector{_MrPop}
    acc = Dict{Tuple{String,String,String},_MrPop}()
    per_image = Tuple{CciaImage,Vector{Any}}[]
    for uid in uids
        img = try init_object(proj.uid, uid) catch; nothing end
        img isa CciaImage || continue
        pops = Any[]
        _observer_each_population(p -> push!(pops, p), img)
        push!(per_image, (img, pops))
    end
    names = _mr_pop_names(per_image)
    for (img, pops) in per_image
        for p in pops
            chans = something(channel_names(img; value_name = p.value_name), String[])
            ns = names[p.value_name]
            kind, d = try _mr_pop_detail(p, chans, ns) catch; (:other, "") end
            key = (string(p.pop_type), p.value_name, p.path)
            e = get!(acc, key) do
                _MrPop(key..., get(ns, p.path, p.name), string(p.parent), kind, Dict{String,Int}(), 0, Any[], false)
            end
            e.images += 1
            isempty(d) || (e.detail[d] = get(e.detail, d, 0) + 1)
            p.created_by === nothing || push!(e.stamps, p.created_by)
            u = p.updated_by
            e.edited |= u !== nothing && !haskey(_METHODS_AI_VIAS, _mr_str(u, "via"))
        end
    end
    sort!(collect(values(acc)); by = e -> (e.value_name, e.pop_type, e.path))
end

# What a population is, and what making it meant — a gate is drawn, a cluster population is an
# assignment of clusters, a combination is defined.
function _mr_pop_kind(e::_MrPop)::Tuple{String,String}
    track = e.pop_type == "track"
    e.kind == :gate    && return (track ? "track gate" : "cell gate", "gate drawn")
    e.kind == :boolean && return ("combination", "defined")
    e.kind == :filter  && return ("filter", "defined")
    e.kind == :cluster && return (e.pop_type == "trackclust" ? "track-cluster population" :
                                  e.pop_type == "region" ? "region population" : "cell-cluster population",
                                  "clusters assigned")
    ("$(e.pop_type) population", "defined")
end

# A detail stated for every image, or its spread
function _mr_detail_text(e::_MrPop)::String
    isempty(e.detail) && return ""
    length(e.detail) == 1 && return first(keys(e.detail))
    join(("$d ($n image$(n == 1 ? "" : "s"))" for (d, n) in sort!(collect(e.detail); by = x -> (-x[2], x[1]))), " or ")
end

_mr_partial(e::_MrPop, n::Int) = e.images < n ? " ($(e.images) of $n images)" : ""

"""
    _mr_populations(pops, n) -> (lines, ai_lines)

The *Populations* paragraphs for `pops` (`_mr_population_index` over `n` images; empty when the
images define none) and one AI line per population Claude drew. See the section comment.
"""
function _mr_populations(pops::Vector{_MrPop}, n::Int)
    lines = String[]; ai = String[]
    by_set = Dict{String,Vector{_MrPop}}()
    for e in pops
        push!(get!(Vector{_MrPop}, by_set, e.value_name), e)
    end
    multi = length(by_set) > 1
    for vn in sort!(collect(keys(by_set)))
        es = by_set[vn]
        name_of = Dict(e.path => e.name for e in es)
        # the gating tree, parents first; siblings gated the same way read as one clause
        gated = [e for e in es if e.kind in (:gate, :boolean, :filter)]
        clauses = String[]
        done = Set{String}()
        for e in sort(gated; by = e -> (count(==('/'), e.path), e.path))
            e.path in done && continue
            d = _mr_detail_text(e)
            sibs = [s for s in gated if s.parent == e.parent && s.kind == e.kind && _mr_detail_text(s) == d &&
                                     !(s.path in done) && s.images == e.images]
            foreach(s -> push!(done, s.path), sibs)
            who = _mr_join([s.name for s in sibs])
            within = is_root(e.parent) ? "" : " within $(get(name_of, e.parent, methods_pop_phrase(e.parent)))"
            push!(clauses, e.kind == :boolean ? "$who = $d$(_mr_partial(e, n))" :
                           "$who$within on $d$(_mr_partial(e, n))")
        end
        prefix = multi ? "In the $vn segmentation, cells" : "Cells"
        isempty(clauses) || push!(lines, "$prefix were gated per image: $(join(clauses, "; ")).")
        # cluster and region populations, by the run they read
        runs = Dict{Tuple{String,String},Vector{Tuple{_MrPop,String}}}()
        for e in es
            e.kind == :cluster || continue
            d = _mr_detail_text(e)
            i = findfirst('|', d)
            i === nothing && continue          # images disagree on the run: listed below as is
            push!(get!(Vector{Tuple{_MrPop,String}}, runs, (e.pop_type, d[1:i-1])), (e, d[i+1:end]))
        end
        for ((pt, run), members) in sort!(collect(runs); by = first)
            # how many images, once for the run when its populations agree
            same = allequal(e.images for (e, _) in members)
            parts = ["$(e.name) ($ids$(same ? "" : "; $(e.images) of $n images"))" for (e, ids) in members]
            push!(lines, "$(_MR_CLUSTER_NOUN[pt]) of run *$run*$(same ? _mr_partial(first(members)[1], n) : "") " *
                         "were grouped into $(_mr_join(parts)).")
        end
        for e in es
            phrase = methods_ai_phrase(e.stamps)
            isempty(phrase) && continue
            what, did = _mr_pop_kind(e)
            d = _mr_detail_text(e)
            e.kind == :cluster && (d = replace(d, r"^([^|]*)\|(.*)$" => s"\2 of run \1"))
            push!(ai, "- *$(e.name)* — $what$(isempty(d) ? "" : " ($d)"): $did by $phrase" *
                      (e.edited ? ", then edited by hand." : "."))
        end
    end
    lines, ai
end

# The cluster runs the populations filter on, `(namespace, name, task prefix)` — what a set's
# Methods keeps however few images a run covered (`_history_usage`)
_mr_pop_runs(proj::CciaProject, uids, pops::Vector{_MrPop}) =
    _hp_cluster_runs(proj, uids, [_parse_tkey("$(e.pop_type)::$(e.value_name)$(e.path)")
                                  for e in pops if _is_cluster_pop_type(e.pop_type)], String[])
