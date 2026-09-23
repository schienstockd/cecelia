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
_clustfeatures_path(props_path::AbstractString) = replace(props_path, r"\.h5ad$" => ".clustfeatures.json")

# The sidecar is keyed by the FULL obs column the run wrote — `{family}.{suffix}` (= the
# `_cluster_measure_prefix` family + the run suffix), so a `clusters.immune` cell clustering and a
# `regions.immune` region clustering coexist on one segmentation instead of clobbering each other.
_clustfeatures_key(suffix::AbstractString, family::AbstractString) = "$(family).$(suffix)"

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

_clustfeatures_raw(props_path::AbstractString) =
    let s = _clustfeatures_path(props_path)
        isfile(s) ? (try JSON3.read(read(s, String), Dict{String,Any}) catch; Dict{String,Any}() end) :
                    Dict{String,Any}()
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

# One run's recorded feature columns (the heatmap's row universe), `String[]` when unrecorded.
function _clustfeatures_features(props_path::AbstractString, suffix::AbstractString;
                                 family::AbstractString = "clusters")::Vector{String}
    e = _clustfeatures_entry(props_path, suffix; family=family)
    e === nothing && return String[]
    f = get(e, "features", get(e, :features, nothing))
    f isa AbstractVector ? String[string(x) for x in f] : String[]
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
_is_cluster_pop_type(pop_type)::Bool = string(pop_type) in ("clust", "trackclust", "region")

# The obs-column family a cluster-style pop type filters over — the one place the prefix is decided.
_cluster_measure_prefix(pop_type)::String = string(pop_type) == "region" ? "regions." : "clusters."
# …and the same decision without the dot, for the clustfeatures sidecar key (`{family}.{suffix}`) and
# any caller that needs the bare family name. Derived, never re-decided.
_cluster_measure_family(pop_type)::String = chopsuffix(_cluster_measure_prefix(pop_type), ".")

# Suffixes a cluster pop map's filters reference (each pop's `filter_measure` = "{prefix}{suffix}").
function _referenced_cluster_suffixes(m::PopulationMap)::Set{String}
    prefix = _cluster_measure_prefix(m.pop_type)
    out = Set{String}()
    for p in values(m.pops)
        fm = p.filter_measure
        (fm === nothing || !startswith(String(fm), prefix)) && continue
        push!(out, String(fm)[ncodeunits(prefix)+1:end])
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
function _borrow_cluster_pop_map(img::CciaImage, value_name::AbstractString,
                                 pop_type::PopTypeArg)::Union{PopulationMap,Nothing}
    granularity = string(pop_type) == "trackclust" ? :track : :cell
    p = granularity === :track ? img_track_props_path(img, value_name) : img_label_props_path(img, value_name)
    my_suffixes = _clustfeatures_suffixes(p; family=_cluster_measure_family(pop_type))
    isempty(my_suffixes) && return nothing              # this vn wasn't clustered → nothing to share
    for vn in versioned_keys(img.label_props)
        v = String(vn); v == value_name && continue
        sib = load_pop_map(img._dir, v; pop_type=pop_type)
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
# Okabe–Ito colourblind-safe palette — the default categorical colours. Matches the frontend
# (`plots/plot.ts` 'okabe-ito') and the napari bridge (`_CATEGORICAL_RGBA`) so a category reads the
# same colour everywhere.
const OKABE_ITO = ["#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"]

_as_value_vec(x) = x === nothing ? Any[] : (x isa AbstractVector ? collect(x) : Any[x])
# tolerant value equality: exact, string-wise, or numeric (a filter value stored as 2.0 == column 2)
_val_eq(a, b) = a == b || string(a) == string(b) ||
    (a isa Real && b isa Real && Float64(a) == Float64(b))

# canonical string key for a category value on the wire (2.0 and 2 → "2") so Julia overrides and the
# bridge's column values match regardless of Int/Float encoding.
_val_key(v) = v isa Real ? (isinteger(v) ? string(Int(v)) : string(v)) : string(v)

"""
    pop_colour_overrides(m, column) -> Dict{String,String}

The `{value => hex}` a user population imposes on `column`: for each pop whose `filter_measure == column`,
its `filter_values` → the pop's `colour`, keyed by the value's canonical STRING form (survives JSON to
the napari bridge). This is the "use the population's colour where one exists" half of the colour-choice
rule — the bridge fills the remaining present values with defaults and returns the full legend. First
pop in insertion order wins a shared value.
"""
function pop_colour_overrides(m::PopulationMap, column::AbstractString)::Dict{String,String}
    col = String(column); out = Dict{String,String}()
    for path in m.order
        p = m.pops[path]
        (p.filter_measure === nothing || String(p.filter_measure) != col) && continue
        for fv in _as_value_vec(p.filter_values)
            k = _val_key(fv)
            haskey(out, k) || (out[k] = p.colour)
        end
    end
    out
end

"""
    pop_label_overrides(m, column) -> Dict{String,String}

The `{value => population name}` companion to [`pop_colour_overrides`](@ref): for each pop whose
`filter_measure == column`, its `filter_values` → the pop's `name`, keyed by the value's canonical
STRING form. Lets a colour-by legend read the **population name** (e.g. "migratory") instead of the raw
category value (e.g. "2") wherever a population defines that value. First pop in insertion order wins.
"""
function pop_label_overrides(m::PopulationMap, column::AbstractString)::Dict{String,String}
    col = String(column); out = Dict{String,String}()
    for path in m.order
        p = m.pops[path]
        (p.filter_measure === nothing || String(p.filter_measure) != col) && continue
        for fv in _as_value_vec(p.filter_values)
            k = _val_key(fv)
            haskey(out, k) || (out[k] = p.name)
        end
    end
    out
end

"""
    colour_by_palette(m, column, values; default_palette=OKABE_ITO) -> Dict{Any,String}

Canonical colour choice for colouring by a categorical `column`: a `value` that a **user-defined
population filters for** on that column (`filter_measure == column` and the value ∈ its `filter_values`)
takes **that population's colour**; every remaining value gets a default-palette colour by sorted
position. This is the ONE "use the population's colour where one exists, else a sensible default" rule —
general across clusters (a cluster pop is just a filter on `clusters.{suffix}`), HMM states, and any
categorical measure — so tracks/points, legends and movies all colour a category identically. Pure.
"""
function colour_by_palette(m::PopulationMap, column::AbstractString, values;
                           default_palette::Vector{String}=OKABE_ITO)::Dict{Any,String}
    col = String(column)
    out = Dict{Any,String}()
    vals = collect(values)
    # 1) values a user pop FILTERS for on this column → that pop's colour (first pop in insertion order
    #    wins if two cover the same value)
    for path in m.order
        p = m.pops[path]
        (p.filter_measure === nothing || String(p.filter_measure) != col) && continue
        for fv in _as_value_vec(p.filter_values), v in vals
            (!haskey(out, v) && _val_eq(v, fv)) && (out[v] = p.colour)
        end
    end
    # 2) remaining values → default palette by sorted position (stable + colourblind-safe)
    uncovered = sort!(unique(v for v in vals if !haskey(out, v)))
    for (i, v) in enumerate(uncovered)
        out[v] = default_palette[(i - 1) % length(default_palette) + 1]
    end
    out
end

function load_pop_map(img::CciaImage; value_name::AbstractString="default", pop_type::PopTypeArg="flow",
                      backfill_save::Bool=true)
    m = load_pop_map(img._dir, value_name; pop_type=pop_type, backfill_save=backfill_save)
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
    borrowed = _borrow_cluster_pop_map(img, String(value_name), string(pop_type))
    borrowed === nothing && return m
    # a borrowed map came from a sibling segmentation of the SAME image → same pixel sizes
    borrowed.physical_sizes = m.physical_sizes
    borrowed
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
            m = load_pop_map(img._dir, String(vn); pop_type=pop_type)
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

