# ── Population positions: every cell at its position, tagged by population ─────────────────────────────
#
# Where do the populations sit on the image, frame by frame? The read behind the `popPositions` plot
# (docs/PLOTS.md → *Population positions*): each cell's centroid (µm when the image is calibrated, else
# pixels), its frame, and the index of the population it is coloured by. Any population type: a track
# family (`live`, `trackclust`, …) resolves to its member cells in every frame through `pop_df`, so a
# behaviour cluster colours the cells along its tracks and a region the cells it holds at each timepoint.

"""
    pop_index_by_label(img, tokens, default_pop_type; granularity=:cell, most_specific=false)
        -> Dict{Tuple{String,Int},Int}

`(value_name, label) → 0-based index` of the population in `tokens` a row is coloured by. Each token is
`"popType~valueNamePrefixedPath"` (`"live~B/qc/_tracked"`), or a bare path read under `default_pop_type`.
Membership goes through `pop_df` at `granularity`, which handles the grain (a track family expands to its
cells at `:cell`; a cell pop rolls up to its tracks at `:track`), value-name pooling and derived pops. A
token that does not resolve contributes nothing (a foreign or deleted pop is not an error on a plot).

A row in several populations takes the FIRST in `tokens`, or with `most_specific` the deepest path
(ties → the first): a parent picked before its child would otherwise leave the child with no rows.
Shared by the UMAP's colour-by-population and the population positions plot.
"""
function pop_index_by_label(img::CciaImage, tokens, default_pop_type::AbstractString;
                            granularity::Symbol = :cell,
                            most_specific::Bool = false)::Dict{Tuple{String,Int},Int}
    parsed = [_parse_pop_token(tok, default_pop_type) for tok in tokens]
    order = most_specific ? sortperm(parsed; by = p -> -_token_depth(p.path), alg = Base.Sort.DEFAULT_STABLE) :
                            eachindex(parsed)
    pop_of = Dict{Tuple{String,Int},Int}()
    for pi in order
        (; pop_type, path) = parsed[pi]
        df = try
            pop_df(img, pop_type, [path]; granularity = granularity, include_obs = false, include_x = false)
        catch
            continue
        end
        ("value_name" in names(df) && "label" in names(df)) || continue
        for r in eachrow(df)
            get!(pop_of, (String(r.value_name), Int(r.label)), pi - 1)
        end
    end
    pop_of
end

function _parse_pop_token(tok, default_pop_type::AbstractString)
    parts = split(String(tok), '~'; limit = 2)
    length(parts) == 2 ? (; pop_type = String(parts[1]), path = String(parts[2])) :
                         (; pop_type = String(default_pop_type), path = String(parts[1]))
end

# path depth: the gates below root (a value-name prefix counts once for every token alike)
_token_depth(path::AbstractString)::Int = count(==('/'), path)

"""
    pop_positions(img, value_name, tokens; default_pop_type = "flow", colour_by = "")
        -> (; x, y, t, pop, label, value_name, unit, value)

The cells of every segmentation the populations in `tokens` live on (a co-clustered run spans several;
the rail can pick from several), or — with no populations — of `value_name` (`nothing`/`""` = the
active one). `x`/`y` the centroid (µm when the image is calibrated, else px — `unit` says which), `t`
the frame (0 on a still), `pop` the index of the population the cell is coloured by (the deepest it is
in, `pop_index_by_label(…; most_specific = true)`; -1 = none, drawn only as the base cloud), `label` and
`value_name` the cell, `value` its `colour_by` cell column (NaN where the cell has none, its table lacks
the column, or none was asked for — a numeric read only: a score map, an intensity). Parallel vectors
in table order, segmentation by segmentation.

Populations that resolve on none of this image's segmentations → all empty: the image has nothing of
them to show, and drawing its active segmentation instead would read as "these populations are empty".
"""
function pop_positions(img::CciaImage, value_name, tokens; default_pop_type::AbstractString = "flow",
                       colour_by::AbstractString = "")
    calibrated = img_is_calibrated(img)
    unit = calibrated ? SPATIAL_UNIT_UM : SPATIAL_UNIT_PX
    pop_of = pop_index_by_label(img, tokens, default_pop_type; granularity = :cell, most_specific = true)
    vns = isempty(tokens) ? [resolve_value_name(img, isempty(something(value_name, "")) ? nothing : value_name)] :
                            _token_value_names(img, tokens, default_pop_type, pop_of)
    x = Float32[]; y = Float32[]; t = Float32[]; pop = Int[]; label = Int[]; vn_of = String[]; value = Float32[]
    for vn in vns
        path = img_label_props_path(img, vn)
        (path === nothing || !isfile(path)) && continue
        cb = !isempty(colour_by) && colour_by in col_names(label_props(path); data_type = :obs)
        df = label_props(path) |> v -> view_centroid_cols(v; order = [:x, :y]) |>
             v -> (cb ? select_cols(v, [String(colour_by)]) : v) |> as_df
        ("centroid_x" in names(df) && "centroid_y" in names(df)) || continue
        # only a calibrated image is scaled: a missing axis defaults to 1.0, so scaling a half-calibrated
        # image would put µm on one axis and pixels on the other under one unit label
        calibrated && scale_centroids!(df, img)
        ls = Int.(df.label)
        append!(x, Float32.(df.centroid_x)); append!(y, Float32.(df.centroid_y))
        append!(t, "centroid_t" in names(df) ? Float32.(df.centroid_t) : zeros(Float32, length(ls)))
        append!(pop, Int[get(pop_of, (vn, l), -1) for l in ls])
        append!(label, ls); append!(vn_of, fill(vn, length(ls)))
        append!(value, cb ? Float32[(v isa Real && !ismissing(v)) ? Float32(v) : NaN32 for v in df[!, colour_by]] :
                            fill(NaN32, length(ls)))
    end
    (; x, y, t, pop, label, value_name = vn_of, unit, value)
end

# The segmentations a set of tokens covers on this image: every one a resolved population has rows on,
# plus a token's own segmentation (`_split_pop_ref` grammar) when the population is DEFINED there but
# empty — its cells still show. A population this image does not define adds nothing, so its tile is
# not a full cloud of uncoloured cells reading as "0 cells here".
function _token_value_names(img::CciaImage, tokens, default_pop_type, pop_of)::Vector{String}
    vns = Set{String}(first(k) for k in keys(pop_of))
    for tok in tokens
        (; pop_type, path) = _parse_pop_token(tok, default_pop_type)
        vn, p = _split_pop_ref(path, "")
        (isempty(vn) || vn in vns || !haskey(img.label_props, vn)) && continue
        _pop_defined(img, vn, pop_type, p) && push!(vns, vn)
    end
    sort!(collect(vns))
end

# Is `path` a population of `vn`'s `pop_type` tree on this image (derived pops layered on, as `pop_df`
# reads them)? No membership is evaluated — the tree alone answers it.
function _pop_defined(img::CciaImage, vn::AbstractString, pop_type::AbstractString, path::AbstractString)::Bool
    is_root(path) && return true
    try
        has_pop(_read_pop_tree(img, vn, pop_type; derived_paths = [path]), path)
    catch
        false
    end
end
