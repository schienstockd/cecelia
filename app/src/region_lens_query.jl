# "Find regions like this one" from a pinned channel lens — docs/todo/LENS_QUERY_PLAN.md (Decisions 1–3).
#
# The signature is the mean neighbourhood composition of the cells inside the lens, read from the
# composition columns a region run already wrote (`clustRegions.cluster` → `spatial.comp.<pop>.<suffix>`,
# recorded as the run's features in the clustfeatures sidecar). Every cell is scored by cosine
# similarity to it. How a score becomes "a match" is open (plan → *P1 findings*: a percentile cut-off
# was rejected; a per-cell z against random lenses is the candidate), so this returns scores only.
# Pure helpers first, then the one reader.

"Fewest cells a lens must hold for its signature to mean anything (Decision 2)."
const LENS_QUERY_MIN_CELLS = 10

"""
    lens_inside(x, y, lens) -> BitVector

Which centroids (level-0 px) lie inside the circle `lens = (; x, y, r)` (level-0 px), edge included.
"""
lens_inside(x::AbstractVector{<:Real}, y::AbstractVector{<:Real}, lens) =
    BitVector(@. (x - lens.x)^2 + (y - lens.y)^2 <= lens.r^2)

"""
    composition_rows(M) -> (X, ok)

The composition matrix with unusable rows zeroed, and which rows were usable. A cell the region run
gave no composition (NaN or missing — e.g. one outside the neighbour graph) is neither part of a lens
nor a match: one NaN would otherwise turn every score into NaN.
"""
function composition_rows(M::AbstractMatrix)
    X = Matrix{Float64}(undef, size(M))
    ok = trues(size(M, 1))
    @inbounds for i in axes(M, 1)
        for j in axes(M, 2)
            v = M[i, j]
            if v === missing || !isfinite(v)
                ok[i] = false
            end
        end
        for j in axes(M, 2)
            X[i, j] = ok[i] ? Float64(M[i, j]) : 0.0
        end
    end
    X, ok
end

"""
    lens_signature(X, inside) -> Vector{Float64}

The mean composition row of the cells `inside` (rows of `X`, one column per composition feature).
"""
function lens_signature(X::AbstractMatrix{<:Real}, inside::AbstractVector{Bool})::Vector{Float64}
    n = count(inside)
    n > 0 || throw(ArgumentError("no cells inside the lens"))
    vec(sum(X[inside, :]; dims = 1)) ./ n
end

"""
    cosine_scores(X, sig) -> Vector{Float64}

Cosine similarity of every row of `X` to `sig`. A row (or signature) of all zeros scores 0 —
a cell with no neighbours in the basis is like nothing. Compositions are non-negative, so 0–1.
"""
function cosine_scores(X::AbstractMatrix{<:Real}, sig::AbstractVector{<:Real})::Vector{Float64}
    ns = sqrt(sum(abs2, sig))
    out = zeros(Float64, size(X, 1))
    ns > 0 || return out
    @inbounds for i in axes(X, 1)
        dot = 0.0; nr = 0.0
        for j in axes(X, 2)
            v = Float64(X[i, j]); dot += v * sig[j]; nr += v * v
        end
        out[i] = nr > 0 ? dot / (sqrt(nr) * ns) : 0.0
    end
    out
end

"""
    region_lens_features(props_path, suffix) -> Vector{String}

The composition columns of region run `suffix` that go into a signature: the run's recorded features
minus a window run's density column (Decision 3 — density would make the score track cell count).
"""
region_lens_features(props_path::AbstractString, suffix::AbstractString)::Vector{String} =
    filter(!=(_comp_col(WINDOW_DENSITY_NAME, suffix)),
           _clustfeatures_features(props_path, suffix; family = "regions"))

"""
    region_lens_query(img, lens; value_name, suffix, t = nothing, min_cells = LENS_QUERY_MIN_CELLS)

Score every cell of segmentation `value_name` against the lens `(; x, y, r)` (level-0 px), on region
run `suffix`. `t` picks the frame whose cells form the lens (a time-lapse); every cell of every frame
is scored.

Returns `(; error)` on a refusal (no such run, too few cells), else
`(; labels, scores, ok, inside, nInside, nScored, features, signature)` — cells the
run gave no composition score 0 and are never in the lens (`composition_rows`).
"""
function region_lens_query(img::CciaImage, lens; value_name::AbstractString, suffix::AbstractString,
                           t::Union{Integer,Nothing} = nothing, min_cells::Integer = LENS_QUERY_MIN_CELLS)
    path = img_label_props_path(img, value_name)
    feats = region_lens_features(path, suffix)
    isempty(feats) && return (; error = "No region run \"$suffix\" on $(value_name).")
    lp = label_props(img; value_name = value_name)
    spatial = centroid_columns(lp; order = [:x, :y])
    length(spatial) == 2 || return (; error = "$(value_name) has no x/y centroids.")
    timed = !isempty(temporal_columns(lp))
    df = lp |> view_centroid_cols |> v -> select_cols(v, feats) |> as_df
    X, ok = composition_rows(Matrix(df[!, feats]))
    inside = lens_inside(df[!, spatial[1]], df[!, spatial[2]], lens) .& ok
    if timed && t !== nothing
        inside .&= round.(Int, df[!, first(temporal_columns(lp))]) .== t
    end
    n_in = count(inside)
    n_in >= min_cells ||
        return (; error = "Too few cells in the lens ($n_in). Make it bigger.")
    sig = lens_signature(X, inside)
    any(>(0), sig) || return (; error = "No neighbourhood composition in the lens. Re-run Cluster regions.")
    scores = cosine_scores(X, sig)
    (; labels = Int.(df[!, :label]), scores, ok, inside, nInside = n_in, nScored = count(ok),
       features = feats, signature = sig)
end
