# "Find regions like this one" from a pinned channel lens — docs/todo/LENS_QUERY_PLAN.md (Decisions 1–3).
#
# The signature is the mean neighbourhood composition of the cells inside the lens, read from the
# composition columns a region run already wrote (`clustRegions.cluster` → `spatial.comp.<pop>.<suffix>`,
# recorded as the run's features in the clustfeatures sidecar). Every cell is scored by cosine
# similarity to it; the saved score is that similarity above the run's average cell, 0–1
# (`lens_similarity_scores`). No match threshold — a filter population on the column does that.
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

# ── The saved score map (`clustRegions.lensQuery`) ──────────────────────────────────────────────────

"""
    lens_score_col(name) -> String

The per-cell obs column a lens query writes: `spatial.lens.<name>`. ONE namer — the task writes it,
the viewer colours by it, a filter population thresholds it.
"""
lens_score_col(name::AbstractString) = LENS_SCORE_PREFIX * replace(strip(String(name)), r"[^A-Za-z0-9_-]+" => "_")
"The obs-column family lens queries write (`lens_score_col`); the Runs tab lists and deletes it."
const LENS_SCORE_PREFIX = "spatial.lens."

"""
    lens_similarity_scores(X, ok, sig, mu) -> Vector{Float64}

Per cell: how alike its neighbourhood mix is to the lens's, above the run's average cell —
`clamp((cos(x, sig) − mu) / (1 − mu), 0, 1)`, where `mu` is the mean cosine to the lens over every
scored cell of the run (`lens_background`). 0 = no more like the lens than the average cell, 1 =
identical. Plain cosine ranks the cells the same; taking `mu` off keeps a run whose cells are all
alike (few composition features) from saturating the ramp. A cell with no composition (`!ok`) is NaN.
Why this score: docs/todo/LENS_QUERY_PLAN.md → *Scoring*.
"""
function lens_similarity_scores(X::AbstractMatrix{<:Real}, ok::AbstractVector{Bool}, sig::AbstractVector{<:Real},
                                mu::Real)::Vector{Float64}
    s = cosine_scores(X, sig)
    span = 1.0 - Float64(mu)
    [ok[i] ? (span > 0 ? clamp((s[i] - mu) / span, 0.0, 1.0) : 0.0) : NaN for i in eachindex(s)]
end

"""
    lens_background(tabs, sig) -> Float64

The run's average cell's cosine to the lens: the mean over every usable cell of every table
(`(; X, ok)`) — the 0 of `lens_similarity_scores`.
"""
function lens_background(tabs, sig::AbstractVector{<:Real})::Float64
    tot = 0.0; n = 0
    for tb in tabs
        s = cosine_scores(tb.X, sig)
        for i in eachindex(s)
            tb.ok[i] || continue
            tot += s[i]; n += 1
        end
    end
    n > 0 ? tot / n : 0.0
end

"""
    lens_coherence(X, inside, sig) -> Float64

How much the lens is one kind of tissue: the median cosine of its own cells to its signature. A lens
across two tissues averages them into a mix no cell has, and its scores light the boundary between
them rather than either.
"""
function lens_coherence(X::AbstractMatrix{<:Real}, inside::AbstractVector{Bool}, sig::AbstractVector{<:Real})::Float64
    s = cosine_scores(X[inside, :], sig)
    isempty(s) ? NaN : median(s)
end

"Below this, no phenotype in the lens differs from the tissue's average share by enough to search on."
const LENS_QUERY_MIN_LIFT = 0.05

"A cell this alike to the lens or more counts as strong in the run's QC (`nStrong`) — not a match cut-off."
const LENS_STRONG = 0.8

"Below this median cosine of the lens's own cells to its signature, the lens spans more than one tissue
(`lens_coherence`) — set from six lenses: five coherent ones at 0.97–1.00, one mixed at 0.65."
const LENS_QUERY_MIN_COHERENCE = 0.8

"""
    lens_query_qc_findings(max_lift, coherence = 1.0) -> Vector

`max_lift` = the largest |lens share − tissue share| over the run's phenotypes: a lens that holds the
tissue's average mix finds everything a little and nothing well. `coherence` = `lens_coherence`: a lens
across tissues matches no cell well.
"""
function lens_query_qc_findings(max_lift::Real, coherence::Real = 1.0)
    out = Any[]
    max_lift < LENS_QUERY_MIN_LIFT && push!(out, qc_finding("warn", "lensQuery.average_mix"))
    coherence < LENS_QUERY_MIN_COHERENCE && push!(out, qc_finding("warn", "lensQuery.mixed_lens"))
    out
end

"""
    LENS_SCORE_RANGE

The colour scale every `spatial.lens.*` column is drawn on: the score's own 0 → 1
(`lens_similarity_scores`). Fixed rather than the column's min–max so one colour means one score on
every image and every lens — an image with no places like the lens stays dark instead of stretching
its best weak match to the top. Read by `fixed_colour_range`.
"""
const LENS_SCORE_RANGE = (0.0, 1.0)

"""
    fixed_colour_range(col) -> (lo, hi) | nothing

A column whose values carry their own scale, so colour-by draws it on that scale and not on the
values' min–max: the lens scores (`LENS_SCORE_RANGE`). `nothing` for every other column.
"""
fixed_colour_range(col::AbstractString) = startswith(col, LENS_SCORE_PREFIX) ? LENS_SCORE_RANGE : nothing

"""
    lens_run_images(img, value_name, suffix) -> Vector{String}

The images a lens query on region run `suffix` scores: every image the run recorded (its clustfeatures
`partOf`), else just `img`. One answer for the task (which images it writes) and a lens figure (which
images its delete cleans up).
"""
function lens_run_images(img::CciaImage, value_name::AbstractString, suffix::AbstractString)::Vector{String}
    p = img_label_props_path(img, value_name)
    entry = (p === nothing || !isfile(p)) ? nothing : _clustfeatures_entry(p, suffix; family = "regions")
    _clustfeatures_part_of(entry, img.uid)
end
