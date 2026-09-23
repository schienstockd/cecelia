# ── Category-source linked brushing (LINKED_BRUSHING_PLAN.md P2, revised 2026-09-23) ─────────
# `POST /api/labels/by_category` resolves `(image, pop, measure, category)` → the distinct
# per-cell `label` ids whose rows match. Used by the frontend brush primitive: user clicks a
# categorical glyph on a summary panel → this endpoint returns the label ids → dropped into the
# shared `linkedSelection` store as `scope: 'cells'` and mirrored into `PickHighlight` so the
# viewer's per-label pick outline lights up.
#
# WHY CELL-SCOPE, NOT TRACK-SCOPE. HMM state (and any per-(cell, timepoint) categorical) does
# not identify a track — a track has many cells, each visiting many states over its lifetime, so
# "tracks that have AT LEAST ONE cell in state X" is a nearly-all-tracks answer for common
# states. The plot glyph aggregates per (cell, t); the selection stays at the per-cell layer.
# Extrapolating "one cell in state X" → "highlight the whole track" is the murky step the design
# now refuses to take. See the plan doc for the fork with track-scope Option A (parked — safe
# only when the plot glyph IS a track identity, e.g. dominant state per track).
#
# READ-ONLY. Reuses `pop_df` (the sanctioned entry — see app/CLAUDE.md → *H5AD / cell-data
# access*) with `granularity=:cell` + `cell_measures=[measure]` + `categorical=[measure]`, then
# filters + dedupes on `df.label`. No `label_props` / `h5open` on the hot path.
#
# CATEGORY MATCHING is done via `_catkey_local` — an exact clone of `_catkey` in
# `app/src/plotting/plot_data.jl:42` (private). The frequency chart's response uses the same
# normalisation to build its `categories` list, so a category string the client got FROM that
# response round-trips here unchanged. Duplicated rather than exposed because it's a one-liner
# and drift is caught by the validation testset (below).
#
# SINGLE-IMAGE by design. `PickHighlight` is per-(imageUid, valueName) — label ids are per-image,
# not portable across images. Callers on cross-image plots pick a target image (the currently-
# open viewer image if in scope, else the first) and hit this endpoint once for that image.
#
# NB: `api/` does not `using DataFrames` — column access via `getproperty` (`df.label`) or
# `df[i, :name]` only, `length(v)` instead of `nrow`. Matches labels_api.jl / tracking_api.jl.

# ONE-LINER CLONE of app/src/plotting/plot_data.jl:42 (`_catkey`). Kept identical so a category
# string the client received from `/api/plot_data` (chartType="frequency") is comparable here.
_catkey_local(v) = (v isa Real && isfinite(v) && v == round(v)) ? string(Int(round(v))) : string(v)

const _LBC_DEFAULT_LIMIT = 20000    # cap on the returned label list — a per-image cell count
                                     # can easily reach 5-10k, so the default is wider than the
                                     # track-scope endpoint's was.
const _LBC_MAX_LIMIT     = 200000

# `names(df)` returns Vector{String}; wrap so the file's api-convention "no DataFrames methods"
# stays honest — `propertynames` is Base, `String.(...)` maps to the same list.
propertynames_str(df) = String.(propertynames(df))

# Resolve one image + return the distinct positive label ids where the measure column matches
# the category. Missing / non-finite / zero-or-negative label rows are dropped by construction.
function _labels_by_category_for(img, value_name::AbstractString, pop_type::AbstractString,
                                 pop::AbstractString, measure::AbstractString,
                                 category::AbstractString)::Vector{Int}
    df = pop_df(img, pop_type, [pop];
                value_name = value_name,
                granularity = :cell,
                cell_measures = [measure],
                categorical = [measure])
    (length(df.label) == 0) && return Int[]
    measure in propertynames_str(df) || return Int[]
    ids = Set{Int}()
    n = length(df.label)
    col = df[!, measure]
    lcol = df.label
    for i in 1:n
        v = col[i]
        (ismissing(v) || (v isa Real && !isfinite(v))) && continue
        _catkey_local(v) == category || continue
        lid = lcol[i]
        (ismissing(lid) || lid <= 0) && continue
        push!(ids, Int(lid))
    end
    collect(ids)
end

"""
    POST /api/labels/by_category

Body: `{ projectUid, imageUid, valueName, pop, popType?, measure, category, limit? }`
Reply: `{ labelIds: [Int], total: Int, truncated: Bool }`

Given a categorical measure and one of its category values on one image, return the distinct
positive `label` ids whose cells match. Producer for the shared `linkedSelection` store — the
frontend brush primitive mirrors the response into `PickHighlight` for that image.

- `imageUid` is REQUIRED — label ids are per-image, so the caller picks one image up-front.
- `popType` defaults to `"live"` (the behaviour page's cell-level pop_type).
- `category` must match one of the values the frequency chart's response reports (they share
  `_catkey` normalisation). A category no cell matches → 200 with `labelIds: []`, not 404.
- `limit` caps the reply; `truncated: true` when the full id set was larger than the cap.
"""
function api_labels_by_category(body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes), Dict{String,Any})
    catch
        return _gerr(400, "invalid JSON body")
    end
    proj      = string(get(body, "projectUid", ""))
    image_uid = string(get(body, "imageUid", ""))
    value_name = string(get(body, "valueName", ""))
    pop       = string(get(body, "pop", ""))
    pop_type  = string(get(body, "popType", "live"))
    measure   = string(get(body, "measure", ""))
    category  = string(get(body, "category", ""))
    isempty(proj)       && return _gerr(400, "projectUid required")
    isempty(image_uid)  && return _gerr(400, "imageUid required")
    isempty(value_name) && return _gerr(400, "valueName required")
    isempty(pop)        && return _gerr(400, "pop required")
    isempty(measure)    && return _gerr(400, "measure required")
    isempty(category)   && return _gerr(400, "category required")

    limit_raw = get(body, "limit", _LBC_DEFAULT_LIMIT)
    limit_int = limit_raw isa Number ? Int(limit_raw) :
                limit_raw isa AbstractString ? something(tryparse(Int, String(limit_raw)), _LBC_DEFAULT_LIMIT) :
                _LBC_DEFAULT_LIMIT
    limit = clamp(limit_int, 1, _LBC_MAX_LIMIT)

    img, err = _gating_image(proj, image_uid)
    err === nothing || return err
    ids = sort!(_labels_by_category_for(img, value_name, pop_type, pop, measure, category))
    total = length(ids)
    truncated = total > limit
    truncated && (ids = ids[1:limit])
    return 200, JSON3.write((; labelIds = ids, total = total, truncated = truncated))
end
