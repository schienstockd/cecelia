# ── Category-source linked brushing (LINKED_BRUSHING_PLAN.md P2) ──────────────────────────────
# `POST /api/tracks/by_category` resolves `(pop, measure, category)` → the distinct `track_id`s
# whose per-cell rows match. Used by the frontend brush primitive: user clicks a categorical
# glyph on a summary panel (an HMM-state bar, a frequency category) → this endpoint returns the
# ids → dropped into the shared `linkedSelection` store (LINKED_BRUSHING_PLAN.md Decision 4).
#
# READ-ONLY. Reuses `pop_df` (the sanctioned entry — see app/CLAUDE.md → *H5AD / cell-data
# access*) with `granularity=:cell` + `cell_measures=[measure]` + `categorical=[measure]`, then
# filters + dedupes. No `label_props` / `h5open` on the hot path — the readers already do that
# and cache per-image.
#
# CATEGORY MATCHING is done via `_catkey_local` — an exact clone of `_catkey` in
# `app/src/plotting/plot_data.jl` (line 42, private). The frequency chart's response uses the
# same normalisation to build its `categories` list, so a category string the client got FROM
# that response round-trips here unchanged (numeric-looking floats collapse to their integer
# form, everything else is `string(v)`). Duplicate rather than expose because it's a
# one-liner and drift is caught by an api-side test (below).
#
# NB: `api/` does not `using DataFrames` — column access via `getproperty` (`df.track_id`) or
# `df[i, :name]` only, `length(v)` instead of `nrow`. Matches labels_api.jl / tracking_api.jl.

# ONE-LINER CLONE of app/src/plotting/plot_data.jl:42 (`_catkey`). Kept identical so a category
# string the client received from `/api/plot_data` (chartType="frequency") is comparable here.
_catkey_local(v) = (v isa Real && isfinite(v) && v == round(v)) ? string(Int(round(v))) : string(v)

const _TBC_DEFAULT_LIMIT = 5000     # cap on the returned id list — anything above this is not a
                                     # meaningful "selection" the user is going to inspect
const _TBC_MAX_LIMIT     = 50000

# Resolve one image + return the distinct positive track_ids where the measure column matches
# the category. Missing / non-finite / zero-track_id rows are dropped by construction.
function _tracks_by_category_for(img, value_name::AbstractString, pop_type::AbstractString,
                                 pop::AbstractString, measure::AbstractString,
                                 category::AbstractString)::Vector{Int}
    # `granularity=:cell` — we need per-cell rows so the measure filter is at the point of
    # observation; expanding tracks to their member cells is what pop_df does when asked. The
    # `categorical=[measure]` tag tells pop_df to keep the column typed as its category label
    # (rather than coercing to a numeric code); `cell_measures=[measure]` pulls the column in.
    df = pop_df(img, pop_type, [pop];
                value_name = value_name,
                granularity = :cell,
                cell_measures = [measure],
                categorical = [measure])
    # A pop with no matching cells (measure absent, or filtered to empty) returns an empty
    # frame — treat as "no matches" rather than 404, because a valid (image, pop) combination
    # is allowed to have zero cells for a given category.
    (length(df.label) == 0) && return Int[]
    # Column presence check is defensive: if the measure isn't attached to this pop's cells,
    # pop_df returns the frame without that column rather than throwing.
    measure in propertynames_str(df) || return Int[]
    has_tid = :track_id in propertynames(df)
    ids = Set{Int}()
    n = length(df.label)
    col = df[!, measure]
    tcol = has_tid ? df.track_id : nothing
    for i in 1:n
        v = col[i]
        (ismissing(v) || (v isa Real && !isfinite(v))) && continue
        _catkey_local(v) == category || continue
        if has_tid
            tid = tcol[i]
            (ismissing(tid) || tid <= 0) && continue
            push!(ids, Int(tid))
        end
    end
    collect(ids)
end

# `names(df)` returns Vector{String}; wrap so the file's api-convention "no DataFrames methods"
# stays honest — `propertynames` is Base, `String.(...)` maps to the same list. Kept local for
# clarity at the call site above.
propertynames_str(df) = String.(propertynames(df))

"""
    POST /api/tracks/by_category

Body: `{ projectUid, imageUids: [String], valueName, pop, popType?, measure, category, limit? }`
Reply: `{ trackIds: [Int], total: Int, truncated: Bool }`

Given a categorical measure and one of its category values, return the distinct positive
`track_id`s (across the requested images) whose cells match. Producer for the shared
`linkedSelection` store — the frontend brush primitive sits on top of this response.

- `popType` defaults to `"live"` (the behaviour page's cell-level pop_type; every summary panel
  there is `live`).
- `imageUids` is REQUIRED and non-empty. Set-scope callers pass the resolved image list;
  the server does not walk sets here (parity with `labels_api.jl` — set walk lives in
  `plotting_api.jl` where it's coupled to attribute grouping).
- `category` must match one of the values the frequency chart's response reports (they share
  `_catkey` normalisation). A category that no cell has → 200 with `trackIds: []`, not 404.
- `limit` caps the reply; `truncated: true` when the full id set was larger than the cap.
"""
function api_tracks_by_category(body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes), Dict{String,Any})
    catch
        return _gerr(400, "invalid JSON body")
    end
    proj      = string(get(body, "projectUid", ""))
    value_name = string(get(body, "valueName", ""))
    pop       = string(get(body, "pop", ""))
    pop_type  = string(get(body, "popType", "live"))
    measure   = string(get(body, "measure", ""))
    category  = string(get(body, "category", ""))
    isempty(proj)       && return _gerr(400, "projectUid required")
    isempty(value_name) && return _gerr(400, "valueName required")
    isempty(pop)        && return _gerr(400, "pop required")
    isempty(measure)    && return _gerr(400, "measure required")
    isempty(category)   && return _gerr(400, "category required")

    raw_uids = get(body, "imageUids", nothing)
    raw_uids isa AbstractVector || return _gerr(400, "imageUids required (non-empty array)")
    image_uids = String[string(u) for u in raw_uids if !isempty(string(u))]
    isempty(image_uids) && return _gerr(400, "imageUids required (non-empty array)")

    limit_raw = get(body, "limit", _TBC_DEFAULT_LIMIT)
    limit_int = limit_raw isa Number ? Int(limit_raw) :
                limit_raw isa AbstractString ? something(tryparse(Int, String(limit_raw)), _TBC_DEFAULT_LIMIT) :
                _TBC_DEFAULT_LIMIT
    limit = clamp(limit_int, 1, _TBC_MAX_LIMIT)

    # Per-image loop mirrors `plotting_api.jl` — resolve each id via `_gating_image` (which
    # already returns a typed (status, JSON body) tuple on failure); fold errors so ONE bad
    # image doesn't null the whole selection. A per-image failure is reported ONCE at 404 with
    # the first bad uid, so the client can fix its input.
    all_ids = Set{Int}()
    for uid in image_uids
        img, err = _gating_image(proj, uid)
        err === nothing || return err
        for tid in _tracks_by_category_for(img, value_name, pop_type, pop, measure, category)
            push!(all_ids, tid)
        end
    end

    ids = sort!(collect(all_ids))
    total = length(ids)
    truncated = total > limit
    truncated && (ids = ids[1:limit])
    return 200, JSON3.write((; trackIds = ids, total = total, truncated = truncated))
end
