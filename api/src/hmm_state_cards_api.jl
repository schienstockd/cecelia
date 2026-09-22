# ── /api/hmm_state_cards — snapshot cards for HMM states ────────────────────────
#
# One card per HMM state value on a chosen `live.cell.hmm.state.<measure>` column of a chosen
# segmentation. Medoid = the STATE-RUN whose owning track spends the largest fraction of its life
# in this state (Fig 4c pattern; `docs/todo/BEHAVIOUR_CARDS_PLAN.md` Decision 3). Ties broken by
# state-run length (prefer longer runs).
#
# Discovery mirrors motif_cards_api: the server enumerates every segmentation whose h5ad carries
# at least one `live.cell.hmm.state.*` column (returned as `availableValueNames`), and returns the
# set of HMM columns on the chosen vn as `availableHmmCols`. The panel exposes both pickers via the
# canonical auto-hide toolbar shape; either can be omitted on the request to accept the server's
# resolution (first eligible / first column).
#
# Rendering goes through `render_medoid_filmstrip` (`api/src/behaviour_cards.jl`) with the
# instance-bbox override — the same crop discipline motif cards uses so a short state-run reads
# as more than a smudge.
#
# `api/` does not `using DataFrames` / `using Statistics` — inline mean/quantile like
# motif_cards_api.jl.
#
# Request:
#   POST /api/hmm_state_cards
#   { projectUid, rootUid, valueName?, hmmCol?, maxPx?=320, padPx?=8 }
# Response (shape shared with cell/motif cards + two picker fields):
#   { pool: [{uid, value_name}], cards: [Card], statScales: {name: [min, max]},
#     availableValueNames: [String], valueName: String,
#     availableHmmCols:    [String], hmmCol:    String }

using JSON3

_hmm_state_cards_dir(img_dir::String) = joinpath(img_dir, "analysis", "hmm_state_cards")
function _hmm_state_cards_sidecar(img_dir::String, value_name::String, hmm_col::String)
    # `hmm_col` is `live.cell.hmm.state.<measure>` — the dots are legal in a filename but visually
    # noisy; keep the raw name so `hmm_state_cards/T__live.cell.hmm.state.movement.json` is easy to
    # match up with the h5ad's obs key.
    joinpath(_hmm_state_cards_dir(img_dir), "$(value_name)__$(hmm_col).json")
end

# mtime of the cells h5ad — same discipline as motif_cards. Non-existent path → 0.0 → always rebuild.
function _hmm_h5ad_mtime(img::CciaImage, value_name::String)::Float64
    p = img_label_props_path(img, value_name)
    isfile(p) ? mtime(p) : 0.0
end

const _HMM_STATE_CARDS_SHAPE_VERSION = 1

function _hmm_state_cards_cache_fresh(sidecar_path::String, mtime_now::Float64,
                                       state_names_now::Vector{String})::Union{Nothing,Dict{String,Any}}
    isfile(sidecar_path) || return nothing
    doc = try JSON3.read(read(sidecar_path, String), Dict{String,Any}); catch; return nothing end
    get(doc, "shapeVersion", 0) == _HMM_STATE_CARDS_SHAPE_VERSION || return nothing
    got_mt = get(doc, "h5adMtime", nothing)
    got_mt isa Number && Float64(got_mt) == mtime_now || return nothing
    got_states = try
        sort!(String[String(x) for x in get(doc, "states", Any[])])
    catch; return nothing end
    got_states == sort(state_names_now) || return nothing
    doc
end

function _write_hmm_state_cards_sidecar(sidecar_path::String, pool, cards_json, stat_scales,
                                        state_names, mtime_now)
    mkpath(dirname(sidecar_path))
    write_json_atomic(sidecar_path, Dict{String,Any}(
        "shapeVersion" => _HMM_STATE_CARDS_SHAPE_VERSION,
        "pool"         => [Dict("uid" => pm.uid, "value_name" => pm.value_name) for pm in pool],
        "cards"        => cards_json,
        "statScales"   => stat_scales,
        "states"       => state_names,
        "h5adMtime"    => mtime_now))
end

# Footer stats — median of speed + angle over ALL cells of the state (not just the medoid run).
# Same shape as motif cards' `_MOTIF_CARD_FOOTER_COLS`, kept parallel.
const _HMM_CARD_FOOTER_COLS = String["live.cell.speed", "live.cell.angle"]

# Inline mean/quantile (api/ off Statistics dep) — same helpers motif_cards_api uses. Duplicated
# rather than shared to keep this file self-contained; extract to a shared helper if a fourth family
# needs them.
_hmm_mean_f64(v::AbstractVector{Float64})::Float64 =
    isempty(v) ? 0.0 : sum(v) / length(v)

# Hyndman-Fan Type 7 (Statistics.quantile default) on a sorted Vector{Float64}.
function _hmm_quantile_sorted(vs::AbstractVector{Float64}, q::Float64)::Float64
    n = length(vs)
    n == 0 && return 0.0
    n == 1 && return vs[1]
    h = q * (n - 1)
    lo = floor(Int, h) + 1
    hi = min(lo + 1, n)
    frac = h - (lo - 1)
    vs[lo] + frac * (vs[hi] - vs[lo])
end

function _hmm_five_num_from_vec(v::Vector{Float64})
    isempty(v) && return (min = 0.0, q25 = 0.0, median = 0.0, q75 = 0.0, max = 0.0)
    sort!(v)
    (min    = v[1],
     q25    = _hmm_quantile_sorted(v, 0.25),
     median = _hmm_quantile_sorted(v, 0.5),
     q75    = _hmm_quantile_sorted(v, 0.75),
     max    = v[end])
end

# Pull `col` values at `rows` from `df` into a `Vector{Float64}`, dropping missings/non-reals.
function _hmm_col_f64_at(df, col::Symbol, rows::AbstractVector{Int})::Vector{Float64}
    col in propertynames(df) || return Float64[]
    colv = getproperty(df, col)
    out = Float64[]
    for r in rows
        v = colv[r]; ismissing(v) && continue
        v isa Real || continue
        push!(out, Float64(v))
    end
    out
end

# ── medoid_state_run(state_col, tracks_col, ts_col, state_value) → (track_id, run_rows)
#
# For each unique track_id, compute the fraction of that track's cells whose state == state_value.
# Pick the track with the highest fraction (ties broken by the LONGER contiguous run of
# state_value in that track). Return the track's longest contiguous state_value run as an ordered
# vector of DataFrame row indices.
function _medoid_state_run(state_col::AbstractVector, tracks_col::AbstractVector,
                            ts_col::AbstractVector, state_value::Float64)
    # Group row indices by track_id, tracking cell-in-state count for the fraction denominator.
    rows_by_tid = Dict{Int,Vector{Int}}()
    in_state_by_tid = Dict{Int,Int}()
    n_by_tid = Dict{Int,Int}()
    for ri in eachindex(state_col)
        t = tracks_col[ri]
        (t isa Real && !ismissing(t) && !isnan(t) && t > 0) || continue
        tid = Int(round(Float64(t)))
        push!(get!(rows_by_tid, tid, Int[]), ri)
        n_by_tid[tid] = get(n_by_tid, tid, 0) + 1
        v = state_col[ri]
        v isa Real && !ismissing(v) && !isnan(v) && Float64(v) == state_value &&
            (in_state_by_tid[tid] = get(in_state_by_tid, tid, 0) + 1)
    end
    isempty(rows_by_tid) && return nothing
    # Fraction, then tie-break on the longest run below.
    tid_frac = Tuple{Int,Float64}[]
    for (tid, rows) in rows_by_tid
        in_ct = get(in_state_by_tid, tid, 0)
        in_ct == 0 && continue   # track has no cells in this state at all
        push!(tid_frac, (tid, in_ct / max(n_by_tid[tid], 1)))
    end
    isempty(tid_frac) && return nothing
    max_frac = maximum(x -> x[2], tid_frac)
    # Only tracks tied at the max fraction advance to the length tie-break — cheap: recompute the
    # longest run in each candidate track and keep the winner.
    winner_tid = 0; winner_run = Int[]
    for (tid, frac) in tid_frac
        frac == max_frac || continue
        rows = rows_by_tid[tid]
        # Order by t so contiguity is meaningful. Cells with missing/NaN t are skipped upstream (see
        # the group loop — a null-t cell wouldn't have been counted), but guard anyway.
        sort!(rows; by = r -> begin
            v = ts_col[r]
            (v isa Real && !ismissing(v) && !isnan(v)) ? Int(round(Float64(v))) : typemax(Int)
        end)
        # Walk the ordered rows; a "run" is a maximal contiguous stretch of the target state.
        best_run = Int[]
        cur = Int[]
        for r in rows
            v = state_col[r]
            in_state = v isa Real && !ismissing(v) && !isnan(v) && Float64(v) == state_value
            if in_state
                push!(cur, r)
            else
                length(cur) > length(best_run) && (best_run = cur)
                cur = Int[]
            end
        end
        length(cur) > length(best_run) && (best_run = cur)
        if length(best_run) > length(winner_run)
            winner_tid = tid
            winner_run = best_run
        end
    end
    winner_tid == 0 && return nothing
    (track_id = winner_tid, run_rows = winner_run)
end

# For one HMM state value:
#   • medoid state-run: track with highest fraction of cells in this state (tie → longer run)
#   • trace_history:    (t, x, y) of the medoid run's cells, native pixels, sorted by t
#   • frames_ts:        [t0, mid, t1] of the run's frame span (same rule as motif cards)
#   • stats:            5-number summary of live.cell.speed + live.cell.angle over ALL state cells
function _hmm_state_card(df, state_indices::Vector{Int}, state_value::Float64,
                          state_name::String, colour::String)
    tracks_col = df.track_id
    ts_col     = df.centroid_t
    xs_col     = df.centroid_x
    ys_col     = df.centroid_y

    # Restrict medoid resolution to state cells (state_col at those rows equals state_value).
    hmm_col_sym = Symbol(_HMM_COL_FOR_CARD[])  # scratched via ref, set by caller
    hmm_state_col = getproperty(df, hmm_col_sym)
    medoid = _medoid_state_run(hmm_state_col, tracks_col, ts_col, state_value)
    medoid === nothing &&
        error("hmm_state_cards: no track carries state $(state_name)")
    track_id = medoid.track_id
    run_rows = medoid.run_rows

    # (t, x, y) for the medoid run's cells, native pixels, sorted by t.
    trace_history = Tuple{Int,Float64,Float64}[]
    for r in run_rows
        tt = ts_col[r]; xx = xs_col[r]; yy = ys_col[r]
        (tt isa Real && xx isa Real && yy isa Real) || continue
        push!(trace_history, (Int(round(Float64(tt))), Float64(xx), Float64(yy)))
    end
    sort!(trace_history; by = first)
    isempty(trace_history) &&
        error("hmm_state_cards: medoid run has no centroids on state $(state_name)")
    t0 = trace_history[1][1]; t1 = trace_history[end][1]
    tmid = (t0 + t1) ÷ 2
    frames_ts = Int[t0, tmid, t1]

    # Stats over ALL cells of this state — same discipline as motif cards' footer.
    stats = @NamedTuple{name::String, min::Float64, q25::Float64, median::Float64,
                       q75::Float64, max::Float64}[]
    for col in _HMM_CARD_FOOTER_COLS
        vs = _hmm_col_f64_at(df, Symbol(col), state_indices)
        isempty(vs) && continue
        s = _hmm_five_num_from_vec(vs)
        push!(stats, (name = col, min = s.min, q25 = s.q25, median = s.median,
                      q75 = s.q75, max = s.max))
    end

    n_state = length(state_indices)

    (name = state_name, colour = colour, n = Int(n_state),
     medoid = (track_id = track_id, t0 = Int(t0), t1 = Int(t1)),
     trace_history = trace_history, frames_ts = frames_ts, stats = stats)
end

# The active hmm column name for the current request — read by `_hmm_state_card` above so it can
# fetch `df[!, hmm_col]` under whatever dotted name the h5ad uses. A `Ref` (rather than a param)
# keeps the medoid-resolution call site symmetric with motif_cards' `_motif_class_card`, which
# doesn't need this because motif classes always live under `motif.class`.
const _HMM_COL_FOR_CARD = Ref{String}("")

function api_hmm_state_cards(body_bytes::Vector{UInt8})
    data = try JSON3.read(String(body_bytes)); catch; nothing end
    data === nothing && return 400, JSON3.write((; error = "invalid JSON body"))

    pu       = _wstr(data, :projectUid)
    root_uid = _wstr_any(data, :rootUid,    :root_uid)
    vn       = _wstr_any(data, :valueName,  :value_name)
    hmm_col  = _wstr_any(data, :hmmCol,     :hmm_col)
    (isempty(pu) || isempty(root_uid)) &&
        return 400, JSON3.write((; error = "projectUid, rootUid required"))

    max_px = Int(round(Float64(get(data, :maxPx, get(data, :max_px, 320)))))
    pad_px = Int(round(Float64(get(data, :padPx, get(data, :pad_px, 8)))))

    img, gerr = _gating_image(pu, root_uid)
    gerr === nothing || return gerr[1], gerr[2]["body"]

    # Enumerate every segmentation whose h5ad carries at least one `live.cell.hmm.state.*` column.
    # Same shape as motif_cards' auto-pick.
    candidates = try
        String[String(k) for k in versioned_keys(img.label_props)]
    catch; String[] end
    _hmm_cols_on = function (cand::String)
        p = try img_label_props_path(img, cand); catch; ""; end
        (isempty(p) || !isfile(p)) && return String[]
        try
            lp = label_props(p; value_name = cand)
            String[c for c in col_names(lp; data_type = :obs)
                   if startswith(String(c), "live.cell.hmm.state.")]
        catch; String[] end
    end
    hmm_cols_by_vn = Dict{String,Vector{String}}()
    for cand in candidates
        cols = _hmm_cols_on(cand)
        isempty(cols) || (hmm_cols_by_vn[cand] = cols)
    end
    available_vns = sort!(collect(keys(hmm_cols_by_vn)))
    isempty(available_vns) &&
        return 404, JSON3.write((; error = "no segmentation with live.cell.hmm.state.* on $(root_uid) — run HMM states first"))

    # Explicit valueName must be one of the eligible vns; a stale persisted pick would otherwise
    # silently render an empty (or wrong) segmentation.
    if !isempty(vn) && !(vn in available_vns)
        return 404, JSON3.write((; error = "no live.cell.hmm.state.* column on $(vn) — run HMM states first",
                                    availableValueNames = available_vns))
    end
    isempty(vn) && (vn = available_vns[1])
    available_hmm_cols = hmm_cols_by_vn[vn]

    # Explicit hmmCol must be one of THIS vn's columns; else fall back to the first.
    if !isempty(hmm_col) && !(hmm_col in available_hmm_cols)
        return 404, JSON3.write((; error = "column $(hmm_col) not on $(vn)",
                                    availableValueNames = available_vns,
                                    availableHmmCols    = available_hmm_cols,
                                    valueName           = String(vn)))
    end
    isempty(hmm_col) && (hmm_col = available_hmm_cols[1])

    # Read ALL live/tracked cells with the columns hmm cards need. `as_df` returns obs columns
    # under their RAW h5ad names — dots are NOT rewritten — so the state column is
    # `df[!, "live.cell.hmm.state.<name>"]`, not `df.live_cell_hmm_state_<name>`.
    df = try
        pop_df(img, "live", ["/_tracked"]; value_name = vn, granularity = :cell,
               centroids = :pixel, include_obs = true)
    catch e
        return 500, JSON3.write((; error = "pop_df failed: $(sprint(showerror, e))"))
    end

    Symbol(hmm_col) in propertynames(df) ||
        return 404, JSON3.write((; error = "no $(hmm_col) column on $(vn) after read",
                                    availableValueNames = available_vns,
                                    availableHmmCols    = available_hmm_cols,
                                    valueName           = String(vn)))

    # Group row indices by state value (numeric — the runner banks HMM states as Float64 codes,
    # rendered as "state N" for card names). Skip missing/NaN rows.
    state_col = df[!, hmm_col]
    state_indices_by_value = Dict{Float64,Vector{Int}}()
    for ri in eachindex(state_col)
        v = state_col[ri]
        (v isa Real && !ismissing(v) && !isnan(v)) || continue
        push!(get!(state_indices_by_value, Float64(v), Int[]), ri)
    end
    state_values = sort!(collect(keys(state_indices_by_value)))
    state_names  = String["state $(Int(round(v)))" for v in state_values]
    isempty(state_names) &&
        return 404, JSON3.write((; error = "no HMM states assigned in $(hmm_col) on $(vn) — HMM ran but assigned nothing",
                                    availableValueNames = available_vns,
                                    availableHmmCols    = available_hmm_cols,
                                    valueName           = String(vn),
                                    hmmCol              = String(hmm_col)))

    # Cache check on the h5ad's mtime + the discovered state set + the picked hmm column.
    sidecar = _hmm_state_cards_sidecar(img._dir, String(vn), String(hmm_col))
    mtime_now = _hmm_h5ad_mtime(img, String(vn))
    cached = _hmm_state_cards_cache_fresh(sidecar, mtime_now, state_names)
    if cached !== nothing
        return 200, JSON3.write(Dict{String,Any}(
            "pool"                => get(cached, "pool", Any[]),
            "cards"               => get(cached, "cards", Any[]),
            "statScales"          => get(cached, "statScales", Dict{String,Any}()),
            "availableValueNames" => available_vns,
            "valueName"           => String(vn),
            "availableHmmCols"    => available_hmm_cols,
            "hmmCol"              => String(hmm_col)))
    end

    # Colours: fall through `colour_by_palette` on `pop_map` for this vn, keyed by the hmm column
    # name (so any future user override on HMM states lines up with the frequency-plot palette).
    pop_map = load_pop_map(img._dir, String(vn); pop_type = "live")
    colours_by_state = colour_by_palette(pop_map, String(hmm_col), state_names)

    _HMM_COL_FOR_CARD[] = String(hmm_col)  # read by _hmm_state_card via propertynames(df)
    cards_meta = @NamedTuple{name::String, colour::String, n::Int,
                             medoid::@NamedTuple{track_id::Int, t0::Int, t1::Int},
                             trace_history::Vector{Tuple{Int,Float64,Float64}},
                             frames_ts::Vector{Int},
                             stats::Vector{@NamedTuple{name::String, min::Float64, q25::Float64,
                                                       median::Float64, q75::Float64, max::Float64}}}[]
    for (vv, sn) in zip(state_values, state_names)
        rows = state_indices_by_value[vv]
        col = get(colours_by_state, sn, "#888888")
        card = try
            _hmm_state_card(df, rows, vv, sn, col)
        catch e
            # Skip states with no eligible medoid (e.g. state value present only in null-track
            # cells) rather than 500 the whole panel — the family gates on `state_names` non-empty
            # already; a missing card is more useful feedback than an aborted response.
            continue
        end
        push!(cards_meta, card)
    end
    isempty(cards_meta) &&
        return 404, JSON3.write((; error = "no renderable HMM states on $(vn) — every state has zero tracked cells",
                                    availableValueNames = available_vns,
                                    availableHmmCols    = available_hmm_cols,
                                    valueName           = String(vn),
                                    hmmCol              = String(hmm_col)))

    # Per-card INSTANCE bbox — the state-RUN's own extent, NOT the whole track's lifetime. Same
    # discipline as motif cards.
    _hmm_instance_bbox(tr) = begin
        isempty(tr) && return nothing
        xs = Int[Int(round(row[2])) for row in tr]
        ys = Int[Int(round(row[3])) for row in tr]
        (x = (minimum(xs), maximum(xs)), y = (minimum(ys), maximum(ys)))
    end
    instance_bboxes = Any[_hmm_instance_bbox(c.trace_history) for c in cards_meta]
    uniform_side = 0
    for bb in instance_bboxes
        bb === nothing && continue
        uniform_side = max(uniform_side, bb.x[2] - bb.x[1] + 1, bb.y[2] - bb.y[1] + 1)
    end
    uniform_side = uniform_side > 0 ?
        max(uniform_side + 2 * pad_px, max_px ÷ 3) : nothing

    interval_s = try
        :T in img_scale_axes(img) ? Float64(img_physical_sizes(img)[2]) * 60.0 : nothing
    catch; nothing end

    cards_json = Any[]
    for (ci, c) in enumerate(cards_meta)
        filmstrip = render_medoid_filmstrip(img, String(vn), c.medoid.track_id, c.frames_ts, pu;
                                            trace_history = c.trace_history,
                                            trace_colour  = hex_to_rgb(c.colour),
                                            max_px = max_px, pad_px = pad_px,
                                            crop_side = uniform_side,
                                            bbox_override = instance_bboxes[ci],
                                            interval_s = interval_s)
        push!(cards_json, Dict{String,Any}(
            "path"      => c.name,
            "name"      => c.name,
            "colour"    => c.colour,
            "n"         => c.n,
            "medoid"    => Dict("uid" => img.uid, "value_name" => String(vn),
                                "track_id" => c.medoid.track_id,
                                "frames" => [c.medoid.t0, c.medoid.t1]),
            "filmstrip" => filmstrip,
            "stats"     => [Dict("name" => s.name,
                                 "min" => s.min, "q25" => s.q25, "median" => s.median,
                                 "q75" => s.q75, "max" => s.max) for s in c.stats]))
    end

    stat_scales = Dict{String,Vector{Float64}}()
    for c in cards_meta, s in c.stats
        cur = get(stat_scales, s.name, nothing)
        stat_scales[s.name] = cur === nothing ? Float64[s.min, s.max] :
                              Float64[min(cur[1], s.min), max(cur[2], s.max)]
    end

    pool = [(uid = img.uid, value_name = String(vn))]
    _write_hmm_state_cards_sidecar(sidecar, pool, cards_json, stat_scales, state_names, mtime_now)

    200, JSON3.write(Dict{String,Any}(
        "pool"                => [Dict("uid" => pm.uid, "value_name" => pm.value_name) for pm in pool],
        "cards"               => cards_json,
        "statScales"          => stat_scales,
        "availableValueNames" => available_vns,
        "valueName"           => String(vn),
        "availableHmmCols"    => available_hmm_cols,
        "hmmCol"              => String(hmm_col)))
end
