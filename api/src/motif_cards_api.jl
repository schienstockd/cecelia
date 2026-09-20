# ── /api/motif_cards — snapshot cards for motif classes ─────────────────────────────
#
# One card per motif class discovered in the requested image's `{vn}.h5ad` `motif.class` column.
# Medoid resolution runs at query time from the same h5ad's `motif.class` / `motif.distance` /
# `motif.instance_id` columns (`docs/todo/MOTIF_DISCOVERY_PLAN.md` Decision 12) — no re-computation
# of DTW, no dependency on the motiffeatures.json sidecar for medoid info (the sidecar bank of
# medoids named in `BEHAVIOUR_CARDS_PLAN.md` is still a future write; medoids on disk today are the
# per-cell assignments the runner leaves in obs).
#
# Rendering goes through `render_medoid_filmstrip` in `api/src/behaviour_cards.jl` — the shared
# spine landed in Phase 1 for cellCards. Overlay per t: dot at the medoid's cell + tail of the
# medoid's instance path in the motif class's own palette colour.
#
# Panel picker: NONE for MVP. Every motif class discovered in the h5ad becomes one card. Panel
# rides `rail: 'none'` (`docs/todo/BEHAVIOUR_CARDS_PLAN.md` Decision 6 re-scoped 2026-09-20 —
# `livePops` was speculative; motif classes don't live on the pop rail today, so the class list is
# server-discovered). Once `motifs` pop_type lands the panel gets a real picker.
#
# `api/` does not `using DataFrames` (convention: only `app/` depends on it; the api handlers pull
# columns off DataFrame returns without importing the type). Same for `Statistics` — mean/quantile
# are inlined below rather than added as a new stdlib dep on this package.
#
# Request:
#   POST /api/motif_cards
#   { projectUid, rootUid, valueName?, maxPx?=320, padPx?=8 }
# Response (same shape as /api/cell_cards):
#   { pool: [{uid, value_name}], cards: [ Card ], statScales: {name: [min, max]} }

using JSON3

_motif_cards_dir(img_dir::String) = joinpath(img_dir, "analysis", "motif_cards")
_motif_cards_sidecar(img_dir::String, value_name::String) =
    joinpath(_motif_cards_dir(img_dir), "$(value_name).json")

# mtime of the cells h5ad — the file the motif discovery writer touches. Non-existent path → 0.0
# so a stale-then-missing degrades to "always rebuild".
function _motif_h5ad_mtime(img::CciaImage, value_name::String)::Float64
    p = img_label_props_path(img, value_name)
    isfile(p) ? mtime(p) : 0.0
end

const _MOTIF_CARDS_SHAPE_VERSION = 1

function _motif_cards_cache_fresh(sidecar_path::String, mtime_now::Float64,
                                   class_names_now::Vector{String})::Union{Nothing,Dict{String,Any}}
    isfile(sidecar_path) || return nothing
    doc = try JSON3.read(read(sidecar_path, String), Dict{String,Any}); catch; return nothing end
    get(doc, "shapeVersion", 0) == _MOTIF_CARDS_SHAPE_VERSION || return nothing
    got_mt = get(doc, "h5adMtime", nothing)
    got_mt isa Number && Float64(got_mt) == mtime_now || return nothing
    got_classes = try
        sort!(String[String(x) for x in get(doc, "classes", Any[])])
    catch; return nothing end
    got_classes == sort(class_names_now) || return nothing
    doc
end

function _write_motif_cards_sidecar(sidecar_path::String, pool, cards_json, stat_scales,
                                    class_names, mtime_now)
    mkpath(dirname(sidecar_path))
    write_json_atomic(sidecar_path, Dict{String,Any}(
        "shapeVersion" => _MOTIF_CARDS_SHAPE_VERSION,
        "pool"         => [Dict("uid" => pm.uid, "value_name" => pm.value_name) for pm in pool],
        "cards"        => cards_json,
        "statScales"   => stat_scales,
        "classes"      => class_names,
        "h5adMtime"    => mtime_now))
end

# Numeric feature columns whose median enters the card footer. `live.cell.hmm.state.movement` is
# also a motif feature (see `MotifDiscoveryParams.featureCols`) but it's categorical, so its 5-num
# summary would be meaningless — omitted here rather than silently degrade to code numbers.
const _MOTIF_CARD_FOOTER_COLS = String["live.cell.speed", "live.cell.angle"]

_mean_f64(v::AbstractVector{Float64})::Float64 =
    isempty(v) ? 0.0 : sum(v) / length(v)

# Linear-interpolation quantile on an already-sorted Vector{Float64}. Matches `Statistics.quantile`'s
# default definition (Hyndman-Fan Type 7), which is what cellCards uses through the app package.
# Inline here so `api/` stays off the `Statistics` dep — same convention as the no-DataFrames rule.
function _quantile_sorted(vs::AbstractVector{Float64}, q::Float64)::Float64
    n = length(vs)
    n == 0 && return 0.0
    n == 1 && return vs[1]
    h = q * (n - 1)
    lo = floor(Int, h) + 1
    hi = min(lo + 1, n)
    frac = h - (lo - 1)
    vs[lo] + frac * (vs[hi] - vs[lo])
end

function _five_num_from_vec(v::Vector{Float64})
    isempty(v) && return (min = 0.0, q25 = 0.0, median = 0.0, q75 = 0.0, max = 0.0)
    sort!(v)
    (min    = v[1],
     q25    = _quantile_sorted(v, 0.25),
     median = _quantile_sorted(v, 0.5),
     q75    = _quantile_sorted(v, 0.75),
     max    = v[end])
end

# Pull `col` values at `rows` from `df` into a `Vector{Float64}`, dropping missings/non-reals.
function _col_f64_at(df, col::Symbol, rows::AbstractVector{Int})::Vector{Float64}
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

# For one motif class:
#   • medoid instance  = the instance whose cells have the LOWEST mean `motif.distance`
#   • trace_history    = (t, x, y) of the medoid instance's cells, native pixels
#   • frames_ts        = [t0, mid, t1] of the instance's frame span (same three-frame rule as cellCards)
#   • stats            = 5-number summary of numeric motif features + motif.distance, over ALL cells
#                        of this class (not just the medoid — the footer summarises the CLASS)
function _motif_class_card(df, class_indices::Vector{Int}, class_name::String, colour::String)
    :motif_instance_id in propertynames(df) ||
        error("motif_cards: df missing motif.instance_id")
    inst_ids_col = df.motif_instance_id
    dists_col    = df.motif_distance
    tracks_col   = df.track_id
    ts_col       = df.centroid_t
    xs_col       = df.centroid_x
    ys_col       = df.centroid_y

    # Group class rows by instance_id.
    inst_rows_by_id = Dict{Int,Vector{Int}}()
    for ri in class_indices
        v = inst_ids_col[ri]; ismissing(v) && continue
        iid = Int(round(Float64(v)))
        push!(get!(inst_rows_by_id, iid, Int[]), ri)
    end
    isempty(inst_rows_by_id) &&
        error("motif_cards: no motif.instance_id values on class $(class_name)")

    # Mean(motif.distance) per instance — min wins.
    dists_by_inst = Dict{Int,Float64}()
    for (iid, rows) in inst_rows_by_id
        ds = Float64[]
        for r in rows
            v = dists_col[r]; ismissing(v) && continue
            push!(ds, Float64(v))
        end
        dists_by_inst[iid] = isempty(ds) ? Inf : _mean_f64(ds)
    end
    medoid_iid = argmin(dists_by_inst)   # returns the Dict KEY (Int)
    inst_rows  = inst_rows_by_id[medoid_iid]

    # track_id: first non-missing in the medoid instance (all cells of one instance share a track).
    track_id = 0; found = false
    for r in inst_rows
        v = tracks_col[r]; ismissing(v) && continue
        track_id = Int(round(Float64(v))); found = true; break
    end
    found || error("motif_cards: medoid instance has no track_id on $(class_name)")

    # (t, x, y) for the medoid instance's cells, native pixels, sorted by t.
    trace_history = Tuple{Int,Float64,Float64}[]
    for r in inst_rows
        tt = ts_col[r]; xx = xs_col[r]; yy = ys_col[r]
        (tt isa Real && xx isa Real && yy isa Real) || continue
        push!(trace_history, (Int(round(Float64(tt))), Float64(xx), Float64(yy)))
    end
    sort!(trace_history; by = first)
    isempty(trace_history) &&
        error("motif_cards: medoid instance has no centroids on $(class_name)")
    t0 = trace_history[1][1]; t1 = trace_history[end][1]

    # Three-frame filmstrip: t0, mid, t1 (matches cellCards Decision 4). For an 8-frame window this
    # is t0, t0+4, t0+7 — the 8 available frames are dense so mid always maps back to a real cell.
    tmid = (t0 + t1) ÷ 2
    frames_ts = Int[t0, tmid, t1]

    # Stats over ALL cells of this class — footer summarises the CLASS, not the medoid.
    stats = @NamedTuple{name::String, min::Float64, q25::Float64, median::Float64,
                       q75::Float64, max::Float64}[]
    for col in _MOTIF_CARD_FOOTER_COLS
        vs = _col_f64_at(df, Symbol(col), class_indices)
        isempty(vs) && continue
        s = _five_num_from_vec(vs)
        push!(stats, (name = col, min = s.min, q25 = s.q25, median = s.median,
                      q75 = s.q75, max = s.max))
    end
    let vs = _col_f64_at(df, :motif_distance, class_indices)
        if !isempty(vs)
            s = _five_num_from_vec(vs)
            push!(stats, (name = "motif.distance", min = s.min, q25 = s.q25, median = s.median,
                          q75 = s.q75, max = s.max))
        end
    end

    n_class = length(class_indices)

    (name = class_name, colour = colour, n = Int(n_class),
     medoid = (track_id = track_id, t0 = Int(t0), t1 = Int(t1), instance_id = Int(medoid_iid)),
     trace_history = trace_history, frames_ts = frames_ts, stats = stats)
end

function api_motif_cards(body_bytes::Vector{UInt8})
    data = try JSON3.read(String(body_bytes)); catch; nothing end
    data === nothing && return 400, JSON3.write((; error = "invalid JSON body"))

    pu       = _wstr(data, :projectUid)
    root_uid = _wstr_any(data, :rootUid,    :root_uid)
    vn       = _wstr_any(data, :valueName,  :value_name)
    (isempty(pu) || isempty(root_uid)) &&
        return 400, JSON3.write((; error = "projectUid, rootUid required"))

    max_px = Int(round(Float64(get(data, :maxPx, get(data, :max_px, 320)))))
    pad_px = Int(round(Float64(get(data, :padPx, get(data, :pad_px, 8)))))

    img, gerr = _gating_image(pu, root_uid)
    gerr === nothing || return gerr[1], gerr[2]["body"]

    # Auto-pick the first segmentation with `motif.class` when the caller omits `valueName`. Keeps
    # MVP simple — user drops the panel on the board and cards appear; later a per-panel valueName
    # picker can override this. Scan is bounded by the image's registered label_props keys (usually
    # 2–4 entries).
    if isempty(vn)
        candidates = try
            String[String(k) for k in versioned_keys(img.label_props)]
        catch; String[] end
        picked = ""
        for cand in candidates
            p = try img_label_props_path(img, cand); catch; ""; end
            (isempty(p) || !isfile(p)) && continue
            has_motif = try
                lp = label_props(p; value_name = cand)
                "motif.class" in col_names(lp; data_type = :obs)
            catch; false end
            if has_motif; picked = cand; break; end
        end
        isempty(picked) &&
            return 404, JSON3.write((; error = "no segmentation with motif.class on $(root_uid) — run motif discovery first"))
        vn = picked
    end

    # Read ALL live/tracked cells with the columns motif cards need. `pop_df` normalises dotted obs
    # names to underscored DataFrame column names (see `label_props.jl`) — `motif.class` becomes
    # `motif_class` here.
    df = try
        pop_df(img, "live", ["/_tracked"]; value_name = vn, granularity = :cell,
               centroids = :pixel, include_obs = true)
    catch e
        return 500, JSON3.write((; error = "pop_df failed: $(sprint(showerror, e))"))
    end

    :motif_class in propertynames(df) ||
        return 404, JSON3.write((; error = "no motif.class column on $(vn) — run motif discovery first"))
    :motif_distance    in propertynames(df) || return 404, JSON3.write((; error = "no motif.distance column on $(vn)"))
    :motif_instance_id in propertynames(df) || return 404, JSON3.write((; error = "no motif.instance_id column on $(vn)"))

    # Group class-cell rows by class name — one pass over motif.class, index-based (no subset).
    class_col = df.motif_class
    class_indices_by_name = Dict{String,Vector{Int}}()
    for ri in eachindex(class_col)
        v = class_col[ri]; ismissing(v) && continue
        s = String(v); isempty(s) && continue
        push!(get!(class_indices_by_name, s, Int[]), ri)
    end
    class_names = sort!(collect(keys(class_indices_by_name)))
    isempty(class_names) &&
        return 404, JSON3.write((; error = "no motif classes assigned in $(vn) — motif discovery ran but assigned nothing"))

    # Cache check on the cells h5ad's mtime + the discovered class set.
    sidecar = _motif_cards_sidecar(img._dir, String(vn))
    mtime_now = _motif_h5ad_mtime(img, String(vn))
    cached = _motif_cards_cache_fresh(sidecar, mtime_now, class_names)
    if cached !== nothing
        return 200, JSON3.write(Dict{String,Any}(
            "pool"       => get(cached, "pool", Any[]),
            "cards"      => get(cached, "cards", Any[]),
            "statScales" => get(cached, "statScales", Dict{String,Any}())))
    end

    # Colours: the canonical categorical-colour helper. `load_pop_map` (live pop map for this vn) may
    # define user-filtered pops on motif.class one day; today it doesn't, so every class falls
    # through to `default_palette = OKABE_ITO`. Parity with any future pop-manager overrides is
    # automatic — one rule, one source of truth (`clustering_colour.jl`).
    pop_map = load_pop_map(img._dir, String(vn); pop_type = "live")
    colours_by_class = colour_by_palette(pop_map, "motif.class", class_names)

    # Per-class computations. Uniform crop side across the response so cards are visually comparable
    # (same rule as cellCards): first pass computes each medoid's bbox for the side, second pass
    # renders with the shared side.
    cards_meta = @NamedTuple{name::String, colour::String, n::Int,
                             medoid::@NamedTuple{track_id::Int, t0::Int, t1::Int, instance_id::Int},
                             trace_history::Vector{Tuple{Int,Float64,Float64}},
                             frames_ts::Vector{Int},
                             stats::Vector{@NamedTuple{name::String, min::Float64, q25::Float64,
                                                       median::Float64, q75::Float64, max::Float64}}}[]
    for cn in class_names
        rows = class_indices_by_name[cn]
        isempty(rows) && continue   # class listed but no rows (rare — categorical stub with no cells)
        col = get(colours_by_class, cn, "#888888")
        card = try
            _motif_class_card(df, rows, cn, col)
        catch e
            return 500, JSON3.write((; error = "medoid resolution failed for class $(cn): $(sprint(showerror, e))"))
        end
        push!(cards_meta, card)
    end
    isempty(cards_meta) &&
        return 404, JSON3.write((; error = "no renderable motif classes on $(vn) — every class has zero cells"))

    # Uniform crop = max bbox extent across every medoid track + 2×pad_px, clamped by the renderer.
    uniform_side = 0
    for c in cards_meta
        bx = try
            bb = track_bbox(img, String(vn), c.medoid.track_id; pad_px = 0)
            max(bb.x[2] - bb.x[1] + 1, bb.y[2] - bb.y[1] + 1)
        catch; 0 end
        uniform_side = max(uniform_side, bx)
    end
    uniform_side = uniform_side > 0 ? uniform_side + 2 * pad_px : nothing

    # TimeIncrement seconds for the active image (same discipline as cellCards — a medoid on a
    # different vn version could have a filepath whose OME-XML lacks the T axis).
    interval_s = try
        :T in img_scale_axes(img) ? Float64(img_physical_sizes(img)[2]) * 60.0 : nothing
    catch; nothing end

    cards_json = Any[]
    for c in cards_meta
        filmstrip = render_medoid_filmstrip(img, String(vn), c.medoid.track_id, c.frames_ts, pu;
                                            trace_history = c.trace_history,
                                            trace_colour  = hex_to_rgb(c.colour),
                                            max_px = max_px, pad_px = pad_px,
                                            crop_side = uniform_side, interval_s = interval_s)
        push!(cards_json, Dict{String,Any}(
            # `path` on the frontend Card is the identity key — for motif cards it is the class name
            # (there is no pop tree path today).
            "path"      => c.name,
            "name"      => c.name,
            "colour"    => c.colour,
            "n"         => c.n,
            "medoid"    => Dict("uid" => img.uid, "value_name" => String(vn),
                                "track_id" => c.medoid.track_id,
                                "frames" => [c.medoid.t0, c.medoid.t1],
                                "motif_instance_id" => c.medoid.instance_id),
            "filmstrip" => filmstrip,
            "stats"     => [Dict("name" => s.name,
                                 "min" => s.min, "q25" => s.q25, "median" => s.median,
                                 "q75" => s.q75, "max" => s.max) for s in c.stats]))
    end

    # statScales: pool-wide [globalMin, globalMax] per measure across every card. Same discipline as
    # cellCards — the mini-boxplot rows are comparable card-to-card.
    stat_scales = Dict{String,Vector{Float64}}()
    for c in cards_meta, s in c.stats
        cur = get(stat_scales, s.name, nothing)
        stat_scales[s.name] = cur === nothing ? Float64[s.min, s.max] :
                              Float64[min(cur[1], s.min), max(cur[2], s.max)]
    end

    pool = [(uid = img.uid, value_name = String(vn))]
    _write_motif_cards_sidecar(sidecar, pool, cards_json, stat_scales, class_names, mtime_now)

    200, JSON3.write(Dict{String,Any}(
        "pool"       => [Dict("uid" => pm.uid, "value_name" => pm.value_name) for pm in pool],
        "cards"      => cards_json,
        "statScales" => stat_scales))
end
