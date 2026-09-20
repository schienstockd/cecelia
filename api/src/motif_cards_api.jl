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
# Request:
#   POST /api/motif_cards
#   { projectUid, rootUid, valueName, maxPx?=320, padPx?=8 }
# Response (same shape as /api/cell_cards):
#   { pool: [{uid, value_name}], cards: [ Card ], statScales: {name: [min, max]} }

using JSON3
using DataFrames: DataFrame, subset
using Statistics: mean, quantile

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

_five_num(xs::AbstractVector{<:Real}) = begin
    v = collect(skipmissing(xs))
    isempty(v) && return (min=0.0, q25=0.0, median=0.0, q75=0.0, max=0.0)
    v = sort!(Float64.(v))
    (min = v[1], q25 = quantile(v, 0.25), median = quantile(v, 0.5),
     q75 = quantile(v, 0.75), max = v[end])
end

# For one motif class:
#   • medoid instance  = the instance whose cells have the LOWEST mean `motif.distance`
#   • trace_history    = (t, x, y) of the medoid instance's cells, native pixels
#   • frames_ts        = [t0, mid, t1] of the instance's frame span (same three-frame rule as cellCards)
#   • stats            = 5-number summary of numeric motif features + motif.distance, over ALL cells
#                        of this class (not just the medoid — the footer summarises the CLASS)
function _motif_class_card(class_df::DataFrame, class_name::String, colour::String)
    :motif_instance_id in propertynames(class_df) ||
        error("motif_cards: class_df missing motif.instance_id")
    # Group by instance, mean(motif.distance) per instance, min wins.
    inst_ids   = Int[Int(round(Float64(x))) for x in class_df[!, :motif_instance_id] if !ismissing(x)]
    unique_ids = sort!(unique(inst_ids))
    isempty(unique_ids) && error("motif_cards: no motif.instance_id values on class $(class_name)")

    dists_by_inst = Dict{Int,Float64}()
    for iid in unique_ids
        sub = subset(class_df,
                     :motif_instance_id => x -> .!ismissing.(x) .& (Int.(round.(Float64.(x))) .== iid))
        ds = collect(skipmissing(sub[!, :motif_distance]))
        dists_by_inst[iid] = isempty(ds) ? Inf : mean(Float64.(ds))
    end
    medoid_iid = argmin(dists_by_inst)   # returns the KEY, since it's a Dict

    inst = subset(class_df,
                  :motif_instance_id => x -> .!ismissing.(x) .& (Int.(round.(Float64.(x))) .== medoid_iid))
    size(inst, 1) == 0 && error("motif_cards: medoid instance $(medoid_iid) has no cells on $(class_name)")

    # track_id: all cells of one instance share a track. Take the first non-missing.
    tids  = Int[Int(round(Float64(x))) for x in inst[!, :track_id] if !ismissing(x)]
    isempty(tids) && error("motif_cards: medoid instance has no track_id on $(class_name)")
    track_id = tids[1]

    ts = Int[Int(round(Float64(x))) for x in inst[!, :centroid_t] if !ismissing(x)]
    xs = Float64.(inst[!, :centroid_x])
    ys = Float64.(inst[!, :centroid_y])
    t0 = minimum(ts); t1 = maximum(ts)

    trace_history = Tuple{Int,Float64,Float64}[]
    for (tt, xx, yy) in zip(ts, xs, ys)
        (xx isa Real && yy isa Real) || continue
        push!(trace_history, (tt, Float64(xx), Float64(yy)))
    end
    sort!(trace_history; by = first)

    # Three-frame filmstrip: t0, mid, t1 (matches cellCards Decision 4). For an 8-frame window this
    # is t0, t0+4, t0+7 — the 8 available frames are dense so mid always maps back to a real cell.
    tmid = (t0 + t1) ÷ 2
    frames_ts = Int[t0, tmid, t1]

    # Stats over ALL cells of this class (not just the medoid instance) — this is a CLASS summary,
    # so the footer reads "typical speed / angle / distance for cells assigned to this class".
    stats = @NamedTuple{name::String, min::Float64, q25::Float64, median::Float64,
                       q75::Float64, max::Float64}[]
    for col in _MOTIF_CARD_FOOTER_COLS
        Symbol(col) in propertynames(class_df) || continue
        s = _five_num(class_df[!, Symbol(col)])
        push!(stats, (name = col, min = s.min, q25 = s.q25, median = s.median,
                      q75 = s.q75, max = s.max))
    end
    # motif.distance is always meaningful — bank it.
    if :motif_distance in propertynames(class_df)
        s = _five_num(class_df[!, :motif_distance])
        push!(stats, (name = "motif.distance", min = s.min, q25 = s.q25, median = s.median,
                      q75 = s.q75, max = s.max))
    end

    n_class = size(class_df, 1)   # how many cells were assigned to this class in this image

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
    # 2–4 entries), and the mtime check on the cells h5ad happens once per candidate.
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
    # names to underscored DataFrame column names (see `label_props.jl`) — so `motif.class` becomes
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

    # Motif class values as they appear in the h5ad, excluding missing / empty. `unique(String, …)`
    # collapses the categorical vector; sort alphabetically so palette colours are stable across
    # renders regardless of DataFrame row order.
    class_col = df[!, :motif_class]
    seen = Set{String}(); class_names = String[]
    for v in class_col
        ismissing(v) && continue
        s = String(v); isempty(s) && continue
        s in seen && continue
        push!(seen, s); push!(class_names, s)
    end
    sort!(class_names)
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
        sub = subset(df, :motif_class => x -> .!ismissing.(x) .& (String.(x) .== cn))
        size(sub, 1) == 0 && continue   # class listed in categories but has zero cells (rare)
        col = get(colours_by_class, cn, "#888888")
        card = try
            _motif_class_card(sub, cn, col)
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
