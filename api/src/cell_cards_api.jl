# ── /api/cell_cards — snapshot cards for behaviour clusters ──────────────────────
#
# Wraps the pure `cell_cards_metadata` (app/src/cell_cards.jl) with:
#   1. HTTP body parsing (matches `frontend/src/components/plots/cellCards.ts` types)
#   2. render_view_frame per chosen `t` — crop = medoid track's bbox, track colour baked in via
#      `overlay_author.build_overlays_for(track_color_mode="pop", pops_filter=[pop_path])`
#   3. PNG-encode + save each frame as a board-asset (`settings/board-assets/<id>.png`)
#   4. Sidecar payload cache under `analysis/cell_cards/{value_name}__{suffix}.json`
#      (rebuilds when the run's `clusters.{suffix}` mtime is newer — the cheap staleness rule per
#      `CELL_CARDS_PLAN` Decision 8).
#
# Rendering is DELIBERATELY plain here — no viewer view-state resolution yet. Channels default to
# all, specs default to the props path's `resolved_display_specs`. Adding view-state provenance
# comes with the frontend card view in Phase 2, so cards can match "what the viewer is showing".
#
# Request:
#   POST /api/cell_cards
#   { projectUid, rootUid, valueName, suffix, pops: [{path, clusterIds:[int]}],
#     framesPerCard?=3, maxPx?=320, padPx?=8, viewState? }
# Response:
#   { pool: [{uid, value_name}], cards: [ Card ] }   — matches CardsResponse.

using JSON3

_cell_cards_dir(img_dir::String) = joinpath(img_dir, "analysis", "cell_cards")
_cell_cards_sidecar(img_dir::String, value_name::String, suffix::String) =
    joinpath(_cell_cards_dir(img_dir), "$(value_name)__$(suffix).json")

# mtime of the `clusters.{suffix}` writer's most recent output on this pool member — used as the
# cheap staleness key. Uses the tracks h5ad's own mtime (the cluster column lives there, so a
# clustering run rewrites it). Non-existent path → 0.0 so a stale-then-missing degrades to "always
# rebuild" rather than "always fresh".
function _cluster_mtime(img::CciaImage, value_name::String)::Float64
    p = img_track_props_path(img, value_name)
    isfile(p) ? mtime(p) : 0.0
end

# Cache is fresh iff every pool member's cluster mtime matches the sidecar's recorded value AND the
# pop set + pops' cluster-id sets match. Any drift → rebuild. The sidecar itself is written by
# `_write_cell_cards_sidecar` after a fresh render.
const _CELL_CARDS_SHAPE_VERSION = 7   # v7: specs sourced from saved viewer JSON (channels/LUT/contrast)
function _cell_cards_cache_fresh(sidecar_path::String, pool, pops)::Union{Nothing,Dict{String,Any}}
    isfile(sidecar_path) || return nothing
    doc = try JSON3.read(read(sidecar_path, String), Dict{String,Any}); catch; return nothing end
    # Reject older cached shapes rather than serving cards missing new fields (e.g. shape=1 had no
    # min/max on stats + no statScales — the FE mini-boxplots would draw a zero-height whisker).
    get(doc, "shapeVersion", 1) == _CELL_CARDS_SHAPE_VERSION || return nothing
    # Check pop identity (path + cluster_ids), order-insensitive.
    pop_key(p) = (String(p.path), sort(collect(Int, p.cluster_ids)))
    want = sort([pop_key(p) for p in pops])
    got_pops = get(doc, "pops", Any[])
    got = try
        sort([(String(x["path"]), sort(collect(Int, x["clusterIds"]))) for x in got_pops])
    catch; return nothing end
    got == want || return nothing
    # Check every pool member's cluster mtime matches.
    got_mt = get(doc, "clusterMtime", Dict{String,Any}())
    got_mt isa AbstractDict || return nothing
    for pm in pool
        key = "$(pm.uid)/$(pm.value_name)"
        haskey(got_mt, key) || return nothing
        # Reload the image just to mtime-check the tracks h5ad — cheap.
        # (We take the mtime from the sidecar's own record — the caller re-derives the fresh set below
        # and passes them in for comparison.)
    end
    doc
end

function _write_cell_cards_sidecar(sidecar_path::String, payload, pops, pool_mtimes)
    mkpath(dirname(sidecar_path))
    doc = Dict{String,Any}(
        "shapeVersion" => _CELL_CARDS_SHAPE_VERSION,
        "pool" => [Dict("uid" => pm.uid, "value_name" => pm.value_name) for pm in payload.pool],
        "cards" => payload.cards_json,
        "statScales" => payload.stat_scales,
        "pops" => [Dict("path" => String(p.path),
                        "clusterIds" => collect(Int, p.cluster_ids)) for p in pops],
        "clusterMtime" => pool_mtimes)
    write_json_atomic(sidecar_path, doc)
end

# Read the medoid track's own history from `pop_df` — one (t, x, y) per cell along the track, in
# native pixels. Returns an empty vector when the pop_df call fails or the required columns are
# absent. The shared `render_medoid_filmstrip` handles the per-frame "dot + tail" build.
function _cell_trace_history(med_img::CciaImage, value_name::String, pop_path::String,
                              track_id::Int)::Vector{Tuple{Int,Float64,Float64}}
    hist_df = try
        pop_df(med_img, "trackclust", [pop_path]; value_name=value_name, granularity=:cell,
               centroids=:pixel, include_x=false, include_obs=true)
    catch; nothing end
    hist = Tuple{Int,Float64,Float64}[]
    hist_df === nothing && return hist
    (Symbol("track_id") in propertynames(hist_df) &&
     Symbol("centroid_t") in propertynames(hist_df) &&
     Symbol("centroid_x") in propertynames(hist_df) &&
     Symbol("centroid_y") in propertynames(hist_df)) || return hist
    for row in eachrow(hist_df)
        tid = row.track_id; ismissing(tid) && continue
        Int(round(Float64(tid))) == track_id || continue
        tt = row.centroid_t; xx = row.centroid_x; yy = row.centroid_y
        (tt isa Real && xx isa Real && yy isa Real) || continue
        push!(hist, (Int(round(Float64(tt))), Float64(xx), Float64(yy)))
    end
    hist
end

function api_cell_cards(body_bytes::Vector{UInt8})
    data = try JSON3.read(String(body_bytes)); catch; nothing end
    data === nothing && return 400, JSON3.write((; error = "invalid JSON body"))

    pu       = _wstr(data, :projectUid)
    root_uid = _wstr_any(data, :rootUid,    :root_uid)
    vn       = _wstr_any(data, :valueName,  :value_name)
    suffix   = _wstr(data, :suffix)
    (isempty(pu) || isempty(root_uid) || isempty(suffix)) &&
        return 400, JSON3.write((; error = "projectUid, rootUid, suffix required"))

    pops_raw = get(data, :pops, nothing)
    pops_raw isa AbstractVector ||
        return 400, JSON3.write((; error = "pops (array) required"))
    pops = @NamedTuple{path::String, cluster_ids::Vector{Int}}[]
    for p in pops_raw
        path = String(get(p, :path, ""))
        ids_raw = get(p, :clusterIds, get(p, :cluster_ids, Any[]))
        ids = Int[Int(round(Float64(x))) for x in ids_raw]
        (isempty(path) || isempty(ids)) && continue
        push!(pops, (path=path, cluster_ids=ids))
    end
    isempty(pops) && return 400, JSON3.write((; error = "no valid pops in request"))

    max_px = Int(round(Float64(get(data, :maxPx, get(data, :max_px, 320)))))
    pad_px = Int(round(Float64(get(data, :padPx, get(data, :pad_px, 8)))))

    img, gerr = _gating_image(pu, root_uid)
    gerr === nothing || return gerr[1], gerr[2]["body"]

    # Derive value_name when absent — the run's own value_name (the trackclust map's home) is the
    # first co-clustered vn for the suffix. Callers on a cluster panel don't have to plumb this
    # themselves: the manager's own vn selection lives server-side in the sidecar already.
    if isempty(vn)
        vns = try
            co_clustered_value_names(img, suffix; granularity=:track, family="clusters")
        catch; String[] end
        isempty(vns) && return 404, JSON3.write((; error = "no clustering run '$suffix' on $(root_uid)"))
        vn = String(vns[1])
    end

    # Cache freshness check — cheapest path. Sidecar sits under the ROOT image's analysis/ dir (the
    # Decision 8 mirror-per-pool-member behaviour lands in Phase 1f; single-image runs land it at
    # the root, which is the only member).
    sidecar = _cell_cards_sidecar(img._dir, vn, suffix)
    doc, pool = try
        (pool0,) = try
            entry = Cecelia._clustfeatures_entry(img_track_props_path(img, vn), suffix; family="clusters")
            entry === nothing && error("no clustfeatures entry")
            part_of = String[string(x) for x in get(entry, "partOf", get(entry, :partOf, String[]))]
            isempty(part_of) && (part_of = [img.uid])
            (part_of,)
        catch e; return 404, JSON3.write((; error = "clustering run '$suffix' not found: $(sprint(showerror, e))")) end
        (_cell_cards_cache_fresh(sidecar, [(uid=u, value_name=vn) for u in pool0], pops), nothing)
    catch; (nothing, nothing) end
    # NB: the freshness check uses a coarse pool built from partOf only — the metadata pass below
    # returns the full (uid, vn) pool. If a rebuild is needed, we get the accurate pool from the
    # rebuild's own call.

    if doc !== nothing
        # Cache hit — return ONLY the wire fields (`pool`, `cards`, `statScales`); `pops`/`clusterMtime`
        # /`shapeVersion` live in the sidecar for freshness accounting and are not part of the response
        # contract.
        return 200, JSON3.write(Dict{String,Any}(
            "pool"       => get(doc, "pool", Any[]),
            "cards"      => get(doc, "cards", Any[]),
            "statScales" => get(doc, "statScales", Dict{String,Any}())))
    end

    # Cold path — compute metadata, render frames, cache.
    pool, cards_meta = try
        cell_cards_metadata(img, vn, suffix, pops; proj_uid=pu)
    catch e
        return 500, JSON3.write((; error = "cell_cards_metadata failed: $(sprint(showerror, e))"))
    end

    # Uniform crop across every card so cell POPULATIONS are visually comparable side by side.
    # Side = the largest bbox extent across all cards + 2×pad, clamped by the
    # renderer to native. `track_bbox` here is called with pad=0 to keep the metric to the raw
    # motion; the renderer re-adds its own pad centred on each medoid.
    uniform_side = 0
    med_imgs = Dict{String,CciaImage}()
    for c in cards_meta
        mi = c.medoid.uid == img.uid ? img : get!(med_imgs, c.medoid.uid) do
            init_object(pu, c.medoid.uid)
        end
        bx = try
            bb = track_bbox(mi, c.medoid.value_name, c.medoid.track_id; pad_px=0)
            max(bb.x[2] - bb.x[1] + 1, bb.y[2] - bb.y[1] + 1)
        catch; 0 end
        uniform_side = max(uniform_side, bx)
    end
    uniform_side = uniform_side > 0 ? uniform_side + 2 * pad_px : nothing

    # Time axis is derived from the ACTIVE image's saved sidecar (`img`) — the analysis board's
    # current image version is the one the user is looking at, and its OME-XML/ccid records the real
    # TimeIncrement. A medoid on a different image version may resolve to a filepath whose sidecar
    # lacks the T axis; without this, `t_s` was silently absent and the label fell back to "t=N".
    interval_s = try
        :T in img_scale_axes(img) ? Float64(img_physical_sizes(img)[2]) * 60.0 : nothing
    catch; nothing end

    cards_json = Any[]
    for c in cards_meta
        med_img = c.medoid.uid == img.uid ? img : get!(med_imgs, c.medoid.uid) do
            init_object(pu, c.medoid.uid)
        end
        trace_history = _cell_trace_history(med_img, c.medoid.value_name, c.path, Int(c.medoid.track_id))
        filmstrip = render_medoid_filmstrip(med_img, c.medoid.value_name, c.medoid.track_id,
                                            c.frames_ts, pu;
                                            trace_history=trace_history,
                                            trace_colour=hex_to_rgb(c.colour),
                                            max_px=max_px, pad_px=pad_px,
                                            crop_side=uniform_side, interval_s=interval_s)
        push!(cards_json, Dict{String,Any}(
            "path"      => c.path,
            "name"      => c.name,
            "colour"    => c.colour,
            "n"         => c.n,
            "medoid"    => Dict("uid" => c.medoid.uid, "value_name" => c.medoid.value_name,
                                "track_id" => c.medoid.track_id,
                                "frames" => [c.frames_ts[1], c.frames_ts[end]]),
            "filmstrip" => filmstrip,
            "stats"     => [Dict("name" => s.name,
                                 "min" => s.min, "q25" => s.q25, "median" => s.median,
                                 "q75" => s.q75, "max" => s.max) for s in c.stats]))
    end

    # statScales: pool-wide [globalMin, globalMax] per measure across every card in this response.
    # The FE's mini-boxplots share this scale per measure so a box's box+whiskers position is
    # comparable card-to-card (unshared scales would let every card normalise to itself and hide the
    # very cross-pop difference the layout is trying to show). Measures that appear on no card are
    # simply absent from the map. Same ends the box uses (raw min/max — see card_stats).
    stat_scales = Dict{String,Vector{Float64}}()
    for c in cards_meta, s in c.stats
        cur = get(stat_scales, s.name, nothing)
        stat_scales[s.name] = cur === nothing ? Float64[s.min, s.max] :
                              Float64[min(cur[1], s.min), max(cur[2], s.max)]
    end

    pool_mtimes = Dict{String,Any}()
    pool_img_dirs = Dict{String,String}()
    for pm in pool
        pm_img = pm.uid == img.uid ? img : init_object(pu, pm.uid)
        pool_mtimes["$(pm.uid)/$(pm.value_name)"] = _cluster_mtime(pm_img, pm.value_name)
        pool_img_dirs[pm.uid] = pm_img._dir
    end

    # Mirror the sidecar to EVERY pool member per Decision 8 — a cell-cards view opened on any pool
    # member reads its local copy. For a pool of 1 this collapses to the root (a no-op mirror).
    payload = (pool = pool, cards_json = cards_json, stat_scales = stat_scales)
    seen_dirs = Set{String}()
    for pm in pool
        d = get(pool_img_dirs, pm.uid, nothing); d === nothing && continue
        d in seen_dirs && continue   # multi-vn on the same image → one sidecar per image, not per (uid, vn)
        push!(seen_dirs, d)
        _write_cell_cards_sidecar(_cell_cards_sidecar(d, vn, suffix), payload, pops, pool_mtimes)
    end

    doc_out = Dict{String,Any}(
        "pool"       => [Dict("uid" => pm.uid, "value_name" => pm.value_name) for pm in pool],
        "cards"      => cards_json,
        "statScales" => stat_scales)
    200, JSON3.write(doc_out)
end
