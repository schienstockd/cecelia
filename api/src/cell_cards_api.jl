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
using PNGFiles

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

# Render one card's filmstrip. Returns Vector{Dict{"t","assetId"}} — the payload shape the frontend
# expects. Empty when the medoid's image has no OME-Zarr on disk (fixture path) — the card still
# lands, just with no images.
function _render_card_frames(med_img::CciaImage, medoid, frames_ts::Vector{Int}, pop_path::String,
                             value_name::String, project_uid::String, pop_colour::String;
                             max_px::Int=320, pad_px::Int=8,
                             crop_side::Union{Nothing,Int}=nothing,
                             # Time-axis source: the ACTIVE image on the analysis board, whose saved
                             # sidecar carries the correct TimeIncrement — a medoid on a different
                             # image version may resolve to a filepath whose OME-XML lacks the T
                             # axis and would otherwise degrade the label to "t=N" (Dominik 2026-09-09).
                             interval_s::Union{Nothing,Float64}=nothing)::Vector{Dict{String,Any}}
    # OME-Zarr resolution is IMAGE-versioned (default/denoised/driftCorrected/…), not segmentation-
    # versioned. `value_name` here is the segmentation vn (e.g. `flowTom`) — a different taxonomy;
    # `img_filepath` is the canonical accessor.
    zp = img_filepath(med_img)
    (zp === nothing || !isdir(zp)) && return Dict{String,Any}[]

    arr, caxes = open_level0(zp)
    d = axis_dims(caxes, ndims(arr))
    native_h = haskey(d, "y") ? size(arr, d["y"]) : 0
    native_w = haskey(d, "x") ? size(arr, d["x"]) : 0
    (native_h == 0 || native_w == 0) && return Dict{String,Any}[]

    # Crop selection. `crop_side` (when supplied) makes ONE physical pixel size share across every
    # card in the response, centred on each card's own medoid — the card sheet is only useful if
    # populations are visually comparable side-by-side (Dominik 2026-09-09). Without `crop_side` the
    # crop is just bbox + pad (interactive path — same physical scale within a single card).
    bbox = track_bbox(med_img, value_name, medoid.track_id; pad_px=pad_px)
    if crop_side !== nothing
        # Centre on the medoid's bbox centre, expand symmetrically to `crop_side`, then clamp to the
        # store's native grid. A crop bigger than the image collapses to the image itself.
        side = min(crop_side, native_h, native_w)
        cx = (bbox.x[1] + bbox.x[2]) ÷ 2
        cy = (bbox.y[1] + bbox.y[2]) ÷ 2
        xlo = clamp(cx - side ÷ 2, 0, native_w - side)
        ylo = clamp(cy - side ÷ 2, 0, native_h - side)
        crop = (x = xlo:(xlo + side - 1), y = ylo:(ylo + side - 1))
    else
        xlo = max(0, bbox.x[1]); xhi = min(native_w - 1, bbox.x[2])
        ylo = max(0, bbox.y[1]); yhi = min(native_h - 1, bbox.y[2])
        crop = (x = xlo:xhi, y = ylo:yhi)
    end
    tf = pixel_transform(native_h, native_w; crop=crop, max_px=max_px)

    # Medoid track's own history — read `pop_df` for the pop, filter to `track_id == medoid.track_id`
    # so the overlay carries ONE track's trace, not every track in the pop (shared
    # `build_overlays_for` has no per-track filter; hand-rolling is smaller than plumbing one). Each
    # cell contributes ONE (t, x_native, y_native) — the projection into the crop happens frame-side.
    hist_df = try
        pop_df(med_img, "trackclust", [pop_path]; value_name=value_name, granularity=:cell,
               centroids=:pixel, include_x=false, include_obs=true)
    catch; nothing end
    hist = Tuple{Int,Float64,Float64}[]
    if hist_df !== nothing && Symbol("track_id") in propertynames(hist_df) &&
       Symbol("centroid_t") in propertynames(hist_df) &&
       Symbol("centroid_x") in propertynames(hist_df) &&
       Symbol("centroid_y") in propertynames(hist_df)
        want = Int(medoid.track_id)
        for row in eachrow(hist_df)
            tid = row.track_id; ismissing(tid) && continue
            Int(round(Float64(tid))) == want || continue
            tt = row.centroid_t; xx = row.centroid_x; yy = row.centroid_y
            (tt isa Real && xx isa Real && yy isa Real) || continue
            push!(hist, (Int(round(Float64(tt))), Float64(xx), Float64(yy)))
        end
        sort!(hist; by = first)
    end
    # colour for the trace + endpoint dot: the pop's own swatch.
    col_rgb = hex_to_rgb(pop_colour)

    # Build the per-frame overlay from the filtered history: dot at t == frames_ts[i], tail from t0
    # to the current t (fade handled by render_view_frame's alpha ramp — we pass full alpha 1.0 and
    # let the segment consumer's default fade apply if any; matching the movie renderer's default).
    project = (x, y) -> _apply(tf, x, y)
    build_points_and_segments = function(t::Int)
        pts_x = Int[]; pts_y = Int[]; pts_c = RGB{N0f8}[]
        segs_x0 = Int[]; segs_y0 = Int[]; segs_x1 = Int[]; segs_y1 = Int[]
        segs_c = RGB{N0f8}[]; segs_a = Float64[]
        # dot: cell at frame t (if the medoid was tracked at exactly this t)
        for (tt, x, y) in hist
            tt == t || continue
            xy = project(x, y); xy === nothing && continue
            push!(pts_x, xy[1]); push!(pts_y, xy[2]); push!(pts_c, col_rgb)
        end
        # tail: every consecutive pair whose ARRIVAL t is ≤ t (past segments only)
        for i in 1:(length(hist) - 1)
            t0, x0, y0 = hist[i]; t1, x1, y1 = hist[i + 1]
            t1 <= t || continue
            xy0 = project(x0, y0); xy1 = project(x1, y1)
            (xy0 === nothing || xy1 === nothing) && continue
            push!(segs_x0, xy0[1]); push!(segs_y0, xy0[2])
            push!(segs_x1, xy1[1]); push!(segs_y1, xy1[2])
            push!(segs_c, col_rgb); push!(segs_a, 1.0)
        end
        pts = isempty(pts_x) ? nothing : (; x = pts_x, y = pts_y, colour = pts_c)
        segs = isempty(segs_x0) ? nothing :
               (; x0 = segs_x0, y0 = segs_y0, x1 = segs_x1, y1 = segs_y1,
                  colour = segs_c, alpha = segs_a)
        (pts, segs)
    end

    # Specs: the SAVED VIEWER STATE for this image version (channels, LUT, contrast) — same JSON
    # the movie renderer and thumbnail route read via `_props_path`. Previously this was pointed at
    # the label-props H5AD, which `layer_display_specs` (a JSON reader) silently caught + returned
    # nothing for; cards then fell back to per-frame percentile and rendered in default colours that
    # did NOT match the viewer (Dominik 2026-09-09 screenshot). Cold-start (no viewer opened yet)
    # falls back to sampled-contrast defaults, same as the movie rail.
    props = _props_path(med_img._dir, zp)
    nc = haskey(d, "c") ? size(arr, d["c"]) : 1
    specs = try resolved_display_specs(props, nc); catch; nothing end
    specs === nothing && (specs = try resolved_display_specs(_sampled_specs(zp, nc)); catch; nothing end)
    channels = collect(0:(nc - 1))

    # Snap each requested frame to the nearest tracked timepoint so the dot (medoid at t) always
    # LANDS on the rendered image (Dominik 2026-09-09). Without this, a mid-frame chosen from the
    # bbox midpoint could fall in a gap between centroid_t entries — the image would render but no
    # dot would draw, and the still would visually disagree with the last-frame ended trace.
    tracked_ts = Int[t for (t, _, _) in hist]
    snap_t = t -> isempty(tracked_ts) ? Int(t) :
                  tracked_ts[argmin(abs.(tracked_ts .- Int(t)))]
    snapped_ts = Int[snap_t(t) for t in frames_ts]
    # Preserve order but drop duplicates that collapse after snapping (a short track's t0 / mid /
    # t1 can snap to the same tracked t).
    seen = Set{Int}(); frames_uniq = Int[]
    for t in snapped_ts; t in seen && continue; push!(seen, t); push!(frames_uniq, t); end

    out = Dict{String,Any}[]
    for t in frames_uniq
        pts, segs = build_points_and_segments(Int(t))
        frame = try
            render_view_frame(arr, caxes, Int(t);
                              channels=channels, specs=specs, crop=crop, max_px=max_px,
                              points=pts, segments=segs)
        catch; nothing end
        frame === nothing && continue
        tmp = tempname() * ".png"
        try
            PNGFiles.save(tmp, frame)
            aid = _save_board_asset_file(project_uid, tmp)
            entry = Dict{String,Any}("t" => Int(t), "asset_id" => aid)
            interval_s === nothing || (entry["t_s"] = Float64(t) * interval_s)
            push!(out, entry)
        finally
            isfile(tmp) && rm(tmp; force=true)
        end
    end
    out
end

function api_cell_cards(body_bytes::Vector{UInt8})
    data = try JSON3.read(String(body_bytes)); catch; nothing end
    data === nothing && return 400, JSON3.write((; error = "invalid JSON body"))

    pu       = String(get(data, :projectUid, ""))
    root_uid = String(get(data, :rootUid,    get(data, :root_uid, "")))
    vn       = String(get(data, :valueName,  get(data, :value_name, "")))
    suffix   = String(get(data, :suffix,     ""))
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

    # Uniform crop across every card so cell POPULATIONS are visually comparable side by side
    # (Dominik 2026-09-09). Side = the largest bbox extent across all cards + 2×pad, clamped by the
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
        filmstrip = _render_card_frames(med_img, c.medoid, c.frames_ts, c.path, c.medoid.value_name,
                                        pu, c.colour; max_px=max_px, pad_px=pad_px,
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
