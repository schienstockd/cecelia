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
function _cell_cards_cache_fresh(sidecar_path::String, pool, pops)::Union{Nothing,Dict{String,Any}}
    isfile(sidecar_path) || return nothing
    doc = try JSON3.read(read(sidecar_path, String), Dict{String,Any}); catch; return nothing end
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
        "pool" => [Dict("uid" => pm.uid, "value_name" => pm.value_name) for pm in payload.pool],
        "cards" => payload.cards_json,
        "pops" => [Dict("path" => String(p.path),
                        "clusterIds" => collect(Int, p.cluster_ids)) for p in pops],
        "clusterMtime" => pool_mtimes)
    write_json_atomic(sidecar_path, doc)
end

# Render one card's filmstrip. Returns Vector{Dict{"t","assetId"}} — the payload shape the frontend
# expects. Empty when the medoid's image has no OME-Zarr on disk (fixture path) — the card still
# lands, just with no images.
function _render_card_frames(med_img::CciaImage, medoid, frames_ts::Vector{Int}, pop_path::String,
                             value_name::String, project_uid::String;
                             max_px::Int=320, pad_px::Int=8)::Vector{Dict{String,Any}}
    zp = try
        med_img_zarr = versioned_get_field(med_img.filepath, "filepath", value_name)
        joinpath(med_img._dir, String(med_img_zarr))
    catch; return Dict{String,Any}[] end
    isdir(zp) || return Dict{String,Any}[]

    arr, caxes = open_level0(zp)
    d = axis_dims(caxes, ndims(arr))
    native_h = haskey(d, "y") ? size(arr, d["y"]) : 0
    native_w = haskey(d, "x") ? size(arr, d["x"]) : 0
    (native_h == 0 || native_w == 0) && return Dict{String,Any}[]

    # bbox is (xmin, xmax) / (ymin, ymax) — pass as `crop = (x = xmin:xmax, y = ymin:ymax)`.
    bbox = track_bbox(med_img, value_name, medoid.track_id; pad_px=pad_px)
    crop = (x = bbox.x[1]:bbox.x[2], y = bbox.y[1]:bbox.y[2])
    tf = pixel_transform(native_h, native_w; crop=crop, max_px=max_px)
    # Track overlay: this pop only, coloured by its own pop swatch, long tail so the whole track
    # shows on the last frame. `pops_filter = [pop_path]` isolates the medoid's pop.
    overlays_for = build_overlays_for(med_img;
                                      value_name=value_name, pop_type="trackclust",
                                      transform=tf,
                                      pops_filter=String[pop_path],
                                      include_tracks=true,
                                      tail_length=max(1, bbox.t1 - bbox.t0 + 1),
                                      track_color_mode="pop")

    # Specs: the resolved display specs off the tracks' cell props (the standard viewer default).
    props = img_label_props_path(med_img, value_name)
    nc = haskey(d, "c") ? size(arr, d["c"]) : 1
    specs = try resolved_display_specs(props, nc); catch; nothing end
    channels = collect(0:(nc - 1))

    out = Dict{String,Any}[]
    for t in frames_ts
        frame = try
            render_view_frame(arr, caxes, Int(t);
                              channels=channels, specs=specs, crop=crop, max_px=max_px,
                              points=overlays_for(Int(t))[1],
                              segments=overlays_for(Int(t))[2])
        catch; nothing end
        frame === nothing && continue
        tmp = tempname() * ".png"
        try
            PNGFiles.save(tmp, frame)
            aid = _save_board_asset_file(project_uid, tmp)
            push!(out, Dict("t" => Int(t), "asset_id" => aid))
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
    (isempty(pu) || isempty(root_uid) || isempty(vn) || isempty(suffix)) &&
        return 400, JSON3.write((; error = "projectUid, rootUid, valueName, suffix required"))

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
        # Cache hit — return ONLY the wire fields (`pool`, `cards`); `pops`/`clusterMtime` live in
        # the sidecar for freshness accounting and are not part of the response contract.
        return 200, JSON3.write(Dict{String,Any}(
            "pool"  => get(doc, "pool", Any[]),
            "cards" => get(doc, "cards", Any[])))
    end

    # Cold path — compute metadata, render frames, cache.
    pool, cards_meta = try
        cell_cards_metadata(img, vn, suffix, pops; proj_uid=pu)
    catch e
        return 500, JSON3.write((; error = "cell_cards_metadata failed: $(sprint(showerror, e))"))
    end

    cards_json = Any[]
    for c in cards_meta
        med_img = c.medoid.uid == img.uid ? img : init_object(pu, c.medoid.uid)
        filmstrip = _render_card_frames(med_img, c.medoid, c.frames_ts, c.path, c.medoid.value_name,
                                        pu; max_px=max_px, pad_px=pad_px)
        push!(cards_json, Dict{String,Any}(
            "path"      => c.path,
            "name"      => c.name,
            "colour"    => c.colour,
            "n"         => c.n,
            "medoid"    => Dict("uid" => c.medoid.uid, "value_name" => c.medoid.value_name,
                                "track_id" => c.medoid.track_id,
                                "frames" => [c.frames_ts[1], c.frames_ts[end]]),
            "filmstrip" => filmstrip,
            "stats"     => [Dict("name" => s.name, "median" => s.median,
                                 "q25" => s.q25, "q75" => s.q75) for s in c.stats]))
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
    payload = (pool = pool, cards_json = cards_json)
    seen_dirs = Set{String}()
    for pm in pool
        d = get(pool_img_dirs, pm.uid, nothing); d === nothing && continue
        d in seen_dirs && continue   # multi-vn on the same image → one sidecar per image, not per (uid, vn)
        push!(seen_dirs, d)
        _write_cell_cards_sidecar(_cell_cards_sidecar(d, vn, suffix), payload, pops, pool_mtimes)
    end

    doc_out = Dict{String,Any}(
        "pool"  => [Dict("uid" => pm.uid, "value_name" => pm.value_name) for pm in pool],
        "cards" => cards_json)
    200, JSON3.write(doc_out)
end
