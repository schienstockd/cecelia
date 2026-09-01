# ── movie_rail.jl — the record button + batch-movie config, driven by the offline renderer ────
#
# The offline renderer (`record_view_movie`) drives the movie rail (`task:status`, `task:progress`,
# `task:log`, `task:result`, working Cancel). Single-shot record, batch, and keyframe animations all
# route here.
#
# **Whose responsibility.** The overlay/mask CLOSURES come from `_resolve_movie_overlays_mask`
# (`viewer_api.jl`) — the ONE reader shared with the smoke route. Cancel goes through `jobs.jl`
# (`start_job!` / `track_job!` / `job_cancelled`), which `sockets.jl` already dispatches to on
# `task:cancel`. Config storage goes through `register_movie!` (`movies_api.jl`).
#
# **Scope of this file.** Timelapse T-sweeps (single-cell), compare grids (versions × masks), and
# keyframe animations. The compare grid renders each cell as a temp mp4 through `record_view_movie`
# and composes them via `stitch_movies_run.py` (`cecelia.utils.movie_io.stitch_movies`) so captions +
# separators stay identical across surfaces.

# ── Small helpers used by both single + batch offline paths ───────────────────────

# The frame's zarr + task dir + specs, the same three inputs `api_viewer_record_test` reads. Pulled out
# so the batch loop doesn't repeat the boilerplate per image.
function _resolve_frame_for_record(pu::AbstractString, iu::AbstractString, value_name)
    vnn = (value_name === nothing || String(value_name) == "") ? nothing : String(value_name)
    zp, td, err = resolve_image_version(pu, iu, vnn)
    err === nothing || return (nothing, nothing, nothing, nothing, err)
    arr, caxes = open_level0(zp)
    d  = axis_dims(caxes, ndims(arr))
    nc = haskey(d, "c") ? size(arr, d["c"]) : 1
    props = _props_path(td, zp)
    specs = resolved_display_specs(props, nc)
    # Same cold-start fallback the smoke route uses: an image nobody has opened has no viewer props,
    # so contrast is sampled from one plane. Sampled per t would flicker (decision 5).
    specs === nothing && (specs = resolved_display_specs(_sampled_specs(zp, nc)))
    (zp, arr, caxes, specs, nothing)
end

# The T sweep the request asks for. Semantics match `run_single_movie` (`t_end === nothing` = last
# frame; range clamped downstream by `record_view_movie` against the image's own nT).
function _record_ts_range(arr, caxes, t_start::Int, t_end)
    d  = axis_dims(caxes, ndims(arr))
    nT = haskey(d, "t") ? size(arr, d["t"]) : 1
    lo = max(0, Int(t_start))
    hi = t_end === nothing ? nT - 1 : min(nT - 1, Int(t_end))
    hi < lo ? Int[] : collect(lo:hi)
end

# The frontend sends canvas H/W here (a viewer's own dimensions, or a user override), which the
# offline renderer has no viewer for — but max(H, W) is a reasonable `max_px` cap, so a UI that sized
# down for a smaller mp4 still gets a smaller mp4. `0` = no downsample (`render_view_frame`'s own
# default), which is what happens when the UI leaves them blank.
_max_px_from_size(size_x, size_y) =
    max(size_x === nothing ? 0 : Int(size_x), size_y === nothing ? 0 : Int(size_y))

# The size-fields' placeholder is "canvas", but the request only sends numbers when the user typed
# them. When they didn't, the movie used to encode at NATIVE crop resolution — which for a zoomed-in
# view is a tiny mp4, and for a zoomed-out view is a huge one, neither matching what the user was
# watching. Fall back to the viewer's canvas long side so the mp4 caps at what the viewer showed
# (aspect stays native — `max_px` is a stride cap, not an exact resize). Returns 0 for a snapshot
# without a usable canvas, which leaves the previous behaviour intact.
function _max_px_from_view_state(vs::Union{Nothing,AbstractDict})
    vs isa AbstractDict || return 0
    canv = get(vs, "canvas", nothing)
    canv isa AbstractDict || return 0
    cw = get(canv, "width", 0); ch = get(canv, "height", 0)
    (cw isa Real && ch isa Real) || return 0
    max(Int(round(Float64(cw))), Int(round(Float64(ch))))
end

# JSON3 hands back Symbol keys; the movie_config/batch config path handles either — same `_to_str_dict`
# gotcha the rest of the code lives with. Two accessors so callers can be terse.
_cfg_get(cfg, k, default) =
    something(get(cfg, Symbol(k), nothing), get(cfg, String(k), default))
_cfg_bool(cfg, k, default::Bool = false) = Bool(_cfg_get(cfg, k, default))
_cfg_str(cfg, k, default::AbstractString = "") = String(_cfg_get(cfg, k, default))
_cfg_int(cfg, k, default::Int) = (v = _cfg_get(cfg, k, default); v isa Integer ? Int(v) : Int(round(Float64(v))))

# ── Apply the batch/`look` channel picks on top of the props-derived specs ───────
#
# `resolved_display_specs(props, nc)` reads whatever colormap the viewer autosaved for the image;
# a batch config / single-record `look` carries the user's PICKS ({name → colormap}). Without this
# override, a compare grid across versions rendered whatever colours the props file happened to
# carry, ignoring the picker on the batch panel (reported by Dominik).
#
# A channel named in `cfg.channels` gets its picked colormap and is visible; every other channel is
# hidden. lo/hi stay from the props, so contrast is still whatever the viewer measured. `cfg`
# without `channels` → specs unchanged (channels-only movie the recorder would render before this
# override existed).
function _apply_channel_picks(specs, cfg, img, vnn::Union{Nothing,AbstractString})
    (specs === nothing) && return specs
    chans = cfg isa AbstractDict ? get(cfg, :channels, get(cfg, "channels", nothing)) : nothing
    (chans isa AbstractDict && !isempty(chans)) || return specs
    wanted = Dict(String(k) => String(v) for (k, v) in pairs(chans))
    ch_all = channel_names(img; value_name = vnn)
    (ch_all === nothing || length(ch_all) != length(specs)) && return specs
    [begin
        s = specs[i]; ch = ch_all[i]
        if haskey(wanted, ch)
            (; lo = s.lo, hi = s.hi, lut = _as_lut(wanted[ch]), visible = true)
        else
            (; lo = s.lo, hi = s.hi, lut = s.lut,             visible = false)
        end
     end for i in eachindex(specs)]
end

# ── Config → `overlays_raw` translator (batch config + viewer `look`) ─────────────
#
# The record request the frontend sends today does not carry the smoke-route-shaped `overlays: {popType,
# popPaths, includeTracks, …}` block; it carries fields like `showPopulations` / `showTracks` /
# `showGatedTracks` / `popType` / `pointsSize` (see `frontend/src/utils/batchMovie.ts` →
# `BatchMovieRequestConfig`, and `seedConfigFromViewState` for the viewer's `look`). One translator so
# offline record + batch stay consistent — the alternative would be a per-panel frontend refactor,
# per surface, all reading the same set of live-view keys.
#
# `has_mask` tells the translator whether the caller has resolved a mask value_name to draw. `nothing`
# means "no overlays to draw"; the caller drops that straight through to `_resolve_movie_overlays_mask`.
function _overlays_raw_from_config(cfg, has_mask::Bool)
    (cfg isa AbstractDict) || return nothing
    show_pops   = _cfg_bool(cfg, "showPopulations")
    show_tracks = _cfg_bool(cfg, "showTracks")
    show_gated  = _cfg_bool(cfg, "showGatedTracks")
    if !(show_pops || show_tracks || show_gated || has_mask)
        return nothing
    end
    out = Dict{String,Any}(
        "popType"          => _cfg_str(cfg, "popType", "flow"),
        # Explicit gate on the pop-dot build. Presence of the field is what stops
        # `_resolve_movie_overlays_mask` from painting pops for a mask-only record — before this
        # was written, the reader defaulted `showPopulations` to true (for smoke-route back-compat)
        # and any `ov_raw` dict leaked pop dots.
        "showPopulations"  => show_pops,
        "includeTracks"    => show_gated,       # cell-track ribbons alongside the pop points
        "allTracks"        => show_tracks,      # whole-segmentation tracks, ignoring pops
        "tailLength"       => 30,               # legacy default; the batch config doesn't author it
        "trackColorMode"   => "track",
        "pointSizePx"      => _cfg_int(cfg, "pointsSize", 6),
        "segmentWidthPx"   => _cfg_int(cfg, "tailWidth", 2),
    )
    if has_mask
        out["showMask"]        = true
        out["maskContourPx"]   = _cfg_int(cfg, "labelContour", 1)
        # `allCells` = whole-segmentation mask (every id painted). If neither pops nor cell tracks are
        # on, that IS the intended mask; else the mask filters by the same pops the points would draw.
        out["allCells"]        = !(show_pops || show_gated)
    end
    # colourBy / colourOverrides — same knobs the overlay author reads (`_build_overlay_state`).
    # `build_mask_for` picks them up so a labels layer coloured by "clusters" and the pop dots
    # coloured by "clusters" share the same palette. Absent / empty → pop-derived colours.
    cb_raw = _cfg_str(cfg, "colourBy", "")
    isempty(cb_raw) || (out["colourBy"] = cb_raw)
    co_raw = get(cfg, "colourOverrides", nothing)
    co_raw === nothing && (co_raw = get(cfg, :colourOverrides, nothing))
    co_raw isa AbstractDict && (out["colourOverrides"] = co_raw)
    # `popsFilter` — restrict pop-dot + all_cells=false mask to a subset of pop paths. Absent / empty
    # = "no filter" = draw every visible pop of `popType`. `_resolve_movie_overlays_mask` reads this
    # as `popPaths` and threads it into `build_overlays_for` + `build_mask_for` as `pops_filter`.
    pf_raw = get(cfg, "popsFilter", nothing)
    pf_raw === nothing && (pf_raw = get(cfg, :popsFilter, nothing))
    if pf_raw isa AbstractVector && !isempty(pf_raw)
        out["popPaths"] = String[String(p) for p in pf_raw]
    end
    # `popValueName` — which segmentation the pop tree is looked up in. Pop trees are per-segmentation
    # (`gating/{value_name}.json`), so a batch that draws mask `default` while filtering `/qc/CD169-`
    # (a pop authored on `flowTom`) needs the resolver to look up `flowTom`'s tree, not `default`'s.
    # Emitted as `valueName` — the field `_resolve_movie_overlays_mask` already reads and threads into
    # `build_overlays_for(; value_name=...)`. Absent → the resolver falls back to `vnn` (the mask
    # segmentation), matching the pre-picker behaviour.
    pvn = _cfg_str(cfg, "popValueName", "")
    isempty(pvn) || (out["valueName"] = pvn)
    out
end

# ── Single record — the viewer's Record button, timelapse only ────────────────────
#
# Emits the same task:* frames `run_single_movie` did, so the frontend task list, cancel button and
# task console keep working unchanged. Uses `jobs.jl` for cancel (the encoder is a subprocess of
# `record_view_movie` via `run_py`, tracked via `on_process`) AND a between-frame flag (`cancelled = ()
# -> job_cancelled(...)`, checked before each frame). One taskId → one job.
function run_single_offline(task_id::String, project_uid::String, image_uid::String;
                            fps::Int = 15,
                            size_x::Union{Int,Nothing} = nothing,
                            size_y::Union{Int,Nothing} = nothing,
                            suffix::AbstractString = "",
                            title_card = nothing,
                            value_name::AbstractString = "",
                            label_value_name::Union{AbstractString,Nothing} = nothing,
                            label_contour::Int = 1,
                            z_slice::Union{Int,Nothing} = nothing,
                            t_start::Int = 0, t_end::Union{Int,Nothing} = nothing,
                            show_timestamp::Bool = true, show_scale_bar::Bool = true,
                            overlays_raw = nothing,
                            # The viewer's captured `viewState` snapshot
                            # (`frontend/src/utils/viewer/viewState.ts`). Threaded in so the offline
                            # record picks up the viewer's CROP — the visible rectangle the user is
                            # looking at, in native pixels — rather than always recording the whole
                            # image at native aspect. Without this, a viewer zoomed in on a corner
                            # produced a full-image movie with a totally different framing. 3D and
                            # snapshots without a camera + canvas return `nothing`, which falls
                            # through to the previous behaviour.
                            view_state::Union{Nothing,AbstractDict} = nothing,
                            movie_config = nothing)
    fun = "movie:record"
    img, ierr = _gating_image(project_uid, image_uid)
    if ierr !== nothing
        ws_log(nothing, task_id, "[ERROR] image not found")
        ws_status(nothing, task_id, "failed", image_uid; fun = fun, pool = "job")
        return nothing
    end
    frame = _resolve_frame_for_record(project_uid, image_uid, value_name)
    if frame[5] !== nothing
        ws_log(nothing, task_id, "[ERROR] " * String(frame[5]))
        ws_status(nothing, task_id, "failed", image_uid; fun = fun, pool = "job")
        return nothing
    end
    zp, arr, caxes, specs, _ = frame

    ts = _record_ts_range(arr, caxes, t_start, t_end)
    if isempty(ts)
        ws_log(nothing, task_id, "[ERROR] no timepoints in range (image is single-frame or range is empty)")
        ws_status(nothing, task_id, "failed", image_uid; fun = fun, pool = "job")
        return nothing
    end
    d  = axis_dims(caxes, ndims(arr))
    nc = haskey(d, "c") ? size(arr, d["c"]) : 1
    max_px = _max_px_from_size(size_x, size_y)
    # Blank size fields → fall back to the viewer's canvas long side, so the mp4 caps at the size the
    # viewer showed rather than at the native crop's pixel count. See `_max_px_from_view_state`.
    max_px == 0 && (max_px = _max_px_from_view_state(view_state))
    # Viewer crop: pull the visible rectangle out of the snapshot and pass it to the encoder as
    # `crop`. Native H/W come from the store, not from the snapshot's canvas — `crop_from_view_state`
    # clamps against these so a stale snapshot can't index off the edge of the image. `nothing` here
    # falls through to the previous behaviour (whole image, native aspect).
    native_h = haskey(d, "y") ? size(arr, d["y"]) : 0
    native_w = haskey(d, "x") ? size(arr, d["x"]) : 0
    view_crop = (native_h > 0 && native_w > 0) ?
        crop_from_view_state(view_state, Int(native_h), Int(native_w)) : nothing
    # Match the viewer's plane when the request didn't pin one. A 2D browser viewer shows ONE z, so
    # a movie that MIPs the whole stack for lack of an explicit `zSlice` diverges from what the user
    # was looking at when they hit Record. `z_from_view_state` returns `nothing` for 3D and for
    # snapshots without a usable step, leaving the render at its previous all-Z MIP fallback.
    if z_slice === nothing
        derived_z = z_from_view_state(view_state)
        derived_z === nothing || (z_slice = derived_z)
    end

    # Overlays: an explicit `overlays_raw` on the request wins (smoke-route shape). Otherwise, translate
    # the on-screen `look` (banked in `movie_config`) into that shape so the offline record doesn't
    # regress to channels-only. `has_mask` reflects the request's `labelValueNames`.
    look_cfg = movie_config === nothing ? nothing : get(movie_config, "look", nothing)
    has_mask = label_value_name !== nothing
    effective_overlays = if overlays_raw isa AbstractDict
        overlays_raw
    else
        _overlays_raw_from_config(look_cfg, has_mask)
    end
    vnn = isempty(value_name) ? nothing : String(value_name)
    # Apply the batch/`look` channel picks on top of the props-derived specs — same override the
    # compare grid uses (see `_apply_channel_picks`).
    specs = _apply_channel_picks(specs, look_cfg, img, vnn)
    ov = _resolve_movie_overlays_mask(img, nothing, arr, caxes, effective_overlays,
                                       label_value_name === nothing ? vnn : String(label_value_name);
                                       z = z_slice, crop = view_crop, max_px = max_px, tally = false)

    out_path = _movie_named_path(img, image_uid; suffix = _movie_suffix(suffix))
    ws_status(nothing, task_id, "running", image_uid; fun = fun, pool = "job")
    ws_progress(nothing, task_id, 0, length(ts))
    ws_log(nothing, task_id, "Recording $(length(ts)) frame(s) → $(basename(out_path))")
    # Physical calibration for the two encoder-side overlays. `pixel_size_um` = µm per native x pixel
    # (index 3 of `img_physical_sizes` skimage order [z, y, x]); `time_step_min` = minutes per frame.
    # Absent metadata → 1.0 fallback (pixel-space), same as the metadata accessor.
    pxsz, ts_min = img_physical_sizes(img)
    pixel_size_um = (length(pxsz) >= 3 && pxsz[3] > 0) ? pxsz[3] : nothing
    time_step_min = ts_min > 0 ? ts_min : nothing

    start_job!(task_id)
    status = "done"
    frames = 0
    try
        # `Cecelia.run_py` inside `record_view_movie` needs a task dir it can write params.json into,
        # so the encoder subprocess has a home the process tree can be reached from. Cancel checks the
        # `_JOBS` flag AND kills the encoder subprocess via `_kill_proc_tree`; one flag, two ways to
        # stop, which is what an mp4 encode being minutes long needs.
        result = record_view_movie(zp, out_path;
                                   ts = ts, fps = fps,
                                   z = z_slice, channels = 0:(nc - 1), specs = specs,
                                   crop = view_crop, max_px = max_px,
                                   title_card = title_card,
                                   overlays_for = ov.overlays_for,
                                   mask_for     = ov.mask_for,
                                   point_size_px    = ov.point_size_px,
                                   segment_width_px = ov.segment_width_px,
                                   mask_contour_px  = ov.mask_contour_px,
                                   show_timestamp = show_timestamp, show_scale_bar = show_scale_bar,
                                   pixel_size_um  = pixel_size_um,
                                   time_step_min  = time_step_min,
                                   on_log      = line -> ws_log(nothing, task_id, line),
                                   on_progress = (n, t) -> ws_progress(nothing, task_id, n, t),
                                   on_process  = p -> track_job!(task_id, p),
                                   cancelled   = () -> job_cancelled(task_id))
        frames = Int(result.frames)
        if result.cancelled
            status = "cancelled"
            ws_log(nothing, task_id, "[CANCELLED] stopped after $frames frame(s) — nothing written")
        else
            # Bank how it was made, keyed by the file just written (Phase 4). A viewer recording is a
            # "look" — the same shape the batch authors, so both edit on the page that owns that kind.
            look  = movie_config === nothing ? nothing : get(movie_config, "look", nothing)
            shown = look isa AbstractDict ?
                _shown_channel_names(img, look, isempty(value_name) ? nothing : String(value_name)) :
                String[]
            register_movie!(project_uid, basename(out_path);
                            produced_by = "viewer", image_uid = image_uid,
                            channels = shown, suffix = suffix,
                            config = movie_config, config_kind = "look")
            ws_log(nothing, task_id, "Recorded $frames frames at $(result.width)x$(result.height) → $(basename(out_path))")
        end
        meta = Dict{String,Any}("path" => out_path, "frames" => frames,
                                "sizeX" => result.width, "sizeY" => result.height,
                                "cancelled" => result.cancelled)
        ws_result(nothing, task_id, image_uid, meta)
    catch e
        status = "failed"
        @warn "offline record failed" exception = e
        ws_log(nothing, task_id, "[ERROR] $(sprint(showerror, e))")
        ws_result(nothing, task_id, image_uid, Dict{String,Any}("path" => out_path,
                                                                "cancelled" => false,
                                                                "error" => sprint(showerror, e)))
    finally
        finish_job!(task_id)
    end
    ws_status(nothing, task_id, status, image_uid; fun = fun, pool = "job")
    nothing
end

# ── Batch record — one authored config → one mp4 per image, timelapse only ────────
#
# Loop shape mirrors `run_batch_movies`: fail fast if the batch is empty, `[i/n]` progress lines, per-
# image cancel via the SAME `job_cancelled` flag (so pressing Cancel stops the image being recorded AND
# ends the loop). `[i/n]` uses image-scoped progress on the rail; the encoder's per-frame progress is
# deliberately dropped here — the batch's bar counts images, which is what `run_batch_movies` did too.
function run_batch_offline(task_id::String, project_uid::String, image_uids::Vector{String},
                           config, file_attrs::Vector{String}, fps::Int;
                           size_x::Union{Int,Nothing} = nothing,
                           size_y::Union{Int,Nothing} = nothing,
                           suffix::AbstractString = "",
                           movie_config = nothing)
    n   = length(image_uids)
    rep = isempty(image_uids) ? "" : first(image_uids)
    if n == 0
        ws_log(nothing, task_id, "[ERROR] no images to record")
        ws_status(nothing, task_id, "failed", ""; fun = "movie:batch", pool = "job")
        return nothing
    end
    ws_status(nothing, task_id, "running", rep; fun = "movie:batch", pool = "job")
    ws_progress(nothing, task_id, 0, n)
    # Read the range/mask/pop config in the same shape the frontend authors, so a saved batch config
    # drives every renderer identically.
    t_start, t_end = _t_range(config)
    by_image       = Bool(get(config, :nameByImage, false))
    show_ts        = Bool(get(config, :showTimestamp, true))
    show_sb        = Bool(get(config, :showScaleBar, true))
    vn_raw         = strip(String(get(config, :valueName, "")))
    value_name     = String(vn_raw)
    lvns_raw       = get(config, :labelValueNames, nothing)
    label_vn = if lvns_raw isa AbstractVector && !isempty(lvns_raw)
        # For the simple (single-cell) batch, the first label VN is what the offline mask overlay uses.
        String(first(lvns_raw))
    else
        nothing
    end
    label_contour  = Int(get(config, :labelContour, 1))
    z_slice        = get(config, :zSlice, nothing) === nothing ? nothing : Int(get(config, :zSlice, 0))
    overlays_raw   = get(config, :overlays, nothing)
    # Compare grid: 2+ versions and/or 2+ masks per image. When true, each image renders through
    # `_render_grid_offline` (same offline stitcher) instead of a single `record_view_movie`.
    versions_cfg   = _config_value_names(config)
    masks_cfg      = _config_compare_segmentations(config)
    is_compare     = length(versions_cfg) > 1 || length(masks_cfg) > 1
    grid_layout    = String(get(config, :compareLayout, "row"))
    grid_share     = _share_contrast(get(config, :compareContrast, ""))

    start_job!(task_id)
    done = 0; errors = String[]
    try
        for (i, uid) in enumerate(image_uids)
            if job_cancelled(task_id)
                ws_log(nothing, task_id, "[CANCELLED] stopped after $done/$n image(s)")
                break
            end
            img, ierr = _gating_image(project_uid, uid)
            if ierr !== nothing
                push!(errors, uid)
                ws_log(nothing, task_id, "[WARN] skip $uid — not a loadable image")
                ws_progress(nothing, task_id, i, n); continue
            end
            try
                # Filename resolved BEFORE the render — the compare grid path also needs `chan_names`
                # for `_shown_channel_names` even though it renders columns from multiple versions;
                # first_column's channels are the same shape all cells share (channel MAP, not values).
                chan_names = _shown_channel_names(img, config,
                                                   isempty(value_name) ? nothing : value_name)
                out_path = _movie_out_path(img, file_attrs, chan_names;
                                            suffix = _movie_suffix(suffix), by_image = by_image)
                ws_log(nothing, task_id, "[$i/$n] $(img.name) → $(basename(out_path))")
                tcard = _title_card_content(img, config)
                cancelled_here = false
                if is_compare
                    # Per-image compare grid — versions across cols, masks down rows.
                    grid = _compare_grid(config)
                    # Same physical calibration the single (non-compare) batch branch reads.
                    px_g, tsm_g = img_physical_sizes(img)
                    pixel_size_um_g = (length(px_g) >= 3 && px_g[3] > 0) ? px_g[3] : nothing
                    time_step_min_g = tsm_g > 0 ? tsm_g : nothing
                    gres = _render_grid_offline(task_id, project_uid, uid, img, grid, out_path;
                                                 fps = fps, size_x = size_x, size_y = size_y,
                                                 title_card = tcard,
                                                 share_contrast = grid_share,
                                                 layout = grid_layout,
                                                 t_start = t_start, t_end = t_end,
                                                 show_timestamp = show_ts, show_scale_bar = show_sb,
                                                 pixel_size_um  = pixel_size_um_g,
                                                 time_step_min  = time_step_min_g)
                    cancelled_here = gres.cancelled
                else
                    frame = _resolve_frame_for_record(project_uid, uid, value_name)
                    if frame[5] !== nothing
                        push!(errors, uid)
                        ws_log(nothing, task_id, "[ERROR] $uid: " * String(frame[5]))
                        ws_progress(nothing, task_id, i, n); continue
                    end
                    zp, arr, caxes, specs, _ = frame
                    ts = _record_ts_range(arr, caxes, t_start, t_end)
                    if isempty(ts)
                        push!(errors, uid)
                        ws_log(nothing, task_id, "[WARN] $uid: no timepoints in range — skipping")
                        ws_progress(nothing, task_id, i, n); continue
                    end
                    d  = axis_dims(caxes, ndims(arr))
                    nc = haskey(d, "c") ? size(arr, d["c"]) : 1
                    max_px = _max_px_from_size(size_x, size_y)
                    vnn = isempty(value_name) ? nothing : value_name
                    # Same overlay-translation flow as the single record: batch config carries
                    # `showPopulations`/`showTracks`/`popType`/… — turn those into the overlays block.
                    has_mask = label_vn !== nothing
                    effective_overlays = overlays_raw isa AbstractDict ?
                        overlays_raw : _overlays_raw_from_config(config, has_mask)
                    ov = _resolve_movie_overlays_mask(img, nothing, arr, caxes, effective_overlays,
                                                       label_vn === nothing ? vnn : label_vn;
                                                       z = z_slice, crop = nothing, max_px = max_px,
                                                       tally = false)
                    # Apply the batch config's channel picks on top of the props-derived specs —
                    # same override the compare grid + single record use.
                    specs = _apply_channel_picks(specs, config, img, vnn)
                    # Per-image encoder progress is deliberately dropped from the rail — the batch's
                    # bar counts IMAGES. Cancel flows via `job_cancelled` regardless.
                    px_i, tsm_i = img_physical_sizes(img)
                    pixel_size_um = (length(px_i) >= 3 && px_i[3] > 0) ? px_i[3] : nothing
                    time_step_min = tsm_i > 0 ? tsm_i : nothing
                    result = record_view_movie(zp, out_path;
                                                ts = ts, fps = fps,
                                                z = z_slice, channels = 0:(nc - 1), specs = specs,
                                                crop = nothing, max_px = max_px,
                                                title_card = tcard,
                                                overlays_for = ov.overlays_for,
                                                mask_for     = ov.mask_for,
                                                point_size_px    = ov.point_size_px,
                                                segment_width_px = ov.segment_width_px,
                                                mask_contour_px  = ov.mask_contour_px,
                                                show_timestamp = show_ts, show_scale_bar = show_sb,
                                                pixel_size_um  = pixel_size_um,
                                                time_step_min  = time_step_min,
                                                on_log      = line -> ws_log(nothing, task_id, line),
                                                on_progress = (_a, _b) -> nothing,
                                                on_process  = p -> track_job!(task_id, p),
                                                cancelled   = () -> job_cancelled(task_id))
                    cancelled_here = result.cancelled
                end
                if cancelled_here
                    ws_log(nothing, task_id, "[$i/$n] cancelled — $(basename(out_path)) not written")
                else
                    done += 1
                    register_movie!(project_uid, basename(out_path);
                                    produced_by = "batch", image_uid = uid,
                                    channels = chan_names, suffix = suffix,
                                    config = movie_config, config_kind = "look")
                    ws_log(nothing, task_id, "[$i/$n] done → $(basename(out_path))")
                end
            catch e
                push!(errors, uid)
                @warn "batch offline: image failed" uid = uid exception = e
                ws_log(nothing, task_id, "[ERROR] $uid: $(sprint(showerror, e))")
            end
            ws_progress(nothing, task_id, i, n)
        end
    finally
        finish_job!(task_id)
    end
    cancelled = job_cancelled(task_id)
    status    = cancelled ? "cancelled" : (isempty(errors) ? "done" : "failed")
    ws_result(nothing, task_id, rep,
        Dict{String,Any}("done" => done, "total" => n, "errors" => errors, "cancelled" => cancelled))
    ws_status(nothing, task_id, status, rep; image_uids = image_uids,
              fun = "movie:batch", pool = "job")
    nothing
end

# ── Compare grid — versions × masks, composed into one file ──────────────────────
#
# Render each cell as a temp mp4, stitch the row's cells side-by-side, stack the row strips. Each
# recording goes through `record_view_movie` (offline renderer) and each stitch through
# `writers/stitch_movies_run.py` (`movie_io.stitch_movies`), so captions + separators are
# pixel-identical across compare grid, single record, and batch.
#
# Cancel: the single `job_cancelled` flag stops the render loop between cells, and each render/stitch
# subprocess registers via `on_process → track_job!` so `cancel_job!` kills whichever is running.

# One stitch pass — a small wrapper around `run_py("writers/stitch_movies_run.py", …)`. Kept out of the
# grid renderer so the fan-out (row stitches then column stack) reads as three ordinary calls.
function _stitch_offline_call(task_id::AbstractString, out_path::AbstractString,
                              sources::AbstractVector{<:AbstractString};
                              labels = nothing, layout::AbstractString = "row",
                              fps::Real = 15, title_card = nothing)
    task_dir = mktempdir()
    params = Dict{String,Any}("outPath" => out_path,
                              "sources" => collect(String, sources),
                              "layout"  => String(layout),
                              "fps"     => Float64(fps))
    labels     === nothing || (params["labels"]    = collect(String, labels))
    title_card === nothing || (params["titleCard"] = title_card)
    Cecelia.run_py("writers/stitch_movies_run.py", params, task_dir;
                   on_log     = line -> ws_log(nothing, task_id, line),
                   on_process = p    -> track_job!(String(task_id), p))
end

# Resolve one cell config (`MovieColumn.config`) into the record args `record_view_movie` needs.
# "Applying" here means BUILDING the specs + overlay/mask closures the recorder consumes for THIS
# cell. `first_specs` supports `share_contrast`: column 1 passes its resolved specs to column 2+, so
# a version comparison reads on ONE ruler.
function _resolve_grid_cell(pu::AbstractString, iu::AbstractString, img, cfg;
                            first_specs = nothing, share_contrast::Bool = true,
                            max_px::Int = 0,
                            view_state::Union{Nothing,AbstractDict} = nothing)
    vn = String(get(cfg, :valueName, ""))
    frame = _resolve_frame_for_record(pu, iu, isempty(vn) ? nothing : vn)
    frame[5] === nothing || throw(ArgumentError(String(frame[5])))
    zp, arr, caxes, specs, _ = frame
    picked_specs = _apply_channel_picks(specs, cfg, img, isempty(vn) ? nothing : vn)
    # If sharing, column 1 dictates the contrast for later columns (D4 of MOVIE_COMPARE_PLAN.md).
    # `first_specs === nothing` means we ARE column 1 or the first cell to be resolved — bank ours.
    # Sharing carries the colour LUT too, so a picked colormap on col 1 doesn't get re-derived per
    # column from each version's props file.
    effective_specs = (share_contrast && first_specs !== nothing) ? first_specs : picked_specs
    lvns_raw = get(cfg, :labelValueNames, nothing)
    label_vn = if lvns_raw isa AbstractVector && !isempty(lvns_raw)
        String(first(lvns_raw))
    else
        nothing
    end
    z_slice = get(cfg, :zSlice, nothing) === nothing ? nothing : Int(get(cfg, :zSlice, 0))
    has_mask = label_vn !== nothing
    overlays_dict = _overlays_raw_from_config(cfg, has_mask)
    # Compare-grid mask outlines default to per-id rainbow. Gray on top of coloured channels was
    # invisible on cpSAM (large blobs, magenta) and looked like undifferentiated dots on flowTom
    # (35k tiny cells collapsed to 2-px rings at 512×512). See docs/todo/MOVIE_COMPARE_PLAN.md and
    # the 2026-08-31 report from Dominik. `build_mask_for` reads "rainbow" as a sentinel and cycles
    # `CECELIA_TRACK_PALETTE` by label id.
    if overlays_dict isa AbstractDict && get(overlays_dict, "allCells", false) === true
        overlays_dict["allCellsColour"] = "rainbow"
    end
    vnn = isempty(vn) ? nothing : vn
    d  = axis_dims(caxes, ndims(arr))
    nc = haskey(d, "c") ? size(arr, d["c"]) : 1
    # Viewer crop — same rule as `run_single_offline`. Computed per cell against THIS cell's arr
    # shape: versions almost always share pixel dims, but a downsampled/derived version could differ,
    # and `crop_from_view_state` clamps to whatever it is handed. 3D snapshots return `nothing`,
    # leaving the cell at whole-image aspect.
    native_h = haskey(d, "y") ? size(arr, d["y"]) : 0
    native_w = haskey(d, "x") ? size(arr, d["x"]) : 0
    view_crop = (native_h > 0 && native_w > 0) ?
        crop_from_view_state(view_state, Int(native_h), Int(native_w)) : nothing
    # Match the viewer's plane when the cell's config didn't pin one (same rule as
    # `run_single_offline`). Every cell of a compare grid shares the viewer's one z-plane view.
    if z_slice === nothing
        derived_z = z_from_view_state(view_state)
        derived_z === nothing || (z_slice = derived_z)
    end
    ov = _resolve_movie_overlays_mask(img, nothing, arr, caxes, overlays_dict,
                                       label_vn === nothing ? vnn : label_vn;
                                       z = z_slice, crop = view_crop, max_px = max_px, tally = false)
    (; zp, arr, caxes, specs = effective_specs, ov, z_slice, nc, view_crop,
       banked_specs = picked_specs)
end

# The compare-grid renderer. `rows` is what `_compare_grid` produced (versions across, masks down; a
# single-cell "grid" is degenerate and unused — this function is only called when `cells > 1`). Emits
# ONE progress bar over cells + composes (a running counter, so a wrap that changes the shape can't
# desync from the arithmetic). Returns a NamedTuple mirroring `record_view_movie`'s so the outer
# rail (single vs batch) reads it uniformly.
function _render_grid_offline(task_id::String, pu::String, iu::String, img,
                              rows::Vector{MovieRow}, out_path::String;
                              fps::Int = 15,
                              size_x::Union{Int,Nothing} = nothing,
                              size_y::Union{Int,Nothing} = nothing,
                              title_card = nothing,
                              share_contrast::Bool = true,
                              layout::AbstractString = "row",
                              t_start::Int = 0, t_end::Union{Int,Nothing} = nothing,
                              # Encoder-side timestamp + scale bar. Baked into EACH cell — every cell
                              # in a compare grid shares the same crop and max_px, so the scale bars
                              # match by construction and the timestamps agree per-frame. Redundant
                              # (identical caption in every cell) but visible; a compose-time overlay
                              # would need a stitcher-side overlays block that doesn't exist yet.
                              show_timestamp::Bool = false, show_scale_bar::Bool = false,
                              pixel_size_um::Union{Nothing,Real} = nothing,
                              time_step_min::Union{Nothing,Real} = nothing,
                              view_state::Union{Nothing,AbstractDict} = nothing)
    rows       = _wrap_grid(rows, String(layout))
    row_layout = layout == "grid" ? "row" : String(layout)
    n_rows     = length(rows)
    cells      = sum(length(r.columns) for r in rows)
    cells > 0 || error("compare grid has no cells")
    max_px     = _max_px_from_size(size_x, size_y)
    # Same "blank → viewer canvas" fallback as the single-record path. See `_max_px_from_view_state`.
    max_px == 0 && (max_px = _max_px_from_view_state(view_state))
    per_pass   = _t_sweep_frames(img, t_start, t_end)
    total_units = cells + (n_rows > 1 ? n_rows : 0) + (n_rows > 1 ? 1 : 0)
    # A one-cell row is its own strip — no compose slot. Mirror `_record_grid!`'s counter.
    unit_size  = per_pass > 0 ? per_pass : 1
    total      = total_units * unit_size

    strips = String[]
    temps  = String[]
    slot   = 0
    done_cells = 0
    banked_specs = nothing         # column 1's specs (shared when `share_contrast`)
    grid_frames  = 0
    grid_w = 0; grid_h = 0
    try
        for (ri, row) in enumerate(rows)
            job_cancelled(task_id) && return (; path = out_path, frames = 0, width = 0, height = 0,
                                                 cancelled = true)
            cell_paths = String[]
            for (ci, col) in enumerate(row.columns)
                job_cancelled(task_id) && return (; path = out_path, frames = 0, width = 0, height = 0,
                                                     cancelled = true)
                cell = _resolve_grid_cell(pu, iu, img, col.config;
                                          first_specs = banked_specs,
                                          share_contrast = share_contrast,
                                          max_px = max_px,
                                          view_state = view_state)
                banked_specs === nothing && (banked_specs = cell.banked_specs)
                ts = _record_ts_range(cell.arr, cell.caxes, t_start, t_end)
                if isempty(ts)
                    error("cell (row $ri col $ci): image has no timepoints in range")
                end
                cell_path = string(out_path, ".r", ri, "c", ci, ".tmp.mp4")
                push!(temps, cell_path); push!(cell_paths, cell_path)
                where = n_rows > 1 ? "$(row.label) · $(col.label)" : col.label
                ws_log(nothing, task_id, "[$(done_cells + 1)/$cells] recording $where")
                # Per-frame progress is delivered as a slot offset — one running counter across the
                # whole render+compose, so a mid-loop wrap can't drift from the total.
                start_slot = slot
                cell_result = record_view_movie(cell.zp, cell_path;
                                                 ts = ts, fps = fps,
                                                 z = cell.z_slice, channels = 0:(cell.nc - 1),
                                                 specs = cell.specs,
                                                 crop = cell.view_crop, max_px = max_px,
                                                 title_card = nothing,           # applied at end
                                                 overlays_for = cell.ov.overlays_for,
                                                 mask_for     = cell.ov.mask_for,
                                                 point_size_px    = cell.ov.point_size_px,
                                                 segment_width_px = cell.ov.segment_width_px,
                                                 mask_contour_px  = cell.ov.mask_contour_px,
                                                 show_timestamp = show_timestamp,
                                                 show_scale_bar = show_scale_bar,
                                                 pixel_size_um  = pixel_size_um,
                                                 time_step_min  = time_step_min,
                                                 on_log      = line -> ws_log(nothing, task_id, line),
                                                 on_progress = (n, t) ->
                                                     ws_progress(nothing, task_id,
                                                                  start_slot * unit_size + n, total),
                                                 on_process  = p -> track_job!(task_id, p),
                                                 cancelled   = () -> job_cancelled(task_id))
                if cell_result.cancelled
                    return (; path = out_path, frames = 0, width = 0, height = 0, cancelled = true)
                end
                grid_frames = max(grid_frames, Int(cell_result.frames))
                slot += 1
                done_cells += 1
            end

            # One-row layout — the compose IS the final file (labels + title_card baked here).
            if n_rows == 1
                ws_log(nothing, task_id, "composing $(length(cell_paths)) columns → $(basename(out_path))")
                ok = _stitch_offline_call(task_id, out_path, cell_paths;
                                          labels = [c.label for c in row.columns],
                                          layout = row_layout, fps = fps, title_card = title_card)
                ok || error("compare grid: row stitch failed")
                slot += 1
                grid_w = 0; grid_h = 0        # sizes are the stitcher's; not read out here
                ws_progress(nothing, task_id, total, total)
                return (; path = out_path, frames = grid_frames, width = grid_w, height = grid_h,
                          cancelled = false)
            end

            # Multi-row: build the row strip. A one-cell row IS the strip — reused directly.
            if length(cell_paths) == 1
                push!(strips, cell_paths[1])
            else
                strip_path = string(out_path, ".row", ri, ".tmp.mp4")
                push!(temps, strip_path)
                ws_log(nothing, task_id, "composing row $ri/$n_rows ($(row.label))")
                ok = _stitch_offline_call(task_id, strip_path, cell_paths;
                                          labels = [c.label for c in row.columns],
                                          layout = "row", fps = fps, title_card = nothing)
                ok || error("compare grid: row $ri stitch failed")
                push!(strips, strip_path)
                slot += 1
            end
        end

        # Multi-row final compose — stack the strips vertically, add title_card to the composed output.
        ws_log(nothing, task_id, "stacking $n_rows rows → $(basename(out_path))")
        row_labels = all(r -> isempty(r.label), rows) ? nothing : String[r.label for r in rows]
        ok = _stitch_offline_call(task_id, out_path, strips;
                                  labels = row_labels, layout = "column",
                                  fps = fps, title_card = title_card)
        ok || error("compare grid: final stack failed")
        ws_progress(nothing, task_id, total, total)
        (; path = out_path, frames = grid_frames, width = grid_w, height = grid_h, cancelled = false)
    finally
        for p in temps
            isfile(p) && (try; rm(p); catch; end)
        end
    end
end

# ── Single record with compare grid ─────────────────────────────────────────────
#
# Entered from `handle_movie_record` when the request's `valueNames` / `labelValueNames` /
# `branchValueNames` add up to more than one cell. Same rail as `run_single_offline`, same jobs.jl
# cancel — the difference is the grid renderer replaces one `record_view_movie` call.
function run_single_grid_offline(task_id::String, project_uid::String, image_uid::String;
                                  fps::Int = 15,
                                  size_x::Union{Int,Nothing} = nothing,
                                  size_y::Union{Int,Nothing} = nothing,
                                  suffix::AbstractString = "",
                                  title_card = nothing,
                                  compare_config,
                                  share_contrast::Bool = true,
                                  layout::AbstractString = "row",
                                  t_start::Int = 0, t_end::Union{Int,Nothing} = nothing,
                                  show_timestamp::Bool = true, show_scale_bar::Bool = true,
                                  # The viewer's captured `viewState` snapshot — same purpose as on
                                  # `run_single_offline`: crop every cell to the visible rectangle
                                  # the user is looking at, not the whole image. Applied per cell
                                  # against the cell version's own arr shape (see
                                  # `_resolve_grid_cell`). 3D snapshots return `nothing`, leaving
                                  # the cell at whole-image aspect.
                                  view_state::Union{Nothing,AbstractDict} = nothing,
                                  movie_config = nothing)
    fun = "movie:record"
    img, ierr = _gating_image(project_uid, image_uid)
    if ierr !== nothing
        ws_log(nothing, task_id, "[ERROR] image not found")
        ws_status(nothing, task_id, "failed", image_uid; fun = fun, pool = "job")
        return nothing
    end
    grid = _compare_grid(compare_config)
    out_path = _movie_named_path(img, image_uid; suffix = _movie_suffix(suffix))
    ws_status(nothing, task_id, "running", image_uid; fun = fun, pool = "job")
    # Physical calibration for the encoder-side timestamp + scale bar (same reader
    # `run_single_offline` uses). Absent metadata → 1.0 fallback = pixel-space bar.
    pxsz, ts_min = img_physical_sizes(img)
    pixel_size_um = (length(pxsz) >= 3 && pxsz[3] > 0) ? pxsz[3] : nothing
    time_step_min = ts_min > 0 ? ts_min : nothing
    start_job!(task_id)
    status = "done"
    frames = 0
    try
        result = _render_grid_offline(task_id, project_uid, image_uid, img, grid, out_path;
                                       fps = fps, size_x = size_x, size_y = size_y,
                                       title_card = title_card,
                                       share_contrast = share_contrast, layout = layout,
                                       t_start = t_start, t_end = t_end,
                                       show_timestamp = show_timestamp,
                                       show_scale_bar = show_scale_bar,
                                       pixel_size_um  = pixel_size_um,
                                       time_step_min  = time_step_min,
                                       view_state = view_state)
        frames = Int(result.frames)
        if result.cancelled
            status = "cancelled"
            ws_log(nothing, task_id, "[CANCELLED] stopped — nothing written")
        else
            look  = movie_config === nothing ? nothing : get(movie_config, "look", nothing)
            shown = look isa AbstractDict ?
                _shown_channel_names(img, look, nothing) : String[]
            register_movie!(project_uid, basename(out_path);
                            produced_by = "viewer", image_uid = image_uid,
                            channels = shown, suffix = suffix,
                            config = movie_config, config_kind = "look")
            ws_log(nothing, task_id, "Recorded compare grid ($frames frame(s)) → $(basename(out_path))")
        end
        ws_result(nothing, task_id, image_uid,
                  Dict{String,Any}("path" => out_path, "frames" => frames,
                                    "cancelled" => result.cancelled))
    catch e
        status = "failed"
        @warn "offline compare grid failed" exception = e
        ws_log(nothing, task_id, "[ERROR] $(sprint(showerror, e))")
        ws_result(nothing, task_id, image_uid,
                  Dict{String,Any}("path" => out_path, "cancelled" => false,
                                    "error" => sprint(showerror, e)))
    finally
        finish_job!(task_id)
    end
    ws_status(nothing, task_id, status, image_uid; fun = fun, pool = "job")
    nothing
end

# ── Keyframe animation offline — the animation page's Record → offline renderer ───
#
# `keyframes` are the animation page's own shape: `[(viewState, steps, …)]`. `record_keyframes_view_movie`
# tweens them into per-frame view states and renders each through `render_view_frame` with the
# viewState-derived args (`viewstate_to_render_args`) — one frame per tween step. Cancel + progress +
# `register_movie!` handling matches `run_single_offline`.
function run_single_keyframes_offline(task_id::String, project_uid::String, image_uid::String;
                                       fps::Int = 15,
                                       size_x::Union{Int,Nothing} = nothing,
                                       size_y::Union{Int,Nothing} = nothing,
                                       suffix::AbstractString = "",
                                       title_card = nothing,
                                       keyframes,
                                       value_name::AbstractString = "",
                                       render_quality::Symbol = :standard,
                                       show_timestamp::Bool = true, show_scale_bar::Bool = true,
                                       # Overlay context, opt-in — nothing → channels-only movie,
                                       # dict → run every state through the overlay author
                                       # (`_overlays_raw_from_config` shape + `valueName` +
                                       # `popType`). See `record_keyframes_view_movie`.
                                       overlays_config::Union{Nothing,AbstractDict} = nothing,
                                       movie_config = nothing)
    fun = "movie:animation"
    img, ierr = _gating_image(project_uid, image_uid)
    if ierr !== nothing
        ws_log(nothing, task_id, "[ERROR] image not found")
        ws_status(nothing, task_id, "failed", image_uid; fun = fun, pool = "job")
        return nothing
    end
    frame = _resolve_frame_for_record(project_uid, image_uid, value_name)
    if frame[5] !== nothing
        ws_log(nothing, task_id, "[ERROR] " * String(frame[5]))
        ws_status(nothing, task_id, "failed", image_uid; fun = fun, pool = "job")
        return nothing
    end
    zp, _arr, _caxes, specs, _ = frame
    chan_names = something(channel_names(img; value_name = isempty(value_name) ? nothing : value_name),
                            String[])
    # `_movie_named_path` names by the IMAGE; animations end with `_animation` so timelapse recordings
    # and animations of one image sort together but never collide.
    out_path = _movie_named_path(img, image_uid;
                                  suffix = _movie_suffix(suffix) * "_animation")
    ws_status(nothing, task_id, "running", image_uid; fun = fun, pool = "job")
    ws_log(nothing, task_id, "Recording animation → $(basename(out_path))")

    # Physical calibration: `z_aniso` for the 3D rotation renderer, `pixel_size_um` +
    # `time_step_min` for the encoder-side scale bar + timestamp overlays.
    pxsz, ts_min = img_physical_sizes(img)           # [sz, sy, sx] µm, ts_min = min/frame
    z_aniso       = (length(pxsz) >= 3 && pxsz[3] > 0) ? pxsz[1] / pxsz[3] : 1.0
    pixel_size_um = (length(pxsz) >= 3 && pxsz[3] > 0) ? pxsz[3] : nothing
    time_step_min = ts_min > 0 ? ts_min : nothing

    start_job!(task_id)
    status = "done"; frames = 0
    try
        result = record_keyframes_view_movie(zp, out_path, keyframes, chan_names;
                                              fps = fps,
                                              default_specs = specs,
                                              canvas_h = size_y, canvas_w = size_x,
                                              z_aniso = z_aniso,
                                              render_quality = render_quality,
                                              show_timestamp = show_timestamp,
                                              show_scale_bar = show_scale_bar,
                                              pixel_size_um  = pixel_size_um,
                                              time_step_min  = time_step_min,
                                              title_card = title_card,
                                              img = img,
                                              overlays_config = overlays_config,
                                              on_log      = line -> ws_log(nothing, task_id, line),
                                              on_progress = (n, t) -> ws_progress(nothing, task_id, n, t),
                                              on_process  = p -> track_job!(task_id, p),
                                              cancelled   = () -> job_cancelled(task_id))
        frames = Int(result.frames)
        if result.cancelled
            status = "cancelled"
            ws_log(nothing, task_id, "[CANCELLED] stopped after $frames frame(s) — nothing written")
        else
            # `channels` deliberately left unset — an animation's channel visibility can change across
            # keyframes, so no single channel list describes the whole movie. `register_movie!` leaves
            # the field alone rather than banking a snapshot of only frame 0.
            register_movie!(project_uid, basename(out_path);
                            produced_by = "animation", image_uid = image_uid,
                            suffix = suffix,
                            config = movie_config, config_kind = "keyframes")
            ws_log(nothing, task_id, "Recorded $frames frames at $(result.width)x$(result.height) → $(basename(out_path))")
        end
        ws_result(nothing, task_id, image_uid,
                  Dict{String,Any}("path" => out_path, "frames" => frames,
                                    "sizeX" => result.width, "sizeY" => result.height,
                                    "cancelled" => result.cancelled))
    catch e
        status = "failed"
        @warn "offline keyframes failed" exception = e
        ws_log(nothing, task_id, "[ERROR] $(sprint(showerror, e))")
        ws_result(nothing, task_id, image_uid,
                  Dict{String,Any}("path" => out_path, "cancelled" => false,
                                    "error" => sprint(showerror, e)))
    finally
        finish_job!(task_id)
    end
    ws_status(nothing, task_id, status, image_uid; fun = fun, pool = "job")
    nothing
end
