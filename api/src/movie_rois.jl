# ── ROI movies — the viewer control panel's Movie section with "What: ROIs" ─────────────────────────
#
# docs/todo/ROI_TRIAGE_PLAN.md Decision 12. One movie per ROI of a "Find ROIs" set, each the ROI's box
# (the crop), its planes (the max over them) and its frames ± a margin, drawn with the viewer's look
# exactly as Record → Match viewer draws the screen; optionally one contact sheet composing them all,
# captioned by rank. The ROIs come from the set on disk (`load_roi_set`), picked by the ids the ROI
# list shows under its current filter — the geometry is the server's, the selection the user's.
#
# The look (frame, display specs, overlays) is resolved ONCE through `_prepare_single_record`, the
# plain record's own preparation, so an ROI movie can't draw differently from a recording of the same
# view. Small boxes are enlarged by a whole factor (`min_px`, `_enlarge_factor`).

# Default long side of an ROI movie, in output pixels: a box is often a few dozen pixels across.
const ROI_MOVIE_MIN_PX = 480

# The least an ROI movie shows around its box, in image pixels: a box is often one object, and an
# event is judged by what surrounds it. The frontend's figure twin is `ROI_CONTEXT_MIN_PX`
# (utils/roiMovie.ts) — keep the two equal.
const ROI_CONTEXT_MIN_PX = 64

"""
    roi_record_window(roi, n_t, n_z, n_y, n_x; pad_frames = 2) -> (; crop, z, ts)

What one ROI's movie shows: a square-ish crop about its box — twice the box, at least
`ROI_CONTEXT_MIN_PX` a side, centred, kept inside the `n_y × n_x` image (inclusive L0 pixels) — its
planes (one `Int`, or a range whose max is drawn) and its frames widened by `pad_frames` each side,
clamped to the image's `n_t` frames and `n_z` planes.
"""
function roi_record_window(roi::AbstractDict, n_t::Integer, n_z::Integer, n_y::Integer, n_x::Integer;
                           pad_frames::Integer = 2)
    b  = roi["box"]
    gi(k) = Int(round(Float64(b[k])))
    z0 = clamp(gi("z0"), 0, max(0, n_z - 1)); z1 = clamp(gi("z1"), z0, max(0, n_z - 1))
    t0 = clamp(Int(roi["t0"]) - pad_frames, 0, max(0, n_t - 1))
    t1 = clamp(Int(roi["t1"]) + pad_frames, t0, max(0, n_t - 1))
    (; crop = (x = _context_span(gi("x0"), gi("x1"), n_x), y = _context_span(gi("y0"), gi("y1"), n_y)),
       z = z0 == z1 ? z0 : z0:z1, ts = t0:t1)
end

# One axis of the context crop: twice the box's extent (at least ROI_CONTEXT_MIN_PX), centred on it,
# shifted — not shrunk — to stay inside 0:n-1 when the image is big enough.
function _context_span(lo::Integer, hi::Integer, n::Integer)::UnitRange{Int}
    side = min(n, max(ROI_CONTEXT_MIN_PX, 2 * (hi - lo + 1)))
    a = round(Int, (lo + hi) / 2 - side / 2)
    a = clamp(a, 0, max(0, n - side))
    a:(a + side - 1)
end

"""
    roi_pick(rois, ids) -> Vector

The set's ROIs with these ids, in the order `ids` gives (the ROI list's order); an id the set no
longer has is skipped. Empty `ids` = the whole set, by rank.
"""
function roi_pick(rois::AbstractVector, ids::AbstractVector)
    isempty(ids) && return sort(collect(rois); by = r -> Int(r["rank"]))
    by_id = Dict(String(r["id"]) => r for r in rois)
    [by_id[String(i)] for i in ids if haskey(by_id, String(i))]
end

"""The movie options' size, as an ROI tile's long side; `nothing` when both are blank."""
_roi_min_px(size_x, size_y) = (v = [x for x in (size_x, size_y) if x isa Integer && x > 0]; isempty(v) ? nothing : maximum(v))

# The config kind an ROI movie is banked under — the Movies page re-records it (`roiRerecordRequest`,
# utils/movieRestore.ts) rather than opening the Batch page, which has no ROIs.
const ROI_CONFIG_KIND = "rois"

"""
    _roi_config(cfg, suffix, ids, pad_frames, sheet) -> MovieRecordConfig

The record's config with THIS movie's ROIs banked on it: one id for an ROI's movie, every recorded one
for the sheet — so re-recording it remakes exactly this file, not the whole selection.
"""
function _roi_config(cfg::Union{Nothing,MovieRecordConfig}, suffix::AbstractString,
                     ids::AbstractVector{<:AbstractString}, pad_frames::Integer, sheet::Bool)
    cfg === nothing && return nothing
    block = Dict{String,Any}("suffix" => String(suffix), "ids" => collect(String, ids),
                             "padFrames" => Int(pad_frames), "sheet" => sheet)
    MovieRecordConfig(; (f => getfield(cfg, f) for f in fieldnames(MovieRecordConfig) if f != :rois)...,
                        rois = block)
end

"""Tiles per row of a contact sheet of `n` movies — the compare grid's near-square wrap (`grid_columns`)."""
roi_sheet_per_row(n::Integer)::Int = grid_columns(n)

"""
    roi_movie_tags(roi_suffix, sheet) -> Vector{String}

The tags an ROI movie gets on the Movies page: `roi`, the ROI set's name (so one scenario's movies
filter together; not for the unnamed default set), and `sheet` on the contact sheet.
"""
roi_movie_tags(roi_suffix::AbstractString, sheet::Bool) =
    String["roi"; (isempty(roi_suffix) || roi_suffix == "default") ? String[] : [String(roi_suffix)];
           sheet ? ["sheet"] : String[]]

"""The caption under an ROI's tile: its rank, and the reviewer's label when it has one."""
roi_caption(roi::AbstractDict, review = nothing)::String = begin
    lbl = review isa AbstractDict ? String(something(get(review, "userLabel", nothing), "")) : ""
    isempty(lbl) ? "#$(roi["rank"])" : "#$(roi["rank"]) $lbl"
end

function run_roi_offline(task_id::String, project_uid::String, image_uid::String;
                         roi_suffix::AbstractString, roi_ids::AbstractVector = String[],
                         pad_frames::Int = 2, sheet::Bool = false,
                         fps::Int = 15, min_px::Int = ROI_MOVIE_MIN_PX,
                         suffix::AbstractString = "", numbered::Bool = false,
                         value_name::AbstractString = "",
                         label_value_name::Union{AbstractString,Nothing} = nothing,
                         label_contour::Int = 1,
                         show_timestamp::Bool = true, show_scale_bar::Bool = true,
                         view_state::Union{Nothing,AbstractDict} = nothing,
                         match_viewer::Bool = true,
                         movie_config::Union{Nothing,MovieRecordConfig} = nothing)
    fun = "movie:record"
    fail(msg) = (ws_log(nothing, task_id, "[ERROR] " * msg);
                 ws_status(nothing, task_id, "failed", image_uid; fun = fun, pool = "job"); nothing)
    img, ierr = _gating_image(project_uid, image_uid)
    ierr === nothing || return fail("image not found")
    set = load_roi_set(img, roi_suffix)
    set === nothing && return fail("no ROI set '$roi_suffix' on this image")
    rois = roi_pick(get(set, "rois", Any[]), roi_ids)
    isempty(rois) && return fail("none of the chosen ROIs are in set '$roi_suffix'")
    reviews = try; load_roi_reviews(img, roi_suffix); catch; Dict{String,Any}(); end
    look_cfg = movie_config === nothing ? nothing : movie_config.look
    prep = _prepare_single_record(task_id, project_uid, image_uid, img;
                                  value_name, label_value_name, label_contour, view_state,
                                  match_viewer, look_cfg, full_stack = true)
    prep isa AbstractString && return fail(prep)
    (; zp, arr, caxes, specs, ov, nc) = prep
    d   = axis_dims(caxes, ndims(arr))
    n_t = haskey(d, "t") ? size(arr, d["t"]) : 1
    n_z = haskey(d, "z") ? size(arr, d["z"]) : 1
    (; native_h, native_w) = prep
    pxsz, ts_min = img_physical_sizes(img)
    pixel_size_um = (length(pxsz) >= 3 && pxsz[3] > 0) ? pxsz[3] : nothing
    time_step_min = ts_min > 0 ? ts_min : nothing
    base = name_suffix(roi_suffix) * _movie_suffix(suffix)     # `_<set>[_<suffix>]`
    n = length(rois)
    total = n + (sheet ? 1 : 0)

    ws_status(nothing, task_id, "running", image_uid; fun = fun, pool = "job")
    ws_progress(nothing, task_id, 0, total)
    start_job!(task_id)
    status = "done"; written = String[]; captions = String[]
    try
        for (i, roi) in enumerate(rois)
            job_cancelled(task_id) && (status = "cancelled"; break)
            w = roi_record_window(roi, n_t, n_z, native_h, native_w; pad_frames)
            out = _numbered_if_taken(_movie_named_path(img, image_uid; suffix = "$(base)_r$(roi["rank"])"), numbered)
            ws_log(nothing, task_id, "[$i/$n] ROI #$(roi["rank"]) frames $(first(w.ts))–$(last(w.ts)) → $(basename(out))")
            r = record_view_movie(zp, out; ts = w.ts, fps = fps, z = w.z, channels = 0:(nc - 1),
                                  specs = specs, crop = w.crop, min_px = min_px,
                                  overlays3d_for = ov.overlays3d_for, mask = ov.mask, style = ov.style,
                                  show_timestamp = show_timestamp, show_scale_bar = show_scale_bar,
                                  pixel_size_um, time_step_min,
                                  on_log = line -> ws_log(nothing, task_id, line),
                                  on_process = p -> track_job!(task_id, p),
                                  cancelled = () -> job_cancelled(task_id))
            r.cancelled && (status = "cancelled"; break)
            push!(written, out)
            push!(captions, roi_caption(roi, get(reviews, String(roi["id"]), nothing)))
            register_movie!(project_uid, basename(out); produced_by = "viewer", image_uid = image_uid,
                            suffix = suffix, config_kind = ROI_CONFIG_KIND, auto_tags = roi_movie_tags(roi_suffix, false),
                            config = _roi_config(movie_config, roi_suffix, [String(roi["id"])], pad_frames, false))
            ws_progress(nothing, task_id, i, total)
        end
        if status == "done" && sheet && length(written) > 1
            sheet_path = _numbered_if_taken(_movie_named_path(img, image_uid; suffix = "$(base)_sheet"), numbered)
            _roi_sheet!(task_id, sheet_path, written, captions; fps)
            register_movie!(project_uid, basename(sheet_path); produced_by = "viewer", image_uid = image_uid,
                            suffix = suffix, config_kind = ROI_CONFIG_KIND, auto_tags = roi_movie_tags(roi_suffix, true),
                            config = _roi_config(movie_config, roi_suffix,
                                                 [String(r["id"]) for r in rois[1:length(written)]], pad_frames, true))
            push!(written, sheet_path)
        end
        status == "cancelled" && ws_log(nothing, task_id, "[CANCELLED] stopped after $(length(written)) movie(s)")
        ws_result(nothing, task_id, image_uid, Dict{String,Any}(
            "paths" => written, "path" => isempty(written) ? "" : last(written),
            "cancelled" => status == "cancelled"))
    catch e
        status = "failed"
        @warn "ROI record failed" exception = e
        ws_log(nothing, task_id, "[ERROR] $(sprint(showerror, e))")
    finally
        finish_job!(task_id)
    end
    ws_status(nothing, task_id, status, image_uid; fun = fun, pool = "job")
    nothing
end

# The contact sheet: rows of `roi_sheet_per_row` tiles stitched across, then the rows stacked — the
# compare grid's two-pass compose (`_stitch_offline_call`), captions under each tile. Unequal lengths
# hold their last frame; unequal tile sizes centre on the largest (`movie_io.stitch_movies`).
function _roi_sheet!(task_id::AbstractString, out_path::AbstractString,
                     paths::AbstractVector{<:AbstractString}, captions::AbstractVector{<:AbstractString};
                     fps::Int = 15)
    per = roi_sheet_per_row(length(paths))
    rows = collect(Iterators.partition(eachindex(paths), per))
    ws_log(nothing, task_id, "composing a $(length(rows))-row sheet → $(basename(out_path))")
    length(rows) == 1 && return _stitch_offline_call(task_id, out_path, paths; labels = captions,
                                                     layout = "row", fps = fps) || error("ROI sheet: stitch failed")
    strips = String[]
    try
        for (k, idx) in enumerate(rows)
            strip = string(out_path, ".row", k, ".tmp.mp4")
            push!(strips, strip)
            _stitch_offline_call(task_id, strip, paths[idx]; labels = captions[idx], layout = "row", fps = fps) ||
                error("ROI sheet: row $k stitch failed")
        end
        _stitch_offline_call(task_id, out_path, strips; layout = "column", fps = fps) ||
            error("ROI sheet: stacking the rows failed")
    finally
        for s in strips; isfile(s) && (try; rm(s); catch; end); end
    end
    true
end
