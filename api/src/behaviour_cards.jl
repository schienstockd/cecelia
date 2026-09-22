# ── behaviour_cards.jl — shared filmstrip renderer for cell / motif / hmm cards ─────────────
#
# One helper (`render_medoid_filmstrip`) that every card family calls after it has resolved its own
# medoid `(uid, value_name, track_id)` triple and the medoid's trace history. Owns everything that
# is family-agnostic: crop from the medoid track's bbox, pixel transform, snap-to-nearest tracked
# t, image specs from the saved viewer sidecar, per-frame render via `render_view_frame`, PNG-save
# each frame as a board-asset.
#
# **What each family supplies:**
#   - the medoid `(uid, value_name, track_id)` and its image (`med_img::CciaImage`)
#   - the trace history `Vector{Tuple{Int,Float64,Float64}}` in native pixels — one `(t, x, y)` per
#     cell along the medoid's own path (cellCards = full track; motifCards = 8-frame window;
#     hmmCards = state-run). The helper does the per-frame "dot at t + tail up to t" build.
#   - a single trace colour (the pop / motif class / hmm state colour, hex-decoded to RGB).
#   - `frames_ts` — which timepoints to render (before snap).
#
# **What the helper decides:**
#   - crop rectangle (bbox + pad, or centred + `crop_side` for card-sheet parity).
#   - `pixel_transform` for that crop → the closure that projects `(x, y)` from native pixels into
#     the drawn frame.
#   - snap every requested t to the nearest tracked t so the dot always lands on a rendered image.
#   - `resolved_display_specs` from the medoid image's saved viewer sidecar (channels/LUT/contrast).
#
# The family-specific overlay resolution (which cells belong to this card, which colour) is EXPLICITLY
# not this file's job — that resolution mirrors `overlay_author.jl` for movies but on a per-card scale.
# Keeping it caller-side lets a new family (region, spatial-neighbourhood, …) land as a small helper
# in the callers' file plus a `render_medoid_filmstrip` call, not a new dispatch arm here.
#
# Board-asset saving is via `_save_board_asset_file` (existing convention shared with the movie rail).
# Sidecar caching is family-local and lives in each caller (naming per Decision 8: `{family}__{...}`).
#
# See docs/todo/BEHAVIOUR_CARDS_PLAN.md → Decision 5.

using PNGFiles
using ColorTypes: RGB
using FixedPointNumbers: N0f8

"""
    render_medoid_filmstrip(med_img, value_name, track_id, frames_ts, project_uid;
                            trace_history, trace_colour,
                            max_px=320, pad_px=8, crop_side=nothing, interval_s=nothing)
        -> Vector{Dict{String,Any}}

Render one card's filmstrip. Returns a list of `Dict("t", "asset_id", ["t_s"])` — the payload shape
the frontend `CardFrame` type expects. Returns an empty vector when the medoid's image has no
OME-Zarr on disk (fixture path) — the card still lands, just with no images.

Arguments the caller resolves:
- `med_img::CciaImage` — the medoid's image (may differ from the board's active image for a
   multi-image pool; caller usually `init_object`s it once and reuses).
- `value_name` — the segmentation value_name inside `med_img` (`track_bbox` looks the medoid track up
   through this).
- `track_id::Integer` — the medoid's track id.
- `frames_ts::AbstractVector{<:Integer}` — the timepoints the caller wants rendered (typically
   sampled evenly across the medoid's frame span; can be as few as one or as many as the span).
- `project_uid::String` — for board-asset save under this project.

Keyword arguments the caller resolves:
- `trace_history` — `Vector{Tuple{Int,Real,Real}}` of `(t, x_native, y_native)` for the cells that
   belong to this card's TRACE. The helper draws a dot at t on each rendered frame and a tail of
   past segments (arrival t ≤ frame t) in `trace_colour`. Empty history → no trace overlay.
- `trace_colour::RGB{N0f8}` — the trace and dot colour, family-resolved (pop colour for cellCards,
   motif class palette entry for motifCards, hmm state palette entry for hmmCards).
- `max_px::Int=320` — largest drawn-frame extent (`render_view_frame` stride target).
- `pad_px::Int=8` — halo around the medoid's bbox when `crop_side === nothing`.
- `crop_side::Union{Nothing,Int}=nothing` — when set, EVERY card in this render batch uses the same
   physical pixel side, centred on each card's own medoid, so a card sheet is visually comparable.
   When `nothing`, crop = bbox + `pad_px` (single-card path).
- `interval_s::Union{Nothing,Float64}=nothing` — TimeIncrement seconds for the ACTIVE image on the
   board. When set, each frame carries `t_s = t * interval_s` so the frontend can label real time
   (mm:ss / N s) instead of just frame index.
"""
function render_medoid_filmstrip(med_img::CciaImage, value_name::AbstractString,
                                  track_id::Integer, frames_ts::AbstractVector{<:Integer},
                                  project_uid::String;
                                  trace_history::AbstractVector = Tuple{Int,Float64,Float64}[],
                                  trace_colour::RGB{N0f8},
                                  max_px::Int=320, pad_px::Int=8,
                                  crop_side::Union{Nothing,Int}=nothing,
                                  bbox_override::Union{Nothing,NamedTuple}=nothing,
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
    # populations are visually comparable side-by-side. Without `crop_side` the crop is just
    # bbox + pad (interactive path — same physical scale within a single card).
    # `bbox_override` lets the caller pass a bbox that isn't the full track's extent — motifCards
    # needs this because a card visualises an INSTANCE (an 8-frame subtrack), not the track's
    # lifetime. Without the override, uniform sizing would zoom every card out to fit the largest
    # track's meander, so a short-lived motif ends up as a few pixels of trace inside a mostly-empty
    # frame. `bbox_override` shape mirrors `track_bbox`'s: `(x = (lo, hi), y = (lo, hi))` in native
    # pixels.
    bbox = bbox_override !== nothing ? bbox_override :
           track_bbox(med_img, value_name, track_id; pad_px=pad_px)
    if crop_side !== nothing
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

    # Materialise the trace so we can iterate it multiple times and read the tracked t's.
    hist = Tuple{Int,Float64,Float64}[]
    for row in trace_history
        tt, xx, yy = row
        push!(hist, (Int(tt), Float64(xx), Float64(yy)))
    end
    sort!(hist; by = first)

    project = (x, y) -> _apply(tf, x, y)
    build_points_and_segments = function(t::Int)
        pts_x = Int[]; pts_y = Int[]; pts_c = RGB{N0f8}[]
        segs_x0 = Int[]; segs_y0 = Int[]; segs_x1 = Int[]; segs_y1 = Int[]
        segs_c = RGB{N0f8}[]; segs_a = Float64[]
        # dot: cell at frame t (if the medoid was tracked at exactly this t)
        for (tt, x, y) in hist
            tt == t || continue
            xy = project(x, y); xy === nothing && continue
            push!(pts_x, xy[1]); push!(pts_y, xy[2]); push!(pts_c, trace_colour)
        end
        # tail: every consecutive pair whose ARRIVAL t is ≤ t (past segments only)
        for i in 1:(length(hist) - 1)
            t0, x0, y0 = hist[i]; t1, x1, y1 = hist[i + 1]
            t1 <= t || continue
            xy0 = project(x0, y0); xy1 = project(x1, y1)
            (xy0 === nothing || xy1 === nothing) && continue
            push!(segs_x0, xy0[1]); push!(segs_y0, xy0[2])
            push!(segs_x1, xy1[1]); push!(segs_y1, xy1[2])
            push!(segs_c, trace_colour); push!(segs_a, 1.0)
        end
        pts = isempty(pts_x) ? nothing : (; x = pts_x, y = pts_y, colour = pts_c)
        segs = isempty(segs_x0) ? nothing :
               (; x0 = segs_x0, y0 = segs_y0, x1 = segs_x1, y1 = segs_y1,
                  colour = segs_c, alpha = segs_a)
        (pts, segs)
    end

    # Specs: the SAVED VIEWER STATE for this image version (channels, LUT, contrast) — same JSON
    # the movie renderer and thumbnail route read via `_props_path`. Cold-start (no viewer opened
    # yet) falls back to sampled-contrast defaults, same as the movie rail.
    props = _props_path(med_img._dir, zp)
    nc = haskey(d, "c") ? size(arr, d["c"]) : 1
    specs = try resolved_display_specs(props, nc); catch; nothing end
    specs === nothing && (specs = try resolved_display_specs(_sampled_specs(zp, nc)); catch; nothing end)
    channels = collect(0:(nc - 1))

    # Snap each requested frame to the nearest tracked timepoint so the dot (medoid at t) always
    # LANDS on the rendered image. Without this, a mid-frame chosen from the bbox midpoint could
    # fall in a gap between centroid_t entries — the image would render but no dot would draw, and
    # the still would visually disagree with the last-frame-ended trace.
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
