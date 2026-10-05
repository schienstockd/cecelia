# ── behaviour_cards.jl — shared filmstrip renderer for cell / motif / hmm cards ─────────────
#
# One helper (`render_medoid_filmstrip`) that every card family calls after it has resolved its own
# medoid `(uid, value_name, track_id)` triple and the medoid's trace history. Owns everything that
# is family-agnostic: crop from the medoid track's bbox, snap-to-nearest tracked t, image specs from
# the saved viewer sidecar, the frames as stills on the viewer's shader (`render_view_stills`), each
# saved as a board-asset.
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
#   - crop rectangle (bbox + pad, or centred + `crop_side` for card-sheet parity); the stride that
#     fits it in `max_px` is `pixel_transform`'s, inside `render_view_stills`.
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

# A card's dot and tail: the weight the cards always had (measured against the CPU renderer's 6-px
# dot and 2-px Bresenham tail: same dot, trace within 5%) — a card is a small crop shown enlarged, not
# the viewer's canvas.
const _CARD_TRACE_STYLE = movie_overlay_style(k -> k == "pointSizePx" ? 4 : k == "segmentWidthPx" ? 4 : nothing)

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
- `max_px::Int=320` — largest drawn-frame extent (`pixel_transform` stride target).
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

    # Materialise the trace so we can iterate it multiple times and read the tracked t's.
    hist = Tuple{Int,Float64,Float64}[]
    for row in trace_history
        tt, xx, yy = row
        push!(hist, (Int(tt), Float64(xx), Float64(yy)))
    end
    sort!(hist; by = first)

    # The trace in native pixels, for the shader to project: a dot at t (the medoid tracked at exactly
    # this t) and the tail of every hop that has ARRIVED by t. No z — the still is the whole stack's
    # max, which draws every overlay.
    overlays3d_for = function(t::Int)
        dots = [(x, y) for (tt, x, y) in hist if tt == t]
        hops = [(hist[i][2], hist[i][3], hist[i + 1][2], hist[i + 1][3])
                for i in 1:(length(hist) - 1) if hist[i + 1][1] <= t]
        pts = isempty(dots) ? nothing :
              (; x = first.(dots), y = last.(dots), z = zeros(length(dots)),
                 colour = fill(trace_colour, length(dots)))
        segs = isempty(hops) ? nothing :
               (; x0 = getindex.(hops, 1), y0 = getindex.(hops, 2), z0 = zeros(length(hops)),
                  x1 = getindex.(hops, 3), y1 = getindex.(hops, 4), z1 = zeros(length(hops)),
                  colour = fill(trace_colour, length(hops)))
        (pts, segs)
    end

    # Specs: the SAVED VIEWER STATE for this image version (channels, LUT, contrast) — same JSON
    # the movie renderer and thumbnail route read via `_props_path`. Cold-start (no viewer opened
    # yet) falls back to sampled-contrast defaults, from the stack's max as the stills are — the one
    # rule every stack render takes (`_render_default_specs`). `nothing` if neither can be read.
    props = _props_path(med_img._dir, zp)
    nc = haskey(d, "c") ? size(arr, d["c"]) : 1
    specs = try _render_default_specs(props, zp, nc; max_projection = true); catch; nothing end
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
    isempty(frames_uniq) && return out
    dir = mktempdir()
    try
        paths = [joinpath(dir, "t$(t).png") for t in frames_uniq]
        try
            render_view_stills(zp, paths, frames_uniq; channels = channels, specs = specs, crop = crop,
                               max_px = max_px, overlays3d_for = overlays3d_for, style = _CARD_TRACE_STYLE,
                               task_dir = joinpath(dir, "task"), on_log = _ -> nothing)
        catch e
            @warn "render_medoid_filmstrip: the stills failed" value_name track_id exception = e
            return out
        end
        for (t, path) in zip(frames_uniq, paths)
            isfile(path) || continue
            entry = Dict{String,Any}("t" => Int(t), "asset_id" => _save_board_asset_file(project_uid, path))
            interval_s === nothing || (entry["t_s"] = Float64(t) * interval_s)
            push!(out, entry)
        end
    finally
        rm(dir; recursive = true, force = true)
    end
    out
end

# ── Shared sidecar cache discipline (every family) ────────────────────────────────────────────
#
# A family's sidecar is served only while its `stamp` — everything outside the request that decides
# what the cards show, plus the request's own knobs — matches exactly. Each family builds its stamp
# from its own sources (cluster output / h5ad mtime, class set, colours, seeds, render size) plus
# `cards_specs_mtime` for the saved viewer display. On a miss, `CardMemo` lets every card whose
# render key is unchanged reuse its frames, so a recolour or one reshuffle re-renders one card.

"""
    cards_specs_mtime(img) -> Float64

mtime of the image's saved viewer display file (channels/LUT/contrast) — the file
`render_medoid_filmstrip` reads specs from. When never saved the cards use the SAMPLED defaults, and
the stamp is `-SAMPLED_SPECS_RULE` instead: a change to how those defaults are computed must re-render
the cards, and with no file there is no mtime to say so (a sidecar from before the zero-fill fix in
`percentile_spec` carried 0.0 and would otherwise keep its washed-out crops forever).
"""
const SAMPLED_SPECS_RULE = 2      # bump when `percentile_spec` / `_sampled_specs` change their answer
function cards_specs_mtime(img::CciaImage)::Float64
    zp = img_filepath(img)
    zp === nothing && return 0.0
    p = _props_path(img._dir, zp)
    isfile(p) ? mtime(p) : -Float64(SAMPLED_SPECS_RULE)
end

"""
    parse_card_seeds(data) -> Dict{String,Int}

The request's `seeds: {cardPath: n}` map (motif / HMM cards), non-positive / malformed entries
dropped. `0` = the medoid, so an absent key and a 0 mean the same thing.
"""
function parse_card_seeds(data)::Dict{String,Int}
    raw = get(data, :seeds, nothing)
    out = Dict{String,Int}()
    raw isa AbstractDict || return out
    for (k, v) in raw
        (v isa Real && isfinite(v)) || continue
        n = Int(round(Float64(v))); n > 0 && (out[String(k)] = n)
    end
    out
end

read_cards_sidecar(path::String) =
    isfile(path) ? (try JSON3.read(read(path, String), Dict{String,Any}); catch; nothing end) : nothing

# Round-trip through JSON so number/array types compare like-for-like with the stored stamp.
cards_stamp_fresh(doc, shape_version::Int, stamp) =
    doc !== nothing && get(doc, "shapeVersion", 0) == shape_version &&
    JSON3.read(JSON3.write(stamp), Dict{String,Any}) == get(doc, "stamp", nothing)

"""
    CardMemo(prev_doc, shape_version)

Per-card filmstrip memo over the previous sidecar (any stamp — that's the point). `card_filmstrip!`
returns the previous frames when the card's `rkey` matches, else calls `render()`; `memo.keys` is
what to write back as the sidecar's `renderKeys`. The key must hold everything the pixels depend on
(track, frames, trace, colour, crop, size, `cards_specs_mtime`).
"""
struct CardMemo
    prev_keys::Dict{String,Any}
    prev_strips::Dict{String,Any}
    keys::Dict{String,Any}
end
function CardMemo(prev, shape_version::Int)
    ok = prev !== nothing && get(prev, "shapeVersion", 0) == shape_version
    pk = ok ? Dict{String,Any}(String(k) => v for (k, v) in get(prev, "renderKeys", Dict{String,Any}())) :
              Dict{String,Any}()
    ps = Dict{String,Any}()
    ok && for pc in get(prev, "cards", Any[]); ps[String(pc["path"])] = pc["filmstrip"]; end
    CardMemo(pk, ps, Dict{String,Any}())
end
function card_filmstrip!(render::Function, m::CardMemo, path::String, rkey::String)
    m.keys[path] = rkey
    get(m.prev_keys, path, nothing) == rkey && haskey(m.prev_strips, path) && return m.prev_strips[path]
    render()
end
