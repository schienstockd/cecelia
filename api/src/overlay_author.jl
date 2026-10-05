# ── overlay_author.jl — resolve populations/tracks into what a movie draws ────────────────────
#
# Given an image, a segmentation and a `pop_type`, this walks `resolve_pops` + the label store's
# centroids ONCE and hands back a per-t closure of positions (`build_overlays3d_for`) that every
# movie's frames read, and the label → colour map a population mask draws (`mask_id_colours`). The
# movie renderer's shader draws both (`writers/render_animation_run.py`).
#
# The design mirrors the browser's overlay pass: one full resolve at open, then a per-frame slice.
# Every frame allocates ONLY the vectors it draws — no per-frame `label_props` read, no re-scoring of
# pop membership. Precomputing per-t bags of (x, y, colour) is what makes a 181-frame movie a
# reasonable amount of work; the alternative (filter by t inside the closure) would rescan the whole
# table 181 times.
#
# **Coordinate space**: native voxels — the shader projects them with the frame's own camera.
# `pixel_transform` is the region + stride a 2D movie or still renders (`_view_render_params`).
#
# `hex_to_rgb` is here rather than as a general utility for the same reason. Pop colours arrive from
# the gating maps as `#rrggbb`/`#rgb`; parsing them is a five-line helper whose only consumer today
# is this file. When a second consumer appears (a legend renderer for the browser overlay pass, say),
# lift it out then.

using ColorTypes: RGB
using FixedPointNumbers: N0f8
using JSON3

const _HEX_RE = r"^#?([0-9a-fA-F]{6}|[0-9a-fA-F]{3})$"

# Shared source of truth for the house track palette, the three track-colour-mode names, and the
# five-stop heat ramp — same JSON the browser reads in `frontend/src/plots/palettes.json`. Kept as
# ONE file so a colour edit lands in the movie renderer AND the browser look without a
# code change, and a mode the browser knows is a mode this author accepts by construction.
# Path is resolved once at include: api/src/overlay_author.jl → ../../frontend/src/plots/palettes.json.
const _PALETTES_JSON_PATH = normpath(joinpath(@__DIR__, "..", "..", "frontend", "src", "plots",
                                              "palettes.json"))

"""
    hex_to_rgb(hex) -> RGB{N0f8}

Parse `#rrggbb` or `#rgb` (case-insensitive, `#` optional) into an `RGB{N0f8}` for the primitives.
An unparseable colour returns opaque white — a pop with a bad colour is legible rather than
invisible, and the malformed value is the caller's problem to surface.
"""
function hex_to_rgb(hex::AbstractString)::RGB{N0f8}
    m = match(_HEX_RE, strip(String(hex)))
    m === nothing && return RGB{N0f8}(1, 1, 1)
    h = m.captures[1]
    length(h) == 3 && (h = string(h[1], h[1], h[2], h[2], h[3], h[3]))
    r = parse(Int, h[1:2]; base = 16) / 255
    g = parse(Int, h[3:4]; base = 16) / 255
    b = parse(Int, h[5:6]; base = 16) / 255
    RGB{N0f8}(r, g, b)
end

"The inverse of `hex_to_rgb`: `#rrggbb`, lower case."
rgb_to_hex(c::RGB{N0f8})::String =
    "#" * join(string(reinterpret(UInt8, v); base = 16, pad = 2) for v in (red(c), green(c), blue(c)))

# The region and stride a 2D movie or still renders (`_view_render_params`). `crop` is 0-based inclusive
# `(x = x0:x1, y = y0:y1)`; the frame is then downsampled so `max(H, W) ≤ max_px` when max_px > 0.
# `x_lo`/`y_lo` are the native 0-based origin of the cropped-and-drawn frame; `step` the stride;
# `dW`/`dH` the size of the drawn frame — one derivation of these numbers for every 2D render.
struct PixelTransform
    x_lo::Int
    y_lo::Int
    step::Int
    cW::Int          # cropped native extent, before stride
    cH::Int
    dW::Int          # drawn frame size, after stride
    dH::Int
end

"""
    pixel_transform(H, W; crop = nothing, max_px = 0) -> PixelTransform

The region (`crop`, 0-based, clamped to the frame) and integer stride (the smallest that fits
`max(H, W)` in `max_px`; 0 = native) a 2D movie or still renders of a native (H, W) frame.
"""
function pixel_transform(H::Int, W::Int; crop = nothing, max_px::Int = 0)::PixelTransform
    (H > 0 && W > 0) || throw(ArgumentError("pixel_transform: frame size must be positive"))
    x_lo, y_lo = 0, 0
    cH, cW = H, W
    if crop !== nothing
        xr = get(crop, :x, nothing)
        yr = get(crop, :y, nothing)
        if xr !== nothing
            x_lo = clamp(first(xr), 0, W - 1)
            x_hi = clamp(last(xr),  x_lo, W - 1)
            cW = x_hi - x_lo + 1
        end
        if yr !== nothing
            y_lo = clamp(first(yr), 0, H - 1)
            y_hi = clamp(last(yr),  y_lo, H - 1)
            cH = y_hi - y_lo + 1
        end
    end
    step = max_px > 0 ? max(1, cld(max(cH, cW), max_px)) : 1
    dW = length(1:step:cW)
    dH = length(1:step:cH)
    PixelTransform(x_lo, y_lo, step, cW, cH, dW, dH)
end

# ─────────────────────────────────────────────────────────────────────────────────
# The overlay author itself.
# ─────────────────────────────────────────────────────────────────────────────────

const _EMPTY_POINTS = (; x = Int[], y = Int[], colour = RGB{N0f8}[])
const _EMPTY_SEGS   = (; x0 = Int[], y0 = Int[], x1 = Int[], y1 = Int[], colour = RGB{N0f8}[])

# Fallback used ONLY when `palettes.json` is missing (a broken checkout — not a normal state), so
# the movie renderer still draws SOMETHING instead of throwing at `include` time. The parity test in
# `api/test/runtests.jl` asserts these numbers match the JSON, so drift is caught immediately.
const _CECELIA_TRACK_PALETTE_FALLBACK = [
    RGB{N0f8}(0xEB / 255, 0xD4 / 255, 0x41 / 255),
    RGB{N0f8}(0x46 / 255, 0x82 / 255, 0xB4 / 255),
    RGB{N0f8}(0xAA / 255, 0x1F / 255, 0x5E / 255),
    RGB{N0f8}(0xB3 / 255, 0xBC / 255, 0xC2 / 255),
    RGB{N0f8}(0x2F / 255, 0x4F / 255, 0x4F / 255),
    RGB{N0f8}(0x5F / 255, 0xB0 / 255, 0xB7 / 255),
    RGB{N0f8}(0xC7 / 255, 0x7D / 255, 0xA6 / 255),
    RGB{N0f8}(0xD9 / 255, 0x8E / 255, 0x32 / 255),
    RGB{N0f8}(0x3E / 255, 0x6D / 255, 0x8E / 255),
    RGB{N0f8}(0x8E / 255, 0x45 / 255, 0x85 / 255),
    RGB{N0f8}(0x7A / 255, 0x8B / 255, 0x99 / 255),
    RGB{N0f8}(0xC1 / 255, 0x55 / 255, 0x3E / 255),
]

# Same five browser heat-ramp anchors (`BLUE_HEAT_ANCHORS` in `frontend/src/plots/flowColors.ts`),
# used as the fallback when the JSON is missing. Cool → hot: dark blue → cyan → green → orange → red.
const _HEAT_STOPS_FALLBACK = (RGB{N0f8}(0x0B / 255, 0x1A / 255, 0x4D / 255),
                              RGB{N0f8}(0x17 / 255, 0x93 / 255, 0xFF / 255),
                              RGB{N0f8}(0x04 / 255, 0xFA / 255, 0x00 / 255),
                              RGB{N0f8}(0xFF / 255, 0xA8 / 255, 0x05 / 255),
                              RGB{N0f8}(0xFF / 255, 0x38 / 255, 0x56 / 255))

# Parse a single hex string in the JSON to `RGB{N0f8}`. Strict — an unparseable colour is a JSON
# authoring error and should fail load, not fall back to white.
function _hex_to_rgb_strict(hex::AbstractString)::RGB{N0f8}
    m = match(_HEX_RE, strip(String(hex)))
    m === nothing && error("palettes.json: unparseable hex colour '$hex'")
    h = m.captures[1]
    length(h) == 3 && (h = string(h[1], h[1], h[2], h[2], h[3], h[3]))
    r = parse(Int, h[1:2]; base = 16) / 255
    g = parse(Int, h[3:4]; base = 16) / 255
    b = parse(Int, h[5:6]; base = 16) / 255
    RGB{N0f8}(r, g, b)
end

# Load palettes.json ONCE at include time. Missing file → warn and fall back to the frozen literals
# above (broken checkout). Malformed content → let the error propagate; JSON drift is a bug, not a
# recovery path.
function _load_palettes()
    if !isfile(_PALETTES_JSON_PATH)
        @warn "palettes.json missing — falling back to frozen literals. Restore \
               frontend/src/plots/palettes.json to keep browser + Julia in sync." path = _PALETTES_JSON_PATH
        return (palette = collect(_CECELIA_TRACK_PALETTE_FALLBACK),
                modes    = ["track", "speed", "solid", "pop"],
                heat     = collect(_HEAT_STOPS_FALLBACK))
    end
    doc = JSON3.read(read(_PALETTES_JSON_PATH, String))
    pal = [_hex_to_rgb_strict(String(h)) for h in doc.palettes.cecelia]
    modes = [String(m) for m in doc.trackColorModes]
    heat  = [_hex_to_rgb_strict(String(h)) for h in doc.heatRamp]
    (palette = pal, modes = modes, heat = heat)
end

const _PALETTES_DATA = _load_palettes()

# The house 12-colour palette from `PALETTES.cecelia` in the shared JSON. Same list as the browser
# look, so a movie's tracks share colours with a look's tracks by construction.
const CECELIA_TRACK_PALETTE = _PALETTES_DATA.palette

# The neutral grey for "every cell" / "every track" with no colour picked — the browser's
# `ALL_TRACKS_GREY` / `TRACK_SOURCE_GREY`.
const OVERLAY_GREY = "#9ca3af"

# The track-colour-mode names accepted by `build_overlays3d_for(track_color_mode = ...)`. Read
# from the JSON so a mode the browser knows is a mode this author accepts — no silent fallback to
# `"track"` on a new mode name.
const TRACK_COLOR_MODES = _PALETTES_DATA.modes

# Heat ramp (cool → hot), used by `track_color_mode = "speed"`. Same anchors as the browser's
# `BLUE_HEAT_ANCHORS`; interpolation is done at draw time in `_heat_ramp` below.
_heat_stops() = _PALETTES_DATA.heat
function _heat_ramp(u::Real)::RGB{N0f8}
    stops = _heat_stops()
    n = length(stops)
    u = clamp(Float64(u), 0.0, 1.0)
    j = u * (n - 1)
    i = clamp(floor(Int, j) + 1, 1, n - 1)
    f = j - (i - 1)
    a = stops[i]; b = stops[i + 1]
    RGB{N0f8}((1 - f) * Float64(a.r) + f * Float64(b.r),
              (1 - f) * Float64(a.g) + f * Float64(b.g),
              (1 - f) * Float64(a.b) + f * Float64(b.b))
end

# ─────────────────────────────────────────────────────────────────────────────────
# Shared overlay state — the collection every author consumes
# ─────────────────────────────────────────────────────────────────────────────────
#
# One walk of the image → per-t bags of native-voxel (x, y, z, colour), which
# `build_overlays3d_for` hands every movie frame, 2D and 3D. Pop resolution,
# `track_color_mode` semantics, tail-length windowing and colour handling all live
# in ONE place.

struct OverlayState
    # Per-t bags in NATIVE VOXEL coords. `x`, `y`, `z` are Float64 (a centroid is
    # sub-voxel), the colour is the pop's / track-mode's resolved colour.
    pts_by_t    :: Dict{Int,NamedTuple{(:x, :y, :z, :colour),
                        Tuple{Vector{Float64},Vector{Float64},
                              Vector{Float64},Vector{RGB{N0f8}}}}}
    # Segments bucketed by arrival timepoint `t1` — the render loop slices this by
    # `[t + 2 - tail_length, t + 1]` to pick a frame's visible tail window.
    segs_by_end :: Dict{Int,NamedTuple{(:x0, :y0, :z0, :x1, :y1, :z1, :colour),
                        Tuple{Vector{Float64},Vector{Float64},Vector{Float64},
                              Vector{Float64},Vector{Float64},Vector{Float64},
                              Vector{RGB{N0f8}}}}}
    has_t         :: Bool
    tail_length   :: Int
    tracks_active :: Bool
end

# Iterate over the segments a frame `t` should show. Returns `(pts_raw, segs_raw)`
# in NATIVE VOXELS — the caller projects. `segs_raw.t1` carries the arrival
# timepoint per segment so a projector can compute a per-segment tail fade
# (alpha ∝ (t + 1 - t1) / tail_length) that matches the browser overlay pass.
function _state_at(state::OverlayState, t::Int)
    pts = get(state.pts_by_t, state.has_t ? t : 0, nothing)
    segs_raw = nothing
    if state.tracks_active && !isempty(state.segs_by_end)
        hi = t + 1
        lo = hi - state.tail_length + 1
        xs0 = Float64[]; ys0 = Float64[]; zs0 = Float64[]
        xs1 = Float64[]; ys1 = Float64[]; zs1 = Float64[]
        cs  = RGB{N0f8}[]; t1_vec = Int[]
        for e in lo:hi
            bag = get(state.segs_by_end, e, nothing)
            bag === nothing && continue
            for k in eachindex(bag.x0)
                push!(xs0, bag.x0[k]); push!(ys0, bag.y0[k]); push!(zs0, bag.z0[k])
                push!(xs1, bag.x1[k]); push!(ys1, bag.y1[k]); push!(zs1, bag.z1[k])
                push!(cs,  bag.colour[k]); push!(t1_vec, e)
            end
        end
        isempty(xs0) || (segs_raw = (; x0 = xs0, y0 = ys0, z0 = zs0,
                                       x1 = xs1, y1 = ys1, z1 = zs1,
                                       colour = cs, t1 = t1_vec))
    end
    (pts, segs_raw)
end

# ── Colour-by / colour-overrides — per-vertex colouring by an obs column ──────────
#
# Viewer colours points / track ribbons by a chosen obs column: categorical values
# (String, Bool, Integer) go through `colour_by_palette` (Okabe-Ito by sorted
# position, but a user pop that filters for a value on that column donates its
# colour); continuous columns (Float) go through a viridis-ish heat ramp
# normalised over the frame's df range. `colour_overrides` is a
# `{value_string → hex}` map that wins per-value.
#
# ONE resolver builds a per-row closure so the collection loop just does
# `_push_point!(t, xyz, cb_resolve(default_col, i))` at every push site — three
# extra characters per site, and colourBy lights up for populations AND tracks in
# BOTH 2D and 3D atomically.
#
# `track_color_mode` interaction — when `colour_by` is set, tracks force to
# `"pop"` (the arriving cell's colour, which the colour_by resolver already
# baked into `col`). Viewer's `color_by` overrides its categorical/speed
# palettes the same way; matching that keeps the browser view and the movie
# in the same colours. NOTE the target is "pop", not "solid": after the two
# modes diverged, "solid" paints a uniform palette[0] and would discard the
# colour_by result.

_prep_overrides(colour_overrides) = colour_overrides === nothing ? nothing :
    Dict{String,RGB{N0f8}}(String(k) => hex_to_rgb(String(v)) for (k, v) in colour_overrides)

# Given a df that has the `colour_by` column present, return a per-row resolver
# `(default_col::RGB, i::Int) -> RGB`. `nothing` means colourBy is disabled OR the
# column is absent — the caller falls back to the pop's own colour. `pop_map`
# supplies the user-pop colour donation for categorical values (`nothing` for the
# `all_tracks` path — no pop map means Okabe-Ito by sorted position).
function _cb_prepare(df, cb_col::Union{Nothing,String},
                     cb_overrides::Union{Nothing,Dict{String,RGB{N0f8}}},
                     pop_map)
    cb_col === nothing && return nothing
    sym = Symbol(cb_col)
    sym in propertynames(df) || return nothing
    col = df[!, sym]
    # Categorical vs continuous — decided by column dtype. `AbstractFloat` → continuous
    # (viridis-ish heat ramp), everything else → categorical. Integer columns (cluster
    # ids, HMM states) stay categorical, which matches napari.
    is_continuous = eltype(col) <: AbstractFloat
    if is_continuous
        vals = Float64[Float64(v) for v in col if v isa Real && isfinite(Float64(v))]
        (lo, hi) = isempty(vals) ? (0.0, 1.0) : (minimum(vals), maximum(vals))
        span = hi > lo ? (hi - lo) : 1.0
        return (default, i) -> begin
            v = col[i]
            (v isa Real && isfinite(Float64(v))) || return default
            if cb_overrides !== nothing
                # Try `string(v)` first, then `string(Int(v))` when v is an integer-valued Real.
                # Frontend override maps come from user-typed values in the settings pane; a user
                # types `"0"` for a category the AnnData column stores as `0.0`, so match both.
                k = string(v)
                haskey(cb_overrides, k) && return cb_overrides[k]
                if v isa Real && isfinite(Float64(v)) && isinteger(Float64(v))
                    ki = string(Int(v))
                    haskey(cb_overrides, ki) && return cb_overrides[ki]
                end
            end
            _heat_ramp(clamp((Float64(v) - lo) / span, 0.0, 1.0))
        end
    else
        uniq = unique(collect(skipmissing(col)))
        hexes = pop_map === nothing ?
            Dict{Any,String}(v => OKABE_ITO[mod1(k, length(OKABE_ITO))]
                              for (k, v) in enumerate(sort(uniq; by = string))) :
            colour_by_palette(pop_map, cb_col, uniq)
        palette = Dict{Any,RGB{N0f8}}(k => hex_to_rgb(String(v)) for (k, v) in hexes)
        return (default, i) -> begin
            v = col[i]
            if cb_overrides !== nothing
                # Try `string(v)` first, then `string(Int(v))` when v is an integer-valued Real.
                # Frontend override maps come from user-typed values in the settings pane; a user
                # types `"0"` for a category the AnnData column stores as `0.0`, so match both.
                k = string(v)
                haskey(cb_overrides, k) && return cb_overrides[k]
                if v isa Real && isfinite(Float64(v)) && isinteger(Float64(v))
                    ki = string(Int(v))
                    haskey(cb_overrides, ki) && return cb_overrides[ki]
                end
            end
            haskey(palette, v) && return palette[v]
            default
        end
    end
end

# Native-voxel collection. Three branches:
#   * `all_tracks = true`  → whole-segmentation ribbons (every cell with `track_id > 0`)
#   * cell pop_types       → `resolve_pops` + centroid table
#   * track pop_types      → `pop_df(...; granularity=:cell)` (gates live on `track_props`)
# All three funnel into the SAME `_push_point!` / `_push_track!` and the same segment build
# (`track_color_mode` + tail-length). If any of these behaviours are wrong here, every
# downstream author is wrong the same way — which is the drift guarantee.
function _build_overlay_state(img; value_name::AbstractString, pop_type::PopTypeArg,
                              pops_filter::Union{Nothing,AbstractVector{<:AbstractString}} = nothing,
                              include_tracks::Bool = true,
                              tail_length::Int = 30,
                              all_tracks::Bool = false,
                              all_tracks_colour::AbstractString = OVERLAY_GREY,
                              track_color_mode::AbstractString = "track",
                              solid_colour::Union{Nothing,AbstractString} = nothing,
                              colour_by::Union{Nothing,AbstractString} = nothing,
                              colour_overrides::Union{Nothing,AbstractDict} = nothing)
    cb_col = (colour_by === nothing || isempty(String(colour_by))) ? nothing : String(colour_by)
    cb_overrides_rgb = _prep_overrides(colour_overrides)
    # When colourBy is on, tracks paint in the arriving-cell colour (the legacy viewer's `color_by`
    # semantics on the tracks layer). "track"/"speed" would ignore what we resolved. Since the two
    # per-source modes diverged, the target is "pop" (uses `col`, i.e. the colour_by result), not
    # "solid" (uniform palette[0]) — see the docstring above `_prep_overrides`.
    effective_tcm = cb_col === nothing ? String(track_color_mode) : "pop"
    pt = string(pop_type)
    vn = String(value_name)
    is_track_pt = is_track_grained(pt)

    lp   = label_props(img; value_name = vn)
    hasT = !isempty(temporal_columns(lp))
    obs  = col_names(lp; data_type = :obs)
    hasK = "track_id" in obs

    pts_by_t   = Dict{Int,NamedTuple{(:x, :y, :z, :colour),
                        Tuple{Vector{Float64},Vector{Float64},
                              Vector{Float64},Vector{RGB{N0f8}}}}}()
    track_hist = Dict{Tuple{Int,RGB{N0f8}},Vector{Tuple{Int,Float64,Float64,Float64}}}()
    _push_point!(t, xyz, colour) = begin
        bag = get!(pts_by_t, t) do
            (; x = Float64[], y = Float64[], z = Float64[], colour = RGB{N0f8}[])
        end
        push!(bag.x, xyz[1]); push!(bag.y, xyz[2]); push!(bag.z, xyz[3])
        push!(bag.colour, colour)
    end
    _push_track!(kid, colour, t, xyz) = begin
        hist = get!(track_hist, (kid, colour), Tuple{Int,Float64,Float64,Float64}[])
        push!(hist, (t, xyz[1], xyz[2], xyz[3]))
    end
    _z_of(df, i, has_z) = has_z ?
        (df[i, :centroid_z] isa Real ? Float64(df[i, :centroid_z]) : 0.0) : 0.0

    if all_tracks
        if !hasK
            @warn "_build_overlay_state: all_tracks requested but no track_id column" value_name = vn
        else
            view_centroid_cols(lp; order = [:x, :y, :z])
            select_cols(lp, ["track_id"])
            cb_col === nothing || select_cols(lp, [cb_col])
            df = as_df(lp)
            has_z = "centroid_z" in names(df)
            default_col = hex_to_rgb(String(all_tracks_colour))
            # No pop map for the whole-segmentation path — categorical values fall to Okabe-Ito by
            # sorted position (napari does the same for a `color_by` on an unpopulated track store).
            cb_resolve = _cb_prepare(df, cb_col, cb_overrides_rgb, nothing)
            @inbounds for i in 1:size(df, 1)
                px = df[i, :centroid_x]; py = df[i, :centroid_y]
                (px isa Real && py isa Real) || continue
                t = hasT ? df[i, :centroid_t] : 0
                (hasT && !(t isa Real && isfinite(Float64(t)))) && continue
                pz = _z_of(df, i, has_z)
                ti = hasT ? Int(round(Float64(t))) : 0
                colour = cb_resolve === nothing ? default_col : cb_resolve(default_col, i)
                _push_point!(ti, (Float64(px), Float64(py), pz), colour)
                if include_tracks
                    traw = df[i, :track_id]
                    (traw isa Real && isfinite(Float64(traw))) || continue
                    kid = Int(round(Float64(traw)))
                    kid > 0 || continue
                    _push_track!(kid, colour, ti, (Float64(px), Float64(py), pz))
                end
            end
        end
    elseif !is_track_pt
        pops = try
            resolve_pops(img, pt; value_name = vn)
        catch e
            @warn "_build_overlay_state: resolve_pops failed" value_name pop_type exception = e
            NamedTuple[]
        end
        if pops_filter !== nothing
            want = Set(String(p) for p in pops_filter)
            pops = [p for p in pops if String(p.path) in want]
        end
        view_centroid_cols(lp; order = [:x, :y, :z])
        hasK && select_cols(lp, ["track_id"])
        cb_col === nothing || select_cols(lp, [cb_col])
        df = as_df(lp)
        has_z = "centroid_z" in names(df)
        n = size(df, 1)
        row_of = Dict{Int,Int}()
        @inbounds for i in 1:n
            row_of[Int(df[i, :label])] = i
        end
        # Optional pop_map load — categorical colour_by uses user-pop-derived colours where a pop
        # filters for a value on the same column (`colour_by_palette`). Cheap to reload here
        # (JSON parse). `nothing` if the sidecar is missing → Okabe-Ito by sorted position.
        cb_pop_map = cb_col === nothing ? nothing :
            try load_pop_map(img; value_name = vn, pop_type = pt) catch; nothing end
        cb_resolve = _cb_prepare(df, cb_col, cb_overrides_rgb, cb_pop_map)
        for p in pops
            Bool(get(p, :show, true)) || continue
            default_col = hex_to_rgb(String(p.colour))
            # A pop is ribbon-drawable when it was TYPED as a track pop OR when its cells actually
            # hold `track_id > 0` (a hand-drawn flow gate over cells that were later tracked). Data
            # fact + typed fact, both required to pass; `hasK` still gates on the segmentation
            # actually having a track_id column at all. See MULTI_POP_TRACKING_PLAN.md Decision 2.
            is_track_pop = (Bool(get(p, :is_track, false)) ||
                            Bool(get(p, :has_tracks, false))) && hasK
            for L in p.labels
                i = get(row_of, Int(L), 0)
                i == 0 && continue
                px = df[i, :centroid_x]; py = df[i, :centroid_y]
                (px isa Real && py isa Real) || continue
                t = hasT ? df[i, :centroid_t] : 0
                (hasT && !(t isa Real && isfinite(Float64(t)))) && continue
                pz = _z_of(df, i, has_z)
                ti = hasT ? Int(round(Float64(t))) : 0
                colour = cb_resolve === nothing ? default_col : cb_resolve(default_col, i)
                _push_point!(ti, (Float64(px), Float64(py), pz), colour)
                if include_tracks && is_track_pop
                    traw = df[i, :track_id]
                    (traw isa Real && isfinite(Float64(traw))) || continue
                    kid = Int(round(Float64(traw)))
                    kid > 0 || continue
                    _push_track!(kid, colour, ti, (Float64(px), Float64(py), pz))
                end
            end
        end
    else
        # Track path — `pop_df(; granularity=:cell)` for track/trackclust: their gates live on
        # `track_props` and `resolve_pops`'s cell fetch cannot evaluate them.
        m = try
            load_pop_map(img; value_name = vn, pop_type = pt)
        catch e
            @warn "_build_overlay_state: load_pop_map failed" value_name pop_type exception = e
            nothing
        end
        pop_meta = Dict{String,NamedTuple}()
        want_paths = String[]
        if m !== nothing
            paths = String[path for path in pop_paths(m) if !pop_at(m, path).transient]
            if pops_filter !== nothing
                pf = Set(String(p) for p in pops_filter)
                paths = [p for p in paths if p in pf]
            end
            for path in paths
                p = pop_at(m, path)
                Bool(hasproperty(p, :show) ? p.show : true) || continue
                pop_meta[String(path)] = (colour = hex_to_rgb(String(p.colour)),)
                push!(want_paths, String(path))
            end
        end
        if !isempty(want_paths)
            df = try
                # `expand_cluster_pops=false`: ONE segmentation's overlay — the run-wide expansion would
                # draw every co-clustered segmentation's cells on it (as `resolve_pops`, the viewer's).
                pop_df(img, pt, want_paths; value_name = vn, granularity = :cell,
                       centroids = :pixel, include_x = false, include_obs = true,
                       expand_cluster_pops = false)
            catch e
                @warn "_build_overlay_state: pop_df failed" value_name pop_type paths = want_paths exception = e
                nothing
            end
            if df !== nothing && size(df, 1) > 0
                col_exists(c) = c in names(df)
                has_z = col_exists("centroid_z")
                # `pop_df(include_obs=true)` already surfaced every obs column — colour_by is
                # already present in the df, no extra select_cols round-trip needed.
                cb_resolve = _cb_prepare(df, cb_col, cb_overrides_rgb, m)
                @inbounds for i in 1:size(df, 1)
                    (col_exists("centroid_x") && col_exists("centroid_y")) || break
                    px = df[i, :centroid_x]; py = df[i, :centroid_y]
                    (px isa Real && py isa Real) || continue
                    t = col_exists("centroid_t") ? df[i, :centroid_t] : 0
                    (col_exists("centroid_t") && !(t isa Real && isfinite(Float64(t)))) && continue
                    pz = _z_of(df, i, has_z)
                    ti = col_exists("centroid_t") ? Int(round(Float64(t))) : 0
                    pop_path = col_exists("pop") ? String(df[i, :pop]) : first(want_paths)
                    meta = get(pop_meta, pop_path, nothing)
                    meta === nothing && continue
                    default_col = meta.colour
                    colour = cb_resolve === nothing ? default_col : cb_resolve(default_col, i)
                    _push_point!(ti, (Float64(px), Float64(py), pz), colour)
                    if include_tracks && col_exists("track_id")
                        traw = df[i, :track_id]
                        (traw isa Real && isfinite(Float64(traw))) || continue
                        kid = Int(round(Float64(traw)))
                        kid > 0 || continue
                        _push_track!(kid, colour, ti, (Float64(px), Float64(py), pz))
                    end
                end
            end
        end
    end

    # ── Segment build — SAME `track_color_mode` semantics as the pre-refactor 2D/3D authors.
    # Speed² uses native-voxel (x, y) only (matching the browser's speedSq); z is threaded through
    # the segment but does NOT enter the speed metric — the 2D still and the 3D animation of the
    # same experiment must share track heat.
    segs_by_end = Dict{Int,NamedTuple{(:x0, :y0, :z0, :x1, :y1, :z1, :colour),
                        Tuple{Vector{Float64},Vector{Float64},Vector{Float64},
                              Vector{Float64},Vector{Float64},Vector{Float64},
                              Vector{RGB{N0f8}}}}}()
    tracks_active = include_tracks && hasT && tail_length > 0
    tcm = effective_tcm
    tcm in TRACK_COLOR_MODES ||
        (@warn "_build_overlay_state: unknown track_color_mode, falling back to \"track\"" mode = tcm;
         tcm = "track")

    if tracks_active
        raw = Tuple{Int,Float64,Float64,Float64,Float64,Float64,Float64,Int,Float64,RGB{N0f8}}[]
        s_min = Inf; s_max = -Inf
        for ((kid, col), hist) in track_hist
            length(hist) >= 2 || continue
            sort!(hist; by = first)
            for k in 1:(length(hist) - 1)
                t0, x0, y0, z0 = hist[k]
                t1, x1, y1, z1 = hist[k + 1]
                t1 > t0 || continue
                dx = x1 - x0; dy = y1 - y0
                sp2 = (dx * dx + dy * dy) / max(1, (t1 - t0))^2
                s_min = min(s_min, sp2); s_max = max(s_max, sp2)
                push!(raw, (t1, x0, y0, z0, x1, y1, z1, kid, sp2, col))
            end
        end
        s_span = (isfinite(s_min) && isfinite(s_max) && s_max > s_min) ? (s_max - s_min) : 0.0
        # "solid" collapses to ONE colour for every ribbon in this author call — there is only ever
        # one (value_name, pop_type) source per _build_overlay_state. That is the source's own colour
        # when the caller has one (`solid_colour`, the viewer's per-source picker), else palette[0]:
        # the browser's `solidRgb`. "pop" keeps `col` — the pop's own swatch that was baked into
        # track_hist's key upstream. This is the Julia mirror of the browser's split.
        solid_col = solid_colour === nothing ? CECELIA_TRACK_PALETTE[1] : hex_to_rgb(String(solid_colour))
        for (t1, x0, y0, z0, x1, y1, z1, kid, sp2, col) in raw
            colour = if tcm == "track"
                CECELIA_TRACK_PALETTE[mod1(abs(kid), length(CECELIA_TRACK_PALETTE))]
            elseif tcm == "speed"
                s_span > 0 ? _heat_ramp((sp2 - s_min) / s_span) : RGB{N0f8}(0.9, 0.9, 0.9)
            elseif tcm == "solid"
                solid_col
            else
                # "pop" (and any other future mode that falls through here): the resolved per-cell
                # colour, which is the pop's own swatch for gated tracks and `all_tracks_colour`
                # in the whole-segmentation path.
                col
            end
            bag = get!(segs_by_end, t1) do
                (; x0 = Float64[], y0 = Float64[], z0 = Float64[],
                   x1 = Float64[], y1 = Float64[], z1 = Float64[], colour = RGB{N0f8}[])
            end
            push!(bag.x0, x0); push!(bag.y0, y0); push!(bag.z0, z0)
            push!(bag.x1, x1); push!(bag.y1, y1); push!(bag.z1, z1)
            push!(bag.colour, colour)
        end
    end

    OverlayState(pts_by_t, segs_by_end, hasT, tail_length, tracks_active)
end

# ── Track-cluster ribbons ──────────────────────────────────────────────────────────
# The movie's "trackclust" chip: a segmentation's shown track-cluster pops, drawn as ribbons by their
# own author call (`pop_type = "trackclust"`, no points) merged with the pops' closure. Drawn WITH the
# pops, as the batch overlay preview has it. Where they draw, the pops' own cell-track ribbons stand
# down on that segmentation — the viewer's rule (`rebuildOverlays`): trackclust colours the SAME tracks
# by cluster, so drawing both stacks two ribbons per track.
trackclust_requested(show_trackclust::Bool, show_pops::Bool, all_tracks::Bool) =
    show_trackclust && show_pops && !all_tracks

"""
    trackclust_draws(img, value_name) -> Bool

Whether `value_name` has any shown track-cluster pop — i.e. whether its trackclust ribbons draw (and
its cell-track ribbons stand down). `false` when the pop map can't be read.
"""
function trackclust_draws(img, value_name::AbstractString)::Bool
    try
        any(L -> L.show, resolve_pops(img, "trackclust"; value_name = value_name))
    catch
        false
    end
end

"""
    merge_overlay_closures(closures) -> ((args...) -> (points, segments)) | nothing

One overlay closure out of several — a movie drawing more than one track source, each built by its
own `build_overlays3d_for` call with its own colour. Each call's points and
segments are concatenated column by column, so the merged closure returns the same shape its parts
do (2D or 3D, whatever their arguments). `nothing` for an empty list.
"""
function merge_overlay_closures(closures::AbstractVector)
    isempty(closures) && return nothing
    length(closures) == 1 && return only(closures)
    cat_nt(a, b) = a === nothing ? b : b === nothing ? a :
                   NamedTuple{keys(a)}(map(vcat, values(a), values(b)))
    (args...) -> begin
        pts = nothing; segs = nothing
        for cl in closures
            p, s = cl(args...)
            pts = cat_nt(pts, p); segs = cat_nt(segs, s)
        end
        (pts, segs)
    end
end

# ─────────────────────────────────────────────────────────────────────────────────
# 3D overlays — world positions; the movie renderer's shader projects them.
# ─────────────────────────────────────────────────────────────────────────────────
#
# A 3D movie is drawn by the browser viewer's own shaders (`writers/render_animation_run.py`), and
# its point and track-tail passes project with the SAME camera as the raycast (one uniform block).
# So Julia hands over positions, not pixels: there is no second projection to keep in step with the
# volume.

"""
    build_overlays3d_for(img; value_name, pop_type,
                         pops_filter = nothing, include_tracks = true, tail_length = 30,
                         all_tracks = false, all_tracks_colour = OVERLAY_GREY,
                         track_color_mode = "track", solid_colour = nothing, include_points = true,
                         colour_by = nothing, colour_overrides = nothing)
        -> (t -> (points, segments))

Per-t closure over the shared overlay state (`_build_overlay_state`, the same one the 2D author
reads), in native voxel coordinates. Points: `(; x, y, z, colour)`; segments: the tail window's
`(; x0, y0, z0, x1, y1, z1, colour, t1)`. Either is `nothing` when the frame has none.
"""
function build_overlays3d_for(img; value_name::AbstractString, pop_type::PopTypeArg,
                              pops_filter::Union{Nothing,AbstractVector{<:AbstractString}} = nothing,
                              include_tracks::Bool = true,
                              tail_length::Int = 30,
                              all_tracks::Bool = false,
                              all_tracks_colour::AbstractString = OVERLAY_GREY,
                              track_color_mode::AbstractString = "track",
                              solid_colour::Union{Nothing,AbstractString} = nothing,
                              include_points::Bool = true,
                              colour_by::Union{Nothing,AbstractString} = nothing,
                              colour_overrides::Union{Nothing,AbstractDict} = nothing)
    state = _build_overlay_state(img;
                                  value_name = value_name, pop_type = pop_type,
                                  pops_filter = pops_filter, include_tracks = include_tracks,
                                  tail_length = tail_length, all_tracks = all_tracks,
                                  all_tracks_colour = all_tracks_colour,
                                  track_color_mode = track_color_mode,
                                  solid_colour = solid_colour,
                                  colour_by = colour_by,
                                  colour_overrides = colour_overrides)
    return function(t::Int)
        pts, segs = _state_at(state, t)
        (!include_points || pts === nothing || isempty(pts.x)) && (pts = nothing)
        (segs === nothing || isempty(segs.x0)) && (segs = nothing)
        (pts, segs)
    end
end


# ─────────────────────────────────────────────────────────────────────────────────
# Mask author — the P4 outline pass, per-t.
# ─────────────────────────────────────────────────────────────────────────────────

"""
    mask_id_colours(img; value_name, pop_type, pops_filter = nothing,
                    all_cells = false, all_cells_colour = OVERLAY_GREY,
                    colour_by = nothing, colour_overrides = nothing) -> Dict{Int,RGB{N0f8}}

Which labels a movie's mask draws, and in what colour: the populations' cells in their colours (or
every cell, with `all_cells`), recoloured by `colour_by` when asked. An id absent from the map is not
drawn. The shader takes it as its label colour table (`labelColours`, `render_animation_run.py`).
"""
function mask_id_colours(img; value_name::AbstractString, pop_type::PopTypeArg,
                         pops_filter::Union{Nothing,AbstractVector{<:AbstractString}} = nothing,
                         all_cells::Bool = false,
                         all_cells_colour::AbstractString = OVERLAY_GREY,
                         colour_by::Union{Nothing,AbstractString} = nothing,
                         colour_overrides::Union{Nothing,AbstractDict} = nothing)
    pt = string(pop_type)
    vn = String(value_name)
    is_track_pt = is_track_grained(pt)
    cb_col = (colour_by === nothing || isempty(String(colour_by))) ? nothing : String(colour_by)
    cb_overrides_rgb = _prep_overrides(colour_overrides)

    # ── id → colour from pops.
    id_colours = Dict{Int,RGB{N0f8}}()
    if all_cells
        # Paint every known cell in one colour. Enumerating labels from `label_props` costs one
        # `.h5ad` read; the alternative — scanning uniques out of the mask per frame — is nT reads
        # over the whole label store, which is what the sweep is trying to avoid in the first place.
        #
        # `all_cells_colour == "rainbow"` cycles `CECELIA_TRACK_PALETTE` by id — asked for by the
        # compare-grid path where a uniform gray outline was invisible against the channels (dense
        # segmentation collapses to identical tiny rings on the encoded frame).
        lp = label_props(img; value_name = vn)
        df = as_df(lp)
        is_rainbow = lowercase(String(all_cells_colour)) == "rainbow"
        solid = is_rainbow ? nothing : hex_to_rgb(String(all_cells_colour))
        palette = CECELIA_TRACK_PALETTE
        pal_n   = length(palette)
        @inbounds for i in 1:size(df, 1)
            lab = df[i, :label]
            (lab isa Real && isfinite(Float64(lab))) || continue
            L = Int(round(Float64(lab)))
            id_colours[L] = is_rainbow ? palette[mod(L - 1, pal_n) + 1] : solid
        end
    elseif !is_track_pt
        pops = try
            resolve_pops(img, pt; value_name = vn)
        catch e
            @warn "mask_id_colours: resolve_pops failed" value_name pop_type exception = e
            NamedTuple[]
        end
        if pops_filter !== nothing
            want = Set(String(p) for p in pops_filter)
            pops = [p for p in pops if String(p.path) in want]
        end
        for p in pops
            Bool(get(p, :show, true)) || continue
            colour = hex_to_rgb(String(p.colour))
            for L in p.labels
                id_colours[Int(L)] = colour
            end
        end
    else
        # Track pop_types — expand via `pop_df` for the same reason as `_build_overlay_state` (gates
        # live on `track_props`). Only `label` + `pop` are read; centroids/track_id are irrelevant
        # for a label→colour lookup.
        m = try
            load_pop_map(img; value_name = vn, pop_type = pt)
        catch e
            @warn "mask_id_colours: load_pop_map failed for track pop_type" value_name pop_type exception = e
            nothing
        end
        pop_meta = Dict{String,RGB{N0f8}}()
        want_paths = String[]
        if m !== nothing
            paths = String[path for path in pop_paths(m) if !pop_at(m, path).transient]
            if pops_filter !== nothing
                pf = Set(String(p) for p in pops_filter)
                paths = [p for p in paths if p in pf]
            end
            for path in paths
                p = pop_at(m, path)
                Bool(hasproperty(p, :show) ? p.show : true) || continue
                pop_meta[String(path)] = hex_to_rgb(String(p.colour))
                push!(want_paths, String(path))
            end
        end
        if !isempty(want_paths)
            df = try
                # `expand_cluster_pops=false`: ONE segmentation's overlay — the run-wide expansion would
                # draw every co-clustered segmentation's cells on it (as `resolve_pops`, the viewer's).
                pop_df(img, pt, want_paths; value_name = vn, granularity = :cell,
                       centroids = :pixel, include_x = false, include_obs = true,
                       expand_cluster_pops = false)
            catch e
                @warn "mask_id_colours: pop_df failed for track pop_type" value_name pop_type paths = want_paths exception = e
                nothing
            end
            if df !== nothing && size(df, 1) > 0
                cols = names(df)
                has_pop = "pop" in cols
                @inbounds for i in 1:size(df, 1)
                    lab = df[i, :label]
                    (lab isa Real && isfinite(Float64(lab))) || continue
                    pop_path = has_pop ? String(df[i, :pop]) : first(want_paths)
                    colour = get(pop_meta, pop_path, nothing)
                    colour === nothing && continue
                    id_colours[Int(round(Float64(lab)))] = colour
                end
            end
        end
    end

    # ── colour_labels — recolour every id in id_colours by its value in an obs column. Same
    # `_cb_prepare` resolver the overlay authors use, so a labels layer coloured by "clusters" and
    # a points layer coloured by "clusters" pick the SAME hex per value — one palette, one place.
    # `nothing` if the column is absent OR colour_by wasn't asked for → falls through to the
    # pop-derived colours built above.
    if cb_col !== nothing && !isempty(id_colours)
        lp = label_props(img; value_name = vn)
        select_cols(lp, [cb_col])
        df = try
            as_df(lp)
        catch e
            @warn "mask_id_colours: colour_by column read failed" value_name colour_by exception = e
            nothing
        end
        if df !== nothing
            # Cell-pops path uses a pop_map for user-pop colour donation; all-cells / track paths
            # get plain Okabe-Ito / heat-ramp. Match the overlay author's per-branch rule.
            cb_pop_map = if all_cells
                nothing
            elseif is_track_pt
                try load_pop_map(img; value_name = vn, pop_type = pt) catch; nothing end
            else
                try load_pop_map(img; value_name = vn, pop_type = pt) catch; nothing end
            end
            cb_resolve = _cb_prepare(df, cb_col, cb_overrides_rgb, cb_pop_map)
            if cb_resolve !== nothing
                @inbounds for i in 1:size(df, 1)
                    lab = df[i, :label]
                    (lab isa Real && isfinite(Float64(lab))) || continue
                    lid = Int(round(Float64(lab)))
                    haskey(id_colours, lid) || continue
                    id_colours[lid] = cb_resolve(id_colours[lid], i)
                end
            end
        end
    end

    id_colours
end

"""
    ViewerPalette()

A movie mask's colouring when it draws every label in the viewer's palette (`label_palette.json`,
`id % rows`) — the other colouring is a label → colour table, where an id absent from it is not drawn.
"""
struct ViewerPalette end

"""
    MovieMask

A movie's mask, for the shader (`_mask_params!`): the label store, its outline width and opacity, and
how labels are coloured — `ViewerPalette()` or a `Dict{Int,RGB{N0f8}}` table. A table may be empty (a
population with no cells here), which draws no labels, not every label.
"""
struct MovieMask
    labels_path::String
    colours::Union{ViewerPalette,Dict{Int,RGB{N0f8}}}
    contour_px::Int
    opacity::Float64
end

"""
    movie_mask(img; value_name, contour_px, opacity, kwargs...) -> MovieMask | nothing

"All cells" with no `colour_by` draws every label in the viewer's palette; otherwise the colours are
`mask_id_colours` — the populations' labels in their colours. `nothing` when the label store is not
on disk or the colours could not be resolved (logged). `kwargs` are `mask_id_colours`'s.
"""
function movie_mask(img; value_name::AbstractString, contour_px::Integer, opacity::Real,
                    all_cells::Bool = false, colour_by = nothing, kwargs...)
    lp = img_labels_path(img, value_name)
    isdir(lp) || return nothing
    colours = if all_cells && (colour_by === nothing || isempty(String(colour_by)))
        ViewerPalette()
    else
        try
            mask_id_colours(img; value_name = value_name, all_cells = all_cells,
                            colour_by = colour_by, kwargs...)
        catch e
            @warn "movie mask: colours failed" value_name exception = e
            return nothing
        end
    end
    MovieMask(String(lp), colours, Int(contour_px), Float64(opacity))
end
