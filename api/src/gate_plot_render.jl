# ── Gate plot raster — the browser's gating scatter as a PNG ────────────────────
# A PURE renderer: dots + gate outlines + axes onto an RGB matrix, in the look of the Gate page's plot
# (`GateScatterCell.vue` axes, `PlotLayers.vue` dots, `GateOverlay.vue` gates), so a picture an agent
# looks at and the plot a person gates on read the same. Drawn at 2× the browser's CSS pixels
# (`_PLOT_SCALE`) so the 1.4-px dots and the 10-px labels survive. No HTTP, no image access — the route
# (`gating_views_api.jl`) hands it transformed values, extents, ticks, titles and gate outlines.
#
# Text: the Julia API has no font rasteriser, so labels come from a prebuilt glyph atlas
# (`api/assets/plot_font.json`, DejaVu Sans, built by `scripts/build_plot_font.py`).
using PNGFiles, Base64, ColorTypes, FixedPointNumbers   # `_heat_ramp`, `hex_to_rgb`: overlay_author.jl

const _PLOT_SCALE  = 2
const _PLOT_SIDE   = 300                                  # CSS px; the browser's plot area is square
const _PLOT_PAD    = (top = 18, right = 22, bottom = 50, left = 84)   # GateScatterCell capture padding
const _PLOT_BG     = RGB{N0f8}(0x16 / 255, 0x1b / 255, 0x22 / 255)  # --cc-surface-1, the panel it sits on
const _PLOT_BORDER = RGB{N0f8}(0x30 / 255, 0x36 / 255, 0x3d / 255)  # --cc-border (axis lines)
const _PLOT_DIM    = RGB{N0f8}(0x7d / 255, 0x85 / 255, 0x90 / 255)  # --cc-text-dim (ticks + tick labels)
const _PLOT_TEXT   = RGB{N0f8}(0xe6 / 255, 0xed / 255, 0xf3 / 255)  # --cc-text (axis titles; white gates)
const _DOT_R       = 0.7                                  # CSS px radius (`plots/density.ts` DOT_R)
const _DOT_BUCKETS = 64                                   # density quantised as PlotLayers paints it
const _GATE_WIDTH  = 1.5                                  # CSS px stroke (GatingPlots lineWidth)
const _DOT_GRID = 160
const _DOT_BLUR_RADIUS = 2
const _DOT_BLUR_PASSES = 2

# ── glyph atlas ──────────────────────────────────────────────────────────────────
const _FONT_PATH = normpath(joinpath(@__DIR__, "..", "assets", "plot_font.json"))
const _FONT = Ref{Any}(nothing)
function _font(face::String)
    if _FONT[] === nothing
        raw = JSON3.read(read(_FONT_PATH, String))
        _FONT[] = Dict(String(k) => (ascent = Int(f.ascent), descent = Int(f.descent),
            glyphs = Dict(first(String(c)) => (adv = Int(g.adv), x = Int(g.x), y = Int(g.y), w = Int(g.w), h = Int(g.h),
                a = haskey(g, :a) ? permutedims(reshape(Float32.(base64decode(String(g.a))) ./ 255f0, Int(g.w), Int(g.h))) :
                    zeros(Float32, 0, 0)) for (c, g) in f.glyphs))
            for (k, f) in raw.faces)
    end
    _FONT[][face]
end

# Coverage mask of `s` in `face`: rows = ascent + descent, columns = the pen advance.
function _text_mask(face::String, s::AbstractString)::Matrix{Float32}
    f = _font(face)
    gl = [get(f.glyphs, c, f.glyphs['?']) for c in s]
    m = zeros(Float32, f.ascent + f.descent, max(1, sum(g.adv for g in gl; init = 0)))
    pen = 0
    for g in gl
        for r in 1:g.h, c in 1:g.w
            rr, cc = g.y + r, pen + g.x + c
            (1 <= rr <= size(m, 1) && 1 <= cc <= size(m, 2)) && (m[rr, cc] = max(m[rr, cc], g.a[r, c]))
        end
        pen += g.adv
    end
    m
end

# Blend `colour` over `img` through `mask` (scaled by `alpha`), mask's top-left at (row, col).
function _blit!(img::AbstractMatrix, mask::AbstractMatrix, row::Int, col::Int, colour; alpha = 1.0)
    H, W = size(img)
    for r in axes(mask, 1), c in axes(mask, 2)
        a = mask[r, c] * alpha
        a > 0 || continue
        i, j = row + r - 1, col + c - 1
        (1 <= i <= H && 1 <= j <= W) || continue
        img[i, j] = _mix(img[i, j], colour, a)
    end
end
_mix(bg, fg, a) = RGB{N0f8}(clamp(red(fg) * a + red(bg) * (1 - a), 0, 1),
                            clamp(green(fg) * a + green(bg) * (1 - a), 0, 1),
                            clamp(blue(fg) * a + blue(bg) * (1 - a), 0, 1))

# A dark halo around text (GateOverlay's strokeText, rgba(0,0,0,0.7), lineWidth 3) — the mask dilated.
function _halo(mask::AbstractMatrix, r::Int)
    out = zeros(Float32, size(mask, 1) + 2r, size(mask, 2) + 2r)
    for dr in -r:r, dc in -r:r
        dr^2 + dc^2 <= r^2 || continue
        @views out[r + 1 + dr:r + dr + size(mask, 1), r + 1 + dc:r + dc + size(mask, 2)] .=
            max.(out[r + 1 + dr:r + dr + size(mask, 1), r + 1 + dc:r + dc + size(mask, 2)], mask)
    end
    out
end

# Tick label as the browser shows it (`GateScatterCell.vue` fmtTick): ≥1e3 → k, ≥1e6 → M, ≥1e9 → G
# with one decimal ("2.1k", "262k"); anything smaller passes the server label through.
function _fmt_tick_label(label)::String
    s = string(label)
    n = tryparse(Float64, s)
    (n === nothing || !isfinite(n)) && return s
    a = abs(n)
    for (lim, suf) in ((1e9, "G"), (1e6, "M"), (1e3, "k"))
        a >= lim || continue
        t = string(round(n / lim; digits = 1))
        return replace(t, r"\.0$" => "") * suf
    end
    s
end

# ── geometry ─────────────────────────────────────────────────────────────────────
# Liang–Barsky: the part of segment (x0,y0)→(x1,y1) inside the box, or `nothing`.
function _clip_segment(x0, y0, x1, y1, xlo, xhi, ylo, yhi)
    t0, t1 = 0.0, 1.0
    dx, dy = x1 - x0, y1 - y0
    for (p, q) in ((-dx, x0 - xlo), (dx, xhi - x0), (-dy, y0 - ylo), (dy, yhi - y0))
        if p == 0
            q < 0 && return nothing
        else
            t = q / p
            if p < 0
                t > t1 && return nothing
                t > t0 && (t0 = t)
            else
                t < t0 && return nothing
                t < t1 && (t1 = t)
            end
        end
    end
    (x0 + t0 * dx, y0 + t0 * dy, x0 + t1 * dx, y0 + t1 * dy)
end

# Anti-aliased stroke of canvas-space segments [(c0, r0, c1, r1), …], `width` px, blended once.
function _stroke!(img::AbstractMatrix, segs, colour, width::Real)
    H, W = size(img)
    cov = Dict{Tuple{Int,Int},Float32}()
    hw = width / 2
    for (c0, r0, c1, r1) in segs
        dc, dr = c1 - c0, r1 - r0
        len2 = dc^2 + dr^2
        for i in max(1, floor(Int, min(r0, r1) - hw - 1)):min(H, ceil(Int, max(r0, r1) + hw + 1)),
            j in max(1, floor(Int, min(c0, c1) - hw - 1)):min(W, ceil(Int, max(c0, c1) + hw + 1))
            t = len2 > 0 ? clamp(((j - c0) * dc + (i - r0) * dr) / len2, 0, 1) : 0.0
            d = hypot(j - (c0 + t * dc), i - (r0 + t * dr))
            a = Float32(clamp(hw + 0.5 - d, 0, 1))
            a > 0 && (cov[(i, j)] = max(get(cov, (i, j), 0f0), a))
        end
    end
    for ((i, j), a) in cov
        img[i, j] = _mix(img[i, j], colour, a)
    end
end

# ── density ──────────────────────────────────────────────────────────────────────
# FlowJo-style pseudocolour, ported from the browser (`plots/density.ts`): bin into a 160² grid over
# the view, box-blur (radius 2, 2 passes), each point takes its cell's LOG-scaled density through the
# shared heat ramp (`_heat_ramp`, palettes.json). Bins via the gating engine's `density_2d`.
function _box_blur!(g::Matrix{Float64}, radius::Int, passes::Int)
    G = size(g, 1); win = 2radius + 1; tmp = similar(g)
    for _ in 1:passes
        for gy in 1:G, gx in 1:G
            acc = 0.0
            for d in -radius:radius; acc += g[gy, clamp(gx + d, 1, G)]; end
            tmp[gy, gx] = acc / win
        end
        for gy in 1:G, gx in 1:G
            acc = 0.0
            for d in -radius:radius; acc += tmp[clamp(gy + d, 1, G), gx]; end
            g[gy, gx] = acc / win
        end
    end
    g
end

"""
    point_densities(xv, yv, xext, yext) -> Vector{Float64}

Per-point local density in 0..1 (log1p of the blurred count over its max) — the colour index of the
browser's dot plot. Non-finite or out-of-box points get 0.
"""
function point_densities(xv::AbstractVector, yv::AbstractVector, xext, yext; G::Int = _DOT_GRID)
    xlo, xhi = float(xext[1]), float(xext[2]); ylo, yhi = float(yext[1]), float(yext[2])
    xs = xhi > xlo ? xhi - xlo : 1.0; ys = yhi > ylo ? yhi - ylo : 1.0
    cell(x, y) = (floor(Int, (x - xlo) / xs * G) + 1, floor(Int, (y - ylo) / ys * G) + 1)
    # the gating engine's own binning (`density_2d`) over the in-box points — the browser drops points
    # outside the box, where `density_2d` would clamp them into the edge bins
    inbox = [i for i in eachindex(xv, yv)
             if isfinite(xv[i]) && isfinite(yv[i]) && (c = cell(float(xv[i]), float(yv[i]));
                                                       1 <= c[1] <= G && 1 <= c[2] <= G)]
    g = Float64.(permutedims(density_2d(xv[inbox], yv[inbox]; bins = G, xlim = (xlo, xhi),
                                        ylim = (ylo, yhi)).counts))          # (x, y) → [gy, gx]
    _box_blur!(g, _DOT_BLUR_RADIUS, _DOT_BLUR_PASSES)
    lmax = log1p(maximum(g; init = 0.0)); lmax == 0 && (lmax = 1.0)
    out = zeros(Float64, length(xv))
    for (k, i) in enumerate(eachindex(xv, yv))
        x = float(xv[i]); y = float(yv[i]); (isfinite(x) && isfinite(y)) || continue
        gx, gy = cell(x, y); (1 <= gx <= G && 1 <= gy <= G) && (out[k] = log1p(g[gy, gx]) / lmax)
    end
    out
end

# ── the plot ─────────────────────────────────────────────────────────────────────
"""
    render_gate_plot(xv, yv, xext, yext, xticks, yticks, gates; xtitle = "", ytitle = "") -> Matrix{RGB{N0f8}}

The Gate page's scatter at 2×: dots (TRANSFORMED values) coloured by local density, painted in 64
density buckets low → high; `gates` (`project_gate` outlines with `colour` and `path`) stroked in their
colour and named above (below / inside when there is no room), as `GateOverlay` does; left + bottom
axes with `_axis_ticks` ticks (labels formatted like the browser) and the axis titles. Points outside
`xext`×`yext` are not drawn; gate outlines are clipped to it.
"""
function render_gate_plot(xv::AbstractVector, yv::AbstractVector,
                          xext::Tuple{<:Real,<:Real}, yext::Tuple{<:Real,<:Real},
                          xticks, yticks, gates; xtitle::AbstractString = "", ytitle::AbstractString = "")
    s = _PLOT_SCALE
    P = _PLOT_SIDE * s
    L, T = _PLOT_PAD.left * s, _PLOT_PAD.top * s
    width, height = L + P + _PLOT_PAD.right * s, T + P + _PLOT_PAD.bottom * s
    img = fill(_PLOT_BG, height, width)
    xlo, xhi = float(xext[1]), float(xext[2]); ylo, yhi = float(yext[1]), float(yext[2])
    xspan = xhi > xlo ? xhi - xlo : 1.0; yspan = yhi > ylo ? yhi - ylo : 1.0
    # data → canvas pixel centre (column, row); y grows upward on the plot, downward in the matrix
    col(x) = L + (x - xlo) / xspan * P + 0.5
    row(y) = T + (yhi - y) / yspan * P + 0.5
    B = T + P                                                  # last row of the plot area

    # dots: anti-aliased discs, density buckets low → high so the dense core sits on top
    dens = point_densities(xv, yv, xext, yext)
    bucket = [min(_DOT_BUCKETS - 1, floor(Int, d * _DOT_BUCKETS)) for d in dens]
    r = _DOT_R * s
    for k in sortperm(bucket)
        x = float(xv[k]); y = float(yv[k])
        (isfinite(x) && isfinite(y) && xlo <= x <= xhi && ylo <= y <= yhi) || continue
        c0, r0 = col(x), row(y)
        colour = _heat_ramp(bucket[k] / (_DOT_BUCKETS - 1))
        for i in max(T + 1, floor(Int, r0 - r)):min(B, ceil(Int, r0 + r)),
            j in max(L + 1, floor(Int, c0 - r)):min(L + P, ceil(Int, c0 + r))
            a = clamp(r + 0.5 - hypot(j - c0, i - r0), 0, 1)
            a > 0 && (img[i, j] = _mix(img[i, j], colour, a))
        end
    end

    # gates, clipped, then their names
    for g in gates
        hex = lowercase(string(get(g, "colour", "")))
        colour = hex in ("", "white", "#fff", "#ffffff", "#fafafa") ? _PLOT_TEXT : hex_to_rgb(hex)
        pts = if get(g, "kind", "") == "rectangle"
            [(g["x_min"], g["y_min"]), (g["x_max"], g["y_min"]), (g["x_max"], g["y_max"]), (g["x_min"], g["y_max"])]
        else
            [(v[1], v[2]) for v in get(g, "vertices", ())]
        end
        length(pts) >= 2 || continue
        segs = Tuple{Float64,Float64,Float64,Float64}[]
        for i in eachindex(pts)
            a = pts[i]; b = pts[mod1(i + 1, length(pts))]
            seg = _clip_segment(float(a[1]), float(a[2]), float(b[1]), float(b[2]), xlo, xhi, ylo, yhi)
            seg === nothing || push!(segs, (col(seg[1]), row(seg[2]), col(seg[3]), row(seg[4])))
        end
        isempty(segs) && continue
        _stroke!(img, segs, colour, _GATE_WIDTH * s)
        parts = split(string(get(g, "path", "")), '/'; keepempty = false)
        name = isempty(parts) ? "" : String(last(parts))
        isempty(name) && continue
        m = _text_mask("label", name)
        cs = [c for sg in segs for c in (sg[1], sg[3])]; rs = [r for sg in segs for r in (sg[2], sg[4])]
        top, bot = minimum(rs) - T, maximum(rs) - T            # gate bbox, plot-area rows
        half = size(m, 2) / 2 + 3s
        cx = clamp((minimum(cs) + maximum(cs)) / 2 - L, half, P - half)
        rtop = top - 4s >= 15s ? top - 4s - size(m, 1) : bot + 19s <= P ? bot + 4s : top + 4s
        rr, cc = round(Int, T + rtop), round(Int, L + cx - size(m, 2) / 2)
        _blit!(img, _halo(m, round(Int, 1.5s)), rr - round(Int, 1.5s), cc - round(Int, 1.5s), RGB{N0f8}(0, 0, 0); alpha = 0.7)
        _blit!(img, m, rr, cc, colour)
    end

    # axes: left + bottom lines, outward 5-px ticks, labels 8 px off the axis, titles
    img[B + 1:B + s, L - s + 1:L + P] .= _PLOT_BORDER
    img[T + 1:B + s, L - s + 1:L] .= _PLOT_BORDER
    for t in xticks
        p = float(t["pos"]); xlo - 1e-9 <= p <= xhi + 1e-9 || continue
        c = round(Int, col(p))
        img[B + s + 1:B + 5s, max(1, c - s ÷ 2):min(width, c + s ÷ 2)] .= _PLOT_DIM
        m = _text_mask("tick", _fmt_tick_label(t["label"]))
        _blit!(img, m, B + 8s, clamp(c - size(m, 2) ÷ 2, 1, width - size(m, 2)), _PLOT_DIM)
    end
    for t in yticks
        p = float(t["pos"]); ylo - 1e-9 <= p <= yhi + 1e-9 || continue
        r0 = round(Int, row(p))
        img[max(1, r0 - s ÷ 2):min(height, r0 + s ÷ 2), L - 5s - s + 1:L - s] .= _PLOT_DIM
        m = _text_mask("tick", _fmt_tick_label(t["label"]))
        _blit!(img, m, clamp(r0 - size(m, 1) ÷ 2, 1, height - size(m, 1)), max(1, L - 8s - size(m, 2)), _PLOT_DIM)
    end
    if !isempty(xtitle)
        m = _text_mask("title", xtitle)
        _blit!(img, m, B + 40s - size(m, 1), L + (P - size(m, 2)) ÷ 2, _PLOT_TEXT)
    end
    if !isempty(ytitle)
        m = rotl90(_text_mask("title", ytitle))                # reads bottom → top
        _blit!(img, m, T + (P - size(m, 1)) ÷ 2, L - 66s, _PLOT_TEXT)
    end
    img
end

render_gate_plot_png(args...; kw...) = (io = IOBuffer(); PNGFiles.save(io, render_gate_plot(args...; kw...)); take!(io))
