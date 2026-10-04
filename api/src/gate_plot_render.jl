# ── Server-side gate plot (PNG) ───────────────────────────────────────────────────
# The gating scatter the GUI draws, as a picture: one population's cells on two (transformed) axes,
# its child gates outlined and numbered, tick values on both axes. For a client that cannot run the
# browser plot — an MCP session sees this image instead of a table of quantiles.
#
# Draws what the browser's gate plot draws (`GateScatterCell` + `PlotLayers`, default dot mode) — not
# the same code (that is canvas/TS), the same look and the same numbers. PURE: takes the transformed points, the display extents, the ticks and the projected gate outlines
# (all computed by the gating API — `_plot_xy`, `_axis_ticks`, `project_gate`) and only draws. No
# text stack here: tick values use a 3×5 digit font (digits . - + e); axis names and the gate number
# → path key travel next to the image in the route's JSON.

using PNGFiles, ColorTypes, FixedPointNumbers   # `_heat_ramp`, `hex_to_rgb`: overlay_author.jl

const _GLYPHS_3x5 = Dict{Char,NTuple{5,UInt8}}(   # rows top→bottom, 3 bits each (MSB = left column)
    '0' => (0b111, 0b101, 0b101, 0b101, 0b111), '1' => (0b010, 0b110, 0b010, 0b010, 0b111),
    '2' => (0b111, 0b001, 0b111, 0b100, 0b111), '3' => (0b111, 0b001, 0b111, 0b001, 0b111),
    '4' => (0b101, 0b101, 0b111, 0b001, 0b001), '5' => (0b111, 0b100, 0b111, 0b001, 0b111),
    '6' => (0b111, 0b100, 0b111, 0b101, 0b111), '7' => (0b111, 0b001, 0b001, 0b010, 0b010),
    '8' => (0b111, 0b101, 0b111, 0b101, 0b111), '9' => (0b111, 0b101, 0b111, 0b001, 0b111),
    '.' => (0b000, 0b000, 0b000, 0b000, 0b010), '-' => (0b000, 0b000, 0b111, 0b000, 0b000),
    '+' => (0b000, 0b010, 0b111, 0b010, 0b000), 'e' => (0b000, 0b111, 0b111, 0b100, 0b111))
const _GLYPH_SCALE = 2
const _GLYPH_ADV   = 4 * _GLYPH_SCALE                 # 3 columns + 1 gap

_text_width(s::AbstractString) = max(0, length(s) * _GLYPH_ADV - _GLYPH_SCALE)
const _TEXT_HEIGHT = 5 * _GLYPH_SCALE

# draw `s` with its top-left at (row, col); characters outside the font are skipped as a blank
function _draw_text!(img::AbstractMatrix, s::AbstractString, row::Int, col::Int, c)
    H, W = size(img)
    for (k, ch) in enumerate(s)
        g = get(_GLYPHS_3x5, ch, nothing)
        g === nothing && continue
        x0 = col + (k - 1) * _GLYPH_ADV
        for r in 1:5, b in 0:2
            (g[r] >> (2 - b)) & 0x1 == 0x1 || continue
            for dr in 0:(_GLYPH_SCALE - 1), dc in 0:(_GLYPH_SCALE - 1)
                y = row + (r - 1) * _GLYPH_SCALE + dr; x = x0 + b * _GLYPH_SCALE + dc
                (1 <= y <= H && 1 <= x <= W) && (img[y, x] = c)
            end
        end
    end
end

# Liang–Barsky: clip segment (x0,y0)→(x1,y1) to [xlo,xhi]×[ylo,yhi]; `nothing` when it misses. A gate
# drawn as "everything above 240" spans ±1e9 on the free axis — it must clip, not be walked pixel by pixel.
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

function _draw_line!(img::AbstractMatrix, x0, y0, x1, y1, c; thick::Int = 2)
    H, W = size(img)
    n = max(1, ceil(Int, max(abs(x1 - x0), abs(y1 - y0))))
    for i in 0:n
        x = round(Int, x0 + (x1 - x0) * i / n); y = round(Int, y0 + (y1 - y0) * i / n)
        for dy in 0:(thick - 1), dx in 0:(thick - 1)
            (1 <= y + dy <= H && 1 <= x + dx <= W) && (img[y + dy, x + dx] = c)
        end
    end
end

# The browser plot's look, so the picture and the GUI read the same: its PNG-export background, the
# theme's border + dim-text colours, and the FlowJo pseudocolour dot plot — each dot coloured by its
# LOG-scaled, box-blurred local density through the shared `heatRamp` (`_heat_ramp`, palettes.json).
# Port of `frontend/src/plots/density.ts` → `pointDensities` (grid 160, blur radius 2 × 2 passes);
# keep the constants in step with it.
const _PLOT_BG     = RGB{N0f8}(0x0d / 255, 0x0b / 255, 0x1a / 255)   # GateScatterCell exportImage bg
const _PLOT_BORDER = RGB{N0f8}(0x30 / 255, 0x36 / 255, 0x3d / 255)   # --cc-border
const _PLOT_DIM    = RGB{N0f8}(0x7d / 255, 0x85 / 255, 0x90 / 255)   # --cc-text-dim
const _DOT_GRID = 160
const _DOT_BLUR_RADIUS = 2
const _DOT_BLUR_PASSES = 2

# separable box blur, clamped edges — `boxBlur` in density.ts
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

"""
    render_gate_plot(xv, yv, xext, yext, xticks, yticks, gates; width=520, height=440)
        -> Matrix{RGB{N0f8}}

The population's points (`xv`, `yv`, already transformed) as the browser's pseudocolour dot plot
(`point_densities` → `_heat_ramp`) inside the display box `xext` × `yext`. `xticks`/`yticks` are `_axis_ticks` dicts
(`pos` in transformed space, `label` the raw value). `gates` are `project_gate` outlines carrying a
`colour`; each is drawn clipped to the box and numbered 1…n in list order at its first on-screen
vertex. Non-finite points are skipped; points outside the box are not drawn.
"""
function render_gate_plot(xv::AbstractVector, yv::AbstractVector,
                          xext::Tuple{<:Real,<:Real}, yext::Tuple{<:Real,<:Real},
                          xticks, yticks, gates; width::Int = 520, height::Int = 440)
    ml, mr, mt, mb = 64, 12, 12, 32
    pw, ph = width - ml - mr, height - mt - mb
    img = fill(_PLOT_BG, height, width)
    xlo, xhi = float(xext[1]), float(xext[2]); ylo, yhi = float(yext[1]), float(yext[2])
    xspan = xhi > xlo ? xhi - xlo : 1.0; yspan = yhi > ylo ? yhi - ylo : 1.0
    # data → canvas (column, row); y grows upward on the plot, downward in the matrix
    col(x) = ml + (x - xlo) / xspan * (pw - 1) + 1
    row(y) = mt + (yhi - y) / yspan * (ph - 1) + 1

    # dots drawn sparse → dense, so the dense core sits on top as in the browser plot
    dens = point_densities(xv, yv, xext, yext)
    for k in sortperm(dens)
        x = float(xv[k]); y = float(yv[k])
        (isfinite(x) && isfinite(y) && xlo <= x <= xhi && ylo <= y <= yhi) || continue
        r = round(Int, row(y)); c = round(Int, col(x)); colour = _heat_ramp(dens[k])
        img[max(mt + 1, r - 1):min(mt + ph, r + 1), max(ml + 1, c - 1):min(ml + pw, c + 1)] .= colour
    end

    ink = _PLOT_DIM
    _draw_line!(img, ml, mt + ph + 1, ml + pw, mt + ph + 1, _PLOT_BORDER; thick = 1)   # x axis
    _draw_line!(img, ml, mt + 1, ml, mt + ph + 1, _PLOT_BORDER; thick = 1)             # y axis
    for t in xticks
        p = float(t["pos"]); xlo <= p <= xhi || continue
        cx = round(Int, col(p))
        _draw_line!(img, cx, mt + ph + 1, cx, mt + ph + 5, ink; thick = 1)
        s = string(t["label"])
        _draw_text!(img, s, mt + ph + 9, clamp(cx - _text_width(s) ÷ 2, 1, width - _text_width(s)), ink)
    end
    for t in yticks
        p = float(t["pos"]); ylo <= p <= yhi || continue
        cy = round(Int, row(p))
        _draw_line!(img, ml - 4, cy, ml, cy, ink; thick = 1)
        s = string(t["label"])
        _draw_text!(img, s, clamp(cy - _TEXT_HEIGHT ÷ 2, 1, height - _TEXT_HEIGHT),
                    max(1, ml - 7 - _text_width(s)), ink)
    end

    for (k, g) in enumerate(gates)
        c = hex_to_rgb(string(get(g, "colour", "#ffffff")))
        pts = if get(g, "kind", "") == "rectangle"
            [(g["x_min"], g["y_min"]), (g["x_max"], g["y_min"]), (g["x_max"], g["y_max"]), (g["x_min"], g["y_max"])]
        else
            [(v[1], v[2]) for v in get(g, "vertices", ())]
        end
        length(pts) >= 2 || continue
        first_on = nothing
        for i in eachindex(pts)
            a = pts[i]; b = pts[mod1(i + 1, length(pts))]
            seg = _clip_segment(float(a[1]), float(a[2]), float(b[1]), float(b[2]), xlo, xhi, ylo, yhi)
            seg === nothing && continue
            _draw_line!(img, col(seg[1]), row(seg[2]), col(seg[3]), row(seg[4]), c)
            first_on === nothing && (first_on = (col(seg[1]), row(seg[2])))
        end
        if first_on !== nothing
            s = string(k)
            r0 = clamp(round(Int, first_on[2]) + 4, mt + 1, mt + ph - _TEXT_HEIGHT)
            c0 = clamp(round(Int, first_on[1]) + 4, ml + 1, ml + pw - _text_width(s))
            img[max(1, r0 - 2):min(height, r0 + _TEXT_HEIGHT + 1), max(1, c0 - 2):min(width, c0 + _text_width(s) + 1)] .=
                _PLOT_BG                              # a backing box, so the number reads over dense dots
            _draw_text!(img, s, r0, c0, c)
        end
    end
    img
end

render_gate_plot_png(args...; kw...) = (io = IOBuffer(); PNGFiles.save(io, render_gate_plot(args...; kw...)); take!(io))
