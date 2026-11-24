# ── Gates ─────────────────────────────────────────────────────────────────────
#
# A gate is self-contained: it names its two channels, carries the per-axis transform,
# and stores its geometry in TRANSFORMED coordinates. Membership = transform the raw
# channel values, then test geometry in transformed space (docs/POPULATION.md).
# Rectangle + polygon here; the split gates (slice, quadrant) in split_gates.jl.

abstract type Gate end

struct RectangleGate <: Gate
    x_channel::String
    y_channel::String
    x_transform::AxisTransform
    y_transform::AxisTransform
    x_min::Float64
    x_max::Float64
    y_min::Float64
    y_max::Float64
end

function RectangleGate(x_channel, y_channel, x_min, x_max, y_min, y_max;
                       x_transform::AxisTransform=LinearTransform(),
                       y_transform::AxisTransform=LinearTransform())
    RectangleGate(String(x_channel), String(y_channel), x_transform, y_transform,
                  float(x_min), float(x_max), float(y_min), float(y_max))
end

struct PolygonGate <: Gate
    x_channel::String
    y_channel::String
    x_transform::AxisTransform
    y_transform::AxisTransform
    vertices::Vector{Tuple{Float64,Float64}}   # transformed coords, in order
end

function PolygonGate(x_channel, y_channel, vertices;
                     x_transform::AxisTransform=LinearTransform(),
                     y_transform::AxisTransform=LinearTransform())
    verts = [(float(v[1]), float(v[2])) for v in vertices]
    PolygonGate(String(x_channel), String(y_channel), x_transform, y_transform, verts)
end

gate_channels(g::Gate) = (g.x_channel, g.y_channel)

# ── Membership ────────────────────────────────────────────────────────────────
# `inside(gate, x_raw, y_raw)` → BitVector. x_raw/y_raw are the RAW channel columns
# for `x_channel`/`y_channel`; the gate applies its own transforms.

function inside(g::RectangleGate, x_raw::AbstractVector, y_raw::AbstractVector)::BitVector
    length(x_raw) == length(y_raw) || error("inside: x and y length mismatch")
    out = BitVector(undef, length(x_raw))
    @inbounds for i in eachindex(x_raw)
        xt = apply_transform(g.x_transform, float(x_raw[i]))
        yt = apply_transform(g.y_transform, float(y_raw[i]))
        out[i] = (xt >= g.x_min) & (xt <= g.x_max) & (yt >= g.y_min) & (yt <= g.y_max)
    end
    out
end

function inside(g::PolygonGate, x_raw::AbstractVector, y_raw::AbstractVector)::BitVector
    length(x_raw) == length(y_raw) || error("inside: x and y length mismatch")
    out = BitVector(undef, length(x_raw))
    @inbounds for i in eachindex(x_raw)
        xt = apply_transform(g.x_transform, float(x_raw[i]))
        yt = apply_transform(g.y_transform, float(y_raw[i]))
        out[i] = point_in_polygon(xt, yt, g.vertices)
    end
    out
end

"""Ray-casting point-in-polygon (transformed coordinates)."""
function point_in_polygon(px::Real, py::Real, verts::Vector{Tuple{Float64,Float64}})::Bool
    n = length(verts)
    n < 3 && return false
    inside = false
    j = n
    @inbounds for i in 1:n
        xi, yi = verts[i]
        xj, yj = verts[j]
        if ((yi > py) != (yj > py)) && (px < (xj - xi) * (py - yi) / (yj - yi) + xi)
            inside = !inside
        end
        j = i
    end
    inside
end

# ── (De)serialisation — JSON-friendly Dicts, readable by Python ─────────────────

function gate_spec(g::RectangleGate)::Dict{String,Any}
    Dict{String,Any}(
        "kind" => "rectangle",
        "x_channel" => g.x_channel, "y_channel" => g.y_channel,
        "x_transform" => transform_spec(g.x_transform),
        "y_transform" => transform_spec(g.y_transform),
        "x_min" => g.x_min, "x_max" => g.x_max,
        "y_min" => g.y_min, "y_max" => g.y_max,
    )
end

function gate_spec(g::PolygonGate)::Dict{String,Any}
    Dict{String,Any}(
        "kind" => "polygon",
        "x_channel" => g.x_channel, "y_channel" => g.y_channel,
        "x_transform" => transform_spec(g.x_transform),
        "y_transform" => transform_spec(g.y_transform),
        "vertices" => [[x, y] for (x, y) in g.vertices],
    )
end

function gate_from_spec(spec::AbstractDict)::Gate
    g(k, default=nothing) = get(spec, k, get(spec, Symbol(k), default))
    kind = lowercase(string(g("kind", "")))
    xt = transform_from_spec(g("x_transform", Dict("kind" => "linear")))
    yt = transform_from_spec(g("y_transform", Dict("kind" => "linear")))
    if kind == "rectangle"
        RectangleGate(g("x_channel"), g("y_channel"),
                      g("x_min"), g("x_max"), g("y_min"), g("y_max");
                      x_transform=xt, y_transform=yt)
    elseif kind == "polygon"
        verts = [(float(v[1]), float(v[2])) for v in g("vertices")]
        PolygonGate(g("x_channel"), g("y_channel"), verts; x_transform=xt, y_transform=yt)
    elseif kind == "slice"
        SliceGate(g("x_channel"), g("y_channel"), g("vertices");
                  side = Int(something(g("side"), 1)), group = string(something(g("group"), "")),
                  x_transform=xt, y_transform=yt)
    elseif kind == "quadrant"
        QuadrantGate(g("x_channel"), g("y_channel"), g("centre"), g("arms");
                     quadrant = Int(something(g("quadrant"), 1)), group = string(something(g("group"), "")),
                     x_transform=xt, y_transform=yt)
    elseif kind == "range" || kind == "bisector"
        # 1D: one channel, named as both x and y (hist_gates.jl) — a different y can't be one axis
        xc = g("x_channel"); yc = something(g("y_channel"), xc)
        string(yc) == string(xc) || error("a $kind gate is on one channel: x_channel and y_channel must match")
        kind == "range" ? RangeGate(xc, g("lo"), g("hi"); x_transform = xt,
                                    lo_rule = rule_from_spec(g("lo_rule")), hi_rule = rule_from_spec(g("hi_rule"))) :
            BisectorGate(xc, g("cut"); side = Int(something(g("side"), 1)), group = string(something(g("group"), "")),
                         rule = rule_from_spec(g("rule")), x_transform = xt)
    else
        error("Unknown gate kind: $kind")
    end
end

# ── Projection for display ──────────────────────────────────────────────────────
# Re-express a gate's stored geometry (in its OWN transform space) into a target DISPLAY transform,
# oriented to the plot's (xcol, ycol). The plot's points arrive already in the display transform (see
# api plotdata), so a gate drawn under a DIFFERENT transform — e.g. drawn on logicle, now shown on a
# coerced/switched linear axis — must be re-projected or its outline lands nowhere near the dots. The
# client has no transform math (points/ticks are all server-computed), so this must happen here.
# Per-axis and monotone: stored → raw (invert the gate's transform) → display (apply the target). Axes
# transform independently, so a rectangle stays a rectangle; polygon vertices map pointwise (edges
# approximated by straight segments — fine for a gating outline). Returns a JSON-ready Dict in DISPLAY
# coords, or `nothing` if the gate isn't on this channel pair (either order). Membership is untouched —
# this is purely for rendering the outline aligned with the displayed points.
function project_gate(g::Gate, xcol::AbstractString, ycol::AbstractString,
                      xt::AxisTransform, yt::AxisTransform)
    gx, gy = g.x_channel, g.y_channel
    direct = (gx == xcol && gy == ycol)
    swap   = (gx == ycol && gy == xcol)
    (direct || swap) || return nothing
    # a stored point (on the gate's own x/y axes) → the plot's (x,y) display coordinates
    to_disp(vgx, vgy) = begin
        rgx = invert_transform(g.x_transform, vgx)          # raw value on the gate's x-channel
        rgy = invert_transform(g.y_transform, vgy)          # raw value on the gate's y-channel
        swap ? (apply_transform(xt, rgy), apply_transform(yt, rgx)) :   # gate x ↦ plot Y, gate y ↦ plot X
               (apply_transform(xt, rgx), apply_transform(yt, rgy))
    end
    _projected(g, to_disp, swap)
end

function _projected(g::RectangleGate, to_disp, swap)
    (ax, ay) = to_disp(g.x_min, g.y_min)
    (bx, by) = to_disp(g.x_max, g.y_max)
    Dict{String,Any}("kind" => "rectangle",
        "x_min" => min(ax, bx), "x_max" => max(ax, bx),
        "y_min" => min(ay, by), "y_max" => max(ay, by))
end
# Map corners 1:1. A cross-transform view (gate stored under transform A, shown under B) draws
# edges straight in the DISPLAY transform, not the gate's — a mild outline drift near a curve.
# Membership is untouched (`inside` uses the stored transform). Never densify: the client sends
# this list back on any handle drag, and a densified round-trip would silently multiply the
# vertex count on every edit until the gate is un-editable (12× per cross-transform round-trip).
_projected(g::PolygonGate, to_disp, swap) =
    Dict{String,Any}("kind" => "polygon", "vertices" => [collect(to_disp(vx, vy)) for (vx, vy) in g.vertices])
