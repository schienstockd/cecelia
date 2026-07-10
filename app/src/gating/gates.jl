# ── Gates ─────────────────────────────────────────────────────────────────────
#
# A gate is self-contained: it names its two channels, carries the per-axis transform,
# and stores its geometry in TRANSFORMED coordinates. Membership = transform the raw
# channel values, then test geometry in transformed space (docs/POPULATION.md).
# Rectangle + polygon now; the abstract `Gate` leaves room for ellipse/quadrant/boolean.

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
    if g isa RectangleGate
        (ax, ay) = to_disp(g.x_min, g.y_min)
        (bx, by) = to_disp(g.x_max, g.y_max)
        Dict{String,Any}("kind" => "rectangle",
            "x_min" => min(ax, bx), "x_max" => max(ax, bx),
            "y_min" => min(ay, by), "y_max" => max(ay, by))
    else
        Dict{String,Any}("kind" => "polygon",
            "vertices" => [collect(to_disp(vx, vy)) for (vx, vy) in g.vertices])
    end
end
