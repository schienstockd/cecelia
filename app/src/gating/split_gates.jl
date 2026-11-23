# ── Split gates: one divider, several populations ─────────────────────────────
#
# A rectangle or polygon encloses ONE population. A split gate divides the whole plane, and every region
# is its own population:
#   SliceGate     a dividing line (2+ vertices, so it may bend). The ends extend to infinity, so the
#                 plane falls into the side to the LEFT of the line's direction (`side = +1`) and the
#                 side to its right (`side = -1`).
#   QuadrantGate  a centre and four arms (FlowJo's quadrant; tilted arms are its "spider" variant). The
#                 arms are rays, listed counter-clockwise; region `q` is the sector from arm `q` to arm
#                 `q+1`. Axis-aligned arms [right, up, left, down] make regions 1–4 = ++, -+, --, +-.
#
# Each region's population stores the SAME divider plus its own region (`side` / `quadrant`), and the
# populations of one split share a `group` id. `set_gate!` moves the divider of every group member at
# once (mutations.jl), so the regions always tile the plane: every cell with finite values on both axes
# lands in exactly one of them. A cell with a NaN/Inf on either axis lands in none (rectangle/polygon do
# the same). Geometry is in TRANSFORMED coordinates, like every gate (docs/POPULATION.md → Split gates).

struct SliceGate <: Gate
    x_channel::String
    y_channel::String
    x_transform::AxisTransform
    y_transform::AxisTransform
    vertices::Vector{Tuple{Float64,Float64}}   # the dividing line, in drawing order
    side::Int                                   # +1 left of the line's direction, -1 right
    group::String
end

function SliceGate(x_channel, y_channel, vertices; side::Integer = 1, group::AbstractString = "",
                   x_transform::AxisTransform = LinearTransform(),
                   y_transform::AxisTransform = LinearTransform())
    verts = [(float(v[1]), float(v[2])) for v in vertices]
    length(verts) >= 2 || error("a slice needs at least 2 vertices")
    all(v -> isfinite(v[1]) && isfinite(v[2]), verts) || error("a slice's vertices must be finite")
    any(i -> verts[i] != verts[i+1], 1:length(verts)-1) || error("a slice's vertices are all the same point")
    side in (1, -1) || error("a slice's side must be 1 or -1")
    SliceGate(String(x_channel), String(y_channel), x_transform, y_transform, verts, Int(side), String(group))
end

struct QuadrantGate <: Gate
    x_channel::String
    y_channel::String
    x_transform::AxisTransform
    y_transform::AxisTransform
    centre::Tuple{Float64,Float64}
    arms::Vector{Tuple{Float64,Float64}}       # a point on each of the 4 rays, counter-clockwise
    quadrant::Int                               # 1..4 — the sector from arm q to arm q+1
    group::String
end

function QuadrantGate(x_channel, y_channel, centre, arms; quadrant::Integer = 1, group::AbstractString = "",
                      x_transform::AxisTransform = LinearTransform(),
                      y_transform::AxisTransform = LinearTransform())
    c = (float(centre[1]), float(centre[2]))
    as = [(float(a[1]), float(a[2])) for a in arms]
    length(as) == 4 || error("a quadrant gate needs 4 arms")
    all(p -> isfinite(p[1]) && isfinite(p[2]), [c; as]) || error("a quadrant gate's points must be finite")
    any(==(c), as) && error("a quadrant arm can't end at the centre")
    _arms_ccw(c, as) || error("a quadrant gate's arms must go round counter-clockwise without crossing")
    1 <= quadrant <= 4 || error("a quadrant must be 1–4")
    QuadrantGate(String(x_channel), String(y_channel), x_transform, y_transform, c, as, Int(quadrant), String(group))
end

QuadrantGate(x_channel, y_channel, x0::Real, y0::Real; kw...) =
    QuadrantGate(x_channel, y_channel, (x0, y0), [(x0 + 1, y0), (x0, y0 + 1), (x0 - 1, y0), (x0, y0 - 1)]; kw...)

const SplitGate = Union{SliceGate,QuadrantGate}

_arm_angles(c, arms) = [atan(a[2] - c[2], a[1] - c[1]) for a in arms]

# The arms go round once, counter-clockwise: the CCW steps between consecutive arms sum to one full turn
# (a crossed order sums to two or more). No two arms on the same ray.
function _arms_ccw(c, arms)::Bool
    θ = _arm_angles(c, arms)
    steps = [mod(θ[mod1(k + 1, 4)] - θ[k], 2π) for k in 1:4]
    all(s -> s > 1e-9, steps) && isapprox(sum(steps), 2π; atol = 1e-6)
end

"""The sector (1–4) of `gq`'s plane that the TRANSFORMED point (x, y) falls in."""
function quadrant_of(g::QuadrantGate, x::Real, y::Real)::Int
    θ = _arm_angles(g.centre, g.arms)
    rel = mod(atan(y - g.centre[2], x - g.centre[1]) - θ[1], 2π)
    q = 1
    for k in 2:4
        mod(θ[k] - θ[1], 2π) <= rel && (q = k)
    end
    q
end

# ── Plane geometry shared by membership, display and the gate-plot image ──────
# A box (xlo, xhi, ylo, yhi) stands in for the infinite plane: a ray is cut where it leaves the box, and
# a region is closed by walking the box border. The caller makes the box contain every vertex.

# where the ray p + t·d (t > 0) leaves the box
function _ray_exit(p, d, box)
    xlo, xhi, ylo, yhi = box
    t = Inf
    d[1] > 0 && (t = min(t, (xhi - p[1]) / d[1])); d[1] < 0 && (t = min(t, (xlo - p[1]) / d[1]))
    d[2] > 0 && (t = min(t, (yhi - p[2]) / d[2])); d[2] < 0 && (t = min(t, (ylo - p[2]) / d[2]))
    isfinite(t) ? (p[1] + t * d[1], p[2] + t * d[2]) : p
end

# position of a border point counter-clockwise round the box: bottom 0–1, right 1–2, top 2–3, left 3–4
function _border_pos(q, box)
    xlo, xhi, ylo, yhi = box
    w = max(xhi - xlo, eps()); h = max(yhi - ylo, eps())
    ϵ = 1e-9 * max(w, h)
    abs(q[2] - ylo) <= ϵ && q[1] < xhi - ϵ && return (q[1] - xlo) / w
    abs(q[1] - xhi) <= ϵ && q[2] < yhi - ϵ && return 1 + (q[2] - ylo) / h
    abs(q[2] - yhi) <= ϵ && q[1] > xlo + ϵ && return 2 + (xhi - q[1]) / w
    3 + (yhi - q[2]) / h
end

# the box corners passed walking counter-clockwise from border point `a` to border point `b`
function _border_walk(a, b, box)
    xlo, xhi, ylo, yhi = box
    corners = ((xhi, ylo), (xhi, yhi), (xlo, yhi), (xlo, ylo))   # at positions 1, 2, 3, 4
    sa = _border_pos(a, box); sb = _border_pos(b, box)
    sb <= sa && (sb += 4)
    [corners[mod1(k, 4)] for k in ceil(Int, sa + 1e-12):floor(Int, sb - 1e-12) if sa < k < sb]
end

function _extended_slice(verts, box)
    entry = _ray_exit(verts[1], verts[1] .- _next_distinct(verts, 1, 1), box)
    exit  = _ray_exit(verts[end], verts[end] .- _next_distinct(verts, length(verts), -1), box)
    entry, exit
end
# the first vertex after `i` (stepping `dir`) that differs from it — a doubled vertex has no direction
function _next_distinct(verts, i, dir)
    j = i + dir
    while 1 <= j <= length(verts) && verts[j] == verts[i]; j += dir; end
    verts[clamp(j, 1, length(verts))]
end

"""The region LEFT of a slice line, closed round `box`, as a polygon."""
function slice_left_polygon(verts::AbstractVector, box)::Vector{Tuple{Float64,Float64}}
    entry, exit = _extended_slice(verts, box)
    [entry; collect(verts); exit; _border_walk(exit, entry, box)]
end

"""Region `q` of a quadrant gate (centre + 4 CCW arms), closed round `box`, as a polygon."""
function quadrant_polygon(c, arms::AbstractVector, q::Integer, box)::Vector{Tuple{Float64,Float64}}
    a = _ray_exit(c, arms[q] .- c, box); b = _ray_exit(c, arms[mod1(q + 1, 4)] .- c, box)
    [c; a; _border_walk(a, b, box); b]
end

"""The divider's segments inside `box`: the slice line with both ends run out to the border, or the
centre-to-border arms of a quadrant. For drawing and for edge distance; the box must hold every vertex."""
function divider_segments(g::SliceGate, box)
    entry, exit = _extended_slice(g.vertices, box)
    pts = [entry; g.vertices; exit]
    [(pts[i], pts[i+1]) for i in 1:length(pts)-1]
end
divider_segments(g::QuadrantGate, box) = [(g.centre, _ray_exit(g.centre, a .- g.centre, box)) for a in g.arms]

# a box holding the given points and finite values, padded on every side by its own span
function _plane_box(xs, ys, pts)
    fx = [x for x in xs if isfinite(x)]; fy = [y for y in ys if isfinite(y)]
    append!(fx, first.(pts)); append!(fy, last.(pts))
    xlo, xhi = extrema(fx); ylo, yhi = extrema(fy)
    px = max(xhi - xlo, 1.0); py = max(yhi - ylo, 1.0)
    (xlo - px, xhi + px, ylo - py, yhi + py)
end
_divider_points(g::SliceGate) = g.vertices
_divider_points(g::QuadrantGate) = [g.centre; g.arms]

# ── Membership ────────────────────────────────────────────────────────────────
function inside(g::SliceGate, x_raw::AbstractVector, y_raw::AbstractVector)::BitVector
    length(x_raw) == length(y_raw) || error("inside: x and y length mismatch")
    xt = Float64[apply_transform(g.x_transform, float(v)) for v in x_raw]
    yt = Float64[apply_transform(g.y_transform, float(v)) for v in y_raw]
    poly = slice_left_polygon(g.vertices, _plane_box(xt, yt, g.vertices))
    want_left = g.side == 1
    # the right side is "finite and NOT left", never a second polygon test — so the two sides are exact
    # complements even for a bent line whose extension crosses itself
    BitVector(isfinite(xt[i]) && isfinite(yt[i]) && (point_in_polygon(xt[i], yt[i], poly) == want_left)
              for i in eachindex(xt))
end

function inside(g::QuadrantGate, x_raw::AbstractVector, y_raw::AbstractVector)::BitVector
    length(x_raw) == length(y_raw) || error("inside: x and y length mismatch")
    out = BitVector(undef, length(x_raw))
    @inbounds for i in eachindex(x_raw)
        xt = apply_transform(g.x_transform, float(x_raw[i]))
        yt = apply_transform(g.y_transform, float(y_raw[i]))
        out[i] = isfinite(xt) && isfinite(yt) && quadrant_of(g, xt, yt) == g.quadrant
    end
    out
end

# ── Group edits ───────────────────────────────────────────────────────────────
# Swapping the axes mirrors the plane: left becomes right, and counter-clockwise arms go clockwise (so
# their order reverses and sector q becomes sector 4-q). Every reader that swaps axes goes through this.
mirror_region(g::SliceGate) = -g.side
mirror_region(g::QuadrantGate) = mod1(4 - g.quadrant, 4)
mirror_arms(arms) = reverse(arms)

"""`template`'s divider with `g`'s own region — what a group member becomes when the group's divider is
moved through `template`. The region is carried across an axis swap (`mirror_region`)."""
function with_divider(g::SliceGate, template::SliceGate)
    side = g.x_channel == template.x_channel ? g.side : -g.side
    SliceGate(template.x_channel, template.y_channel, template.x_transform, template.y_transform,
              template.vertices, side, template.group)
end
function with_divider(g::QuadrantGate, template::QuadrantGate)
    q = g.x_channel == template.x_channel ? g.quadrant : mod1(4 - g.quadrant, 4)
    QuadrantGate(template.x_channel, template.y_channel, template.x_transform, template.y_transform,
                 template.centre, template.arms, q, template.group)
end
with_divider(g::Gate, ::Gate) = nothing

"""`gate_spec` without the split gate's `group` — an id linking its regions, not part of the shape. What a
hash or a cross-image comparison of a gate's DEFINITION reads (the gate record, claim checks): the same
slice drawn twice must compare equal. Rectangle/polygon specs carry no `group`, so theirs is unchanged."""
gate_shape_spec(g::Gate)::Dict{String,Any} = filter(kv -> kv.first != "group", gate_spec(g))

gate_group(g::SplitGate) = g.group
gate_group(::Gate) = ""

# ── (De)serialisation + display projection (gate_from_spec / project_gate live in gates.jl) ──
function gate_spec(g::SliceGate)::Dict{String,Any}
    Dict{String,Any}(
        "kind" => "slice",
        "x_channel" => g.x_channel, "y_channel" => g.y_channel,
        "x_transform" => transform_spec(g.x_transform),
        "y_transform" => transform_spec(g.y_transform),
        "vertices" => [[x, y] for (x, y) in g.vertices],
        "side" => g.side, "group" => g.group,
    )
end

function gate_spec(g::QuadrantGate)::Dict{String,Any}
    Dict{String,Any}(
        "kind" => "quadrant",
        "x_channel" => g.x_channel, "y_channel" => g.y_channel,
        "x_transform" => transform_spec(g.x_transform),
        "y_transform" => transform_spec(g.y_transform),
        "centre" => collect(g.centre),
        "arms" => [[x, y] for (x, y) in g.arms],
        "quadrant" => g.quadrant, "group" => g.group,
    )
end

# Split gates: the same pointwise map. Each axis transform is increasing, so only an axis SWAP changes
# the plane's handedness — then the region is mirrored (`mirror_region`, split_gates.jl).
_projected(g::SliceGate, to_disp, swap) =
    Dict{String,Any}("kind" => "slice", "vertices" => [collect(to_disp(vx, vy)) for (vx, vy) in g.vertices],
                     "side" => swap ? mirror_region(g) : g.side, "group" => g.group)
function _projected(g::QuadrantGate, to_disp, swap)
    arms = [collect(to_disp(ax, ay)) for (ax, ay) in g.arms]
    Dict{String,Any}("kind" => "quadrant", "centre" => collect(to_disp(g.centre...)),
                     "arms" => swap ? mirror_arms(arms) : arms,
                     "quadrant" => swap ? mirror_region(g) : g.quadrant, "group" => g.group)
end

# ── Display: a projected split gate (`project_gate`'s Dict) ───────────────────
"""
    split_outline(pj, box) -> (dividers, region)

For a `project_gate` Dict of kind slice / quadrant, in display coordinates: the divider's segments and
the polygon of this population's region, both run out to `box` (xlo, xhi, ylo, yhi), widened to hold
the divider's points. The caller clips. Used by the gate-plot image; the browser has its twin in
`frontend/src/plots/splitGeometry.ts`.
"""
function split_outline(pj::AbstractDict, box)
    pt(v) = (float(v[1]), float(v[2]))
    g = if get(pj, "kind", "") == "slice"
        SliceGate("x", "y", [pt(v) for v in pj["vertices"]]; side = Int(pj["side"]))
    else
        QuadrantGate("x", "y", pt(pj["centre"]), [pt(a) for a in pj["arms"]]; quadrant = Int(pj["quadrant"]))
    end
    # widened to hold the divider's points STRICTLY inside: a centre on the border gives an arm no length,
    # and the region would close round the whole border (splitGeometry.ts `holding`, same rule)
    pts = _divider_points(g)
    px = 0.05 * max(box[2] - box[1], 1e-9); py = 0.05 * max(box[4] - box[3], 1e-9)
    b = (min(box[1], minimum(first, pts) - px), max(box[2], maximum(first, pts) + px),
         min(box[3], minimum(last, pts) - py),  max(box[4], maximum(last, pts) + py))
    region = g isa QuadrantGate ? quadrant_polygon(g.centre, g.arms, g.quadrant, b) :
             g.side == 1 ? slice_left_polygon(g.vertices, b) : slice_left_polygon(reverse(g.vertices), b)
    divider_segments(g, b), region
end

# A polygon cut to the box (Sutherland–Hodgman; the box is convex, so one pass per edge).
function _clip_to_box(poly::AbstractVector, box)
    xlo, xhi, ylo, yhi = box
    lerp(a, c, t) = (a[1] + t * (c[1] - a[1]), a[2] + t * (c[2] - a[2]))
    edges = ((p -> p[1] >= xlo, (a, c) -> lerp(a, c, (xlo - a[1]) / (c[1] - a[1]))),
             (p -> p[1] <= xhi, (a, c) -> lerp(a, c, (xhi - a[1]) / (c[1] - a[1]))),
             (p -> p[2] >= ylo, (a, c) -> lerp(a, c, (ylo - a[2]) / (c[2] - a[2]))),
             (p -> p[2] <= yhi, (a, c) -> lerp(a, c, (yhi - a[2]) / (c[2] - a[2]))))
    out = Tuple{Float64,Float64}[(float(p[1]), float(p[2])) for p in poly]
    for (ins, cut) in edges
        src = out; out = Tuple{Float64,Float64}[]
        for i in eachindex(src)
            p = src[i]; prev = src[mod1(i - 1, length(src))]
            if ins(p)
                ins(prev) || push!(out, cut(prev, p)); push!(out, p)
            elseif ins(prev)
                push!(out, cut(prev, p))
            end
        end
        isempty(out) && break
    end
    out
end

"""Where a split population's name goes on the gate-plot image: the area centroid of its region's part
inside `box`, or `nothing` when none of it is in view. The browser's twin is `splitLabelPoint`."""
function split_label_point(pj::AbstractDict, box)
    _, region = split_outline(pj, box)
    p = _clip_to_box(region, box)
    length(p) < 3 && return nothing
    a = cx = cy = 0.0
    for i in eachindex(p)
        q = p[i]; r = p[mod1(i + 1, length(p))]; c = q[1] * r[2] - r[1] * q[2]
        a += c; cx += (q[1] + r[1]) * c; cy += (q[2] + r[2]) * c
    end
    abs(a) < 1e-12 && return nothing
    (cx / (3a), cy / (3a))
end
