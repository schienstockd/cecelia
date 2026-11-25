# ── 1D gates: drawn on one channel's histogram ────────────────────────────────
#
#   RangeGate     an interval [lo, hi] — one population. Either end may follow its own valley rule
#                 (`lo_rule` / `hi_rule`): a middle population between two dips.
#   BisectorGate  one cut that splits the axis in two: `side = +1` is above the cut (the `+` population),
#                 `side = -1` at or below it. Both halves share a `group`, like a slice's two sides, so
#                 moving the cut of one moves the other (`set_gate!`) and every finite cell lands in
#                 exactly one of them.
#
# A 1D gate stores its channel as BOTH `x_channel` and `y_channel` (and its transform as both): the
# engine's two-channel plumbing — the columns it fetches, the missing-column guard, plotmeta matching a
# gate to a plot — then carries it unchanged, and it is drawn on the histogram, whose plot asks for
# x = y = that channel. Geometry is in TRANSFORMED coordinates, like every gate.
#
# A bisector's cut, or a range's end, may follow a RULE instead of a fixed position: `ValleyRule(lo, hi)`
# puts it at the deepest valley of the parent's density between lo and hi, re-found on every image (and
# whenever the parent's cells change). The stored position is then only the fallback for a parent with no
# valley in the window.
# docs/POPULATION.md → 1D gates.

using Statistics: quantile, std

"""Cut at the most prominent density valley between `lo` and `hi` (transformed units). `adjust` scales
the kernel bandwidth (> 1 smooths more)."""
struct ValleyRule
    lo::Float64
    hi::Float64
    adjust::Float64
end
function ValleyRule(lo::Real, hi::Real; adjust::Real = 1.0)
    (isfinite(lo) && isfinite(hi)) || error("a valley rule's window must be finite")
    adjust > 0 || error("a valley rule's adjust must be positive")
    a, b = minmax(float(lo), float(hi))
    ValleyRule(a, b, float(adjust))
end

struct RangeGate <: Gate
    x_channel::String
    y_channel::String                   # == x_channel
    x_transform::AxisTransform
    y_transform::AxisTransform          # == x_transform
    lo::Float64
    hi::Float64
    lo_rule::Union{Nothing,ValleyRule}  # each end may follow its own valley (its stored value the fallback)
    hi_rule::Union{Nothing,ValleyRule}
end

function RangeGate(channel, lo, hi; x_transform::AxisTransform = LinearTransform(),
                   lo_rule::Union{Nothing,ValleyRule} = nothing, hi_rule::Union{Nothing,ValleyRule} = nothing)
    (isfinite(lo) && isfinite(hi)) || error("a range gate's ends must be finite")
    a, b = minmax(float(lo), float(hi))
    RangeGate(String(channel), String(channel), x_transform, x_transform, a, b, lo_rule, hi_rule)
end

struct BisectorGate <: Gate
    x_channel::String
    y_channel::String                   # == x_channel
    x_transform::AxisTransform
    y_transform::AxisTransform          # == x_transform
    cut::Float64                        # the cut, or a rule's fallback
    side::Int                           # +1 above the cut, -1 at or below it
    group::String
    rule::Union{Nothing,ValleyRule}
end

function BisectorGate(channel, cut; side::Integer = 1, group::AbstractString = "",
                      rule::Union{Nothing,ValleyRule} = nothing,
                      x_transform::AxisTransform = LinearTransform())
    isfinite(cut) || error("a bisector's cut must be finite")
    side in (1, -1) || error("a bisector's side must be 1 or -1")
    BisectorGate(String(channel), String(channel), x_transform, x_transform, float(cut), Int(side),
                 String(group), rule)
end

const HistGate = Union{RangeGate,BisectorGate}

# ── Membership ────────────────────────────────────────────────────────────────
# `y_raw` is the same column as `x_raw` (the gate names its channel twice); only x is read.
# (`inside_t` takes the transformed column twice, the way `inside` takes the raw one twice.)
inside(g::Union{RangeGate,BisectorGate}, x_raw::AbstractVector, ::AbstractVector)::BitVector =
    (xt = transform_column(g.x_transform, x_raw); inside_t(g, xt, xt))

function inside_t(g::RangeGate, xt::AbstractVector{Float64}, ::AbstractVector{Float64})::BitVector
    out = BitVector(undef, length(xt))
    @inbounds for i in eachindex(xt, out)
        v = xt[i]
        out[i] = (v >= g.lo) & (v <= g.hi)
    end
    out
end

function inside_t(g::BisectorGate, xt::AbstractVector{Float64}, ::AbstractVector{Float64})::BitVector
    out = BitVector(undef, length(xt))
    @inbounds for i in eachindex(xt, out)
        v = xt[i]
        out[i] = isfinite(v) && (g.side == 1 ? v > g.cut : v <= g.cut)
    end
    out
end

# ── Rules: resolved against the parent's cells ────────────────────────────────
"""Whether `g`'s membership depends on the parent's distribution (a rule), so the caller must
`resolve_gate` it with the parent's values first."""
needs_parent_data(::Gate) = false
needs_parent_data(g::BisectorGate) = g.rule !== nothing
needs_parent_data(g::RangeGate) = g.lo_rule !== nothing || g.hi_rule !== nothing

"""The rule's cut on these RAW parent values, or `nothing` (no rule, or no valley in its window)."""
resolve_cut(::Gate, raw) = nothing
function resolve_cut(g::BisectorGate, raw::AbstractVector)
    g.rule === nothing && return nothing
    _rule_cut(g.rule, apply_transform(g.x_transform, raw))
end
_rule_cut(r::Nothing, v) = nothing
_rule_cut(r::ValleyRule, v) = valley_cut(v, r.lo, r.hi; adjust = r.adjust)

"""For a display: whether each of `g`'s rules found its valley on these RAW parent values — `rule_ok` for a
bisector, `lo_rule_ok` / `hi_rule_ok` for a range's ruled ends; empty for a gate without rules."""
rule_flags(::Gate, raw) = Dict{String,Any}()
rule_flags(g::BisectorGate, raw::AbstractVector) =
    g.rule === nothing ? Dict{String,Any}() : Dict{String,Any}("rule_ok" => resolve_cut(g, raw) !== nothing)
function rule_flags(g::RangeGate, raw::AbstractVector)
    _, _, lo_ok, hi_ok = _resolved_ends(g, apply_transform(g.x_transform, raw))
    out = Dict{String,Any}()
    g.lo_rule === nothing || (out["lo_rule_ok"] = lo_ok)
    g.hi_rule === nothing || (out["hi_rule_ok"] = hi_ok)
    out
end

# A range's ends on transformed values `v` and whether each ruled end's valley is the one used: each moves to
# its valley inside its `end_window` or keeps its stored value; a found end past the other's stored value
# (that one missed) is not used — both stored ends stand, both flagged. One answer for membership and display.
function _resolved_ends(g::RangeGate, v)
    a, b = _end_cut(g, :lo, v), _end_cut(g, :hi, v)
    lo, hi = something(a, g.lo), something(b, g.hi)
    lo <= hi || return (g.lo, g.hi, false, false)
    (lo, hi, a !== nothing, b !== nothing)
end

# The window a range end's rule actually searches, or `nothing` (no rule there). Where the two ends' windows
# overlap they split the overlap at its middle, so both ends can't land in one dip; against a fixed other
# end the window stops at that end, so a ruled end can't cross it. The client draws the same windows
# (plots/histGeometry.ts `histWindows`).
function end_window(g::RangeGate, e::Symbol)
    r = e === :lo ? g.lo_rule : g.hi_rule
    r === nothing && return nothing
    a, b = r.lo, r.hi
    if e === :lo
        o = g.hi_rule
        b = o === nothing ? min(b, g.hi) : o.lo < b ? (o.lo + b) / 2 : b
    else
        o = g.lo_rule
        a = o === nothing ? max(a, g.lo) : a < o.hi ? (a + o.hi) / 2 : a
    end
    (a, b)
end

# A range end's valley on transformed values `v` inside its `end_window`, or `nothing`.
function _end_cut(g::RangeGate, e::Symbol, v)
    w = end_window(g, e)
    (w === nothing || !(w[2] > w[1])) && return nothing
    r = e === :lo ? g.lo_rule : g.hi_rule
    valley_cut(v, w[1], w[2]; adjust = r.adjust)
end

"""`g` with its rule applied to the RAW parent values: a bisector's cut moves to the valley; with no
valley in the window (or no rule) the stored cut stands. Any other gate is returned as is."""
resolve_gate(g::Gate, raw) = g
function resolve_gate(g::BisectorGate, raw::AbstractVector)
    c = resolve_cut(g, raw)
    c === nothing ? g : BisectorGate(g.x_channel, g.y_channel, g.x_transform, g.y_transform, c, g.side, g.group, g.rule)
end
# The two-column form every caller uses (a rectangle / polygon's follow rule reads both axes — follow_rule.jl);
# a 1D gate names its channel twice, so it reads x alone.
resolve_gate(g::Gate, xraw, yraw) = g
resolve_gate(g::HistGate, xraw::AbstractVector, yraw) = resolve_gate(g, xraw)
rule_flags(g::Gate, xraw, yraw) = Dict{String,Any}()
rule_flags(g::HistGate, xraw::AbstractVector, yraw) = rule_flags(g, xraw)
# both at once, for a display (a follow rule computes its density once — follow_rule.jl)
resolve_with_flags(g::Gate, xraw, yraw) = (resolve_gate(g, xraw, yraw), rule_flags(g, xraw, yraw))
# each ruled end moves to its valley inside its `end_window`, or keeps its stored value
function resolve_gate(g::RangeGate, raw::AbstractVector)
    needs_parent_data(g) || return g
    v = apply_transform(g.x_transform, raw)
    lo, hi = _resolved_ends(g, v)
    RangeGate(g.x_channel, g.y_channel, g.x_transform, g.y_transform, lo, hi, g.lo_rule, g.hi_rule)
end

const VALLEY_GRID = 512        # density evaluation points, at least (more when the span needs them)
const VALLEY_GRID_MAX = 1 << 16
const VALLEY_RANGE_Q = 0.001   # the grid spans these quantiles (∪ the window), not the extremes
const VALLEY_MIN_CELLS = 20    # fewer finite values than this → no density, no valley
const VALLEY_MIN_PEAK = 0.05   # the smaller of the two peaks, as a fraction of the tallest
const VALLEY_MIN_DEPTH = 0.2   # the dip below the smaller peak, as a fraction of it

"""
    valley_cut(v, lo, hi; adjust = 1.0) -> Union{Float64,Nothing}

The cut between two populations on one axis: the most prominent valley of a Gaussian kernel density of
the finite values `v` whose position lies in [lo, hi]. Prominence = the lower of the highest density on
either side of it minus the density at it — so a ripple in a sparse tail, however low, loses to the dip
between two real peaks. A dip only counts when both sides are a population: the lower side's peak at
least 5 % of the tallest, and the dip at least 20 % below that peak (`VALLEY_MIN_PEAK`/`_DEPTH`, set on
multiplex tissue images where a lone population's sparse tail otherwise took ~1 % of its cells). A
flat-bottomed valley (an empty gap between two clouds) cuts at its middle. `nothing` when no valley
lies in the window (one population there) or there are too few values.

The approach is openCyto's `mindensity` (Finak et al. 2014, PLoS Comput Biol 10:e1003806,
doi:10.1371/journal.pcbi.1003806), written from that description — openCyto is AGPL, nothing is copied.
Bandwidth: Silverman's rule of thumb, 0.9·min(sd, IQR/1.34)·n^(-1/5), times `adjust`; the density is a
binned KDE (linear binning onto the grid, then the Gaussian kernel), exact enough for a cut and O(n).
"""
function valley_cut(v::AbstractVector, lo::Real, hi::Real; adjust::Real = 1.0)
    x = Float64[a for a in v if isfinite(a)]
    n = length(x)
    n < VALLEY_MIN_CELLS && return nothing
    # one sorted copy for every quantile: each unsorted `quantile` call copies and sorts the column again
    # (~30 ms apiece on a 600k-cell parent). The density below still reads `x` in table order.
    xs = sort(x)
    xmin, xmax = xs[1], xs[end]
    xmax > xmin || return nothing
    q1, q3 = quantile(xs, (0.25, 0.75); sorted = true)
    sd = std(x)
    spread = q3 > q1 ? min(sd, (q3 - q1) / 1.34) : sd
    h = 0.9 * spread * n^(-1 / 5) * adjust
    h > 0 || return nothing
    # The grid spans the bulk of the data (0.1–99.9 %) and the window, inside the data — not the
    # extremes: one debris object far out on a linear axis stretched a fixed grid until its spacing
    # exceeded the bandwidth and the valley moved or vanished. It is also fine enough for the
    # bandwidth wherever the window reaches (≥ 4 points per h, capped).
    wlo, whi = minmax(float(lo), float(hi))
    qa, qb = quantile(xs, (VALLEY_RANGE_Q, 1 - VALLEY_RANGE_Q); sorted = true)
    a = max(xmin, min(qa, wlo)) - 3h
    b = min(xmax, max(qb, whi)) + 3h
    G = clamp(ceil(Int, 4 * (b - a) / h), VALLEY_GRID, VALLEY_GRID_MAX)
    d, a, dx = _binned_kde(x, a, b, h, G)
    _most_prominent_valley(d, a, dx, wlo, whi)
end

# values outside [a, b] are left out (not piled onto the edge bins, which would make a false peak)
function _binned_kde(x::Vector{Float64}, a::Float64, b::Float64, h::Float64, G::Int = VALLEY_GRID)
    dx = (b - a) / (G - 1)
    counts = zeros(G)
    for xi in x
        a <= xi <= b || continue
        t = (xi - a) / dx
        i = clamp(floor(Int, t), 0, G - 2)
        f = t - i
        counts[i+1] += 1 - f
        counts[i+2] += f
    end
    r = ceil(Int, 4h / dx)
    k = [exp(-0.5 * (j * dx / h)^2) for j in -r:r]
    d = zeros(G)
    @inbounds for i in 1:G, j in -r:r
        0 < i + j <= G && (d[i] += counts[i+j] * k[j+r+1])
    end
    d, a, dx
end

# Valleys = runs of equal density lower than both neighbouring runs. Each is scored by its prominence;
# the best one whose middle lies in [lo, hi] wins.
function _most_prominent_valley(d::Vector{Float64}, a, dx, lo, hi)
    G = length(d)
    dmax = maximum(d)
    tol = 1e-12 * dmax
    runs = Tuple{Int,Int}[]                       # (first, last) index of each run of equal values
    s = 1
    for i in 2:G+1
        if i > G || abs(d[i] - d[s]) > tol
            push!(runs, (s, i - 1)); s = i
        end
    end
    best, best_prom = nothing, 0.0
    for k in 2:length(runs)-1
        (s, e) = runs[k]
        dv = d[s]
        (d[runs[k-1][1]] > dv && d[runs[k+1][1]] > dv) || continue
        pos = a + ((s + e) / 2 - 1) * dx
        lo <= pos <= hi || continue
        side_peak = min(maximum(view(d, 1:s)), maximum(view(d, e:G)))
        # both sides must be a population: the lower flanking peak at least VALLEY_MIN_PEAK of the
        # tallest, and the dip at least VALLEY_MIN_DEPTH below it — else it is a ripple in a tail or a
        # shoulder, and one population has no valley
        (side_peak >= VALLEY_MIN_PEAK * dmax && dv <= (1 - VALLEY_MIN_DEPTH) * side_peak) || continue
        prom = side_peak - dv
        if prom > best_prom
            best, best_prom = pos, prom
        end
    end
    best
end

# ── Group edits, spec, display ────────────────────────────────────────────────
# One axis, so no swap can mirror it: a group member keeps its side as is.
with_divider(g::BisectorGate, t::BisectorGate) =
    BisectorGate(t.x_channel, t.y_channel, t.x_transform, t.y_transform, t.cut, g.side, t.group, t.rule)
gate_group(g::BisectorGate) = g.group

rule_spec(r::ValleyRule) = Dict{String,Any}("kind" => "valley", "lo" => r.lo, "hi" => r.hi, "adjust" => r.adjust)
"""A gate's stored `rule` → a `ValleyRule` (a 1D gate's) or a `FollowRule` (a rectangle / polygon's —
follow_rule.jl), or `nothing`. `want` is the rule type the gate takes; another kind is refused by name."""
function rule_from_spec(spec, want::Type = ValleyRule)
    spec === nothing && return nothing
    g(k, default = nothing) = get(spec, k, get(spec, Symbol(k), default))
    kind = lowercase(string(g("kind", "")))
    r = kind == "valley" ? ValleyRule(g("lo"), g("hi"); adjust = something(g("adjust"), 1.0)) :
        kind == "follow" ? _follow_rule_from_spec(g) : error("Unknown gate rule: $kind")
    r isa want || error(want === FollowRule ?
        "A rectangle or polygon's rule is {\"kind\": \"follow\"}, not \"$kind\"" :
        "A range or bisector's rule is {\"kind\": \"valley\", …}, not \"$kind\"")
    r
end

function _hist_spec(g::HistGate, kind)
    Dict{String,Any}("kind" => kind, "x_channel" => g.x_channel, "y_channel" => g.y_channel,
                     "x_transform" => transform_spec(g.x_transform), "y_transform" => transform_spec(g.y_transform))
end
function gate_spec(g::RangeGate)
    d = merge(_hist_spec(g, "range"), Dict{String,Any}("lo" => g.lo, "hi" => g.hi))
    g.lo_rule === nothing || (d["lo_rule"] = rule_spec(g.lo_rule))
    g.hi_rule === nothing || (d["hi_rule"] = rule_spec(g.hi_rule))
    d
end
function gate_spec(g::BisectorGate)
    d = merge(_hist_spec(g, "bisector"), Dict{String,Any}("cut" => g.cut, "side" => g.side, "group" => g.group))
    g.rule === nothing || (d["rule"] = rule_spec(g.rule))
    d
end

# `to_disp(v, v)[1]` = a stored value on the gate's axis → the plot's x display coordinate. A 1D gate
# only matches a plot with x = y = its channel (the histogram), so there is no swap to honour.
_rule_disp(r::ValleyRule, to_disp) = Dict{String,Any}("kind" => "valley",
    "lo" => to_disp(r.lo, r.lo)[1], "hi" => to_disp(r.hi, r.hi)[1], "adjust" => r.adjust)
function _projected(g::RangeGate, to_disp, swap)
    d = Dict{String,Any}("kind" => "range", "lo" => to_disp(g.lo, g.lo)[1], "hi" => to_disp(g.hi, g.hi)[1])
    g.lo_rule === nothing || (d["lo_rule"] = _rule_disp(g.lo_rule, to_disp))
    g.hi_rule === nothing || (d["hi_rule"] = _rule_disp(g.hi_rule, to_disp))
    d
end
function _projected(g::BisectorGate, to_disp, swap)
    d = Dict{String,Any}("kind" => "bisector", "cut" => to_disp(g.cut, g.cut)[1], "side" => g.side, "group" => g.group)
    g.rule === nothing || (d["rule"] = _rule_disp(g.rule, to_disp))
    d
end
