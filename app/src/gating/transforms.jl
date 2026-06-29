# ── Axis transforms for gating ───────────────────────────────────────────────────
#
# Gating plots use nonlinear axes (logicle/biexponential). Gates are stored in
# TRANSFORMED coordinates; to test a raw cell we apply the same transform then do the
# geometry test (see docs/POPULATION.md). Implemented once, in Julia (ARCHITECTURE.md:191,
# :250).
#
# Each transform provides:
#   apply_transform(t, x)  raw → display coordinate   (vectorised + scalar)
#   invert_transform(t, y)     display → raw              (for axis tick placement)
#
# References
# ----------
# Logicle (biexponential) scale:
#   - Parks DR, Roederer M, Moore WA. "A new 'Logicle' display method avoids deceptive
#     effects of logarithmic scaling for low signals and compensated data."
#     Cytometry A. 2006;69A(6):541-551. doi:10.1002/cyto.a.20258
#   - Moore WA, Parks DR. "Update for the logicle data scale including operational code
#     implementations." Cytometry A. 2012;81A(4):273-277. doi:10.1002/cyto.a.22030
#     (this is the reference C++ `Logicle` class the implementation below ports:
#      parameters T/W/M/A; solve() for d; Taylor series near x1; Halley's method in scale())
#   - Reference implementation cross-checked against FlowUtils' `logicle_c` (C port of the
#     above): https://github.com/whitews/FlowUtils  — golden values are asserted in
#     app/test/runtests.jl ("Transforms"), agreement < 5e-11 over [-1000, 262144].
# Arcsinh / biexponential conventions: GatingML 2.0 spec (ISAC), https://flowcyt.github.io/gating-ml/

abstract type AxisTransform end

# Vectorised wrappers (scalar method defined per type below)
apply_transform(t::AxisTransform, x::AbstractArray) = map(v -> apply_transform(t, float(v)), x)
invert_transform(t::AxisTransform, y::AbstractArray)    = map(v -> invert_transform(t, float(v)), y)

# ── Linear ───────────────────────────────────────────────────────────────────────
struct LinearTransform <: AxisTransform end
apply_transform(::LinearTransform, x::Real) = float(x)
invert_transform(::LinearTransform, y::Real)    = float(y)

# ── Log10 (values ≤ 0 clamped to a small positive floor) ──────────────────────────
struct LogTransform <: AxisTransform
    floor::Float64
end
LogTransform(; floor::Real=1.0) = LogTransform(float(floor))
apply_transform(t::LogTransform, x::Real) = log10(max(float(x), t.floor))
invert_transform(::LogTransform, y::Real)     = exp10(float(y))

# ── Arcsinh (cofactor) — common, well-behaved alternative to logicle ──────────────
struct AsinhTransform <: AxisTransform
    cofactor::Float64
end
AsinhTransform(; cofactor::Real=150.0) = AsinhTransform(float(cofactor))
apply_transform(t::AsinhTransform, x::Real) = asinh(float(x) / t.cofactor)
invert_transform(t::AsinhTransform, y::Real)    = sinh(float(y)) * t.cofactor

# ── Logicle (Moore & Parks 2012; see References at top of file) ──────────────────
# Port of the reference C++ `Logicle` class. Maps data → [0,1] scale over M decades.
# Parameters:
#   T = top of scale (data value mapping to 1.0)
#   W = number of decades of linearisation around zero
#   M = total number of decades
#   A = additional decades of negative data below zero
const _LN10 = log(10.0)
const _TAYLOR_LEN = 16

struct LogicleTransform <: AxisTransform
    T::Float64; W::Float64; M::Float64; A::Float64
    a::Float64; b::Float64; c::Float64; d::Float64; f::Float64
    w::Float64; x0::Float64; x1::Float64; x2::Float64
    xTaylor::Float64
    taylor::Vector{Float64}
end

# solve for d: root of 2*(ln(d)-ln(b)) + w*(b+d) = 0 in (0, b); monotone increasing.
function _logicle_solve(b::Float64, w::Float64)::Float64
    w == 0 && return b
    tol = 2 * eps(Float64)
    d_lo, d_hi = 0.0, b
    d = (d_lo + d_hi) / 2          # bisection-safeguarded Newton
    for _ in 1:100
        f  = 2 * (log(d) - log(b)) + w * (b + d)
        f < 0 ? (d_lo = d) : (d_hi = d)
        df = 2 / d + w
        dstep = f / df
        dnew = d - dstep
        (dnew <= d_lo || dnew >= d_hi) && (dnew = (d_lo + d_hi) / 2)  # fall back to bisection
        abs(dnew - d) < tol && return dnew
        d = dnew
    end
    d
end

function LogicleTransform(; T::Real=262144.0, W::Real=0.5, M::Real=4.5, A::Real=0.0)
    T = float(T); W = float(W); M = float(M); A = float(A)
    w  = W / (M + A)
    x2 = A / (M + A)
    x1 = x2 + w
    x0 = x2 + 2 * w
    b  = (M + A) * _LN10
    d  = _logicle_solve(b, w)
    c_a  = exp(x0 * (b + d))
    mf_a = exp(b * x1) - c_a / exp(d * x1)
    a = T / ((exp(b) - mf_a) - c_a / exp(d))
    c = c_a * a
    f = -mf_a * a
    # Taylor series of the biexponential around x1 (for the slowly-converging region)
    xTaylor = x1 + w / 4
    taylor = Vector{Float64}(undef, _TAYLOR_LEN)
    posCoef = a * exp(b * x1)
    negCoef = -c * exp(-d * x1)
    for i in 1:_TAYLOR_LEN
        posCoef *= b / i
        negCoef *= -d / i
        taylor[i] = posCoef + negCoef
    end
    taylor[2] = 0.0   # exact result of the Logicle condition (drop linear term)
    LogicleTransform(T, W, M, A, a, b, c, d, f, w, x0, x1, x2, xTaylor, taylor)
end

# biexponential evaluated via Taylor series near x1 (scale → data)
function _series_biexp(t::LogicleTransform, scale::Float64)::Float64
    x = scale - t.x1
    s = t.taylor[_TAYLOR_LEN]
    @inbounds for i in (_TAYLOR_LEN-1):-1:1
        s = s * x + t.taylor[i]
    end
    s * x
end

# inverse: scale (display, [0,1]) → data (raw)
function invert_transform(t::LogicleTransform, scale::Real)::Float64
    s = float(scale)
    negative = s < t.x1
    negative && (s = 2 * t.x1 - s)
    rv = s < t.xTaylor ? _series_biexp(t, s) :
         t.a * exp(t.b * s) + t.f - t.c * exp(-t.d * s)
    negative ? -rv : rv
end

# transform: data (raw) → scale (display, [0,1]). Halley's method (Moore & Parks scale()).
function apply_transform(t::LogicleTransform, value::Real)::Float64
    value = float(value)
    value == 0.0 && return t.x1                 # true zero maps to x1
    negative = value < 0.0                       # reflect negatives, solve positive branch
    negative && (value = -value)
    # initial guess (log region); fall back to x1 in the quasi-linear region
    x = log(value / t.a) / t.b
    isfinite(x) || (x = t.x1)
    tol = 3 * eps(Float64)
    for _ in 1:40
        ae2bx  = t.a * exp(t.b * x)
        ce2mdx = t.c * exp(-t.d * x)
        y = x < t.xTaylor ? (_series_biexp(t, x) - value) :
                            ((ae2bx + t.f) - (ce2mdx + value))
        abe2bx  = t.b * ae2bx
        cde2mdx = t.d * ce2mdx
        dy  = abe2bx + cde2mdx
        ddy = t.b * abe2bx - t.d * cde2mdx
        delta = y / (dy * (1 - y * ddy / (2 * dy * dy)))   # Halley step
        x -= delta
        abs(delta) < tol && return negative ? 2 * t.x1 - x : x
    end
    negative ? 2 * t.x1 - x : x
end

# ── (De)serialisation — transform spec travels with the gate, readable by Python ──
"""Build a transform from a spec dict (e.g. parsed from gating JSON)."""
function transform_from_spec(spec::AbstractDict)::AxisTransform
    kind = lowercase(string(get(spec, "kind", get(spec, :kind, "linear"))))
    g(k, default) = get(spec, k, get(spec, Symbol(k), default))
    if kind == "linear"
        LinearTransform()
    elseif kind == "log"
        LogTransform(; floor=g("floor", 1.0))
    elseif kind == "asinh"
        AsinhTransform(; cofactor=g("cofactor", 150.0))
    elseif kind == "logicle"
        LogicleTransform(; T=g("T", 262144.0), W=g("W", 0.5), M=g("M", 4.5), A=g("A", 0.0))
    else
        error("Unknown transform kind: $kind")
    end
end

transform_spec(::LinearTransform) = Dict("kind" => "linear")
transform_spec(t::LogTransform)   = Dict("kind" => "log", "floor" => t.floor)
transform_spec(t::AsinhTransform) = Dict("kind" => "asinh", "cofactor" => t.cofactor)
transform_spec(t::LogicleTransform) = Dict("kind" => "logicle", "T" => t.T, "W" => t.W, "M" => t.M, "A" => t.A)
