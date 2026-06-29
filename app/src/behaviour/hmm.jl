# ── hmm.jl — Gaussian HMM over track measurements ────────────────────────────────
#
# Port of behaviourAnalysis `hmmStates` + `hmmTransitions` (R/Shiny cecelia). The original used
# depmixS4: a Gaussian-emission HMM with measurements conditionally independent given the state,
# one sequence per track (depmixS4 `ntimes`), and a Viterbi global decode (`posterior()$state`).
#
# We replicate that with HiddenMarkovModels.jl, supplying a diagonal-Gaussian emission whose
# weighted M-step we own — the package's Distributions-based refit has no `suffstats` for a
# diagonal MvNormal, and owning the M-step keeps depmixS4's independent-per-measurement semantics
# exactly. HiddenMarkovModels.jl handles forward-backward, transition re-estimation, and Viterbi.
#
# Per-cell semantics (the subtle part): a track's first cell has no speed and its first two have no
# angle (see track_measures.jl — those land as NaN). Such cells are dropped before fitting and get
# `missing` on join-back, so states are per-cell but undefined where a measurement can't exist —
# matching the R `drop_na`/`filter(!is.infinite)` then left-join behaviour, including its quirk of
# treating a mid-track drop as if the surrounding cells were contiguous.
#
# Pure functions over a pooled per-cell DataFrame (built by `pop_df(imgs, uids, …)`); no I/O, no
# HTTP — headless-testable. See docs/MODULES.md (behaviour tasks).

using HiddenMarkovModels: HMM, baum_welch, viterbi
import StatsAPI
import DensityInterface
using DensityInterface: HasDensity
using Random: AbstractRNG, randn
using Statistics: mean, std, median
using DataFrames: DataFrame, nrow

# ── Diagonal-Gaussian emission (independent Normal per measurement) ───────────────

mutable struct DiagGaussEmission
    mu::Vector{Float64}
    sd::Vector{Float64}
end

DensityInterface.DensityKind(::DiagGaussEmission) = HasDensity()
Base.length(d::DiagGaussEmission) = length(d.mu)

function DensityInterface.logdensityof(d::DiagGaussEmission, x)
    s = 0.0
    @inbounds for i in eachindex(x)
        z = (x[i] - d.mu[i]) / d.sd[i]
        s += -0.5 * log(2π) - log(d.sd[i]) - 0.5 * z * z
    end
    s
end

function Base.rand(rng::AbstractRNG, d::DiagGaussEmission)
    [d.mu[i] + d.sd[i] * randn(rng) for i in eachindex(d.mu)]
end

# Weighted MLE (the HMM M-step). `x` = vector of observation vectors, `w` = posterior weights.
# Variance floored to keep states from collapsing onto a single point (degenerate emission).
const _SD_FLOOR = 1e-6
function StatsAPI.fit!(d::DiagGaussEmission, x, w)
    D = length(d.mu)
    W = sum(w)
    fill!(d.mu, 0.0)
    @inbounds for (xi, wi) in zip(x, w), k in 1:D
        d.mu[k] += wi * xi[k]
    end
    d.mu ./= W
    s2 = zeros(D)
    @inbounds for (xi, wi) in zip(x, w), k in 1:D
        s2[k] += wi * (xi[k] - d.mu[k])^2
    end
    s2 ./= W
    @. d.sd = sqrt(max(s2, _SD_FLOOR))
    d
end

# ── Preprocessing helpers ─────────────────────────────────────────────────────────

# Centered running mean over a sequence of observation vectors (per measurement), window `k`.
# Edges use the available window (shrinking) — an approximation of R caTools::runmean(endrule).
function _running_mean_vecs(M::Vector{Vector{Float64}}, k::Int)
    k <= 1 && return M
    n = length(M)
    D = length(first(M))
    h = k ÷ 2
    out = [zeros(D) for _ in 1:n]
    for i in 1:n
        lo = max(1, i - h); hi = min(n, i + h)
        cnt = hi - lo + 1
        @inbounds for d in 1:D
            s = 0.0
            for j in lo:hi
                s += M[j][d]
            end
            out[i][d] = s / cnt
        end
    end
    out
end

# Global per-measurement normalisation (÷ a summary stat) then scaling (÷ sd, no centering),
# applied across all observations in place. `normalise`: measure => "min"|"max"|"median"|"mean".
# `scale_measures`: measures to divide by their (uncentered) standard deviation. Mirrors the R
# normMeasurements / scaleMeasurements steps (scale(center = FALSE)).
# `measures`/`scale_measures` accept any string vector (an empty selection from the GUI arrives as
# `Vector{Union{}}`, which is not `Vector{String}` — keep the signature abstract to match the public
# `hmm_fit_states`).
function _normalise_scale!(obs::Vector{Vector{Float64}}, measures::AbstractVector{<:AbstractString},
                           normalise::AbstractDict, scale_measures::AbstractVector{<:AbstractString})
    pos = Dict(m => i for (i, m) in enumerate(measures))
    for (m, how) in normalise
        haskey(pos, m) || continue
        d = pos[m]
        col = [o[d] for o in obs]
        v = how == "min"    ? minimum(col) :
            how == "max"    ? maximum(col) :
            how == "median" ? median(col)  : mean(col)
        (v == 0 || !isfinite(v)) && continue
        for o in obs
            o[d] /= v
        end
    end
    for m in scale_measures
        haskey(pos, m) || continue
        d = pos[m]
        col = [o[d] for o in obs]
        s = std(col; corrected=true)
        (s == 0 || !isfinite(s)) && continue
        for o in obs
            o[d] /= s
        end
    end
    obs
end

# Windowed-mode smoothing of a decoded state sequence (per track), `iters` passes, centered
# window `w` (odd-ish; uses the available window at edges). Ties → smallest state. Ports the R
# postFiltering/postIterations step (DescTools::Mode over a frollapply window, take-first on ties).
function _mode_smooth(seq::Vector{Int}, w::Int, iters::Int)
    (w <= 1 || isempty(seq)) && return seq
    n = length(seq)
    h = w ÷ 2
    cur = copy(seq)
    for _ in 1:iters
        nxt = similar(cur)
        for i in 1:n
            lo = max(1, i - h); hi = min(n, i + h)
            counts = Dict{Int,Int}()
            for j in lo:hi
                counts[cur[j]] = get(counts, cur[j], 0) + 1
            end
            best = cur[i]; bestc = -1
            for (s, c) in counts
                if c > bestc || (c == bestc && s < best)
                    best = s; bestc = c
                end
            end
            nxt[i] = best
        end
        cur = nxt
    end
    cur
end

# Deterministic HMM initialisation: order observations by the first measurement, split into K
# contiguous quantile bins, seed each state's emission from its bin's mean/sd. Transition matrix
# is diagonal-heavy (0.9 self), initial distribution uniform. Deterministic → reproducible fit
# (so the R `seed` param is unnecessary for our path; accepted but unused at the engine level).
function _init_hmm(obs::Vector{Vector{Float64}}, K::Int)
    N = length(obs)
    D = length(first(obs))
    perm = sortperm([o[1] for o in obs])
    bins = [Int[] for _ in 1:K]
    for (rank, idx) in enumerate(perm)
        b = min(K, 1 + (rank - 1) * K ÷ N)
        push!(bins[b], idx)
    end
    dists = DiagGaussEmission[]
    for b in 1:K
        members = isempty(bins[b]) ? collect(1:N) : bins[b]
        mu = [mean(obs[i][d] for i in members) for d in 1:D]
        sd = [max(std([obs[i][d] for i in members]; corrected=false), 1e-3) for d in 1:D]
        push!(dists, DiagGaussEmission(mu, sd))
    end
    init  = fill(1.0 / K, K)
    off   = (1.0 - 0.9) / max(K - 1, 1)
    trans = fill(off, K, K)
    for i in 1:K
        trans[i, i] = 0.9
    end
    HMM(init, trans, dists)
end

# ── Fit states ─────────────────────────────────────────────────────────────────

"""
    hmm_fit_states(df, measures; num_states, time_col,
                   group_cols=["uID","value_name","track_id"],
                   noise_filter=0, normalise=Dict(), scale_measures=String[],
                   max_iter=200) -> Vector{Union{Int,Missing}}

Fit one Gaussian HMM over the `measures` columns of pooled per-cell `df` (one sequence per track,
identified by `group_cols`, ordered within a track by `time_col`) and return a per-row state vector
**aligned to `df`'s rows**. Rows with a `missing`/non-finite value in any measure (track-start cells
with no speed/angle) are excluded from the fit and returned as `missing`.

Preprocessing order mirrors the R port: order-within-track → drop NA/Inf → per-track noise filter
(running mean) → global normalise → global scale → fit → Viterbi decode.
"""
function hmm_fit_states(df::DataFrame, measures::AbstractVector{<:AbstractString};
                        num_states::Int,
                        time_col::AbstractString,
                        group_cols::AbstractVector{<:AbstractString}=["uID", "value_name", "track_id"],
                        noise_filter::Int=0,
                        normalise::AbstractDict=Dict{String,String}(),
                        scale_measures::AbstractVector{<:AbstractString}=String[],
                        post_filter::Int=0, post_iterations::Int=1,
                        max_iter::Int=200)
    measures = String.(collect(measures))
    n = nrow(df)
    states = Vector{Union{Int,Missing}}(missing, n)
    n == 0 && return states

    _finite(v) = !(v === missing) && v isa Number && isfinite(Float64(v))

    # valid rows: finite in every measure
    valid = trues(n)
    for m in measures
        col = df[!, m]
        @inbounds for i in 1:n
            valid[i] &= _finite(col[i])
        end
    end

    # group valid rows, preserving first-seen group order
    gcols = [df[!, c] for c in group_cols]
    tcol  = df[!, time_col]
    groups = Dict{Tuple,Vector{Int}}()
    order  = Tuple[]
    for i in 1:n
        valid[i] || continue
        key = ntuple(c -> gcols[c][i], length(group_cols))
        haskey(groups, key) || (groups[key] = Int[]; push!(order, key))
        push!(groups[key], i)
    end
    isempty(order) && return states

    seqs    = Vector{Vector{Float64}}[]   # per-track obs vectors
    seq_idx = Vector{Int}[]               # original row indices, aligned
    for key in order
        idxs = sort(groups[key]; by=i -> Float64(tcol[i]))
        M = [Float64[Float64(df[i, m]) for m in measures] for i in idxs]
        noise_filter > 1 && (M = _running_mean_vecs(M, noise_filter))
        push!(seqs, M)
        push!(seq_idx, idxs)
    end

    obs = reduce(vcat, seqs)
    _normalise_scale!(obs, measures, normalise, scale_measures)
    seq_ends = cumsum(length.(seqs))

    hmm0 = _init_hmm(obs, num_states)
    est, _ = baum_welch(hmm0, obs; seq_ends=seq_ends, max_iterations=max_iter)
    q = viterbi(est, obs; seq_ends=seq_ends)
    q = q isa Tuple ? first(q) : q

    # scatter per sequence (so post-filter smoothing stays within a track)
    start = 1
    for (s, idxs) in enumerate(seq_idx)
        stop = seq_ends[s]
        seg = Int[Int(q[k]) for k in start:stop]
        post_filter > 1 && (seg = _mode_smooth(seg, post_filter, max(post_iterations, 1)))
        @inbounds for (j, i) in enumerate(idxs)
            states[i] = seg[j]
        end
        start = stop + 1
    end
    states
end

# ── Transitions ──────────────────────────────────────────────────────────────────

# state value → label string (1, 2, … — drop the float decimal a numeric obs column carries)
_state_str(v) = v isa Integer ? string(v) :
                (v isa AbstractFloat ? string(Int(round(v))) : string(v))

"""
    hmm_transitions(df, state_cols; time_col, group_cols=["uID","value_name","track_id"],
                    include_start=false, include_self=true) -> Vector{Union{String,Missing}}

Per-cell HMM transition labels. First the **hybrid** state is formed by pasting `state_cols` with
`.` (combining several independently-fit models — the cross-model hybrid). Then, within each track
(ordered by `time_col`), the lagged transition `"<prev>_<cur>"` is emitted per cell:

- a cell with any `missing`/NaN state column has a `missing` hybrid → `missing` transition;
- `include_start` keeps the first in-track transition (lag = start, labelled `"NA_<cur>"`);
- `include_self` keeps self-transitions (`prev == cur`).

Faithful to the R `hmmTransitions` truth table (includeStart × includeSelfTransitions).
"""
function hmm_transitions(df::DataFrame, state_cols::AbstractVector{<:AbstractString};
                         time_col::AbstractString,
                         group_cols::AbstractVector{<:AbstractString}=["uID", "value_name", "track_id"],
                         include_start::Bool=false, include_self::Bool=true)
    state_cols = String.(collect(state_cols))
    n = nrow(df)
    trans = Vector{Union{String,Missing}}(missing, n)
    n == 0 && return trans

    # a state value is "present" if not missing and not an empty/NaN placeholder. State columns may
    # arrive as Ints (in-memory, fresh fit) OR as Strings (read back from a categorical obs column,
    # which is how the composite hmm_states → hmm_transitions step sees them).
    _present(v) = !(v === missing) &&
                  (v isa Number ? isfinite(Float64(v)) : !isempty(strip(string(v))))

    # hybrid per row (missing if any state col is absent)
    hybrid = Vector{Union{String,Missing}}(missing, n)
    cols = [df[!, c] for c in state_cols]
    for i in 1:n
        ok = true
        parts = String[]
        for c in 1:length(state_cols)
            v = cols[c][i]
            if !_present(v)
                ok = false
                break
            end
            push!(parts, _state_str(v))
        end
        ok && (hybrid[i] = join(parts, "."))
    end

    # group, order by time, lag within track
    gcols = [df[!, c] for c in group_cols]
    tcol  = df[!, time_col]
    groups = Dict{Tuple,Vector{Int}}()
    order  = Tuple[]
    for i in 1:n
        key = ntuple(c -> gcols[c][i], length(group_cols))
        haskey(groups, key) || (groups[key] = Int[]; push!(order, key))
        push!(groups[key], i)
    end

    for key in order
        idxs = sort(groups[key]; by=i -> Float64(tcol[i]))
        prev = missing                       # lag = shift(hybrid) — includes missing values
        for i in idxs
            cur = hybrid[i]
            if cur === missing
                prev = cur
                continue
            end
            if prev === missing
                # track start: only with include_start, and only as a non-self "NA_cur"
                include_start && include_self && (trans[i] = "NA_" * cur)
            elseif include_self || prev != cur
                trans[i] = prev * "_" * cur
            end
            prev = cur
        end
    end
    trans
end
