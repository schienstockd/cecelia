# Correction-plan QC score layer (docs/todo/CORRECTION_QC_PLAN.md).
#
# **What this file is.** The score-band layer from §2 of the plan: pure functions that read an
# image's `meta` and return `QCResult`s in `[0.0, 1.0]`. No consumer yet — no rule engine, no
# preset cards, no `CorrectionPlan` type. That layer is Phase B; the plan's §3 (tie-break) and §5
# (cards) will consume these results later.
#
# **What this file is NOT.** Not a task, not a preset, not the plan engine. See `docs/qc.jl` for the
# advisory-finding framework these scores complement — a finding is a discrete "the output looks off"
# fact, a score is a continuous 0-1 signal a rule engine can trigger on with a tunable threshold.
#
# **All bands are unvalidated placeholders** — per §2.4, calibrated on Dominik's dev movies only.
# The scoring FORMULAS live here; the tunable trigger ranges live at the (not-yet-built) rule engine.
#
# Phase B scope: the §2.1 metadata-derived scores (no pixel work at plan time). §2.2 (post-hoc
# reliability from `qc/{fun}/{value_name}.json`) and §2.3 (cohort) are deferred to later phases.


"""
    QCResult(metric, score, level, subs, scope)

One QC signal. `metric` is the score name (e.g. `"axis.T_present"`). `score` is the normalised
value in `[0.0, 1.0]` (higher = more confidence the condition holds). `level` mirrors
`qc_finding` severity (`"info"` / `"warn"`) so a Phase B consumer can turn a score into a finding
without a second wording pass. `subs` are placeholder substitutions for `QC_TEXT[metric]` on read.
`scope` records the metric's natural granularity so a Phase B consumer knows how to reduce across
scopes (per 1d §3): `:image` for a per-image metric, `:channel` for per-channel (with `subs[:channel]`
set), `:pair` for per-channel-pair, `:frame`, or `:plane`.

Constructor validates `score ∈ [0.0, 1.0]` and normalises `NaN` to a special "no signal" sentinel
that a caller can distinguish from `0.0`.
"""
struct QCResult
    metric::String
    score::Float64
    level::String
    subs::Dict{Symbol, Any}
    scope::Symbol
end

# Sentinel score for "the check did not run" — distinguished from `0.0` ("checked and the answer was
# no"). A rule engine treating a signal-absent metric as `0.0` would silently include (or exclude)
# every task it depends on; NaN forces the caller to handle absence explicitly.
const QC_SCORE_ABSENT = NaN

_validate_score(x::Real) = isnan(x) ? Float64(x) :
    (0.0 <= x <= 1.0 ? Float64(x) :
     error("QCResult.score must be in [0.0, 1.0] or NaN, got $x"))

function QCResult(metric::AbstractString, score::Real; level::AbstractString = "info",
                  subs::AbstractDict = Dict{Symbol,Any}(), scope::Symbol = :image)
    QCResult(String(metric), _validate_score(score), String(level),
             Dict{Symbol,Any}(k => v for (k, v) in subs), scope)
end

qc_score_absent(r::QCResult) = isnan(r.score)


# ── §2.1 scores — metadata-derived, no pixel work at plan time ─────────────────────────────────────

"""
    qc_axis_t_present(meta) -> QCResult

Structural. `1.0` when `meta.SizeT > 1` (a timelapse), `0.0` otherwise. Feeds the rule table's
`include driftCorrect / temporal-smooth / denoise-T` trigger.
"""
function qc_axis_t_present(meta::AbstractDict)::QCResult
    size_t = _cal_int(get(meta, "SizeT", nothing), 1)
    QCResult("axis.T_present", size_t > 1 ? 1.0 : 0.0; scope = :image,
             subs = Dict{Symbol,Any}(:sizeT => size_t))
end

"""
    qc_axis_z_present(meta) -> QCResult

Structural. `1.0` when `meta.SizeZ > 1` (a stack), `0.0` otherwise. Feeds `include stackAlign`.
"""
function qc_axis_z_present(meta::AbstractDict)::QCResult
    size_z = _cal_int(get(meta, "SizeZ", nothing), 1)
    QCResult("axis.Z_present", size_z > 1 ? 1.0 : 0.0; scope = :image,
             subs = Dict{Symbol,Any}(:sizeZ => size_z))
end

"""
    qc_all_channels_saturated(meta) -> QCResult

`1.0` when EVERY channel in `meta.saturation.channels` is `saturated: true` — the exclude signal
for `denoise` (PR #796: refuse when there is no clean channel to learn from). `0.0` when at least
one is clean. `NaN` when the saturation probe never ran (an image imported before the check
existed, or a non-integer store).

Per-image scope: the roll-up is over all channels. A caller with a specific channel selection can
inspect `meta.saturation.channels[i]` directly rather than filtering here — this score's job is the
whole-image gate.
"""
function qc_all_channels_saturated(meta::AbstractDict)::QCResult
    chans = _saturation_channels(meta)
    if isempty(chans)
        return QCResult("denoise.channel_saturated_frac", QC_SCORE_ABSENT; scope = :image,
                        subs = Dict{Symbol,Any}(:nChannels => 0))
    end
    n_sat = count(ch -> get(ch, "saturated", false) === true, chans)
    frac  = n_sat / length(chans)
    QCResult("denoise.channel_saturated_frac", frac; scope = :image,
             level = (frac >= 1.0 ? "warn" : "info"),
             subs = Dict{Symbol,Any}(:nSaturated => n_sat, :nChannels => length(chans)))
end

"""
    qc_photon_limited_frac(meta) -> QCResult

The worst channel's `zeroFrac` (the sparsest, most photon-limited one). `NaN` when the probe never
ran or none of the channels carry sparsity fields (pre-#811 imports). Directly readable — no
threshold applied here. Feeds the C-Resonance card's recommender in §5 and the "run denoise before
segmentation" nudge.

Per-image scope with `subs[:channel]` set to the worst channel's index so the caller can point at
it. If two channels tie for worst, the first wins — the tie is not meaningful given the placeholder
bands.
"""
function qc_photon_limited_frac(meta::AbstractDict)::QCResult
    chans = _saturation_channels(meta)
    if isempty(chans)
        return QCResult("smooth.photon_limited_frac", QC_SCORE_ABSENT; scope = :image,
                        subs = Dict{Symbol,Any}())
    end
    worst_zf = -1.0; worst_i = -1; any_field = false
    for ch in chans
        zf = _cal_num(get(ch, "zeroFrac", nothing))
        isnothing(zf) && continue
        any_field = true
        if zf > worst_zf
            worst_zf = zf
            worst_i  = _cal_int(get(ch, "index", nothing), 0)
        end
    end
    any_field || return QCResult("smooth.photon_limited_frac", QC_SCORE_ABSENT; scope = :image,
                                  subs = Dict{Symbol,Any}())
    QCResult("smooth.photon_limited_frac", worst_zf; scope = :image,
             subs = Dict{Symbol,Any}(:channel => worst_i, :zeroFrac => worst_zf))
end


# ── Roll-up ────────────────────────────────────────────────────────────────────────────────────────

"""
    compute_qc_scores(img) -> Vector{QCResult}
    compute_qc_scores(meta) -> Vector{QCResult}

Every §2.1 metadata-derived score for one image. Pure over `meta` — an `img` argument is
convenience, the persisted ccid meta is what actually decides the scores. Order is stable so a
downstream diff-of-two-runs can align by index.

Post-hoc scores (§2.2) and cohort scores (§2.3) are NOT included here; they read from
`qc/{fun}/{value_name}.json` / `qc_cohort.jl` and belong to their own roll-ups.
"""
function compute_qc_scores(meta::AbstractDict)::Vector{QCResult}
    QCResult[
        qc_axis_t_present(meta),
        qc_axis_z_present(meta),
        qc_all_channels_saturated(meta),
        qc_photon_limited_frac(meta),
    ]
end

function compute_qc_scores(img::CciaImage)::Vector{QCResult}
    ccid = state_file(img)
    isfile(ccid) || return QCResult[]
    raw  = read_ccid_raw(ccid)
    meta = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    compute_qc_scores(meta)
end
