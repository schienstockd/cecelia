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


# ── §1 rule-table encoding + §8 plan types ─────────────────────────────────────────────────────────
#
# Phase C consumes §2.1 scores + a preset card + wizard answers → a `CorrectionPlan`. It is a set of
# pure functions (no chain wiring, no plan.json write yet — those land in Phase D). Provenance
# fields sketched in §8 (writer_versions, upstream_value_names, saturation_fingerprint) are
# deliberately absent from `CorrectionPlan` here — populating them meaningfully requires the sidecar
# write path, and adding empty placeholders now would leave a lying field.

# Order-weight buckets per §1: pre-drift geometry, drift, post-drift residuals, intensity/signal,
# storage/output. Bucket VALUES are the tie-broken order on the chain; a value between buckets is
# reserved for future tasks that want to slot in without a bucket move.
const _ORDER_WEIGHTS = Dict{String,Int}(
    "cleanupImages.flip"         => 100,
    "cleanupImages.stackAlign"   => 100,
    "cleanupImages.driftCorrect" => 200,
    "cleanupImages.flowRegister" => 300,
    "cleanupImages.smooth"       => 300,
    "cleanupImages.afCorrect"    => 400,
    "cleanupImages.denoise"      => 400,
    "cleanupImages.dtype"        => 500,
)

_order_weight(fn::AbstractString) = get(_ORDER_WEIGHTS, String(fn), 250)  # 250 = "unclassified"


"""
    CorrectionStep(fun_name, params, order_weight, source, exclusion_reason)

One task in a plan. `params` is the resolved param dict at plan-build time (not yet validated —
`validate_params` is a Phase D concern, once the plan mounts on a chain). `source` is one of
`:rule_default`, `:card`, `:wizard`, `:computed_qc`, `:user_edit`. `exclusion_reason` is set when
this step was considered and dropped; the caller keeps the row so the audit trail is complete.
"""
struct CorrectionStep
    fun_name::String
    params::Dict{String,Any}
    order_weight::Int
    source::Symbol
    exclusion_reason::Union{Nothing,String}
end

CorrectionStep(fun_name::AbstractString, params::AbstractDict;
               order_weight::Integer = _order_weight(fun_name),
               source::Symbol = :rule_default,
               exclusion_reason::Union{Nothing,AbstractString} = nothing) =
    CorrectionStep(String(fun_name),
                   Dict{String,Any}(String(k) => v for (k, v) in params),
                   Int(order_weight), source,
                   exclusion_reason === nothing ? nothing : String(exclusion_reason))


"""
    CorrectionPlan(image_uid, preset_id, wizard_answers, included, excluded, qc_scores;
                   cecelia_version, saturation_fingerprint)

A per-image plan. `included` is the ordered steps that will run; `excluded` keeps the rows the
engine considered and dropped (with `exclusion_reason` set), so the UI can render the audit trail.
`qc_scores` is the score snapshot the engine used — a re-plan against the same snapshot is a pure
function of these inputs.

**Persistence (Phase D).** `save_plan(img, plan)` writes this to `1/{image_uid}/plan.json`;
`load_plan(img)` reads it back. Two provenance fields answer "is this plan still current?":
`cecelia_version` from `cecelia_version()` (the code that would run it now — one-file-bump per
`docs/RELEASING.md`), and `saturation_fingerprint` from the input's `meta.saturation` (detects a
re-import that shifted channel-level saturation numbers, which a version stamp cannot catch). The
other §8 provenance field — `upstream_value_names` per step — is deferred to Phase E (chain-mount
knows which `value_name` each step reads; the standalone engine does not).
"""
struct CorrectionPlan
    image_uid::String
    preset_id::Symbol
    wizard_answers::Dict{Symbol,Any}
    included::Vector{CorrectionStep}
    excluded::Vector{CorrectionStep}
    qc_scores::Vector{QCResult}
    cecelia_version::String
    saturation_fingerprint::String
end

CorrectionPlan(image_uid::AbstractString, preset_id::Symbol, wizard_answers::AbstractDict,
               included::AbstractVector{CorrectionStep}, excluded::AbstractVector{CorrectionStep},
               qc_scores::AbstractVector{QCResult};
               cecelia_version::AbstractString = cecelia_version(),
               saturation_fingerprint::AbstractString = "") =
    CorrectionPlan(String(image_uid), preset_id,
                   Dict{Symbol,Any}(Symbol(k) => v for (k, v) in wizard_answers),
                   collect(CorrectionStep, included), collect(CorrectionStep, excluded),
                   collect(QCResult, qc_scores),
                   String(cecelia_version), String(saturation_fingerprint))


# Score lookup — Phase C reads a handful of metrics by name; a linear scan is fine at ~5 scores per
# image and keeps the callers readable (no ordering assumption on `compute_qc_scores`).
function _score(scores::AbstractVector{QCResult}, metric::AbstractString)::Union{Nothing,QCResult}
    for r in scores
        r.metric == metric && return r
    end
    return nothing
end

_score_val(scores, metric, default::Float64 = QC_SCORE_ABSENT) =
    (r = _score(scores, metric); r === nothing || qc_score_absent(r) ? default : r.score)


"""
    apply_rules(scores, preset, wizard) -> (included, excluded)

The §1 rule table + §3 tie-break, as a pure function. Precedence (highest wins, per §3):
`user_edit > wizard > card > computed_qc > rule_default`. Phase C does not read `user_edit` (no
chain wiring), so the effective ladder is `wizard > card > score > default`.

The engine seeds from the preset (a card is a bag of starting-point params), then structural QC
scores force include/exclude for the two auto-rules the plan doc calls out (T→driftCorrect,
all-channels-saturated → NO denoise), then wizard answers overwrite params + include tasks the card
did not carry.
"""
function apply_rules(scores::AbstractVector{QCResult},
                     preset,  # AcquisitionPreset from correction_presets.jl
                     wizard::AbstractDict = Dict{Symbol,Any}())
    steps_by_fn = Dict{String,CorrectionStep}()
    excluded    = CorrectionStep[]

    # 1. Seed from the card's ordered fun list. Card is the starting point — any structural score
    #    or wizard answer downstream will override.
    for fn in preset.order_hints
        params = copy(get(preset.params_by_task, fn, Dict{String,Any}()))
        steps_by_fn[fn] = CorrectionStep(fn, params; source = :card)
    end

    # 2. Structural include/exclude from scores.
    #    (a) T-axis → driftCorrect. If missing on this image, exclude driftCorrect regardless of card.
    t_present = _score_val(scores, "axis.T_present", 0.0) >= 0.5
    if !t_present
        _mark_excluded!(steps_by_fn, excluded, "cleanupImages.driftCorrect",
                        "No T axis — drift correction not applicable")
        _mark_excluded!(steps_by_fn, excluded, "cleanupImages.flowRegister",
                        "No T axis — flow registration not applicable")
    elseif !haskey(steps_by_fn, "cleanupImages.driftCorrect")
        # T-axis present and card didn't ship a driftCorrect — auto-include with defaults.
        steps_by_fn["cleanupImages.driftCorrect"] = CorrectionStep(
            "cleanupImages.driftCorrect", Dict{String,Any}(); source = :computed_qc)
    end

    # (b) Z-axis missing → exclude stackAlign. Presence alone does NOT auto-include (needs the
    #     "breathing" card / W5 answer — see §Q-C1); the auto-signal is one-directional.
    z_present = _score_val(scores, "axis.Z_present", 0.0) >= 0.5
    if !z_present
        _mark_excluded!(steps_by_fn, excluded, "cleanupImages.stackAlign", "No Z axis")
    end

    # (c) All-channels-saturated → exclude denoise (PR #796 refusal). This wins over the card
    #     because the card cannot know about the image's channel-level saturation.
    sat_frac = _score_val(scores, "denoise.channel_saturated_frac", 0.0)
    if !isnan(sat_frac) && sat_frac >= 1.0
        _mark_excluded!(steps_by_fn, excluded, "cleanupImages.denoise",
                        "All selected channels saturated (meta.saturation)")
    end

    # 3. Wizard overrides (§3 tier 2, above card and score).
    _apply_wizard!(steps_by_fn, excluded, wizard, t_present, z_present)

    included = collect(values(steps_by_fn))
    sort!(included, by = s -> (s.order_weight, s.fun_name))
    return (included = included, excluded = excluded)
end

# W2 = stage rotated → driftEstimator = sitkRigid.
# W3 = intra-frame deformation → include flowRegister.
# W5 = intra-stack breathing → include stackAlign (only meaningful when Z present).
function _apply_wizard!(steps_by_fn, excluded, wizard, t_present, z_present)
    if get(wizard, :W2, :unknown) === :yes
        _set_param!(steps_by_fn, "cleanupImages.driftCorrect",
                    "driftEstimator", "sitkRigid", :wizard;
                    autocreate = t_present)
    end
    if get(wizard, :W3, :unknown) === :yes && t_present
        get!(steps_by_fn, "cleanupImages.flowRegister",
             CorrectionStep("cleanupImages.flowRegister", Dict{String,Any}(); source = :wizard))
    end
    if get(wizard, :W5, :unknown) === :yes && z_present
        get!(steps_by_fn, "cleanupImages.stackAlign",
             CorrectionStep("cleanupImages.stackAlign", Dict{String,Any}(); source = :wizard))
    end
end

# Mutate a step's params + bump its provenance. If the fun is not in the plan and `autocreate` is
# set, add a fresh step — needed for wizard params that touch a task the card did not ship.
function _set_param!(steps_by_fn, fn::AbstractString, key::AbstractString, value,
                     src::Symbol; autocreate::Bool = false)
    fn = String(fn)
    if haskey(steps_by_fn, fn)
        old = steps_by_fn[fn]
        new_params = copy(old.params)
        new_params[String(key)] = value
        steps_by_fn[fn] = CorrectionStep(fn, new_params;
                                          order_weight = old.order_weight,
                                          source = src)
    elseif autocreate
        steps_by_fn[fn] = CorrectionStep(fn, Dict{String,Any}(String(key) => value); source = src)
    end
    return steps_by_fn
end

function _mark_excluded!(steps_by_fn, excluded, fn::AbstractString, reason::AbstractString)
    fn = String(fn)
    if haskey(steps_by_fn, fn)
        s = pop!(steps_by_fn, fn)
        push!(excluded, CorrectionStep(fn, s.params;
                                        order_weight = s.order_weight,
                                        source = s.source,
                                        exclusion_reason = reason))
    else
        push!(excluded, CorrectionStep(fn, Dict{String,Any}();
                                        source = :computed_qc,
                                        exclusion_reason = reason))
    end
    return excluded
end


"""
    recommend_plan(img; card_id, wizard) -> CorrectionPlan
    recommend_plan(meta::AbstractDict; image_uid, card_id, wizard) -> CorrectionPlan

Pure meta→plan for testability; the `img` method wraps it. `card_id` defaults to `:custom` — the
no-preset fallback the plan doc names for `preset.card_confidence < 0.4`. A card-classifier that
picks a card automatically is deferred (§2.1 `preset.card_confidence` needs calibration ground
truth — see the plan doc's Open questions).
"""
function recommend_plan(meta::AbstractDict;
                        image_uid::AbstractString = "",
                        card_id::Symbol = :custom,
                        wizard::AbstractDict = Dict{Symbol,Any}())::CorrectionPlan
    scores = compute_qc_scores(meta)
    preset = preset_by_id(card_id)
    res    = apply_rules(scores, preset, wizard)
    CorrectionPlan(String(image_uid), card_id, wizard,
                   res.included, res.excluded, scores;
                   saturation_fingerprint = saturation_fingerprint(meta))
end

function recommend_plan(img::CciaImage;
                        card_id::Symbol = :custom,
                        wizard::AbstractDict = Dict{Symbol,Any}())::CorrectionPlan
    ccid = state_file(img)
    isfile(ccid) || return CorrectionPlan(String(img.uid), card_id, wizard,
                                          CorrectionStep[], CorrectionStep[], QCResult[])
    raw  = read_ccid_raw(ccid)
    meta = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    recommend_plan(meta; image_uid = String(img.uid), card_id = card_id, wizard = wizard)
end


# ── Phase D: plan.json persistence + provenance ───────────────────────────────────────────────────
#
# Sidecar shape (versioned, JSON):
#
#   { "planVersion": 1,
#     "ceceliaVersion": "0.2.0",
#     "imageUid": "img-abc",
#     "presetId": "resonance",
#     "wizardAnswers": { "W2": "yes" },
#     "saturationFingerprint": "<hex sha256>",
#     "included": [ { "funName": ..., "params": {...}, "orderWeight": 200,
#                     "source": "card", "exclusionReason": null }, ... ],
#     "excluded": [ ... ],
#     "qcScores":  [ { "metric": ..., "score": 0.94, "level": "info",
#                      "subs": {...}, "scope": "image" }, ... ] }
#
# The two provenance fields answer "is this plan still current?" — a version bump invalidates every
# plan (code that would run it changed), a fingerprint mismatch invalidates one image's plan (that
# image was re-imported). NO per-step writer versions — cecelia ships as one package.

const PLAN_JSON_VERSION = 1
const _PLAN_FILENAME = "plan.json"


"""
    saturation_fingerprint(meta) -> String

Lowercase-hex SHA-256 of a canonicalised `meta.saturation`. Empty string when the field is absent —
a caller comparing fingerprints should treat empty == empty as "both unknown, no signal". Ordinary
JSON3 round-trip preserves the fingerprint because the canonicaliser sorts keys and channels are
already indexed.

The fingerprint's job is one specific invalidation: someone re-imported the image and the saturation
detector changed its mind about which channels are photon-limited. It is NOT a general meta
fingerprint — pixel-size, axes, and channel names are stable across the read paths that matter to
the plan and don't need one.
"""
function saturation_fingerprint(meta::AbstractDict)::String
    sat = get(meta, "saturation", nothing)
    sat === nothing && return ""
    bytes2hex(SHA.sha256(_canonical_json(sat)))
end

# Canonical JSON: sorted keys recursively, so a JSON3 round-trip that reshuffles Dict order still
# hashes the same. Iterates through the dict's own keys (Symbol for JSON3.Object, String for
# Dict{String,Any}) and looks values up by the same key type — mixed-type key lookup is what got
# the versioned-field helpers before this convention landed (app/CLAUDE.md → JSON3 gotchas).
#
# **Numeric coercion is deliberate.** JSON3 reads `0.0` as `Float64(0.0)` but writes it back as
# `"0"` (indistinguishable from an Int), so a plain `JSON3.write` on a round-tripped number is not
# byte-stable. Every `Real` is coerced to `Float64` and stringified — `saturation_fingerprint(meta)`
# and `saturation_fingerprint(round_trip(meta))` MUST agree, or the invalidation logic is a lie.
function _canonical_json(v)
    if v isa AbstractDict
        entries = Tuple{String,Any}[]
        for k in keys(v)
            push!(entries, (String(k), v[k]))
        end
        sort!(entries; by = first)
        return "{" * join(("$(JSON3.write(k)):$(_canonical_json(x))" for (k, x) in entries), ",") * "}"
    elseif v isa AbstractVector
        return "[" * join((_canonical_json(x) for x in v), ",") * "]"
    elseif v isa Bool
        return v ? "true" : "false"   # ahead of Real — Bool <: Integer in Julia
    elseif v isa Real
        return string(Float64(v))     # coerces Int(0) and Float64(0.0) to the same "0.0"
    elseif v === nothing
        return "null"
    else
        return JSON3.write(v)         # strings, symbols
    end
end


"""
    save_plan(img, plan) -> String

Atomic write of `plan` to `1/{image_uid}/plan.json`, via `write_json_atomic`. Returns the written
path. Overwrites any prior plan — the versioned sidecar is authoritative, there is no history file
(prior plans are recoverable from git-tracked ccid.json + `recompute` if ever needed).
"""
function save_plan(img::CciaImage, plan::CorrectionPlan)::String
    path = joinpath(img._dir, _PLAN_FILENAME)
    write_json_atomic(path, _plan_to_dict(plan))
    return path
end


"""
    load_plan(img) -> Union{CorrectionPlan, Nothing}

Read plan.json for `img`. Returns `nothing` when:
- the file does not exist (never planned) — caller should call `recommend_plan`,
- `planVersion` differs from `PLAN_JSON_VERSION` (schema drift) — the plan is discarded, caller
  re-plans on the current code path,
- the JSON is malformed — same treatment as unknown version.

The `cecelia_version` and `saturation_fingerprint` fields on the returned plan are NOT compared to
`cecelia_version()` or the current meta — that's the caller's decision (a stale plan is still
readable; the caller decides whether "stale" means "re-plan" or "surface a warning").
"""
function load_plan(img::CciaImage)::Union{CorrectionPlan,Nothing}
    path = joinpath(img._dir, _PLAN_FILENAME)
    isfile(path) || return nothing
    raw = try
        JSON3.read(read(path, String))
    catch
        return nothing
    end
    return _plan_from_dict(raw)
end


function _plan_to_dict(plan::CorrectionPlan)::Dict{String,Any}
    Dict{String,Any}(
        "planVersion"           => PLAN_JSON_VERSION,
        "ceceliaVersion"        => plan.cecelia_version,
        "imageUid"              => plan.image_uid,
        "presetId"              => String(plan.preset_id),
        "wizardAnswers"         => _wizard_to_dict(plan.wizard_answers),
        "saturationFingerprint" => plan.saturation_fingerprint,
        "included"              => [_step_to_dict(s) for s in plan.included],
        "excluded"              => [_step_to_dict(s) for s in plan.excluded],
        "qcScores"              => [_score_to_dict(s) for s in plan.qc_scores],
    )
end

function _plan_from_dict(raw)::Union{CorrectionPlan,Nothing}
    ver = get(raw, :planVersion, nothing)
    ver === PLAN_JSON_VERSION || return nothing
    try
        CorrectionPlan(
            String(get(raw, :imageUid, "")),
            Symbol(get(raw, :presetId, "custom")),
            _wizard_from_dict(get(raw, :wizardAnswers, Dict{Symbol,Any}())),
            [_step_from_dict(s) for s in get(raw, :included, [])],
            [_step_from_dict(s) for s in get(raw, :excluded, [])],
            [_score_from_dict(s) for s in get(raw, :qcScores, [])];
            cecelia_version       = String(get(raw, :ceceliaVersion, "")),
            saturation_fingerprint = String(get(raw, :saturationFingerprint, "")),
        )
    catch
        return nothing
    end
end

# ── serialization helpers (one row each; kept together, small on purpose) ─────────────────────────

_wizard_to_dict(w::AbstractDict) = Dict{String,Any}(String(k) => _wizard_v_to_json(v) for (k, v) in w)
_wizard_v_to_json(v::Symbol)     = String(v)
_wizard_v_to_json(v)             = v

_wizard_from_dict(raw) = Dict{Symbol,Any}(Symbol(k) => _wizard_v_from_json(v) for (k, v) in raw)
_wizard_v_from_json(v::AbstractString) = Symbol(v)   # wizard values are enums; round-trip to Symbol
_wizard_v_from_json(v)                  = v

_step_to_dict(s::CorrectionStep) = Dict{String,Any}(
    "funName"         => s.fun_name,
    "params"          => Dict{String,Any}(String(k) => v for (k, v) in s.params),
    "orderWeight"     => s.order_weight,
    "source"          => String(s.source),
    "exclusionReason" => s.exclusion_reason,
)

function _step_from_dict(raw)::CorrectionStep
    CorrectionStep(
        String(get(raw, :funName, "")),
        Dict{String,Any}(String(k) => v for (k, v) in get(raw, :params, Dict{String,Any}()));
        order_weight     = Int(get(raw, :orderWeight, 0)),
        source           = Symbol(get(raw, :source, "rule_default")),
        exclusion_reason = let r = get(raw, :exclusionReason, nothing); r === nothing ? nothing : String(r) end,
    )
end

_score_to_dict(r::QCResult) = Dict{String,Any}(
    "metric" => r.metric,
    "score"  => isnan(r.score) ? nothing : r.score,   # JSON has no NaN; null represents QC_SCORE_ABSENT
    "level"  => r.level,
    "subs"   => Dict{String,Any}(String(k) => v for (k, v) in r.subs),
    "scope"  => String(r.scope),
)

function _score_from_dict(raw)::QCResult
    s = get(raw, :score, nothing)
    QCResult(
        String(get(raw, :metric, "")),
        s === nothing ? QC_SCORE_ABSENT : Float64(s);
        level = String(get(raw, :level, "info")),
        subs  = Dict{Symbol,Any}(Symbol(k) => v for (k, v) in get(raw, :subs, Dict{Symbol,Any}())),
        scope = Symbol(get(raw, :scope, "image")),
    )
end
