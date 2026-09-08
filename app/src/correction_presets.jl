# Correction-plan preset cards — §5 of docs/todo/CORRECTION_QC_PLAN.md.
#
# Each card is a named starting-point over the SAME parameter set the wizard populates and the chain
# executor runs (§5 constraint — no parallel data model). Consumed by `apply_rules` in
# `correction_plan.jl`; the rule table's order weights (§1) are authoritative, so `order_hints` here
# is a listing convention, not the enforced order.
#
# **All cards are unvalidated** — per 1d §4 there is no reference-acquisition-property dataset. Every
# card carries `validation_status = :unvalidated` until at least one dev/ image attests to the card's
# regime (see the plan doc "Cards + ground truth (the honest bit)").


"""
    AcquisitionPreset(id, name, description, params_by_task, order_hints,
                      hard_commitments, validation_status)

A named preset. `params_by_task` maps `fun_name => Dict(param_name => value)` — the values the
card ships as its starting point. `order_hints` is the fun list in the order the card lists them,
for a stable UI ordering within a bucket. `hard_commitments` is the subset of `(fun_name, param_name)`
pairs that DEFINE the card; changing one should re-classify the image to a different card. The
recommender that picks a card lives in `correction_plan.jl` (not on the preset), keeping presets as
pure data.
"""
struct AcquisitionPreset
    id::Symbol
    name::String
    description::String
    params_by_task::Dict{String,Dict{String,Any}}
    order_hints::Vector{String}
    hard_commitments::Set{Tuple{String,String}}
    validation_status::Symbol  # :unvalidated | :validated
end


# ── Seed cards (5) ────────────────────────────────────────────────────────────────────────────────

_resonance_preset() = AcquisitionPreset(
    :resonance,
    "Resonance / photon-limited",
    "Fast dwell, single-digit photon counts per pixel. Smooth before AF; denoise seeded (SUPPORT) " *
    "when the vault holds a trained model and channels are not saturated — model picked at the " *
    "task widget's vault picker.",
    Dict{String,Dict{String,Any}}(
        "cleanupImages.smooth" => Dict{String,Any}(
            "spatialMethod"  => "bilateral_vst",  # PR #777 — hard
            "temporalStat"   => "median",          # commit 95cb553f — starting point
            "temporalFrames" => 3,                 # starting point
        ),
        "cleanupImages.driftCorrect" => Dict{String,Any}(
            "driftEstimator" => "multiLag",
        ),
        # denoise ships no params from the card — the SUPPORT model is picked at the task widget
        # (vault picker). The rule engine gates it on `denoise.vault_model_present` +
        # `denoise.channel_saturated_frac`; either failing turns the card seed into an excluded row
        # with the reason attached, so the audit trail records why it did not run.
        "cleanupImages.denoise" => Dict{String,Any}(),
    ),
    ["cleanupImages.smooth", "cleanupImages.driftCorrect", "cleanupImages.denoise"],
    Set{Tuple{String,String}}([
        ("cleanupImages.smooth", "spatialMethod"),  # bilateral_vst defines the card
    ]),
    :unvalidated,
)

_galvo_preset() = AcquisitionPreset(
    :galvo,
    "Galvo / clean signal",
    "High-SNR. Smooth off; gaussian if smoothing is added later. AF via triangle background when " *
    "afCombinations are declared.",
    Dict{String,Dict{String,Any}}(
        "cleanupImages.driftCorrect" => Dict{String,Any}(
            "driftEstimator" => "multiLag",
        ),
        # smooth intentionally absent — off by default; a card that *turns smooth on* would be the
        # resonance card.
    ),
    ["cleanupImages.driftCorrect", "cleanupImages.afCorrect"],
    Set{Tuple{String,String}}(),
    :unvalidated,
)

_spinning_disk_preset() = AcquisitionPreset(
    :spinning_disk,
    "Spinning-disk / live-cell / fast timelapse",
    "Short frames, translation-only drift, minimal spatial noise. Temporal mean inflates masks " *
    "~34% (commit 95cb553f) — smooth stays off by default.",
    Dict{String,Dict{String,Any}}(
        "cleanupImages.driftCorrect" => Dict{String,Any}(
            "driftEstimator" => "multiLag",
        ),
    ),
    ["cleanupImages.driftCorrect"],
    Set{Tuple{String,String}}(),
    :unvalidated,
)

_deep_3d_preset() = AcquisitionPreset(
    :deep_3d,
    "Deep 3D / breathing-affected",
    "Z-stacks with intra-stack offset AND depth-dependent inter-frame motion (breathing shear). " *
    "stackAlign (intra-stack per-frame anchor) composes with driftCorrect(driftPerPlane) " *
    "(inter-frame per-Z-plane rigid): the two target different axes and do not double-count. " *
    "Pilot zolIMa/x4E5HU (PR #818) — per-plane σ=0 dropped deep-plane residuals from ±8–10 px to " *
    "<3 px on the same overlay; raise driftZSmoothness only if planes still jump.",
    Dict{String,Dict{String,Any}}(
        "cleanupImages.stackAlign" => Dict{String,Any}(
            "referenceMode" => "middle",   # matches stack_align.json default
            "minConfidence" => 0.35,        # STACK_ALIGN_APPLIED_FRAC_WARN
        ),
        "cleanupImages.driftCorrect" => Dict{String,Any}(
            "driftEstimator"    => "multiLag",
            "driftPerPlane"     => true,   # PR #818 — the breathing-shear case is why the card exists
            "driftZSmoothness"  => 0.0,     # starting point per drift_correct.json tip (raise if jump)
        ),
    ),
    ["cleanupImages.stackAlign", "cleanupImages.driftCorrect"],
    Set{Tuple{String,String}}([
        ("cleanupImages.stackAlign", "referenceMode"),   # stackAlign presence defines the card
        ("cleanupImages.driftCorrect", "driftPerPlane"), # per-plane defines the breathing regime
    ]),
    :unvalidated,
)

_custom_preset() = AcquisitionPreset(
    :custom,
    "Custom / no preset",
    "Empty starting point. Structural rules still apply (T→driftCorrect, Z-absent excludes " *
    "stackAlign); every other task is user-pick.",
    Dict{String,Dict{String,Any}}(),
    String[],
    Set{Tuple{String,String}}(),
    :unvalidated,
)


const CORRECTION_PRESETS = Dict{Symbol,AcquisitionPreset}(
    :resonance     => _resonance_preset(),
    :galvo         => _galvo_preset(),
    :spinning_disk => _spinning_disk_preset(),
    :deep_3d       => _deep_3d_preset(),
    :custom        => _custom_preset(),
)


"""
    preset_by_id(id) -> AcquisitionPreset

Look up a card. Unknown ids fall through to `:custom` — the plan doc's designated fallback for
low-confidence recommendations. This keeps `apply_rules` total: a stale plan.json referencing a
retired card still yields a workable plan.
"""
function preset_by_id(id::Symbol)::AcquisitionPreset
    get(CORRECTION_PRESETS, id, CORRECTION_PRESETS[:custom])
end

preset_ids() = collect(keys(CORRECTION_PRESETS))
