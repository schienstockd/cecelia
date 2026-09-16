# Outbound QC record for one channel, parsed out of the per-channel stats block Python writes into
# `af_output_stats.json`. Typing this at the JSON boundary is what makes a Python-side key rename
# fail loudly instead of producing zero findings — a `get(s, "saturatedFrac", 0.0)` cannot tell an
# absent key from a genuinely-zero value. See docs/archive/comment-audit-findings.md → Tier 1
# boundary bags → af_qc_findings.
struct AfChannelStats
    saturatedFrac::Float64
    levelsUsed::Float64
    levelsAvailable::Float64
    bleedthrough::Dict{String,Float64}
end

function parse_af_channel_stats(d::AbstractDict)::AfChannelStats
    leaks = get(d, "bleedthrough", nothing)
    parsed_leaks = if leaks isa AbstractDict
        Dict{String,Float64}(string(k) => Float64(v) for (k, v) in leaks)
    else
        Dict{String,Float64}()
    end
    AfChannelStats(
        Float64(get(d, "saturatedFrac", 0.0)),
        Float64(get(d, "levelsUsed", 0)),
        max(1.0, Float64(get(d, "levelsAvailable", 1))),
        parsed_leaks)
end
"""
    af_qc_findings(per_channel) -> (findings, worst)

QC for AF correction from the per-channel output stats. Advisory only, per `docs/MODULES.md` — never
an `error`, never a gate.

Two findings, both about things the user can act on OUTSIDE this task:

* **saturated input** (`af.saturated_input`) — the channel was clipped at the sensor, before we saw
  it. No correction recovers a clipped voxel's true value; the action is at the microscope.
* **bleedthrough detected** (`af.bleedthrough`) — a derived, non-zero `alpha` for some source
  channel (`correction_utils.af_bleedthrough_alphas`), one finding per source. A leak is a property
  of the filter set; one image of a cohort differing from its peers is a real optics signal. No
  invented threshold: the coefficient is already floored at `AF_ALPHA_MIN` on the Python side, so
  anything reported is something the estimator was willing to claim.

Cohort metric `levelsUsedFrac` is banked but not gated (its threshold was retired). Full rationale,
rejected alternatives (a suppression finding; retention of `af-low-range` / `clippedFrac` / `ceiling`),
and the datasets that shaped these decisions: [`docs/todo/AF_CORRECTION_AUDIT.md`](../../../../docs/todo/AF_CORRECTION_AUDIT.md).
"""
function af_qc_findings(per_channel::AbstractDict)
    findings = Vector{Dict{String,Any}}()
    worst_saturated, worst_levels, worst_leak = 0.0, 1.0, 0.0
    for (ch, raw) in sort(collect(per_channel); by = first)
        raw isa AbstractDict || continue
        s = parse_af_channel_stats(raw)
        worst_saturated = max(worst_saturated, s.saturatedFrac)
        worst_levels    = min(worst_levels, s.levelsUsed / s.levelsAvailable)

        if s.saturatedFrac > 0.001
            # short = problem; long = the action; figures go in `detail` as a Dict. Every finding
            # is built via `qc_finding` + QC_TEXT — no ad-hoc detail strings.
            push!(findings, qc_finding("warn", "af.saturated_input"; channel = ch,
                detail = Dict{String,Any}(
                    "saturatedFrac" => round(s.saturatedFrac; digits = 5),
                    "saturatedPct"  => round(s.saturatedFrac * 100; digits = 3))))
        end

        # Bleedthrough INTO this channel, one finding per source — the sources are what the user would
        # go and look at, and collapsing them into one finding would hide which filter pair is leaking.
        for (src, alpha) in sort(collect(s.bleedthrough); by = first)
            worst_leak = max(worst_leak, alpha)
            push!(findings, qc_finding("warn", "af.bleedthrough"; channel = ch,
                value = string(round(alpha * 100; digits = 2), "%"),
                detail = Dict{String,Any}(
                    "sourceChannel" => src,
                    "alpha"         => round(alpha; digits = 5),
                    "alphaPct"      => round(alpha * 100; digits = 2))))
        end
    end
    findings, (; saturated = worst_saturated, levels = worst_levels, leak = worst_leak)
end
