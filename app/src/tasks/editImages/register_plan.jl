# Pure helpers for `editImages.register` (register.jl): which source channel lands at which output
# position, what each output channel is called, and the per-cycle QC. Kept out of the handler so the
# rules are unit-tested without a store.

# Output channel order, as `(image index, 0-based channel index)` pairs: every reference channel, then
# each moving cycle's channels in their own order — its alignment channel only with `keep_reg`. The
# Python runner builds the same order (`register_run.output_plan`); the two must agree, or names land
# on the wrong channels.
function _register_channel_plan(n_channels::AbstractVector{<:Integer}, reg_idx::AbstractVector{<:Integer},
                                keep_reg::Bool)::Vector{Tuple{Int,Int}}
    plan = Tuple{Int,Int}[(1, c) for c in 0:(n_channels[1] - 1)]
    for i in 2:length(n_channels), c in 0:(n_channels[i] - 1)
        (keep_reg || c != reg_idx[i]) && push!(plan, (i, c))
    end
    plan
end

# Output channel names for `plan`: each channel keeps its marker name; a name that occurs more than
# once in the output (the alignment marker every cycle carries, or a marker stained twice) gets
# " (cycle N)" — N the image's position in the selection, the reference being cycle 1. A channel with
# no registered name is "ch<c>".
function _register_channel_names(names_per_img::AbstractVector, plan::AbstractVector{Tuple{Int,Int}})::Vector{String}
    base = String[(c + 1 <= length(names_per_img[i]) ? string(names_per_img[i][c + 1]) : "ch$c")
                  for (i, c) in plan]
    counts = Dict{String,Int}()
    for b in base; counts[b] = get(counts, b, 0) + 1; end
    String[counts[b] > 1 ? "$b (cycle $(plan[k][1]))" : b for (k, b) in enumerate(base)]
end

# Residual (µm) past which a cycle reads as misaligned: half a nucleus — beyond it a cell's pixels in a
# moving cycle's markers come from its neighbour in the reference. Same value as the runner's
# `register_utils.RESIDUAL_WARN_UM`, which only uses it for its log line.
const REGISTER_RESIDUAL_WARN_UM = 3.0
# Fraction of the reference's tissue a cycle must cover / still show. Below it, a fifth or more of
# the sample has no marker data from that cycle (the scan did not reach it, or the section lifted or
# tore between cycles) and every measure on those cells reads that cycle's markers as 0.
const REGISTER_TISSUE_WARN_FRAC = 0.8

_reg_num(x) = (x isa Real && isfinite(x)) ? Float64(x) : nothing

# One warn per cycle and failure kind, from the runner's per-cycle checks (`register_utils.cycle_checks`
# + `describe`): misaligned, tissue not covered, tissue lost. `cycles` entries carry `cycle` (1-based
# position in the selection, the reference being 1) and `name`.
function _register_qc_findings(cycles::AbstractVector)::Vector{Dict{String,Any}}
    out = Dict{String,Any}[]
    for c in cycles
        cyc, name = _ag(c, "cycle"), string(something(_ag(c, "name"), ""))
        res = _reg_num(_ag(something(_ag(c, "residualUm"), Dict()), "median"))
        if !isnothing(res) && res > REGISTER_RESIDUAL_WARN_UM
            push!(out, qc_finding("warn", "register.misaligned"; value = cyc,
                detail = Dict{String,Any}("cycle" => cyc, "name" => name, "residualUm" => res)))
        end
        cov = _reg_num(_ag(c, "tissueCovered"))
        if !isnothing(cov) && cov < REGISTER_TISSUE_WARN_FRAC
            push!(out, qc_finding("warn", "register.tissue_not_covered"; value = cyc,
                detail = Dict{String,Any}("cycle" => cyc, "name" => name, "tissueCovered" => cov)))
        end
        ret = _reg_num(_ag(c, "tissueRetained"))
        if !isnothing(ret) && ret < REGISTER_TISSUE_WARN_FRAC
            push!(out, qc_finding("warn", "register.tissue_lost"; value = cyc,
                detail = Dict{String,Any}("cycle" => cyc, "name" => name, "tissueRetained" => ret)))
        end
    end
    out
end

# Set-level numbers: `nCycles` (images registered, the reference included — also when the whole-plane
# path wrote no per-cycle checks) and the worst cycle on each check. Signal and sharpness ratios are banked per cycle
# (in the QC doc's `cycles`) with no finding: stain and focus differ between cycles for reasons that
# are not failures (a different exposure, a thicker section), so they are read in company.
function _register_qc_metrics(cycles::AbstractVector, n_images::Integer)::Dict{String,Any}
    m = Dict{String,Any}("nCycles" => Int(n_images))
    pick(f) = filter(!isnothing, [f(c) for c in cycles])
    res = pick(c -> _reg_num(_ag(something(_ag(c, "residualUm"), Dict()), "median")))
    isempty(res) || (m["worstResidualUm"] = maximum(res))
    cov = pick(c -> _reg_num(_ag(c, "tissueCovered")))
    isempty(cov) || (m["minTissueCovered"] = minimum(cov))
    ret = pick(c -> _reg_num(_ag(c, "tissueRetained")))
    isempty(ret) || (m["minTissueRetained"] = minimum(ret))
    # tissue a cycle imaged that the reference's scan did not: no finding — it says which cycle would
    # have made the better reference, not that anything failed
    only = pick(c -> _reg_num(_ag(c, "tissueOnlyInCycle")))
    isempty(only) || (m["maxTissueOnlyInCycle"] = maximum(only))
    m
end

# The registered image's `meta["acquisition"]`: the reference's settings, with one channel entry per
# output channel taken from the image it came from (`plan`), its `cycle` set to that image's position
# in the selection (the scan's own cycle number moves to `scanCycle`: one scan per cycle numbers each
# scan's cycle 1). So per-channel facts — the scanner's `afSubtracted`, exposure, filters — follow the
# channel into the stacked image, where the import probe and AF correction read them by index.
# `nothing` when the reference has no acquisition or any image's channel list does not cover the plan.
function _register_acquisition(acqs::AbstractVector, plan::AbstractVector{Tuple{Int,Int}})
    ref = acqs[1]
    ref isa AbstractDict || return nothing
    chans_of(a) = a isa AbstractDict ? something(_ag(a, "channels"), nothing) : nothing
    out = _plain_copy(ref)        # nested settings as plain String-keyed dicts, not JSON3 objects
    channels = Any[]
    for (i, c) in plan
        cs = chans_of(acqs[i])
        (cs isa AbstractVector && c + 1 <= length(cs) && cs[c + 1] isa AbstractDict) || return nothing
        ch = _plain_copy(cs[c + 1])
        haskey(ch, "cycle") && (ch["scanCycle"] = ch["cycle"])
        ch["cycle"] = i
        push!(channels, ch)
    end
    out["channels"] = channels
    out["cycles"] = Dict{String,Any}("count" => length(acqs), "registered" => true)
    modes = String[string(m) for m in something(_ag(ref, "modes"), Any[])]
    "cyclic" in modes || push!(modes, "cyclic")
    out["modes"] = modes
    out
end

# The picker (`channelSelection`, `multiple: false`) may hand back a one-element list; a REPL caller a
# bare name. Either way one name — `string(["DAPI"])` would be the name `["DAPI"]`.
_register_channel_name(v) = v isa AbstractVector ? (isempty(v) ? "" : string(first(v))) :
                            isnothing(v) ? "" : string(v)

# The per-cycle checks the slide path wrote (`qcOutPath`), as `(cycles, findings, metrics)` for the
# new image's QC; each cycle gains `cycle` (its position, the reference being 1), `uid` and `name`. The
# whole-plane (sitkibex) path writes none: no cycles, and metrics carry only the image count.
function _register_qc(ordered::Vector{CciaImage}, qc_out_path::AbstractString; on_log::Function = println)
    cycles = Dict{String,Any}[]
    if isfile(qc_out_path)
        try
            doc = JSON3.read(read(qc_out_path, String))
            for c in get(doc, :cycles, [])
                d = Dict{String,Any}(String(k) => v for (k, v) in c)
                i = Int(get(d, "cycle_index", 0)) + 1          # runner's 0-based position → cycle N
                d["cycle"] = i
                1 <= i <= length(ordered) && (d["uid"] = ordered[i].uid; d["name"] = ordered[i].name)
                push!(cycles, d)
            end
        catch e
            on_log("[QC] could not read the per-cycle checks: $e")
        end
    end
    (cycles, _register_qc_findings(cycles), _register_qc_metrics(cycles, length(ordered)))
end
