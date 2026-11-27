# segment.afCells — per-cell autofluorescence correction for cyclic multiplex panels.
#
# Every marker read through one fluorophore carries that cell's autofluorescence in the fluorophore's
# band, so on a cyclic panel the markers on one dye rise and fall together. This task estimates each
# cell's background per dye from its OTHER markers on that dye and writes corrected copies of the
# intensities as `af_{measure}_intensity_{i}` feature columns beside the measured ones (which stay
# untouched). Channel-name resolution then offers them everywhere as `af_<channel>`. The method and its
# validation: `python/cecelia/utils/cell_af_utils.py`, docs/DATAMODEL.md → *Autofluorescence-corrected
# intensities*.

struct AfCells <: CciaTask end
register_builtin_task!("segment.afCells", AfCells(); spec = joinpath(@__DIR__, "af_cells.json"))

const AF_CELLS_GROUP_BY = ("dye", "order")

Base.@kwdef struct AfCellsParams
    valueName::String       = VERSIONED_DEFAULT_VAL
    groupBy::String         = "dye"
    period::Int             = 3
    nuclear::Vector{String} = String[]
    exclude::Vector{String} = String[]
end

function parse_af_cells_params(d::AbstractDict)::AfCellsParams
    group_by = string(get(d, "groupBy", "dye"))
    group_by in AF_CELLS_GROUP_BY ||
        throw(ParamValidationError("groupBy must be one of $(join(AF_CELLS_GROUP_BY, ", ")), got \"$group_by\""))
    period = Int(get(d, "period", 3))
    period >= 2 || throw(ParamValidationError("period must be at least 2, got $period"))
    strs(k) = String[string(x) for x in something(get(d, k, String[]), String[])]
    AfCellsParams(; valueName = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)), groupBy = group_by,
                  period = period, nuclear = strs("nuclear"), exclude = strs("exclude"))
end

# A nuclear stain is on its own dye; in "order" mode it would otherwise join the marker group it lines up
# with. The *Nuclear stains* param names them; this name match is only its default (the run log says
# what it picked): DAPI / Hoechst / DRAQn / SYTOX alone or after a prefix (`Ch1_DAPI`), `nuc-…`, and the
# CyCIF/MCMICRO `DNA_n`.
const AF_NUCLEAR_NAME = r"(^|[-_ ])(dapi|hoechst|draq\d|sytox)|^nuc[-_]|^dna([-_ ]?\d+)?$"i   # cf. mcp _classify_stain

"""
    af_nuclear_channels(names) -> Vector{Int}

0-based indices of channels named like a nuclear stain (DAPI, Hoechst, DRAQ5, …).
"""
af_nuclear_channels(names::AbstractVector) =
    [i - 1 for (i, n) in enumerate(names) if occursin(AF_NUCLEAR_NAME, strip(string(n)))]

"""
    af_order_groups(names; period = 3, nuclear = af_nuclear_channels(names)) -> Vector{Union{String,Nothing}}

"Channel order" grouping, for a stack with no dye record: nuclear stains (`nuclear`, 0-based) are
taken out first — they get no group — and the remaining MARKERS are grouped by position, the k-th with
the k ± `period`-th (`period` = markers per cycle; a cycle images the same dyes in the same order).
Counting markers, not stack slots, is what holds whether a stain leads the stack, is re-imaged every
cycle, or sits once mid-stack (a registered stack keeps only the reference cycle's). Leaving a marker
as measured never changes the count (the runner skips it on its own). The one input it cannot check:
which channels are nuclear stains — named by the user, else guessed by name.
"""
function af_order_groups(names::AbstractVector; period::Integer = 3,
                         nuclear = af_nuclear_channels(names))::Vector{Union{String,Nothing}}
    nuc = Set(nuclear)
    out = Union{String,Nothing}[nothing for _ in names]
    k = 0
    for i in 0:length(names)-1
        i in nuc && continue
        out[i + 1] = string(k % period); k += 1
    end
    out
end

"""
    af_channel_groups(acquisition, n_channels) -> Vector{Union{String,Nothing}}

"Dye" grouping: one dye key per channel (0-based order), `nothing` where none is known — each channel's
`dyePreset` from the acquisition record, else its emission filter (the band IS what the
autofluorescence shares).
"""
function af_channel_groups(acquisition, n_channels::Integer)::Vector{Union{String,Nothing}}
    out = Union{String,Nothing}[nothing for _ in 1:n_channels]
    chans = _ag(acquisition, "channels")
    chans isa AbstractVector || return out
    for (i, ch) in enumerate(chans)
        i > n_channels && break
        dye = something(_ag(ch, "dyePreset"), _ag(ch, "emissionFilter"), Some(nothing))
        out[i] = (dye === nothing || isempty(strip(string(dye)))) ? nothing : string(dye)
    end
    out
end

"""
    af_cells_qc_findings(qc) -> Vector{Dict}

`qc` is the runner's summary. Warns when nothing could be corrected (no dye carries 3 markers), or when
the markers on one dye still move together after correction — less than half of their shared signal
removed, so the grouping is probably wrong (a dye mis-recorded, or the wrong channel order).
"""
function af_cells_qc_findings(qc::AbstractDict)::Vector{Dict{String,Any}}
    findings = Dict{String,Any}[]
    if Int(get(qc, "nCorrected", 0)) == 0
        push!(findings, qc_finding("warn", "afcells.nothing_to_correct"))
        return findings
    end
    before = get(qc, "sameDyeBefore", nothing); after = get(qc, "sameDyeAfter", nothing)
    if before isa Real && after isa Real && before >= 0.15 && after > 0.5 * before
        push!(findings, qc_finding("warn", "afcells.little_removed";
                                   detail = Dict{String,Any}("before" => before, "after" => after)))
    end
    findings
end

function _run_task(::AfCells, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_af_cells_params(params)
    props = img_label_props_path(img, p.valueName)
    isfile(props) || (on_log("[ERROR] afCells: no cell table for '$(p.valueName)' — measure it first"); return nothing)

    names = something(channel_names(img; value_name = nothing), String[])
    exclude = channel_indices(isempty(p.exclude) ? nothing : p.exclude, names; what = "exclude")
    if p.groupBy == "order"
        nuc = isempty(p.nuclear) ? af_nuclear_channels(names) :
              channel_indices(p.nuclear, names; what = "nuclear stains")
        groups = af_order_groups(names; period = p.period, nuclear = nuc)
        on_log(isempty(nuc) ?
               ">> afCells: no nuclear stain in the channel order — set 'Nuclear stains' if there is one" :
               ">> afCells: nuclear stains outside the marker cycle: $(join(names[nuc .+ 1], ", "))" *
               (isempty(p.nuclear) ? " (by name)" : ""))
    else
        raw = read_ccid_raw(state_file(img._dir))
        groups = af_channel_groups(_ag(_ag(raw, "meta"), "acquisition"), length(names))
    end
    if p.groupBy == "dye" && all(isnothing, groups)
        on_log("[ERROR] afCells: this image records no dye per channel — set 'Group channels by' to Channel order")
        return nothing
    end
    on_progress(1, 3)

    qc_out_path = joinpath(task_run_dir(img._dir), "af_cells_qc.json")
    ok = run_py("tasks/segment/af_cells_run.py",
                Dict{String,Any}("propsPath" => props, "groups" => groups, "exclude" => exclude,
                                 "qcOutPath" => qc_out_path),
                task_run_dir(img._dir); reads = [props], writes = [props],
                on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || (on_log("[ERROR] afCells: Python runner failed"); return nothing)

    n_corrected = 0
    try
        qc = JSON3.read(read(qc_out_path, String), Dict{String,Any})
        n_corrected = Int(get(qc, "nCorrected", 0))
        metrics = Dict{String,Any}(k => qc[k] for k in ("nCells", "nCorrected", "sameDyeBefore", "sameDyeAfter")
                                   if get(qc, k, nothing) !== nothing)
        write_qc(img, "segment.afCells", p.valueName, af_cells_qc_findings(qc); metrics = metrics)
        b = get(qc, "sameDyeBefore", nothing); a = get(qc, "sameDyeAfter", nothing)
        (b isa Real && a isa Real) &&
            on_log("[QC] markers on one dye correlated $(round(b; digits = 2)) before, $(round(a; digits = 2)) after.")
    catch e
        on_log("[QC] could not compute afCells QC: $e")
    end
    on_progress(3, 3)
    Dict{String,Any}("valueName" => p.valueName, "nCorrected" => n_corrected)
end
