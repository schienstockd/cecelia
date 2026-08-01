struct AfCorrect <: CciaTask end

"""
    af_combinations_for_python(params, raw) -> Dict

The `afCombinations` bag as the PYTHON side needs it: `divisionChannels` resolved from channel NAMES
to 0-based indices, and `quotientChannel` resolved to the combination's key (the channel being
corrected). Names come from the **default** version deliberately — a corrected variant inherits them
by versioned fallback and may carry no list of its own.

**Shared by the run and the task preview, and it has to be.** Exactly the shape of the cellpose bug
(`cellpose_models_for_python`): the frontend sends names, Python wants indices, and the preview sends
the frontend's params. Real saved params on a live project carry
`divisionChannels = ["CH4", "CD169-Kat"]` where `"CH4"` is not in that image's `imChannelNames` at all
— a stale name that silently resolves to nothing. Keeping one translator means the preview drops it
exactly where the run does, rather than the two disagreeing about which channels were used.
"""
function af_combinations_for_python(params::AbstractDict, raw::AbstractDict)::Dict{String,Any}
    channel_names_raw = versioned_get_field(raw, "imChannelNames", VERSIONED_DEFAULT_VAL)
    ch_names = channel_names_raw isa AbstractVector ?
               collect(String, channel_names_raw) : String[]

    af_combos_raw = get(params, "afCombinations", nothing)
    af_combos = Dict{String,Any}()
    (isnothing(af_combos_raw) || !(af_combos_raw isa AbstractDict)) && return af_combos

    for (k, v) in af_combos_raw
        entry = Dict{String,Any}(String(ck) => cv for (ck, cv) in v)

        # divisionChannels: channel names → 0-based indices (an already-translated index passes
        # through, so this is idempotent — a REPL or chain caller may hand back a converted dict)
        idx_channels = Int[]
        for ch in get(entry, "divisionChannels", [])
            if ch isa Integer
                push!(idx_channels, Int(ch))
                continue
            end
            idx = findfirst(==(String(ch)), ch_names)
            isnothing(idx) || push!(idx_channels, idx - 1)
        end
        entry["divisionChannels"] = idx_channels

        # quotientChannel: resolve name → 0-based index → use as the af_combos key
        raw_quot = get(entry, "quotientChannel", [])
        delete!(entry, "quotientChannel")
        combo_key = String(k)
        if !isempty(raw_quot)
            q = first(raw_quot)
            if q isa Integer
                combo_key = string(Int(q))
            else
                idx = findfirst(==(String(q)), ch_names)
                isnothing(idx) || (combo_key = string(idx - 1))
            end
        end
        af_combos[combo_key] = entry
    end
    af_combos
end

# AF correction is previewable: the worker runs `af_correct_frame` — the run's own per-voxel
# arithmetic — over the visible region, using globals derived from the whole image and cached
# (`PreviewState.af_stats`). See `task_previewable` in task.jl.
task_previewable(::AfCorrect) = true

# The preview sends the FRONTEND's params, so `divisionChannels` arrive as channel NAMES. Same hook and
# same reason as cellpose's: sharing the compute does not make the params shared.
function preview_params(::AfCorrect, params::AbstractDict, img::CciaImage)::Dict{String,Any}
    out = Dict{String,Any}(String(k) => v for (k, v) in params)
    out["afCombinations"] = af_combinations_for_python(params, read_ccid_raw(state_file(img)))
    out
end

"""
    af_qc_findings(per_channel) -> (findings, worst)

QC for AF correction, from the per-channel output stats the runner writes.

This task used to be QC-exempt, with a comment calling itself the weakest exemption in the codebase:
over-subtraction *does* have an objective signal — the fraction of voxels clipped — but nothing
reported it. Now that the output ceiling is derived rather than dialled in, that signal is exactly
what says whether the derivation landed, so the exemption is gone.

Two ways it can be wrong, in opposite directions:

* **ceiling too low** → bright structure flattens against the top of the range. `clippedFrac`.
* **ceiling too high** → the data crams into a handful of levels and quantisation is thrown away.
  `levelsUsed / levelsAvailable`. Measured under the percentile window this replaced: 99% of a real
  image landed in ~13 of 255 levels, which nothing ever flagged.

Advisory only, per `docs/MODULES.md` — never an `error`, never a gate.
"""
function af_qc_findings(per_channel::AbstractDict)
    findings = Vector{Dict{String,Any}}()
    worst_clipped, worst_levels = 0.0, 1.0
    for (ch, s) in sort(collect(per_channel); by = first)
        clipped = Float64(get(s, "clippedFrac", 0.0))
        used    = Float64(get(s, "levelsUsed", 0))
        avail   = max(1.0, Float64(get(s, "levelsAvailable", 1)))
        frac    = used / avail
        worst_clipped = max(worst_clipped, clipped)
        worst_levels  = min(worst_levels, frac)

        if clipped > 0.01
            push!(findings, Dict{String,Any}(
                "level" => "warn", "code" => "af-clipped",
                "short" => "Channel $ch clipped",
                "detail" => "$(round(clipped * 100; digits = 1))% of voxels hit the top of the " *
                            "range — bright structure is flattened. Try a lower background method."))
        end
        if frac < 0.2
            push!(findings, Dict{String,Any}(
                "level" => "warn", "code" => "af-low-range",
                "short" => "Channel $ch uses little of the range",
                "detail" => "$(Int(round(used))) of $(Int(round(avail))) levels — the corrected " *
                            "channel is quantised coarsely."))
        end
    end
    findings, (; clipped = worst_clipped, levels = worst_levels)
end

function _run_task(task::AfCorrect, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    ccid       = state_file(img)
    raw        = read_ccid_raw(ccid)

    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$value_name'")
        return nothing
    end

    proj_dir           = dirname(dirname(img._dir))
    im_path            = joinpath(proj_dir, "0", img.uid, string(filename))
    im_correction_path = joinpath(proj_dir, "0", img.uid, "ccidAfCorrected.ome.zarr")

    if !ispath(im_path)
        on_log("[ERROR] Input image not found: $im_path")
        return nothing
    end

    # Channel names → 0-based indices. Shared with the preview so the two cannot disagree about
    # which channels were used — see `af_combinations_for_python`.
    af_combos = af_combinations_for_python(params, raw)

    on_log("[INFO] Input:  $im_path")
    on_log("[INFO] Output: $im_correction_path")
    on_log("[INFO] Combinations: $(length(af_combos))")

    qc_out_path = joinpath(task_run_dir(img._dir), "af_output_stats.json")

    ok = run_py("tasks/cleanupImages/af_correct_run.py",
        (; imPath           = im_path,
           imCorrectionPath = im_correction_path,
           afCombinations   = af_combos,
           # the one remaining choice, global to every combination (was two percentiles per
           # combination plus a rescale window, all now derived — see `af_division_stats`)
           backgroundMethod = string(get(params, "backgroundMethod", "triangle")),
           qcOutPath        = qc_out_path),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    on_log("[INFO] AF correction complete.")

    out_value_name = _spec_output_value_name(task, "afCorrected")
    out_filename   = "ccidAfCorrected.ome.zarr"

    commit_state!(img) do raw
        versioned_set_field!(raw, "filepath", out_filename, out_value_name)
    end

    # QC: the derived ceiling is the one thing that can go wrong invisibly, and it has an objective
    # signal — how much of the output range got used, and how much was clipped away.
    if isfile(qc_out_path)
        try
            stats = JSON3.read(read(qc_out_path, String))
            per_ch = Dict{String,Any}(String(k) => Dict{String,Any}(String(m) => v for (m, v) in s)
                                      for (k, s) in stats)
            findings, worst = af_qc_findings(per_ch)
            write_qc(img, "cleanupImages.afCorrect", out_value_name, findings;
                     metrics = Dict{String,Any}("clippedFrac" => worst.clipped,
                                                "levelsUsedFrac" => worst.levels,
                                                "byChannel" => per_ch))
            on_log("[QC] clipped $(round(worst.clipped * 100; digits = 2))% of voxels; " *
                   "$(round(worst.levels * 100; digits = 1))% of the output range used.")
        catch e
            on_log("[QC] could not compute AF QC: $e")
        end
    end

    Dict{String,Any}("valueName" => out_value_name, "filename" => out_filename)
end
