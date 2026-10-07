# ── Segmentation label-store conventions (algorithm-agnostic) ─────────────────
#
# The Julia-side counterpart to `python/cecelia/utils/segmentation_utils.py`. On the Python side the
# algorithm is already a swappable detail: `SegmentationUtils` owns the tiled T × XY loop, global
# label IDs, seam stitching, post-processing, nuc↔cyto matching and the label-zarr writing, and a
# concrete backend only implements `predict_slice` (`CellposeUtils` today; "cellpose, stardist, etc."
# per its own docstring). This file is the same idea for the Julia handler: everything a segmentation
# task does with its label output that is NOT specific to one algorithm lives here, so a second
# backend adds a `_run_task` + its own param resolution and nothing else.
#
# What stays in the task's own `.jl`: resolving the input image, translating params into the backend's
# arguments, and any model/checkpoint lookup (e.g. `cellpose_model_path`, `BUILTIN_CELLPOSE_MODELS`).
# What lives here: the output filename convention, registering the result in `ccid.json`, the live
# preview declaration, and the QC findings.
#
# See docs/SEGMENTATION.md.

"""
    segment_label_files(out_value_name, models) -> Vector{String}

The label zarr filenames a segmentation run writes for `out_value_name`: the primary `base` type
becomes `{vn}.zarr`, and every other `matchAs` type gets a `{vn}_{ma}.zarr` sibling.

This mirrors `_store_path` in `segmentation_utils.py` — the writer — and is the ONE derivation shared
by the post-run `ccid.json` registration and the live-preview declaration, so a preview can never
name a different file than the run actually produces. `models` is the raw param value: a
`JSON3.Object` from the WS, a `Dict` from the REPL, or `nothing` (treated as a single `base` model).
"""
function segment_label_files(out_value_name::AbstractString, models)::Vector{String}
    match_as = models isa AbstractDict ?
        unique([string(get(m, "matchAs", "base")) for (_, m) in models if m isa AbstractDict]) :
        String["base"]
    isempty(match_as) && (match_as = String["base"])
    vcat(["$(out_value_name).zarr"],
         ["$(out_value_name)_$(ma).zarr" for ma in match_as if ma != "base"])
end

"""
    segment_live_outputs(params) -> Vector{LiveOutput}

The `live_outputs` declaration for any segmentation backend that streams through
`SegmentationUtils`: its label stores are created at full shape before the first frame and filled one
timepoint at a time, so a viewer can show them mid-run. Each segmentation task opts in with a
one-line overload — deliberately per-task rather than inherited, because writing-as-you-go is a
property of the backend, not of segmentation (`segment.branching` assembles its store in RAM and
writes it once at the end, so it has nothing to watch and correctly declares nothing).

    live_outputs(::MySegment, params::AbstractDict) = segment_live_outputs(params)

The declared `files` are the STAGING stores (`{vn}.zarr.partial`), not the final ones. A run writes
through `zarr_utils.staged_store`, so while it is going the final path either doesn't exist yet or —
on a re-run — still holds the PREVIOUS segmentation, and a preview aimed there would quietly show the
old labels while the new ones are being computed. `value_name` is carried separately so the viewer
still names the layer `({vn})`, which is what the recolour and layer-eviction logic match on.
"""
function segment_live_outputs(params::AbstractDict)::Vector{LiveOutput}
    out_value_name = string(get(params, "outputValueName", VERSIONED_DEFAULT_VAL))
    label_files = segment_label_files(out_value_name, get(params, "models", nothing))
    LiveOutput[(kind = "labels", value_name = out_value_name,
                files = staging_store_path.(label_files))]
end

"""
    register_label_files!(img, out_value_name, label_files)

Record a completed run's label zarrs in the image's `ccid.json` `labels` dict
(`{value_name => [filename, …]}`). This is what makes the set appear in every `labels` picker, and
it happens only on success — which is exactly why an in-progress store needs `live_outputs` to be
discoverable at all.

Rewrites `ccid.json` in place rather than going through `save!`, because the running task holds a
`CciaImage` loaded before the run and saving the whole object would clobber any field a
concurrently-running task on the same image has since written.

Goes through `commit_state!` (`model/project.jl`), which does that read-modify-write **inside the
image's transaction** — the completion of the same thought. Rewriting in place instead of `save!`
narrows the clobber to one field, but two concurrent registrations still both read the old `labels`
dict and the second write drops the first's entry; the transaction is what makes registering an output
atomic against a concurrent registration. It also gets `write_json_atomic` (so a write killed
mid-flight can't leave a half-written `ccid.json` — with no per-image guard in `_load_set`, ONE such
file makes the WHOLE project fail to open, every other image intact but unreachable; see #420) and
`state_file`, so the filename is never joined at a call site.
"""
function register_label_files!(img::CciaImage, out_value_name::AbstractString,
                              label_files::Vector{String})
    commit_state!(img) do raw
        unversioned_set_field!(raw, "labels", label_files, String(out_value_name); set_active = false)
    end
    label_files
end

"""
    segment_qc_findings(counts) -> (findings, primary_count)

Pure QC helper (drift pattern): objective per-type segment counts → advisory findings + the primary
(`base`) count. Only the unambiguous "0 cells" case is a finding; the counts themselves are banked as
metrics by the caller. Algorithm-agnostic — every segmentation backend produces per-type counts.
"""
function segment_qc_findings(counts::AbstractDict)
    primary = haskey(counts, "base") ? counts["base"] :
              (isempty(counts) ? 0 : first(values(counts)))
    findings = primary == 0 ?
        [qc_finding("warn", "segment.no_cells", "No cells segmented",
            "Segmentation produced no objects — check the channels/diameter and re-run this step.")] :
        Dict{String,Any}[]
    findings, primary
end

# ── Segmentation shape QC (`seg.*`) — findings that point back a step ─────────────────────────────
#
# docs/todo/TASK_DISCOVERY_PLAN.md Decision 6 / P4. A segmenter run on noisy or uneven input tends to
# be TUNED (diameter, thresholds, minimum size) when the fix is upstream, in Cleanup. These findings
# say so at the moment it matters, and their `long` text names the earlier step to check first
# (qc/text.jl).
#
# Inputs are only what the runner already had in hand: per-object voxel counts from the `np.unique`
# that counts the labels, summarised by `segmentation_utils.object_stats` into per-frame counts and the
# percentiles of each object's equivalent diameter (circle of equal area in 2D, sphere of equal volume
# in 3D). The size findings need the diameter the user GAVE (cellpose's `cellDiameter`); a segmenter
# with no diameter param (coastal) gets only the count-stability one.
#
# **Every band below is an unvalidated placeholder.** They were picked from geometry (what a 2-cell
# merge does to an equivalent diameter: ×1.26 in 3D, ×1.41 in 2D) and counting noise, NOT calibrated on
# real data. They are `info`, never `warn`, until Dominik has looked at what fires on real crops (the
# P4 checkpoint; CLAUDE.md → *Real-data visual validation*). Move them here, nowhere else.
const SEG_QC_MIN_OBJECTS     = 20     # fewer objects than this → too few to say anything about sizes
const SEG_FRAG_MEDIAN_RATIO  = 0.5    # median eq. diameter below half the given diameter → fragmented
const SEG_TINY_RATIO         = 0.33   # an object below a third of the given diameter is "tiny"…
const SEG_TINY_FRAC          = 0.4    # …and this share of tiny objects → fragmented
const SEG_MERGE_RATIO        = 1.6    # an object above 1.6× the given diameter is "far larger"…
const SEG_MERGE_FRAC         = 0.15   # …and this share of them → merged
const SEG_UNSTABLE_MIN_FRAMES = 5     # fewer timepoints → no count-stability verdict
const SEG_UNSTABLE_MIN_MEAN   = 10    # fewer cells per frame on average → counting noise dominates
const SEG_UNSTABLE_STEP       = 0.2   # median frame-to-frame change above 20% of the count → unstable

"""
    _frac_below(q, x) -> Float64

Share of objects whose value is below `x`, read off `q` — the 101 percentiles (0..100) banked by
`object_stats` — by linear interpolation between neighbouring percentiles. Exact at the ends (0 below
the minimum, 1 above the maximum), ~1% resolution in between.
"""
function _frac_below(q::AbstractVector{<:Real}, x::Real)::Float64
    n = length(q)
    n < 2 && return isempty(q) ? 0.0 : Float64(x > q[1])
    x <= q[1] && return 0.0
    x > q[end] && return 1.0
    i = searchsortedlast(q, x)                      # q[i] ≤ x (ties: the LAST equal percentile)
    # A run of equal percentiles is a point mass: everything in it is NOT below x when x sits on it.
    q[i] == x && return (searchsortedfirst(q, x) - 1) / (n - 1)
    (i - 1 + (x - q[i]) / (q[i + 1] - q[i])) / (n - 1)
end

"""
    seg_object_qc_findings(stats; diameter_um = nothing) -> Vector{Dict}

Pure QC helper: a runner's `objectStats` (see `segmentation_utils.object_stats`) → advisory `seg.*`
findings. `diameter_um` is the cell diameter the user gave the segmenter, or `nothing` when the
backend has none — then only `seg.counts_unstable` can fire. All three are `info`, with the measured
numbers in `detail`. Bands: the `SEG_*` placeholders above.
"""
function seg_object_qc_findings(stats; diameter_um::Union{Nothing,Real} = nothing)
    findings = Dict{String,Any}[]
    stats isa AbstractDict || return findings
    _get(k) = get(stats, k, get(stats, Symbol(k), nothing))

    q = _get("eqDiameterUm")
    n_obj = something(_get("nObjects"), 0)
    if !isnothing(diameter_um) && diameter_um > 0 && q isa AbstractVector && length(q) >= 2 &&
       n_obj >= SEG_QC_MIN_OBJECTS
        qv = Float64.(collect(q))
        d = Float64(diameter_um)
        median_ratio = qv[cld(length(qv), 2)] / d
        tiny_frac    = _frac_below(qv, SEG_TINY_RATIO * d)
        large_frac   = 1.0 - _frac_below(qv, SEG_MERGE_RATIO * d)
        detail = Dict{String,Any}("diameterUm" => d, "medianDiameterUm" => round(median_ratio * d; digits = 2),
                                  "nObjects" => n_obj)
        if median_ratio < SEG_FRAG_MEDIAN_RATIO || tiny_frac >= SEG_TINY_FRAC
            push!(findings, qc_finding("info", "seg.fragmented"; detail = merge(detail,
                Dict{String,Any}("medianRatio" => round(median_ratio; digits = 2),
                                 "tinyFrac" => round(tiny_frac; digits = 3)))))
        end
        if large_frac >= SEG_MERGE_FRAC
            push!(findings, qc_finding("info", "seg.merged"; pct = round(Int, large_frac * 100),
                detail = merge(detail, Dict{String,Any}("largeFrac" => round(large_frac; digits = 3)))))
        end
    end

    fc = _get("frameCounts")
    if fc isa AbstractVector && length(fc) >= SEG_UNSTABLE_MIN_FRAMES
        c = Float64.(collect(fc))
        mean_c = sum(c) / length(c)
        if mean_c >= SEG_UNSTABLE_MIN_MEAN
            steps = [abs(c[t] - c[t - 1]) / max((c[t] + c[t - 1]) / 2, 1.0) for t in 2:length(c)]
            med = sort(steps)[cld(length(steps), 2)]
            med >= SEG_UNSTABLE_STEP && push!(findings, qc_finding("info", "seg.counts_unstable";
                detail = Dict{String,Any}("medianStep" => round(med; digits = 3),
                                          "meanPerFrame" => round(mean_c; digits = 1),
                                          "nFrames" => length(c))))
        end
    end
    findings
end

"""
    seg_given_diameter(models) -> Union{Nothing,Float64}

The cell diameter (µm) the size findings compare against: the `cellDiameter` of the `base` model
group(s). `nothing` when there is none to compare with — no base group, a diameter ≤ 0 (cellpose's
own-estimate mode), or stacked base passes that disagree (a second pass with a smaller diameter is
deliberately finding smaller things, so "smaller than the diameter" would be the design, not a fault).
A group without the key reads 15, the runner's own default (`cellpose_utils.py`).
"""
function seg_given_diameter(models)::Union{Nothing,Float64}
    models isa AbstractDict || return nothing
    ds = Float64[]
    for (_, m) in models
        m isa AbstractDict || continue
        String(get(m, "matchAs", "base")) == "base" || continue
        push!(ds, Float64(get(m, "cellDiameter", 15)))
    end
    (isempty(ds) || length(unique(ds)) > 1 || ds[1] <= 0) && return nothing
    ds[1]
end

"""
    bank_segment_qc!(img, fun_name, out_value_name, qc_out_path; diameter_um, on_log)

Shared tail of every segmenter: read the runner's `qcOutPath` JSON, turn its counts + object stats
into findings (`segment_qc_findings` + `seg_object_qc_findings`), bank them under `fun_name`, and log
one `[QC]` line per finding. Advisory and best-effort — a failure is logged, never raised.
"""
function bank_segment_qc!(img::CciaImage, fun_name::AbstractString, out_value_name::AbstractString,
                          qc_out_path::AbstractString; diameter_um = nothing,
                          on_log::Function = _ -> nothing)
    isfile(qc_out_path) || return nothing
    try
        qmeta  = JSON3.read(read(qc_out_path, String))
        counts = Dict{String,Any}(String(k) => Int(v) for (k, v) in get(qmeta, :labelCounts, ()))
        findings, primary = segment_qc_findings(counts)
        stats = get(qmeta, :objectStats, nothing)
        seg = seg_object_qc_findings(stats; diameter_um = diameter_um)
        append!(findings, seg)
        metrics = Dict{String,Any}("nCells" => primary, "byType" => counts)
        isnothing(diameter_um) || (metrics["diameterUm"] = diameter_um)
        write_qc(img, fun_name, out_value_name, findings; metrics = metrics)
        on_log("[QC] segmented $primary cell(s)" *
               (length(counts) > 1 ? " ($(join(["$k=$v" for (k, v) in counts], ", ")))" : "") * ".")
        # One line per shape finding, code first: the task log is where a user (or an agent) reading
        # the run looks, and the code prefix is what the MCP discovery toggle filters on.
        for f in seg
            on_log("[QC] $(f["code"]): $(f["short"]) — $(f["long"])")
        end
    catch e
        on_log("[QC] could not compute segment QC: $e")
    end
    nothing
end
