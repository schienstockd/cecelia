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
    segment_label_files(out_value_name, models; halo = false) -> Vector{String}

The label zarr filenames a segmentation run writes for `out_value_name`: the primary `base` type
becomes `{vn}.zarr`, every other `matchAs` type gets a `{vn}_{ma}.zarr` sibling, and a run with a
halo (`haloSize > 0`) adds `{vn}_halo.zarr` — grown from the primary labels, same ids.

This mirrors `_store_path` in `segmentation_utils.py` — the writer — and is the ONE derivation shared
by the post-run `ccid.json` registration and the live-preview declaration, so a preview can never
name a different file than the run actually produces. `models` is the raw param value: a
`JSON3.Object` from the WS, a `Dict` from the REPL, or `nothing` (treated as a single `base` model).
"""
function segment_label_files(out_value_name::AbstractString, models; halo::Bool = false)::Vector{String}
    match_as = models isa AbstractDict ?
        unique([string(get(m, "matchAs", "base")) for (_, m) in models if m isa AbstractDict]) :
        String["base"]
    isempty(match_as) && (match_as = String["base"])
    vcat(["$(out_value_name).zarr"],
         ["$(out_value_name)_$(ma).zarr" for ma in match_as if ma != "base"],
         halo ? ["$(out_value_name)_halo.zarr"] : String[])
end

"""
    segment_model_reads(models; manifests = false) -> Vector{String}

The model files a segmenter's runner reads, for its `run_py` `reads` declaration (HPC dispatch): every
model given as a path (a custom checkpoint; a built-in is a name and needs no file), plus, with
`manifests`, the `<name>.json` beside it (coastal reads its training manifest from there).
"""
function segment_model_reads(models; manifests::Bool = false)::Vector{String}
    out = String[]
    models isa AbstractDict || return out
    for (_, m) in models
        m isa AbstractDict || continue
        p = string(get(m, "model", ""))
        isabspath(p) || continue
        push!(out, p)
        j = splitext(p)[1] * ".json"
        manifests && isfile(j) && push!(out, j)
    end
    unique(out)
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
    label_files = segment_label_files(out_value_name, get(params, "models", nothing);
                                      halo = Float64(get(params, "haloSize", 0.0)) > 0)
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

# The label registries a writer can claim a name in: the ccid.json field, the noun its refusal uses,
# and every file a run under `vn` writes for it. ONE table, so a new registry is a row here, not a
# second guard. `labels` covers both `matchAs` roles and the halo; `branch_labels` (segment.branching) its skeleton
# store and the per-branch table pops are built on.
const LABEL_REGISTRIES = Dict{String,NamedTuple{(:noun, :paths),Tuple{String,Function}}}(
    "labels" => (noun = "Labels", paths = (img, vn) -> [joinpath(img_labels_dir(img), f)
        for f in segment_label_files(vn, Dict("0" => Dict("matchAs" => "base"),
                                              "1" => Dict("matchAs" => "nuc")); halo = true)]),
    "branch_labels" => (noun = "Branches", paths = (img, vn) ->
        [joinpath(img_branch_labels_dir(img), "$(vn).zarr"), img_branch_props_path(img, vn)]),
)

# Is `vn` claimed in `field`: registered there, or one of the files a run would write is on disk.
function _label_name_taken(img::CciaImage, raw::AbstractDict, vn::AbstractString, field::AbstractString)::Bool
    registered = get(raw, field, Dict{String,Any}())
    (registered isa AbstractDict && haskey(registered, vn)) ||
        any(ispath, LABEL_REGISTRIES[field].paths(img, vn))
end

"""
    free_label_name(img, raw, out_value_name; field = "labels") -> String

The first of `{vn}_2`, `{vn}_3`, … that is free in `field` — registry and disk both — so a refusal
can name a concrete alternative rather than only the toggle.
"""
function free_label_name(img::CciaImage, raw::AbstractDict, out_value_name::AbstractString;
                         field::AbstractString = "labels")::String
    n = 2
    while _label_name_taken(img, raw, "$(out_value_name)_$(n)", field)
        n += 1
    end
    "$(out_value_name)_$(n)"
end

"""
    labels_taken(img, raw, out_value_name; field = "labels") -> Union{Nothing,String}

Why `out_value_name` is already taken in `field` on `img`, or `nothing`. The message is the one every
label writer shows, and it names a free alternative FIRST: the toggle replaces the user's data, so
the obvious next step — for a person or an agent — should be the name that does not.
"""
function labels_taken(img::CciaImage, raw::AbstractDict, out_value_name::AbstractString;
                      field::AbstractString = "labels")::Union{Nothing,String}
    _label_name_taken(img, raw, out_value_name, field) || return nothing
    alt = free_label_name(img, raw, out_value_name; field = field)
    "$(LABEL_REGISTRIES[field].noun) '$out_value_name' already exist — use '$alt', " *
        "or tick Overwrite existing to replace them"
end

"""
    output_registry(task) -> String

Which `LABEL_REGISTRIES` field a writer's output name lives in, read off its spec: the
`outputRegistry` of its output-name `valueNameInput` (`"branch_labels"` on segment.branching),
`"labels"` when none is declared. The form's taken-name hint reads the SAME declaration
(`labelsTakenAdvisory`), so the two cannot disagree about which registry a name is checked in.
"""
function output_registry(task::CciaTask)::String
    spec = _task_spec(task)
    for p in (isnothing(spec) ? [] : get(spec, "params", []))
        p isa AbstractDict && get(p, "type", "") == "valueNameInput" || continue
        reg = get(p, "outputRegistry", nothing)
        isnothing(reg) || return string(reg)
    end
    "labels"
end

"""
    assert_labels_free(img, out_value_name; field, overwrite, raw, on_log) -> Bool

The guard every label writer (cellpose, coastal, ridges, label import, and branching with
`field = "branch_labels"`) calls FIRST, before it reads or writes anything: `false` — with the reason
logged as `[ERROR]` — when `out_value_name` is already taken in `field` on `img` and `overwrite` is
off. The handler then returns `nothing`, so a composite stops before Measure labels too. Output names
default to `default`, the name an image's segmentation most likely already has, so a re-run used to
replace it — and orphan the measures and pops built on it — silently. `overwrite` is the spec's
shared "Overwrite existing" toggle (`fragments/overwriteLabels.json`).
"""
function assert_labels_free(img::CciaImage, out_value_name::AbstractString;
                            field::AbstractString = "labels",
                            overwrite::Bool = false,
                            raw::AbstractDict = read_ccid_raw(state_file(img)),
                            on_log::Function = _ -> nothing)::Bool
    overwrite && return true
    why = labels_taken(img, raw, out_value_name; field = field)
    isnothing(why) && return true
    on_log("[ERROR] $why")
    false
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
# Tile seams (`seg_seam_qc_findings`): label-to-label contacts across a seam vs across control lines
# halfway through each tile. Calibrated on one tile scan, docs/SEGMENTATION.md → *Seam QC*.
const SEG_SEAM_MIN_CONTACTS   = 20    # fewer contacts on the seams → too few to say anything
const SEG_SEAM_RATIO          = 10.0  # seam contact rate this many times the control rate → cut cells

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
    seg_seam_qc_findings(stats) -> Vector{Dict}

`seg.seam_cuts` when cells look cut at tile seams: different labels touch across a seam far more
often than across the control lines halfway through each tile (`segmentation_utils.seam_contacts`,
banked as `objectStats.seam` only on a tiled run). A joined seam looks like any other line through
the tissue; an unjoined one leaves a cut, two ids touching, along every cell it crosses.
"""
function seg_seam_qc_findings(stats)
    stats isa AbstractDict || return Dict{String,Any}[]
    seam = get(stats, "seam", get(stats, :seam, nothing))
    seam isa AbstractDict || return Dict{String,Any}[]
    _n(k) = Float64(get(seam, k, get(seam, Symbol(k), 0)))
    sc, sp, cc, cp = _n("seamContacts"), _n("seamPixels"), _n("controlContacts"), _n("controlPixels")
    (sc < SEG_SEAM_MIN_CONTACTS || sp <= 0) && return Dict{String,Any}[]
    seam_rate = sc / sp
    ctrl_rate = cp > 0 ? cc / cp : 0.0
    ratio = ctrl_rate > 0 ? seam_rate / ctrl_rate : Inf
    ratio >= SEG_SEAM_RATIO || return Dict{String,Any}[]
    [qc_finding("info", "seg.seam_cuts";
        detail = Dict{String,Any}("seamContacts" => Int(sc), "controlContacts" => Int(cc),
                                  "ratio" => isfinite(ratio) ? round(ratio; digits = 1) : nothing))]
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
    bank_segment_qc!(img, fun_name, out_value_name, qc_out_path; diameter_um, extra_findings, on_log)

Shared tail of every segmenter: read the runner's `qcOutPath` JSON, turn its counts + object stats
into findings (`segment_qc_findings` + `seg_object_qc_findings`), bank them under `fun_name`, and log
one `[QC]` line per finding. Advisory and best-effort — a failure is logged, never raised.

`extra_findings(qmeta)` adds a task's own findings from the same JSON (label import's id pairing), so
a segmenter with one more signal still banks through here rather than a second `write_qc`.
"""
function bank_segment_qc!(img::CciaImage, fun_name::AbstractString, out_value_name::AbstractString,
                          qc_out_path::AbstractString; diameter_um = nothing,
                          extra_findings::Function = _ -> Dict{String,Any}[],
                          on_log::Function = _ -> nothing)
    isfile(qc_out_path) || return nothing
    try
        qmeta  = JSON3.read(read(qc_out_path, String))
        counts = Dict{String,Any}(String(k) => Int(v) for (k, v) in get(qmeta, :labelCounts, ()))
        findings, primary = segment_qc_findings(counts)
        stats = get(qmeta, :objectStats, nothing)
        seg = vcat(seg_object_qc_findings(stats; diameter_um = diameter_um),
                   seg_seam_qc_findings(stats), extra_findings(qmeta))
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
