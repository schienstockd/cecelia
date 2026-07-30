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
"""
function segment_live_outputs(params::AbstractDict)::Vector{LiveOutput}
    out_value_name = string(get(params, "outputValueName", VERSIONED_DEFAULT_VAL))
    LiveOutput[(kind = "labels", value_name = out_value_name,
                files = segment_label_files(out_value_name, get(params, "models", nothing)))]
end

"""
    register_label_files!(img, out_value_name, label_files)

Record a completed run's label zarrs in the image's `ccid.json` `labels` dict
(`{value_name => [filename, …]}`). This is what makes the set appear in every `labels` picker, and
it happens only on success — which is exactly why an in-progress store needs `live_outputs` to be
discoverable at all.

Reads and rewrites `ccid.json` in place rather than going through `save!`, because the running task
holds a `CciaImage` loaded before the run and saving the whole object would clobber any field a
concurrently-running task on the same image has since written.

MERGE NOTE (`fix/atomic-state-writes`): that branch makes `write_json_atomic` the one way to write
durable state (a truncating `open(path, "w")` can leave a half-written ccid.json if the process dies
mid-write, which bricks the WHOLE project's load) and `state_file(img)` the one way to locate it. It
converts the *inline* ccid write that used to live in `cellpose.jl` — which this helper replaced — so
whichever branch lands second, the resolution is these two lines here, once, and dropping that
branch's now-obsolete `cellpose.jl` hunk:

    ccid = state_file(img)                # not joinpath(img._dir, "ccid.json")
    write_json_atomic(ccid, raw)          # not open(ccid, "w")
"""
function register_label_files!(img::CciaImage, out_value_name::AbstractString,
                              label_files::Vector{String})
    ccid = joinpath(img._dir, "ccid.json")
    raw  = read_ccid_raw(ccid)   # the canonical reader (helpers.jl) — don't re-spell the JSON3 walk
    labels_dict = Dict{String, Vector{String}}(
        String(k) => (v isa AbstractVector ? collect(String, v) : [string(v)])
        for (k, v) in get(raw, "labels", Dict{String,Any}()))
    labels_dict[String(out_value_name)] = label_files
    raw["labels"] = labels_dict
    open(ccid, "w") do io; JSON3.write(io, raw); end
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
