struct Branching <: CciaTask end

# skan branch-type codes → semantic pop names (BRANCHING_PLAN Decision 3). Stable + documented
# in skan (see https://skeleton-analysis.org/stable/getting_started/quickstart.html): 0 = a lone
# segment with two termini, 1 = one junction + one endpoint, 2 = two junctions, 3 = a closed loop.
const _BRANCH_TYPE_POP_NAME = Dict{Int,String}(
    0 => "endpoint-to-endpoint",
    1 => "endpoint-to-junction",
    2 => "junction-to-junction",
    3 => "isolated-cycle",
)

# Pure QC helper: objective branch count → advisory finding for the empty-skeleton case
# (unambiguous failure mode: over-eroded input or an empty ref population).
function _branching_qc_findings(n_branches::Integer)
    n_branches == 0 ? [qc_finding("warn", "branching.no_branches")] : Dict{String,Any}[]
end

# ── Anisotropy scale params are in µm; the compute is in PIXELS ────────────────────────────────
# The user sets a physical scale, because that is the thing with meaning — a fibre is ~2 µm thick
# whatever the objective, whereas "12 px" means something different on every image. It also makes
# the readout comparable across a cohort with mixed calibration, which px never was.
#
# Conversion happens HERE, not in Python: the Julia handler is what holds the image and therefore
# its calibration. Python keeps working in array space, which is the only space its arrays have.

"""Smallest usable box, in pixels. Below ~3 px a box holds too few pixels to pool and the grid
stops being a spatial summary — it just resamples noise."""
const _ANISO_MIN_BOX_PX = 3
const _ANISO_MIN_SIGMA_PX = 0.5

"""
    _um_to_px(value_um, um_per_px; minimum_px) -> (px, clamped)

Physical → pixel, rounded to whole pixels for the box grid. Returns the clamp flag so the caller can
say so out loud rather than silently substituting a different setting than the user asked for.
"""
function _um_to_px(value_um::Real, um_per_px::Real; minimum_px::Real)
    um_per_px > 0 || error("_um_to_px: non-positive µm/px ($um_per_px)")
    raw = Float64(value_um) / Float64(um_per_px)
    px = max(raw, Float64(minimum_px))
    (px, px > raw)
end

"""
    _aniso_grid_bytes(n_boxes, n_frames) -> Int

Size of the stored `orientation_*` block for a grid the run ACTUALLY produced (the runner reports
its shape; nothing here guesses from the box size). The five grids carry 10 float32 per box —
2 coords + 2 eigenvalues + 4 eigenvector components + length + coherence — so 40 bytes per box per
frame. This is the whole reason a small box is expensive: boxes scale as 1/box², so halving the box
quadruples the file.
"""
_aniso_grid_bytes(n_boxes::Integer, n_frames::Integer) = Int(n_boxes) * 40 * max(1, Int(n_frames))

"""Advisory finding when the orientation grid would dominate the sidecar. Never a gate — a fine grid
is a legitimate choice, the user just deserves to know what it costs before finding a 300 MB file."""
const _ANISO_SIDECAR_WARN_BYTES = 100_000_000

function _aniso_grid_findings(bytes::Integer, n_boxes::Integer, box_um::Real)
    bytes < _ANISO_SIDECAR_WARN_BYTES ? Dict{String,Any}[] :
        # the size goes in `detail`, not the prose — the catalog's rule, and it keeps the
        # placeholder allow-list (asserted in runtests) tight.
        [qc_finding("warn", "branching.aniso_grid_large";
                    detail = "$(round(Int, bytes / 1e6)) MB · $(n_boxes) boxes/frame " *
                             "at $(round(box_um, digits = 2)) µm")]
end

function _run_task(task::Branching, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)

    value_name     = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    out_value_name = string(get(params, "outputValueName", VERSIONED_DEFAULT_VAL))
    ref_pops       = string(get(params, "refPops", "NONE"))
    calc_anisotropy = Bool(get(params, "calcAnisotropy", false))

    ccid = joinpath(img._dir, "ccid.json")
    raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))

    # Channel names → 0-based indices for fibreChannels (Phase 3 anisotropy input). Read the
    # ACTIVE image version's channel names (nothing → `_active`; falls back to `default`) so a
    # corrected image with extra/renamed channels resolves correctly.
    channel_names_raw = versioned_get_field(raw, "imChannelNames", nothing)
    ch_names = channel_names_raw isa AbstractVector ?
               collect(String, channel_names_raw) : String[]
    fibre_channels_raw = get(params, "fibreChannels", [])
    fibre_indices = Int[]
    for ch in fibre_channels_raw
        idx = findfirst(==(String(ch)), ch_names)
        isnothing(idx) || push!(fibre_indices, idx - 1)
    end
    # Only `anisotropySource="channel"` reads raw pixels; "skeleton"/"mask" work off the labels, so
    # an empty fibreChannels is only a problem for the channel source.
    aniso_source = string(get(params, "anisotropySource", "skeleton"))
    if calc_anisotropy && aniso_source == "channel" && isempty(fibre_indices)
        on_log("[WARN] anisotropySource=channel but fibreChannels resolved to []; anisotropy will not be computed.")
    end
    run_anisotropy = calc_anisotropy && (aniso_source != "channel" || !isempty(fibre_indices))

    # ── µm → px for the two anisotropy scales ──────────────────────────────────────────────────
    # In-plane resolution: the box grid and the tensor smoothing both act on Y/X. `:x` by name, never
    # by position (docs/todo/CENTROID_AXES_PLAN.md). Absent calibration returns 1.0, i.e. the µm
    # numbers land as pixels — silently, which is why it is a QC finding rather than a log line.
    um_per_px = physical_size_for_axis(img, :x)
    uncalibrated = !(um_per_px > 0) || um_per_px == 1.0
    um_per_px = um_per_px > 0 ? um_per_px : 1.0
    sigma_um = Float64(get(params, "structureTensorSigmaUm", 7.0))
    box_um   = Float64(get(params, "anisotropyBoxUm", 5.0))
    st_sigma_px, sigma_clamped = _um_to_px(sigma_um, um_per_px; minimum_px = _ANISO_MIN_SIGMA_PX)
    box_px_f,    box_clamped   = _um_to_px(box_um,   um_per_px; minimum_px = _ANISO_MIN_BOX_PX)
    box_px = round(Int, box_px_f)
    if run_anisotropy
        on_log("[ANISO] $(um_per_px) µm/px → smoothing $(sigma_um) µm = " *
               "$(round(st_sigma_px, digits = 1)) px, grid box $(box_um) µm = $(box_px) px")
        (sigma_clamped || box_clamped) &&
            on_log("[WARN] a scale was below the usable pixel minimum and was clamped — " *
                   "this image's resolution cannot resolve what you asked for")
        uncalibrated &&
            on_log("[WARN] no pixel size on this image; the µm scales were used as PIXELS")
    end

    # Resolve input image path. Note: `value_name` here is the SEGMENTATION name (e.g. "SHG"),
    # which lives in `img.labels`. Image versions (`default`, drift-corrected, af-corrected, …)
    # are a DIFFERENT namespace — keyed under `filepath` — and there is no correspondence between
    # a segmentation's value_name and any image version. So resolve the raw image via the ACTIVE
    # image version (`nothing` → `_active` → falls back to `default`); anisotropy reads the raw
    # pixels off that store, and OME-XML for physical scale comes from the same file.
    filename = versioned_get_field(raw, "filepath", nothing)
    if isnothing(filename)
        on_log("[ERROR] No image filepath registered on this image — nothing to skeletonise against.")
        return nothing
    end
    im_path = joinpath(dirname(dirname(img._dir)), "0", img.uid, string(filename))

    # Resolve input labels zarr — a segmentation the user chose from `img.labels`
    if !haskey(img.labels, value_name) || isempty(img.labels[value_name])
        on_log("[ERROR] No labels registered for valueName='$value_name'")
        return nothing
    end
    labels_path = joinpath(img._dir, "labels", first(img.labels[value_name]))
    if !ispath(labels_path)
        on_log("[ERROR] Input labels not found: $labels_path")
        return nothing
    end

    # Resolve refPops in Julia (Decision 7): Python receives a plain list of label IDs, never a
    # pop map. Multi-accept picker → resolve_pop_type discovers which map to load.
    label_ids = nothing
    if ref_pops != "NONE"
        vn, path = _split_pop_ref(ref_pops, value_name)
        pt = resolve_pop_type(img, vn, path)
        m = try; load_pop_map(img; value_name = vn, pop_type = pt); catch; nothing; end
        if isnothing(m) || !has_pop(m, path)
            on_log("[ERROR] Population not found for refPops='$ref_pops' (value_name=$vn, pop_type=$pt)")
            return nothing
        end
        recompute!(m, cols -> (label_props(img; value_name = vn) |>
                               lp -> select_cols(lp, cols) |> as_df))
        label_ids = collect(Int, cells_in_pop(m, path))
        on_log("[INFO] Restricting to $(length(label_ids)) label(s) from population '$ref_pops'")
        if isempty(label_ids)
            on_log("[ERROR] Population '$ref_pops' is empty — nothing to skeletonise")
            return nothing
        end
    end

    task_dir       = img._dir
    branch_zarr    = "$(out_value_name).zarr"
    branch_lbl_dir = img_branch_labels_dir(img)
    branch_lbl_out = joinpath(branch_lbl_dir, branch_zarr)
    branch_props   = img_branch_props_path(img, out_value_name)
    qc_out_path    = joinpath(task_run_dir(task_dir), "branching_counts.json")

    isdir(branch_lbl_dir) || mkpath(branch_lbl_dir)

    on_log("[INFO] Input labels:  $labels_path")
    on_log("[INFO] Branch labels: $branch_lbl_out")
    on_log("[INFO] Branch props:  $branch_props")

    ok = run_py("tasks/segment/branching_run.py",
        (; imPath              = im_path,
           labelsPath           = labels_path,
           branchLabelsOutPath  = branch_lbl_out,
           branchPropsOutPath   = branch_props,
           qcOutPath            = qc_out_path,
           labelIds             = something(label_ids, Int[]),
           preDilationSize      = Int(get(params, "preDilationSize", 2)),
           postDilationSize     = Int(get(params, "postDilationSize", 2)),
           useBorders           = Bool(get(params, "useBorders", false)),
           flattenBranching     = Bool(get(params, "flattenBranching", false)),
           integrateTime        = Bool(get(params, "integrateTime", false)),
           integrateTimeMode    = string(get(params, "integrateTimeMode", "max")),
           calcAnisotropy       = run_anisotropy,
           calcFlattened        = Bool(get(params, "calcFlattened", false)),
           anisotropySource     = aniso_source,
           fibreChannels        = fibre_indices,
           # Python works in ARRAY space; the µm→px conversion happened above, where the image's
           # calibration lives. The µm values ride along so the runner can record what was asked.
           structureTensorSigma = st_sigma_px,
           anisotropyBoxSize    = box_px,
           structureTensorSigmaUm = sigma_um,
           anisotropyBoxUm        = box_um,
           umPerPx                = um_per_px),
        task_run_dir(task_dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    # Register the branch labels zarr in ccid.json under `branch_labels` — NOT `labels`. Decision 6:
    # branch labels get their own registry so the generic labels picker never lists them.
    raw2 = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
    bl_dict = Dict{String,Vector{String}}(
        String(k) => (v isa AbstractVector ? collect(String, v) : [string(v)])
        for (k, v) in get(raw2, "branch_labels", Dict{String,Any}()))
    bl_dict[out_value_name] = [branch_zarr]
    raw2["branch_labels"] = bl_dict
    open(ccid, "w") do io; JSON3.write(io, raw2); end

    # QC (advisory): objective branch count + zero-branches warning
    n_branches = 0
    branch_types = Int[]
    if isfile(qc_out_path)
        try
            qmeta = JSON3.read(read(qc_out_path, String))
            n_branches = Int(get(qmeta, :nBranches, 0))
            n_skeletons = Int(get(qmeta, :nSkeletons, 0))
            mean_branch_length = Float64(get(qmeta, :meanBranchLength, 0.0))
            branch_types = Int[Int(x) for x in get(qmeta, :branchTypes, Int[])]
            findings = _branching_qc_findings(n_branches)
            metrics = Dict{String,Any}("nBranches"        => n_branches,
                                       "nSkeletons"       => n_skeletons,
                                       "meanBranchLength" => mean_branch_length)
            # Only bank `anisotropy` when the pass actually ran — otherwise a structural 0.0 would
            # enter the cohort stats and make every image that skipped it look like an outlier.
            if run_anisotropy
                metrics["anisotropy"] = Float64(get(qmeta, :anisotropy, 0.0))
                on_log("[QC] anisotropy $(round(metrics["anisotropy"], digits = 3)) (1 = non-uniform).")
                # What the chosen grid box actually cost, from the grid the runner produced -- not an
                # estimate. This is the number that makes "how fine should I go?" answerable at all.
                n_boxes  = Int(get(qmeta, :anisoBoxes, 0))
                n_frames = Int(get(qmeta, :anisoFrames, 0))
                if n_boxes > 0
                    bytes = _aniso_grid_bytes(n_boxes, n_frames)
                    on_log("[ANISO] grid $(n_boxes) boxes x $(n_frames) frame(s) = " *
                           "$(round(bytes / 1e6, digits = 1)) MB stored")
                    append!(findings, _aniso_grid_findings(bytes, n_boxes, box_um))
                end
                uncalibrated && push!(findings, qc_finding("warn", "branching.uncalibrated"))
            end
            write_qc(img, "segment.branching", out_value_name, findings; metrics = metrics)
            on_log("[QC] $n_branches branch(es) across $n_skeletons skeleton(s).")
        catch e
            on_log("[QC] could not compute branching QC: $e")
        end
    end

    # Decision 3: auto-create one filter pop per unique branch-type at the branch pop map's root.
    # Idempotent (ensure_filter_pop! replaces an existing name), so re-runs stay clean.
    for bt in branch_types
        name = get(_BRANCH_TYPE_POP_NAME, bt, "branch-type-$(bt)")
        ensure_filter_pop!(img, "branch", out_value_name, ["/"], name;
                           filter_measure = "branch-type",
                           filter_fun = "eq", filter_values = bt)
    end
    isempty(branch_types) || on_log("[INFO] Auto-created $(length(branch_types)) branch-type filter pop(s).")

    Dict{String,Any}("outputValueName" => out_value_name,
                     "branchLabelFile" => branch_zarr,
                     "branchPropsFile" => "$(out_value_name)$(BRANCH_PROPS_SUFFIX).h5ad",
                     "nBranches"       => n_branches,
                     "branchTypes"     => branch_types)
end
