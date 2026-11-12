struct Register <: CciaTask end

# The stacked output lives at a NEW image uid on the reference's set (add_image!). The Edit picker
# surfaces this as "new image" so the user knows the run duplicates the reference rather than
# adding a version — same declaration every other add_image!-writing editImages task carries.
task_output_effect(::Register) = "new-image"

# Typed shape of what `_run_task(::Register, …)` reads from `params`. `regChannel` is a single
# channel NAME (not a `channelSelection` list) — it names one shared marker each cycle must
# carry, resolved to a per-image index below.
Base.@kwdef struct RegisterParams
    regChannel::String           = ""
    doFftInitialization::Bool    = false
    doAffine2d::Bool             = true
    doAffine3d::Bool             = false
    ignoreSpacing::Bool          = true
    sigma::Float64               = 1.0
    autoMask::Bool               = false
    samplesPerParameter::Int     = 5000
    expand::Int                  = 0
    keepRegChannels::Bool        = true
    version::Union{String,Nothing} = nothing   # P3 chain-pinning (docs/todo/VN_VERSIONING_PLAN.md)
end

function parse_register_params(d::AbstractDict)::RegisterParams
    RegisterParams(;
        regChannel          = _register_channel_name(get(d, "regChannel", "")),
        doFftInitialization = Bool(get(d, "doFftInitialization", false)),
        doAffine2d          = Bool(get(d, "doAffine2d", true)),
        doAffine3d          = Bool(get(d, "doAffine3d", false)),
        ignoreSpacing       = Bool(get(d, "ignoreSpacing", true)),
        sigma               = Float64(get(d, "sigma", 1.0)),
        autoMask            = Bool(get(d, "autoMask", false)),
        samplesPerParameter = Int(get(d, "samplesPerParameter", 5000)),
        expand              = Int(get(d, "expand", 0)),
        keepRegChannels     = Bool(get(d, "keepRegChannels", true)),
        version = parse_version_pin(d))
end

# Pure: the meta the registered output inherits from the REFERENCE image + a channel count assembled
# across all the cycles. Registration keeps the reference's physical extent, calibration and
# timeline; only the channel dimension grows because every non-reference cycle contributes its
# non-reg channels. `total_c` is passed in from the handler (which already resolved reg-channel
# indices per source), so this helper stays free of file I/O. See the channel-stacking rule in
# `_run_task` for the exact formula.
function _register_inherited_meta(ref_meta::AbstractDict, total_c::Integer)::Dict{String,Any}
    out = Dict{String,Any}()
    for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "PhysicalSizeUnit",
              TIME_META_KEYS..., "SizeX", "SizeY", "SizeZ", "SizeT")
        haskey(ref_meta, k) && (out[k] = ref_meta[k])
    end
    out["SizeC"] = Int(total_c)
    out
end

# Register N images (staining cycles) onto ONE reference and stack them into a new image.
# Multi-image-in, single-image-out — the frontend dispatches this as a set-scope task and hands us
# the whole Vector{CciaImage}; the reference is named by `referenceUid`. Modality: sitkibex's
# SimpleITK ITKv4 registration on a single shared channel (`regChannel`, resolved per image by
# NAME, not index, so `mem-TOM` vs `mem-Tom` errors clearly instead of registering the wrong
# reporter). Output: `add_image!` on the reference's set — a new image holding every reference
# channel then each moving cycle's (its alignment channel too with `keepRegChannels`, to check the
# alignment and the tissue cycle by cycle) — `_register_channel_plan`. A whole-slide cycle is
# registered on its pyramid (`register_utils`), which also checks each cycle against the reference;
# those checks are this task's QC (`_register_qc_findings`).
#
# Direct port of the old-R `registerImages.R` + vendored sitkibex (see python/sitkibex/). The one
# improvement over old R is that the per-cycle affine transforms are persisted to the new image's
# `data/registration_transforms.json` — old R computed and threw them away, so the alignment could
# never be re-applied to labels or tracks without recomputing.
function _run_task(task::Register, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    length(imgs) < 2 && (on_log("[ERROR] Select at least two images (one reference + one cycle to align)."); return nothing)

    p = parse_register_params(params)
    reg_channel_name = p.regChannel
    isempty(reg_channel_name) && (on_log("[ERROR] No `regChannel` chosen."); return nothing)

    # First selected = reference (old R convention). The reference is the "home" — the new registered
    # image is added_image!'d onto its set — and every moving cycle is aligned to it. A later release
    # can offer an explicit reference picker without changing this dispatch shape.
    ordered = imgs
    ref_img = ordered[1]
    ref_raw = read_ccid_raw(state_file(ref_img))
    ref_meta = Dict{String,Any}(String(k) => v for (k, v) in get(ref_raw, "meta", Dict{String,Any}()))

    # Resolve the reg-channel INDEX per image via `channel_indices` (0-based, errors by name with a
    # case-differ hint — see app/CLAUDE.md). A cycle that lists no channel names or a different name
    # for the reg marker is a real user error, not a warning; refuse rather than register on the
    # wrong channel.
    reg_channels = Int[]
    src_paths   = String[]
    src_C       = Int[]
    src_names_per_img = Vector{String}[]
    src_acqs    = Any[]
    for im in ordered
        raw   = read_ccid_raw(state_file(im))
        names = something(channel_names(im), String[])
        push!(src_names_per_img, names)
        try
            idx = channel_indices([reg_channel_name], names; what = "regChannel")
            push!(reg_channels, Int(first(idx)))
        catch e
            on_log("[ERROR] $(im.uid): $(e isa ErrorException ? e.msg : sprint(showerror, e))")
            return nothing
        end
        filename = versioned_get_field_at(raw, "filepath", VERSIONED_DEFAULT_VAL; version = p.version)
        if isnothing(filename)
            on_log("[ERROR] $(im.uid): no filepath registered on the default version.")
            return nothing
        end
        proj_dir = dirname(dirname(im._dir))
        im_path  = joinpath(proj_dir, "0", im.uid, string(filename))
        if !ispath(im_path)
            on_log("[ERROR] $(im.uid): image not found on disk — $im_path")
            return nothing
        end
        push!(src_paths, im_path)
        this_meta = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
        push!(src_C, Int(get(this_meta, "SizeC", length(names))))
        push!(src_acqs, get(this_meta, "acquisition", nothing))
    end

    # Reference channels + each cycle's; a cycle's alignment channel only with `keepRegChannels`.
    plan    = _register_channel_plan(src_C, reg_channels, p.keepRegChannels)
    total_c = length(plan)

    proj_dir = dirname(dirname(ref_img._dir))
    proj_uid = basename(proj_dir)
    proj    = load_project(proj_uid)
    set_idx = findfirst(s -> ref_img.uid in s.image_uids, proj._sets)
    isnothing(set_idx) && (on_log("[ERROR] Could not find the set containing reference $(ref_img.uid)"); return nothing)
    s = proj._sets[set_idx]

    reg_meta = Dict{String,Any}(
        "register_reference_uid"  => ref_img.uid,
        "register_source_uids"    => [im.uid for im in ordered],
        "register_channel_name"   => reg_channel_name,
        "register_do_affine_2d"   => p.doAffine2d,
        "register_do_affine_3d"   => p.doAffine3d)
    merge!(reg_meta, _register_inherited_meta(ref_meta, total_c))
    copy_source_meta!(reg_meta, ref_meta)
    acq = _register_acquisition(src_acqs, plan)
    isnothing(acq) || (reg_meta["acquisition"] = acq)

    new_img = add_image!(s; name = derived_image_name(proj, ref_img.name, "registered"), meta = reg_meta)
    out_filename = "ccidRegistered.ome.zarr"
    im_out_path  = joinpath(proj_dir, "0", new_img.uid, out_filename)
    on_log("[INFO] Reference: $(ref_img.uid) ($(ref_img.name))")
    on_log("[INFO] Registering $(length(ordered) - 1) cycle(s) on channel '$reg_channel_name' → $(new_img.uid)")

    # The per-cycle transforms (old R threw them away), under the NEW image's `data/` like crop's
    # napari-colours sidecar, so labels/tracks can be re-aligned without recomputing.
    transforms_path = joinpath(new_img._dir, "data", "registration_transforms.json"); mkpath(dirname(transforms_path))
    qc_out_path = joinpath(task_run_dir(ref_img._dir), "register_qc_$(new_img.uid).json")
    rm(qc_out_path; force = true)
    pixel_um = img_is_calibrated(ref_img) ? img_physical_sizes(ref_img)[1][3] : nothing   # µm, unit applied

    ok = run_py("tasks/editImages/register_run.py",
        (; imPaths        = src_paths,
           imOutPath      = im_out_path,
           regChannels    = reg_channels,
           transformsOut  = transforms_path,
           doFftInitialization = p.doFftInitialization,
           doAffine2d          = p.doAffine2d,
           doAffine3d          = p.doAffine3d,
           ignoreSpacing       = p.ignoreSpacing,
           sigma               = p.sigma,
           autoMask            = p.autoMask,
           samplesPerParameter = p.samplesPerParameter,
           expand              = p.expand,
           keepRegChannels     = p.keepRegChannels,
           pixelSizeUm         = pixel_um,
           qcOutPath           = qc_out_path),
        task_run_dir(ref_img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    # Output channel names in the runner's order; a marker every cycle carries reads "DAPI (cycle 2)".
    combined_names = _register_channel_names(src_names_per_img, plan)

    commit_state!(new_img) do raw2
        versioned_set_field!(raw2, "filepath", out_filename, VERSIONED_DEFAULT_VAL)
        versioned_set_field!(raw2, "imChannelNames", combined_names, VERSIONED_DEFAULT_VAL)
        raw2["status"] = "done"
        adopt_store_counts!(raw2, im_out_path)   # the written store, not the source image, sizes C/T/Z
    end

    # Carry the REFERENCE image's napari-colours sidecar so the new image's first ref_C channels
    # keep their look. Colours for the moving-cycle channels stay defaulted — a 1:1 remap isn't
    # possible when their indices have been rewritten.
    ref_filename = versioned_get_field_at(ref_raw, "filepath", VERSIONED_DEFAULT_VAL)   # `_latest` leaf
    if !isnothing(ref_filename)
        ref_sidecar = joinpath(ref_img._dir, "data", basename(string(ref_filename)) * ".json")
        if isfile(ref_sidecar)
            dst_dir = joinpath(new_img._dir, "data"); mkpath(dst_dir)
            cp(ref_sidecar, joinpath(dst_dir, out_filename * ".json"); force = true)
            on_log("[INFO] Carried reference napari colours to the registered image")
        end
    end

    cycles, findings, metrics = _register_qc(ordered, qc_out_path; on_log = on_log)
    write_qc(new_img, "editImages.register", VERSIONED_DEFAULT_VAL, findings; metrics = metrics, cycles = cycles)
    isempty(findings) || on_log("[QC] $(length(findings)) finding(s) — see the image's QC badge.")

    on_log("[INFO] Registration complete → new image $(new_img.uid) ($total_c channels)")
    Dict{String,Any}("newImageUid" => new_img.uid, "newImageName" => new_img.name, "setUid" => s.uid,
                     "totalChannels" => total_c, "transformsPath" => transforms_path)
end

