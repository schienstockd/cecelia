struct Register <: CciaTask end

# The stacked output lives at a NEW image uid on the reference's set (add_image!). The Edit picker
# surfaces this as "new image" so the user knows the run duplicates the reference rather than
# adding a version — same declaration every other add_image!-writing editImages task carries.
task_output_effect(::Register) = "new-image"

# Pure: the meta the registered output inherits from the REFERENCE image + a channel count assembled
# across all the cycles. Registration keeps the reference's physical extent, calibration and
# timeline; only the channel dimension grows because every non-reference cycle contributes its
# non-reg channels. `total_c` is passed in from the handler (which already resolved reg-channel
# indices per source), so this helper stays free of file I/O. See the channel-stacking rule in
# `_run_task` for the exact formula.
function _register_inherited_meta(ref_meta::AbstractDict, total_c::Integer)::Dict{String,Any}
    out = Dict{String,Any}()
    for k in ("PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "PhysicalSizeUnit",
              "TimeIncrement", "TimeIncrementUnit", "SizeX", "SizeY", "SizeZ", "SizeT")
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
# reporter). Output: `add_image!` on the reference's set — a new image whose channel dimension is
# `ref_C + Σ(cycle_C − 1)` (the reg channel of each moving cycle is dropped as duplicate). QC-EXEMPT:
# deterministic geometric alignment with no measurement output; the intent of the task is to
# reshape a set of cycles into one image, so it belongs in editImages.
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

    reg_channel_name = string(get(params, "regChannel", ""))
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
        filename = versioned_get_field(raw, "filepath", VERSIONED_DEFAULT_VAL)
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
        # SizeC is a scalar meta field; every image carries one.
        this_meta = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
        push!(src_C, Int(get(this_meta, "SizeC", length(names))))
    end

    # Output channel geometry: reference channels + Σ(cycle_C − 1). Old R drops the reg channel of
    # every MOVING cycle as a duplicate of the reference's; the reference's own reg channel stays.
    total_c = src_C[1] + sum(c - 1 for c in src_C[2:end]; init = 0)

    # Reference is the "home" — new image lands in the SAME set. add_image! on the reference's set.
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
        "register_do_affine_2d"   => Bool(get(params, "doAffine2d", true)),
        "register_do_affine_3d"   => Bool(get(params, "doAffine3d", false)))
    merge!(reg_meta, _register_inherited_meta(ref_meta, total_c))
    haskey(ref_meta, "ori_path") && (reg_meta["ori_path"] = ref_meta["ori_path"])

    new_img = add_image!(s; name = "$(ref_img.name) (registered)", meta = reg_meta)
    out_filename = "ccidRegistered.ome.zarr"
    im_out_path  = joinpath(proj_dir, "0", new_img.uid, out_filename)
    on_log("[INFO] Reference: $(ref_img.uid) ($(ref_img.name))")
    on_log("[INFO] Registering $(length(ordered) - 1) cycle(s) on channel '$reg_channel_name' → $(new_img.uid)")

    # Where the runner persists the per-cycle affine transforms (one improvement over old R). Kept
    # under the NEW image's `data/` — same convention as the napari-colours sidecar for crop — so a
    # future step can reapply the alignment to labels/tracks without redoing the compute.
    transforms_path = joinpath(new_img._dir, "data", "registration_transforms.json"); mkpath(dirname(transforms_path))

    ok = run_py("tasks/editImages/register_run.py",
        (; imPaths        = src_paths,
           imOutPath      = im_out_path,
           regChannels    = reg_channels,
           transformsOut  = transforms_path,
           doFftInitialization = Bool(get(params, "doFftInitialization", false)),
           doAffine2d          = Bool(get(params, "doAffine2d", true)),
           doAffine3d          = Bool(get(params, "doAffine3d", false)),
           ignoreSpacing       = Bool(get(params, "ignoreSpacing", true)),
           sigma               = Float64(get(params, "sigma", 1.0)),
           autoMask            = Bool(get(params, "autoMask", false)),
           samplesPerParameter = Int(get(params, "samplesPerParameter", 5000)),
           expand              = Int(get(params, "expand", 0))),
        task_run_dir(ref_img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    # Assemble the output channel-name list: reference names, then each moving cycle's non-reg names
    # in the same order. Prefixed with the source uid so duplicate marker names across cycles are
    # distinguishable in the UI. If a cycle has no registered names, fall back to positional labels.
    combined_names = String[]
    for (name, uid) in zip(src_names_per_img[1], fill(ordered[1].uid, length(src_names_per_img[1])))
        push!(combined_names, name)
    end
    for i in 2:length(ordered)
        cyc_names = src_names_per_img[i]
        n_c       = src_C[i]
        reg_idx   = reg_channels[i]
        for c in 0:(n_c - 1)
            c == reg_idx && continue
            base = c + 1 <= length(cyc_names) ? cyc_names[c + 1] : "cyc$(i - 1)_ch$c"
            push!(combined_names, "$(ordered[i].uid[1:min(6,end)]):$base")
        end
    end

    commit_state!(new_img) do raw2
        versioned_set_field!(raw2, "filepath", out_filename, VERSIONED_DEFAULT_VAL)
        versioned_set_field!(raw2, "imChannelNames", combined_names, VERSIONED_DEFAULT_VAL)
        raw2["status"] = "done"
    end

    # Carry the REFERENCE image's napari-colours sidecar so the new image's first ref_C channels
    # keep their look. Colours for the moving-cycle channels stay defaulted — a 1:1 remap isn't
    # possible when their indices have been rewritten.
    ref_filename = versioned_get_field(ref_raw, "filepath", VERSIONED_DEFAULT_VAL)
    if !isnothing(ref_filename)
        ref_sidecar = joinpath(ref_img._dir, "data", basename(string(ref_filename)) * ".json")
        if isfile(ref_sidecar)
            dst_dir = joinpath(new_img._dir, "data"); mkpath(dst_dir)
            cp(ref_sidecar, joinpath(dst_dir, out_filename * ".json"); force = true)
            on_log("[INFO] Carried reference napari colours to the registered image")
        end
    end

    on_log("[INFO] Registration complete → new image $(new_img.uid) ($total_c channels)")
    Dict{String,Any}("newImageUid" => new_img.uid, "newImageName" => new_img.name, "setUid" => s.uid,
                     "totalChannels" => total_c, "transformsPath" => transforms_path)
end
