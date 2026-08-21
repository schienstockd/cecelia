struct ExportOmeTiff <: CciaTask end

# Pure: the calibration an export carries into its OME-XML, read from `ccid.json` — which is the
# AUTHORITATIVE copy (see docs/OBJECTMODEL.md → *Calibration — three copies, one stamp*). The store's own
# OME-XML is a derived copy, so a source whose sidecar drifted would otherwise export the wrong
# scale. Absent fields are simply omitted: OME treats a missing PhysicalSize as unknown, which is
# honest, whereas defaulting to 1.0 would state a pixel size we do not have.
#
# A Z-MIP collapses z to one plane, so PhysicalSizeZ stops being meaningful and is dropped; a
# single-timepoint export has no interval between frames, so TimeIncrement goes too.
function _export_calibration(meta::AbstractDict; z_mip::Bool = false, one_frame::Bool = false)
    out = Dict{String,Any}()
    # `ccid.json` stores the NGFF/UDUNITS spelling ("micrometer", "second"); OME-XML's UnitsLength /
    # UnitsTime are ENUMERATIONS of symbols ("µm", "s"). `ome_xml_unit_name` is the one converter
    # (mirrored in `zarr_utils._OME_XML_UNIT`, kept equal by the cross-language golden test).
    #
    # This is not cosmetic. A value outside the enumeration makes the whole `<Pixels>` element
    # schema-invalid, and Bio-Formats then DISCARDS the entire OME block and falls back to counting
    # IFDs — so a 31x4x32 movie opened as 3968 timepoints, one channel, no names and no voxel size.
    # Writing "micrometer" is exactly the bug this task exists to prevent, one layer down.
    unit = get(meta, "PhysicalSizeUnit", nothing)
    unit = unit isa AbstractString ? ome_xml_unit_name(unit) : unit
    for (k, ax) in (("PhysicalSizeX", "X"), ("PhysicalSizeY", "Y"), ("PhysicalSizeZ", "Z"))
        (ax == "Z" && z_mip) && continue
        v = get(meta, k, nothing)
        (isnothing(v) || v == "") && continue
        pv = tryparse_f64(v)
        (isnothing(pv) || pv <= 0) && continue
        out[k] = pv
        isnothing(unit) || unit == "" || (out[k * "Unit"] = string(unit))
    end
    if !one_frame
        ti = get(meta, "TimeIncrement", nothing)
        if !(isnothing(ti) || ti == "")
            tv = tryparse_f64(ti)
            if !isnothing(tv) && tv > 0
                out["TimeIncrement"]     = tv
                out["TimeIncrementUnit"] = ome_xml_unit_name(
                    string(get(meta, "TimeIncrementUnit", "s")))
            end
        end
    end
    out
end

# Pure: the QC findings for one export. The single objective signal a file-writing task has is
# whether the CALIBRATION made it out, because that is the whole reason the task exists — the old
# workflow (OME-TIFF → ImageJ → plain TIFF → Imaris File Converter) lost the Z spacing, since a plain
# TIFF has nowhere to record it, and Imaris then guessed the voxel size. An export that silently
# carries no PhysicalSizeZ reproduces exactly that failure in a new file, so it is worth flagging
# even though the write itself succeeded.
function _export_qc_findings(cal::AbstractDict, size_z::Integer)
    findings = Dict{String,Any}[]
    if size_z > 1 && !haskey(cal, "PhysicalSizeZ")
        push!(findings, qc_finding("warn", "export.no_z_calibration"; value = size_z))
    end
    if !haskey(cal, "PhysicalSizeX") || !haskey(cal, "PhysicalSizeY")
        push!(findings, qc_finding("warn", "export.no_xy_calibration"))
    end
    findings
end

# Write one image VERSION out as an OME-TIFF, outside the project tree.
#
# The need is that figures get rendered in Imaris, which cannot read our zarr stores. Imaris File
# Converter reads through Bio-Formats, so a correct OME-TIFF is the input for the `.ims` route too —
# there is nothing Imaris-specific to write here, only calibration that has to actually survive.
# `docs/TODO.md` recorded the two dead ends (PyImarisWriter is Windows-only; do not shell out to
# ImarisConvertBioformats).
#
# The output is an ARTEFACT, not data: it lands under `outDir` (default `default_export_dir()`),
# never inside the project, and no image version is registered. Params: `valueName`, `channels`
# (empty = all), `zMip`, `timepoint` (-1 = all), `outDir`.
function _run_task(task::ExportOmeTiff, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    raw        = read_ccid_raw(state_file(img))

    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        on_log("[ERROR] No filepath for valueName='$value_name'")
        return nothing
    end

    proj_dir = dirname(dirname(img._dir))              # {proj}
    im_path  = joinpath(proj_dir, "0", img.uid, string(filename))
    if !ispath(im_path)
        on_log("[ERROR] Source image not found: $im_path")
        return nothing
    end

    z_mip     = get(params, "zMip", false) === true
    timepoint = round(Int, something(tryparse_f64(string(get(params, "timepoint", -1))), -1.0))

    # `channelSelection` submits channel NAMES, not indices — resolving them is `channel_indices`'
    # job (0-based, which is what the Python runner slices with). Converting here by hand is what
    # broke this: `Int("DAPI")` threw a MethodError out of the task with a Julia stacktrace.
    #
    # Names come from `channel_names`, which falls back to the ACTIVE version when the requested one
    # has no entry of its own — names are typically registered only under `default` while processed
    # versions carry none. That fallback is the whole point of the helper, and reading the raw
    # versioned field instead (`ccid_channel_names(raw, value_name)`) is what made this report
    # "(none registered)" for an image whose channels the picker was happily listing: the picker is
    # fed by `channel_names(img)` in the image payload, so anything else disagrees with the UI.
    all_names = something(channel_names(img; value_name = value_name), String[])
    local channels::Vector{Int}
    try
        channels = channel_indices(get(params, "channels", nothing), all_names; what = "channels")
    catch e
        # A name this version doesn't have is a parameter problem, not a crash — say which, and stop
        # before doing any work. (Channel names are per image VERSION, so a chain built on one image
        # can name a channel another one lacks.)
        on_log("[ERROR] $(e isa ErrorException ? e.msg : sprint(showerror, e))")
        return nothing
    end

    # Destination: an artefact, so never inside the project tree. Empty → the same shared folder the
    # `.ccbundle` project export writes to, so exports of both kinds land in one place.
    out_dir = strip(string(get(params, "outDir", "")))
    isempty(out_dir) && (out_dir = default_export_dir())
    out_dir = expand_user(out_dir)
    try
        mkpath(out_dir)
    catch e
        on_log("[ERROR] Cannot write to destination '$out_dir': $(sprint(showerror, e))")
        return nothing
    end

    # Name the file after the IMAGE, through the one filename rule the movie recorders also use, so
    # an image called "… -res (cropped)" can't produce two different spellings. The version is part
    # of the name whenever it isn't the default — exporting `default` and `corrected` of the same
    # image must not collide.
    stem = safe_name_part(img.name)
    isempty(stem) && (stem = img.uid)
    value_name != VERSIONED_DEFAULT_VAL && (stem *= "_" * safe_name_part(value_name))
    out_path = joinpath(out_dir, stem * ".ome.tif")

    # Channel names for the OME-XML, restricted to the exported subset (and in that order).
    ch_names = isempty(channels) ? all_names :
               String[1 <= c + 1 <= length(all_names) ? all_names[c + 1] : "Channel $c" for c in channels]

    meta = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict{String,Any}()))
    cal  = _export_calibration(meta; z_mip = z_mip, one_frame = timepoint >= 0)

    on_log("[INFO] Export source: $im_path (version '$value_name')")
    on_log("[INFO] Destination:   $out_path")
    isempty(cal) && on_log("[WARN] Source has no physical calibration — the export will carry none")

    qc_out_path = joinpath(task_run_dir(img._dir), "ome_tiff_export.json")
    ok = run_py("tasks/exportImages/ome_tiff_run.py",
        (; imPath      = im_path,
           outPath     = out_path,
           channels    = channels,
           channelNames = ch_names,
           zMip        = z_mip,
           timepoint   = timepoint,
           calibration = cal,
           qcOutPath   = qc_out_path),
        task_run_dir(img._dir);
        on_log = on_log, on_progress = on_progress, on_process = on_process)
    ok || return nothing

    written = isfile(out_path) ? filesize(out_path) : 0
    if written == 0
        on_log("[ERROR] Export produced no file at $out_path")
        return nothing
    end

    size_z = 1
    if isfile(qc_out_path)
        try
            qmeta  = JSON3.read(read(qc_out_path, String))
            size_z = Int(get(qmeta, "sizeZ", 1))
            findings = _export_qc_findings(cal, size_z)
            write_qc(img, "exportImages.ome_tiff", value_name, findings;
                     metrics = Dict{String,Any}(
                         "exportBytes"    => written,
                         "exportPlanes"   => Int(get(qmeta, "planes", 0)),
                         "exportChannels" => Int(get(qmeta, "sizeC", 0))),
                     source = Dict{String,Any}("path" => im_path),
                     output = Dict{String,Any}("path" => out_path,
                                               "shape" => collect(Int, get(qmeta, "shape", Int[]))))
        catch e
            on_log("[WARN] Could not bank export QC: $(sprint(showerror, e))")
        end
    end

    on_log("[INFO] Wrote $(round(written / 1024^2; digits = 1)) MB → $out_path")
    Dict{String,Any}("outPath" => out_path, "bytes" => written, "valueName" => value_name)
end
