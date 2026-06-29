using JSON3

# ── OME-ZARR metadata reader ──────────────────────────────────────────────────

"""
Read OME-ZARR metadata (axes, shape, channel names, physical pixel sizes) produced by
bioformats2raw. bioformats2raw wraps arrays in a series group: multiscales in zarr/0/.zattrs.
Returns a flat Dict with keys SizeC, SizeT, SizeZ, optionally channel_names, and the physical
scale per axis (PhysicalSizeX/Y/Z µm/px, TimeIncrement s/frame) from the level-0 NGFF
coordinate transform — read here so `img_physical_sizes` is a pure-Julia `meta` lookup.
"""
function read_ome_metadata(zarr_path::String)::Dict{String,Any}
    result = Dict{String,Any}()
    zattrs_file = joinpath(zarr_path, "0", ".zattrs")
    isfile(zattrs_file) || return result

    try
        zattrs     = JSON3.read(read(zattrs_file, String))
        multiscales = get(zattrs, :multiscales, nothing)
        (isnothing(multiscales) || isempty(multiscales)) && return result
        ms = first(multiscales)

        axes       = [lowercase(string(get(ax, :name, ""))) for ax in get(ms, :axes, [])]
        datasets   = get(ms, :datasets, [])
        level_path = isempty(datasets) ? "0" : string(get(first(datasets), :path, "0"))

        zarray_file = joinpath(zarr_path, "0", level_path, ".zarray")
        if isfile(zarray_file)
            zarray = JSON3.read(read(zarray_file, String))
            shape  = collect(Int, get(zarray, :shape, []))
            if length(shape) == length(axes)
                idx(name) = findfirst(==(name), axes)
                ci = idx("c"); ti = idx("t"); zi = idx("z")
                result["SizeC"] = isnothing(ci) ? 1 : shape[ci]
                result["SizeT"] = isnothing(ti) ? 1 : shape[ti]
                result["SizeZ"] = isnothing(zi) ? 1 : shape[zi]
            end
        end

        omero    = get(zattrs, :omero, nothing)
        channels = isnothing(omero) ? [] : get(omero, :channels, [])
        if !isempty(channels)
            result["channel_names"] = [
                string(get(ch, :label, "Ch$(i-1)")) for (i, ch) in enumerate(channels)
            ]
        end

        # physical pixel sizes from the level-0 `scale` coordinate transform (OME-NGFF).
        # `scale[]` aligns with `axes`; map by axis name. Pure-Julia (no ome_types/Python).
        if !isempty(datasets) && !isempty(axes)
            aidx(name) = findfirst(==(name), axes)
            scale = nothing
            for ct in get(first(datasets), :coordinateTransformations, [])
                string(get(ct, :type, "")) == "scale" &&
                    (scale = collect(Float64, get(ct, :scale, [])))
            end
            if !isnothing(scale) && length(scale) == length(axes)
                xi = aidx("x"); yi = aidx("y"); zi = aidx("z"); ti = aidx("t")
                isnothing(xi) || (result["PhysicalSizeX"] = scale[xi])
                isnothing(yi) || (result["PhysicalSizeY"] = scale[yi])
                isnothing(zi) || (result["PhysicalSizeZ"] = scale[zi])
                isnothing(ti) || (result["TimeIncrement"] = scale[ti])
            end
        end
    catch e
        @warn "Could not read zarr metadata" zarr_path exception = e
    end

    result
end

# ── ccid.json helpers ─────────────────────────────────────────────────────────

function _update_image_status!(img::CciaImage, status::String)
    ccid = joinpath(img._dir, "ccid.json")
    try
        raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
        raw["status"] = status
        open(ccid, "w") do io; JSON3.write(io, raw); end
    catch e
        @warn "Could not update image status" exception = e
    end
end

function _merge_zarr_meta_into_ccid!(img::CciaImage, zarr_meta::Dict;
                                      zarr_filename::Union{String,Nothing} = nothing,
                                      value_name::String = VERSIONED_DEFAULT_VAL)
    isempty(zarr_meta) && isnothing(zarr_filename) && return
    ccid = joinpath(img._dir, "ccid.json")
    try
        raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
        m   = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict()))
        for (k, v) in zarr_meta
            if k == "channel_names"
                versioned_set_field!(raw, "imChannelNames", collect(String, v))
            else
                m[k] = v
            end
        end
        raw["meta"] = m
        !isnothing(zarr_filename) &&
            versioned_set_field!(raw, "filepath", zarr_filename, value_name)
        open(ccid, "w") do io; JSON3.write(io, raw); end
    catch e
        @warn "Could not update image metadata" exception = e
    end
end

# ── Task ──────────────────────────────────────────────────────────────────────

struct ImportOmezarr <: CciaTask end

function _run_task(task::ImportOmezarr, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))

    # Source path: explicit param > stored ori_path in metadata
    src_path = string(get(params, "src_path",
                     get(img.meta, "ori_path", "")))

    if isempty(src_path) || !isfile(src_path)
        on_log("[ERROR] Source file not found: $(isempty(src_path) ? "(no src_path)" : src_path)")
        return nothing
    end

    zarr_out      = joinpath(img_zero_dir(img), "ccidImage.ome.zarr")
    pyramid_scale = Int(get(params, "pyramidScale", 2))

    bf2raw = bioformats2raw_bin()
    if !isfile(bf2raw)
        on_log("[ERROR] bioformats2raw not found at $bf2raw")
        on_log("[ERROR] Set dirs.bioformats2raw in ~/cecelia-pineapple/dev/custom.toml")
        return nothing
    end

    if isdir(zarr_out)
        on_log("[INFO] Removing previous output: $zarr_out")
        rm(zarr_out; recursive = true)
    end

    on_log("[INFO] Source:  $src_path")
    on_log("[INFO] Output:  $zarr_out")
    on_log("[INFO] Pyramid: $pyramid_scale levels")

    out_pipe = Pipe()
    proc = run(pipeline(`$bf2raw --resolutions $pyramid_scale $src_path $zarr_out`;
                        stdout = out_pipe, stderr = out_pipe); wait = false)
    close(out_pipe.in)
    on_process(proc)

    src_size = filesize(src_path)
    monitor  = @async begin
        while process_running(proc)
            if isdir(zarr_out) && src_size > 0
                p = min(_dir_bytes(zarr_out) / src_size, 0.98)
                on_progress(round(Int, p * 100), 100)
            end
            sleep(2)
        end
    end

    for line in eachline(out_pipe); on_log(line); end
    wait(proc)
    wait(monitor)

    ok = proc.exitcode == 0 && proc.termsignal == 0
    ok || return nothing

    on_progress(1, 1)
    on_log("[INFO] Conversion complete.")

    zarr_meta = read_ome_metadata(zarr_out)
    _update_image_status!(img, "done")
    _merge_zarr_meta_into_ccid!(img, zarr_meta;
                                zarr_filename = basename(zarr_out),
                                value_name    = value_name)

    merge(zarr_meta, Dict{String,Any}(
        "valueName" => value_name,
        "filename"  => basename(zarr_out),
    ))
end
