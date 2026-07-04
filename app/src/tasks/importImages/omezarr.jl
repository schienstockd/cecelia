using JSON3

# ── OME-ZARR metadata reader ──────────────────────────────────────────────────

"""
Fallback for the time interval when there's no top-level NGFF `t`-axis scale: many OME-XML
sources (including ones bioformats2raw converts) carry no single `TimeIncrement` on `Pixels`,
only a per-`Plane` `DeltaT` — the interval between frames at `TheZ="0" TheT="1"`. Scrapes
bioformats2raw's `OME/METADATA.ome.xml` sidecar with a plain regex (ports the same idea as the old
R `cciaImage.R::omeXMLTimelapseInfo` crutch; no XML dependency, see `image.jl` header note).
Returns the interval in seconds, or `nothing` if the file/tag isn't there.
"""
function _delta_t_fallback(zarr_path::String)::Union{Float64,Nothing}
    xml_file = joinpath(zarr_path, "OME", "METADATA.ome.xml")
    isfile(xml_file) || return nothing
    try
        xml = read(xml_file, String)
        for m in eachmatch(r"<Plane\b[^>]*?/>", xml)
            tag = m.match
            occursin(r"TheZ=\"0\"", tag) || continue
            occursin(r"TheT=\"1\"", tag) || continue
            dm = match(r"DeltaT=\"([-\d.eE+]+)\"", tag)
            isnothing(dm) && continue
            value = parse(Float64, dm.captures[1])
            um    = match(r"DeltaTUnit=\"([a-zA-Z]+)\"", tag)
            unit  = isnothing(um) ? "s" : lowercase(um.captures[1])
            return unit == "ms" ? value / 1000 : (unit == "min" ? value * 60 : value)
        end
    catch e
        @warn "Could not read OME-XML for DeltaT fallback" zarr_path exception = e
    end
    nothing
end

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

        ax_list    = get(ms, :axes, [])
        axes       = [lowercase(string(get(ax, :name, ""))) for ax in ax_list]
        ax_units   = [haskey(ax, :unit) ? string(ax[:unit]) : nothing for ax in ax_list]
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

                # spatial axes share one calibration unit in practice; take the first present
                spatial_unit = nothing
                for i in (xi, yi, zi)
                    if !isnothing(i) && !isnothing(ax_units[i])
                        spatial_unit = ax_units[i]
                        break
                    end
                end
                isnothing(spatial_unit) || (result["PhysicalSizeUnit"] = spatial_unit)

                # bioformats2raw always writes a t-axis scale (defaulting to 1.0) even when it has
                # no real timing for the file — but it only attaches a `unit` to the t axis when it
                # actually found one. A unit-less t scale is a placeholder, not a reading: trusting
                # it verbatim produced a bogus "TimeIncrement": 1.0 for files with no real interval
                # (and skipped the DeltaT fallback below, since 1.0 isn't the "missing" sentinel).
                if !isnothing(ti) && !isnothing(ax_units[ti])
                    result["TimeIncrement"]     = scale[ti]
                    result["TimeIncrementUnit"] = ax_units[ti]
                end
            end
        end

        # per-plane DeltaT fallback — only when there's genuinely a timelapse and the top-level
        # scale-t gave nothing usable (missing or zero)
        size_t = get(result, "SizeT", 1)
        if size_t > 1 && get(result, "TimeIncrement", 0.0) == 0.0
            fallback = _delta_t_fallback(zarr_path)
            if !isnothing(fallback)
                result["TimeIncrement"]     = fallback
                result["TimeIncrementUnit"] = "second"
            end
        end
    catch e
        @warn "Could not read zarr metadata" zarr_path exception = e
    end

    result
end

"""
Propagate a physical-size/timing correction into the OME-ZARR's OWN `.zattrs` NGFF scale — the
actual value napari (and any other zarr-reading consumer) uses for spatial calibration and
rendering. The metadata editor (`api_images_meta_set`) only wrote `ccid.json`'s `meta` dict (the
API/display copy); that left the zarr itself uncorrected, so napari kept showing the old (wrong)
spacing even after the editor said it was fixed. `updates` maps axis name ("x"/"y"/"z"/"t") to its
new value AT LEVEL 0; every pyramid level's scale for that axis is rescaled by the same ratio
(new/old), so a level-dependent downsampling factor (x/y shrink per level; z/t normally don't) is
preserved rather than clobbered with one flat value.

`units` maps the same axis names to an NGFF unit name ("micrometer"/"second"/…) and rewrites each
axis's `unit` field. This matters for round-tripping: `read_ome_metadata` derives
`PhysicalSizeUnit`/`TimeIncrementUnit` from the axis `unit`, and — for the t axis — will only trust
the t scale AT ALL when a unit is present (a unit-less t scale is treated as a placeholder). So a
correction that changes only the unit, or that adds a real time interval to a file that had none,
must write the unit here too or a later `resync_ome_meta!` re-read wouldn't see it.
"""
function update_ome_scale!(zarr_path::String, updates::Dict{String,Float64};
                           units::Dict{String,String} = Dict{String,String}())
    (isempty(updates) && isempty(units)) && return
    zattrs_file = joinpath(zarr_path, "0", ".zattrs")
    isfile(zattrs_file) || return
    try
        raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(zattrs_file, String)))
        multiscales = get(raw, "multiscales", nothing)
        (isnothing(multiscales) || isempty(multiscales)) && return
        ms = Dict{String,Any}(String(k) => v for (k, v) in first(multiscales))

        ax_list  = get(ms, "axes", [])
        axes     = [lowercase(string(get(ax, :name, ""))) for ax in ax_list]
        datasets = get(ms, "datasets", [])
        isempty(datasets) && return

        changed = false

        # numeric scale — rescale every level by the level-0 ratio (preserves per-level downsampling)
        level0 = Dict{String,Any}(String(k) => v for (k, v) in first(datasets))
        level0_scale = nothing
        for ct in get(level0, "coordinateTransformations", [])
            string(get(ct, :type, "")) == "scale" &&
                (level0_scale = collect(Float64, get(ct, :scale, [])))
        end
        ratios = Dict{Int,Float64}()
        if !isnothing(level0_scale)
            for (axis_name, new_val) in updates
                idx = findfirst(==(axis_name), axes)
                isnothing(idx) && continue
                old_val = level0_scale[idx]
                old_val == 0 && continue
                ratios[idx] = new_val / old_val
            end
        end
        if !isempty(ratios)
            new_datasets = map(datasets) do d
                dd = Dict{String,Any}(String(k) => v for (k, v) in d)
                cts = get(dd, "coordinateTransformations", [])
                new_cts = map(cts) do ct
                    ctd = Dict{String,Any}(String(k) => v for (k, v) in ct)
                    if string(get(ctd, "type", "")) == "scale"
                        scale = collect(Float64, get(ctd, "scale", []))
                        for (idx, r) in ratios
                            scale[idx] *= r
                        end
                        ctd["scale"] = scale
                    end
                    ctd
                end
                dd["coordinateTransformations"] = new_cts
                dd
            end
            ms["datasets"] = new_datasets
            changed = true
        end

        # axis units — see docstring: what read_ome_metadata reads back, and the t-axis trust gate
        if !isempty(units)
            new_axes = map(ax_list) do ax
                axd = Dict{String,Any}(String(k) => v for (k, v) in ax)
                nm  = lowercase(string(get(axd, "name", "")))
                haskey(units, nm) && (axd["unit"] = units[nm])
                axd
            end
            ms["axes"] = new_axes
            changed = true
        end

        changed || return
        multiscales_new    = [ms; multiscales[2:end]...]
        raw["multiscales"] = multiscales_new
        open(zattrs_file, "w") do io; JSON3.write(io, raw); end
    catch e
        @warn "Could not update OME-ZARR scale metadata" zarr_path exception = e
    end
end

# NGFF unit names (what the frontend/ccid.json use) → OME-XML's unit abbreviations
const _OME_XML_UNIT = Dict(
    "micrometer" => "µm", "nanometer" => "nm", "millimeter" => "mm",
    "second" => "s", "minute" => "min",
)

"""Map an NGFF unit name (e.g. `"micrometer"`) to its OME-XML abbreviation (`"µm"`), or pass the
value through unchanged if it's not one of the known NGFF names (e.g. already an abbreviation)."""
ome_xml_unit_name(ngff_unit::AbstractString)::String = get(_OME_XML_UNIT, ngff_unit, ngff_unit)

"""
Patch `OME/METADATA.ome.xml`'s `<Pixels>` attributes directly (regex text edit — no XML
dependency, see `image.jl` header note). This is a THIRD, separate metadata location from
`.zattrs`: napari's `_read_time_increment` (`napari_bridge.py`) reads `TimeIncrement` from here
UNCONDITIONALLY, with no NGFF/`.zattrs` fallback the way spatial scale has — so correcting
`.zattrs` alone (`update_ome_scale!`) fixes the 3D view but leaves the timestamp overlay showing
the raw frame index ("t = N") because this file still has the old/absent value. `attrs` maps the
OME attribute name (e.g. `"TimeIncrement"`, `"PhysicalSizeZ"`) to its new string value; an
existing attribute is replaced, a missing one is inserted.
"""
function update_ome_xml_pixels!(zarr_path::String, attrs::Dict{String,String})
    isempty(attrs) && return
    xml_file = joinpath(zarr_path, "OME", "METADATA.ome.xml")
    isfile(xml_file) || return
    try
        xml = read(xml_file, String)
        m = match(r"<Pixels\b[^>]*>", xml)
        isnothing(m) && return
        tag = m.match
        for (k, v) in attrs
            attr_re = Regex(k * "=\"[^\"]*\"")
            tag = occursin(attr_re, tag) ?
                replace(tag, attr_re => "$k=\"$v\"") :
                replace(tag, "<Pixels " => "<Pixels $k=\"$v\" "; count = 1)
        end
        new_xml = replace(xml, m.match => tag; count = 1)
        open(xml_file, "w") do io; write(io, new_xml); end
    catch e
        @warn "Could not update OME-XML Pixels attributes" zarr_path exception = e
    end
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

# Keys `read_ome_metadata` is the sole, authoritative source for. Cleared unconditionally before
# merging so a re-import always reflects exactly what THIS read found — never a zombie value
# left over from some earlier (possibly much older, possibly buggy) import that this run's
# zarr_meta simply doesn't happen to produce again (e.g. TimeIncrement staying missing is a real,
# meaningful "we found nothing" that a plain additive merge would otherwise mask forever).
const _OME_DERIVED_META_KEYS = (
    "SizeC", "SizeT", "SizeZ",
    "PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "PhysicalSizeUnit", "PhysicalSizeZ_raw",
    "TimeIncrement", "TimeIncrementUnit",
)

# `overwrite`:
#   true  (import) — re-read is authoritative: clear the derived keys first (see
#          `_OME_DERIVED_META_KEYS`) so a value THIS read no longer produces can't linger as a
#          zombie, and take the fresh channel names.
#   false (backfill / `resync_ome_meta!`) — fill-only: set a key ONLY when it's genuinely absent,
#          never clobber a value already on disk. That value may be a human correction, or the
#          ImageJ-TIFF Z auto-fix, both of which live only in ccid.json and are NOT reproducible by
#          re-reading the zarr — a plain overwrite would silently revert them. Channel names are
#          likewise left untouched (the user may have renamed them).
function _merge_zarr_meta_into_ccid!(img::CciaImage, zarr_meta::Dict;
                                      zarr_filename::Union{String,Nothing} = nothing,
                                      value_name::String = VERSIONED_DEFAULT_VAL,
                                      overwrite::Bool = true)
    isempty(zarr_meta) && isnothing(zarr_filename) && return
    ccid = joinpath(img._dir, "ccid.json")
    try
        raw = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))
        m   = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict()))
        if overwrite
            for k in _OME_DERIVED_META_KEYS
                delete!(m, k)
            end
        end
        for (k, v) in zarr_meta
            if k == "channel_names"
                overwrite && versioned_set_field!(raw, "imChannelNames", collect(String, v))
            elseif overwrite || !haskey(m, k)
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

"""
Backfill an already-imported image's physical-size/timing `meta` fields by re-reading them from
its OME-ZARR — the same reader `ImportOmezarr` uses at import time — WITHOUT re-running
bioformats2raw. For images converted before this metadata was tracked (or whose `meta` predates
the `PhysicalSizeUnit`/`TimeIncrementUnit` fields), the zarr itself is already correct; only
`ccid.json`'s `meta` dict is stale/missing these keys.

Strictly a FILL-ONLY backfill (`overwrite=false`): it adds fields that are genuinely absent and
never overwrites one already on disk. This is NOT equivalent to a fresh import — it does not re-run
the ImageJ-TIFF Z-spacing auto-fix (that step lives in the import task, outside `read_ome_metadata`,
and its result — a corrected `PhysicalSizeZ` + `PhysicalSizeZ_raw` marker — is stored only in
ccid.json). Overwriting would silently revert both that auto-fix and any human correction back to
bioformats2raw's raw value; fill-only makes resync safe to run on any image, corrected or not.

Deliberately reads the `VERSIONED_DEFAULT_VAL` ("default") zarr — the original bioformats2raw
output — rather than whichever version is currently `active`. Downstream tasks (drift/AF
correction, cellpose correction) write their own zarr with a plain NGFF layout that has no `unit`
on its axes and no OME-XML sidecar with calibration; `read_ome_metadata` only understands the
bioformats2raw nested-series layout, so pointing this at an `active` post-processing output
silently found nothing. Physical size/timing are acquisition properties anyway — unaffected by
which processed variant happens to be active for viewing.

Returns `false` (no-op) when the default zarr path is missing or has no usable metadata.
"""
function resync_ome_meta!(img::CciaImage)::Bool
    zarr_path = img_filepath(img, VERSIONED_DEFAULT_VAL)
    (isnothing(zarr_path) || !isdir(zarr_path)) && return false
    zarr_meta = read_ome_metadata(zarr_path)
    isempty(zarr_meta) && return false
    _merge_zarr_meta_into_ccid!(img, zarr_meta; overwrite = false)
    true
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

    # ImageJ-sourced TIFFs: bioformats2raw applies the source's calibration-unit conversion
    # correctly for X/Y but not for Z, so a non-micron unit (e.g. an ImageJ file saved with
    # unit=inch) leaves PhysicalSizeZ wildly wrong. Re-derive it ourselves from the original
    # file's ImageJ tags rather than trust bioformats2raw's raw value — only for TIFF sources,
    # cheap extension check first so every import doesn't pay for a Python subprocess.
    if endswith(lowercase(src_path), ".tif") || endswith(lowercase(src_path), ".tiff")
        run_dir     = task_run_dir(img._dir)
        result_file = joinpath(run_dir, "read_imagej_physical_size.$(string(rand(UInt32); base = 16)).result.json")
        ok_z = run_py("tasks/importImages/read_imagej_physical_size_run.py",
            (; imPath = src_path, resultPath = result_file), run_dir;
            on_log = on_log)
        if ok_z && isfile(result_file)
            try
                corrected = JSON3.read(read(result_file, String))
                if haskey(corrected, :PhysicalSizeZ)
                    raw_z = get(zarr_meta, "PhysicalSizeZ", nothing)
                    new_z = Float64(corrected[:PhysicalSizeZ])
                    on_log("[INFO] Corrected Z spacing from source ImageJ metadata (unit=$(get(corrected, :sourceUnit, "?"))): $raw_z -> $new_z um")
                    isnothing(raw_z) || (zarr_meta["PhysicalSizeZ_raw"] = raw_z)
                    zarr_meta["PhysicalSizeZ"] = new_z
                end
            catch e
                @warn "Could not read ImageJ physical-size result" exception = e
            finally
                rm(result_file; force = true)
            end
        end
    end

    _update_image_status!(img, "done")
    _merge_zarr_meta_into_ccid!(img, zarr_meta;
                                zarr_filename = basename(zarr_out),
                                value_name    = value_name)

    merge(zarr_meta, Dict{String,Any}(
        "valueName" => value_name,
        "filename"  => basename(zarr_out),
    ))
end
