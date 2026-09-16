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
    # BOTH layouts (`series_base`) — crop and the corrections write a FLAT store, so hardcoding
    # the series `0/.zattrs` here silently no-opped for them: the OME-XML half of the sync landed and
    # the NGFF half didn't, leaving a store whose t axis said `unit: second, scale: 1.0` while its
    # OME-XML said `TimeIncrement="10.0"`. napari prefers the NGFF value → "0:00:01" per frame.
    zattrs_file = joinpath(series_base(zarr_path), ".zattrs")
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
        write_json_atomic(zattrs_file, raw)
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
`.zattrs`: the legacy viewer's `_read_time_increment` (`napari_bridge.py`) reads `TimeIncrement` from here
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
            # replace an existing attr in place; else insert right after the `<Pixels` token — anchor
            # on the token (not a literal `"<Pixels "`) so a bare `<Pixels>` gets it too. The
            # original separator (space or `>`) is preserved, keeping the tag well-formed.
            tag = occursin(attr_re, tag) ?
                replace(tag, attr_re => "$k=\"$v\"") :
                replace(tag, r"<Pixels\b" => "<Pixels $k=\"$v\""; count = 1)
        end
        new_xml = replace(xml, m.match => tag; count = 1)
        write_atomic(io -> write(io, new_xml), xml_file)
    catch e
        @warn "Could not update OME-XML Pixels attributes" zarr_path exception = e
    end
end

# Meta keys that carry calibration (the ccid.json / import / editor shape). `sync_zarr_calibration!`
# is the single translator from these to the zarr's own copies — used by BOTH the importer and the
# metadata editor so the field→axis/XML mapping never lives in two places.
const _CALIBRATION_META_KEYS = (
    "PhysicalSizeX", "PhysicalSizeY", "PhysicalSizeZ", "PhysicalSizeUnit",
    "TimeIncrement", "TimeIncrementUnit",
)

"""Whether `meta` carries any calibration value worth syncing into the zarr (a `nothing` — a JSON
`null` clear — doesn't count). Lets a caller skip the (non-trivial) object load when there's
nothing to do."""
has_calibration_meta(meta::AbstractDict) =
    any(k -> !isnothing(get(meta, k, nothing)), _CALIBRATION_META_KEYS)

"""
Copy the physical-size/timing values in a `meta`-shaped dict (keys in `_CALIBRATION_META_KEYS` — the
same names ccid.json, the importer, and the frontend editor all use) INTO the zarr's own calibration
copies: the NGFF `.zattrs` scale + axis units (`update_ome_scale!`) and the OME-XML `<Pixels>` attrs
(`update_ome_xml_pixels!`, which napari reads unconditionally for the time interval).

This is the one place both `ImportOmezarr` (materialising its ImageJ Z-spacing fix + DeltaT time
fallback) and `api_images_meta_set` (a user edit) funnel through, so napari always renders the SAME
calibration ccid.json / `img_physical_sizes` already compute with — otherwise the two diverge (the
viewer showing the raw spacing / "t = N" while analysis uses the corrected number). `zarr_path` is
the `"default"` zarr in EITHER layout — series or flat, resolved by `series_base`
(CLAUDE.md → OME-ZARR dual-format).
"""
function sync_zarr_calibration!(zarr_path::String, meta::AbstractDict)
    # numeric scale, per axis
    axis_updates = Dict{String,Float64}()
    for (key, axis) in (("PhysicalSizeX", "x"), ("PhysicalSizeY", "y"),
                        ("PhysicalSizeZ", "z"), ("TimeIncrement", "t"))
        v = get(meta, key, nothing)
        v isa Real && (axis_updates[axis] = Float64(v))
    end

    # NGFF axis units — one PhysicalSizeUnit covers x/y/z; TimeIncrementUnit is the t axis
    unit_updates = Dict{String,String}()
    spatial_unit = get(meta, "PhysicalSizeUnit", nothing)
    if spatial_unit isa AbstractString
        for axis in ("x", "y", "z"); unit_updates[axis] = spatial_unit; end
    end
    time_unit = get(meta, "TimeIncrementUnit", nothing)
    time_unit isa AbstractString && (unit_updates["t"] = time_unit)

    # OME-XML <Pixels> attributes (napari reads the time interval only from here)
    xml_attrs   = Dict{String,String}()
    ome_spatial = spatial_unit isa AbstractString ? ome_xml_unit_name(spatial_unit) : nothing
    for (key, unit_key) in (("PhysicalSizeX", "PhysicalSizeXUnit"), ("PhysicalSizeY", "PhysicalSizeYUnit"),
                            ("PhysicalSizeZ", "PhysicalSizeZUnit"))
        v = get(meta, key, nothing)
        if v isa Real
            xml_attrs[key] = string(Float64(v))
            isnothing(ome_spatial) || (xml_attrs[unit_key] = ome_spatial)
        end
    end
    tv = get(meta, "TimeIncrement", nothing)
    if tv isa Real
        xml_attrs["TimeIncrement"] = string(Float64(tv))
        time_unit isa AbstractString && (xml_attrs["TimeIncrementUnit"] = ome_xml_unit_name(time_unit))
    end

    (isempty(axis_updates) && isempty(unit_updates)) ||
        update_ome_scale!(zarr_path, axis_updates; units = unit_updates)
    isempty(xml_attrs) || update_ome_xml_pixels!(zarr_path, xml_attrs)
end

# ── ccid.json helpers ─────────────────────────────────────────────────────────

