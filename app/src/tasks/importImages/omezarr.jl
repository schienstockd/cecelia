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
        # match the <Plane …> opening tag whether self-closing (`/>`, bioformats2raw) or not
        # (`>…</Plane>`, some vendors) — DeltaT is an attribute on the opening tag either way
        for m in eachmatch(r"<Plane\b[^>]*?>", xml)
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
    _recover_ims_time_increment!(zarr_meta, src_path, run_dir; on_log) -> Bool

Fill `TimeIncrement`/`TimeIncrementUnit` from an Imaris `.ims` SOURCE when the converted store has
none. Returns whether it filled anything.

Bio-Formats' `ImarisHDFReader` reports no `TimeIncrement` and no per-plane `DeltaT` at all — it loads
the file's timing into the unstructured original-metadata table (what ImageJ's "Show Info" prints)
and never promotes it into the OME model. So bioformats2raw writes a store with no interval, and
`_delta_t_fallback` above has nothing to scrape: the number is in the source file and only reachable
by reading it directly. Same shape as the ImageJ Z-spacing fix in the task below, and for the same
reason — a source-specific calibration recovery Bio-Formats doesn't hand us.

Only ever ADDS a value: if the store already carried an interval, that is what bioformats2raw
actually found and it wins. An irregular series yields nothing (the runner refuses to flatten it to a
median) — logged, because "no interval" and "an interval we declined to guess" are different answers
and only the second is worth acting on.
"""
function _recover_ims_time_increment!(zarr_meta::Dict{String,Any}, src_path::AbstractString,
                                      run_dir::AbstractString;
                                      on_log::Function = _ -> nothing)::Bool
    endswith(lowercase(src_path), ".ims") || return false
    isfile(src_path) || return false

    result_file = joinpath(run_dir, "read_ims_time_interval.$(string(rand(UInt32); base = 16)).result.json")
    try
        ok = run_py("tasks/importImages/read_ims_time_interval_run.py",
                    (; imPath = src_path, resultPath = result_file), run_dir; on_log = on_log)
        (ok && isfile(result_file)) || return false
        res = JSON3.read(read(result_file, String))
        if haskey(res, :TimeIncrement)
            zarr_meta["TimeIncrement"] = Float64(res[:TimeIncrement])
            # The runner returns SECONDS and no unit, on purpose: ccid/NGFF spell it `second` and
            # OME-XML spells it `s`, so the unit belongs to whoever stores the value. Stamped here
            # exactly as the `_delta_t_fallback` path does; `sync_zarr_calibration!` converts at the
            # OME-XML boundary (enforced by `test_ome_unit_symbols.py`).
            zarr_meta["TimeIncrementUnit"] = "second"
            nominal = get(res, :nominal, false) === true
            on_log("[INFO] Frame interval $(zarr_meta["TimeIncrement"]) s recovered from the Imaris " *
                   "source ($(get(res, :source, "?"))$(nominal ? ", nominal" : ""))")
            return true
        end
        on_log("[WARN] No frame interval in the Imaris source: $(get(res, :reason, "unknown"))")
    catch e
        @warn "Could not read Imaris time interval" src_path exception = e
    finally
        rm(result_file; force = true)
    end
    false
end

"""
Directory whose `.zattrs` carries the NGFF `multiscales` — the bioformats2raw series wrapper
(`zarr/0`) or, for a flat `create_multiscales` store, `zarr` itself. Julia mirror of Python's
`zarr_utils.series_base`; the ONE place on this side that decides the layout, so a reader or a
writer can't quietly understand only one of the two (docs/ARCHITECTURE.md → **OME-ZARR dual-format**).

Detection is STRUCTURAL — does `0/.zattrs` actually carry a `multiscales` attr — not the path
suffix, because both layouts have a `0/` child: a group in the series layout, the level-0 ARRAY
(whose `.zattrs` is `{}`) in the flat one.
"""
function series_base(zarr_path::AbstractString)::String
    series = joinpath(zarr_path, "0")
    ms = ngff_multiscales(series)
    (isnothing(ms) || isempty(ms)) || return series
    String(zarr_path)
end

"""
NGFF attributes of a zarr GROUP directory, for **either** zarr format. `nothing` when the directory
carries no readable group metadata.

This is the Julia half of the v2-vs-v3 question, and it lives next to `series_base` on purpose: one
resolver per question per language (docs/ARCHITECTURE.md → **OME-ZARR dual-format**). Do NOT add a parallel set
of v3 readers — route through here.

Name matches the Python twin (`zarr_utils.ngff_attrs`) on purpose — the cross-language contract on
this question is by name.

* zarr v2 / NGFF 0.4 — `.zattrs`, attributes at the **top level**
* zarr v3 / NGFF 0.5 — `zarr.json` → `attributes`, everything nested one level down under **`ome`**

The *content* is identical in both (same axes, datasets, coordinateTransformations), which is why
unwrapping is the whole difference. Python gets this cheaper because `zarr-python`'s `Group.attrs`
already hides the file-level half (its `ngff_attrs` takes an `attrs` object); Julia reads the JSON
itself, so it handles both halves here and takes a directory path.
"""
function ngff_attrs(group_dir::AbstractString)
    zattrs = joinpath(group_dir, ".zattrs")
    if isfile(zattrs)
        try
            return JSON3.read(read(zattrs, String))
        catch
            return nothing
        end
    end
    zjson = joinpath(group_dir, "zarr.json")
    isfile(zjson) || return nothing
    try
        attrs = get(JSON3.read(read(zjson, String)), :attributes, nothing)
        isnothing(attrs) && return nothing
        inner = get(attrs, :ome, nothing)      # NGFF 0.5 nests under `ome`; 0.4-in-v3 would not
        isnothing(inner) ? attrs : inner
    catch
        nothing
    end
end

"""
`multiscales` list of a zarr group directory, or `nothing` — version-agnostic (see `ngff_attrs`).
"""
function ngff_multiscales(group_dir::AbstractString)
    attrs = ngff_attrs(group_dir)
    isnothing(attrs) && return nothing
    ms = get(attrs, :multiscales, nothing)
    (isnothing(ms) || isempty(ms)) ? nothing : ms
end

"""
OME-NGFF spec version a store declares (e.g. `"0.4"`, `"0.5"`), or `nothing` when it declares none.

Not the same question as the ZARR format, and the two are not interchangeable even though they move
together in practice: the zarr format is how the bytes and metadata files are laid out, the NGFF
version is which image-metadata spec those attributes follow. Reported side by side in the image
metadata modal so "what is this store?" is answerable without opening a terminal.

0.5 carries it on the `ome` group attribute (which `ngff_attrs` has already unwrapped by the
time we see it); 0.4 and earlier carry it per-multiscales-entry.
"""
function ngff_version(zarr_path::AbstractString)
    base  = series_base(zarr_path)
    attrs = ngff_attrs(base)
    isnothing(attrs) && return nothing
    v = get(attrs, :version, nothing)                     # NGFF 0.5 (on the `ome` attribute)
    isnothing(v) || return string(v)
    ms = get(attrs, :multiscales, nothing)                # NGFF 0.4 and earlier (per entry)
    (isnothing(ms) || isempty(ms)) && return nothing
    mv = get(first(ms), :version, nothing)
    isnothing(mv) ? nothing : string(mv)
end

"""
Metadata of a zarr ARRAY directory (`shape`, `chunks`, dtype/codecs), for either format — `.zarray`
(v2) or `zarr.json` (v3). `nothing` when unreadable. Both carry `shape`, so a caller that only needs
the extent can treat them alike; anything format-specific must branch explicitly.
"""
function zarr_array_meta(array_dir::AbstractString)
    zarray = joinpath(array_dir, ".zarray")
    if isfile(zarray)
        try
            return JSON3.read(read(zarray, String))   # a `.zarray` IS an array — nothing to discriminate
        catch
            return nothing
        end
    end
    # v3 puts groups AND arrays in the same filename, so here the node type has to be checked —
    # otherwise a group's `zarr.json` would be handed back as if it described an array.
    zjson = joinpath(array_dir, "zarr.json")
    isfile(zjson) || return nothing
    try
        m = JSON3.read(read(zjson, String))
        string(get(m, :node_type, "")) == "array" ? m : nothing
    catch
        nothing
    end
end

"""
Read OME-ZARR metadata (axes, shape, channel names, physical pixel sizes). Handles BOTH layouts
via `series_base` — bioformats2raw's series wrapper (multiscales in `zarr/0/.zattrs`) and the flat
`create_multiscales` store that crop and the correction tasks write (multiscales at the root).
Returns a flat Dict with keys SizeC, SizeT, SizeZ, optionally channel_names, and the physical
scale per axis (PhysicalSizeX/Y/Z µm/px, TimeIncrement s/frame) from the level-0 NGFF
coordinate transform — read here so `img_physical_sizes` is a pure-Julia `meta` lookup.
"""
function read_ome_metadata(zarr_path::String)::Dict{String,Any}
    result = Dict{String,Any}()
    base  = series_base(zarr_path)
    zattrs = ngff_attrs(base)          # v2 `.zattrs` or v3 `zarr.json`→attributes[→ome]
    isnothing(zattrs) && return result

    try
        multiscales = get(zattrs, :multiscales, nothing)
        (isnothing(multiscales) || isempty(multiscales)) && return result
        ms = first(multiscales)

        ax_list    = get(ms, :axes, [])
        axes       = [lowercase(string(get(ax, :name, ""))) for ax in ax_list]
        ax_units   = [haskey(ax, :unit) ? string(ax[:unit]) : nothing for ax in ax_list]
        datasets   = get(ms, :datasets, [])
        level_path = isempty(datasets) ? "0" : string(get(first(datasets), :path, "0"))

        zarray = zarr_array_meta(joinpath(base, level_path))
        if !isnothing(zarray)
            shape = collect(Int, get(zarray, :shape, []))
            if length(shape) == length(axes)
                idx(name) = findfirst(==(name), axes)
                ci = idx("c"); ti = idx("t"); zi = idx("z")
                result["SizeC"] = isnothing(ci) ? 1 : shape[ci]
                result["SizeT"] = isnothing(ti) ? 1 : shape[ti]
                result["SizeZ"] = isnothing(zi) ? 1 : shape[zi]
                # NOT SizeX/SizeY. They were added here and reverted (2026-07-30): the X/Y extent is
                # NOT a per-image property. `filepath` is versioned, and drift correction expands the
                # canvas (see the `output.canvas_expansion` QC finding) while a crop shrinks it — so a
                # flat SizeX/SizeY describes the default import and silently misdescribes whichever
                # version is actually active. A consumer that needs the frame extent must ask for a
                # SPECIFIC version: `GET /api/images/geometry` reads it off that version's store.
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

function _update_image_status!(img::CciaImage, status::String)
    try
        commit_state!(img) do raw
            raw["status"] = status
        end
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

"""
Reconcile the channel names a fresh store read produced with the ones already in `ccid.json`.

Channel names are the one "authoritative re-read" value a human routinely edits, and the edit lives
ONLY in ccid.json: bioformats2raw writes the vendor's own labels (`CH1`…`CHn`) into the store's
`omero.channels[].label`, so re-reading a store ALWAYS yields placeholders — never the names the
user typed. Taking the fresh read verbatim on a *re*-import therefore silently reverts every rename,
which is the same failure the fill-only merge guards against for a human calibration correction
(see `resync_ome_meta!`), with a nastier tail: saved task params reference channels BY NAME
(`af_combinations_for_python`, `cellpose_models_for_python`), so a reset turns a live `"CD169-Kat"`
into a name that is no longer in the list and resolves to nothing instead of erroring.

So the fresh names are taken only when there is nothing to preserve (first import, or an image
un-imported by `remove_image_version!`), or when the count no longer matches — stored names cannot
describe a store with a different channel count, and the fresh ones are then the only valid list.
Deliberate renaming stays the API's job (`set_channel_names!`).

Returns `:set` (fresh names written) or `:kept` (existing names preserved).
"""
function _merge_channel_names!(raw::Dict{String,Any}, fresh)::Symbol
    fresh_names = collect(String, fresh)
    stored      = versioned_get_field(raw, "imChannelNames", VERSIONED_DEFAULT_VAL)
    if stored isa AbstractVector && !isempty(stored) && length(stored) == length(fresh_names)
        return :kept
    end
    versioned_set_field!(raw, "imChannelNames", fresh_names)
    :set
end

# `overwrite`:
#   true  (import) — re-read is authoritative for the DERIVED keys: clear them first (see
#          `_OME_DERIVED_META_KEYS`) so a value THIS read no longer produces can't linger as a
#          zombie. Channel names are the documented exception — see `_merge_channel_names!`.
#   false (backfill / `resync_ome_meta!`) — fill-only: set a key ONLY when it's genuinely absent,
#          never clobber a value already on disk. That value may be a human correction, or the
#          ImageJ-TIFF Z auto-fix, both of which live only in ccid.json and are NOT reproducible by
#          re-reading the zarr — a plain overwrite would silently revert them. Channel names are
#          likewise left untouched (the user may have renamed them).
#
# Returns the resulting `meta` dict as committed (empty when nothing was written), so a caller can
# act on the merged result without re-deriving the merge rule — `resync_ome_meta!` needs it to push
# the same values back into the zarr.
function _merge_zarr_meta_into_ccid!(img::CciaImage, zarr_meta::Dict;
                                      zarr_filename::Union{String,Nothing} = nothing,
                                      value_name::String = VERSIONED_DEFAULT_VAL,
                                      overwrite::Bool = true,
                                      on_log::Function = _ -> nothing)::Dict{String,Any}
    merged = Dict{String,Any}()
    isempty(zarr_meta) && isnothing(zarr_filename) && return merged
    ch_action = :none
    try
        commit_state!(img) do raw
            m = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict()))
            if overwrite
                for k in _OME_DERIVED_META_KEYS
                    delete!(m, k)
                end
            end
            for (k, v) in zarr_meta
                if k == "channel_names"
                    overwrite && (ch_action = _merge_channel_names!(raw, v))
                elseif overwrite || !haskey(m, k)
                    m[k] = v
                end
            end
            raw["meta"] = m
            merged = m
            !isnothing(zarr_filename) &&
                versioned_set_field!(raw, "filepath", zarr_filename, value_name)
        end
    catch e
        @warn "Could not update image metadata" exception = e
    end
    # A re-import keeping the user's names is invisible otherwise — the store and ccid.json now
    # disagree by design, so say which list won and what the other one was.
    if ch_action === :kept
        on_log("[INFO] Kept the existing channel names; the store labels its channels " *
               join(collect(String, zarr_meta["channel_names"]), ", "))
    end
    merged
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

Deliberately reads the `VERSIONED_DEFAULT_VAL` ("default") zarr — the import output — rather than
whichever version is currently `active`: physical size/timing are ACQUISITION properties, and the
default is the one store the importer syncs its corrections into (`sync_zarr_calibration!`). A
post-processing output (drift/AF/cellpose) is a derived copy, and pointing this at it would make
the answer depend on which variant happens to be selected for viewing.

Then pushes the merged result back the OTHER way (`sync_zarr_calibration!`), so resync converges the
two copies instead of only reading one of them. ccid.json is the authoritative side — it holds the
human corrections and the values analysis computes with — and a zarr that disagrees is exactly the
divergence `sync_zarr_calibration!` exists to prevent. Without this, an image whose ccid `meta` is
right but whose store is stale (e.g. an import whose NGFF write was skipped) had no repair path
short of re-importing: the metadata editor only syncs fields the user actually re-types.

Returns `false` (no-op) when the default zarr path is missing or has no usable metadata.
"""
function resync_ome_meta!(img::CciaImage)::Bool
    zarr_path = img_filepath(img, VERSIONED_DEFAULT_VAL)
    (isnothing(zarr_path) || !isdir(zarr_path)) && return false
    zarr_meta = read_ome_metadata(zarr_path)
    isempty(zarr_meta) && return false

    # An Imaris timelapse imported before the source-timing recovery existed has no interval anywhere
    # — not in the store, not in ccid — so re-reading the store can't produce one. Go back to the
    # source file, which is the whole point of this being the repair path that needs no re-import.
    if get(zarr_meta, "SizeT", 1) > 1 && get(zarr_meta, "TimeIncrement", 0.0) == 0.0
        src = string(get(img.meta, "ori_path", ""))
        isempty(src) || _recover_ims_time_increment!(zarr_meta, src, task_run_dir(img._dir))
    end

    merged = _merge_zarr_meta_into_ccid!(img, zarr_meta; overwrite = false)
    has_calibration_meta(merged) && sync_zarr_calibration!(zarr_path, merged)
    write_metadata_qc!(img)     # recompute calibration QC from the refreshed meta
    true
end

# ── Task ──────────────────────────────────────────────────────────────────────

# Files belonging to ONE multi-file image: the main file plus Olympus OIR companions, which are named
# `<stem>_<5 digits><ext>` (e.g. `Img.oir` + `Img_00001.oir`, `Img_00002.oir`). Pure → unit-tested.
# bioformats auto-discovers companions in a directory, so once the whole set is staged under the same
# names, pointing bioformats2raw at the copied main file just works.
#
# Ported from the old R `prepFilelistToSync` (cciaHelpers.R), including its two hard-won lessons:
#  1. Match the stem LITERALLY (`startswith`), never by interpolating it into a regex — a filename with
#     regex metacharacters (their example: `basal+NECA`) breaks an interpolated `"<stem>_[0-9]+"` pattern.
#  2. Require the companion suffix to be `_` + EXACTLY five digits, so a sibling image like
#     `Img_processed.oir` / `Img_v2.oir` isn't mistaken for a companion of `Img.oir`.
function _companion_files(names::AbstractVector{<:AbstractString}, main::AbstractString)
    stem   = first(splitext(main))       # main name WITHOUT extension, e.g. "…-res_0001"
    prefix = stem * "_"
    filter(names) do n
        n == main && return true
        # Olympus OIR companions are `<main-stem>_<digits>` and are typically EXTENSIONLESS, e.g.
        # `…-res_0001.oir` (main) + `…-res_0001_00001`, `…-res_0001_00002`, … . So match a numeric
        # run after `<stem>_` (with an optional extension), not a fixed 5-digit + same-extension shape
        # — the earlier rule matched none of these, so only the main file staged and bioformats saw a
        # fraction of the timepoints. Literal stem prefix still avoids grabbing a sibling `…-res_0002`.
        startswith(n, prefix) || return false
        occursin(r"^[0-9]+(\.[^.]+)?$", chop(n; head = length(prefix), tail = 0))
    end
end

"""
Copy a source image (+ its companion file set) to a local scratch dir and return the path to the
copied main file. Reading a multi-file format like Olympus OIR directly over SMB is dominated by
per-read network latency (bioformats does many small random seeks); a bulk sequential copy is
throughput-bound and far faster — this automates the manual copy-to-tmp workaround.

`_companion_files` matches the main file + its companions by LITERAL stem prefix — never interpolate
the stem into a regex, `basal+NECA` would break it. Real Olympus naming: the registered file already
ends in `_NNNN.oir` and the companions are EXTENSIONLESS (`…-res_0001.oir` + `…-res_0001_00001`, …),
so the match is `<main-stem>_<digits>` with an OPTIONAL extension, not a fixed `_<5 digits><same-ext>`.
The first version matched none of the extensionless parts, so only the main file staged and bioformats
saw ~4 of 181 timepoints. The literal-stem prefix still excludes a sibling acquisition (`…-res_0002`).
"""
# Copy one file in chunks, yielding between blocks. Julia's `cp` is a single NON-yielding blocking
# call (`jl_fs_sendfile`); when the pool worker running it is scheduled onto the event-loop thread, a
# multi-GB copy freezes the WS server (and the whole GUI) until it finishes. A chunked loop with an
# explicit `yield()` keeps the scheduler/event loop responsive, and lets us report progress.
function _copy_file_yielding(src::AbstractString, dst::AbstractString;
                             chunk::Int = 8 * 1024 * 1024, on_bytes::Function = _ -> nothing)
    buf = Vector{UInt8}(undef, chunk)
    open(src, "r") do s
        open(dst, "w") do d
            while !eof(s)
                n = readbytes!(s, buf, chunk)
                write(d, view(buf, 1:n))
                on_bytes(n)
                yield()   # let the WS event loop + other tasks run between chunks
            end
        end
    end
end

function _stage_source!(src_path::AbstractString, stage_dir::AbstractString;
                        on_log::Function = _ -> nothing, on_progress::Function = (_, _) -> nothing)
    src_dir = dirname(src_path)
    files   = _companion_files(readdir(src_dir), basename(src_path))
    isempty(files) && (files = [basename(src_path)])
    mkpath(stage_dir)

    grand_total = sum(f -> filesize(joinpath(src_dir, f)), files; init = 0)
    on_log("[INFO] Staging $(length(files)) source file(s), $(round(grand_total / 1e9; digits = 1)) GB, to local scratch …")

    copied = 0
    for f in files
        _copy_file_yielding(joinpath(src_dir, f), joinpath(stage_dir, f);
                            on_bytes = n -> (copied += n; grand_total > 0 && on_progress(copied, grand_total)))
    end
    on_log("[INFO] Staged $(length(files)) file(s) ($(round(copied / 1e9; digits = 1)) GB).")
    joinpath(stage_dir, basename(src_path))
end

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
    pyramid_levels = Int(get(params, "pyramidLevels", get(params, "pyramidScale", 2)))

    bf2raw = bioformats2raw_bin()
    if !isfile(bf2raw)
        on_log("[ERROR] bioformats2raw not found at $bf2raw")
        on_log("[ERROR] Set dirs.bioformats2raw in $(custom_toml_path())")
        return nothing
    end

    # clean any previous outputs (the final store, a leftover stage dir)
    stage_dir = joinpath(img_zero_dir(img), "_stage_src")
    for d in unique([zarr_out, stage_dir])
        if isdir(d)
            on_log("[INFO] Removing previous output: $d")
            rm(d; recursive = true)
        end
    end

    # Stage the source locally first when reading from a slow/network location (SMB): copies the whole
    # companion set to local scratch, then bioformats2raw reads at disk speed. Deleted right after the
    # conversion's source read finishes (independent of the 16-bit transient).
    stage_local = Bool(get(params, "stageLocal", false))
    eff_src     = src_path
    if stage_local
        try
            eff_src = _stage_source!(src_path, stage_dir; on_log = on_log, on_progress = on_progress)
        catch e
            on_log("[ERROR] Failed to stage source locally: $e")
            rm(stage_dir; recursive = true, force = true)
            return nothing
        end
    end

    on_log("[INFO] Source:  $src_path")
    on_log("[INFO] Output:  $zarr_out")
    on_log("[INFO] Pyramid: $pyramid_levels levels")
    stage_local && on_log("[INFO] Staged source locally (network-source speedup).")

    # Tell bioformats2raw to use the configured compressor — it defaults to blosc/lz4-5, which would
    # leave the imported original encoded differently from every correction derived from it.
    compression = bf2raw_compression_flags()
    on_log("[INFO] Compression: $(image_compressor())")

    # Chunk (bioformats2raw calls it the TILE) size. This param existed in the JSON for a long time as
    # `chunkSizeX`/`chunkSizeY` and was read by NOTHING — no tile flag ever reached the CLI, so a user
    # who set 512 still got bioformats2raw's 1024. One control now, and it is actually passed.
    chunk_flags = bf2raw_chunk_flags(get(params, "chunkSize", "auto"))
    on_log("[INFO] Chunk size: $(isempty(chunk_flags) ? "auto (1024, capped to the frame)" : chunk_flags[2])")

    # Store FORMAT — chosen here and only here; every derived store inherits it (ZARR_V3_PLAN D9).
    # `z_planes` lets "all z" resolve to a real depth; the source is not converted yet, so it comes from
    # ccid meta when a previous import recorded it, else 0 (which drops the flag rather than guessing).
    # Unset params fall back to the Settings DEFAULTS, not to hardcoded literals — Settings is where the
    # store-layout default lives and the import form pre-fills from it (ZARR_V3_PLAN D10). A run
    # launched headlessly (REPL, chain) therefore gets the same layout as one launched from the form.
    fmt_flags = bf2raw_format_flags(
        get(params, "ngffVersion", store_layout().ngffVersion), get(params, "shardSize", "auto");
        shard_depth = get(params, "shardDepth", "1"),
        z_planes    = Int(get(img.meta, "SizeZ", 0)))
    on_log("[INFO] Format: $(isempty(fmt_flags) ? "NGFF 0.4 (zarr v2), nested keys" : join(fmt_flags, " "))")

    # Worker/heap controls — key defaults on the SOURCE extension so Imaris (`.ims`) gets the safe
    # floor without the user having to know why. Measured 2026-08-27 on `Human_Lymph_Node_Manual_IBEX.ims`:
    # bioformats2raw's `--max-workers=4` + JVM-default heap = 105 OOMs, 0-3 chunks written before the
    # task terminated (`H5tiledLayoutBB\$DataChunk.getByteBuffer` → `Deflate.decode`). At workers=2,
    # -Xmx16g: 2 OOMs and 3820 chunks / 1.5 GB written. At workers=1: OOMs go to 0. That's why the
    # auto-default for `.ims` is 1, not 2. See `bf2raw_worker_flags` / `bf2raw_default_workers`.
    worker_choice = string(get(params, "maxWorkers", "auto"))
    if lowercase(strip(worker_choice)) == "auto"
        worker_choice = bf2raw_default_workers(eff_src)
    end
    worker_flags = bf2raw_worker_flags(worker_choice)

    heap_choice = get(params, "jvmHeapGiB", "auto")
    heap_gib    = bf2raw_java_heap_gib(heap_choice)
    if heap_gib == 0 && lowercase(strip(string(heap_choice))) == "auto"
        heap_gib = bf2raw_default_heap_gib(eff_src)
    end
    java_env = bf2raw_java_env(heap_gib)

    on_log("[INFO] Workers: $(isempty(worker_flags) ? "auto (bioformats2raw default: 4)" : worker_flags[1])")
    on_log("[INFO] JVM heap: $(heap_gib > 0 ? "-Xmx$(heap_gib)g" : "auto (JVM default)")")

    cmd = `$bf2raw --resolutions $pyramid_levels $compression $chunk_flags $fmt_flags $worker_flags $eff_src $zarr_out`
    if !isempty(java_env)
        cmd = addenv(cmd, java_env)
    end
    out_pipe = Pipe()
    proc = run(pipeline(cmd; stdout = out_pipe, stderr = out_pipe); wait = false)
    close(out_pipe.in)
    on_process(proc)

    # progress denominator: the full staged set when staged (the OIR main file alone understates the
    # data held in its `_000nn` companions), else the source file size
    src_size = stage_local ? _dir_bytes(stage_dir) : filesize(src_path)
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

    # bioformats2raw is done reading the source — drop the local stage copy now (on success OR failure)
    stage_local && rm(stage_dir; recursive = true, force = true)

    ok = proc.exitcode == 0 && proc.termsignal == 0
    ok || return nothing

    on_log("[INFO] Conversion complete.")

    # Read calibration metadata from the bioformats2raw (nested) output — the only layout
    # read_ome_metadata understands (CLAUDE.md → OME-ZARR dual-format).
    zarr_meta = read_ome_metadata(zarr_out)

    on_progress(1, 1)

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

    # Imaris sources: Bio-Formats hands over no timing whatsoever, so a timelapse arrives with no
    # interval. Recover it from the source file (see `_recover_ims_time_increment!`). Only when the
    # store genuinely has none — a real value from bioformats2raw always wins.
    if get(zarr_meta, "SizeT", 1) > 1 && get(zarr_meta, "TimeIncrement", 0.0) == 0.0
        _recover_ims_time_increment!(zarr_meta, src_path, task_run_dir(img._dir); on_log = on_log)
    end

    # Clipping at ACQUISITION, checked on every import. A channel the detector clipped has lost
    # information nothing downstream recovers, and import is the only point where the useful answer is
    # still "re-acquire with less gain". One streamed pass over the store we just wrote (~3 s/GB), in
    # the io pool alongside the conversion. Advisory: a failure here never fails the import.
    let run_dir     = task_run_dir(img._dir),
        result_file = joinpath(run_dir, "saturation.$(string(rand(UInt32); base = 16)).result.json")
        ok_s = run_py("tasks/importImages/saturation_run.py",
            (; imPath = zarr_out, resultPath = result_file), run_dir;
            on_log = on_log, on_progress = on_progress, on_process = on_process)
        if ok_s && isfile(result_file)
            try
                res   = JSON3.read(read(result_file, String))
                chans = get(res, :channels, nothing)
                if !isnothing(chans)
                    zarr_meta["saturation"] = Dict{String,Any}(
                        "channels" => [Dict{String,Any}(String(k) => v for (k, v) in ch) for ch in chans],
                    )
                    n = count(ch -> get(ch, :saturated, false) === true, chans)
                    n > 0 && on_log("[WARN] $n channel(s) clipped at acquisition — see QC")
                end
            catch e
                @warn "Could not read saturation result" exception = e
            finally
                rm(result_file; force = true)
            end
        end
    end

    # Copy our import-time corrections back INTO the zarr's own calibration (`.zattrs` + OME-XML),
    # so napari renders the same numbers ccid.json / `img_physical_sizes` (analysis) will use —
    # otherwise the ImageJ Z-spacing fix and the per-plane DeltaT time interval live only in
    # ccid.json and the viewer keeps showing the raw spacing / "t = N". Only when something actually
    # diverges from what bioformats2raw wrote: a corrected Z, or a timelapse (the DeltaT fallback /
    # unit-less-t placeholder cases). The value stays flagged for human confirmation regardless —
    # this just keeps the viewer honest about the number we've already decided to compute with.
    if haskey(zarr_meta, "PhysicalSizeZ_raw") || get(zarr_meta, "SizeT", 1) > 1
        sync_zarr_calibration!(zarr_out, zarr_meta)
    end

    _update_image_status!(img, "done")
    _merge_zarr_meta_into_ccid!(img, zarr_meta;
                                zarr_filename = basename(zarr_out),
                                value_name    = value_name,
                                on_log        = on_log)
    # bank calibration QC (missing/untrustworthy physical sizes) — the single source the image-table
    # indicator, whiteboard, lab log and MCP all read (replaces the frontend's own re-derivation).
    write_metadata_qc!(img)

    merge(zarr_meta, Dict{String,Any}(
        "valueName" => value_name,
        "filename"  => basename(zarr_out),
    ))
end
