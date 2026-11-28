# ── OME-ZARR metadata reader ──────────────────────────────────────────────────

"""
    _ome_xml_file(zarr_path) -> Union{String,Nothing}

The store's `OME/METADATA.ome.xml` sidecar, or `nothing`. It lives at the STORE ROOT, not at the
series subdir the multiscales sit in: bioformats2raw writes `<store>/OME/METADATA.ome.xml` and puts
the multiscales in `<store>/0/` (or `<store>/N/` for a series pick), and callers pass either — the
import's `resolved_zarr` and `img_filepath(img, "default")` are both the series subdir. Probes the
given path first (store root, or a flat store), then its parent; at each, `OME/METADATA.ome.xml`
then the legacy root-level `METADATA.ome.xml` — the same four candidates, in the same order, as
Python's `load_ome_xml`. The ONE lookup for reads and writes
(`_delta_t_fallback`, `update_ome_xml_pixels!`); Python's twin is `ome_xml_utils.load_ome_xml`.
Without the parent probe the writer silently skipped every bioformats2raw import and metadata edit.
"""
function _ome_xml_file(zarr_path::AbstractString)::Union{String,Nothing}
    path = rstrip(zarr_path, ['/', '\\'])
    for root in (path, dirname(path))
        # the legacy layout keeps the XML at the root with no `OME/` dir — Python reads it too
        for f in (joinpath(root, "OME", "METADATA.ome.xml"), joinpath(root, "METADATA.ome.xml"))
            isfile(f) && return f
        end
    end
    nothing
end

"""
Fallback for the time interval when there's no top-level NGFF `t`-axis scale: many OME-XML
sources (including ones bioformats2raw converts) carry no single `TimeIncrement` on `Pixels`,
only per-`Plane` `DeltaT` — the elapsed seconds since T=0 for each frame. Scrapes
bioformats2raw's `OME/METADATA.ome.xml` sidecar with a plain regex (ports the same idea as the old
R `cciaImage.R::omeXMLTimelapseInfo` crutch; no XML dependency, see `image.jl` header note).
Returns the interval from successive `TheZ="0"` planes by `_uniform_interval`: their median, as long
as the series really has one. `nothing` if the file/tag isn't there, there aren't enough planes, or
the gaps are irregular (a paused acquisition) — then the image says it has no interval rather than
carrying one that is wrong across the pause.

Uses the median rather than a single sample: a Leica LIF from a working microscope showed the
first interval at 34.69 s (a scanner warm-up) while every other interval clustered at 30.26 s. The
old logic sampled `TheT="1"` alone and would have baked in the outlier.
"""
# Fractional tolerance on a frame gap — the Julia twin of `ims_meta.UNIFORM_TOL` (Python): clock
# jitter passes, a pause does not.
const DELTA_T_UNIFORM_TOL = 0.05
# How much LONGER the first gap may run (scanner warm-up). The case that set it was +15%; a first gap
# beyond this is a pause, not a warm-up, and a shorter-than-normal one is never excused.
const DELTA_T_WARMUP_TOL = 0.25

"""
    _uniform_interval(diffs; tol = DELTA_T_UNIFORM_TOL) -> Union{Float64,Nothing}

The median of `diffs` when the series has a single frame interval, else `nothing`. The same rule as
the Imaris reader (`ims_meta.summarise_gaps`): an irregular series — a paused acquisition — has no
interval, and its median would make every speed across the pause wrong with no warning. One
allowance: with three or more gaps the FIRST may run up to `warmup` longer than the rest, because a
scanner warm-up lengthens it (the Leica case below, +15%) without the series being irregular. The
interval is the median of the later gaps. Pure.
"""
function _uniform_interval(diffs::AbstractVector{<:Real}; tol::Real = DELTA_T_UNIFORM_TOL,
                           warmup::Real = DELTA_T_WARMUP_TOL)
    isempty(diffs) && return nothing
    rest = length(diffs) > 2 ? diffs[2:end] : diffs
    med  = median(rest)
    med > 0 || return nothing
    all(g -> abs(g - med) / med <= tol, rest) || return nothing
    first_ok = length(diffs) <= 2 || -tol <= (diffs[1] - med) / med <= warmup
    first_ok ? Float64(med) : nothing
end

function _delta_t_fallback(zarr_path::String)::Union{Float64,Nothing}
    xml_file = _ome_xml_file(zarr_path)
    isnothing(xml_file) && return nothing
    try
        xml = read(xml_file, String)
        # Elapsed seconds since T=0, keyed by TheT. Match the <Plane …> opening tag whether
        # self-closing (`/>`, bioformats2raw) or not (`>…</Plane>`, some vendors) — the DeltaT
        # attribute is on the opening tag either way.
        deltas = Dict{Int,Float64}()
        for m in eachmatch(r"<Plane\b[^>]*?>", xml)
            tag = m.match
            occursin(r"TheZ=\"0\"", tag) || continue
            tm = match(r"TheT=\"(\d+)\"", tag)
            isnothing(tm) && continue
            dm = match(r"DeltaT=\"([-\d.eE+]+)\"", tag)
            isnothing(dm) && continue
            value = parse(Float64, dm.captures[1])
            um    = match(r"DeltaTUnit=\"([a-zA-Z]+)\"", tag)
            unit  = isnothing(um) ? "s" : lowercase(um.captures[1])
            secs  = unit == "ms" ? value / 1000 : (unit == "min" ? value * 60 : value)
            deltas[parse(Int, tm.captures[1])] = secs
        end
        isempty(deltas) && return nothing
        # OME's implicit anchor: DeltaT is elapsed time since the first plane, so TheT=0 → 0 s.
        # Add it explicitly so a file that only carries TheT>=1 still yields an interval (matches
        # the previous behaviour for the "single TheT=1" case).
        get!(deltas, 0, 0.0)
        ts = sort!(collect(keys(deltas)))
        length(ts) < 2 && return nothing
        diffs = Float64[deltas[ts[i+1]] - deltas[ts[i]] for i in 1:length(ts)-1]
        dt = _uniform_interval(diffs)
        isnothing(dt) && @warn "Per-plane DeltaT gaps are irregular — no single frame interval recorded" zarr_path gaps = extrema(diffs)
        return dt
    catch e
        @warn "Could not read OME-XML for DeltaT fallback" zarr_path exception = e
    end
    nothing
end


"""
Which subdir of a bf2raw output actually carries the multiscales, when the caller can't assume it is
`0/`. `bioformats2raw --series N` PRESERVES the source index in the output group name (`<zarr>/N/`),
so a series-picked import writes to `<zarr>/3/`, not `<zarr>/0/` — `series_base` alone would miss it
because it only looks under `0/`. Returns `""` when multiscales lives at the flat root (crop and
correction outputs), `"N"` when a numbered subdir holds them; `nothing` when neither does. If
`prefer` is given (the series index the caller asked bf2raw for), that subdir is checked first so a
same-name collision on `0/` never masks the requested pick.

Read-only; the caller decides what to do with the result (typically: store it as the relative
filepath for the next `series_base` to resolve through the flat-store branch).
"""
function bf2raw_series_subdir(zarr_out::AbstractString; prefer::Union{Int,Nothing} = nothing)::Union{String,Nothing}
    isdir(zarr_out) || return nothing
    # flat root wins when it carries multiscales — that's how derived stores (crop/correction) look
    root_ms = ngff_multiscales(zarr_out)
    (isnothing(root_ms) || isempty(root_ms)) || return ""
    candidates = String[]
    if !isnothing(prefer)
        push!(candidates, string(prefer))
    end
    for entry in readdir(zarr_out)
        entry in candidates && continue
        all(isdigit, entry) && push!(candidates, entry)
    end
    for name in candidates
        d = joinpath(zarr_out, name)
        isdir(d) || continue
        ms = ngff_multiscales(d)
        (isnothing(ms) || isempty(ms)) && continue
        return name
    end
    nothing
end

"""
The metadata files that make `dir` a zarr GROUP, in either format: `.zgroup` (+ `.zattrs`) for v2,
`zarr.json` for v3. Empty when `dir` is not a group. Paths only, nothing is parsed — for a caller that
has to copy a group's own metadata alongside one of its children.
"""
function zarr_group_meta_files(dir::AbstractString)::Vector{String}
    isfile(joinpath(dir, ".zgroup")) || isfile(joinpath(dir, "zarr.json")) || return String[]
    filter(isfile, [joinpath(dir, f) for f in (".zgroup", ".zattrs", "zarr.json")])
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
        # `.zattrs` is the file's author's text: a dataset path must stay inside the store (audit F15)
        path_inside(base, joinpath(base, level_path)) || (level_path = "0")

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
            # raw: a label can be present but blank (a .lif with unnamed channels). The import names
            # those from the acquisition settings (`_merge_channel_names!` → `unique_channel_names`).
            result["channel_names"] = [string(get(ch, :label, "")) for ch in channels]
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

                # ONE shared `PhysicalSizeUnit` (the first spatial axis with a unit). An axis in a
                # different unit (z in nm, x/y in µm) is converted into it, so every PhysicalSize*
                # is in that unit — `sync_zarr_calibration!` writes the shared unit onto all three
                # axes, and a z left in its own unit would be relabelled (1000× off, no error). A
                # unit with no known factor can't be converted: it keeps its own value and says so
                # in `PhysicalSize{X,Y,Z}Unit` (`_meta_axis_length_unit` reads it).
                spatial_unit = nothing
                for i in (xi, yi, zi)
                    if !isnothing(i) && !isnothing(ax_units[i])
                        spatial_unit = ax_units[i]
                        break
                    end
                end
                if !isnothing(spatial_unit)
                    result["PhysicalSizeUnit"] = spatial_unit
                    shared_um = length_unit_um(spatial_unit)
                    for (i, axis) in ((xi, "X"), (yi, "Y"), (zi, "Z"))
                        (isnothing(i) || isnothing(ax_units[i]) || ax_units[i] == spatial_unit) && continue
                        axis_um = length_unit_um(ax_units[i])
                        if isnothing(axis_um) || isnothing(shared_um)
                            result["PhysicalSize$(axis)Unit"] = ax_units[i]
                        else
                            result["PhysicalSize$(axis)"] = scale[i] * axis_um / shared_um
                        end
                    end
                end

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
