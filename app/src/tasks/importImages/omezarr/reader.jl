# ── OME-ZARR metadata reader ──────────────────────────────────────────────────

"""
Fallback for the time interval when there's no top-level NGFF `t`-axis scale: many OME-XML
sources (including ones bioformats2raw converts) carry no single `TimeIncrement` on `Pixels`,
only per-`Plane` `DeltaT` — the elapsed seconds since T=0 for each frame. Scrapes
bioformats2raw's `OME/METADATA.ome.xml` sidecar with a plain regex (ports the same idea as the old
R `cciaImage.R::omeXMLTimelapseInfo` crutch; no XML dependency, see `image.jl` header note).
Returns the **median** of successive frame intervals across every `TheZ="0"` plane, so a single
warm-up gap or paused frame can't skew the recorded rate. Falls back to `nothing` if the file/tag
isn't there or there aren't enough planes to form an interval.

Uses the median rather than a single sample: a Leica LIF from a working microscope showed the
first interval at 34.69 s (a scanner warm-up) while every other interval clustered at 30.26 s. The
old logic sampled `TheT="1"` alone and would have baked in the outlier.
"""
function _delta_t_fallback(zarr_path::String)::Union{Float64,Nothing}
    # The OME/METADATA.ome.xml sidecar lives at the STORE ROOT, not at the series subdir the
    # multiscales sit in. bioformats2raw writes `<store>/OME/METADATA.ome.xml` and puts the
    # multiscales in `<store>/0/` (or `<store>/N/` for a series pick), and callers here pass either
    # the store root (import task) or the series subdir (`img_filepath` → `series_base`). Probe the
    # given path first (flat-store case) and fall back to the parent (bf2raw wrapper case); missing
    # this second lookup meant every bf2raw import silently skipped the fallback.
    xml_file = joinpath(zarr_path, "OME", "METADATA.ome.xml")
    if !isfile(xml_file)
        xml_file = joinpath(dirname(zarr_path), "OME", "METADATA.ome.xml")
        isfile(xml_file) || return nothing
    end
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
        return median(diffs)
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

