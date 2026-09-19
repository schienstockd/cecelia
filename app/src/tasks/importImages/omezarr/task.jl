struct ImportOmezarr <: CciaTask end

# Typed shape of what `_run_task(::ImportOmezarr, …)` reads from `params`. Every "auto" is a
# sentinel that resolves to either a Settings default (ngffVersion) or a reader-aware default at
# runtime (maxWorkers / jvmHeapGiB). Kept as raw strings so the sentinel survives to the resolver.
Base.@kwdef struct ImportOmezarrParams
    valueName::String    = VERSIONED_DEFAULT_VAL
    src_path::String     = ""
    pyramidLevels::Any   = nothing   # nothing → falls back to `pyramidScale` in the parser
    pyramidScale::Int    = 2
    stageLocal::Bool     = false
    chunkSize::Any       = "auto"
    ngffVersion::Any     = nothing   # nothing → Settings default (store_layout().ngffVersion)
    shardSize::Any       = "auto"
    shardDepth::Any      = "1"
    maxWorkers::String   = "auto"
    jvmHeapGiB::Any      = "auto"
end

function parse_import_omezarr_params(d::AbstractDict)::ImportOmezarrParams
    ImportOmezarrParams(;
        valueName     = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        src_path      = string(get(d, "src_path", "")),
        pyramidLevels = get(d, "pyramidLevels", nothing),
        pyramidScale  = Int(get(d, "pyramidScale", 2)),
        stageLocal    = Bool(get(d, "stageLocal", false)),
        chunkSize     = get(d, "chunkSize", "auto"),
        ngffVersion   = get(d, "ngffVersion", nothing),
        shardSize     = get(d, "shardSize", "auto"),
        shardDepth    = get(d, "shardDepth", "1"),
        maxWorkers    = string(get(d, "maxWorkers", "auto")),
        jvmHeapGiB    = get(d, "jvmHeapGiB", "auto"))
end

# Where the next import writes, and whether it's a version-append or an overwrite.
#
# Rules (see `docs/audit/vn-versioning-p4-design.md`):
#   - Fresh import (no `img.filepath[value_name]`): flat `img_zero_dir/ccidImage.ome.zarr`, `false`.
#     v1 always lives at the legacy path so a project imported with the toggle on is still readable
#     by anything that hasn't yet been P1-routed (belt-and-braces, given P2 infra's union type
#     already handles it in-model).
#   - `keep_previous_version()` false: same as fresh — overwrite semantics preserved.
#   - Re-import with toggle on: next `vN` at `img_zero_dir/{value_name}/vN/ccidImage.ome.zarr`,
#     `true`. `_merge_zarr_meta_into_ccid!` routes the ccid.json write through
#     `versioned_upgrade_entry!` + `version_write!` at the end of the run.
function _plan_import_target(img::CciaImage, value_name::AbstractString)
    flat = joinpath(img_zero_dir(img), "ccidImage.ome.zarr")
    prior = get(img.filepath, String(value_name), nothing)
    (isnothing(prior) || !keep_previous_version()) && return (flat, false)
    # `version_next` needs the versioned inner dict; wrap the legacy scalar in memory (the ccid.json
    # write path re-does this via `versioned_upgrade_entry!` on the raw dict — this copy is
    # ephemeral, for the path calculation only).
    inner = prior isa AbstractDict ?
                prior :
                Dict{String,Any}(LATEST_DEFAULT_VAL => prior, LATEST_ACTIVE_KEY => LATEST_DEFAULT_VAL)
    next_v = version_next(inner)
    (joinpath(img_zero_dir(img), String(value_name), next_v, "ccidImage.ome.zarr"), true)
end

function _run_task(task::ImportOmezarr, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_import_omezarr_params(params)
    value_name = p.valueName

    # Source path: explicit param > stored ori_path in metadata
    src_path = isempty(p.src_path) ? string(get(img.meta, "ori_path", "")) : p.src_path

    if isempty(src_path) || !isfile(src_path)
        on_log("[ERROR] Source file not found: $(isempty(src_path) ? "(no src_path)" : src_path)")
        return nothing
    end

    # Two write modes — decided ONCE per run:
    #   - flat (default): `img_zero_dir/ccidImage.ome.zarr`, overwrite in place, ccid.json records
    #     legacy scalar. Backward-compatible with every existing project.
    #   - versioned: re-import with `keep_previous_version()`=true → next `vN` at
    #     `img_zero_dir/{value_name}/vN/ccidImage.ome.zarr` alongside the prior version. Ccid.json
    #     entry gets upgraded to versioned shape on the write. First-ever import stays flat regardless
    #     of the toggle — v1 always lives at the legacy path, only v2+ nest.
    # `_plan_import_target` returns (zarr_out, as_new_version); `as_new_version` threads through to
    # `_merge_zarr_meta_into_ccid!` at the end of the run.
    zarr_out, as_new_version = _plan_import_target(img, value_name)
    pyramid_levels = isnothing(p.pyramidLevels) ? p.pyramidScale : Int(p.pyramidLevels)

    # Multi-series source (LIF, CZI, …): the register step (or the series picker) recorded which
    # series the user chose in `meta.ori_series`. We ask bioformats2raw to convert ONLY that series
    # (`--series N`) instead of every series in the file — a 4-series LIF where only the last carries
    # the timelapse used to convert all four (wasted disk + minutes of wall clock) and then be pinned
    # to series 0 downstream via `series_base`. `bf2raw` preserves the source series index in the
    # output subdir name (so `--series 3` writes `<zarr>/3/…`), which is why `_resolve_series_subdir!`
    # below rewrites `filepath` to point at that subdir — the flat-store branch of `series_base` then
    # picks it up transparently.
    series_choice = get(img.meta, "ori_series", nothing)
    series_flags  = isnothing(series_choice) ? `` : `--series $(Int(series_choice))`

    bf2raw = bioformats2raw_bin()
    if !isfile(bf2raw)
        on_log("[ERROR] bioformats2raw not found at $bf2raw")
        on_log("[ERROR] Set dirs.bioformats2raw in $(custom_toml_path())")
        return nothing
    end

    # clean any previous outputs (the final store, a leftover stage dir).
    # Versioned target: `zarr_out` is a fresh vN subdir; anything there is crashed-run debris, safe to
    # clean. Also ensure the parent dir exists — bioformats2raw creates its own leaf, not the
    # `default/vN/` intermediate.
    stage_dir = joinpath(img_zero_dir(img), "_stage_src")
    for d in unique([zarr_out, stage_dir])
        if isdir(d)
            on_log("[INFO] Removing previous output: $d")
            rm(d; recursive = true)
        end
    end
    as_new_version && mkpath(dirname(zarr_out))

    # Stage the source locally first when reading from a slow/network location (SMB): copies the whole
    # companion set to local scratch, then bioformats2raw reads at disk speed. Deleted right after the
    # conversion's source read finishes (independent of the 16-bit transient).
    stage_local = p.stageLocal
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
    chunk_flags = bf2raw_chunk_flags(p.chunkSize)
    on_log("[INFO] Chunk size: $(isempty(chunk_flags) ? "auto (1024, capped to the frame)" : chunk_flags[2])")

    # Store FORMAT — chosen here and only here; every derived store inherits it (ZARR_V3_PLAN D9).
    # `z_planes` lets "all z" resolve to a real depth; the source is not converted yet, so it comes from
    # ccid meta when a previous import recorded it, else 0 (which drops the flag rather than guessing).
    # Unset params fall back to the Settings DEFAULTS, not to hardcoded literals — Settings is where the
    # store-layout default lives and the import form pre-fills from it (ZARR_V3_PLAN D10). A run
    # launched headlessly (REPL, chain) therefore gets the same layout as one launched from the form.
    ngff_version = isnothing(p.ngffVersion) ? store_layout().ngffVersion : p.ngffVersion
    fmt_flags = bf2raw_format_flags(ngff_version, p.shardSize;
        shard_depth = p.shardDepth,
        z_planes    = something(meta_int(img.meta, "SizeZ"), 0))
    on_log("[INFO] Format: $(isempty(fmt_flags) ? "NGFF 0.4 (zarr v2), nested keys" : join(fmt_flags, " "))")

    # Worker/heap controls — key defaults on the SOURCE extension so Imaris (`.ims`) gets the safe
    # floor without the user having to know why. Measured 2026-08-27 on `Human_Lymph_Node_Manual_IBEX.ims`:
    # bioformats2raw's `--max-workers=4` + JVM-default heap = 105 OOMs, 0-3 chunks written before the
    # task terminated (`H5tiledLayoutBB\$DataChunk.getByteBuffer` → `Deflate.decode`). At workers=2,
    # -Xmx16g: 2 OOMs and 3820 chunks / 1.5 GB written. At workers=1: OOMs go to 0. That's why the
    # auto-default for `.ims` is 1, not 2. See `bf2raw_worker_flags` / `bf2raw_default_workers`.
    worker_choice = p.maxWorkers
    if lowercase(strip(worker_choice)) == "auto"
        worker_choice = bf2raw_default_workers(eff_src)
    end
    worker_flags = bf2raw_worker_flags(worker_choice)

    heap_gib    = bf2raw_java_heap_gib(p.jvmHeapGiB)
    if heap_gib == 0 && lowercase(strip(string(p.jvmHeapGiB))) == "auto"
        heap_gib = bf2raw_default_heap_gib(eff_src)
    end
    java_env = bf2raw_java_env(heap_gib)

    on_log("[INFO] Workers: $(isempty(worker_flags) ? "auto (bioformats2raw default: 4)" : worker_flags[1])")
    on_log("[INFO] JVM heap: $(heap_gib > 0 ? "-Xmx$(heap_gib)g" : "auto (JVM default)")")

    cmd = `$bf2raw --resolutions $pyramid_levels $compression $chunk_flags $fmt_flags $worker_flags $series_flags $eff_src $zarr_out`
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

    # With `--series N`, bioformats2raw writes `<zarr>/N/` and leaves `<zarr>/0/` absent, so both the
    # NGFF read below and every downstream `series_base` must be pointed at `<zarr>/N/` instead. When
    # no --series was passed, this returns "" (root has multiscales? no) or "0" (the bf2raw default),
    # so the plain-old single-series case is unchanged. See `bf2raw_series_subdir` above.
    series_subdir = bf2raw_series_subdir(zarr_out;
                                         prefer = isnothing(series_choice) ? nothing : Int(series_choice))
    # zarr path pointed at whichever subdir carries the multiscales — flat root, "0/", or "N/" for a
    # series-picked import. `store_rel` lands verbatim in `filepath["default"]`, so downstream
    # `img_filepath(img)` → `series_base(...)` resolves to this same directory (flat-store branch).
    resolved_zarr = isnothing(series_subdir) || isempty(series_subdir) ? zarr_out :
                    joinpath(zarr_out, series_subdir)
    # `store_rel` is the path RECORDED in ccid.json — relative to `img_zero_dir(img)`. Flat imports
    # keep the legacy basename (`ccidImage.ome.zarr`); versioned imports carry the vN subdir
    # (`{value_name}/vN/ccidImage.ome.zarr`) so a v1 sibling can coexist.
    store_rel_root = relpath(zarr_out, img_zero_dir(img))
    store_rel      = isnothing(series_subdir) || isempty(series_subdir) ? store_rel_root :
                     joinpath(store_rel_root, series_subdir)

    # Read calibration metadata from the bioformats2raw (nested) output — the only layout
    # read_ome_metadata understands (CLAUDE.md → OME-ZARR dual-format).
    zarr_meta = read_ome_metadata(resolved_zarr)

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
            (; imPath = resolved_zarr, resultPath = result_file), run_dir;
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
        # NGFF sync targets the SUBDIR that actually holds multiscales (`resolved_zarr`); OME-XML
        # lives at `<zarr_out>/OME/METADATA.ome.xml` regardless of series, so a series-picked import
        # can't reach it via this path (update_ome_xml_pixels walks `<resolved>/OME/…`). The browser
        # viewer + offline renderer read NGFF; the legacy napari OME-XML path is being retired.
        sync_zarr_calibration!(resolved_zarr, zarr_meta)
    end

    _update_image_status!(img, IMAGE_DONE)
    _merge_zarr_meta_into_ccid!(img, zarr_meta;
                                zarr_filename  = store_rel,
                                value_name     = value_name,
                                as_new_version = as_new_version,
                                on_log         = on_log)
    # bank calibration QC (missing/untrustworthy physical sizes) — the single source the image-table
    # indicator, whiteboard, lab log and MCP all read (replaces the frontend's own re-derivation).
    write_metadata_qc!(img)

    merge(zarr_meta, Dict{String,Any}(
        "valueName" => value_name,
        "filename"  => store_rel,
    ))
end
