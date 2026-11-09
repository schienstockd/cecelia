# Frame interval from the SOURCE, for a store that arrives with none (Imaris: Bio-Formats hands over no
# timing). Two tiers below the store's own value, in order: what the source file measured
# (`_recover_ims_time_increment!`), then what the data's publisher declared at registration
# (`_apply_declared_time_increment!`, Immunemap — import only). `_fill_source_time_increment!` is the
# one entry point, shared by import and the resync repair path.

"""
    _fill_source_time_increment!(zarr_meta, img_meta, src_path, run_dir; on_log) -> Bool

Fill `TimeIncrement` for a timelapse whose store has none: the source file's measured interval, else
the declared one — unless the file's timestamps exist and are irregular, in which case there is no
single interval and none is invented. A no-op for a single timepoint or a store that already has one.
`recover` is the file tier, injectable so the decision is tested without a real `.ims`.

`declared = false` on the resync path: a declared interval applied at import was written into the
store (`sync_zarr_calibration!`), so resync reads it back from there. Resync must not apply one fresh:
by then the download is usually deleted, so an irregular file can no longer veto it.
"""
function _fill_source_time_increment!(zarr_meta::Dict{String,Any}, img_meta::AbstractDict,
                                      src_path::AbstractString, run_dir::AbstractString;
                                      on_log::Function = _ -> nothing,
                                      recover::Function = _recover_ims_time_increment!,
                                      declared::Bool = true)::Bool
    needs() = get(zarr_meta, "SizeT", 1) > 1 && get(zarr_meta, "TimeIncrement", 0.0) == 0.0
    needs() || return false
    irregular = Ref(false)
    isempty(src_path) ||
        recover(zarr_meta, src_path, run_dir; on_log, irregular) && return true
    if irregular[]
        haskey(img_meta, "declaredTimeIncrement") &&
            on_log("[WARN] Declared frame interval not applied: the file's own timestamps are irregular")
        return false
    end
    declared && _apply_declared_time_increment!(zarr_meta, img_meta; on_log)
end

"""
    _apply_declared_time_increment!(zarr_meta, img_meta; on_log) -> Bool

Last-resort frame interval: one DECLARED at registration by whoever published the data, for a source
that carries no timing of its own (Imaris files often have none). `img_meta["declaredTimeIncrement"]`
is `{"value", "unit", "source"}` — set today only by Import from Immunemap, from the acquisition's
timestamps or frame rate. Applies only when nothing was read from the store or the file, so a measured
interval always wins. Records the source as `TimeIncrementDeclared` (cleared on re-import with the other
derived keys) so the image says where its interval came from. Pure — no IO.
"""
function _apply_declared_time_increment!(zarr_meta::Dict{String,Any}, img_meta::AbstractDict;
                                         on_log::Function = _ -> nothing)::Bool
    d = get(img_meta, "declaredTimeIncrement", nothing)
    d isa AbstractDict || return false
    v = get(d, "value", nothing)
    (v isa Real && v > 0) || return false
    unit = string(get(d, "unit", "second"))
    zarr_meta["TimeIncrement"]         = Float64(v)
    zarr_meta["TimeIncrementUnit"]     = unit
    zarr_meta["TimeIncrementDeclared"] = string(get(d, "source", "declared at registration"))
    on_log("[INFO] Frame interval $(Float64(v)) $unit from $(zarr_meta["TimeIncrementDeclared"]) — " *
           "the file has none")
    true
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
                                      on_log::Function = _ -> nothing,
                                      irregular::Ref{Bool} = Ref(false))::Bool
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
        # Measured but not uniform (a paused acquisition): the file's own answer is "no single
        # interval", which a declared one must not then override.
        irregular[] = get(res, :uniform, nothing) === false
        on_log("[WARN] No frame interval in the Imaris source: $(get(res, :reason, "unknown"))")
    catch e
        @warn "Could not read Imaris time interval" src_path exception = e
    finally
        rm(result_file; force = true)
    end
    false
end
