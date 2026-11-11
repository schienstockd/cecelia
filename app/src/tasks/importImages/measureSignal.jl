# Measure an already-imported image's signal for the acquisition check — the import's pixel probe
# (`meta["saturation"]`: clipping, photons per channel, shared structure) for an image imported before
# it, or before its current fields. The background path for many images at once (the acquisition
# check's "Measure N movies"): minutes per movie, too long for one HTTP request. A single image's
# Refresh metadata runs the same `ensure_saturation_meta!` inline. No drift: estimating it is the drift
# correction task's job, not a side effect of this button.
struct MeasureSignal <: CciaTask end

# No settings: it always probes the `default` store (an acquisition property, see ensure_saturation_meta!)
Base.@kwdef struct MeasureSignalParams end
function parse_measure_signal_params(::AbstractDict)::MeasureSignalParams
    MeasureSignalParams()
end

function _run_task(task::MeasureSignal, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    parse_measure_signal_params(params)
    wrote = ensure_saturation_meta!(img; on_log = on_log, on_progress = on_progress,
                                    on_process = on_process)
    # the result IS import QC (clipping, photon-limited, depth): bank it whether or not this run wrote,
    # so an image already current still shows its findings
    write_metadata_qc!(img)
    on_progress(1, 1)
    wrote || on_log("Already measured — nothing to do.")
    Dict{String,Any}("measured" => wrote)
end
