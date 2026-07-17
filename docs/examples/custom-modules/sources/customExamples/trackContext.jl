# Example custom module — Julia + Python + NESTED parameters.
#
# Computes a per-cell "track context" measure across BOTH languages and writes BOTH results back to
# the .h5ad, to show the full Julia → Python → h5ad round-trip in one task:
#   • Julia  — `<measure>.trackMean`          : for each cell, the mean of `measure` over its whole track.
#   • Python — `<measure>.trackMean.<method>` : a standardised (z-score / robust) version of that,
#              handy for comparing across images. (Contrived — its only job is to exercise Python.)
#
# Category `customExamples` has no built-in page, so this also exercises the generic
# `/custom/:category` page — a "Custom" group appears in the sidebar. Copy this whole `custom-modules/`
# tree into your config dir's `modules/` (see docs/CUSTOM_MODULES.md), giving:
#   <config_dir>/modules/sources/customExamples/trackContext.jl              ← this file
#   <config_dir>/modules/inputDefinitions/customExamples/trackContext.json
#   <config_dir>/modules/python/customExamples/trackContext_run.py
#
# The file is `include`d INTO the Cecelia module, so names resolve with the `Cecelia.` prefix.

using Statistics: mean

# safe mean over finite values (empty / all-NaN → NaN)
_trackctx_mean(x) = (v = filter(isfinite, Float64.(collect(x))); isempty(v) ? NaN : mean(v))

struct TrackContext <: Cecelia.CciaTask end

function Cecelia._run_task(::TrackContext, img::Cecelia.CciaImage, params::Dict{String,Any};
                           on_log::Function      = line -> println(line),
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function  = _ -> nothing)
    vn      = string(get(params, "valueName", Cecelia.VERSIONED_DEFAULT_VAL))
    measure = string(get(params, "measure", "live.cell.speed"))
    # `section` params are flattened to the top level by the scheduler before _run_task runs.
    minlen  = Int(get(params, "minTrackLength", 1))
    method  = string(get(params, "method", "zscore"))

    path = Cecelia.img_label_props_path(img, vn)
    isfile(path) || (on_log("[ERROR] No label props for valueName='$vn'"); return nothing)

    # ── Julia: per-track mean of `measure`, broadcast back to each cell ────────────
    on_progress(0, 2)
    cell = Cecelia.label_props(img; value_name = vn) |>
           Cecelia.select_cols(["track_id", measure]) |> Cecelia.as_df
    "track_id" in Cecelia.DataFrames.names(cell) ||
        (on_log("[ERROR] Segmentation '$vn' is not tracked (no track_id)"); return nothing)
    measure in Cecelia.DataFrames.names(cell) ||
        (on_log("[ERROR] Measure '$measure' not found"); return nothing)

    keep = [r isa Number && isfinite(r) && r > 0 for r in cell[!, "track_id"]]
    cell = cell[keep, :]
    cell[!, :track_id] = Int.(cell[!, "track_id"])

    g   = Cecelia.DataFrames.groupby(cell, :track_id)
    agg = Cecelia.DataFrames.combine(g, measure => _trackctx_mean => :tm,
                                        Cecelia.DataFrames.nrow => :n)
    agg = agg[agg[!, :n] .>= minlen, :]
    merged = Cecelia.DataFrames.leftjoin(
        Cecelia.DataFrames.select(cell, [:label, :track_id]), agg, on = :track_id)

    jcol = measure * ".trackMean"
    out  = Cecelia.DataFrames.DataFrame(:label => merged[!, :label],
                                        Symbol(jcol) => coalesce.(merged[!, :tm], NaN))
    Cecelia.label_props(path) |> Cecelia.add_obs(out) |> Cecelia.save!
    on_log("[INFO] Julia wrote $jcol for $(Cecelia.DataFrames.nrow(out)) cells")
    on_progress(1, 2)

    # ── Python: standardise the Julia column, write that back too ──────────────────
    pcol = jcol
    if method != "none"
        pcol   = "$jcol.$method"
        script = joinpath(Cecelia.custom_modules_dir(), "python", "customExamples", "trackContext_run.py")
        ok = Cecelia.run_py(script,
                (; labelPropsPath = path, inColumn = jcol, outColumn = pcol, method = method),
                Cecelia.task_run_dir(img._dir);
                on_log = on_log, on_progress = on_progress, on_process = on_process)
        ok || (on_log("[ERROR] Python standardisation failed"); return nothing)
    end
    on_progress(2, 2)

    Dict{String,Any}("juliaColumn" => jcol, "pythonColumn" => pcol, "method" => method)
end

Cecelia.register_task!("customExamples.trackContext", TrackContext();
                       spec = joinpath(@__DIR__, "..", "..",
                                       "inputDefinitions", "customExamples", "trackContext.json"))
