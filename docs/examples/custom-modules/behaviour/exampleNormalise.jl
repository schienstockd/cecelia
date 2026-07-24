# Example custom (user drop-in) task — Julia-only, no Python.
#
# Copy this whole `custom-modules/` tree into your per-user config dir so the layout becomes:
#   <config_dir>/modules/behaviour/exampleNormalise.jl        ← this file
#   <config_dir>/modules/behaviour/exampleNormalise.json
# (config_dir = ~/.cecelia for an installed app; your dev dir otherwise — see docs/CUSTOM_MODULES.md).
#
# It min-max normalises one per-cell measure column to 0..1 and writes it back as a new obs column
# `<column><suffix>`. Category is `behaviour`, so it appears in the Behaviour module page's task list
# automatically — no frontend change, no rebuild.
#
# The file is `include`d INTO the Cecelia module at load time, so we define the struct + method with
# the `Cecelia.` prefix and finish by calling `Cecelia.register_task!`.

struct ExampleNormalise <: Cecelia.CciaTask end

function Cecelia._run_task(::ExampleNormalise, img::Cecelia.CciaImage, params::Dict{String,Any};
                           on_log::Function      = line -> println(line),
                           on_progress::Function = (n, t) -> nothing,
                           on_process::Function  = _ -> nothing)
    value_name = string(get(params, "valueName", Cecelia.VERSIONED_DEFAULT_VAL))
    column     = string(get(params, "column", ""))
    suffix     = string(get(params, "suffix", ".norm"))
    isempty(column) && (on_log("[ERROR] No column given"); return nothing)

    # Read a label-keyed DataFrame through the sanctioned reader — never touch the .h5ad directly.
    # (A production task would push the column selection into the view; kept simple here.)
    path = Cecelia.img_label_props_path(img, value_name)
    isfile(path) || (on_log("[ERROR] No label props for valueName='$value_name'"); return nothing)
    df = Cecelia.as_df(Cecelia.label_props(path))
    column in names(df) || (on_log("[ERROR] Column '$column' not found"); return nothing)

    on_progress(0, 1)
    x  = Float64.(df[!, column])
    lo, hi = extrema(x)
    out_col = column * suffix
    norm = hi > lo ? (x .- lo) ./ (hi - lo) : zeros(length(x))

    # Write the derived column back — label-aligned, via the writer chain.
    out = Cecelia.DataFrames.DataFrame(:label => df.label, Symbol(out_col) => norm)
    Cecelia.label_props(path) |> v -> Cecelia.add_obs(v, out) |> Cecelia.save!
    on_progress(1, 1)
    on_log("[INFO] Wrote $out_col ($(length(norm)) cells, range $lo..$hi)")

    Dict{String,Any}("column" => column, "outputColumn" => out_col, "n" => length(norm))
end

Cecelia.register_task!("behaviour.exampleNormalise", ExampleNormalise();
                       spec = joinpath(@__DIR__, "exampleNormalise.json"))
