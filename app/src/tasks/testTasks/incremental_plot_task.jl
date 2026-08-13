struct IncrementalPlotTask <: CciaTask end

function _run_task(::IncrementalPlotTask, imgs::Vector{CciaImage}, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    label = get(params, "label", "plot")
    on_log("$label: $(length(imgs)) images in batch")
    on_progress(1, 1)
    Dict{String,Any}("ok" => true, "image_count" => length(imgs), "plot_type" => "histogram")
end
