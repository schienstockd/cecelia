struct TestImageTask <: CciaTask end

function _run_task(::TestImageTask, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    msg     = get(params, "message", "imageTask ran")
    wait_ms = get(params, "waitMs",  0)
    on_log("$msg (image=$(img.name))")
    wait_ms > 0 && sleep(wait_ms / 1000)
    on_progress(1, 1)
    Dict{String,Any}("ok" => true, "image" => img.name)
end
