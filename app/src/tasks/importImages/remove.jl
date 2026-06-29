struct RemoveImage <: CciaTask end

function _run_task(task::RemoveImage, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name  = string(get(params, "valueName",  VERSIONED_DEFAULT_VAL))
    new_default = string(get(params, "newDefault",  VERSIONED_DEFAULT_VAL))

    ccid = joinpath(img._dir, "ccid.json")
    raw  = Dict{String,Any}(String(k) => v for (k, v) in JSON3.read(read(ccid, String)))

    filename = versioned_get_field(raw, "filepath", value_name)
    if isnothing(filename)
        on_log("[ERROR] No filepath registered for valueName='$value_name'")
        return nothing
    end

    # Search data dir first, then labels dir
    proj_dir   = dirname(dirname(img._dir))
    candidates = [
        joinpath(proj_dir, "0", img.uid, string(filename)),
        joinpath(proj_dir, "1", img.uid, "labels", string(filename)),
    ]
    target = findfirst(ispath, candidates)
    if !isnothing(target)
        on_log("[INFO] Removing: $(candidates[target])")
        rm(candidates[target]; recursive = true)
        on_log("[INFO] Done.")
    else
        on_log("[INFO] File '$filename' not found on disk — clearing metadata only.")
    end

    versioned_set_field!(raw, "filepath", nothing, value_name)
    fp = raw["filepath"]::Dict{String,Any}
    fp[VERSIONED_ACTIVE_KEY] = new_default

    is_primary = value_name == VERSIONED_DEFAULT_VAL
    if is_primary
        versioned_set_field!(raw, "imChannelNames", nothing, VERSIONED_DEFAULT_VAL)
        m = Dict{String,Any}(String(k) => v for (k, v) in get(raw, "meta", Dict()))
        for key in ("SizeC", "SizeT", "SizeZ"); delete!(m, key); end
        raw["meta"]   = m
        raw["status"] = "pending"
    end

    open(ccid, "w") do io; JSON3.write(io, raw); end
    on_progress(1, 1)

    Dict{String,Any}(
        "removedValue" => value_name,
        "newDefault"   => new_default,
        "cleared"      => is_primary,
    )
end
