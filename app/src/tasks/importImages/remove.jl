struct RemoveImage <: CciaTask end

function _run_task(task::RemoveImage, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    value_name  = string(get(params, "valueName",  VERSIONED_DEFAULT_VAL))
    new_default = string(get(params, "newDefault",  VERSIONED_DEFAULT_VAL))

    # Shared removal core (storage.jl) — the one deletion path, also used by the storage-reclaim API.
    # Its safe-primary rule only "un-imports" (clears channel names/dims, status=pending) when the
    # primary is removed AND no other version remains, so removing `default` while a corrected variant
    # is still active keeps the image working.
    res = remove_image_version!(img, value_name, new_default; on_log = on_log)
    isnothing(res) && return nothing            # no such version → task failure (propagates in chains)
    freed, cleared = res
    on_progress(1, 1)

    Dict{String,Any}(
        "removedValue" => value_name,
        "newDefault"   => new_default,
        "cleared"      => cleared,
        "freedBytes"   => freed,
    )
end
