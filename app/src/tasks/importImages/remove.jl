struct RemoveImage <: CciaTask end

Base.@kwdef struct RemoveImageParams
    valueName::String  = VERSIONED_DEFAULT_VAL
    newDefault::String = VERSIONED_DEFAULT_VAL
end

function parse_remove_image_params(d::AbstractDict)::RemoveImageParams
    RemoveImageParams(;
        valueName  = string(get(d, "valueName", VERSIONED_DEFAULT_VAL)),
        newDefault = string(get(d, "newDefault", VERSIONED_DEFAULT_VAL)))
end

# QC-EXEMPT: this deletes a registered version. There is no output to score, and the image's own
# QC is recomputed by whatever next writes it — the sanctioned "genuinely no objective signal" case.
function _run_task(task::RemoveImage, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    p = parse_remove_image_params(params)

    # Shared removal core (storage.jl) — the one deletion path, also used by the storage-reclaim API.
    # Its safe-primary rule only "un-imports" (clears channel names/dims, status=pending) when the
    # primary is removed AND no other version remains, so removing `default` while a corrected variant
    # is still active keeps the image working.
    res = remove_image_version!(img, p.valueName, p.newDefault; on_log = on_log)
    isnothing(res) && return nothing            # no such version → task failure (propagates in chains)
    freed, cleared = res
    on_progress(1, 1)

    Dict{String,Any}(
        "removedValue" => p.valueName,
        "newDefault"   => p.newDefault,
        "cleared"      => cleared,
        "freedBytes"   => freed,
    )
end
