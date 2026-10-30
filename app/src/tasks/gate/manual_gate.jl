struct ManualGate <: CciaTask end

# A manual chain step (docs/SCHEDULER.md → Manual steps): the run parks here per image so the user can
# gate it, because nothing automatic can. PREPARE copies the exemplar's gates onto an image that has
# none, so the user adjusts rather than starts over; the CHECK holds the run until the populations the
# rest of the chain reads exist. Design: docs/todo/CHAIN_FROM_HISTORY_PLAN.md, Part A.

"""Population paths from the `requiredPops` text (or a list): trimmed, leading `/` ensured."""
function _required_pops(raw)::Vector{String}
    items = raw isa AbstractVector ? string.(raw) : split(string(something(raw, "")), ',')
    paths = String[]
    for s in items
        p = strip(s)
        isempty(p) && continue
        push!(paths, startswith(p, "/") ? String(p) : "/" * p)
    end
    paths
end

function _run_task(::ManualGate, img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    vn   = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    pt   = string(get(params, "popType", "flow"))
    from = string(something(get(params, "copyFrom", ""), ""))
    is_gating_pop_type(pt) || error("not a gating pop type: $pt (flow or track)")
    # QC-EXEMPT: this step produces nothing to score — the user's gates are the result, and the
    # chain's check (`manual_check` below) is what holds a run that lacks them.
    if isempty(from) || from == img.uid
        on_log("[INFO] No exemplar to copy from — gate this image by hand")
        return Dict{String,Any}("copied" => false)
    end
    src = init_object(img_project_uid(img), from)
    src isa CciaImage || error("copyFrom '$from' is not an image")
    r = copy_gating!(src, CciaImage[img]; value_name = vn, pop_type = pt, replace = false)
    if img.uid in r.copied
        on_log("[INFO] Copied $pt gates on '$vn' from $(src.name)")
        for w in get(r.warnings, img.uid, [])
            on_log("[WARN] Column '$(w.column)' missing here — empty: $(join(w.pops, ", "))")
        end
    else
        on_log("[INFO] Gates not copied: $(get(r.skipped, img.uid, "unknown reason"))")
    end
    Dict{String,Any}("copied" => img.uid in r.copied)
end

function manual_check(::ManualGate, img::CciaImage, params::Dict{String,Any})::Union{String,Nothing}
    required = _required_pops(get(params, "requiredPops", ""))
    isempty(required) && return nothing
    vn = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    pt = string(get(params, "popType", "flow"))
    m  = load_pop_map(img; value_name = vn, pop_type = pt, backfill_save = false)
    absent = filter(p -> !has_pop(m, p), required)
    isempty(absent) ? nothing : "no $(join(absent, ", ")) on '$vn'"
end
