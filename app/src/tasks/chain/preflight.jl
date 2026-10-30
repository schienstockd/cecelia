# ── Pre-flight: what a chain run will hit on each image, before it starts ──────
#
# Run 29 images through a chain built on three, and the first sign that image 17 has no `SHG` channel is
# a failed node an hour in. This reads the template against each target image up front and names, per
# image and step, what is missing. Advisory: the run is still the user's call (fault isolation keeps a
# bad image from stopping the others). Design: docs/todo/CHAIN_FROM_HISTORY_PLAN.md → Pre-flight checks.
#
#   fail — the step cannot work here: a channel the image lacks, an input that is neither on the image
#          nor made earlier in the chain
#   warn — it will run, but not as you may expect: skipped (missing axis / pixel size), or it replaces
#          an output the image already has

# The image field a name in namespace `ns` lives in ("" = image versions). `nothing` = not checkable here.
function _preflight_fields(ns::String)
    ns == ""       ? (:filepath,) :
    ns == "labels" ? (:labels, :label_props) : nothing
end

_preflight_has(img, name, ns) =
    (fs = _preflight_fields(ns); fs === nothing || any(f -> img_has_value_name(img, name; field = f), fs))

# Names a node's ancestors produce in this chain — an input that will exist by the time the node runs.
function _preflight_produced(t::ChainTemplate, node_id::String)::Set{String}
    out = Set{String}()
    for n in t.nodes
        n.id in _ancestors(t, node_id) || continue
        for name in (task_output_name(n.fn, n.params), task_fixed_output(n.fn))
            isempty(name) || push!(out, name)
        end
    end
    out
end

"""
    chain_preflight(proj, template, image_uids) -> Vector{Dict}

`[{imageUid, nodeId, fn, level: "fail" | "warn", message}]` — empty when nothing stands out.
"""
function chain_preflight(proj::CciaProject, t::ChainTemplate, image_uids::Vector{String})
    problems = Dict{String,Any}[]
    add!(uid, n, level, msg) = push!(problems, Dict{String,Any}(
        "imageUid" => uid, "nodeId" => n.id, "fn" => n.fn, "level" => level, "message" => msg))
    produced = Dict(n.id => _preflight_produced(t, n.id) for n in t.nodes)
    for uid in image_uids
        img = try init_object(proj.uid, uid) catch; nothing end
        img isa CciaImage || continue
        for n in t.nodes
            task = try _task_from_fun_name(n.fn) catch; nothing end
            task === nothing && continue
            if !task_applies(task, img)
                add!(uid, n, "warn", "skipped: $(task_applicability_reason(task, img))")
                continue
            end
            leaves = something(_history_leaves(n.fn), Dict{String,_HistoryLeaf}())
            vn = string(get(n.params, "valueName", VERSIONED_DEFAULT_VAL))
            for (k, v) in n.params
                l = get(leaves, k, nothing)
                l === nothing && continue
                if l.type == "valueNameSelection" && !l.writes && v isa AbstractString && !isempty(v)
                    (v in produced[n.id] || _preflight_has(img, v, l.ns)) ||
                        add!(uid, n, "fail", "no '$v' on this image, and no earlier step makes it")
                elseif l.type == "channelSelection" && !isnothing(v) && !(v isa AbstractVector && isempty(v))
                    names = something(channel_names(img; value_name = vn in produced[n.id] ? nothing : vn), String[])
                    try
                        channel_indices(v, names; what = k)
                    catch e
                        add!(uid, n, "fail", sprint(showerror, e))
                    end
                end
            end
            out, ns = _history_output(n.fn, n.params, leaves)
            if !isempty(out) && out != VERSIONED_DEFAULT_VAL && _preflight_fields(ns) !== nothing &&
               _preflight_has(img, out, ns) && !keep_previous_version()
                add!(uid, n, "warn", "replaces the existing '$out'")
            end
        end
    end
    problems
end
