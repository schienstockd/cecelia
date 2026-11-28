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
#          an output the image already has (an image version only while keepPreviousVersion is off)

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
            # keepPreviousVersion mints a new version only for image versions (ns ""); a label set is
            # always overwritten (its writers never version), so it warns whatever the toggle says
            if !isempty(out) && out != VERSIONED_DEFAULT_VAL && _preflight_fields(ns) !== nothing &&
               _preflight_has(img, out, ns) && (ns != "" || !keep_previous_version())
                add!(uid, n, "warn", "replaces the existing '$out'")
            end
        end
    end
    problems
end

# ── One run, before it is submitted: what it writes, and what that replaces ─────────────────────

"""
    run_write_check(fun_name, imgs, params) -> Dict

What a run of `fun_name` with `params` WOULD write on `imgs`, asked before it is submitted — for an agent
starting work in a GUI user's project (`POST /api/tasks/write-check`, docs/todo/AGENT_GUI_RUNS_PLAN.md
D3). Omitted params take the spec default, as the run would. It lives with the chain pre-flight: the
same question — what a run will hit, before it starts — for one run, from the run log's `run_replaces`. Keys, each absent when there is nothing:

  output   — `{name, ns, into, key?}`: what the run writes (`_run_output_identity`) and the param that
             names it. Absent = the run names no output.
  replaces — `{imageUid => run_replaces(…)}` for each image where that output already exists.
  free     — when something is replaced and a param names the output (not a write-into): the first of
             `<name>_2`, `<name>_3`, … that replaces nothing on any of `imgs` (`free_label_name`'s naming).
"""
function run_write_check(fun_name::AbstractString, imgs::AbstractVector, params::AbstractDict)
    fn = String(fun_name)
    out = Dict{String,Any}()
    leaves = _history_leaves(fn)
    leaves === nothing && return out
    p = merge(_history_param_defaults(fn), Dict{String,Any}(String(k) => v for (k, v) in params))
    name, ns, into = _run_output_identity(fn, p, leaves)
    isempty(name) && return out
    key = into ? task_writes_into_key(fn) : _run_output_key(p, leaves, name)
    o = Dict{String,Any}("name" => name, "ns" => ns, "into" => into)
    isempty(key) || (o["key"] = key)
    out["output"] = o
    reps = Dict{String,Any}()
    for img in imgs
        r = run_replaces(fn, img, p)
        r === nothing || (reps[img.uid] = r)
    end
    isempty(reps) && return out
    out["replaces"] = reps
    (into || isempty(key)) && return out
    for i in 2:99
        q = merge(p, Dict{String,Any}(key => "$(name)_$(i)"))
        all(img -> run_replaces(fn, img, q) === nothing, imgs) || continue
        out["free"] = "$(name)_$(i)"
        break
    end
    out
end

# The param that names output `name`: a writing leaf holding it (a renamed one under its saved key, as
# `_history_output` matches), else the legacy `outputValueName`. "" = a fixed output no param names.
function _run_output_key(params::AbstractDict, leaves, name::AbstractString)::String
    for (k, l) in leaves
        l.writes || continue
        kk = something(_stored_key(k, l.former, params), k)
        string(get(params, kk, "")) == name && return kk
    end
    string(get(params, "outputValueName", "")) == name ? "outputValueName" : ""
end
