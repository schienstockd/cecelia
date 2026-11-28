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
#          an output the image already has (`run_replaces`: an image version only while
#          keepPreviousVersion is off; a name outside the image only when the run log made it)

# The image field a name in namespace `ns` lives in ("" = image versions). `nothing` = not checkable here.
function _preflight_fields(ns::String)
    ns == ""       ? (:filepath,) :
    ns == "labels" ? (:labels, :label_props) : nothing
end

_preflight_has(img, name, ns) =
    (fs = _preflight_fields(ns); fs === nothing || any(f -> img_has_value_name(img, name; field = f), fs))

# The params a run of `task` will execute with: `resolve_run_params`, as `run_task` prepares every run,
# plus — for a composite, whose own spec declares no params — each step's spec defaults (first step wins
# a shared key). The steps run through `_run_task` directly, so a name left out falls back to the
# handler's own default, which is the step spec's default; this names it before the run does.
function _will_run_params(task::CciaTask, params::AbstractDict, scope)::Dict{String,Any}
    p = resolve_run_params(task, params, scope)
    task isa CompositeTask || return p
    for sub in _composite_steps(task), (k, v) in resolve_params_unscoped(sub, p)
        haskey(p, k) || (p[k] = v)
    end
    p
end

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
            # what the step overwrites: `run_replaces`, the answer the run log stamps at open — every
            # namespace (regions, clusters, HMM columns are found through the run log, not the image),
            # write-intos, and keepPreviousVersion (image versions are kept; a label set never is)
            # …asked with the params the step will RUN with (`resolve_run_params`, as `run_task` prepares
            # every run): an output name the node leaves out is the spec default it will write
            r = try run_replaces(n.fn, img, _will_run_params(task, n.params, img)) catch; nothing end
            r === nothing || add!(uid, n, "warn", "replaces the existing '$(r["name"])'")
        end
    end
    problems
end

# ── One run, before it is submitted: what it writes, and what that replaces ─────────────────────

"""
    run_write_check(fun_name, imgs, params) -> Dict

What a run of `fun_name` with `params` WOULD write on `imgs`, asked before it is submitted — for an agent
starting work in a GUI user's project (`POST /api/tasks/write-check`, docs/todo/AGENT_GUI_RUNS_PLAN.md
D3). Params resolve as `run_task` resolves them (`resolve_run_params`), so an omitted name is the
default name the run will write. It lives with the chain pre-flight: the
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
    task = try _task_from_fun_name(fn) catch; nothing end
    task === nothing && return out
    p = _will_run_params(task, params, imgs)
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
