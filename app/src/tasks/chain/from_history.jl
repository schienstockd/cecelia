# ── Chain from history ────────────────────────────────────────────────────────
#
# "I tried things on these images; do the rest the same way." Turns what actually ran on a few exemplar
# images into a chain template, mechanically — no LLM. Pure: reads run logs, chain run records and task
# specs, writes nothing; the result is reviewed and saved through the whiteboard's own path.
# Design + the two real projects it was checked against: docs/todo/CHAIN_FROM_HISTORY_PLAN.md, Part B.
#
#   1. collect   every DONE run on the exemplars — the run log, plus chain run records (they outlive
#                the run log's start and its 200-row cap)
#   2. drop      what must not replay: unknown tasks, per-image geometry (an `imagePicker` param), a run
#                logged before params were recorded
#   3. group     by identity (B4): task + output name; a task with no output name is keyed by what it
#                reads. Latest done run wins per image — exact for an output that overwrites
#   4. merge     across exemplars into one node; params that differ become CONFLICTS (B7), never a guess
#   5. wire      A → B when a param of B names A's output IN A's NAMESPACE (B5)
#   6. keep      the chosen final outputs (default: all) and everything upstream of them (B3)
#   7. models    a training step whose model exists is dropped; its consumers keep using the model (B9)
#   8. gates     a population read downstream that no step produces → a `gate.manual_gate` step (B6)

const _HISTORY_GATE_FN = "gate.manual_gate"

"""One task as it ran on the exemplars — a node of the suggested chain."""
struct HistoryStep
    id::String
    fn::String
    params::Dict{String,Any}
    at::String                    # latest run (ISO, sortable)
    image_uids::Vector{String}    # exemplars it ran on
    output::String                # the name it writes ("" = none)
    output_ns::String             # that name's namespace: "labels", "models", … ("" = an image version)
end

# ── spec reading ──

# What a param says about the names it holds. `ns` is the namespace a value lives in — the output
# namespace for a name the task WRITES (`namespace`), the field it READS from (`field`: labels,
# spatialGraphs, …; image versions are ""), or "models" for a model picker (`optionsFrom` a vault).
const _HistoryLeaf = @NamedTuple{type::String, ns::String, writes::Bool, accepts::Vector{String}}

# a label set is named the same whether a picker lists its labels or its measurements
_field_ns(field) = (f = string(something(field, ""));
                    f in ("", "filepath", "filepaths") ? "" : f == "labelPropsNames" ? "labels" : f)

function _spec_leaf_params!(out::Dict{String,_HistoryLeaf}, spec)
    walk(ps) = for p in ps
        p isa AbstractDict || continue
        t = string(get(p, "type", ""))
        t in ("section", "group") && (walk(get(p, "params", [])); continue)
        k = string(get(p, "key", ""))
        isempty(k) && continue
        ns_w = string(something(get(p, "namespace", nothing), ""))
        from = string(something(get(p, "optionsFrom", nothing), ""))
        ns   = !isempty(ns_w) ? ns_w : endswith(lowercase(from), "models") ? "models" : _field_ns(get(p, "field", nothing))
        # a composite's steps share keys: measureLabels READS `outputValueName`, the label set its segment
        # step WROTE under that key; track_measures re-reads `valueName`, which the tracking step already
        # took as its input. The first step to declare a key says what it is, unless a later one writes it.
        (haskey(out, k) && isempty(ns_w)) && continue
        out[k] = (; type = t, ns, writes = !isempty(ns_w),
                    accepts = String[string(a) for a in something(get(p, "accepts", nothing), [])])
    end
    isnothing(spec) || walk(get(spec, "params", []))
    out
end

# Leaves of a task, a composite's steps included (its own spec declares none).
function _history_leaves(fn::String)::Union{Dict{String,_HistoryLeaf},Nothing}
    task = try _task_from_fun_name(fn) catch; nothing end
    task === nothing && return nothing
    out = Dict{String,_HistoryLeaf}()
    task isa CompositeTask && foreach(sub -> _spec_leaf_params!(out, _task_spec(sub)), _composite_steps(task))
    _spec_leaf_params!(out, _task_spec(task))
end

# (name, namespace) a run writes: a named output param, else the spec's fixed `outputValueName`
# (an image version).
function _history_output(fn::String, params::Dict{String,Any}, leaves)::Tuple{String,String}
    name = task_output_name(fn, params)
    isempty(name) && return (task_fixed_output(fn), "")
    for (k, l) in leaves
        l.writes && string(get(params, k, "")) == name && return (name, l.ns)
    end
    (name, "")
end

# What a run READS, for keying a task that names no output: its input names and populations.
function _history_inputs(params::Dict{String,Any}, leaves)::String
    parts = String[]
    for k in sort!(collect(keys(params)))
        l = get(leaves, k, nothing)
        (l === nothing || l.writes || !(l.type in ("valueNameSelection", "popSelection"))) && continue
        push!(parts, "$k=$(JSON3.write(params[k]))")
    end
    join(parts, "&")
end

# Every (param key, string) pair inside a run's params, nested groups included (cellpose `models.1.model`).
function _keyed_strings(params::AbstractDict, key::String = "")::Vector{Tuple{String,String}}
    out = Tuple{String,String}[]
    for (k, v) in params
        kk = string(k)
        if v isa AbstractString
            push!(out, (kk, String(v)))
        elseif v isa AbstractDict
            append!(out, _keyed_strings(v, kk))
        elseif v isa AbstractVector
            for x in v
                x isa AbstractString && push!(out, (kk, String(x)))
                x isa AbstractDict && append!(out, _keyed_strings(x, kk))
            end
        end
    end
    out
end

# Does run `b` read the output `out` (namespace `ns`)? Only through a param that READS that namespace:
# a name picker on that field, a model picker (`flow.small` ↔ `flow.small.pt`), or a population picker
# naming the label set (`B/qc/_tracked` reads labels `B`).
function _reads_output(b_params, b_leaves, out::String, ns::String)::Bool
    for (k, s) in _keyed_strings(b_params)
        l = get(b_leaves, k, nothing)
        (l === nothing || l.writes) && continue
        if l.type == "popSelection"
            ns == "labels" && startswith(s, out * "/") && return true
        elseif l.ns == ns
            (s == out || (ns == "models" && s == out * ".pt")) && return true
        end
    end
    false
end

# ── collection ──

const _HISTORY_DONE = ("done", "")

function _history_rows(proj::CciaProject, exemplars::Vector{String})
    rows = NamedTuple{(:fn, :params, :at, :uid, :legacy),
                      Tuple{String,Dict{String,Any},String,String,Bool}}[]
    want = Set(exemplars)
    for uid in exemplars
        img = init_object(proj.uid, uid)
        img isa CciaImage || continue
        for e in read_run_log(img)
            g(k) = (v = get(e, k, get(e, Symbol(k), nothing)); v === nothing ? "" : string(v))
            g("status") in _HISTORY_DONE || continue
            raw = get(e, "params", get(e, :params, nothing))
            p = raw isa AbstractDict ? Dict{String,Any}(string(k) => v for (k, v) in raw) : Dict{String,Any}()
            legacy = isempty(p)                       # logged before the run log recorded params
            vn = g("valueName")                       # the run log keeps valueName beside params
            isempty(vn) || (p["valueName"] = vn)
            push!(rows, (; fn = g("fun"), params = p, at = g("at"), uid, legacy))
        end
    end
    # chain run records: a run's frozen template + per-image status, from before the run log existed
    runs = _runs_dir(proj)
    for rid in (isdir(runs) ? readdir(runs) : String[])
        run = try load_chain_run(proj, rid) catch; continue end
        at = Dates.format(Dates.unix2datetime(run.created_at), "yyyy-mm-ddTHH:MM:SS")
        for uid in run.image_uids, node in run.template_snapshot.nodes
            uid in want || continue
            st = get(run.image_states[uid], node.id, nothing)
            (st === nothing || st.status != NODE_DONE) && continue
            push!(rows, (; fn = node.fn, params = Dict{String,Any}(node.params), at, uid, legacy = false))
        end
    end
    rows
end

"""
    chain_from_history(proj, exemplars; keep = nothing, retrain = false, name = "From history")
        -> Dict

Suggest a chain template from what ran on `exemplars`. `keep` names the final-output step ids to
build towards (default: every final output); `retrain = true` keeps training steps instead of reusing
their models. Returns, all plain JSON:

- `template`    — `{name, nodes: [{id, fn, scope, params}], edges: [{from, to}], startTargets}` —
                  the start dot on the chain's roots
- `finalOutputs`— `[{id, fn, output, detail, multi, at, imageUids, kept}]`: what the user may tick.
                  `detail` tells one version of a step from another (its output name, else the
                  populations it read); `multi` = keeping several versions is a real fork (it writes
                  a label set, or runs per population), else the versions are alternatives — keep one
- `conflicts`   — `[{nodeId, param, values: {uid => value}}]`: exemplars disagree; the template holds
                  the latest value until the user picks
- `notReplayed` — `[{fn, imageUid, why}]`; `reusedModels` — `[{fn, model}]`; `gateSteps` — node ids
- `valid`, `error` — whether the template passes `validate_chain_template` (e.g. a reused model that
                  is not in this install's vault fails here, by name)
"""
function chain_from_history(proj::CciaProject, exemplars::Vector{String};
                            keep::Union{Nothing,AbstractVector} = nothing, retrain::Bool = false,
                            name::AbstractString = "From history")::Dict{String,Any}
    rows   = _history_rows(proj, exemplars)
    leaves = Dict{String,Any}()
    leaves_of(fn) = get!(() -> _history_leaves(fn), leaves, fn)
    not_replayed = Dict{String,Any}[]
    skip!(r, why) = push!(not_replayed, Dict{String,Any}("fn" => r.fn, "imageUid" => r.uid, "why" => why))

    # identity → per-image latest run
    latest = Dict{Tuple{String,String},Dict{String,Any}}()
    for r in sort(rows; by = r -> r.at)
        lv = leaves_of(r.fn)
        lv === nothing && (skip!(r, "this task no longer exists"); continue)
        startswith(r.fn, "importImages.") && (skip!(r, "imports an image — never replayed"); continue)
        r.legacy && (skip!(r, "ran before params were recorded"); continue)
        any(l -> l.type == "imagePicker", values(lv)) &&
            (skip!(r, "per-image geometry (e.g. a crop box)"); continue)
        out, _ = _history_output(r.fn, r.params, lv)
        key = isempty(out) ? "in:" * _history_inputs(r.params, lv) : "out:" * out
        get!(Dict{String,Any}, latest, (r.fn, key))[r.uid] = r
    end
    unique!(d -> (d["fn"], d["imageUid"], d["why"]), not_replayed)

    # merge across exemplars → steps; conflicts where their latest params differ
    steps     = HistoryStep[]
    conflicts = Dict{String,Any}[]
    for ((fn, _), by_img) in sort!(collect(latest); by = kv -> maximum(r.at for r in values(kv[2])))
        last = argmax(r -> r.at, collect(values(by_img)))
        out, ns = _history_output(fn, last.params, leaves_of(fn))
        id = "$(fn).$(length(steps) + 1)"
        push!(steps, HistoryStep(id, fn, deepcopy(last.params), last.at, sort!(collect(keys(by_img))), out, ns))
        length(by_img) > 1 || continue
        for k in sort!(unique(vcat((collect(keys(r.params)) for r in values(by_img))...)))
            vals = Dict(u => get(r.params, k, nothing) for (u, r) in by_img)
            length(unique(JSON3.write.(values(vals)))) > 1 &&
                push!(conflicts, Dict{String,Any}("nodeId" => id, "param" => k, "values" => vals))
        end
    end

    # wiring: B reads A's output (A ran first; "default" is the raw image, never a step's output)
    edges = Set{Tuple{String,String}}()
    for a in steps, b in steps
        (a === b || isempty(a.output) || a.output == VERSIONED_DEFAULT_VAL || a.at > b.at) && continue
        _reads_output(b.params, leaves_of(b.fn), a.output, a.output_ns) && push!(edges, (a.id, b.id))
    end

    # final outputs = steps nothing else reads; keep those asked for and everything upstream
    finals = [s for s in steps if !any(e -> e[1] == s.id, edges)]
    kept_finals = Set(isnothing(keep) ? [s.id for s in finals] : String.(keep))
    keep_ids = Set{String}()
    stack = [s.id for s in finals if s.id in kept_finals]
    while !isempty(stack)
        id = pop!(stack)
        id in keep_ids && continue
        push!(keep_ids, id)
        append!(stack, [e[1] for e in edges if e[2] == id])
    end

    # training: reuse the model the exemplars already trained (B9)
    reused = Dict{String,Any}[]
    if !retrain
        for s in steps
            (s.id in keep_ids && s.output_ns == "models") || continue
            delete!(keep_ids, s.id)
            push!(reused, Dict{String,Any}("fn" => s.fn, "model" => s.output))
        end
    end
    kept  = [s for s in steps if s.id in keep_ids]
    edges = Set(e for e in edges if e[1] in keep_ids && e[2] in keep_ids)

    # gates: populations read but produced by no step → one manual gate step per label set
    gate_ids = String[]
    gate_steps = HistoryStep[]
    refs = Dict(s.id => _history_pop_refs(s, leaves_of(s.fn)) for s in kept)
    by_set = Dict{String,Set{String}}()
    for s in kept, (ls, path) in refs[s.id]
        push!(get!(Set{String}, by_set, ls), path)
    end
    for (ls, paths) in sort!(collect(by_set); by = first)
        gid = "gate.$(ls)"
        push!(gate_ids, gid)
        push!(gate_steps, HistoryStep(gid, _HISTORY_GATE_FN, Dict{String,Any}(
                "valueName" => ls, "popType" => "flow", "copyFrom" => first(exemplars),
                "requiredPops" => join(sort!(collect(paths)), ", ")), "", String[], "", ""))
        for s in kept
            s.output == ls && s.output_ns == "labels" && push!(edges, (s.id, gid))
            any(r -> r[1] == ls, refs[s.id]) && push!(edges, (gid, s.id))
        end
    end
    append!(kept, gate_steps)

    # the canonical node constructor (spec-inherited scope, default barrier policy + pool) and the
    # canonical serialiser, so the suggestion is exactly what the whiteboard would save
    template = ChainTemplate(String(name), [chain_node(s.fn; id = s.id, params = s.params) for s in kept],
                             [ChainEdge(f, t) for (f, t) in sort!(collect(edges))], String[])
    # the start dot on the roots, as the create/save routes repair it: the canvas draws the dot from this
    # list, and a suggestion may be shown before (or without) a save
    template = ChainTemplate(template.name, template.nodes, template.edges, chain_root_ids(template))
    tpl = JSON3.read(_template_json(template), Dict{String,Any})
    valid, err = try
        validate_chain_template(template); (true, "")
    catch e
        (false, e isa ChainTemplateError ? e.msg : sprint(showerror, e))
    end
    Dict{String,Any}(
        "template"     => tpl,
        "finalOutputs" => [Dict{String,Any}("id" => s.id, "fn" => s.fn, "output" => s.output,
                                            "detail" => _history_detail(s, leaves_of(s.fn)),
                                            "multi" => _history_forks(s, leaves_of(s.fn)), "at" => s.at,
                                            "imageUids" => s.image_uids, "kept" => s.id in kept_finals)
                           for s in finals],
        "conflicts"    => [c for c in conflicts if c["nodeId"] in keep_ids],
        "notReplayed"  => not_replayed,
        "reusedModels" => reused,
        "gateSteps"    => gate_ids,
        "valid"        => valid,
        "error"        => err)
end

# (label set, population path) pairs a step reads through its population pickers — only pickers that
# take gated cell populations (`accepts` has "live", or none declared). A bare path (`/qc/frags`) is on
# the step's own `valueName`; `B/qc/_tracked` names its label set. System segments (`_tracked`, …) are
# cut: what must exist is the gated population above them. A segment naming a run (`clusters.immune`)
# is a population a task produces, not a gate, so that reference is skipped.
function _history_pop_refs(s::HistoryStep, lv)::Vector{Tuple{String,String}}
    refs = Tuple{String,String}[]
    own = string(get(s.params, "valueName", VERSIONED_DEFAULT_VAL))
    for (k, str) in _keyed_strings(s.params)
        l = get(lv, k, nothing)
        (l !== nothing && l.type == "popSelection" && (isempty(l.accepts) || "live" in l.accepts)) || continue
        str in ("", "NONE", "root", "/") && continue
        parts = split(str, '/'; limit = 2)
        ls, path = startswith(str, "/") ? (own, str) :
                   length(parts) == 2   ? (String(parts[1]), "/" * parts[2]) : (own, "/" * str)
        segs = String[]
        produced = false
        for seg in split(path, '/'; keepempty = false)
            startswith(seg, "_") && break
            occursin('.', seg) && (produced = true; break)
            push!(segs, String(seg))
        end
        (produced || isempty(segs)) || push!(refs, (ls, "/" * join(segs, "/")))
    end
    unique!(refs)
end

# What tells this version of a step from its siblings: the name it writes, else what it read (the
# populations — tracking `/qc/frags` vs `/qc/cells`), else "" (one version only).
function _history_detail(s::HistoryStep, lv)::String
    isempty(s.output) || return s.output
    vals = String[]
    for (k, v) in sort!(collect(s.params); by = first)
        l = get(lv, k, nothing)
        (l !== nothing && l.type == "popSelection") && append!(vals, [x for (_, x) in _keyed_strings(Dict(k => v))])
    end
    join(vals, ", ")
end

# Can several versions of this step sensibly run side by side? Yes when each writes its own label set
# (two segmentations), or runs per population (tracking frags and cells). Otherwise the versions are
# alternatives — two region clusterings, two stats runs — and the user keeps one.
_history_forks(s::HistoryStep, lv)::Bool =
    s.output_ns == "labels" ||
    (isempty(s.output) && any(l -> l.type == "popSelection" && !l.writes, values(lv)) &&
     !isempty(_history_detail(s, lv)))
