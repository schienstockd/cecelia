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
#   5. wire      A → B when a param of B names A's output IN A's NAMESPACE (B5), reads a population
#                A derives (`producesPop`: tracking's `…/_tracked`) or a column A writes
#                (`producesColumns`: HMM's `live.cell.hmm.state.<colName>`); then drop the edges
#                another path already implies (segment → gate → tracking needs no segment → tracking)
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
    columns::Vector{Tuple{String,String}}       # (label set, column) it writes (`producesColumns`)
    pops::Vector{Tuple{String,String,String}}   # (label set, population, marker) it derives (`producesPop`)
end

HistoryStep(id, fn, params, at, image_uids, output, output_ns) =
    HistoryStep(id, fn, params, at, image_uids, output, output_ns, Tuple{String,String}[],
                Tuple{String,String,String}[])

# A population picker value that means the whole segmentation, not a gated population.
_whole_pop(v::AbstractString) = is_root(v) || v == "NONE"

# ── spec reading ──

# What a param says about the names it holds. `ns` is the namespace a value lives in — the output
# namespace for a name the task WRITES (`namespace`), the field it READS from (`field`: labels,
# spatialGraphs, …; image versions are ""), or "models" for a model picker (`optionsFrom` a vault).
# `former` = the param's `formerKeys` (a renamed param), which a run saved before the rename still uses.
# `columns` = the param's `producesColumns` templates (`{}` = its value), `yields` its `producesPop`
# marker: what a task writes that no namespace names, so a reader can still be wired to it.
const _HistoryLeaf = @NamedTuple{type::String, ns::String, writes::Bool, accepts::Vector{String},
                                 former::Vector{String}, columns::Vector{String}, yields::String}

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
        # Columns add up: HMM's states step and its transitions step both write under `colName`.
        cols = String[string(c) for c in something(get(p, "producesColumns", nothing), [])]
        if haskey(out, k) && isempty(ns_w)
            isempty(cols) || (out[k] = merge(out[k], (; columns = unique!(vcat(out[k].columns, cols)))))
            continue
        end
        out[k] = (; type = t, ns, writes = !isempty(ns_w),
                    accepts = String[string(a) for a in something(get(p, "accepts", nothing), [])],
                    former = String[string(f) for f in something(get(p, "formerKeys", nothing), [])],
                    columns = cols, yields = string(something(get(p, "producesPop", nothing), "")))
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

# A run's saved `params` keyed by current key: a renamed param (`formerKeys`, from `_history_leaves`)
# saved under a former spelling moves to its current key — which spelling wins is `_stored_key`'s
# precedence — and the former spellings are dropped. Comparing runs saved either side of a rename by
# this view keeps one param one param. Top level only; `_hp_flat` applies it to group entries too.
function _history_current_params(params::AbstractDict, leaves)::Dict{String,Any}
    out = Dict{String,Any}(string(k) => v for (k, v) in params)
    leaves === nothing && return out
    for (k, l) in leaves
        isempty(l.former) && continue
        sk = _stored_key(k, l.former, out)
        v = sk === nothing ? nothing : out[sk]
        foreach(f -> delete!(out, f), l.former)
        sk === nothing || (out[k] = v)
    end
    out
end

# (name, namespace) a run writes: a named output param, else the spec's fixed `outputValueName`
# (an image version).
function _history_output(fn::String, params::Dict{String,Any}, leaves)::Tuple{String,String}
    name = task_output_name(fn, params)
    isempty(name) && return (task_fixed_output(fn), "")
    for (k, l) in leaves
        l.writes || continue
        # `task_output_name` resolves a renamed param through its former keys — match the same way
        kk = something(_stored_key(k, l.former, params), k)
        string(get(params, kk, "")) == name && return (name, l.ns)
    end
    (name, "")
end

# The label sets a run's population pickers name (`Tcells/qc/OTI/_tracked` → `Tcells`).
function _history_pop_sets(params::AbstractDict, lv)::Vector{String}
    own = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    sets = String[]
    for (k, s) in _keyed_strings(params)
        l = get(lv, k, nothing)
        (l !== nothing && l.type == "popSelection" && !_whole_pop(s)) || continue
        push!(sets, first(_split_pop_ref(s, own)))
    end
    sort!(unique!(sets))
end

# The cell-table columns a run writes (`producesColumns`, `{}` = the param's value) — into each label
# set its population pickers name ("" = none named) — and the populations it derives (`producesPop`:
# tracking `/qc/OTI` on `Tcells` → (`Tcells`, `/qc/OTI`, `_tracked`); the whole segmentation → path
# ""). Neither is an output name, so neither goes through `_history_output`.
function _history_products(params::Dict{String,Any}, lv)
    params = _history_current_params(params, lv)
    cols = Tuple{String,String}[]
    pops = Tuple{String,String,String}[]
    own = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    sets = _history_pop_sets(params, lv)
    for (k, l) in lv
        v = get(params, k, nothing)
        v isa AbstractString || continue
        for c in l.columns, ls in (isempty(sets) ? [""] : sets)
            push!(cols, (ls, replace(c, "{}" => v)))
        end
        if !isempty(l.yields)
            ls, path = _whole_pop(v) ? (own, "") : _split_pop_ref(v, own)
            push!(pops, (ls, rstrip(path, '/'), l.yields))
        end
    end
    sort!(unique!(cols)), unique!(pops)
end

# What a run READS, for keying a task that names no output: its input names and populations.
# By current key, so runs saved either side of a picker's rename (`formerKeys`) are one step.
function _history_inputs(params::Dict{String,Any}, leaves)::String
    params = _history_current_params(params, leaves)
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

# Does a run with `b_params` read what step `a` made — anything with `output`, `output_ns`, `columns`
# and `pops` fields? Its named output (`_reads_output`), a column `a` wrote into a label set `b` reads
# (a column picker holds the full name; `b`'s label sets are its population pickers'), or a population
# `a` derives: a picker naming `Tcells/qc/OTI/_tracked` (or anything under it, or under a sub-gate of
# `/qc/OTI`) reads the tracking that ran on `/qc/OTI` of `Tcells`. THE edge rule — the chain's and the
# board scope's (`_history_scope`).
function _reads_made(b_params, b_leaves, a)::Bool
    (!isempty(a.output) && _reads_output(b_params, b_leaves, a.output, a.output_ns)) && return true
    (isempty(a.columns) && isempty(a.pops)) && return false
    own = string(get(b_params, "valueName", VERSIONED_DEFAULT_VAL))
    b_sets = isempty(a.columns) ? String[] : _history_pop_sets(b_params, b_leaves)
    for (k, s) in _keyed_strings(b_params)
        l = get(b_leaves, k, nothing)
        (l === nothing || l.writes) && continue
        any(c -> c[2] == s && (isempty(c[1]) || isempty(b_sets) || c[1] in b_sets), a.columns) && return true
        l.type == "popSelection" || continue
        ls, path = _split_pop_ref(s, own)
        segs = split(path, '/'; keepempty = false)
        for (als, apath, marker) in a.pops
            als == ls || continue
            i = findfirst(==(marker), segs)
            i === nothing && continue
            base = "/" * join(segs[1:i-1], "/")
            (isempty(apath) || base == apath || startswith(base, apath * "/")) && return true
        end
    end
    false
end

# Drop an edge another path already implies: segment → gate → tracking needs no segment → tracking.
# The chain runs the same either way; the canvas then shows the order the work happened in.
function _transitive_reduce(edges::Set{Tuple{String,String}})::Set{Tuple{String,String}}
    succ = Dict{String,Vector{String}}()
    for (f, t) in edges
        push!(get!(Vector{String}, succ, f), t)
    end
    function reaches(from, to, skip)
        seen = Set{String}()
        stack = [n for n in get(succ, from, String[]) if n != skip]
        while !isempty(stack)
            n = pop!(stack)
            n == to && return true
            n in seen && continue
            push!(seen, n)
            append!(stack, get(succ, n, String[]))
        end
        false
    end
    Set(e for e in edges if !reaches(e[1], e[2], e[2]))
end

# ── collection ──

function _history_rows(proj::CciaProject, exemplars::Vector{String})
    rows = NamedTuple{(:fn, :params, :at, :uid, :legacy, :taskId),
                      Tuple{String,Dict{String,Any},String,String,Bool,String}}[]
    want = Set(exemplars)
    for uid in exemplars
        img = init_object(proj.uid, uid)
        img isa CciaImage || continue
        for e in read_run_log(img)
            r = run_log_row(e)
            r.status == "done" || continue
            push!(rows, (; r.fn, r.params, r.at, uid, r.legacy, r.taskId))
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
            # with the overrides the run recorded — what the step ran with, not just the template's values
            params = _apply_overrides(Dict{String,Any}(node.params), node.id,
                                      something(run.overrides, Dict{String,Any}()))
            push!(rows, (; fn = node.fn, params, at, uid, legacy = false, taskId = ""))   # a chain record holds no task id
        end
    end
    rows
end

# Steps 2–3: identity (task + output name, else what it reads) → per-image latest done run. Returns
# `(latest, held, skipped)`: `latest` replays; `held` ran fine but must not replay (an import, per-image
# geometry) — kept so the parameter table can still report it; `skipped` = `[(row, why)]` in time
# order, every run that is not in `latest` (held ones included), for the dialog's "Left out" list.
function _history_latest(rows, leaves_of)
    latest = Dict{Tuple{String,String},Dict{String,Any}}()
    held   = Dict{Tuple{String,String},Dict{String,Any}}()
    skipped = Tuple{Any,String}[]
    for r in sort(rows; by = r -> r.at)
        lv = leaves_of(r.fn)
        lv === nothing && (push!(skipped, (r, "this task no longer exists")); continue)
        why = startswith(r.fn, "importImages.") ? "imports an image — never replayed" :
              r.legacy ? "ran before params were recorded" :
              any(l -> l.type == "imagePicker", values(lv)) ? "per-image geometry (e.g. a crop box)" : ""
        isempty(why) || push!(skipped, (r, why))
        r.legacy && continue
        out, _ = _history_output(r.fn, r.params, lv)
        # a task that writes columns, not a named output, is one step per column name: an HMM sweep
        # (movement_2 … movement_6) is five versions, not one where the newest wins
        cols, _ = isempty(out) ? _history_products(r.params, lv) : (Tuple{String,String}[], nothing)
        key = !isempty(out) ? "out:" * out :
              !isempty(cols) ? "cols:" * join(("$ls:$c" for (ls, c) in cols), "&") :
              "in:" * _history_inputs(r.params, lv)
        get!(Dict{String,Any}, isempty(why) ? latest : held, (r.fn, key))[r.uid] = r
    end
    latest, held, skipped
end

"""
    chain_from_history(proj, exemplars; keep = nothing, retrain = false, name = "From history")
        -> Dict

Suggest a chain template from what ran on `exemplars`. `keep` names the final-output step ids to
build towards (default: every final output); `retrain = true` keeps training steps instead of reusing
their models. Returns, all plain JSON:

- `template`    — `{name, nodes: [{id, fn, scope, params}], edges: [{from, to}], startTargets}` —
                  the start dot on the chain's roots
- `finalOutputs`— `[{id, fn, output, detail, multi, at, imageUids, kept, siblingRead, inputs, inputsKey}]`: what the user
                  may tick. `detail` tells one version of a step from another (its output name, else
                  its column name, else the populations it read); `multi` = keeping several versions
                  is a real fork (it writes a label set, or runs per population), else the versions
                  are alternatives — keep one; `siblingRead` = another version of this task feeds a
                  later step, so this one was a try the exemplars moved past; `inputs` = the
                  populations it read (display), `inputsKey` = what it read (`_history_inputs`, the
                  same identity `siblingRead` uses) — alternatives are versions on the same inputs
- `conflicts`   — `[{nodeId, param, values: {uid => value}}]`: exemplars disagree; the template holds
                  the latest value until the user picks
- `notReplayed` — `[{fn, imageUid, why}]`; `reusedModels` — `[{fn, model}]`; `gateSteps` — node ids
- `valid`, `error` — whether the template passes `validate_chain_template` (e.g. a reused model that
                  is not in this install's vault fails here, by name)
"""
function chain_from_history(proj::CciaProject, exemplars::Vector{String};
                            keep::Union{Nothing,AbstractVector} = nothing, retrain::Bool = false,
                            name::AbstractString = "From history")::Dict{String,Any}
    leaves = Dict{String,Any}()
    leaves_of(fn) = get!(() -> _history_leaves(fn), leaves, fn)
    latest, _, skipped = _history_latest(_history_rows(proj, exemplars), leaves_of)
    not_replayed = [Dict{String,Any}("fn" => r.fn, "imageUid" => r.uid, "why" => why) for (r, why) in skipped]
    unique!(d -> (d["fn"], d["imageUid"], d["why"]), not_replayed)

    # merge across exemplars → steps; conflicts where their latest params differ
    steps     = HistoryStep[]
    conflicts = Dict{String,Any}[]
    for ((fn, _), by_img) in sort!(collect(latest); by = kv -> maximum(r.at for r in values(kv[2])))
        last = argmax(r -> r.at, collect(values(by_img)))
        out, ns = _history_output(fn, last.params, leaves_of(fn))
        id = "$(fn).$(length(steps) + 1)"
        # by current key: a run saved before a param's rename (`formerKeys`) is the same param — the
        # template node too, so a conflict's `param` names a key the node holds
        lv = leaves_of(fn)
        cols, pops = _history_products(last.params, lv)
        push!(steps, HistoryStep(id, fn, deepcopy(_history_current_params(last.params, lv)), last.at,
                                 sort!(collect(keys(by_img))), out, ns, cols, pops))
        length(by_img) > 1 || continue
        cur = Dict(u => _history_current_params(r.params, lv) for (u, r) in by_img)
        for k in sort!(unique(vcat((collect(keys(p)) for p in values(cur))...)))
            vals = Dict(u => get(p, k, nothing) for (u, p) in cur)
            length(unique(JSON3.write.(values(vals)))) > 1 &&
                push!(conflicts, Dict{String,Any}("nodeId" => id, "param" => k, "values" => vals))
        end
    end

    # wiring: B reads what A made (A ran first; "default" is the raw image, never a step's output)
    edges = Set{Tuple{String,String}}()
    for a in steps, b in steps
        (a === b || a.at > b.at || a.output == VERSIONED_DEFAULT_VAL) && continue
        _reads_made(b.params, leaves_of(b.fn), a) && push!(edges, (a.id, b.id))
    end

    # final outputs = steps nothing else reads; keep those asked for and everything upstream. A final
    # whose task ran on the SAME inputs in a version something did read (`siblingRead`) is a sweep the
    # exemplars moved past: cluster read HMM movement_3, so movement_2/4/5/6 on those tracks are left
    # out by default — an HMM on other populations is not a sibling
    finals = [s for s in steps if !any(e -> e[1] == s.id, edges)]
    sibling(s) = (s.fn, _history_inputs(s.params, leaves_of(s.fn)))
    read_sweeps = Set(sibling(s) for s in steps if any(e -> e[1] == s.id, edges))
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
    edges = _transitive_reduce(edges)

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
                                            "imageUids" => s.image_uids, "kept" => s.id in kept_finals,
                                            "siblingRead" => sibling(s) in read_sweeps,
                                            "inputs" => _history_pops_read(s.params, leaves_of(s.fn)),
                                            "inputsKey" => last(sibling(s)))
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
# is a population a task produces, not a gate, so that reference is skipped. `all_pickers = true` takes
# every population picker, track pickers included (`run_gates`: a track analysis on `B/qc/_tracked`
# reads the flow gate `/qc` too); the caller then resolves which sidecar each path lives in.
_history_pop_refs(s::HistoryStep, lv) = gated_pop_refs(s.params, lv)

function gated_pop_refs(params::AbstractDict, lv; all_pickers::Bool = false)::Vector{Tuple{String,String}}
    refs = Tuple{String,String}[]
    own = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    for (k, str) in _keyed_strings(params)
        l = get(lv, k, nothing)
        (l !== nothing && l.type == "popSelection" &&
         (all_pickers || isempty(l.accepts) || "live" in l.accepts)) || continue
        _whole_pop(str) && continue
        ls, path = _split_pop_ref(str, own)
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

# A run's params with label set `from` renamed to `to`: a population picker's `from/…` prefix, and a
# label-set name picker (`labels` namespace) holding exactly `from`. Walks the keys as `_keyed_strings`
# does, so a nested group is matched by its own keys. A key the task's spec doesn't declare is left alone.
function rename_label_set_in_params(v, lv, from::String, to::String, key::String = "")
    if v isa AbstractString
        l = get(lv, key, nothing)
        l === nothing && return v
        l.type == "popSelection" && startswith(v, from * "/") && return to * v[ncodeunits(from)+1:end]
        (l.ns == "labels" && v == from) && return to
        return v
    elseif v isa AbstractDict
        return Dict{String,Any}(string(k) => rename_label_set_in_params(x, lv, from, to, string(k)) for (k, x) in v)
    elseif v isa AbstractVector
        return Any[rename_label_set_in_params(x, lv, from, to, key) for x in v]
    end
    v
end

# What tells this version of a step from its siblings: the name it writes, else what it read (the
# populations — tracking `/qc/frags` vs `/qc/cells`), else "" (one version only).
function _history_detail(s::HistoryStep, lv)::String
    isempty(s.output) || return s.output
    for (k, l) in lv     # an HMM version is told apart by its `colName`, not its long column names
        isempty(l.columns) || return string(get(s.params, k, ""))
    end
    _history_pops_read(s.params, lv)
end

# The populations a run's pickers name, in key order — `/qc/frags`, or HMM's three `…/_tracked`.
function _history_pops_read(params::AbstractDict, lv)::String
    vals = String[]
    for (k, v) in sort!(collect(params); by = first)
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
    (isempty(s.output) && isempty(s.columns) && any(l -> l.type == "popSelection" && !l.writes, values(lv)) &&
     !isempty(_history_detail(s, lv)))
