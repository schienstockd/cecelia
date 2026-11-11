# ── Parameter table from history ──────────────────────────────────────────────────────────────────
#
# The Methods-section companion to `chain_from_history`: the same steps, grouped the same way
# (`_history_latest`, task + output name, latest done run per image), written as one row per step with
# only what differs from the task spec's defaults. Where the images disagree the spread is reported
# with counts, never resolved — a table for a paper states what ran, not what a chain would pick.
#
# Unlike the chain, nothing is dropped: an import or a crop (never replayed) is a row with a note,
# a run logged before params were recorded is a row saying so, and a training step stays in.

"""Flat spec defaults of task `fn` — `key => default`; a repeatable group is `key => (params, entry)`."""
function _history_param_defaults(fn::String)::Dict{String,Any}
    task = try _task_from_fun_name(fn) catch; nothing end
    out = Dict{String,Any}()
    task === nothing && return out
    specs = task isa CompositeTask ? Any[_task_spec(s) for s in _composite_steps(task)] : Any[]
    push!(specs, _task_spec(task))
    for sp in specs
        sp === nothing || _hp_defaults!(out, get(sp, "params", []))
    end
    out
end

function _hp_defaults!(out, ps)
    for p in ps
        p isa AbstractDict || continue
        t = string(get(p, "type", "")); k = string(get(p, "key", ""))
        if t == "group" && get(p, "repeatable", false) == true
            child = Dict{String,Any}()
            _hp_defaults!(child, get(p, "params", []))
            out[k] = (; child, first = get(p, "default", nothing), table = get(p, "entryDefaults", nothing))
        elseif t in ("section", "group")
            _hp_defaults!(out, get(p, "params", []))   # children are stored under their own flat key
        elseif !isempty(k) && haskey(p, "default")
            out[k] = p["default"]
        end
    end
    out
end

# What a NEW entry at `pos` (0-based) of a repeatable group starts with — the rule of the form's
# `newEntryDefaults` (frontend/src/tasks/paramValues.ts): child defaults, the group default for the
# first entry, `entryDefaults[min(pos, end)]` for later ones.
function _hp_entry_defaults(g, pos::Int)::Dict{String,Any}
    d = Dict{String,Any}(g.child)
    seed = if pos == 0
        g.first isa AbstractDict && !isempty(g.first) ? first(values(g.first)) : nothing
    elseif g.table isa AbstractVector && !isempty(g.table)
        g.table[min(pos, length(g.table) - 1) + 1]
    else
        nothing
    end
    seed isa AbstractDict && for (k, v) in seed
        haskey(d, string(k)) && (d[string(k)] = v)
    end
    d
end

# A run's params as flat `key => (value, default)`; a repeatable group's entries become `key[i].child`.
# A key the spec gives no default for carries `missing` (always reported when set). A renamed param
# saved under a former key (`leaves`' `formerKeys`, see `_history_current_params`) is reported under
# its current key, against that key's default — in a group entry too.
function _hp_flat(params::AbstractDict, defaults::Dict{String,Any}, leaves = nothing)
    out = Dict{String,Tuple{Any,Any}}()
    for (k, v) in _history_current_params(params, leaves)
        g = get(defaults, k, missing)
        if g isa NamedTuple && v isa AbstractDict
            ids = sort!(collect(keys(v)); by = x -> something(tryparse(Int, string(x)), typemax(Int)))
            for (pos, i) in enumerate(ids)
                e = v[i]
                e isa AbstractDict || continue
                ed = _hp_entry_defaults(g, pos - 1)
                for (ck, cv) in _history_current_params(e, leaves)
                    out["$k[$i].$ck"] = (cv, get(ed, ck, missing))
                end
            end
        else
            out[k] = (v, g isa NamedTuple ? missing : g)
        end
    end
    out
end

_hp_empty(v) = v === nothing || v === missing || v == "" || (v isa Union{AbstractVector,AbstractDict} && isempty(v))
_hp_key(v) = _hp_empty(v) ? "" : v isa Real && !(v isa Bool) ? string(Float64(v)) : JSON3.write(v)
_hp_show(v) = _hp_empty(v) ? "(none)" : v isa AbstractString ? v :
              v isa AbstractVector && all(x -> x isa Union{AbstractString,Real}, v) ? join(string.(v), ", ") :
              v isa Real ? string(v) : JSON3.write(v)

"""
    history_params_table(proj, image_uids) -> Vector{Dict}

One row per step that ran on `image_uids` (grouped as `chain_from_history` groups them), for a Methods
table: `step`, `task`, `output`, `images` (`"n/N"`), `lastRun`, `relation` (scoped only: `made` /
`ran on`), `taskIds` (the run-log task ids behind the row), `changedFromDefault` (`key=value; …`,
only params every image ran with the same non-default value), `conflicts` (`key: a (n) | b (m); …`,
params the images disagree on), `note` (why a step is not something a chain replays, or why its
params are unknown).

`pops` cuts the table to what a board shows (`nothing` = the whole history): board population keys
(`popType::labelSet/pop`) plus `cluster_runs` (`"popType:suffix"`, the run a board's cluster panels
draw) — see `_history_scope`.
"""
function history_params_table(proj::CciaProject, image_uids::Vector{String};
                              pops::Union{Nothing,AbstractVector} = nothing,
                              cluster_runs::AbstractVector = String[])::Vector{Dict{String,Any}}
    leaves = Dict{String,Any}()
    leaves_of(fn) = get!(() -> _history_leaves(fn), leaves, fn)
    rows = _history_rows(proj, image_uids)
    latest, held, skipped = _history_latest(rows, leaves_of)
    N = length(image_uids)
    why_of = Dict((r.fn, r.uid) => w for (r, w) in skipped)
    out = Dict{String,Any}[]

    groups = [(k, v, false) for (k, v) in latest]
    append!(groups, [(k, v, true) for (k, v) in held])
    sort!(groups; by = g -> minimum(r.at for r in values(g[2])))
    groups = [(g..., "") for g in groups]
    unmade = Tuple{String,String}[]
    if pops !== nothing
        runs = _hp_cluster_runs(proj, image_uids, [_parse_tkey(string(k)) for k in pops], String.(cluster_runs))
        # label set → the populations shown on it; a `labels` key (a segmentation QC plot) draws the
        # objects themselves, so nothing run on them since is part of it
        shown = Dict{String,Vector{String}}()
        for t in (_parse_tkey(string(k)) for k in pops)
            isempty(t.valueName) && continue
            v = get!(Vector{String}, shown, t.valueName)
            t.popType == "labels" || push!(v, t.pop)
        end
        relation, unmade = _history_scope(groups, leaves_of, shown, runs)
        groups = [(g[1:3]..., relation[i]) for (i, g) in enumerate(groups) if haskey(relation, i)]
    end
    for ((fn, _), by_img, is_held, relation) in groups
        defaults = _history_param_defaults(fn)
        spec = try _task_spec(_task_from_fun_name(fn)) catch; nothing end
        out_name, _ = _history_output(fn, first(values(by_img)).params, leaves_of(fn))
        flats = [_hp_flat(r.params, defaults, leaves_of(fn)) for r in values(by_img)]
        changed = String[]; conflicts = String[]
        for k in sort!(unique(vcat((collect(keys(f)) for f in flats)...)))
            k == "valueName" && continue                       # shown as the output
            l = get(something(leaves_of(fn), Dict{String,_HistoryLeaf}()), k, nothing)
            # a crop box is drawn per image; its coordinates mean nothing in a table
            (l !== nothing && l.type == "imagePicker") && (push!(changed, "$k=(set per image)"); continue)
            vals = [haskey(f, k) ? f[k][1] : nothing for f in flats]
            dflt = first(f[k][2] for f in flats if haskey(f, k))
            counts = Dict{String,Tuple{Any,Int}}()
            for v in vals
                kk = _hp_key(v)
                counts[kk] = (v, get(counts, kk, (v, 0))[2] + 1)
            end
            if length(counts) == 1
                v = first(vals)
                same = dflt === missing ? _hp_empty(v) : _hp_key(v) == _hp_key(dflt)
                same || push!(changed, "$k=$(_hp_show(v))")
            else
                spread = sort!(collect(values(counts)); by = x -> -x[2])
                push!(conflicts, "$k: " * join(("$(_hp_show(v)) ($n)" for (v, n) in spread), " | "))
            end
        end
        note = is_held ? get(why_of, (fn, first(keys(by_img))), "") : ""
        push!(out, Dict{String,Any}(
            "step" => spec === nothing ? fn : string(get(spec, "label", fn)), "task" => fn,
            "output" => out_name, "images" => "$(length(by_img))/$N",
            "lastRun" => maximum(r.at for r in values(by_img)),
            "changedFromDefault" => join(changed, "; "), "conflicts" => join(conflicts, "; "),
            "note" => note, "relation" => relation,
            # the run-log task ids of the runs this row reports (one per image's latest run; a chain
            # record or an entry from before task ids were logged has none) — the export provenance
            "taskIds" => sort!(unique(String[r.taskId for r in values(by_img) if !isempty(r.taskId)]))))
    end

    # runs with no params to report — logged before params were recorded, or a task that is gone —
    # unless a later run of the same task on that image is already in a row above
    known = Set((fn, u) for ((fn, _), by_img, _, _) in groups for u in keys(by_img))
    lost = Dict{Tuple{String,String},Set{String}}()
    for (r, w) in skipped
        (r.legacy || w == "this task no longer exists") || continue
        # a scoped table can't place a run whose params (and so outputs) are unknown — say so anyway
        # for a task that is otherwise in the lineage, or that would have made something left unmade
        pops === nothing || any(g -> g[1][1] == r.fn, groups) ||
            any(u -> !isempty(u[2]) && startswith(r.fn, u[2]), unmade) || continue
        (r.fn, r.uid) in known && continue
        push!(get!(Set{String}, lost, (r.fn, r.legacy ? "ran before params were recorded" : w)), r.uid)
    end
    for ((fn, w), uids) in sort!(collect(lost); by = first)
        push!(out, Dict{String,Any}("step" => fn, "task" => fn, "output" => "", "images" => "$(length(uids))/$N",
                                    "lastRun" => "", "changedFromDefault" => "", "conflicts" => "",
                                    "note" => "parameters unknown — $w", "relation" => "", "taskIds" => String[]))
    end
    # what the board shows that no recorded run made: said, so a gap never reads as a complete answer
    for (what, _) in unmade
        push!(out, Dict{String,Any}("step" => what, "task" => "", "output" => "", "images" => "",
                                    "lastRun" => "", "changedFromDefault" => "", "conflicts" => "",
                                    "note" => "no recorded run made this — ran before the run log, or outside Cecelia",
                                    "relation" => "made", "taskIds" => String[]))
    end
    out
end
