# ── Task-spec leaf readers ───────────────────────────────────────────────────────────────────────
#
# What a task's declared params say about the names a run of it reads and writes: the leaves
# (`_history_leaves` — type, namespace, writes?, renamed keys …), the name a run writes
# (`_history_output`), every keyed string in its saved params (`_keyed_strings`), and the gated
# populations its pickers read (`gated_pop_refs`). Spec reading only — no run log, no image writes —
# so the chain pre-flight, chain-from-history, the Methods table, the claim checks and the run log's
# provenance (run_provenance.jl) all read through the one copy here.

# What a param says about the names it holds. `ns` is the namespace a value lives in — the output
# namespace for a name the task WRITES (`namespace`), the field it READS from (`field`: labels,
# spatialGraphs, …; image versions are ""), or "models" for a model picker (`optionsFrom` a vault).
# `former` = the param's `formerKeys` (a renamed param), which a run saved before the rename still uses.
# `columns` = the param's `producesColumns` templates (`{}` = its value), `yields` its `producesPop`
# marker: what a task writes that no namespace names, so a reader can still be wired to it.
const _HistoryLeaf = @NamedTuple{type::String, ns::String, writes::Bool, accepts::Vector{String},
                                 former::Vector{String}, columns::Vector{String}, yields::String,
                                 multiple::Bool}

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
                    columns = cols, yields = string(something(get(p, "producesPop", nothing), "")),
                    multiple = get(p, "multiple", false) == true)
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

# A population picker value that means the whole segmentation, not a gated population.
_whole_pop(v::AbstractString) = is_root(v) || v == "NONE"

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

# (label set, population path) pairs a step reads through its population pickers — only pickers that
# take gated cell populations (`accepts` has "live", or none declared). A bare path (`/qc/frags`) is on
# the step's own `valueName`; `B/qc/_tracked` names its label set. System segments (`_tracked`, …) are
# cut: what must exist is the gated population above them. A segment naming a run (`clusters.immune`)
# is a population a task produces, not a gate, so that reference is skipped. `all_pickers = true` takes
# every population picker, track pickers included (`run_gates`: a track analysis on `B/qc/_tracked`
# reads the flow gate `/qc` too); the caller then resolves which sidecar each path lives in.

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

# The image field a name in namespace `ns` lives in ("" = image versions). `nothing` = not checkable here.
function _preflight_fields(ns::String)
    ns == ""       ? (:filepath,) :
    ns == "labels" ? (:labels, :label_props) : nothing
end

_preflight_has(img, name, ns) =
    (fs = _preflight_fields(ns); fs === nothing || any(f -> img_has_value_name(img, name; field = f), fs))

"""
    is_bookkeeping_param(key) -> Bool

A task param that says how a run writes, not how it processes: `overwrite` (replace a label set of the
same name) — the cells come out the same either way. `key` is the param name or a dotted path to it.
The Methods table and the claim checks leave it out; a chain still replays it.
"""
const BOOKKEEPING_PARAMS = ("overwrite",)
is_bookkeeping_param(key::AbstractString) = last(split(key, '.')) in BOOKKEEPING_PARAMS
