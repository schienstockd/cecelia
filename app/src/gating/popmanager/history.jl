# Gate content hashes + the append-only gate history beside each gating sidecar.
#
# A gating sidecar is overwritten in place (it is the user's current strategy), so on its own it cannot
# say which gate a plotted population or a downstream run was made with. Two pieces close that:
#
#   • `pop_gate_hashes(m)` — one short content hash per population over its whole GATE DEFINITION: its
#     own gate/filter/boolean record, its parent's hash (membership is ∩ parent, so a moved parent gate
#     changes every hash under it), the hashes of the pops a boolean combines, and — for a gate on a
#     position axis — the map's `spatial_unit`. Name, path, uid, colour, show and author stamps are NOT
#     in it: a rename or recolour keeps the hash. It names the gate, not the data it is applied to: a
#     re-segmentation or a corrected pixel size changes the cells under the same hash (the run log's
#     `inputs` / `replaces` record those). The scheduler stamps the hashes of the pops a
#     run reads into its run-log entry (`gates`, `run_gates` in run_log.jl).
#   • `gating/{vn}[__type].history.jsonl` — `save_pop_map!` (the ONE gating writer: GUI routes, the MCP
#     gate tools, gate copy, tasks and the REPL all reach the file through it) appends the PREVIOUS
#     state there whenever a write changes any population's definition: `{at, author, valueName,
#     popType, kind, changes:[{kind, path, previousPath?, hash?, previousHash?}], previous}`.
#     `previous` is the whole prior tree (null for the first write); cosmetic-only writes (colour,
#     show, a uid backfill) record nothing. Together with the current file this holds every state a
#     run could have read, so `gate_state_for_hash` can answer "which gate is hash h".
#
# Absent history (a project from before this) = unknown, never an error. Append-only, like the run-log
# archive (`append_jsonl!` / `read_jsonl`, utils.jl); a torn last line is skipped by the reader.

const GATING_HISTORY_SUFFIX = ".history.jsonl"

"""The gate history file beside a gating sidecar path (`…/gating/B.json` → `…/gating/B.history.jsonl`)."""
gating_history_file(sidecar::AbstractString) = sidecar[1:end-length(".json")] * GATING_HISTORY_SUFFIX

"""`gating/{value_name}[__type].history.jsonl` — the gate history beside the sidecar `gating_path` names."""
gating_history_path(task_dir::AbstractString, value_name::AbstractString; pop_type::PopTypeArg="flow") =
    gating_history_file(gating_path(task_dir, value_name; pop_type = pop_type))

"""The history entries for one sidecar, oldest first; [] when there is none (see the file header)."""
read_gating_history(task_dir::AbstractString, value_name::AbstractString; pop_type::PopTypeArg="flow") =
    read_jsonl(gating_history_path(task_dir, value_name; pop_type = pop_type))

# A canonical string for hashing: dict keys sorted, so two equal definitions hash equal whatever order
# their dicts were built in. Scalars go through JSON3, which writes the shortest round-tripping float.
_gate_canon(x::AbstractDict) =
    "{" * join((JSON3.write(string(k)) * ":" * _gate_canon(x[k]) for k in sort!(collect(keys(x)); by = string)), ",") * "}"
_gate_canon(x::AbstractVector) = "[" * join((_gate_canon(v) for v in x), ",") * "]"
_gate_canon(x) = JSON3.write(x)

# What a population's OWN record says about its cells (no parent, no identity, no ornaments).
function _pop_own_defn(p::Population)::Dict{String,Any}
    d = Dict{String,Any}()
    p.gate === nothing || (d["gate"] = gate_spec(p.gate))
    if p.filter_measure !== nothing || p.filter_conditions !== nothing
        d["filter"] = Dict{String,Any}(
            "measure" => p.filter_measure,
            "fun" => p.filter_fun === nothing ? nothing : string(p.filter_fun),
            "values" => p.filter_values, "default_all" => p.filter_default_all,
            "conditions" => p.filter_conditions === nothing ? nothing :
                [Dict{String,Any}("measure" => c.measure, "fun" => string(c.fun), "values" => c.values)
                 for c in p.filter_conditions])
    end
    p.boolean_op === nothing ||
        (d["boolean"] = Dict{String,Any}("op" => string(p.boolean_op),
                                         "pops" => something(p.boolean_pops, String[]),
                                         "not" => something(p.boolean_not, String[])))
    p.is_track && (d["is_track"] = true)
    p.explicit_labels === nothing || (d["labels"] = sort(p.explicit_labels))
    d
end

"""
    pop_gate_hashes(m) -> Dict{path => hash}

The content hash of every population's gate definition in `m` (16 hex chars of a SHA-256). See the
file header for what it covers — the gate, not the cells it selects today. Compute it on a map read from disk (or round-tripped through JSON), so an in-memory
`0` and an on-disk `0.0` cannot differ.
"""
function pop_gate_hashes(m::PopulationMap)::Dict{String,String}
    memo = Dict{String,String}()
    for path in m.order
        _pop_gate_hash!(memo, m, path, Set{String}())
    end
    memo
end

function _pop_gate_hash!(memo::Dict{String,String}, m::PopulationMap, path::AbstractString, visiting::Set{String})
    path = String(path)
    haskey(memo, path) && return memo[path]
    (is_root(path) || !has_pop(m, path)) && return ""
    path in visiting && return "cycle"      # only reachable by hand-editing a sidecar
    push!(visiting, path)
    p = m.pops[path]
    d = _pop_own_defn(p)
    if haskey(d, "boolean")                 # a combination's cells are its terms' cells: hash those
        d["boolean"]["pops"] = [_pop_gate_hash!(memo, m, r, visiting) for r in d["boolean"]["pops"]]
        d["boolean"]["not"]  = [_pop_gate_hash!(memo, m, r, visiting) for r in d["boolean"]["not"]]
    end
    d["parent"] = _pop_gate_hash!(memo, m, p.parent, visiting)
    pop_has_spatial_gate(p) && (d["spatial_unit"] = m.spatial_unit)
    delete!(visiting, path)
    memo[path] = bytes2hex(SHA.sha256(_gate_canon(d)))[1:16]
end

# Only a gate on a position axis (`pop_has_spatial_gate`, population.jl) changes meaning with the map's `spatial_unit`:
# an intensity gate is unit-free, so a px → µm restamp of its map is no change.

# The per-population changes between two states (`nothing` = no file before). Pops are matched by uid,
# so a rename/move reads as one `move`, not a delete plus an add — except against a state whose uids
# were backfilled on load (they are fresh random ones), which is matched by path instead.
function _gating_changes(prev::Union{PopulationMap,Nothing}, new::PopulationMap)::Vector{Any}
    by_path = prev === nothing || prev._uid_backfilled
    keyed(m) = m === nothing ? Dict{String,Population}() :
        Dict{String,Population}((by_path ? p.path : p.uid) => p for p in values(m.pops) if !p.transient)
    old, cur = keyed(prev), keyed(new)
    oh = prev === nothing ? Dict{String,String}() : pop_gate_hashes(prev)
    nh = pop_gate_hashes(new)
    unit_changed = prev !== nothing && prev.spatial_unit != new.spatial_unit
    out = Any[]
    for k in sort!(collect(union(keys(old), keys(cur))))
        o = get(old, k, nothing); n = get(cur, k, nothing)
        if o === nothing
            push!(out, Dict{String,Any}("kind" => "add", "path" => n.path, "hash" => nh[n.path]))
        elseif n === nothing
            push!(out, Dict{String,Any}("kind" => "delete", "path" => o.path, "previousHash" => oh[o.path]))
        else
            moved = o.path != n.path
            changed = (unit_changed && pop_has_spatial_gate(n)) || _gate_canon(_pop_own_defn(o)) != _gate_canon(_pop_own_defn(n))
            (moved || changed) || continue
            c = Dict{String,Any}("kind" => changed ? "change" : "move", "path" => n.path,
                                 "hash" => nh[n.path], "previousHash" => oh[o.path])
            moved && (c["previousPath"] = o.path)
            push!(out, c)
        end
    end
    out
end

# Called by `save_pop_map!` under `with_popmap_lock`, BEFORE the new file is written: a kill between the
# two leaves an entry whose new state never landed, never a lost previous state. Never throws — a gate
# edit must not fail because its history line could not be saved. `kind` overrides the summarised kind
# (a value_name rename, whose sidecar is new under its new name).
function _record_gating_history!(task_dir::AbstractString, m::PopulationMap, new_tree::AbstractDict;
                                 kind::Union{AbstractString,Nothing} = nothing)
    try
        path = gating_path(task_dir, m.value_name; pop_type = m.pop_type)
        prev_tree = isfile(path) ? (try JSON3.read(read(path, String), Dict{String,Any}) catch; nothing end) : nothing
        prev = prev_tree === nothing ? nothing : (try from_tree(prev_tree) catch; nothing end)
        new = from_tree(JSON3.read(JSON3.write(new_tree), Dict{String,Any}))
        changes = _gating_changes(prev, new)
        (isempty(changes) && kind === nothing) && return nothing
        kinds = unique(String[c["kind"] for c in changes])
        author = try author_stamp() catch; Dict{String,Any}() end
        entry = Dict{String,Any}(
            "at" => Dates.format(Dates.now(), "yyyy-mm-ddTHH:MM:SS"), "author" => author,
            "valueName" => m.value_name, "popType" => string(m.pop_type),
            "kind" => kind !== nothing ? String(kind) : length(kinds) == 1 ? only(kinds) : "edit",
            "changes" => changes, "previous" => prev_tree)
        append_jsonl!(gating_history_path(task_dir, m.value_name; pop_type = m.pop_type), [entry])
    catch e
        @warn "gate history append failed; the gate is saved without it" value_name = m.value_name exception = e
    end
    nothing
end

"""
    gate_state_for_hash(task_dir, value_name, hash; pop_type) -> (; path, definition, at, current) | nothing

The population whose content hash (`pop_gate_hashes`) is `hash` — in the current sidecar (`current =
true`, `at = nothing`), else in the newest history state holding it (`at` = when that state was
replaced). `definition` is its own gate/filter/boolean record. `nothing` = no state on disk has it
(made before the history existed, or a different sidecar).
"""
function gate_state_for_hash(task_dir::AbstractString, value_name::AbstractString, hash::AbstractString;
                             pop_type::PopTypeArg="flow")
    find(m) = (for (p, h) in pop_gate_hashes(m); h == hash && return p; end; nothing)
    cur = load_pop_map(task_dir, value_name; pop_type = pop_type, backfill_save = false)
    p = find(cur)
    p === nothing || return (; path = p, definition = _pop_own_defn(cur.pops[p]), at = nothing, current = true)
    for e in Iterators.reverse(read_gating_history(task_dir, value_name; pop_type = pop_type))
        prev = get(e, :previous, get(e, "previous", nothing))
        prev isa AbstractDict || continue
        m = try from_tree(json_native(prev)) catch; continue end
        p = find(m)
        p === nothing || return (; path = p, definition = _pop_own_defn(m.pops[p]),
                                   at = string(get(e, :at, get(e, "at", ""))), current = false)
    end
    nothing
end
