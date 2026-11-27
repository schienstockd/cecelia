# Mutations — the write API for a PopulationTree: add/rename/remove/move, gate/filter updates,
# colour swaps, child-parent moves, path-recompute on rename. Every write is applied through here
# so persistence.jl's save-on-change can trigger from one place and boolean-pop dependents stay
# consistent (an operand rename cascades through descendant filters, not just the renamed pop).

# ── Mutations ────────────────────────────────────────────────────────────────────
function add_pop!(m::PopulationTree, name::AbstractString;
                  parent::AbstractString=ROOT, gate::Union{Gate,Nothing}=nothing,
                  colour::AbstractString="#ffffff", show::Bool=true,
                  filter_measure=nothing, filter_fun=nothing, filter_values=nothing,
                  filter_default_all::Bool=false, filter_conditions=nothing, is_track::Bool=false,
                  boolean_op=nothing, boolean_pops=nothing, boolean_not=nothing,
                  explicit_labels=nothing, transient::Bool=false,
                  # UID for outside references (`track_source`, MULTI_POP_TRACKING_PLAN.md Decision 0).
                  # Empty ⇒ generated fresh; explicit ⇒ used on load (round-trip stability) but rerolled
                  # if it collides with another pop's UID in this map. Never reflect this back to the
                  # caller as anything other than the freshly-picked one.
                  uid::AbstractString="",
                  # `author_stamp()` of the request that drew it — see `Population.created_by`
                  created_by::Union{AbstractDict,Nothing}=nothing,
                  updated_by::Union{AbstractDict,Nothing}=nothing,
                  run_at::Union{AbstractString,Nothing}=nothing,   # see `Population.run_at`
                  track_class::Union{AbstractDict,Nothing}=nothing, # see `Population.track_class`
                  track_class_of::Union{AbstractString,Nothing}=nothing, # see `Population.track_class_of`
                  reserved_ok::Bool=false, validate_refs::Bool=true)::String
    # `_`-prefixed names are reserved for derived populations (e.g. _tracked); only the derived
    # injection (reserved_ok=true) may create them, so a hand-drawn gate can't shadow one.
    (reserved_ok || !is_reserved_pop_name(name)) ||
        error("add_pop!: names beginning with \"$DERIVED_POP_PREFIX\" are reserved for derived " *
              "populations (e.g. _tracked) — choose another name")
    parent = is_root(parent) ? ROOT : String(parent)
    (parent == ROOT || has_pop(m, parent)) || error("add_pop!: parent not found: $parent")
    path = pop_path(parent, name)
    has_pop(m, path) && error("add_pop!: population already exists: $path")
    # `validate_refs=false` for deserialisation: `from_tree` walks the tree depth-first, so a boolean
    # pop is often built BEFORE the siblings it references. A file's references are checked where it
    # matters instead — `recompute!` degrades an unresolvable one to empty + a warning.
    bop, bpops, bnot = _normalise_boolean(boolean_op, boolean_pops, boolean_not; self = path)
    if bop !== nothing && validate_refs
        for r in [bpops; bnot]
            has_pop(m, r) || error("add_pop!: population to combine not found: $r")
        end
        boolean_cycle(m, path, [bpops; bnot]) &&
            error("add_pop!: that combination would depend on itself")
    end
    conds = _normalise_conditions(filter_conditions)
    # when compound, mirror the single fields onto conditions[1] so single-field readers still work.
    if conds !== nothing
        filter_measure = conds[1].measure; filter_fun = conds[1].fun; filter_values = conds[1].values
    end
    resolved_uid = _fresh_pop_uid(m; preferred = String(uid))
    m.pops[path] = Population(resolved_uid,
                              String(name), path, parent, String(colour), show,
                              m.pop_type, m.value_name, gate,
                              filter_measure === nothing ? nothing : String(filter_measure),
                              filter_fun === nothing ? nothing : _coerce_filter_fun(filter_fun),
                              filter_values, filter_default_all, conds, is_track, bop, bpops, bnot,
                              explicit_labels === nothing ? nothing : Int[Int(l) for l in explicit_labels], transient,
                              _stamp_dict(created_by), _stamp_dict(updated_by),
                              run_at === nothing ? nothing : String(run_at), _stamp_dict(track_class),
                              track_class_of === nothing ? nothing : String(track_class_of))
    m.uid_index[resolved_uid] = path
    push!(m.order, path)
    _invalidate!(m)
    path
end

_stamp_dict(s) = s === nothing ? nothing : Dict{String,Any}(String(k) => v for (k, v) in s)

"""
    stamp_edit!(m, path) -> Bool

Record the request's `author_stamp()` as `updated_by` on a population that carries an author stamp
(a Claude proposal); a plain GUI gate is left alone, so its sidecar keeps its shape. `true` if stamped.
"""
function stamp_edit!(m::PopulationTree, path::AbstractString)::Bool
    p = get(m.pops, String(path), nothing)
    (p === nothing || p.created_by === nothing) && return false
    p.updated_by = author_stamp()
    true
end

function set_gate!(m::PopulationTree, path::AbstractString, gate::Gate)
    has_pop(m, path) || error("set_gate!: not found: $path")
    p = m.pops[String(path)]
    p.gate = gate
    # a split gate's regions move together: every sibling of its group takes the new divider and keeps
    # its own region, so the regions still tile the plane (split_gates.jl)
    grp = gate_group(gate)
    if !isempty(grp)
        for q in values(m.pops)
            (q === p || q.parent != p.parent || q.gate === nothing || gate_group(q.gate) != grp) && continue
            moved = with_divider(q.gate, gate)
            moved === nothing || (q.gate = moved)
        end
    end
    _invalidate!(m)
    m
end


"""
    set_boolean!(m, path; op, pops, nots)

Rewrite (or, with `op=nothing`, clear) a population's boolean definition — the edit counterpart of
`set_gate!`. Same guards as `add_pop!`: known references, no self-reference, no dependency loop.
"""
function set_boolean!(m::PopulationTree, path::AbstractString; op=nothing, pops=nothing, nots=nothing)
    has_pop(m, path) || error("set_boolean!: not found: $path")
    path = String(path)
    bop, bpops, bnot = _normalise_boolean(op, pops, nots; self = path)
    if bop !== nothing
        for r in [bpops; bnot]
            has_pop(m, r) || error("set_boolean!: population to combine not found: $r")
        end
        boolean_cycle(m, path, [bpops; bnot]) &&
            error("set_boolean!: that combination would depend on itself")
    end
    p = m.pops[path]
    p.boolean_op = bop
    p.boolean_pops = bpops
    p.boolean_not = bnot
    _invalidate!(m)
    m
end

"""Rename a population, cascading the path change to all descendants."""
function rename_pop!(m::PopulationTree, path::AbstractString, newname::AbstractString)::String
    has_pop(m, path) || error("rename_pop!: not found: $path")
    !is_reserved_pop_name(newname) ||
        error("rename_pop!: names beginning with \"$DERIVED_POP_PREFIX\" are reserved for " *
              "derived populations (e.g. _tracked) — choose another name")
    path = String(path)
    p = m.pops[path]
    newpath = pop_path(p.parent, newname)
    newpath == path && return path
    has_pop(m, newpath) && error("rename_pop!: target exists: $newpath")
    _repath!(m, path, newpath)
end

"""
    move_pop!(m, path, newparent) -> String

Re-parent a population (with its whole subtree) under `newparent` (`ROOT` for top level), returning
its new path. The population keeps its name, colour, gate/filter and children — only where it sits in
the tree changes, and with it its MEMBERSHIP: a pop's cells are its own gate ∩ its parent's, so moving
`/qc/B` to `/B` re-derives it against all cells instead of the qc-passing ones (`_invalidate!`, then
`recompute!` on the next read). That is the point of the operation, not a side effect.

Rejects a move into the pop's own subtree (a cycle: a pop cannot be its own ancestor) and one whose
target path is already taken. Same cascade as `rename_pop!` — both are one path rewrite.
"""
function move_pop!(m::PopulationTree, path::AbstractString, newparent::AbstractString)::String
    has_pop(m, path) || error("move_pop!: not found: $path")
    path = String(path)
    newparent = is_root(newparent) ? ROOT : String(newparent)
    (newparent == ROOT || has_pop(m, newparent)) || error("move_pop!: parent not found: $newparent")
    (newparent == path || startswith(newparent, path * "/")) &&
        error("move_pop!: cannot move a population into itself or one of its own descendants")
    p = m.pops[path]
    newparent == p.parent && return path
    newpath = pop_path(newparent, p.name)
    has_pop(m, newpath) && error("move_pop!: target exists: $newpath")
    _repath!(m, path, newpath)
    m.pops[newpath].parent = newparent          # the subtree kept its own parent; this one changes
    # keep `order`'s parents-before-children invariant: the moved subtree goes after its new parent
    # (which may sit later in the list than the old one did). Relative order within it is preserved.
    moved = [newpath; descendants(m, newpath)]
    m.order = [[o for o in m.order if !(o in moved)]; moved]
    newpath
end

# Rewrite `path` and every descendant to sit at `newpath` — the cascade shared by rename (the leaf
# name changes) and move (the parent does). Callers own the guards; this one only rewrites.
function _repath!(m::PopulationTree, path::AbstractString, newpath::AbstractString)::String
    affected = [path; descendants(m, path)]
    for old in affected
        np = _replace_prefix(old, path, newpath)
        pop = m.pops[old]
        pop.path = np
        pop.parent = _replace_prefix(pop.parent, path, newpath)
        pop.name = pop_name(np)
        pop.value_name = m.value_name
        # uid_index tracks CURRENT path — update every affected pop's mapping. UID itself is unchanged
        # (that is the point: rename/move preserves identity).
        m.uid_index[pop.uid] = np
        if np != old
            m.pops[np] = pop
            delete!(m.pops, old)
        end
    end
    m.order = [_replace_prefix(o, path, newpath) for o in m.order]
    # boolean references are PATHS, so a rename/move of a referenced pop has to rewrite them too —
    # otherwise "GFP+ or TOM+" silently loses a term the moment either gate is renamed or re-parented.
    rewrite(rs) = rs === nothing ? nothing : [_replace_prefix(r, path, newpath) for r in rs]
    for pop in values(m.pops)
        pop.boolean_pops = rewrite(pop.boolean_pops)
        pop.boolean_not  = rewrite(pop.boolean_not)
        pop.track_class_of === nothing || (pop.track_class_of = _replace_prefix(pop.track_class_of, path, newpath))
    end
    _invalidate!(m)
    newpath
end

"""Delete a population and all its descendants (cascade)."""
function del_pop!(m::PopulationTree, path::AbstractString)
    has_pop(m, path) || error("del_pop!: not found: $path")
    path = String(path)
    _del_paths!(m, Set([path; descendants(m, path)]))
end

"""
    del_children!(m, path)

Delete everything UNDER `path` — its whole subtree — and keep the population itself. The other half
of `del_pop!`: pruning a strategy back to a gate you want to re-gate from, without redrawing that
gate. Membership of `path` is unaffected (nothing below it feeds into it).
"""
function del_children!(m::PopulationTree, path::AbstractString)
    has_pop(m, path) || error("del_children!: not found: $path")
    _del_paths!(m, Set(descendants(m, String(path))))
end

"""
    selection_roots(m, paths) -> Vector{String}

The populations a bulk action on `paths` acts on directly, in map order: duplicates dropped, and
every path whose ancestor is also selected dropped too — it travels with (moves with, or is deleted
with) that ancestor, so acting on it separately would double-apply. Errors on an unknown path.
"""
function selection_roots(m::PopulationTree, paths)::Vector{String}
    sel = unique(String[String(p) for p in paths])
    isempty(sel) && error("no populations given")
    for p in sel
        has_pop(m, p) || error("not found: $p")
    end
    [p for p in m.order if p in sel && !any(a -> startswith(p, a * "/"), sel)]
end

"""
    move_pops!(m, paths, newparent) -> Vector{String}

Re-parent several populations under `newparent` in one mutation — `move_pop!` for a selection. A
selected population whose ancestor is also selected keeps its place under that ancestor (the subtree
moves whole). Everything is checked BEFORE anything moves, so a refused selection leaves the map as it
was: a target inside any selected subtree (a cycle), a target path already taken, or two selected
populations with the same name landing under the same parent. Returns the new path of each moved root,
in map order (one already under `newparent` stays and is returned as is).
"""
function move_pops!(m::PopulationTree, paths, newparent::AbstractString)::Vector{String}
    roots = selection_roots(m, paths)
    newparent = is_root(newparent) ? ROOT : String(newparent)
    (newparent == ROOT || has_pop(m, newparent)) || error("move_pops!: parent not found: $newparent")
    for r in roots
        (newparent == r || startswith(newparent, r * "/")) &&
            error("cannot move \"$(pop_name(r))\" into itself or one of its own descendants")
    end
    landing = Dict{String,String}()
    for r in roots
        m.pops[r].parent == newparent && continue
        np = pop_path(newparent, m.pops[r].name)
        has_pop(m, np) && error("target exists: $np")
        haskey(landing, np) && error("\"$(landing[np])\" and \"$r\" would both land at $np — rename one first")
        landing[np] = r
    end
    [move_pop!(m, r, newparent) for r in roots]
end

"""
    del_pops!(m, paths)

Delete several populations (each with its subtree) in one mutation — `del_pop!` for a selection. A
selected child of a selected parent is simply part of the parent's cascade. Errors on an unknown path
before deleting anything.
"""
function del_pops!(m::PopulationTree, paths)
    _del_paths!(m, Set(pops_with_subtrees(m, paths)))
end

"""Every population in `paths` plus all their descendants, deduplicated (what `del_pops!` removes)."""
pops_with_subtrees(m::PopulationTree, paths)::Vector{String} =
    unique(reduce(vcat, ([r; descendants(m, r)] for r in selection_roots(m, paths)); init = String[]))

function _del_paths!(m::PopulationTree, targets::Set{String})
    for t in targets
        # drop the reverse-lookup entry first so `uid_index` never points at a deleted path, then
        # RETIRE the UID so `_fresh_pop_uid` can never reissue it — a `track_source` obs value in the
        # h5ad outlives the pop that stamped it, and a reused UID would silently inherit the deleted
        # pop's lineage (MULTI_POP_TRACKING_ORPHANS_PLAN decision 4).
        p = get(m.pops, t, nothing)
        if p !== nothing
            delete!(m.uid_index, p.uid)
            push!(m.retired_uids, p.uid)
        end
        delete!(m.pops, t)
    end
    filter!(p -> !(p in targets), m.order)
    _invalidate!(m)
    m
end

_invalidate!(m::PopulationTree) = (m._labels = nothing; m._membership = nothing; m)

