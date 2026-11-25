# ── Copy a gating strategy between images ─────────────────────────────────────
#
# The package half of `POST /api/gating/copy`, so a task can do it too: the manual gate step
# (`tasks/gate/manual_gate.jl`) prefills each image of a chain run from an exemplar, and a chain may run
# in the detached runner, which never loads `api/src`.

# Serialise gating load→mutate→save. Every writer of a gating sidecar holds it across the whole cycle;
# without it two concurrent edits to one map both load the same tree and the second save drops the
# first. One process-wide lock is enough — gating edits are user-paced and cheap. The API's gating
# handlers take it through `with_pop_tree_lock` too, so a chain step and a click cannot interleave.
const _POP_TREE_LOCK = ReentrantLock()
with_pop_tree_lock(f) = lock(f, _POP_TREE_LOCK)

"""
    plan_gating_copy(m, timg; value_name, needed = _needed_columns(m), src_names) -> (; tree, remapped, skip)

What copying the source map `m` onto `timg` would write, without writing it: `tree` — `m` with its
gate/filter columns pointed at `timg`'s columns for the same channel names (`m` itself when nothing
moves), `remapped` — those moves, or `skip` — why the copy would leave this image out (no such label
set, a µm position gate on an uncalibrated image, a channel it cannot match). `copy_gating!` writes
`tree`; the rule preview (`GET /api/gating/rule-preview`) evaluates it, so both decide the same way.
"""
function plan_gating_copy(m::PopulationTree, timg::CciaImage; value_name::AbstractString,
                          needed = _needed_columns(m), src_names)
    vn = String(value_name)
    none = @NamedTuple{from::String, to::String, channel::String}[]
    skip(why) = (; tree = nothing, remapped = none, skip = why)
    img_has_value_name(timg, vn) || return skip("no segmentation '$vn'")
    # A µm POSITION gate needs the target's own pixel size; without one the coordinates would
    # silently be read as pixels (docs/todo/SPATIAL_GATE_UNITS_PLAN.md decision 7).
    (m.spatial_unit == SPATIAL_UNIT_UM && has_spatial_gate(m) && !img_is_calibrated(timg)) &&
        return skip("no physical pixel size — set it to copy a position gate")
    # Gate columns are named by channel index: point them at the target's column for the same
    # channel name, so a target with another channel order still gates the same marker.
    mt = match_channel_columns(needed, src_names, channel_names(timg; value_name = vn))
    isempty(mt.problems) || return skip("channels don't match — " * describe_channel_problems(mt.problems))
    tree = isempty(mt.remapped) ? m : rename_pop_columns(m, Dict(r.from => r.to for r in mt.remapped))
    (; tree, remapped = mt.remapped, skip = nothing)
end

"""
    copy_gating!(src, targets; value_name, pop_type, replace = true, on_copied = (img, m) -> nothing)
        -> (; copied, skipped, warnings, remapped)

Copy `src`'s gating for one label set + gating pop type onto each target, by writing the source's
sidecar there. Membership recomputes per image on read, so gates alone are enough. Per target:

- `skipped[uid]` — no such label set, a µm position gate on an uncalibrated image (it would be read as
  pixels), a channel a gate reads that the target lacks or holds twice (`match_channel_columns` — no
  right column to point it at), the write failed, or (`replace = false`) the target already has gating
  of its own.
- `remapped[uid]` — copied, with gate/filter columns pointed at the target's column for the same
  channel NAME, because the target orders its channels differently (`(from, to, channel)` each;
  intensity columns are named by channel index). Each target gets its own copy of the map.
- `warnings[uid]` — copied, but a gate/filter column the target's table lacks leaves those populations
  empty (`missing_gate_columns`).

A copied Claude proposal is the copier's edit (`stamp_edit!`, the current `author_stamp()`): a copy made
from the GUI is the user's, and Claude's own-gate routes refuse it.

Throws `ArgumentError` when the source has no gating to copy. `on_copied` runs after each write (the
API broadcasts the new map to any client viewing that image). Takes the gating lock itself.
"""
function copy_gating!(src::CciaImage, targets::AbstractVector{CciaImage};
                      value_name::AbstractString, pop_type::AbstractString,
                      replace::Bool = true, on_copied::Function = (_, _) -> nothing)
    vn, pt = String(value_name), String(pop_type)
    with_pop_tree_lock() do
        m = load_pop_tree(src; value_name = vn, pop_type = pt)
        isempty(m.order) && throw(ArgumentError("$(src.uid) has no $pt gating on '$vn' to copy"))
        foreach(path -> stamp_edit!(m, path), m.order)    # in memory only — the source is not re-saved
        src_names = channel_names(src; value_name = vn)
        copied   = String[]
        skipped  = Dict{String,String}()
        warnings = Dict{String,Vector{@NamedTuple{column::String, pops::Vector{String}}}}()
        remapped = Dict{String,Vector{@NamedTuple{from::String, to::String, channel::String}}}()
        needed   = _needed_columns(m)
        for timg in targets
            tuid = timg.uid
            tuid == src.uid && continue
            if !replace && img_has_value_name(timg, vn) &&
               !isempty(load_pop_tree(timg; value_name = vn, pop_type = pt, backfill_save = false).order)
                skipped[tuid] = "already gated"; continue
            end
            plan = plan_gating_copy(m, timg; value_name = vn, needed, src_names)
            plan.skip === nothing || (skipped[tuid] = plan.skip; continue)
            tm, mt = plan.tree, plan
            try
                save_pop_tree!(tm, timg)
                on_copied(timg, tm)
                push!(copied, tuid)
                isempty(mt.remapped) || (remapped[tuid] = mt.remapped)
            catch e
                skipped[tuid] = sprint(showerror, e); continue
            end
            miss = try
                missing_gate_columns(tm, timg; value_name = vn)
            catch e
                @warn "gating copy: could not check columns on $tuid" exception = e; nothing
            end
            (miss === nothing || isempty(miss)) || (warnings[tuid] = miss)
        end
        (; copied, skipped, warnings, remapped)
    end
end
