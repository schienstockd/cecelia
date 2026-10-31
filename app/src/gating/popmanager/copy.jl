# ── Copy a gating strategy between images ─────────────────────────────────────
#
# The package half of `POST /api/gating/copy`, so a task can do it too: the manual gate step
# (`tasks/gate/manual_gate.jl`) prefills each image of a chain run from an exemplar, and a chain may run
# in the detached runner, which never loads `api/src`.

# Serialise gating load→mutate→save. Every writer of a gating sidecar holds it across the whole cycle;
# without it two concurrent edits to one map both load the same tree and the second save drops the
# first. One process-wide lock is enough — gating edits are user-paced and cheap. The API's gating
# handlers take it through `with_popmap_lock` too, so a chain step and a click cannot interleave.
const _POPMAP_LOCK = ReentrantLock()
with_popmap_lock(f) = lock(f, _POPMAP_LOCK)

"""
    copy_gating!(src, targets; value_name, pop_type, replace = true, on_copied = (img, m) -> nothing)
        -> (; copied, skipped, warnings)

Copy `src`'s gating for one label set + gating pop type onto each target, by writing the source's
sidecar there. Membership recomputes per image on read, so gates alone are enough. Per target:

- `skipped[uid]` — no such label set, a µm position gate on an uncalibrated image (it would be read as
  pixels), the write failed, or (`replace = false`) the target already has gating of its own.
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
    with_popmap_lock() do
        m = load_pop_map(src; value_name = vn, pop_type = pt)
        isempty(m.order) && throw(ArgumentError("$(src.uid) has no $pt gating on '$vn' to copy"))
        foreach(path -> stamp_edit!(m, path), m.order)    # in memory only — the source is not re-saved
        copied   = String[]
        skipped  = Dict{String,String}()
        warnings = Dict{String,Vector{@NamedTuple{column::String, pops::Vector{String}}}}()
        for timg in targets
            tuid = timg.uid
            tuid == src.uid && continue
            img_has_value_name(timg, vn) || (skipped[tuid] = "no segmentation '$vn'"; continue)
            if !replace && !isempty(load_pop_map(timg; value_name = vn, pop_type = pt,
                                                 backfill_save = false).order)
                skipped[tuid] = "already gated"; continue
            end
            # A µm POSITION gate needs the target's own pixel size; without one the coordinates would
            # silently be read as pixels (docs/todo/SPATIAL_GATE_UNITS_PLAN.md decision 7).
            (m.spatial_unit == SPATIAL_UNIT_UM && has_spatial_gate(m) && !img_is_calibrated(timg)) &&
                (skipped[tuid] = "no physical pixel size — set it to copy a position gate"; continue)
            try
                save_pop_map!(m, timg)
                on_copied(timg, m)
                push!(copied, tuid)
            catch e
                skipped[tuid] = sprint(showerror, e); continue
            end
            miss = try
                missing_gate_columns(m, timg; value_name = vn)
            catch e
                @warn "gating copy: could not check columns on $tuid" exception = e; nothing
            end
            (miss === nothing || isempty(miss)) || (warnings[tuid] = miss)
        end
        (; copied, skipped, warnings)
    end
end
