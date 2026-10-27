# ── Contact targets — which B populations each contact column set is measured against ─────────────
#
# Shared by both contact routes (spatialAnalysis.cellContacts points, spatialAnalysis.contactsMeshes
# mesh). One column set PER TARGET (#1734): several picked B populations give one distance each, not
# one distance to the nearest of any. Targets group by population PATH with the segmentation prefix
# stripped, so one population split across segmentations (`B/T`, `C/T`) still pools into one set.
# `combineTargets` keeps the pooled form — one set for every B pop together — as an opt-in.

# The mesh route's optional containment columns, `{pt}.cell.{kind}#{target}` beside the distances
# (contactsMeshes `checkContained`). Both routes write the same column set, so a run that does not
# measure containment drops these rather than leave an earlier run's beside its fresh distances.
const CONTAINMENT_COLS = ("contained_by", "contains_n")

_contact_target(pop_type_b, popsB) =
    replace(string(pop_type_b, ".", join(sort(collect(String, popsB)), "+")), "/" => "_")

"""
    _contact_target_groups(popsB; combine=false) -> Vector{Vector{String}}

The B refs each contact column set is computed against, in first-picked order. Default: one group per
population PATH with the segmentation prefix stripped — `["B/T", "B/DC"]` → two groups, while the same
population picked from two segmentations (`["B/T", "C/T"]`) stays ONE group and pools (the "may span
segmentations" case). `combine = true` → one group of every ref (the pooled, nearest-of-any form).
Shared by the points (cellContacts) and mesh (contactsMeshes) routes.
"""
function _contact_target_groups(popsB; combine::Bool = false)::Vector{Vector{String}}
    refs = unique(collect(String, popsB))
    (combine || isempty(refs)) && return [refs]
    groups = Vector{String}[]; idx = Dict{String,Int}()
    for r in refs
        path = _split_pop_ref(r, "")[2]
        haskey(idx, path) ? push!(groups[idx[path]], r) : (push!(groups, [r]); idx[path] = length(groups))
    end
    groups
end

# A per-target run that leaves an older COMBINED column set of the same targets beside it: name it in
# the log so a reader seeing both `#flow.B_T` and `#flow.B_DC+B_T` knows the latter is the earlier
# pooled run (kept — it is a different measure, and deleting is the Analysis runs page's job).
function _log_stale_combined(on_log, props_path, pop_type, pop_type_b, popsB, groups)
    length(groups) > 1 || return
    pooled = "$(pop_type).cell.contact#$(_contact_target(pop_type_b, popsB))"
    pooled in col_names(label_props(props_path); data_type = :obs) &&
        on_log("[INFO] earlier combined columns `$(pooled)` (+ min_distance/contact_id) kept; " *
               "delete them under Analysis runs if not wanted")
end
