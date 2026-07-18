# Population summaries for the read-only observer (Slice B of docs/todo/OBSERVER_DATA_ACCESS_PLAN.md).
# The lineage (Slice A) says an image was "gated" and lists pop names; this gives the actual gate/filter
# DEFINITIONS — "CD3 is a rectangle on channels X/Y", "Directed = clusters.movement in {3}" — so the
# observer can reason about what a population actually means.
#
# READ-ONLY and CHEAP: definitions come straight from the gating sidecar (`gating/{vn}*.json`), no cell
# data read. Membership COUNTS (n cells/tracks per pop) are deliberately NOT here — they require
# applying every gate/filter over the full label table, which is heavy compute the plan forbids on an
# always-on observer read; they belong with the measure slice (C), which already materializes pop_df.

const _POP_SUMMARY_CAP = 300   # hard cap per image (plan: caps on every list); `truncated` flags a cut

# One population → its definition (names/tree links + the gate geometry or the filter spec).
function _pop_summary(p::Population)
    gate = p.gate === nothing ? nothing : gate_spec(p.gate)
    filt = p.filter_measure === nothing ? nothing :
        (; measure = p.filter_measure, fun = something(p.filter_fun, ""), values = p.filter_values)
    (; path = p.path, name = p.name, parent = p.parent, popType = p.pop_type,
       valueName = p.value_name, colour = p.colour, isTrack = p.is_track, gate = gate, filter = filt)
end

# Every persisted population defined on an image (via the shared enumerator), capped.
function _image_populations(img::CciaImage)
    out = Vector{Any}(); truncated = false
    _observer_each_population(img) do p
        length(out) >= _POP_SUMMARY_CAP ? (truncated = true) : push!(out, _pop_summary(p))
    end
    (out, truncated)
end

# Per-image builder: the identity header + its population definitions (+ a cap flag).
function _population_image(img::CciaImage)
    pops, trunc = _image_populations(img)
    (; _observer_image_header(img)..., populations = pops, truncated = trunc)
end

"""
    populations_summary(proj; image_uid="", set_uid="") -> NamedTuple

Per image, the persisted population definitions (tree + gate/filter specs), scoped to one `image_uid`
or `set_uid` or the whole project. Definitions only — cheap, sidecar-read; membership counts are Slice
C. See OBSERVER_DATA_ACCESS_PLAN.md.
"""
populations_summary(proj::CciaProject; image_uid::AbstractString = "", set_uid::AbstractString = "") =
    observer_image_summary(proj, _population_image; image_uid = image_uid, set_uid = set_uid)
