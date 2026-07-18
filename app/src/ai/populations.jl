# Population summaries for the read-only observer (Slice B of docs/todo/OBSERVER_DATA_ACCESS_PLAN.md).
# The lineage (Slice A) says an image was "gated" and lists pop names; this gives the actual gate/filter
# DEFINITIONS — "CD3 is a rectangle on channels X/Y", "Directed = clusters.movement in {3}" — so the
# observer can reason about what a population actually means.
#
# READ-ONLY and CHEAP: definitions come straight from the gating sidecar (`gating/{vn}*.json`), no cell
# data read. Membership COUNTS (n cells/tracks per pop) are deliberately NOT here — they require
# applying every gate/filter over the full label table, which is heavy compute the plan forbids on an
# always-on observer read; they belong with the measure slice (C), which already materializes pop_df.

# The gate/pop_type flavours a value_name can carry a gating file for (flow/track = gate-drawn;
# clust/trackclust = cluster pops filtering a `clusters.{suffix}` column).
const _POP_SUMMARY_TYPES = ("flow", "track", "clust", "trackclust")
const _POP_SUMMARY_CAP = 300   # hard cap per image (plan: caps on every list); `truncated` flags a cut

# One population → its definition (names/tree links + the gate geometry or the filter spec). Transient
# napari-selection pops are skipped (never persisted, not part of the analysis).
function _pop_summary(p::Population)
    gate = p.gate === nothing ? nothing : gate_spec(p.gate)
    filt = p.filter_measure === nothing ? nothing :
        (; measure = p.filter_measure, fun = something(p.filter_fun, ""), values = p.filter_values)
    (; path = p.path, name = p.name, parent = p.parent, popType = p.pop_type,
       valueName = p.value_name, colour = p.colour, isTrack = p.is_track, gate = gate, filter = filt)
end

# Every persisted population defined on an image, across its segmentations × pop_type flavours.
function _image_populations(img::CciaImage)
    out = Vector{Any}()
    for v in sort(img_value_names(img)), pt in _POP_SUMMARY_TYPES
        isfile(gating_path(img._dir, v; pop_type = pt)) || continue
        m = load_pop_map(img; value_name = v, pop_type = pt)
        for path in pop_paths(m)
            p = pop_at(m, path)
            p.transient && continue
            push!(out, _pop_summary(p))
            length(out) >= _POP_SUMMARY_CAP && return (out, true)
        end
    end
    (out, false)
end

"""
    populations_summary(proj; image_uid="", set_uid="") -> NamedTuple

Per image, the persisted population definitions (tree + gate/filter specs), scoped to one `image_uid`
or `set_uid` or the whole project. Definitions only — cheap, sidecar-read; membership counts are Slice
C. See OBSERVER_DATA_ACCESS_PLAN.md.
"""
function populations_summary(proj::CciaProject; image_uid::AbstractString = "", set_uid::AbstractString = "")
    imgs = _lineage_images(proj, image_uid, set_uid)
    (; projectUid = proj.uid,
       images = [begin
                     pops, trunc = _image_populations(img)
                     (; uid = img.uid, name = img.name, included = image_included(img),
                        populations = pops, truncated = trunc)
                 end for img in imgs])
end
