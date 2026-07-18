# Shared scaffold for the read-only observer's per-image summary tools (Slices A–E of
# docs/todo/OBSERVER_DATA_ACCESS_PLAN.md). Every summary tool has the SAME shape: scope to one image,
# one set, or the whole project, then map a per-image builder into `{projectUid, images: [...]}`. This
# holds that shape once so each tool (lineage, populations, and the measure/behaviour/board slices to
# come) is just its per-image builder — not another copy of the scope/scaffold plumbing. The matching
# API-route and MCP-client scaffolds live in `api/src/routes.jl` (`_observer_summary_route`) and
# `mcp/cecelia_mcp/client.py` (`_analysis_summary`).

# The images a summary call covers: one image, one set, or (default) the whole project.
function _observer_scope_images(proj::CciaProject, image_uid::AbstractString, set_uid::AbstractString)
    if !isempty(image_uid)
        i = findfirst(im -> im.uid == image_uid, images(proj))
        return isnothing(i) ? CciaImage[] : [images(proj)[i]]
    elseif !isempty(set_uid)
        s = findfirst(st -> st.uid == set_uid, sets(proj))
        return isnothing(s) ? CciaImage[] : images(sets(proj)[s])
    end
    images(proj)
end

# The identity header every per-image summary entry starts with, so builders don't each re-spell it.
_observer_image_header(img::CciaImage) = (; uid = img.uid, name = img.name, included = image_included(img))

# The gate/pop_type flavours a value_name can carry a persisted gating file for (flow/track = gate-drawn;
# clust/trackclust = cluster pops). "live" derived pops (e.g. _tracked) are NOT persisted maps — they're
# addressed directly through pop_df, not enumerated here.
const _OBSERVER_POP_TYPES = ("flow", "track", "clust", "trackclust")

# Iterate every persisted (non-transient) population on an image — `f(pop)` per pop, across the image's
# segmentations × pop_type flavours. The shared enumerator for the population-based summary tools
# (definitions AND measures), so the "walk the gating maps" loop lives once.
function _observer_each_population(f::Function, img::CciaImage)
    for v in sort(img_value_names(img)), pt in _OBSERVER_POP_TYPES
        isfile(gating_path(img._dir, v; pop_type = pt)) || continue
        m = load_pop_map(img; value_name = v, pop_type = pt)
        for path in pop_paths(m)
            p = pop_at(m, path); p.transient && continue
            f(p)
        end
    end
end

# Map a per-image builder over the scope → `{projectUid, images}`. A tool that needs project-level
# fields too (e.g. lineage's chains/boards/rollup) merges onto this base.
observer_image_summary(proj::CciaProject, per_image::Function;
                       image_uid::AbstractString = "", set_uid::AbstractString = "") =
    (; projectUid = proj.uid,
       images = [per_image(img) for img in _observer_scope_images(proj, image_uid, set_uid)])
