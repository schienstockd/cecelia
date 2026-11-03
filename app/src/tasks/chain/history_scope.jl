# ── Board scope ─────────────────────────────────────────────────────────────────────────────────────
#
# "What did you use for this plot?" — the same rows, cut to the lineage of what a board shows. Seeds:
# the steps that wrote the label sets its populations sit on, the cluster runs it draws, and the steps
# run on those label sets that write no named output (tracking, behaviour). Another clustering, region
# run or label set made FROM a shown label set is a different result, so it only counts when shown; a
# step run on populations counts when one of them is a shown population or above/below one (tracking
# `/qc/cells` is what a `/qc/cells/_tracked` plot drew; tracking `/qc/frags` is not). Then everything
# upstream of a seed, wired as the chain wires (`_reads_output`). Each row says which: `made` what is
# shown, or only `ran on` the shown cells (see `_history_scope`). Imports stay: every image starts
# there. A gated population has no run of its own — its label set's steps are the answer.

# a cluster pop type → the task that writes its column (the column family itself is
# `_cluster_measure_prefix`'s; `RUN_KINDS` maps run kinds, not pop types, to tasks)
const _HP_CLUSTER_TASK = Dict("clust" => "clustPops.", "trackclust" => "clustTracks.", "region" => "clustRegions.")
const _HP_RUN_NS = ("clusters", "regions", "rois", "labels", "models", "spatialGraphs", "stats")

"""
Cluster runs `(namespace, name, task prefix)` the board draws: each cluster population's own and its
parents' `filter_measure` (`clusters.movement` — a cluster pop is a filter on a run's column, read from
the image's gating sidecar), plus the board's explicit `"popType:suffix"` runs.
"""
function _hp_cluster_runs(proj::CciaProject, image_uids, tkeys, explicit::Vector{String})
    out = Set{Tuple{String,String,String}}()
    for r in explicit
        i = findlast(':', r)
        pt, suffix = i === nothing ? ("", r) : (r[1:i-1], r[i+1:end])
        (_is_cluster_pop_type(pt) && !isempty(suffix)) || continue
        push!(out, (_cluster_measure_family(pt), suffix, _HP_CLUSTER_TASK[pt]))
    end
    imgs = [im for im in (init_object(proj.uid, u) for u in image_uids) if im isa CciaImage]
    for t in tkeys, img in imgs
        (isempty(t.pop) || isempty(t.valueName) || !_is_cluster_pop_type(t.popType)) && continue
        pt = t.popType
        prefix = _cluster_measure_prefix(pt)
        isfile(gating_path(img._dir, t.valueName; pop_type = pt)) || continue
        m = try load_pop_map(img._dir, t.valueName; pop_type = pt, backfill_save = false) catch; continue end
        p = t.pop
        while (pop = get(m.pops, p, nothing)) !== nothing
            fm = something(pop.filter_measure, "")
            startswith(fm, prefix) && push!(out, (_cluster_measure_family(pt), fm[ncodeunits(prefix)+1:end],
                                                  _HP_CLUSTER_TASK[pt]))
            p = pop.parent
        end
    end
    out
end

# Does a step run on what is shown? A step that picks populations on a shown label set: when one is
# related to a shown population. Otherwise: when a label-set picker names a shown label set (tracking
# names its label set AND its populations — the populations decide).
function _hp_on_shown(params, lv, shown::Dict{String,Vector{String}})::Bool
    own = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    near(a, b) = isempty(a) || isempty(b) || startswith(a * "/", b * "/") || startswith(b * "/", a * "/")
    picked = false; by_name = false
    for (k, str) in _keyed_strings(params)
        l = get(lv, k, nothing)
        (l === nothing || l.writes) && continue
        if l.type == "popSelection"
            ls, path = _split_pop_ref(str, own)
            haskey(shown, ls) || continue
            picked = true
            any(p -> near(rstrip(path, '/'), rstrip(p, '/')), shown[ls]) && return true
        elseif l.ns == "labels"
            by_name |= !isempty(get(shown, str, String[]))
        end
    end
    !picked && by_name
end

"""
`groups` index → relation, for the lineage of the `shown` label sets + `runs` (see the section
comment): `"made"` = wrote what is shown, or fed a step that did; `"ran on"` = ran on the shown cells
(and its own inputs). No task declares the measures it writes, so which "ran on" step a given plot
reads is not knowable here — the column says the weaker thing instead of guessing. Also returns
what is shown that no step wrote, as `(what, task prefix)`.
"""
function _history_scope(groups, leaves_of, shown::Dict{String,Vector{String}}, runs)
    nodes = map(groups) do ((fn, _), by_img, _, _)
        lr = argmax(r -> r.at, collect(values(by_img)))
        out, ns = _history_output(fn, lr.params, leaves_of(fn))
        (; fn, params = lr.params, at = lr.at, out, ns, lv = something(leaves_of(fn), Dict{String,_HistoryLeaf}()))
    end
    # the chain's edge rule, except that the raw `default` version counts: the table keeps the import
    reads(b, a) = a != b && !isempty(a.out) && a.at <= b.at && _reads_output(b.params, b.lv, a.out, a.ns)
    made(s) = startswith(s.fn, "importImages.") ||
              (s.ns == "labels" && haskey(shown, s.out)) ||
              any(r -> s.ns == r[1] && s.out == r[2] && startswith(s.fn, r[3]), runs)
    ran_on(s) = !(s.ns in _HP_RUN_NS) && _hp_on_shown(s.params, s.lv, shown)
    out = Dict{Int,String}()
    for (relation, seed) in (("made", made), ("ran on", ran_on))   # "made" wins where both reach
        stack = [i for (i, s) in enumerate(nodes) if seed(s)]
        while !isempty(stack)
            i = pop!(stack)
            haskey(out, i) && continue
            out[i] = relation
            append!(stack, [j for (j, a) in enumerate(nodes) if reads(nodes[i], a)])
        end
    end
    unmade = Tuple{String,String}[]           # (what, task prefix) the board shows that no node wrote
    for ls in sort!(collect(keys(shown)))
        any(s -> s.ns == "labels" && s.out == ls, nodes) || push!(unmade, ("label set $ls", "segment."))
    end
    for r in sort!(collect(runs))
        any(s -> s.ns == r[1] && s.out == r[2] && startswith(s.fn, r[3]), nodes) ||
            push!(unmade, ("$(r[1]).$(r[2])", r[3]))
    end
    # an image version a kept step read that no step wrote (the raw `default` is the import's)
    versions = Set{String}()
    for i in keys(out), (k, v) in _keyed_strings(nodes[i].params)
        l = get(nodes[i].lv, k, nothing)
        (l !== nothing && l.type == "valueNameSelection" && isempty(l.ns) && !l.writes) || continue
        (v == VERSIONED_DEFAULT_VAL || any(s -> isempty(s.ns) && s.out == v, nodes)) || push!(versions, v)
    end
    append!(unmade, [("image version $v", "") for v in sort!(collect(versions))])
    out, unmade
end
