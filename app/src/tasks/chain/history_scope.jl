# ── Board scope ─────────────────────────────────────────────────────────────────────────────────────
#
# "What did you use for this plot?" — the same rows, cut to the lineage of what a board shows. Seeds:
# the steps that wrote the label sets its populations sit on, the cluster runs it draws, and the steps
# run on those label sets that write no named output (tracking, behaviour). Another clustering, region
# run or label set made FROM a shown label set is a different result, so it only counts when shown; a
# step run on populations counts when one of them is a shown population or above/below one (tracking
# `/qc/cells` is what a `/qc/cells/_tracked` plot drew; tracking `/qc/frags` is not). Then everything
# upstream of a seed, wired as the chain wires (`_reads_made`: named outputs, derived populations,
# declared columns). Each row says which: `made` what is
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
        m = try load_pop_tree(img._dir, t.valueName; pop_type = pt, backfill_save = false) catch; continue end
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
            _whole_pop(str) && continue        # an older run's "NONE": the whole segmentation, as `[]` is
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
(and its own inputs). Only some tasks declare the columns they write (`producesColumns`), so which
"ran on" step a given plot reads is not knowable here — the column says the weaker thing instead of
guessing; the upstream walk does follow a declared column (clustering → the HMM it read). Also returns
what is shown that no step wrote, as `(what, task prefix)`.
"""
function _history_scope(groups, leaves_of, shown::Dict{String,Vector{String}}, runs)
    nodes = _hs_nodes(groups, leaves_of)
    # the chain's edge rule, except that the raw `default` version counts: the table keeps the import
    reads = _hs_reads
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

# One node per table group: the latest run's params (by current key, `formerKeys`, as
# `chain_from_history`'s steps are), what it writes and derives, and when it ran on each image.
function _hs_nodes(groups, leaves_of)
    map(groups) do g
        (fn, _), by_img = g[1], g[2]
        lr = argmax(r -> r.at, collect(values(by_img)))
        out, ns = _history_output(fn, lr.params, leaves_of(fn))
        lv = something(leaves_of(fn), Dict{String,_HistoryLeaf}())
        columns, pops = _history_products(lr.params, lv)
        (; fn, params = _history_current_params(lr.params, lv), at = lr.at, out, ns, lv,
           output = out, output_ns = ns, columns, pops,
           imgs = Dict{String,String}(u => r.at for (u, r) in by_img),
           # every image's params: images of one step may read different inputs (one segmented on
           # its drift-corrected version, the rest on raw)
           inputs = unique([_history_current_params(r.params, lv) for r in values(by_img)]))
    end
end

# B reads A: A ran first, and B's run on some image names what A made
_hs_reads(b, a) = a !== b && a.at <= b.at && any(p -> _reads_made(p, b.lv, a), b.inputs)

# ── Set scope: what the analysis moved past ─────────────────────────────────────────────────────────
#
# A set's history holds the analysis AND the tries on the way to it. A Methods section states the
# first; the tries stay in the parameter table. Two rules, both read from the record, no board needed:
#
#   redone — a later run of the same task re-made everything this one made, on every image it ran on
#            (tracking three populations one at a time on one image, then all three on every image).
#            Not an input of anything after it: the later run's output is what was read.
#   trial  — not part of the analysis: nothing that is reads it, and either it ran on only part of the
#            set, or it is one version of a sweep whose sibling something did read (`chain_from_history`'s
#            `siblingRead`: the clustering read HMM movement_3, so movement_2/4/5/6 were tries).
#
# The analysis = every full-set step that is not a passed-over sweep version, every step that made a
# population still defined on the images (`runs`, the cluster runs current populations filter on — a
# region run on 9 of 16 images that defines regions 1–3 is the analysis), and everything upstream of
# those, over edges into a redone step dropped.

"""
    _history_usage(groups, leaves_of, n_images, runs) -> Dict{Int,String}

`groups` index → `"redone"` | `"trial"` for the steps a set's Methods leaves out; a step not in the
result is part of the analysis. `runs` = `(namespace, name, task prefix)` of the cluster runs the
images' current populations filter on (`_hp_cluster_runs`). See the section comment.
"""
function _history_usage(groups, leaves_of, n_images::Int, runs)::Dict{Int,String}
    nodes = _hs_nodes(groups, leaves_of)
    made_set(s) = Set{Any}(vcat(s.columns, s.pops, isempty(s.out) ? Any[] : Any[(s.ns, s.out)]))
    redone = Set{Int}()
    for (i, a) in enumerate(nodes)
        pa = made_set(a)
        isempty(pa) && continue
        any(nodes) do b
            b !== a && b.fn == a.fn && issubset(pa, made_set(b)) &&
                all(((u, t),) -> haskey(b.imgs, u) && b.imgs[u] > t, a.imgs)
        end && push!(redone, i)
    end
    # a redone A fed nothing after its redo
    reads = _hs_reads
    read_by = [Int[j for (j, b) in enumerate(nodes) if reads(b, a)] for a in nodes]
    sibling(s) = (s.fn, _history_inputs(s.params, s.lv))
    read_sweeps = Set(sibling(nodes[i]) for i in eachindex(nodes) if !isempty(read_by[i]))
    final_try(i) = isempty(read_by[i]) && sibling(nodes[i]) in read_sweeps
    makes_pop(s) = any(r -> s.ns == r[1] && s.out == r[2] && startswith(s.fn, r[3]), runs)
    seed(i) = !(i in redone) && ((length(nodes[i].imgs) >= n_images && !final_try(i)) || makes_pop(nodes[i]))
    used = Set{Int}()
    stack = [i for i in eachindex(nodes) if seed(i)]
    while !isempty(stack)
        i = pop!(stack)
        i in used && continue
        push!(used, i)
        append!(stack, [j for (j, a) in enumerate(nodes) if !(j in redone) && reads(nodes[i], a)])
    end
    Dict{Int,String}(i => (i in redone ? "redone" : "trial") for i in eachindex(nodes) if !(i in used))
end
