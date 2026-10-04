# analysis_runs.jl — the per-RUN delete: list the analysis runs an image carries, delete one.
#
# WHY THIS EXISTS. Between "delete the label set" and "delete all analysis" there was nothing: a
# mistaken tracking run (a whole-segmentation run that should have been `/qc`), a stray HMM column or a
# clustering suffix could only go by wiping the image's analysis. This is the middle scope the
# Manage-images Delete dialog's "Runs" tab offers (docs/todo/IMAGE_DELETE_PLAN.md → Decision 14).
#
# ONE REGISTRY, ONE ROW PER RUN KIND. `RUN_KINDS` is the single list: each kind names the task funs
# that produce it, how to LIST its runs from disk (presence is the record — obs columns, sidecars,
# files; nothing is registered in ccid.json for most of these), and how to DELETE one. The
# `run-kind coverage ratchet` testset holds every registered task fun to either a kind here or a
# reasoned entry in `NOT_RUN_TASKS`, so a new run-producing task cannot ship without a delete.
#
# WHAT A DELETE TAKES. Only what that run wrote — with one exception, tracks, whose delete leaves the
# table exactly as a re-run of that source would (see the `tracks` kind). Never gating: gate and
# population definitions are user work (IMAGE_DELETE_PLAN Decision 13), and a re-run under the same
# name makes them apply again. Never `runlog.json` / `funParams` (history, Decision 8). Derived card
# caches (`analysis/*_cards`) are left: they are stamp-validated against the tables this edits.

"""
One deletable run on one image. Identity is `(kind, key, value_name)`; `value_name` is `""` for a
run that spans segmentations (a joint clustering, an HMM run) or is image-level (a neighbour graph),
in which case `value_names` lists the segmentations it touches on this image.
"""
Base.@kwdef struct AnalysisRun
    kind::String
    key::String
    value_name::String = ""
    label::String
    detail::String = ""
    value_names::Vector{String} = String[]
    invalidates::Vector{String} = String[]   # other runs this delete takes with it ("HMM movement")
end

"""
A run family. `funs` are the task fun_names that produce it (the coverage ratchet's join key);
`list(img) -> Vector{AnalysisRun}`; `delete(img, run; on_log)` removes one run's outputs.
"""
struct RunKind
    kind::String
    label::String
    funs::Vector{String}
    list::Function
    delete::Function
end

# ── shared readers ──────────────────────────────────────────────────────────────────────────────

_run_obs_cols(path::AbstractString)::Vector{String} =
    isfile(path) ? col_names(label_props(path); data_type = :obs) : String[]

_run_segs(img::CciaImage)::Vector{String} = String.(img_value_names(img))

# Group (value_name, key) hits into one run per key that spans every segmentation carrying it.
function _cross_seg_runs(kind::AbstractString, hits::Vector{Tuple{String,String}};
                         label = k -> k)::Vector{AnalysisRun}
    by_key = Dict{String,Vector{String}}()
    order  = String[]
    for (vn, k) in hits
        haskey(by_key, k) || (by_key[k] = String[]; push!(order, k))
        vn in by_key[k] || push!(by_key[k], vn)
    end
    [AnalysisRun(; kind = String(kind), key = k, label = label(k),
                   detail = join(by_key[k], ", "), value_names = by_key[k]) for k in sort(order)]
end

# Suffixes of obs columns `{prefix}{suffix}` on a table.
_obs_suffixes(cols, prefix::AbstractString) =
    [c[ncodeunits(prefix)+1:end] for c in cols if startswith(c, prefix) && ncodeunits(c) > ncodeunits(prefix)]

_rm_if(path::AbstractString; on_log = _ -> nothing) =
    ispath(path) && (on_log("[INFO] Removing: $path"); rm(path; recursive = true); true)

# Drop keys from a JSON manifest sidecar (clustfeatures / motiffeatures) already read by its owner's
# reader (`raw`); remove the file once empty.
function _drop_sidecar_keys!(sidecar::AbstractString, raw::AbstractDict, keys)
    isfile(sidecar) || return
    kept = Dict{String,Any}(String(k) => v for (k, v) in raw if !(String(k) in keys))
    length(kept) == length(raw) && return
    isempty(kept) ? rm(sidecar) : write_json_atomic(sidecar, kept)
end

# ── clustering-family runs (cell clusters, track clusters, regions) ────────────────────────────
# One column family on one table per kind; the run is joint across segmentations. Cell clusters and
# regions share the `X_umap.{suffix}` embedding key on the cell table, so a delete leaves the embedding
# when the OTHER family still has that suffix there.

function _clust_list(kind, img, family, table)
    hits = Tuple{String,String}[]
    for vn in _run_segs(img)
        p = table === :track ? img_track_props_path(img, vn) : img_label_props_path(img, vn)
        for s in _obs_suffixes(_run_obs_cols(p), family * ".")
            push!(hits, (vn, s))
        end
    end
    _cross_seg_runs(kind, hits)
end

function _clust_delete!(img, run, family, table, fun; on_log = _ -> nothing)
    s = run.key
    other = family == "regions" ? "clusters" : "regions"
    for vn in run.value_names
        p = table === :track ? img_track_props_path(img, vn) : img_label_props_path(img, vn)
        isfile(p) || continue
        cols  = _run_obs_cols(p)
        drops = ["$family.$s"]
        # region composition columns `spatial.comp.{pop}.{suffix}` belong to the region run
        family == "regions" && append!(drops, [c for c in cols if startswith(c, "spatial.comp.") && endswith(c, ".$s")])
        shared = "$other.$s" in cols
        on_log("[INFO] $(run.label): dropping $(join(drops, ", ")) from $(basename(p))")
        label_props(p) |> drop_obs(drops) |> drop_obsm(shared ? String[] : ["X_umap.$s"]) |> save!
        # the legacy family-less sidecar key matches every family — only take it when nothing else uses it
        _drop_sidecar_keys!(_clustfeatures_path(p), _clustfeatures_raw(p),
                            shared ? [_clustfeatures_key(s, family)] : [_clustfeatures_key(s, family), s])
        _rm_if(qc_path(img, fun, "$vn.$s"); on_log)
    end
end

# ── tracks: one run per (segmentation, track_source) ───────────────────────────────────────────

function _track_source_label(src::AbstractString, m)
    src == WHOLE_SEG_TRACK_SOURCE && return " (all cells)"
    isempty(src) && return " (unattributed)"
    p = m === nothing ? nothing : pop_path_by_uid(m, src)
    p === nothing ? " (deleted population)" : p
end

function _tracks_list(img)
    out = AnalysisRun[]
    for vn in _run_segs(img)
        "track_id" in _run_obs_cols(img_label_props_path(img, vn)) || continue
        cells = label_track_sources(img, vn)
        isempty(cells) && continue
        n = Dict{String,Int}()
        for src in values(cells)
            k = something(src, "")
            n[k] = get(n, k, 0) + 1
        end
        m = try; load_pop_map(img; value_name = vn, pop_type = "flow"); catch; nothing; end
        for k in sort(collect(keys(n)))
            push!(out, AnalysisRun(; kind = "tracks", key = k, value_name = vn,
                                     label = vn * _track_source_label(k, m),
                                     detail = "$(n[k]) tracked cells", value_names = [vn]))
        end
    end
    out
end

# Deleting a track set IS a re-run of its source with no tracks: the tracker's own lineage merge
# deletes the rows, compacts the surviving ids and invalidates every track-derived `live.*` column
# (speed/angle, HMM states, live contacts). Then the per-track table is rebuilt for what survives —
# the second half of the `bayesian_track_measures` composite — or removed with its sidecar when
# nothing does. Same end state a re-run leaves, so a delete has no rules of its own to drift.
function _tracks_delete!(img, run; on_log = _ -> nothing)
    vn = run.value_name
    props = img_label_props_path(img, vn)
    ok = run_py("tasks/tracking/delete_track_source_run.py",
                (; propsPath = props, trackSource = run.key), task_run_dir(img._dir); on_log = on_log)
    ok || error("deleting track set '$(run.label)' failed — see the log")
    if isempty(label_track_sources(img, vn))
        _rm_if(img_track_props_path(img, vn); on_log)
        _rm_if(_clustfeatures_path(img_track_props_path(img, vn)); on_log)
        return
    end
    last = something(read_module_fun_params(img._dir, "tracking.bayesian_track_measures"; value_name = vn),
                     read_module_fun_params(img._dir, "tracking.track_measures"; value_name = vn),
                     Dict{String,Any}())
    res = _run_task(TrackMeasures(), img,
                    Dict{String,Any}("valueName" => vn, "forceRecompute" => true,
                                     "dims" => string(get(last, "dims", "auto")));
                    on_log = on_log)
    res === nothing && error("track set deleted, but rebuilding track measures for $vn failed — re-run track measures")
end

# ── HMM: one run per colName, across segmentations ─────────────────────────────────────────────

const _HMM_STATE_PREFIX = "live.cell.hmm.state."
const _HMM_TRANS_PREFIX = "live.cell.hmm.transitions."

function _hmm_list(img)
    hits = Tuple{String,String}[]
    for vn in _run_segs(img)
        cols = _run_obs_cols(img_label_props_path(img, vn))
        for c in unique([_obs_suffixes(cols, _HMM_STATE_PREFIX); _obs_suffixes(cols, _HMM_TRANS_PREFIX)])
            push!(hits, (vn, c))
        end
    end
    _cross_seg_runs("hmm", hits)
end

function _hmm_delete!(img, run; on_log = _ -> nothing)
    for vn in run.value_names
        p = img_label_props_path(img, vn)
        isfile(p) || continue
        on_log("[INFO] HMM $(run.key): dropping state + transition columns from $(basename(p))")
        label_props(p) |> drop_obs([_HMM_STATE_PREFIX * run.key, _HMM_TRANS_PREFIX * run.key]) |> save!
    end
end

# ── spatial: neighbour graphs + stats (image-level files), contacts + aggregates (obs) ─────────

_file_runs(kind, suffixes) =
    [AnalysisRun(; kind = kind, key = s, label = s) for s in suffixes]

function _graph_delete!(img, run; on_log = _ -> nothing)
    _rm_if(img_spatial_graph_path(img, run.key); on_log)
    _rm_if(qc_path(img, "spatialAnalysis.cellNeighbours", run.key); on_log)
end

function _stats_delete!(img, run; on_log = _ -> nothing)
    _rm_if(img_stats_path(img, run.key); on_log)
    _rm_if(qc_path(img, "spatialAnalysis.neighbourStats", run.key); on_log)
end

# `{pt}.cell.contact#{target}` (+ min_distance / contact_id) — one run per (segmentation, pt, target).
# Points and mesh contacts share the columns, so one run covers both tasks.
const _CONTACT_RE = r"^([A-Za-z]+)\.cell\.contact#(.+)$"
_contact_cols(pt, t) = ["$pt.cell.contact#$t", "$pt.cell.min_distance#$t", "$pt.cell.contact_id#$t"]

function _contacts_list(img)
    out = AnalysisRun[]
    for vn in _run_segs(img), c in _run_obs_cols(img_label_props_path(img, vn))
        mt = match(_CONTACT_RE, c)
        mt === nothing && continue
        pt, t = String(mt.captures[1]), String(mt.captures[2])
        push!(out, AnalysisRun(; kind = "contacts", key = "$pt#$t", value_name = vn,
                                 label = "$vn → $t", detail = pt, value_names = [vn]))
    end
    out
end

function _contacts_delete!(img, run; on_log = _ -> nothing)
    pt, t = split(run.key, "#"; limit = 2)
    label_props(img_label_props_path(img, run.value_name)) |> drop_obs(_contact_cols(pt, t)) |> save!
    on_log("[INFO] contacts $(run.label): dropped")
end

# `{pt}.cell.is.aggregate` + `.aggregate.id` — singleton per (segmentation, pt). Its auto-created
# `_aggregated` filter pop stays (gating is never deleted — it applies again on a re-run).
function _aggregates_list(img)
    out = AnalysisRun[]
    for vn in _run_segs(img), c in _run_obs_cols(img_label_props_path(img, vn))
        endswith(c, ".cell.is.aggregate") || continue
        pt = String(split(c, "."; limit = 2)[1])
        push!(out, AnalysisRun(; kind = "aggregates", key = pt, value_name = vn,
                                 label = vn, detail = pt, value_names = [vn]))
    end
    out
end

_aggregates_delete!(img, run; on_log = _ -> nothing) =
    label_props(img_label_props_path(img, run.value_name)) |>
        drop_obs(["$(run.key).cell.is.aggregate", "$(run.key).cell.aggregate.id"]) |> save!

# ── motifs: singleton per segmentation ─────────────────────────────────────────────────────────

const _MOTIF_CELL_COLS = [MOTIF_CLASS_COL, MOTIF_DISTANCE_COL, MOTIF_INSTANCE_ID_COL]

_motifs_list(img) =
    [AnalysisRun(; kind = "motifs", key = vn, value_name = vn, label = vn, value_names = [vn])
     for vn in _run_segs(img) if MOTIF_CLASS_COL in _run_obs_cols(img_label_props_path(img, vn))]

function _motifs_delete!(img, run; on_log = _ -> nothing)
    vn = run.value_name
    p  = img_label_props_path(img, vn)
    label_props(p) |> drop_obs(_MOTIF_CELL_COLS) |> save!
    tp = img_track_props_path(img, vn)
    MOTIF_SEQUENCE_COL in _run_obs_cols(tp) && (label_props(tp) |> drop_obs([MOTIF_SEQUENCE_COL]) |> save!)
    sidecar = _motiffeatures_path(p)
    raw = isfile(sidecar) ? (try JSON3.read(read(sidecar, String), Dict{String,Any}) catch; Dict{String,Any}() end) :
                            Dict{String,Any}()
    _drop_sidecar_keys!(sidecar, raw, [vn])
    _rm_if(qc_path(img, "behaviour.motif_discovery", vn); on_log)
end

# ── branching: one run per output value_name (independent of its input segmentation) ───────────

_branching_list(img) =
    [AnalysisRun(; kind = "branching", key = b, value_name = b, label = b, value_names = [b])
     for b in img_branch_value_names(img)]

function _branching_delete!(img, run; on_log = _ -> nothing)
    b = run.key
    entry = get(img.branch_labels, b, nothing)
    if entry !== nothing
        for leaf in version_leaves(entry), fn in (leaf isa AbstractVector ? leaf : [string(leaf)])
            _rm_if(joinpath(img_branch_labels_dir(img), string(fn)); on_log)
        end
    end
    _rm_if(joinpath(img_label_props_dir(img), b * BRANCH_PROPS_SUFFIX * ".h5ad"); on_log)
    _rm_if(qc_path(img, "segment.branching", b); on_log)
    commit_state!(img) do raw
        entries = get(raw, "branch_labels", Dict{String,Any}())
        raw["branch_labels"] = Dict{String,Any}(String(k) => v for (k, v) in entries if string(k) != b)
    end
end

# ── the registry ────────────────────────────────────────────────────────────────────────────────

"""
Every run family, in the order the Delete dialog lists them. Add a kind here when a task starts
producing a new kind of named output; the coverage ratchet fails until its fun is listed here or
in `NOT_RUN_TASKS`.
"""
const RUN_KINDS = RunKind[
    RunKind("tracks", "Tracks",
            ["tracking.bayesian_tracking", "tracking.bayesian_track_measures", "tracking.track_measures"],
            _tracks_list, _tracks_delete!),
    RunKind("hmm", "HMM",
            ["behaviour.hmm", "behaviour.hmm_states", "behaviour.hmm_transitions"],
            _hmm_list, _hmm_delete!),
    RunKind("motifs", "Motifs", ["behaviour.motif_discovery"], _motifs_list, _motifs_delete!),
    RunKind("clusters", "Cell clusters", ["clustPops.cluster"],
            img -> _clust_list("clusters", img, "clusters", :cell),
            (img, run; on_log = _ -> nothing) -> _clust_delete!(img, run, "clusters", :cell, "clustPops.cluster"; on_log)),
    RunKind("trackClusters", "Track clusters", ["clustTracks.cluster"],
            img -> _clust_list("trackClusters", img, "clusters", :track),
            (img, run; on_log = _ -> nothing) -> _clust_delete!(img, run, "clusters", :track, "clustTracks.cluster"; on_log)),
    RunKind("regions", "Regions", ["clustRegions.cluster"],
            img -> _clust_list("regions", img, "regions", :cell),
            (img, run; on_log = _ -> nothing) -> _clust_delete!(img, run, "regions", :cell, "clustRegions.cluster"; on_log)),
    RunKind("graphs", "Neighbour graphs", ["spatialAnalysis.cellNeighbours"],
            img -> _file_runs("graphs", img_spatial_graph_suffixes(img)), _graph_delete!),
    RunKind("stats", "Neighbour stats", ["spatialAnalysis.neighbourStats"],
            img -> _file_runs("stats", img_stats_suffixes(img)), _stats_delete!),
    RunKind("contacts", "Contacts", ["spatialAnalysis.cellContacts", "spatialAnalysis.contactsMeshes"],
            _contacts_list, _contacts_delete!),
    RunKind("aggregates", "Aggregates", ["spatialAnalysis.detectAggregates", "spatialAnalysis.aggregatesMeshes"],
            _aggregates_list, _aggregates_delete!),
    RunKind("branching", "Branching", ["segment.branching"], _branching_list, _branching_delete!),
]

"""
Registered task funs that produce NO deletable run, each with why. The other half of the coverage
ratchet: a fun must be here or in a `RUN_KINDS` entry.
"""
const NOT_RUN_TASKS = Dict{String,String}(
    # new image versions → the dialog's Versions scope
    "cleanupImages.afCorrect" => "image version", "cleanupImages.denoise" => "image version",
    "cleanupImages.driftCorrect" => "image version", "cleanupImages.flowRegister" => "image version",
    "cleanupImages.smooth" => "image version", "cleanupImages.stackAlign" => "image version",
    "editImages.bin" => "image version", "editImages.cropImage" => "new image",
    "editImages.copyImage" => "new image", "editImages.dtype" => "image version",
    "editImages.flip" => "image version", "editImages.register" => "image version",
    "editImages.resampleZ" => "image version", "editImages.tProject" => "image version",
    "editImages.zProject" => "image version",
    "importImages.omezarr" => "image import", "importImages.migrateLegacy" => "image import",
    "importImages.remove" => "deletes, produces nothing",
    "exportImages.ome_tiff" => "writes outside the project",
    # new label sets → the dialog's Label sets scope
    "segment.cellpose" => "label set", "segment.cellposeMeasure" => "label set",
    "segment.coastal" => "label set", "segment.coastalMeasure" => "label set",
    "segment.ridges" => "label set", "segment.measureLabels" => "the label set's base table",
    # in-place corrections — edits, not outputs; undone in the correction cockpit
    "segment.correct" => "in-place label correction", "segment.correct_measures" => "in-place label correction",
    "segment.correct_carryover_snapshot" => "transient correction scratch",
    "segment.correct_carryover_restore" => "transient correction scratch",
    "segment.staleness_report" => "advisory report", "tracking.staleness_report" => "advisory report",
    "tracking.correct" => "in-place track correction", "tracking.correct_measures" => "in-place track correction",
    # models live in the machine-local model vault, not the project
    "opticalFlow.train" => "vault model", "opticalFlow.trainSupportDenoise" => "vault model",
    "testTasks.image_task" => "test task", "testTasks.set_task" => "test task",
    "testTasks.incremental_plot_task" => "test task",
)

_run_kind(kind::AbstractString) =
    something(findfirst(k -> k.kind == kind, RUN_KINDS), 0) |> i -> i == 0 ? nothing : RUN_KINDS[i]

"""
    list_analysis_runs(img) -> Vector{AnalysisRun}

Every deletable run on one image, in `RUN_KINDS` order. A kind whose reader fails (a truncated table)
is skipped with a warning rather than hiding every other kind.
"""
function list_analysis_runs(img::CciaImage)::Vector{AnalysisRun}
    out = AnalysisRun[]
    for k in RUN_KINDS
        try
            append!(out, k.list(img))
        catch e
            @warn "listing $(k.kind) runs failed" image = img.uid exception = (e, catch_backtrace())
        end
    end
    [r.kind == "tracks" ? _with_track_dependents(r, out) : r for r in out]
end

# What a track-set delete takes on its segmentation besides the tracks — the runs built on track
# ids or on the `live.cell.*` measures its lineage merge invalidates (see `_tracks_delete!`). Named
# so the dialog can say so before the confirm: on a joint run it means re-running that run.
function _with_track_dependents(r::AnalysisRun, all_runs::Vector{AnalysisRun})::AnalysisRun
    vn = r.value_name
    deps = String[]
    for o in all_runs
        hit = o.kind in ("hmm", "trackClusters") ? vn in o.value_names :
              o.kind in ("contacts", "aggregates") ? (o.value_name == vn && o.detail == "live") : false
        hit && push!(deps, "$(_run_kind(o.kind).label) $(o.label)")
    end
    AnalysisRun(; kind = r.kind, key = r.key, value_name = r.value_name, label = r.label,
                  detail = r.detail, value_names = r.value_names, invalidates = deps)
end

"""
    delete_analysis_run!(img, kind, key, value_name = ""; on_log) -> Bool

Delete one run if THIS image carries it (`false` when it doesn't — a multi-image selection applies
a run where present and skips the rest, IMAGE_DELETE_PLAN Decision 6). Resolved from the image's own
listing, so a cross-segmentation run takes exactly the segmentations it spans here.
"""
function delete_analysis_run!(img::CciaImage, kind::AbstractString, key::AbstractString,
                              value_name::AbstractString = ""; on_log::Function = _ -> nothing)::Bool
    k = _run_kind(kind)
    k === nothing && throw(ArgumentError("unknown run kind: $kind"))
    runs = k.list(img)
    i = findfirst(r -> r.key == key && r.value_name == value_name, runs)
    i === nothing && return false
    k.delete(img, runs[i]; on_log = on_log)
    true
end
