# correction_staleness.jl — enumerate downstream artefacts made stale by a label/track correction
# (docs/todo/CORRECTION_PLAN.md, P3 / Decision 5).
#
# WHY THIS EXISTS. A correction rewrites labels or tracks; several SEPARATE artefacts derived from
# that row set are then silently stale — the per-track table, cluster runs, gating populations, the
# spatial neighbour graph. Old R shipped them without a peep. Decision 5: "This is the one place
# where Feijoa must beat old R rather than match it." Report, don't auto-delete.
#
# WHAT IT IS. A pure enumerator: given (image, value_name, changed), list what NOW sits on disk that
# was derived from the pre-correction row set. Presence is the signal — we don't attempt to reason
# about which specific rows are affected (a label.merge changes cluster assignments for the merged
# cell; a label.remove drops it entirely from cluster runs; a track.join stitches lineage across two
# clusterings) because "beat old R" is met by *any* surfacing. The task shells around this helper
# bank the finding through `write_qc` (advisory) and mirror the list into a sidecar for the cockpit.
#
# VOCABULARY. Uses the ANALYSIS_KEEP list from `storage.jl:144` as its reverse contract — every dir
# that is NOT in KEEP is derived output, and this helper enumerates the specific classes of it. If a
# new derived class ships, add it here alongside its detection.

using JSON3

const _STALE_KIND_TRACKS         = "tracks_h5ad"
const _STALE_KIND_CLUSTERS       = "cluster_runs"
const _STALE_KIND_GATING         = "gating_pops"
const _STALE_KIND_SPATIAL_GRAPH  = "spatial_graph"

"""
    stale_artefacts_for(img, value_name; changed) -> Vector{Dict{String,Any}}

Enumerate downstream artefacts that were derived from the pre-correction row set for `value_name`.
`changed` is `:labels` (a `segment.correct` run) or `:tracks` (a `tracking.correct` run).

Each element carries `kind` (a stable slug — one of `_STALE_KIND_*`), a repo-relative `path` (for
the cockpit's inline list), and `detail` with a count when the class supports one. The list is
ordered coarsest → finest so the QC finding's `detail.kinds` reads naturally.

`:labels` triggers every class (a label edit invalidates trackings on those labels, cluster runs on
those labels, gating pops keyed on labels, and the spatial neighbour graph over labels). `:tracks`
is narrower: the per-cell tables, per-label gates and the spatial graph are unchanged; only track-
derived artefacts (`{vn}__tracks.h5ad`, trackclust runs, `__tracks` gating pops) are stale.

Detection is disk-presence — if the file exists, it's on the list. See the file-level comment for
why we deliberately don't try to reason about per-row impact.
"""
function stale_artefacts_for(img::CciaImage, value_name::AbstractString;
                             changed::Symbol)::Vector{Dict{String,Any}}
    changed in (:labels, :tracks) ||
        throw(ArgumentError("stale_artefacts_for: `changed` must be :labels or :tracks, got $changed"))
    vn = String(value_name)
    out = Dict{String,Any}[]

    tracks_path = img_track_props_path(img, vn)
    if isfile(tracks_path)
        push!(out, _stale_entry(_STALE_KIND_TRACKS, tracks_path, img._dir;
                                summary = "per-track table ($(vn)__tracks.h5ad)"))
    end

    # Cluster runs live inside the CELL table (obs cols starting `clusters.` — see
    # `image.jl:161` img_cluster_suffixes). A track-only correction leaves per-cell clustering alone,
    # so this class only fires on :labels — the trackclust analogue on the tracks h5ad is folded into
    # the tracks entry above (dropping that file makes trackclust runs unreachable).
    if changed === :labels
        cluster_sfxs = try
            img_cluster_suffixes(img, vn; family = "clusters")
        catch
            String[]
        end
        if !isempty(cluster_sfxs)
            push!(out, _stale_entry(_STALE_KIND_CLUSTERS, img_label_props_path(img, vn), img._dir;
                                    summary = "$(length(cluster_sfxs)) cluster run(s) on $(vn)",
                                    detail  = Dict{String,Any}("suffixes" => cluster_sfxs,
                                                               "count"    => length(cluster_sfxs))))
        end
    end

    # Gating: flow pops (`gating/{vn}.json`) are label-keyed → stale on :labels; track pops
    # (`gating/{vn}__tracks.json`) are track_id-keyed → stale on :tracks.
    for (pop_type, applies) in (("flow", changed === :labels), ("track", changed === :tracks))
        applies || continue
        gpath = gating_path(img._dir, vn; pop_type = pop_type)
        isfile(gpath) || continue
        n_pops = _gating_pop_count(gpath)
        push!(out, _stale_entry(_STALE_KIND_GATING, gpath, img._dir;
                                summary = "$(n_pops) $(pop_type) gating pop(s) on $(vn)",
                                detail  = Dict{String,Any}("pop_type" => pop_type,
                                                           "count"    => n_pops)))
    end

    # Spatial neighbour graphs are POP-AGNOSTIC and pool across segmentations (`image.jl:125`), so
    # they aren't keyed by value_name. A `:labels` change to ANY segmentation that fed a graph
    # invalidates it; we can't cheaply verify which graphs did — list all present and let the user
    # decide. `:tracks` leaves the graph alone (label positions unchanged).
    if changed === :labels
        for suffix in img_spatial_graph_suffixes(img)
            gpath = img_spatial_graph_path(img, suffix)
            push!(out, _stale_entry(_STALE_KIND_SPATIAL_GRAPH, gpath, img._dir;
                                    summary = "spatial neighbour graph ($(suffix))",
                                    detail  = Dict{String,Any}("suffix" => suffix)))
        end
    end

    out
end

# One entry shape — one place that keys it, so a rename doesn't drift between the helper and the
# task shells that write the sidecar/QC.
function _stale_entry(kind::AbstractString, abs_path::AbstractString, task_dir::AbstractString;
                      summary::AbstractString, detail = nothing)::Dict{String,Any}
    rel = try
        relpath(abs_path, task_dir)
    catch
        abs_path
    end
    e = Dict{String,Any}("kind" => String(kind), "path" => String(rel), "summary" => String(summary))
    isnothing(detail) || (e["detail"] = detail)
    e
end

# Count leaf pops in a gating sidecar without instantiating the PopulationMap struct — the map's own
# constructor runs UID backfill + a save (`load_pop_map`), which the enumerator MUST NOT trigger on a
# read-only staleness scan. Falls back to 0 on any parse error (the report is advisory; a truncated
# gating file is a separate issue). The tree layout is `pops: {path: Population}` (see
# `population_manager.jl:96`).
function _gating_pop_count(gating_json::AbstractString)::Int
    try
        doc = JSON3.read(read(gating_json, String), Dict{String,Any})
        pops = get(doc, "pops", nothing)
        pops isa AbstractDict ? length(pops) : 0
    catch
        0
    end
end

# ── Sidecar path ───────────────────────────────────────────────────────────────
#
# One durable location for the enumerated set — peer to `corrections/labels_{vn}.json` and
# `corrections/tracks_{vn}.json` (`label_correction.jl` / `track_correction.jl`), so a chain reader
# or the cockpit finds it without a new dir. Overwritten on each report (previous run's staleness is
# no longer authoritative once a new correction has run — the QC finding is the durable history).
staleness_sidecar_path(task_dir::AbstractString, value_name::AbstractString) =
    joinpath(task_dir, "corrections", "$(value_name).staleness.json")

# ── Task shells ────────────────────────────────────────────────────────────────
#
# Two typed tasks so each composite invokes its own `changed` scope (the composite executor threads
# ONE params dict through every step — a shared task keyed by a `stalenessScope` param would work
# but adds a param whose only correct values are set by the composite itself, which is worse UX than
# a task per scope). Both share `_run_task_staleness_report` below.

struct SegmentStalenessReport <: CciaTask end
struct TrackingStalenessReport <: CciaTask end

_staleness_changed(::SegmentStalenessReport)  = :labels
_staleness_changed(::TrackingStalenessReport) = :tracks

_staleness_fun_name(::SegmentStalenessReport)  = "segment.staleness_report"
_staleness_fun_name(::TrackingStalenessReport) = "tracking.staleness_report"

function _run_task(task::Union{SegmentStalenessReport,TrackingStalenessReport},
                   img::CciaImage, params::Dict{String,Any};
                   on_log::Function      = line -> println(line),
                   on_progress::Function = (n, t) -> nothing,
                   on_process::Function  = _ -> nothing)
    _run_task_staleness_report(task, img, params; on_log = on_log, on_progress = on_progress)
end

function _run_task_staleness_report(task, img::CciaImage, params::Dict{String,Any};
                                    on_log::Function, on_progress::Function)
    value_name = string(get(params, "valueName", VERSIONED_DEFAULT_VAL))
    changed    = _staleness_changed(task)
    fun_name   = _staleness_fun_name(task)

    on_progress(1, 2)
    artefacts = stale_artefacts_for(img, value_name; changed = changed)
    on_progress(2, 2)

    # Sidecar: one file, always written — the empty case is meaningful ("we checked and nothing was
    # stale"), and overwriting a previous non-empty report on a subsequent clean run stops stale
    # findings from lingering.
    sidecar = staleness_sidecar_path(img._dir, value_name)
    mkpath(dirname(sidecar))
    write_json_atomic(sidecar, Dict{String,Any}(
        "valueName" => value_name,
        "changed"   => String(changed),
        "artefacts" => artefacts,
    ))

    # QC: one `warn` finding when there IS something stale — advisory, no gating. When the list is
    # empty we still call `write_qc` with an empty findings vector so a previous warning clears on a
    # follow-up run (`qc.jl:328` explicitly documents that shape).
    findings = _staleness_findings(artefacts, changed)
    metrics  = Dict{String,Any}("nArtefacts" => length(artefacts),
                                "byKind"     => _count_by_kind(artefacts))
    try
        write_qc(img, fun_name, value_name, findings; metrics = metrics)
    catch e
        on_log("[QC] could not write staleness QC: $e")
    end

    if isempty(artefacts)
        on_log("[INFO] No stale downstream artefacts for $value_name.")
    else
        on_log("[WARN] $(length(artefacts)) downstream artefact(s) now predate this correction:")
        for a in artefacts
            on_log("       - $(a["summary"]) — $(a["path"])")
        end
        on_log("[WARN] Re-run the affected tasks (see the QC finding on $fun_name).")
    end

    Dict{String,Any}("valueName"   => value_name,
                     "changed"     => String(changed),
                     "nArtefacts"  => length(artefacts),
                     "sidecarPath" => sidecar)
end

function _staleness_findings(artefacts::Vector{Dict{String,Any}}, changed::Symbol)
    isempty(artefacts) && return Vector{Dict{String,Any}}()
    kinds = unique(String[a["kind"] for a in artefacts])
    scope = changed === :labels ? "labels" : "tracks"
    [qc_finding("warn", "correction.stale_artefacts";
                detail = Dict{String,Any}("changed"   => scope,
                                          "artefacts" => artefacts,
                                          "kinds"     => kinds),
                n = length(artefacts), scope = scope)]
end

function _count_by_kind(artefacts::Vector{Dict{String,Any}})::Dict{String,Int}
    counts = Dict{String,Int}()
    for a in artefacts
        k = String(a["kind"])
        counts[k] = get(counts, k, 0) + 1
    end
    counts
end
