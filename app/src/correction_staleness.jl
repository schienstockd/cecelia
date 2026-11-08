# correction_staleness.jl — enumerate downstream artefacts made stale by a label/track correction
# (docs/todo/CORRECTION_PLAN.md, P3 / Decision 5), and — at the bottom — outputs made stale by an
# upstream re-run, read from the run log's `inputs` / `replaces` (`stale_outputs_for`).
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
const _STALE_KIND_CELL_CARDS     = "cell_cards"
const _STALE_KIND_ROI_SETS       = "roi_sets"

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

    # Cell-cards sidecars (`analysis/cell_cards/{vn}__{suffix}.json`) depend on trackclust cluster
    # membership + the medoid track's centroid — either a `:labels` change (cluster runs may rerun)
    # or a `:tracks` change (medoid track ids may shift, per-track centroids move) invalidates them.
    # See docs/todo/CELL_CARDS_PLAN.md Decision 8. Report presence; the route rebuilds on next call.
    cc_dir = joinpath(img._dir, "analysis", "cell_cards")
    if isdir(cc_dir)
        # Any sidecar named `{vn}__*.json` under this image is derived from this value_name's cluster
        # runs. Match on the vn-prefix so a cell-cards sidecar for a sibling value_name isn't listed.
        prefix = "$(vn)__"
        for f in readdir(cc_dir; join = true)
            (endswith(f, ".json") && startswith(basename(f), prefix)) || continue
            push!(out, _stale_entry(_STALE_KIND_CELL_CARDS, f, img._dir;
                                    summary = "cell-cards ($(basename(f)))"))
        end
    end

    # ROI sets (`rois/{suffix}.json`, ROI_TRIAGE_PLAN) score ONE segmentation's objects — the set
    # records it as `valueName` — so a `:labels` change to that segmentation leaves its boxes and object
    # tables describing labels that may no longer exist. The REVIEWS are user work and are not listed:
    # a re-run keeps those whose ROI id recurs and reports the rest as orphaned.
    if changed === :labels
        for suffix in roi_suffixes_on(img, vn)
            push!(out, _stale_entry(_STALE_KIND_ROI_SETS, img_rois_path(img, suffix), img._dir;
                                    summary = "ROI set ($(suffix))",
                                    detail  = Dict{String,Any}("suffix" => suffix)))
        end
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

# ── Run-record staleness: an output whose producing run read an input since replaced ──────────────────
#
# The second staleness question, answered from the run log instead of disk presence. `run_provenance`
# stamps each run's `inputs` (name + version read) and `replaces` (the output it overwrote); a re-run
# upstream therefore leaves, in the record, a `replaces` naming what a downstream run read. Here the
# downstream output is the one whose CURRENT maker (the newest `done` run that wrote it) read an input a
# later `done` run replaced — directly, or through an upstream run that is itself stale. A run with no
# `inputs` for a task that reads names was logged before inputs were recorded: its output, and anything
# made from it, is `unknown`, never guessed stale or clean — as is a read whose version has several
# possible makers that disagree (see `upstream`). A `replaces` with a version matches a read of that version only, so re-running
# `default` v2 does not stale a step pinned to v1. Report, don't auto-delete — as for corrections.

# ("name", "ns") of one `{name, ns?, version?}` record, and its version ("" = not recorded)
_stale_ref(d) = (_rl_str(d, "name"), _rl_str(d, "ns"))
_stale_ver(d) = _rl_str(d, "version")

# the run an entry is, as the chain names it
_stale_run(e) = Dict{String,Any}("fun" => _rl_str(e, "fun"), "taskId" => _rl_str(e, "taskId"), "at" => _rl_str(e, "at"))

# Does the task read any name? Only then can missing `inputs` mean "not recorded" rather than "none".
_stale_reads_names(leaves) =
    leaves !== nothing && any(l -> l.type == "valueNameSelection" && !l.writes, values(leaves))

"""
    stale_outputs_for(img) -> (; stale, unknown)

Outputs on `img` whose producing run read an input that has since been replaced, from the run log
(archive included). `stale` = `[{name, ns?, into?, run: {fun, taskId, at}, because: [link…], summary}]`,
where each link is `{read: {name, ns?, version?}, replacedBy: {fun, taskId, at}}` (the input was
overwritten by that later run) or `{read, madeBy: {fun, taskId, at}}` (the input's maker is itself
stale; the next link says why). `unknown` = `[{name, ns?, into?, run, reason}]` — the maker, or the
maker of something it read, predates recorded inputs, or the version it read has several possible
makers (several logged writers, or a pinned version its one logged writer may not have made) that
disagree. An output no longer on the image is left out. Read-only.
"""
function stale_outputs_for(img::CciaImage)
    entries = [e for e in read_run_log_history(img) if e isa AbstractDict]
    leaves  = Dict{String,Any}()
    lv(fn)  = get!(() -> _history_leaves(fn), leaves, fn)
    done    = Int[]                                     # indices of done runs with params
    ident   = Dict{Int,Tuple{String,String,Bool}}()     # i => (name, ns, into) it wrote
    for (i, e) in enumerate(entries)
        run_entry_status(e) == "done" || continue
        _rl_get(e, "params", nothing) isa AbstractDict || continue
        push!(done, i)
        r = run_log_row(e)
        l = lv(r.fn); l === nothing && continue
        id = try _run_output_identity(r.fn, r.params, l) catch; ("", "", false) end
        isempty(id[1]) || (ident[i] = id)
    end
    # A run that writes INTO what it reads (tracking into its label set) reads the labels, not an earlier
    # write-into of them: for it, a later re-track is not a replaced input, and an earlier track run is
    # not the maker of what it read. Without this every re-track would stale the one before it.
    into_self(i, ref) = haskey(ident, i) && ident[i][3] && (ident[i][1], ident[i][2]) == ref
    writes_into(j)    = haskey(ident, j) && ident[j][3]
    # a later done run that replaced (name, ns) at the version `i` read
    function replaced_after(i, inp)
        ref, ver = _stale_ref(inp), _stale_ver(inp)
        skip_into = into_self(i, ref)
        for j in done
            j > i || continue
            (skip_into && writes_into(j)) && continue
            rep = _rl_get(entries[j], "replaces", nothing)
            rep isa AbstractDict && _stale_ref(rep) == ref || continue
            rv = _stale_ver(rep)
            (isempty(ver) || isempty(rv) || rv == ver) && return j
        end
        nothing
    end
    # The maker of the version run `i` read: the newest done run before `i` that wrote (name, ns), as a
    # write or a write-into. It is taken at its word when the read names no version, when it overwrote
    # exactly the version read (`replaces`), or when it is the only writer of a read that follows the
    # latest. Otherwise (several writers, or a pinned read) the record cannot say which run made the
    # version read — a run minting a version records no `replaces`, and one before the log is not in it —
    # so all candidates are asked: all clean is clean, and any stale one makes the answer `:unknown`.
    function upstream(i, inp)
        ref, ver = _stale_ref(inp), _stale_ver(inp)
        skip_into = into_self(i, ref)
        ks = Int[j for j in done if j < i && haskey(ident, j) && (ident[j][1], ident[j][2]) == ref &&
                                    !(skip_into && writes_into(j))]
        isempty(ks) && return (nothing, 0)
        k   = last(ks)
        rep = _rl_get(entries[k], "replaces", nothing)
        pinned = _rl_get(inp, "pinned", false) === true
        (isempty(ver) || (rep isa AbstractDict && _stale_ver(rep) == ver) || (length(ks) == 1 && !pinned)) &&
            return (chain(k), k)
        rs = Any[chain(j) for j in ks]
        all(isnothing, rs) ? (nothing, k) : (:unknown, k)
    end
    # why run `i` is stale (the chain of links), `:unknown` (its record, or an input's maker's, predates
    # recorded inputs), or nothing. Stale on any input beats unknown on another.
    memo = Dict{Int,Any}()
    function chain(i)
        haskey(memo, i) && return memo[i]
        memo[i] = nothing                               # indices only decrease below; this is a guard
        e = entries[i]
        inputs = _rl_get(e, "inputs", nothing)
        inputs isa AbstractVector ||
            return memo[i] = (_stale_reads_names(lv(_rl_str(e, "fun"))) ? :unknown : nothing)
        unk = false
        for inp in inputs
            inp isa AbstractDict || continue
            read = Dict{String,Any}("name" => _rl_str(inp, "name"))
            isempty(_rl_str(inp, "ns")) || (read["ns"] = _rl_str(inp, "ns"))
            isempty(_stale_ver(inp)) || (read["version"] = _stale_ver(inp))
            j = replaced_after(i, inp)
            j === nothing ||
                return memo[i] = Any[Dict{String,Any}("read" => read, "replacedBy" => _stale_run(entries[j]))]
            up, k = upstream(i, inp)
            up isa AbstractVector &&
                return memo[i] = vcat(Any[Dict{String,Any}("read" => read, "madeBy" => _stale_run(entries[k]))], up)
            up === :unknown && (unk = true)
        end
        memo[i] = unk ? :unknown : nothing
    end
    # the current maker of each output: the newest done run that wrote it
    current = Dict{Tuple{String,String,Bool},Int}()
    for i in done
        haskey(ident, i) && (current[ident[i]] = i)
    end
    stale, unknown = Any[], Any[]
    for ((name, ns, into), i) in sort!(collect(current); by = last)
        into || _run_has_output(img, name, ns) !== false || continue
        e = entries[i]
        d = Dict{String,Any}("name" => name, "run" => _stale_run(e))
        isempty(ns) || (d["ns"] = ns)
        into && (d["into"] = true)
        why = chain(i)
        why === nothing && continue
        if why === :unknown
            d["reason"] = _rl_get(e, "inputs", nothing) isa AbstractVector ?
                "the record cannot say whether what it read is current (an older maker, or a version " *
                "the log does not say who made)" :
                "made by a run from before inputs were recorded"
            push!(unknown, d)
            continue
        end
        d["because"] = why
        d["summary"] = _stale_summary(name, e, why)
        push!(stale, d)
    end
    (; stale, unknown)
end

# "B (segment.cellposeMeasure, 2026-…) read 'default' v2, which cleanupImages.smooth replaced at …"
function _stale_summary(name, e, why)
    parts = String[]
    for l in why
        r = l["read"]
        v = haskey(r, "version") ? " $(r["version"])" : ""
        if haskey(l, "replacedBy")
            push!(parts, "read '$(r["name"])'$v, since replaced by $(l["replacedBy"]["fun"]) at $(l["replacedBy"]["at"])")
        else
            push!(parts, "read '$(r["name"])'$v made by $(l["madeBy"]["fun"]) at $(l["madeBy"]["at"]), which")
        end
    end
    "'$name' ($(_rl_str(e, "fun")) at $(_rl_str(e, "at"))) " * join(parts, " ")
end
