# ── label_correction — manual segmentation edits as pure ops over a labels frame ────
#
# The Julia-side engine behind `segment.correct` (docs/todo/CORRECTION_PLAN.md, P2). A label
# correction is a REWRITE of the labels array: `label.merge` sets every pixel carrying id B to id A,
# `label.remove` sets every pixel carrying id A to 0. Frame-local by design (Decision 6b) — an op
# always carries a `t` and applies to that timepoint only. Multi-frame propagation is a
# `SEG_QUALITY_PLAN.md` problem, not a correction one.
#
# WHERE THE APPLY LIVES. The array mutation itself is Python — the labels store is a zarr, the writer
# path is `zarr_utils.staged_store` + `store_compressor('labels')`, and per-frame numpy assignment is
# idiomatic there. Julia builds a compact `Dict{Int, Dict{Int,Int}}` rewrite table (t → source →
# target) from the op list, hands it to the Python runner, and receives back the metrics. Julia
# still owns:
#
#   * validation (op kind, integer args, t range, non-empty ids)
#   * the JOURNAL — same shape and same file convention as tracking's (`corrections/{vn}.json`)
#   * metrics + QC banking (Decision 8)
#
# EVERYTHING HERE IS PURE. Ops take a rewrite dict or a labels-array proxy and return an updated one
# in the same shape; nothing here reads or writes a store. That is what lets the whole op set be
# unit-tested without a fixture, and it is the same rule track_correction.jl follows.
#
# WHY A SEPARATE FILE FROM segmentation.jl. `segmentation.jl` is the cross-algorithm output-writing
# conventions (filenames, ccid registration, live preview). Correction is a distinct engine with its
# own op vocabulary, journal, QC and metrics — mirrors `tracking/track_correction.jl` peer-to-tasks.

using JSON3

# ── Op vocabulary ────────────────────────────────────────────────────────────────

"""
The three verbs shipped so far.

Fields:
  * `label.merge`  — `{ op, t, ids: [a, b, …], into }` — every non-`into` id in `ids` is rewritten
                     to `into`, in frame `t`. `into` must be one of `ids` (surface makes it
                     unambiguous). `ids` may contain more than two, so a highlighted cluster of
                     touching labels folds in one op.
  * `label.remove` — `{ op, t, ids: [a, …] }`         — every id in `ids` is rewritten to 0
                     (background), in frame `t`.
  * `label.split`  — `{ op, t, id, xs: [x1, x2, …], ys: [y1, y2, …] }` — cut label `id` at frame
                     `t` along the polyline `(xs, ys)` (image-pixel coords, integer). The runner
                     rasterises the polyline as a 1-pixel-wide line, subtracts it from the label's
                     mask, runs connected components on the remainder, and assigns fresh label ids
                     to every component except the largest (which keeps the original id). A cut
                     that does not fully divide the label is a no-op (the runner logs a warning).
                     Introduced in Phase 4; the polyline shape supports the two-click cockpit
                     affordance today and a raster brush later without a second op kind.
"""
const LABEL_OP_KINDS = ("label.merge", "label.remove", "label.split")

_op_get(op::AbstractDict, k::AbstractString) = get(op, k, get(op, Symbol(k), nothing))

function _op_int(op::AbstractDict, k::AbstractString)::Union{Nothing,Int}
    v = _op_get(op, k)
    isnothing(v) && return nothing
    v isa Integer && return Int(v)
    v isa Real    && return Int(round(Float64(v)))
    v isa AbstractString && return parse(Int, v)
    throw(ArgumentError("label op field \"$k\" must be an integer, got $(typeof(v))"))
end

function _op_int_list(op::AbstractDict, k::AbstractString)::Vector{Int}
    v = _op_get(op, k)
    v isa AbstractVector || throw(ArgumentError("label op field \"$k\" must be a list, got $(typeof(v))"))
    out = Int[]
    for (i, x) in enumerate(v)
        x isa Integer && (push!(out, Int(x)); continue)
        x isa Real    && (push!(out, Int(round(Float64(x)))); continue)
        x isa AbstractString && (push!(out, parse(Int, x)); continue)
        throw(ArgumentError("label op field \"$k\"[$i] must be an integer, got $(typeof(x))"))
    end
    out
end

# ── Validation (per-op, before any IO) ───────────────────────────────────────────

"""
    validate_label_op(op) -> Nothing

Throw `ArgumentError` when `op` is malformed — an unknown kind, a missing field, out-of-range values.
Called by both the engine and the task's param validator so nonsense fails BEFORE the labels are
opened. Same discipline as `apply_track_op!`'s upfront checks.
"""
function validate_label_op(op)::Nothing
    op isa AbstractDict || throw(ArgumentError("label op must be an object, got $(typeof(op))"))
    kind = string(something(_op_get(op, "op"), ""))
    kind in LABEL_OP_KINDS ||
        throw(ArgumentError("unknown label correction op \"$kind\" — expected one of " *
                            join(LABEL_OP_KINDS, ", ")))

    t = _op_int(op, "t")
    (isnothing(t) || t < 0) && throw(ArgumentError("label op \"$kind\" needs a non-negative integer `t`"))

    if kind == "label.split"
        # Split identifies ONE label + a polyline. `id` (singular) keeps the shape distinct from
        # merge/remove — a plural `ids` on split would be ambiguous (are we splitting several
        # labels with the same cut?), and the runner works one label at a time anyway.
        id = _op_int(op, "id")
        (isnothing(id) || id < 1) &&
            throw(ArgumentError("label.split needs a single positive integer `id` (0 = background)"))
        xs = _op_int_list(op, "xs")
        ys = _op_int_list(op, "ys")
        length(xs) == length(ys) ||
            throw(ArgumentError("label.split `xs`/`ys` must be the same length, got " *
                                "$(length(xs)) and $(length(ys))"))
        length(xs) >= 2 ||
            throw(ArgumentError("label.split polyline needs at least 2 vertices, got $(length(xs))"))
        all(>=(0), xs) && all(>=(0), ys) ||
            throw(ArgumentError("label.split polyline coords must be non-negative integers"))
        return nothing
    end

    ids = _op_int_list(op, "ids")
    isempty(ids) && throw(ArgumentError("label op \"$kind\" needs a non-empty `ids` list"))
    all(>=(1), ids) || throw(ArgumentError("label op \"$kind\" ids must all be >= 1 (0 = background)"))

    if kind == "label.merge"
        length(ids) >= 2 || throw(ArgumentError("label.merge needs at least 2 ids"))
        into = _op_int(op, "into")
        isnothing(into) && throw(ArgumentError("label.merge needs `into` (the surviving id)"))
        into in ids || throw(ArgumentError("label.merge `into` ($into) must be one of `ids`"))
    end
    nothing
end

# ── Building the rewrite table ───────────────────────────────────────────────────

"""
    build_rewrite(ops) -> Dict{Int, Dict{Int,Int}}

Fold an ordered op list into a per-timepoint `{t => {source_id => target_id}}` map. The Python
runner then does exactly one array pass per touched frame: `arr[t][mask == src] = tgt`.

**Order matters WITHIN a frame** — if the caller queues `merge(t=0, [1,2] into 1)` and then
`merge(t=0, [1,3] into 1)`, the second rewrites both source ids to 1. Chains collapse at the end
of the fold (a→b, b→c ⇒ a→c) so the runner never applies more than one rewrite to any pixel and
the number of Python array passes is bounded by the number of DISTINCT frames touched, not the
number of ops.

**Ops targeting different `t` are independent** — no frame ever sees another frame's edits, which
is the Decision-6b invariant made explicit.
"""
function build_rewrite(ops)::Dict{Int, Dict{Int,Int}}
    out = Dict{Int, Dict{Int,Int}}()
    for op in ops
        validate_label_op(op)
        kind = string(_op_get(op, "op"))
        # Split is a NON-rewrite op — it creates fresh ids by connected-component analysis, which a
        # {src → tgt} table cannot represent. The Python runner applies it directly against the
        # frame; this table is for callers reasoning about merges + removes only, so split is a
        # no-op here rather than an error (a mixed queue is legal).
        kind == "label.split" && continue
        t    = _op_int(op, "t")::Int
        ids  = _op_int_list(op, "ids")
        m    = get!(out, t, Dict{Int,Int}())
        if kind == "label.merge"
            into = _op_int(op, "into")::Int
            for id in ids
                id == into && continue
                m[id] = into
            end
        else  # label.remove
            for id in ids
                m[id] = 0
            end
        end
    end
    # Collapse chains inside each frame so the Python side does one rewrite per pixel, not N.
    for (_, m) in out
        for (from, _) in collect(m)
            cur, seen = from, Set{Int}([from])
            while haskey(m, cur)
                nxt = m[cur]
                nxt in seen && break              # cycle-safe (shouldn't happen; defensive)
                push!(seen, nxt); cur = nxt
            end
            m[from] = cur
        end
    end
    out
end

# ── Journal I/O (parallel to track_correction.jl) ────────────────────────────────
#
# One file per value_name, appended to per correction run. Same directory + shape as the tracking
# journal, so a future viewer can render both together without a second reader.

# `corrections_dir` is defined by `tracking/track_correction.jl`; both correction engines share the
# same directory so a future combined-history viewer reads one place.
label_corrections_path(task_dir::AbstractString, value_name::AbstractString) =
    joinpath(corrections_dir(task_dir), "labels_" * string(value_name) * ".json")

"""
    load_label_corrections(task_dir, value_name) -> Dict

Read the journal (`{"valueName", "entries"}`) or return an empty one when absent. Never throws for a
missing file — an image with no prior corrections is the base case, not an error.
"""
function load_label_corrections(task_dir::AbstractString, value_name::AbstractString)::Dict{String,Any}
    path = label_corrections_path(task_dir, value_name)
    isfile(path) || return Dict{String,Any}("valueName" => String(value_name),
                                            "entries"   => Dict{String,Any}[])
    raw = JSON3.read(read(path, String), Dict{String,Any})
    get!(raw, "valueName", String(value_name))
    get!(raw, "entries", Dict{String,Any}[])
    raw
end

"""
    append_label_corrections!(task_dir, value_name, entries; run_id) -> String

Append `entries` to the journal and write it atomically. Each entry is stamped with `seq` (append
order across ALL runs, so replay is deterministic) and `runId` (the correction run that produced
it, so the history can be grouped). Returns the path.

Entries are the caller's shape — normally `{op, t, ids, into?, nPixels}` — and this function does
NOT validate them. Validation is `validate_label_op`'s job and happens BEFORE the run touches
anything; a journal write only fires after a successful apply, so an invalid op never appears.
"""
function append_label_corrections!(task_dir::AbstractString, value_name::AbstractString,
                                   entries::AbstractVector; run_id = nothing)::String
    doc = load_label_corrections(task_dir, value_name)
    existing = collect(Dict{String,Any}, doc["entries"])
    seq = length(existing)
    for e in entries
        rec = Dict{String,Any}(string(k) => v for (k, v) in pairs(e))
        rec["seq"] = (seq += 1)
        isnothing(run_id) || (rec["runId"] = string(run_id))
        push!(existing, rec)
    end
    doc["entries"] = existing
    dir = corrections_dir(task_dir)
    isdir(dir) || mkpath(dir)
    path = label_corrections_path(task_dir, String(value_name))
    write_json_atomic(path, doc)
    path
end

# ── Metrics + QC (Decision 8) ────────────────────────────────────────────────────

"""
    label_correction_metrics(ops, per_op_pixels; n_labels_before, n_labels_after) -> Dict

Objective counts for a correction run. Pure so it can be unit-tested without a store.

  * `nOps`               — ops applied
  * `nMerge` / `nRemove` — per-kind counts
  * `nFramesTouched`     — distinct t values across the ops
  * `nLabelsRemoved`     — labels no longer in the store (== label_ids B/C/... in merges + removes)
  * `nPixelsRewritten`   — sum of `per_op_pixels`; the Python runner returns this per op
  * `fracLabelsEdited`   — nLabelsRemoved / n_labels_before, the share QC's threshold reads

`per_op_pixels` is the raw pixel-count list from the runner, one per op in the same order. When the
runner hasn't reported (e.g. dry-run), pass an empty vector and the pixel total is 0.
"""
function label_correction_metrics(ops::AbstractVector, per_op_pixels::AbstractVector;
                                  n_labels_before::Integer = 0,
                                  n_labels_after::Integer  = 0)::Dict{String,Any}
    n_merge, n_remove, n_split = 0, 0, 0
    ts_touched, labels_removed, labels_split = Set{Int}(), Set{Int}(), Set{Int}()
    for op in ops
        kind = string(_op_get(op, "op"))
        push!(ts_touched, _op_int(op, "t")::Int)
        if kind == "label.split"
            n_split += 1
            id = _op_int(op, "id")::Int
            push!(labels_split, id)
            continue
        end
        ids = _op_int_list(op, "ids")
        if kind == "label.merge"
            n_merge += 1
            into = _op_int(op, "into")::Int
            for id in ids
                id == into || push!(labels_removed, id)
            end
        elseif kind == "label.remove"
            n_remove += 1
            for id in ids
                push!(labels_removed, id)
            end
        end
    end
    n_labels_before = Int(n_labels_before)
    # `nLabelsEdited` = union of removed + split (a label that was both merged-away and split by two
    # ops counts once). Split doesn't REMOVE the original id (the largest fragment keeps it), so it
    # goes in a separate `nLabelsSplit` bucket for the QC threshold to consider both.
    n_edited = length(union(labels_removed, labels_split))
    Dict{String,Any}(
        "nOps"             => length(ops),
        "nMerge"           => n_merge,
        "nRemove"          => n_remove,
        "nSplit"           => n_split,
        "nFramesTouched"   => length(ts_touched),
        "nLabelsRemoved"   => length(labels_removed),
        "nLabelsSplit"     => length(labels_split),
        "nLabelsBefore"    => n_labels_before,
        "nLabelsAfter"     => Int(n_labels_after),
        "nPixelsRewritten" => sum(Int.(per_op_pixels); init = 0),
        "fracLabelsEdited" => n_labels_before > 0 ? n_edited / n_labels_before : 0.0,
    )
end

# Threshold matches the tracking side (Decision 8). A mask that needs > 30 % of its labels
# hand-fixed is a segmentation problem, not a correction one — surface it as a warn.
const LABEL_CORRECTION_WARN_FRAC = 0.3

"""
    label_correction_qc_findings(metrics) -> Vector{Dict}

Advisory findings for a correction run. Pure (no I/O) so it is unit-tested directly, mirrors
`track_correction_qc_findings`. Never `error` — a correction the user asked for is not a failure.
One warn when `fracLabelsEdited` crosses the threshold (a mask hand-fixed at that scale is a
segmentation-parameter problem, not a correction one). Empty ops = no findings.
"""
function label_correction_qc_findings(metrics::AbstractDict)::Vector{Dict{String,Any}}
    out = Dict{String,Any}[]
    frac = Float64(get(metrics, "fracLabelsEdited", 0.0))
    if frac >= LABEL_CORRECTION_WARN_FRAC
        push!(out, qc_finding("warn", "correction.labels_large_share_edited";
                              pct = string(round(Int, frac * 100))))
    end
    out
end
