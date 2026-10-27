# ── ROI sets + their reviews (docs/todo/ROI_TRIAGE_PLAN.md §5, §7) ─────────────────────────────
#
# A ROI set (`rois/{suffix}.json`) is task output, read-only here. Its reviews
# (`roiReviews/{suffix}.json`) are the shared surface the person and Claude both write through
# `POST /api/rois/review`. Everything that decides what a write may do lives in `apply_roi_review!`,
# a pure function over the reviews dict, so the guard rules are unit-tested without a server:
#
# - Claude (`by.via == "claude"`) PROPOSES: it may set `claudeLabel`, flag an undecided ROI, and undo
#   its own changes. It may not accept, reject, set the person's label or note, or clear a flag a
#   person set (Decision 7; the Blackboard section-verdict rule).
# - Every change appends `{at, by, field, from, to}` to the ROI's history; `undo` reverts the latest
#   one the caller may revert. Nothing is ever deleted (an orphaned review stays on disk).

using Dates: now

const ROI_STATUSES = ("pending", "accepted", "rejected", "flagged")
const ROI_NOTE_MAX = 2048
const ROI_REASON_MAX = 500

# One lock for every reviews file: writes are rare (a click, a label) and a per-path lock table would
# be more machinery than the contention warrants.
const _ROI_REVIEWS_LOCK = ReentrantLock()

"""
    load_roi_set(img, suffix) -> Dict or nothing

The ROI set a `spatialAnalysis.roiDiscovery` run wrote, or `nothing` when there is none.
"""
function load_roi_set(img::CciaImage, suffix::AbstractString)
    p = img_rois_path(img, suffix)
    isfile(p) ? read_state_json(p; as = Dict{String,Any}) : nothing
end

"""
    roi_suffixes_on(img, value_name) -> Vector{String}

The ROI sets scored on ONE segmentation (their recorded `valueName`) — what a rename rewrites, a
label delete removes and a label correction marks stale. An unreadable set is skipped, not fatal: all
three callers act on many artefacts and one bad sidecar must not stop the rest.
"""
function roi_suffixes_on(img::CciaImage, value_name::AbstractString)::Vector{String}
    filter(img_rois_suffixes(img)) do suffix
        set = try
            load_roi_set(img, suffix)
        catch
            nothing
        end
        set isa AbstractDict && get(set, "valueName", nothing) == value_name
    end
end

"""
    load_roi_reviews(img, suffix) -> Dict{String,Any}

The reviews of one ROI set, keyed by ROI id — empty when none were written yet.
"""
function load_roi_reviews(img::CciaImage, suffix::AbstractString)::Dict{String,Any}
    p = img_roi_reviews_path(img, suffix)
    isfile(p) || return Dict{String,Any}()
    doc = read_state_json(p; as = Dict{String,Any})
    r = get(doc, "reviews", nothing)
    r isa AbstractDict ? Dict{String,Any}(String(k) => v for (k, v) in r) : Dict{String,Any}()
end

function save_roi_reviews!(img::CciaImage, suffix::AbstractString, reviews::AbstractDict)
    p = img_roi_reviews_path(img, suffix)
    mkpath(dirname(p))
    write_json_atomic(p, Dict{String,Any}("version" => 1, "reviews" => reviews))
end

"""
    roi_status(review) -> String

What the list shows: the stored status, except an undecided ROI Claude has labelled reads
`claude-labelled` — derived, so no write ever has to keep the two in step.
"""
function roi_status(review)::String
    review isa AbstractDict || return "pending"
    s = String(get(review, "status", "pending"))
    s == "pending" && get(review, "claudeLabel", nothing) isa AbstractDict ? "claude-labelled" : s
end

_by_claude(by) = by isa AbstractDict && get(by, "via", "") == "claude"

_roi_labels(roi) = Set{Int}(Int(o["label"]) for o in get(roi, "objects", Any[]) if haskey(o, "label"))

"""
    roi_set_labels(set) -> Vector{String}

The review labels a ROI set offers — chosen on the "Find ROIs" run that made it (`reviewLabels`), so
review fits the question that run asked; the task default for a set that recorded none.
"""
function roi_set_labels(set)::Vector{String}
    l = set isa AbstractDict ? get(set, "labels", nothing) : nothing
    l isa AbstractVector && !isempty(l) ? String[string(x) for x in l] : roi_review_labels(ROI_LABELS_DEFAULT)
end

# Validate Claude's structured label against the ROI it is about and the set's labels (plan §4: ids
# and frames that are not in the ROI are hallucinations, refused rather than stored).
function _validate_claude_label(cl, roi, labels)::Union{Nothing,String}
    cl isa AbstractDict || return "claudeLabel must be an object"
    t = String(get(cl, "label", ""))
    t in labels || return "claudeLabel.label must be one of $(join(labels, ", "))"
    c = get(cl, "confidence", nothing)
    (c isa Real && 0 <= c <= 1) || return "claudeLabel.confidence must be a number in [0, 1]"
    reason = String(get(cl, "reason", ""))
    isempty(strip(reason)) && return "claudeLabel.reason required"
    length(reason) > ROI_REASON_MAX && return "claudeLabel.reason exceeds $ROI_REASON_MAX characters"
    ids = get(cl, "objectIds", Any[])
    ids isa AbstractVector || return "claudeLabel.objectIds must be a list"
    known = _roi_labels(roi)
    bad = [i for i in ids if !(i isa Integer) || !(Int(i) in known)]
    isempty(bad) || return "claudeLabel.objectIds not in this ROI: $(join(string.(bad), ", "))"
    frames = get(cl, "frames", Any[])
    frames isa AbstractVector || return "claudeLabel.frames must be a list"
    t0, t1 = Int(get(roi, "t0", 0)), Int(get(roi, "t1", 0))
    badf = [f for f in frames if !(f isa Integer) || !(t0 <= f <= t1)]
    isempty(badf) || return "claudeLabel.frames outside $(t0)–$(t1): $(join(string.(badf), ", "))"
    nothing
end

function _set_field!(rev::AbstractDict, field::String, value, by, at)
    from = get(rev, field, nothing)
    from == value && return false
    value === nothing ? delete!(rev, field) : (rev[field] = value)
    push!(get!(rev, "history", Any[]),
          Dict{String,Any}("at" => at, "by" => by, "field" => field, "from" => from, "to" => value))
    true
end

"""
    apply_roi_review!(reviews, roi, change, by; labels, at) -> (status::Int, error::Union{String,Nothing})

Apply one review write to `reviews` (mutated in place) for the ROI dict `roi`. `change` may carry
`status`, `userLabel`, `note`, `claudeLabel`, or `undo = true` (alone). `labels` are the set's review
labels (`roi_set_labels`) a label must be one of. `by` is the caller's `author_stamp()`. Returns
`(200, nothing)` on success — including a no-op — else a 400/409 status and the reason. Nothing is
written to disk here.
"""
function apply_roi_review!(reviews::AbstractDict, roi::AbstractDict, change::AbstractDict, by::AbstractDict;
                           labels::AbstractVector{<:AbstractString} = roi_review_labels(ROI_LABELS_DEFAULT),
                           at::String = string(now()))
    id = String(roi["id"])
    rev = get!(reviews, id, Dict{String,Any}("status" => "pending"))
    claude = _by_claude(by)

    if get(change, "undo", false) === true
        hist = get(rev, "history", Any[])
        isempty(hist) && return (409, "nothing to undo")
        h = hist[end]
        # Claude may only revert its OWN latest change — reaching past a person's later edit would
        # overwrite it. A person may revert anything.
        claude && !_by_claude(get(h, "by", nothing)) &&
            return (409, "the latest change is a person's; Claude cannot undo it")
        field = String(h["field"])
        _set_field!(rev, field, get(h, "from", nothing), by, at)
        return (200, nothing)
    end

    status = get(change, "status", nothing)
    if status !== nothing
        status = String(status)
        status in ROI_STATUSES || return (400, "status must be one of $(join(ROI_STATUSES, ", "))")
        if claude
            status == "flagged" || return (409, "Claude may flag an ROI, not accept, reject or reset it")
            get(rev, "status", "pending") in ("accepted", "rejected") &&
                return (409, "a person already decided this ROI")
        end
    end
    if claude && (haskey(change, "userLabel") || haskey(change, "note"))
        return (409, "the label and note are the person's; Claude writes claudeLabel")
    end
    if haskey(change, "userLabel")
        ul = String(something(change["userLabel"], ""))
        isempty(ul) || ul in labels || return (400, "userLabel must be one of $(join(labels, ", ")) or empty")
    end
    if haskey(change, "note")
        length(String(something(change["note"], ""))) > ROI_NOTE_MAX &&
            return (400, "note exceeds $ROI_NOTE_MAX characters")
    end
    cl = get(change, "claudeLabel", nothing)
    if cl !== nothing
        err = _validate_claude_label(cl, roi, labels)
        err === nothing || return (400, err)
    end

    status === nothing || _set_field!(rev, "status", status, by, at)
    if haskey(change, "userLabel")
        ul = String(something(change["userLabel"], ""))
        _set_field!(rev, "userLabel", isempty(ul) ? nothing : ul, by, at)
    end
    if haskey(change, "note")
        n = String(strip(String(something(change["note"], ""))))
        _set_field!(rev, "note", isempty(n) ? nothing : n, by, at)
    end
    if cl !== nothing
        stored = Dict{String,Any}(String(k) => v for (k, v) in cl)
        stored["by"] = by
        stored["at"] = at
        _set_field!(rev, "claudeLabel", stored, by, at)
    end
    (200, nothing)
end

"""
    roi_review!(img, suffix, roi_id, change, by) -> (status, error_or_review)

Load → `apply_roi_review!` → save, under the reviews lock. 404 when the set or the ROI is missing.
"""
function roi_review!(img::CciaImage, suffix::AbstractString, roi_id::AbstractString,
                     change::AbstractDict, by::AbstractDict)
    set = load_roi_set(img, suffix)
    set === nothing && return (404, "no ROI set $(suffix)")
    k = findfirst(r -> String(get(r, "id", "")) == roi_id, get(set, "rois", Any[]))
    k === nothing && return (404, "no ROI $(roi_id) in $(suffix)")
    lock(_ROI_REVIEWS_LOCK) do
        reviews = load_roi_reviews(img, suffix)
        st, err = apply_roi_review!(reviews, set["rois"][k], change, by; labels = roi_set_labels(set))
        st == 200 || return (st, err)
        save_roi_reviews!(img, suffix, reviews)
        (200, reviews[String(roi_id)])
    end
end

"""
    roi_list_payload(img, suffix; objects = false) -> Dict

The ROI set merged with its reviews, for the viewer's list and MCP: each ROI carries `review` +
derived `status`; object tables are reduced to `objectLabels` unless `objects = true`. Reviews whose
ROI no longer exists (a re-run with other params) come back under `orphaned`, never dropped.
"""
function roi_list_payload(img::CciaImage, suffix::AbstractString; objects::Bool = false)
    set = load_roi_set(img, suffix)
    reviews = load_roi_reviews(img, suffix)
    out = Dict{String,Any}("suffix" => suffix, "suffixes" => img_rois_suffixes(img))
    set === nothing && return merge(out, Dict{String,Any}("rois" => Any[], "orphaned" => reviews))
    ids = Set{String}()
    rows = Any[]
    for r in get(set, "rois", Any[])
        row = Dict{String,Any}(String(k) => v for (k, v) in r)
        id = String(row["id"])
        push!(ids, id)
        rev = get(reviews, id, nothing)
        row["review"] = rev
        row["status"] = roi_status(rev)
        objs = get(row, "objects", Any[])
        row["objectLabels"] = [Int(o["label"]) for o in objs if haskey(o, "label")]
        objects || delete!(row, "objects")
        push!(rows, row)
    end
    for k in ("version", "generatedAt", "params", "geometry", "valueName", "rankBy")
        haskey(set, k) && (out[k] = set[k])
    end
    out["labels"] = roi_set_labels(set)
    out["rois"] = rows
    out["orphaned"] = Dict{String,Any}(k => v for (k, v) in reviews if !(k in ids))
    out
end
