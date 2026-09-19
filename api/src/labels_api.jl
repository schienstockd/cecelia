# Enumerate raw label / track IDs for an image's segmentation, so consumers that need to point at
# real objects can do so instead of guessing (BIDIR follow-up: Claude's `mark_cells` calls used to
# fail silently because it picked ids blind — see docs/todo/BIDIR_CONTEXT_PLAN.md).
#
# READ-ONLY. Two kinds share ONE endpoint to keep the surface small:
#   • kind=cells  → `label_props(img; value_name=vn)` → `:label` column (per-cell label ids)
#   • kind=tracks → `label_props(track_props path)`   → `:label` column (per-track track ids;
#                                                       one row per track, label == track_id)
#
# Behaviour under load: the segmentation may have tens of thousands of labels. Payloads are
# CAPPED at `limit` (default 200) and the response reports `total` + `truncated` so the caller
# knows whether it's seeing the full picture. `sample=true` returns a stride-uniform sample of
# `limit` ids across the full population — same first + last, but the middle is thinned — so a
# consumer that wants to "point at a few random cells" gets coverage, not just the first N.

using DataFrames

const _LABELS_KINDS = ("cells", "tracks")
const _LABELS_DEFAULT_LIMIT = 200
const _LABELS_MAX_LIMIT     = 5000    # cap the cap — beyond this the JSON grows unreasonably

# Read `:label` for one kind. Returns Vector{Int} — cheap for the sizes we deal with; a Vector
# scan for `sample` beats a lazy iterator, and the DataFrame is on disk already.
function _labels_ids_for(img::CciaImage, value_name::AbstractString, kind::AbstractString)::Vector{Int}
    if kind == "cells"
        lp = label_props(img; value_name = value_name)
        df = as_df(select_cols(lp, String[]))     # just the :label column (always included)
        return Int.(df.label)
    elseif kind == "tracks"
        p = img_track_props_path(img, value_name)
        isfile(p) || return Int[]
        lp = label_props(p)
        df = as_df(select_cols(lp, String[]))
        return Int.(df.label)
    else
        error("unknown kind: $kind")
    end
end

# Stride-uniform sample: pick `k` positions from `0..n-1` at equal spacing, always including the
# first and last. Deterministic — a random sample would return a different set per call, which
# would make caching / debugging harder. Same idea as a fixed decimate for a plot preview.
function _stride_sample(ids::Vector{Int}, k::Int)::Vector{Int}
    n = length(ids)
    (n <= k || k <= 0) && return copy(ids)
    k == 1 && return [ids[1]]
    # positions 0..n-1 at (k-1) intervals of (n-1)/(k-1); rounded to Int
    step = (n - 1) / (k - 1)
    out = Vector{Int}(undef, k)
    for i in 1:k
        out[i] = ids[round(Int, (i - 1) * step) + 1]
    end
    return out
end

"""
    GET /api/labels/ids?projectUid=…&imageUid=…&valueName=…&kind=cells|tracks&limit=200&sample=false

Return the label / track ids present in the segmentation `value_name` for one image, capped and
optionally sampled. Response shape:

  { kind, valueName, ids: [Int, …], total: Int, truncated: Bool, sampled: Bool }
"""
function api_labels_ids(req::HTTP.Request)
    query = HTTP.queryparams(HTTP.URI(req.target))
    project_uid = get(query, "projectUid", "")
    image_uid   = get(query, "imageUid", "")
    value_name  = get(query, "valueName", "")
    kind        = get(query, "kind", "cells")
    isempty(project_uid) && return 400, JSON3.write((; error = "projectUid required"))
    isempty(image_uid)   && return 400, JSON3.write((; error = "imageUid required"))
    isempty(value_name)  && return 400, JSON3.write((; error = "valueName required"))
    kind in _LABELS_KINDS || return 400, JSON3.write((;
        error = "kind must be one of $(join(_LABELS_KINDS, ", "))"))

    limit = clamp(parse(Int, get(query, "limit", string(_LABELS_DEFAULT_LIMIT))),
                  1, _LABELS_MAX_LIMIT)
    sample = get(query, "sample", "false") == "true"

    # Reuses `_gating_image` from gating_api.jl — same project/image resolution every gating +
    # tracking + correction endpoint uses, so a valid id here is a valid id everywhere.
    img, err = _gating_image(project_uid, image_uid)
    err === nothing || return err   # err is already (status, JSON3-body) from _gerr

    ids = try
        _labels_ids_for(img, value_name, kind)
    catch e
        # A missing label_props file / a value_name that doesn't exist → 404 rather than 500, so
        # the caller can distinguish "this image has no such segmentation" from a server bug.
        return 404, JSON3.write((; error = "no $(kind) for valueName=$(value_name): $(sprint(showerror, e))"))
    end

    total = length(ids)
    sampled = false
    out_ids = if total <= limit
        ids
    elseif sample
        sampled = true
        _stride_sample(ids, limit)
    else
        ids[1:limit]
    end
    truncated = length(out_ids) < total && !sampled

    return 200, JSON3.write((;
        kind = kind, valueName = value_name,
        ids = out_ids, total = total,
        truncated = truncated, sampled = sampled))
end
