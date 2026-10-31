# ── ROI list routes (docs/todo/ROI_TRIAGE_PLAN.md §7) ─────────────────────────────────────────
# The viewer's ROI section and (P2) the MCP `roi_*` tools read and write ONE surface: the ROI set a
# `spatialAnalysis.roiDiscovery` run wrote, merged with its reviews. Thin handlers — the guard rules
# (what Claude may write, history, undo) live in `app/src/rois.jl::apply_roi_review!`.

using JSON3

# The ROI suffix to show: the asked-for one, else `default` when it exists, else the first on disk.
function _rois_suffix(img::CciaImage, asked::AbstractString)::String
    isempty(asked) || return String(asked)
    have = img_rois_suffixes(img)
    ("default" in have || isempty(have)) ? "default" : first(have)
end

"""
    GET /api/rois?projectUid&imageUid[&suffix][&objects=1]

Reply: `roi_list_payload` — `{suffix, suffixes, rois:[{id, rank, score, type, t0, t1, box, centroid,
nObjects, cues, objectLabels, review, status, objects?}], orphaned, params, geometry, generatedAt}`.
No ROI set yet → `rois: []` with the `suffixes` that do exist (none → run "Find ROIs" first).
"""
function api_rois_list(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    suffix = _rois_suffix(img, get(q, "suffix", ""))
    200, JSON3.write(roi_list_payload(img, suffix; objects = get(q, "objects", "") in ("1", "true")))
end

"""
    POST /api/rois/review

Body: `{ projectUid, imageUid, suffix, roiId, status?, userLabel?, note?, claudeLabel?, undo? }`
Reply: `{ ok: true, roiId, review }`; 400 on a bad value, 404 for an unknown set/ROI, 409 when the
caller may not make this change (a call from Claude — `X-Cecelia-Client: claude` — proposes: it can
set `claudeLabel` and flag, never accept/reject or touch the person's label/note). Every change is
attributed (`author_stamp()`) and appended to the ROI's history; `undo: true` reverts the latest.
Broadcasts `rois:changed` so every open list refreshes.
"""
function api_rois_review(body_bytes::Vector{UInt8})
    body = _parse_body(body_bytes)
    body isa Tuple && return body
    pu, iu = _wstr(body, :projectUid), _wstr(body, :imageUid)
    img, err = _gating_image(pu, iu)
    err === nothing || return err
    roi_id = _wstr(body, :roiId)
    isempty(roi_id) && return _gerr(400, "roiId required")
    suffix = _rois_suffix(img, _wstr(body, :suffix))
    change = Dict{String,Any}()
    for k in ("status", "userLabel", "note", "claudeLabel", "undo")
        haskey(body, Symbol(k)) || continue
        v = body[Symbol(k)]
        change[k] = v isa AbstractDict ? JSON3.read(JSON3.write(v), Dict{String,Any}) :
                    v isa AbstractVector ? collect(v) : v
    end
    isempty(change) && return _gerr(400, "nothing to change")
    st, out = roi_review!(img, suffix, roi_id, change, author_stamp())
    st == 200 || return _gerr(st, out)
    broadcast_ws(Dict{String,Any}("type" => "rois:changed", "projectUid" => pu,
                                  "imageUid" => iu, "suffix" => suffix, "roiId" => roi_id))
    200, JSON3.write((; ok = true, roiId = roi_id, review = out))
end
