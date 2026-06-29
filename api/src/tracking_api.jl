# Tracking-related HTTP endpoints. Thin wrappers over the package (Revise-tracked, headless-tested) —
# this file only resolves images and shapes JSON. Reuses _gating_image / _resolve_vn / _gerr from
# gating_api.jl (included before this one).

# ── GET /api/tracking/motion-dims — auto 2D-vs-3D recommendation for a segmentation ──
# Powers the run-form preflight for `tracking.track_measures`: tells the user whether the z-axis
# carries real migration (3D) or only jitter (recommend 2D) BEFORE measures are computed. Cached by
# the h5ad mtime in the package, so repeated selections are instant.
function api_motion_dims(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    props = img_label_props_path(img, vn)
    isfile(props) || return _gerr(400, "no labelProps for valueName '$vn'")
    pixel_res, time_step = img_physical_sizes(img)
    try
        det = detect_motion_dims(props, pixel_res, time_step)
        return 200, JSON3.write((; dims = det.dims, zUsed = det.z_used, confidence = det.confidence,
                                   reason = det.reason, metrics = det.metrics, valueName = vn))
    catch e
        # no track_id yet (tracking not run) or unreadable — N/A, not a hard error for the preflight
        return 200, JSON3.write((; dims = 3, zUsed = true, confidence = "low",
                                   reason = "could not assess z (no tracks yet?) — " * sprint(showerror, e),
                                   metrics = Dict{String,Float64}(), valueName = vn))
    end
end
