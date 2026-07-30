# ── Crop panel HTTP routes ────────────────────────────────────────────────────────
# The two GET routes behind the in-app crop panel (docs/todo/CROP_PANEL_PLAN.md). Everything they
# stand on is generic and lives elsewhere: version resolution + geometry in `image_geometry.jl`, the
# preview render + frame cache in `image_render.jl`. What is genuinely crop-specific is only this:
# the route names, and downscaling the reported frame size to `maxPx` for the panel's canvas.

# ── HTTP routes (GET) ────────────────────────────────────────────────────────────
# Resolve (projectUid, imageUid, valueName) → (zarr_path, task_dir) using the SAME ccid.json convention
# GET /api/crop/info?projectUid=&imageUid=&valueName=&maxPx= → {nT,nZ,fullW,fullH,frameW,frameH,maxPx}
# Dimensions the panel needs: the timepoint/slice counts for the scrubber + range sliders, the displayed
# frame size, and the full-res size (Phase 2 maps a drawn rectangle back to full px from these).
function api_crop_info(req::HTTP.Request)
    q  = HTTP.queryparams(HTTP.URI(req.target))
    vn = get(q, "valueName", ""); vnn = isempty(vn) ? nothing : vn
    zp, _, err = resolve_image_version(get(q, "projectUid", ""), get(q, "imageUid", ""), vnn)
    err === nothing || return 404, JSON3.write((; error = err))
    try
        arr, caxes = open_level0(zp)
        d  = axis_dims(caxes, ndims(arr))
        fx = size(arr, d["x"]); fy = size(arr, d["y"])
        nt = haskey(d, "t") ? size(arr, d["t"]) : 1
        nz = haskey(d, "z") ? size(arr, d["z"]) : 1
        max_px = parse(Int, get(q, "maxPx", "512"))
        step = max(1, cld(max(fx, fy), max_px))
        200, JSON3.write((; nT = nt, nZ = nz, fullW = fx, fullH = fy,
                            frameW = cld(fx, step), frameH = cld(fy, step), maxPx = max_px))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end

# GET /api/crop/frame?projectUid=&imageUid=&valueName=&t=&maxPx= → PNG bytes (coloured z-MIP of frame t).
# Served as application/octet-stream (the byte-body path); the frontend wraps it in an image/png blob.
function api_crop_frame(req::HTTP.Request)
    q  = HTTP.queryparams(HTTP.URI(req.target))
    vn = get(q, "valueName", ""); vnn = isempty(vn) ? nothing : vn
    zp, td, err = resolve_image_version(get(q, "projectUid", ""), get(q, "imageUid", ""), vnn)
    err === nothing || return 404, JSON3.write((; error = err))
    t = something(tryparse(Int, get(q, "t", "0")), 0)
    max_px = something(tryparse(Int, get(q, "maxPx", "512")), 512)
    zlo = clamp(something(tryparse(Float64, get(q, "zLo", "0")), 0.0), 0.0, 1.0)   # z-range fractions:
    zhi = clamp(something(tryparse(Float64, get(q, "zHi", "1")), 1.0), 0.0, 1.0)   # project only these z
    props = _props_path(td, zp)                       # JSON layer props (napari_api._props_path)
    key = string(zp, "|", vn, "|", t, "|", max_px, "|", zlo, "|", zhi, "|", isfile(props) ? mtime(props) : 0.0)
    try
        200, cached_render!(key, () -> render_preview_frame(zp, props, t; max_px = max_px, z_lo_frac = zlo, z_hi_frac = zhi))
    catch e
        500, JSON3.write((; error = sprint(showerror, e)))
    end
end
