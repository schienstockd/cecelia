# ── Image thumbnails — one image, or a whole set as a montage ─────────────────────
#
# Both routes draw with the ONE preview renderer (`image_render.jl`: `preview_chw` → specs →
# `composite_rgb`), from the coarsest pyramid level that is still at least `size` px on its long side,
# so a thumbnail costs one small read rather than a full-resolution one. Timepoint 0, z max-projected.
#
#   GET /api/images/thumbnail?projectUid&imageUid&valueName?&size?
#       → `{png, width, height}` — the image table's per-row thumbnail. Coloured the way the viewer
#         shows the image (its saved channel colours + contrast), else per-channel percentile contrast.
#   GET /api/sets/thumbnails?projectUid&setUid&valueName?&size?&contrast?&offset?&limit?
#       → `{png, width, height, cols, tileSize, contrast, channels, cells, total, offset, truncated}` —
#         every image of a set in one grid, for looking across a set before a batch run (the MCP
#         `get_thumbnails` tool). `contrast=shared` (default) draws every image with the SAME
#         per-channel contrast (pooled over the set) in the default palette, so a dim, saturated or empty
#         image stands out; `contrast=image` draws each the way its viewer shows it.
#
# Docs: docs/API.md → Route index.

const THUMB_DEFAULT_PX = 160
const THUMB_MAX_PX     = 512
const MONTAGE_MAX_IMAGES = 64
const MONTAGE_PAD = 4
const MONTAGE_BG  = RGB{N0f8}(0.12, 0.12, 0.12)

"""
    thumbnail_level(levels, size) -> Int

The coarsest pyramid level (0-based) whose long XY side is still ≥ `size` — the cheapest read that
does not upscale. `levels` is `store_pyramid_levels` output (C-order shapes, X last); `nothing` or
an empty list → 0.
"""
function thumbnail_level(levels, size::Int)
    (levels === nothing || isempty(levels)) && return 0
    best = 0
    for (i, l) in enumerate(levels)
        length(l.shape) >= 2 || continue
        maximum(l.shape[end-1:end]) >= size && (best = i - 1)
    end
    best
end

_thumb_size(q) = clamp(something(tryparse(Int, get(q, "size", "")), THUMB_DEFAULT_PX), 32, THUMB_MAX_PX)
_vn_or_nothing(q) = (v = get(q, "valueName", ""); isempty(v) ? nothing : String(v))

# One image's projected (C, Y, X) block at thumbnail size, through the renderer's `cached_render!` (keyed
# by the store's mtime, so a re-written store is re-read). Returns `(chw, props_path)` or `(nothing, error)`.
function _thumb_chw(project_uid::AbstractString, image_uid::AbstractString, value_name, size::Int)
    zp, td, err = resolve_image_version(project_uid, image_uid, value_name)
    err === nothing || return nothing, err
    chw = cached_render!(string("thumb-chw|", zp, "|", mtime(zp), "|", size)) do
        preview_chw(zp, 0; max_px = size, level = thumbnail_level(store_pyramid_levels(zp), size))
    end
    chw, _props_path(td, zp)
end

_png_data_url(bytes) = string("data:image/png;base64,", Base64.base64encode(bytes))

function api_image_thumbnail(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    pu = get(q, "projectUid", ""); iu = get(q, "imageUid", "")
    (isempty(pu) || isempty(iu)) && return 400, JSON3.write((; error = "projectUid and imageUid required"))
    chw, props = try
        _thumb_chw(pu, iu, _vn_or_nothing(q), _thumb_size(q))
    catch e
        return 500, JSON3.write((; error = "thumbnail failed: $(sprint(showerror, e))"))
    end
    chw === nothing && return 404, JSON3.write((; error = props))
    img = srgb_encode(composite_rgb(chw, preview_specs(chw, props)))
    200, JSON3.write((; png = _png_data_url(png_bytes(img)), width = size(img, 2), height = size(img, 1)))
end

"""
    shared_specs(chws) -> Vector

One contrast per channel index, pooled over every block in `chws` that has that channel (the same
nonzero 1st/99.9th percentile as `percentile_spec`), in the default palette — so every image in a
montage is drawn by the same rule and differences between tiles are differences in the data.
"""
function shared_specs(chws::AbstractVector)
    nc = maximum((size(c, 1) for c in chws); init = 0)
    [percentile_spec(reduce(vcat, (vec(view(c, k, :, :)) for c in chws if size(c, 1) >= k)),
                     DEFAULT_CMAPS[mod1(k, 4)]) for k in 1:nc]
end

"""
    montage_layout(n, tile) -> (cols, rows, width, height)

A near-square grid of `n` tiles of `tile` px with `MONTAGE_PAD` between and around them.
"""
function montage_layout(n::Int, tile::Int)
    cols = max(1, ceil(Int, sqrt(n)))
    rows = max(1, cld(n, cols))
    cols, rows, cols * (tile + MONTAGE_PAD) + MONTAGE_PAD, rows * (tile + MONTAGE_PAD) + MONTAGE_PAD
end

function api_set_thumbnails(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    pu = get(q, "projectUid", ""); su = get(q, "setUid", "")
    (isempty(pu) || isempty(su)) && return 400, JSON3.write((; error = "projectUid and setUid required"))
    contrast = get(q, "contrast", "shared")
    contrast in ("shared", "image") ||
        return 400, JSON3.write((; error = "contrast must be \"shared\" or \"image\""))
    set = _cohort_set(pu, su)
    set isa Tuple && return set
    tile = _thumb_size(q)
    vn = _vn_or_nothing(q)
    all_imgs = images(set)
    offset = clamp(something(tryparse(Int, get(q, "offset", "")), 0), 0, length(all_imgs))
    limit = clamp(something(tryparse(Int, get(q, "limit", "")), MONTAGE_MAX_IMAGES), 1, MONTAGE_MAX_IMAGES)
    imgs = all_imgs[offset+1:min(end, offset + limit)]
    isempty(imgs) && return 404, JSON3.write((; error = "No images in this set at offset $(offset)"))

    reads = map(imgs) do img
        try
            _thumb_chw(pu, img.uid, vn, tile)
        catch e
            nothing, sprint(showerror, e)
        end
    end
    ok = [r[1] for r in reads if r[1] !== nothing]
    # Nothing readable is a failure, not a blank montage: say why, from the first image.
    isempty(ok) && return 500, JSON3.write((; error = "no image in this set could be read: $(reads[1][2])"))
    shared = contrast == "shared" && !isempty(ok) ? shared_specs(ok) : nothing

    cols, rows, W, H = montage_layout(length(imgs), tile)
    canvas = fill(MONTAGE_BG, H, W)
    cells = NamedTuple[]
    for (i, (img, (chw, props))) in enumerate(zip(imgs, reads))
        r, c = divrem(i - 1, cols)
        push!(cells, (; index = offset + i - 1, row = r, col = c, imageUid = img.uid, name = img.name,
                        included = image_included(img), error = chw === nothing ? props : nothing))
        if chw !== nothing
            specs = shared === nothing ? preview_specs(chw, props) : shared[1:size(chw, 1)]
            t = srgb_encode(composite_rgb(chw, specs))
            th, tw = size(t)
            y0 = MONTAGE_PAD + r * (tile + MONTAGE_PAD) + (tile - th) ÷ 2
            x0 = MONTAGE_PAD + c * (tile + MONTAGE_PAD) + (tile - tw) ÷ 2
            canvas[y0+1:y0+th, x0+1:x0+tw] .= t
        end
    end

    # Channel legend for the shared palette — named from the first image that reports names.
    channels = Any[]
    if shared !== nothing
        names = String[]
        for img in imgs
            ns = try something(channel_names(img; value_name = vn), String[]) catch; String[] end
            isempty(ns) || (names = ns; break)
        end
        for (k, s) in enumerate(shared)
            push!(channels, (; index = k - 1, colour = s[3], name = k <= length(names) ? names[k] : "",
                               lo = s[1], hi = s[2]))
        end
    end

    total = length(all_imgs)
    200, JSON3.write((; png = _png_data_url(png_bytes(canvas)), width = W, height = H, cols, rows,
                        tileSize = tile, contrast, channels, cells, total, offset,
                        truncated = offset + length(imgs) < total))
end
