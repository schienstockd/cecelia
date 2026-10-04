# ── Gating as pictures — for a client without the browser plot ────────────────────
#
# The two looks a person takes while gating, served as PNGs (base64 inside JSON, so the numbers that
# go with the picture arrive in the same response):
#
#   GET /api/gating/plot-image   — the gate plot: one population's cells on two transformed axes,
#                                  its child gates outlined + named (`render_gate_plot`).
#   GET /api/gating/cells-image  — the cells in the image: one timepoint, segmentation outlines
#                                  coloured inside vs outside a population (`render_view_stills`).
#
# The MCP autonomous server's `gate_plot` / `gate_cells_view` are the consumers; neither route writes.
# Both build on what the browser plot and the movie renderer already use: `_plot_xy`, `_axis_ticks`,
# `_child_gate_outlines` (gating_api.jl) and `MovieMask` (overlay_author.jl).

using Base64, PNGFiles

const _CELLS_IN_COLOUR  = "#ffffff"
const _CELLS_OUT_COLOUR = "#ff2bd6"

# ── GET /api/gating/plot-image ──────────────────────────────────────────────────
# ?projectUid&imageUid&valueName&x&y[&pop][&xt…&yt… as plotmeta]
# → { png, n, x:{channel, title, transform, extent}, y:{…}, gates:[{path, colour, kind, …display coords}] }
# Axes as the Gate page draws them by default: raw 0 → the WHOLE segmentation's max (so walking down
# the tree keeps the scale), grown to enclose the child gates; a gate reaching past the axes is clipped
# in the picture and stated in full in `gates`.
function api_gating_plot_image(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    x = get(q, "x", ""); y = get(q, "y", "")
    (isempty(x) || isempty(y)) && return _gerr(400, "x and y required")
    pop = get(q, "pop", ROOT)
    m = _live_map(img, vn, "flow")
    (is_root(pop) || has_pop(m, pop)) || return _gerr(404, "Population not found: $pop")
    xt = _axis_transform(q, "x"); yt = _axis_transform(q, "y")
    rxv, ryv = _plot_xy(img, vn, "flow", x, y, ROOT, xt, yt)
    isempty(rxv) && return _gerr(400, "No values for $x / $y on $vn — " * _missing_column_hint(img, vn, (x, y)))
    xv, yv = is_root(pop) ? (rxv, ryv) : _plot_xy(img, vn, "flow", x, y, pop, xt, yt)
    # the Gate page's default axes (plotmeta x0/y0=1): raw 0 → the WHOLE dataset's max, so walking
    # down the tree keeps the scale; grown to enclose the child gates, as the browser does
    rx = _finite_extrema(invert_transform.(Ref(xt), rxv)); ry = _finite_extrema(invert_transform.(Ref(yt), ryv))
    xext = (apply_transform(xt, 0.0), apply_transform(xt, float(rx[2])))
    yext = (apply_transform(yt, 0.0), apply_transform(yt, float(ry[2])))
    gates = _child_gate_outlines(m, pop, x, y, xt, yt)
    gb = _gates_bbox(gates)
    xext = _include_range(xext, gb[1], gb[2]); yext = _include_range(yext, gb[3], gb[4])
    png = render_gate_plot_png(xv, yv, xext, yext,
                               _axis_ticks(xt, invert_transform(xt, xext[1]), invert_transform(xt, xext[2])),
                               _axis_ticks(yt, invert_transform(yt, yext[1]), invert_transform(yt, yext[2])),
                               gates; xtitle = _axis_title(img, vn, "flow", x), ytitle = _axis_title(img, vn, "flow", y))
    200, JSON3.write((;
        png = base64encode(png), valueName = vn, pop = pop, n = count(isfinite, xv),
        x = (; channel = x, title = _axis_title(img, vn, "flow", x), transform = transform_spec(xt), extent = [xext[1], xext[2]]),
        y = (; channel = y, title = _axis_title(img, vn, "flow", y), transform = transform_spec(yt), extent = [yext[1], yext[2]]),
        gates = gates))
end

# What to say when a requested axis is not a cell-table column: which one, where it lives instead (a
# per-track column is on the track table, which this plot does not read), near names, and the columns
# there are — so a caller can pick a real one without another round trip.
function _missing_column_hint(img, vn, wanted)::String
    cell = _has_label_props(img) ? vcat(col_names(label_props(img; value_name = vn); data_type = :vars),
                                        col_names(label_props(img; value_name = vn); data_type = :obs)) : String[]
    track = _track_free_cols(img, vn)
    missing = [c for c in wanted if !(c in cell)]
    isempty(missing) && return "the columns have no finite values"
    notes = String[]
    for c in missing
        if c in track
            push!(notes, "`$c` is a per-track column (the track table); this plot reads the cell table")
        else
            near = filter(k -> occursin(lowercase(c), lowercase(k)), cell)
            push!(notes, "`$c` is not a column" * (isempty(near) ? "" : " (near: $(join(first(near, 5), ", ")))"))
        end
    end
    shown = first(cell, 40)
    join(notes, "; ") * ". Cell columns: " * join(shown, ", ") * (length(cell) > length(shown) ? ", …" : "")
end

# A small crop renders at its native size — a 190-px frame is too little to judge a cell's outline
# by. Nearest-neighbour upscale by a whole factor until the long side reaches `target`.
function _upscaled_png(path::AbstractString, target::Int)
    img = PNGFiles.load(path)
    k = max(1, target ÷ max(size(img)...))
    k == 1 && return read(path)
    io = IOBuffer(); PNGFiles.save(io, repeat(img; inner = (k, k))); take!(io)
end

# ── GET /api/gating/cells-image ─────────────────────────────────────────────────
# ?projectUid&imageUid&valueName&pop[&t][&channels=0,2][&imageVersion][&maxPx]
# → { png, t, sizeT, imageVersion, pop, parent, inside, outside, colours:{inside, outside} }
# One timepoint, z max-projected, in the saved viewer contrast; the segmentation's outlines at that t:
# cells of `pop` in one colour, the rest of its PARENT in another (cells outside the parent are not
# drawn). Outlines are per z-plane, so a cell spanning several planes shows as stacked rings.
# `inside`/`outside` count the cells (table rows) at that t.
function api_gating_cells_image(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    pu = get(q, "projectUid", ""); iu = get(q, "imageUid", "")
    img, err = _gating_image(pu, iu)
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    pop = get(q, "pop", ROOT)
    m = _live_map(img, vn, "flow")
    (is_root(pop) || has_pop(m, pop)) || return _gerr(404, "Population not found: $pop")
    parent = is_root(pop) ? ROOT : m.pops[pop].parent
    lp = img_labels_path(img, vn)
    isdir(lp) || return _gerr(404, "No label store on disk for $vn")

    ivn = get(q, "imageVersion", "")
    zp, td, verr = resolve_image_version(pu, iu, isempty(ivn) ? nothing : ivn)
    verr === nothing || return _gerr(404, verr)
    geo = image_geometry(zp)
    t = clamp(parse(Int, get(q, "t", string(geo.sizeT ÷ 2))), 0, geo.sizeT - 1)

    inside = Set{Int}(Int.(cells_in_pop(m, pop)))
    in_parent = Int.(cells_in_pop(m, parent))
    c_in = hex_to_rgb(_CELLS_IN_COLOUR); c_out = hex_to_rgb(_CELLS_OUT_COLOUR)
    colours = Dict{Int,RGB{N0f8}}(l => (l in inside ? c_in : c_out) for l in in_parent)
    # counts at THIS t — the label table's `centroid_t` says which cells the frame holds
    df = as_df(label_props(img; value_name = vn) |> select_cols(["label", "centroid_t"]))
    at_t = "centroid_t" in names(df) ?
           Set{Int}(Int(l) for (l, ct) in zip(df.label, df.centroid_t) if round(Int, ct) == t) :
           Set{Int}(Int.(df.label))
    n_in = count(l -> l in at_t, inside)
    n_out = count(l -> l in at_t && !(l in inside), in_parent)

    arr, caxes = open_level0(zp)
    d = axis_dims(caxes, ndims(arr))
    nc = haskey(d, "c") ? size(arr, d["c"]) : 1
    chans = [c for c in (parse(Int, s) for s in split(get(q, "channels", ""), ","; keepempty = false))
             if 0 <= c < nc]
    specs = resolved_display_specs(_props_path(td, zp), nc)
    specs === nothing && (specs = resolved_display_specs(_sampled_specs(zp, nc)))
    # The renderer takes every channel and a spec per channel; which ones show is the spec's `visible`
    # (the movie rail's convention). Asked-for channels show even if hidden in the saved viewer.
    isempty(chans) || (specs = [merge(sp, (; visible = (c - 1) in chans)) for (c, sp) in enumerate(specs)])
    chans = [c - 1 for (c, sp) in enumerate(specs) if sp.visible]
    max_px = clamp(parse(Int, get(q, "maxPx", "768")), 256, 1600)

    dir = mktempdir()
    try
        out = joinpath(dir, "cells.png")
        render_view_stills(zp, [out], [t]; channels = 0:(nc - 1), specs = specs, max_px = max_px,
                           mask = MovieMask(String(lp), colours, 1, 1.0),
                           task_dir = joinpath(dir, "task"), on_log = _ -> nothing)
        isfile(out) || return _gerr(500, "The renderer produced no image")
        200, JSON3.write((;
            png = base64encode(_upscaled_png(out, 512)), t = t, sizeT = geo.sizeT, channels = chans,
            imageVersion = isempty(ivn) ? "active" : ivn, valueName = vn, pop = pop, parent = parent,
            inside = n_in, outside = n_out,
            colours = (; inside = _CELLS_IN_COLOUR, outside = _CELLS_OUT_COLOUR)))
    catch e
        Cecelia._is_probe_code_bug(e) && rethrow()
        _gerr(500, "render failed: $(sprint(showerror, e))")
    finally
        rm(dir; recursive = true, force = true)
    end
end
