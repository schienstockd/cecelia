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
# the tree keeps the scale), padded and grown to enclose child-gate edges near the data (`_display_extent`); a gate reaching past the axes is clipped
# in the picture and stated in full in `gates`.
function api_gating_plot_image(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    x = get(q, "x", ""); y = get(q, "y", "")
    (isempty(x) || isempty(y)) && return _gerr(400, "x and y required")
    pop = get(q, "pop", ROOT)
    pt = read_pop_type(img, vn, pop, "flow")     # cell gates — or `live` for a derived `_tracked` set
    m = _live_map(img, vn, pt)
    (is_root(pop) || has_pop(m, pop)) || return _pop_not_found(m, pop)
    xt = _axis_transform(q, "x"); yt = _axis_transform(q, "y")
    xt, yt = _auto_axes(q, img, vn, "flow", x, y, xt, yt)   # unparameterised → the channel's own, as on the GUI plot
    rxv, ryv = _plot_xy(img, vn, "flow", x, y, ROOT, xt, yt)
    isempty(rxv) && return _gerr(400, "No values for $x / $y on $vn — " * _missing_column_hint(img, vn, (x, y)))
    xv, yv = is_root(pop) ? (rxv, ryv) : _plot_xy(img, vn, pt, x, y, pop, xt, yt)
    # the Gate page's default axes (plotmeta x0/y0=1): raw 0 → the WHOLE dataset's max, so walking
    # down the tree keeps the scale; grown to the child gates' near edges, as the browser does
    rx = _finite_extrema(invert_transform.(Ref(xt), rxv)); ry = _finite_extrema(invert_transform.(Ref(yt), ryv))
    xext = (apply_transform(xt, 0.0), apply_transform(xt, float(rx[2])))
    yext = (apply_transform(yt, 0.0), apply_transform(yt, float(ry[2])))
    gates = _child_gate_outlines(m, pop, x, y, xt, yt)
    gx, gy = _gate_edges(gates)
    # ticks label the data range on an unpadded side, as plotmeta does (`_display_extent`)
    tick_range(t, (lo, hi), r, grew) = (grew[1] ? invert_transform(t, lo) : 0.0, grew[2] ? invert_transform(t, hi) : float(r[2]))
    xpad, xgrew = _display_extent(xext, gx); ypad, ygrew = _display_extent(yext, gy)
    xtr = tick_range(xt, xpad, rx, xgrew); ytr = tick_range(yt, ypad, ry, ygrew)
    xext, yext = xpad, ypad
    png = render_gate_plot_png(xv, yv, xext, yext,
                               _axis_ticks(xt, xtr[1], xtr[2]), _axis_ticks(yt, ytr[1], ytr[2]),
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
    track = track_table_cols(img, vn)
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
# ?projectUid&imageUid&valueName&pop[&pops=/a,/a/b][&t][&z=k|lo-hi][&region=x0,y0,x1,y1][&channels=0,2]
#  [&imageVersion][&maxPx]
# → { png, t, sizeT, z, region, imageVersion, pop, parent, inside, outside, colours:{inside, outside} }
#   or, with `pops`: { …, pops:[{path, colour, atT}] }
# One timepoint in the saved viewer contrast; the segmentation's outlines at that t. With `pop`: the
# cells of `pop` in one colour, the rest of its PARENT in another (cells outside the parent are not
# drawn). With `pops`: each population's cells in ITS OWN colour (later ones drawn over earlier, so
# list a parent before its children), nothing else outlined — the viewer's look. `z` is one plane or
# a range (its max; default the whole stack's max); `region` crops to native pixels (x1/y1 exclusive),
# upscaled so a cell's outline can be judged. Outlines are per z-plane, so a cell spanning several
# planes shows as stacked rings. Counts are cells (table rows) at that t.
function api_gating_cells_image(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    pu = get(q, "projectUid", ""); iu = get(q, "imageUid", "")
    img, err = _gating_image(pu, iu)
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    multi = [String(strip(p)) for p in split(get(q, "pops", ""), ","; keepempty = false)]
    pop = get(q, "pop", isempty(multi) ? ROOT : first(multi))
    m = _live_map(img, vn, read_pop_type(img, vn, pop, "flow"))   # `live` for a derived `_tracked` set
    for p in (isempty(multi) ? [pop] : multi)
        (is_root(p) || has_pop(m, p)) || return _pop_not_found(m, p)
    end
    lp = img_labels_path(img, vn)
    isdir(lp) || return _gerr(404, "No label store on disk for $vn")
    ivn = get(q, "imageVersion", "")
    zp, td, verr = resolve_image_version(pu, iu, isempty(ivn) ? nothing : ivn)
    verr === nothing || return _gerr(404, verr)
    geo = image_geometry(zp)
    t = clamp(parse(Int, get(q, "t", string(geo.sizeT ÷ 2))), 0, geo.sizeT - 1)
    z, zerr = _parse_z(get(q, "z", "")); zerr === nothing || return _gerr(400, zerr)
    crop, cerr = _parse_region(get(q, "region", "")); cerr === nothing || return _gerr(400, cerr)
    at_t = _labels_at_t(img, vn, t)
    reply = Dict{String,Any}("t" => t, "sizeT" => geo.sizeT, "valueName" => vn,
                             "z" => z === nothing ? nothing : [first(z), last(z)],
                             "region" => crop === nothing ? nothing : [first(crop.x), first(crop.y), last(crop.x) + 1, last(crop.y) + 1],
                             "imageVersion" => isempty(ivn) ? "active" : ivn)
    colours = Dict{Int,RGB{N0f8}}()
    if isempty(multi)
        parent = is_root(pop) ? ROOT : m.pops[pop].parent
        inside = Set{Int}(Int.(cells_in_pop(m, pop)))
        in_parent = Int.(cells_in_pop(m, parent))
        c_in = hex_to_rgb(_CELLS_IN_COLOUR); c_out = hex_to_rgb(_CELLS_OUT_COLOUR)
        for l in in_parent; colours[l] = l in inside ? c_in : c_out; end
        merge!(reply, Dict("pop" => pop, "parent" => parent,
                           "inside" => count(l -> l in at_t, inside),
                           "outside" => count(l -> l in at_t && !(l in inside), in_parent),
                           "colours" => (; inside = _CELLS_IN_COLOUR, outside = _CELLS_OUT_COLOUR)))
    else
        listed = Any[]
        for p in multi
            hex = is_root(p) ? _CELLS_IN_COLOUR : m.pops[p].colour
            c = hex_to_rgb(hex); ls = Int.(cells_in_pop(m, p))
            for l in ls; colours[l] = c; end
            push!(listed, (; path = p, colour = hex, atT = count(l -> l in at_t, ls)))
        end
        reply["pops"] = listed
    end
    png, rerr = _render_still(zp, td, t; z, crop, channels = get(q, "channels", ""),
                              mask = MovieMask(String(lp), colours, 1, 1.0),
                              max_px = clamp(parse(Int, get(q, "maxPx", "768")), 256, 1600), target = 512)
    rerr === nothing || return rerr
    reply["png"] = base64encode(png.bytes); reply["channels"] = png.channels
    200, JSON3.write(reply)
end

# `z` query → nothing (the whole stack's max) | a plane range, or why not
function _parse_z(s::AbstractString)
    isempty(s) && return (nothing, nothing)
    parts = split(s, "-")
    vals = [tryparse(Int, strip(p)) for p in parts]
    (length(vals) in (1, 2) && all(v -> v !== nothing && v >= 0, vals)) ||
        return (nothing, "z must be a plane (12) or a range (10-14)")
    lo, hi = minmax(first(vals), last(vals))
    (lo:hi, nothing)
end

# `region` query → nothing | a crop in native pixels `(; x, y)` (0-based ranges), or why not
function _parse_region(s::AbstractString)
    isempty(s) && return (nothing, nothing)
    v = [tryparse(Int, strip(p)) for p in split(s, ",")]
    (length(v) == 4 && all(!isnothing, v) && v[1] >= 0 && v[2] >= 0 && v[3] > v[1] && v[4] > v[2]) ||
        return (nothing, "region must be x0,y0,x1,y1 in image pixels with x1 > x0 and y1 > y0")
    ((; x = v[1]:(v[3] - 1), y = v[2]:(v[4] - 1)), nothing)
end

# the labels the table places at frame `t` (`centroid_t`), or every label for a still image
function _labels_at_t(img, vn, t::Int)::Set{Int}
    df = as_df(label_props(img; value_name = vn) |> select_cols(["label", "centroid_t"]))
    "centroid_t" in names(df) || return Set{Int}(Int.(df.label))
    Set{Int}(Int(l) for (l, ct) in zip(df.label, df.centroid_t) if round(Int, ct) == t)
end

# ONE still of image `zp` at frame `t` in the saved viewer contrast — the cells view's and the example
# sheet's renderer. `channels` ("0,2") shows those even if hidden in the viewer; empty = the viewer's.
# The crop is clamped to the frame. Returns `((; bytes, channels, width, height), nothing)` or
# `(nothing, error)`; the PNG is nearest-upscaled until its long side reaches `target` (0 = native).
function _render_still(zp, td, t::Int; z = nothing, crop = nothing, channels::AbstractString = "",
                       mask = nothing, max_px::Int = 0, target::Int = 0)
    arr, caxes = open_level0(zp)
    d = axis_dims(caxes, ndims(arr))
    nc = haskey(d, "c") ? size(arr, d["c"]) : 1
    if crop !== nothing
        H = haskey(d, "y") ? size(arr, d["y"]) : 1; W = haskey(d, "x") ? size(arr, d["x"]) : 1
        xr = max(0, first(crop.x)):min(W - 1, last(crop.x)); yr = max(0, first(crop.y)):min(H - 1, last(crop.y))
        (isempty(xr) || isempty(yr)) && return (nothing, _gerr(400, "region lies outside the $(W)×$(H) image"))
        crop = (; x = xr, y = yr)
    end
    chans = [c for c in (parse(Int, s) for s in split(channels, ","; keepempty = false)) if 0 <= c < nc]
    specs = _render_default_specs(_props_path(td, zp), zp, nc; max_projection = z === nothing || length(z) > 1)
    # The renderer takes every channel and a spec per channel; which ones show is the spec's `visible`
    # (the movie rail's convention). Asked-for channels show even if hidden in the saved viewer.
    isempty(chans) || (specs = [merge(sp, (; visible = (c - 1) in chans)) for (c, sp) in enumerate(specs)])
    shown = [c - 1 for (c, sp) in enumerate(specs) if sp.visible]
    dir = mktempdir()
    try
        out = joinpath(dir, "still.png")
        r = render_view_stills(zp, [out], [t]; z = z, channels = 0:(nc - 1), specs = specs, crop = crop,
                               max_px = max_px, mask = mask,
                               task_dir = joinpath(dir, "task"), on_log = _ -> nothing)
        isfile(out) || return (nothing, _gerr(500, "The renderer produced no image"))
        bytes = target > 0 ? _upscaled_png(out, target) : read(out)
        ((; bytes, channels = shown, width = r.width, height = r.height), nothing)
    catch e
        Cecelia._is_probe_code_bug(e) && rethrow()
        (nothing, _gerr(500, "render failed: $(sprint(showerror, e))"))
    finally
        rm(dir; recursive = true, force = true)
    end
end

# ── GET /api/gating/examples ────────────────────────────────────────────────────
# ?projectUid&imageUid&valueName & (pop | parent + gate=<add-gate's gate as JSON>)
#  [&mode=random|boundary][&n][&sideUm][&zHalf][&channels=0,2][&seed]
# → { png, mode, valueName, pop?, parent, gate?, n, tiles:[{row, col, side, label, t, z, x, y, distance}],
#     colours:{inside, outside} }
# Cells to LOOK at to judge a gate: each one cropped round its own centroid at its own t, z ± `zHalf`
# planes max-projected, in the saved viewer contrast, as a pair — the raw crop, then the same crop
# with that one cell outlined. A bar above each pair says which side of the gate it is on.
#   random   — `n` random cells of the population (one row).
#   boundary — the `n` cells just INSIDE the gate's edge (row 1) and the `n` just OUTSIDE it, in the
#              parent (row 2): drawn from the nearest 5 % on each side, so they are the cells the cut
#              decides. Distance is in the plot's transformed space, each axis scaled to the parent's
#              1–99 % range. Needs a gate (not a filter or boolean population).
# `gate` + `parent` instead of `pop` previews a gate that is not written (add_gate's `gate` shape).
# Reads only; every crop is a `render_view_stills` still, as the cells view.
const _EXAMPLES_MAX_N = 8

function api_gating_examples(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    # the query as a body: one reader for both (the exact-segmentation check, typed getters); `gate`
    # arrives as JSON text — a preview gate is add-gate's `gate`, which no flat query key can carry
    body = Dict{String,Any}(k => v for (k, v) in q)
    for k in ("n", "zHalf", "seed"); haskey(body, k) && (body[k] = something(tryparse(Int, body[k]), nothing)); end
    haskey(body, "sideUm") && (body["sideUm"] = something(tryparse(Float64, body["sideUm"]), nothing))
    if haskey(body, "gate")
        body["gate"] = try JSON3.read(body["gate"], Dict{String,Any}) catch; return _gerr(422, "gate must be JSON"); end
    end
    img, vn, body, err = _claude_gate_body(Vector{UInt8}(JSON3.write(body))); err === nothing || return err
    pu = _wstr(body, "projectUid"); iu = _wstr(body, "imageUid")
    mode = lowercase(_wstr(body, "mode", "boundary"))
    mode in ("random", "boundary") || return _gerr(422, "mode must be \"random\" or \"boundary\"")
    n = clamp(_wint(body, "n", 6), 1, _EXAMPLES_MAX_N)
    m = _live_map(img, vn, "flow")
    pop = _wstr(body, "pop", "")
    if !isempty(pop)
        has_pop(m, pop) || return _pop_not_found(m, pop)
        parent = m.pops[pop].parent
        gate = m.pops[pop].gate
        members = Set{Int}(Int.(cells_in_pop(m, pop)))
    else
        parent = let p = strip(_wstr(body, "parent", ROOT)); is_root(p) ? ROOT : String(p) end
        (is_root(parent) || has_pop(m, parent)) || return _pop_not_found(m, parent)
        gate, gerr = _proposed_gate(get(body, "gate", nothing), img, vn)
        gerr === nothing || return _gerr(422, "pop, or parent + gate, required: " * gerr)
        members = nothing
    end
    mode == "boundary" && gate === nothing &&
        return _gerr(422, "\"$pop\" is not a gate — boundary examples need one; use mode \"random\"")

    # the parent's cells, aligned: label + (for a gate) its two axes in the gate's own unit
    cols = gate === nothing ? ["label"] : ["label", gate.x_channel, gate.y_channel]
    v = pop_plot_cols(img, "flow", parent, cols; value_name = vn)
    labels = Int.(v[1])
    isempty(labels) && return _gerr(404, "\"$parent\" has no cells")
    ins = members !== nothing ? BitVector(l in members for l in labels) : inside(gate, v[2], v[3])
    picks = _example_picks(mode, n, ins, mode == "boundary" ? _gate_edge_distance(gate, v[2], v[3]) : nothing;
                           seed = _wint(body, "seed", 1))
    isempty(picks) && return _gerr(404, mode == "random" ? "the population has no cells" : "no cells to show")

    # where each picked cell is: pixel centroids from the label table (not the gate's µm)
    want = Set(labels[i] for (_, i, _) in picks)
    df = as_df(label_props(img; value_name = vn) |>
               select_cols(["label", "centroid_t", "centroid_z", "centroid_y", "centroid_x"]))
    pos = Dict{Int,NTuple{4,Float64}}()
    for r in eachrow(df)
        l = Int(r.label); l in want || continue
        pos[l] = (get(r, :centroid_t, 0.0), get(r, :centroid_z, 0.0), r.centroid_y, r.centroid_x)
    end
    zp, td, verr = resolve_image_version(pu, iu, nothing)
    verr === nothing || return _gerr(404, verr)
    geo = image_geometry(zp)
    um = img_is_calibrated(img) ? first(img_physical_sizes(img))[end] : 1.0
    side_um = let v = get(body, "sideUm", nothing); v isa Real && !(v isa Bool) && v > 0 ? float(v) : 15.0 end
    side_px = clamp(round(Int, side_um / um), 16, 256)
    zhalf = clamp(_wint(body, "zHalf", 2), 0, 10)
    lp = img_labels_path(img, vn)
    isdir(lp) || return _gerr(404, "No label store on disk for $vn")
    white = hex_to_rgb(_CELLS_IN_COLOUR)

    tiles = Any[]; rows = Dict{String,Vector{Any}}("inside" => Any[], "outside" => Any[])
    for (side, i, d) in picks
        l = labels[i]; haskey(pos, l) || continue
        ct, cz, cy, cx = pos[l]
        t = clamp(round(Int, ct), 0, geo.sizeT - 1); zc = round(Int, cz)
        # every crop the same size, shifted inside the frame at an edge (as the card sheets do)
        sw = min(side_px, geo.sizeX); sh = min(side_px, geo.sizeY)
        x0 = clamp(round(Int, cx) - sw ÷ 2, 0, geo.sizeX - sw); y0 = clamp(round(Int, cy) - sh ÷ 2, 0, geo.sizeY - sh)
        crop = (; x = x0:(x0 + sw - 1), y = y0:(y0 + sh - 1))
        z = max(0, zc - zhalf):min(geo.sizeZ - 1, zc + zhalf)
        ch = _wstr(body, "channels", "")
        raw, e1 = _render_still(zp, td, t; z, crop, channels = ch, target = 160)
        e1 === nothing || return e1
        ol, e2 = _render_still(zp, td, t; z, crop, channels = ch, target = 160,
                               mask = MovieMask(String(lp), Dict{Int,RGB{N0f8}}(l => white), 1, 1.0))
        e2 === nothing || return e2
        push!(rows[mode == "random" ? "inside" : side], (raw = raw.bytes, outline = ol.bytes))
        r = (mode == "random" || side == "inside") ? 1 : 2
        push!(tiles, (; row = r, col = length(rows[mode == "random" ? "inside" : side]), side, label = l, t, z = zc,
                        x = gate === nothing ? nothing : apply_transform(gate.x_transform, v[2][i]),
                        y = gate === nothing ? nothing : apply_transform(gate.y_transform, v[3][i]),
                        distance = isfinite(d) ? round(d; digits = 4) : nothing))
    end
    isempty(tiles) && return _gerr(404, "no cells to show")   # every pick lacked a centroid row
    png = _example_sheet([rows["inside"], rows["outside"]],
                         [hex_to_rgb("#22c55e"), hex_to_rgb(_CELLS_OUT_COLOUR)])
    200, JSON3.write((; png = base64encode(png), mode, valueName = vn, pop = isempty(pop) ? nothing : pop,
                        parent = is_root(parent) ? "root" : parent,
                        gate = gate === nothing ? nothing : gate_spec(gate), n = length(tiles), tiles,
                        sideUm = round(side_px * um; digits = 1), zHalf = zhalf,
                        colours = (; insideBar = "#22c55e", outsideBar = _CELLS_OUT_COLOUR, outline = _CELLS_IN_COLOUR)))
end

# Which cells a sheet shows → [(side, index, distance)], best-first per side. `random`: `n` of the
# inside cells. `boundary`: per side, `n` drawn from that side's nearest max(n, 5 %) to the edge —
# the cells the cut decides, not one tight clump — listed nearest first. Seeded, so a sheet repeats.
function _example_picks(mode::AbstractString, n::Int, ins::AbstractVector{Bool}, dist; seed::Int = 1)
    rng = Cecelia.Random.Xoshiro(seed)
    picks = Tuple{String,Int,Float64}[]
    if mode == "random"
        idx = findall(ins)
        for i in Cecelia.Random.shuffle(rng, idx)[1:min(n, length(idx))]; push!(picks, ("inside", i, NaN)); end
        return picks
    end
    for (side, want) in (("inside", true), ("outside", false))
        idx = [i for i in eachindex(ins) if ins[i] == want && isfinite(dist[i])]
        isempty(idx) && continue
        sort!(idx; by = i -> (dist[i], i))      # ties by row, so a seed always gives the same sheet
        band = idx[1:min(length(idx), max(n, ceil(Int, 0.05 * length(idx))))]
        for i in sort(Cecelia.Random.shuffle(rng, band)[1:min(n, length(band))]; by = i -> (dist[i], i))
            push!(picks, (side, i, dist[i]))
        end
    end
    picks
end

# Each cell's distance to the gate's EDGE in the transformed plane, both axes scaled to the cells' own
# 1–99 % range so neither axis dominates. Rectangle and polygon alike: the nearest edge segment.
function _gate_edge_distance(g::Gate, xraw::AbstractVector, yraw::AbstractVector)::Vector{Float64}
    xt = Float64[apply_transform(g.x_transform, float(x)) for x in xraw]
    yt = Float64[apply_transform(g.y_transform, float(y)) for y in yraw]
    function span(v)
        f = sort!(filter(isfinite, v)); isempty(f) && return 1.0
        at(p) = f[clamp(round(Int, p * (length(f) - 1)) + 1, 1, length(f))]
        max(at(0.99) - at(0.01), eps())
    end
    sx = span(xt); sy = span(yt)
    verts = g isa RectangleGate ?
        [(g.x_min, g.y_min), (g.x_max, g.y_min), (g.x_max, g.y_max), (g.x_min, g.y_max)] : g.vertices
    segs = [(verts[k], verts[k % length(verts) + 1]) for k in eachindex(verts)]
    map(eachindex(xt)) do i
        (isfinite(xt[i]) && isfinite(yt[i])) || return Inf
        px = xt[i] / sx; py = yt[i] / sy
        minimum(segs) do ((ax, ay), (bx, by))
            ax /= sx; bx /= sx; ay /= sy; by /= sy
            dx = bx - ax; dy = by - ay; L = dx^2 + dy^2
            u = L == 0 ? 0.0 : clamp(((px - ax) * dx + (py - ay) * dy) / L, 0.0, 1.0)
            hypot(px - (ax + u * dx), py - (ay + u * dy))
        end
    end
end

# Rows of (raw, outline) PNG pairs → one sheet: a coloured bar over each pair (its row's colour),
# a gap between pairs and rows. Tiles are already upscaled to the same size.
function _example_sheet(rows::AbstractVector, bars::AbstractVector)::Vector{UInt8}
    load(b) = RGB{N0f8}.(PNGFiles.load(IOBuffer(b)))
    pairs = [[(load(p.raw), load(p.outline)) for p in row] for row in rows]
    first_tile = first(first(filter(!isempty, pairs)))[1]
    th, tw = size(first_tile)
    gap = 6; bar = 6; inner = 2
    pw = 2tw + inner
    ncol = maximum(length, pairs)
    nrow = count(!isempty, pairs)
    H = nrow * (bar + th) + (nrow + 1) * gap; W = ncol * pw + (ncol + 1) * gap
    sheet = fill(RGB{N0f8}(0.08, 0.08, 0.1), H, W)
    y = gap
    for (ri, row) in enumerate(pairs)
        isempty(row) && continue
        x = gap
        for (a, b) in row
            sheet[y:(y + bar - 2), x:(x + pw - 1)] .= bars[ri]
            ya = y + bar
            ha, wa = min.(size(a), (th, tw)); hb, wb = min.(size(b), (th, tw))
            sheet[ya:(ya + ha - 1), x:(x + wa - 1)] .= a[1:ha, 1:wa]
            sheet[ya:(ya + hb - 1), (x + tw + inner):(x + tw + inner + wb - 1)] .= b[1:hb, 1:wb]
            x += pw + gap
        end
        y += bar + th + gap
    end
    io = IOBuffer(); PNGFiles.save(io, sheet); take!(io)
end
