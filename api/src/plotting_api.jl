# ── Plotting API ─────────────────────────────────────────────────────────────────
#
# HTTP routes for the analysis-plot canvas (docs/UI.md). Thin wrappers (docs/ARCHITECTURE.md
# layer boundary): the aggregation is the PACKAGE function `plot_summary_data` (pop_df → bins /
# frequency counts); these handlers only parse the request, delegate, and serialise. Plot specs
# are PACKAGE JSON under app/src/plotDefinitions, served like task specs (Vue keeps no copy).
# Reuses `_gating_image` / `_resolve_vn` / `_gerr` from gating_api.jl (same include scope).

const _PLOT_SPECS_ROOT = joinpath(@__DIR__, "..", "..", "app", "src", "plotDefinitions")

# ── GET /api/plots/definitions[?module=X] — the plot-type registry ────────────────
# Flat list of plot specs (each carries its own `module`); the frontend groups by module and
# filters the per-module vs universal canvas. Optional `module` query narrows server-side.
function api_plot_definitions(req::HTTP.Request)
    q    = HTTP.queryparams(HTTP.URI(req.target))
    want = get(q, "module", "")
    specs = Any[]
    isdir(_PLOT_SPECS_ROOT) || return 200, JSON3.write(specs)
    for f in readdir(_PLOT_SPECS_ROOT; join=true)
        endswith(f, ".json") || continue
        try
            spec = JSON3.read(read(f, String), Dict{String,Any})
            (isempty(want) || string(get(spec, "module", "")) == want) && push!(specs, spec)
        catch e
            @warn "Skipping malformed plot spec" path=f exception=e
        end
    end
    200, JSON3.write(specs)
end

# ── GET /api/plots/populations — populations available across selected images/segmentations ──
# Query: projectUid, popType (default "live"), and an image selector (one of):
#   - setUid [+ imageUids? comma-sep subset]  → the set's images (optionally narrowed), OR
#   - imageUid                                → a single image.
# Returns the UNION of populations across the selected images, grouped by segmentation:
#   [{ valueName, populations: [{ path, name, colour }] }]
# This is the read-only series picker for the summary canvas — it lets the user overlay populations
# from several images AND several segmentations on one plot (docs/UI.md). Dedup by path per
# segmentation (first image wins colour/name); segmentations in first-appearance order.
function api_plot_populations(req::HTTP.Request)
    q    = HTTP.queryparams(HTTP.URI(req.target))
    proj = get(q, "projectUid", "")
    set_uid = get(q, "setUid", "")
    pop_type = get(q, "popType", "live")
    granularity = get(q, "granularity", "")
    isempty(proj) && return _gerr(400, "projectUid required")
    imgs = CciaImage[]
    if !isempty(set_uid)
        obj = try; init_object(proj, set_uid); catch e; return _gerr(400, sprint(showerror, e)); end
        obj isa CciaSet || return _gerr(400, "setUid is not a set: $set_uid")
        want_raw = get(q, "imageUids", "")
        want = isempty(want_raw) ? Set(string.(obj.image_uids)) :
               Set(string.(split(want_raw, ","; keepempty = false)))
        imgs = [im for (im, uid) in zip(obj._images, obj.image_uids) if string(uid) in want]
        isempty(imgs) && return _gerr(400, "no matching images in set $set_uid")
    else
        img, err = _gating_image(proj, get(q, "imageUid", ""))
        err === nothing || return err
        imgs = CciaImage[img]
    end

    # All the picker logic (pop_type selection by granularity, cross-image/cross-pop_type union +
    # dedup, derived-pop injection, pop_type tagging) lives in the PACKAGE (Revise-tracked, tested) —
    # this route just resolves the images and shapes the result as JSON.
    groups = plot_population_groups(
        imgs,
        img -> versioned_keys(img.label_props),
        (img, vn, pt) -> load_pop_map(img; value_name = vn, pop_type = pt),
        plot_pop_types(pop_type, granularity))
    result = [Dict("valueName" => g.value_name,
                   "populations" => [Dict("path" => p.path, "name" => p.name,
                                          "colour" => p.colour, "popType" => p.pop_type)
                                     for p in g.populations])
              for g in groups]
    200, JSON3.write(result)
end

# ── GET /api/plots/attrs — image-attribute names available across a set ────────────
# Query: projectUid, setUid [+ imageUids? comma-sep subset]. Returns the attribute names present on
# the set's images plus their distinct values: `{ attrs: [{name, values:[…]}] }`. Powers the
# "compare by attribute" scope picker on the summary canvas (group images by e.g. Treatment).
function api_plot_attrs(req::HTTP.Request)
    q    = HTTP.queryparams(HTTP.URI(req.target))
    proj = get(q, "projectUid", "")
    set_uid = get(q, "setUid", "")
    isempty(proj) && return _gerr(400, "projectUid required")
    isempty(set_uid) && return 200, JSON3.write(Dict("attrs" => []))   # single image → no cross-image attrs
    obj = try; init_object(proj, set_uid); catch e; return _gerr(400, sprint(showerror, e)); end
    obj isa CciaSet || return _gerr(400, "setUid is not a set: $set_uid")
    want_raw = get(q, "imageUids", "")
    want = isempty(want_raw) ? Set(string.(obj.image_uids)) :
           Set(string.(split(want_raw, ","; keepempty = false)))
    names = String[]; vals = Dict{String,Vector{String}}()           # first-appearance order; distinct values
    for (im, uid) in zip(obj._images, obj.image_uids)
        string(uid) in want || continue
        for (k, v) in im.attr
            ks = string(k); vs = string(v)
            haskey(vals, ks) || (push!(names, ks); vals[ks] = String[])
            (isempty(vs) || vs in vals[ks]) || push!(vals[ks], vs)
        end
    end
    200, JSON3.write(Dict("attrs" => [Dict("name" => n, "values" => sort(vals[n])) for n in names]))
end

# ── POST /api/plot_data — server-side aggregation for one summary panel ────────────
# Body: { projectUid, popType, granularity ("cell"|"track"),
#         chartType ("histogram"|"frequency"|"bar"|"boxplot"), measure, bins?, normalize?,
#         POPULATION selector (one of):
#           - series: [{valueName, pop}, …]  → one series per (segmentation, pop); lets the user
#             overlay populations from DIFFERENT segmentations on one plot, OR
#           - valueName + pops:[…]           → all pops on a single segmentation (legacy shape),
#         AND an image selector (one of):
#           - imageUid                       → single image, OR
#           - setUid [+ imageUids subset]    → CROSS-IMAGE: scope="per_image" (default; one series
#             per image) or "summarised" (pool the images). }
# Returns the `plot_summary_data` result (binEdges/categories + series; each series carries `uID`).
function api_plot_data(body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes), Dict{String,Any})
    catch
        return _gerr(400, "invalid JSON body")
    end
    proj      = string(get(body, "projectUid", ""))
    pop_type  = string(get(body, "popType", "live"))
    chart     = string(get(body, "chartType", "histogram"))
    gran      = Symbol(string(get(body, "granularity", "cell")))
    measure_v = get(body, "measure", nothing)
    measure   = measure_v === nothing ? nothing : string(measure_v)
    nbins     = Int(round(Float64(get(body, "bins", 30))))
    normalize = Bool(get(body, "normalize", false)) ? :fraction : :none
    scope     = Symbol(string(get(body, "scope", "per_image")))
    vn_req    = string(get(body, "valueName", ""))
    raw_pts   = Bool(get(body, "rawPoints", false))
    gb_v      = get(body, "groupBy", nothing)
    group_by  = (gb_v === nothing || isempty(string(gb_v))) ? nothing : string(gb_v)
    collapse  = Bool(get(body, "collapseSeries", false))   # pool pops/images → series by groupBy only

    # matrix/heatmap (chartType="matrix"): pools the whole frame into one grid (docs/PLOTS.md §9).
    # `matrixMode` profile → `measures` rows × `category` levels (z-scorable); crosstab → split the
    # `category` "from<sep>to" values into a transition matrix (normalised row/col/total or counts).
    mm_v      = get(body, "matrixMode", nothing)
    matrix_mode = (mm_v === nothing || isempty(string(mm_v))) ? nothing : string(mm_v)
    cat_v     = get(body, "category", nothing)
    category  = (cat_v === nothing || isempty(string(cat_v))) ? nothing : string(cat_v)
    measures_v = get(body, "measures", nothing)
    measures  = measures_v === nothing ? nothing : String[string(m) for m in measures_v]
    separator = string(get(body, "separator", "_"))
    zscore    = Bool(get(body, "zscore", false))
    mnorm_v   = string(get(body, "matrixNormalize", "none"))
    matrix_normalize = mnorm_v in ("row", "col", "total") ? Symbol(mnorm_v) : :none
    # group cross-image series by one or MORE image ATTRIBUTES (e.g. "Treatment", or "Treatment" ×
    # "Mouse") instead of per-image — images sharing the combined value pool into one series labelled by
    # the combination (joined with ".", mirroring the old R `paste0(axisX, ".", interaction)`). Accepts a
    # string (single) or an array (combined); docs/PLOTS.md §3.
    ga_v      = get(body, "groupAttr", nothing)
    group_attrs = ga_v === nothing ? String[] :
                  ga_v isa AbstractString ? (isempty(ga_v) ? String[] : String[String(ga_v)]) :
                  ga_v isa AbstractVector ? String[string(a) for a in ga_v if !isempty(string(a))] :
                  String[]

    gran in (:cell, :track) || return _gerr(400, "granularity must be cell or track")
    scope in (:per_image, :summarised) || return _gerr(400, "scope must be per_image or summarised")

    # population targets: explicit (value_name, pop) pairs, or the legacy single-segmentation form.
    # `targets === nothing` → use the legacy `pops` + resolved `value_name` path.
    raw_series = get(body, "series", nothing)
    targets = nothing
    if raw_series !== nothing
        targets = Tuple{String,String}[(string(get(s, "valueName", "")), string(get(s, "pop", "")))
                                       for s in raw_series]
        isempty(targets) && return _gerr(400, "series required (select populations to plot)")
        any(t -> isempty(t[1]) || isempty(t[2]), targets) &&
            return _gerr(400, "each series needs a valueName and a pop")
    else
        pops = [string(p) for p in get(body, "pops", String[])]
        isempty(pops) && return _gerr(400, "pops (or series) required (select populations to plot)")
    end

    set_uid = string(get(body, "setUid", ""))
    try
        if !isempty(set_uid)
            # CROSS-IMAGE: load the set, optionally narrow to a requested image subset (in set order).
            obj = init_object(proj, set_uid)
            obj isa CciaSet || return _gerr(400, "setUid is not a set: $set_uid")
            want = Set(string.(get(body, "imageUids", obj.image_uids)))
            pairs = [(im, uid) for (im, uid) in zip(obj._images, obj.image_uids) if uid in want]
            isempty(pairs) && return _gerr(400, "no matching images in set $set_uid")
            # uID → combined attribute value (the chosen attributes' values joined with "."; empty
            # components dropped). Images with no value for any chosen attribute fall back to their uID
            # in _series_groups. Built once, passed to the aggregation.
            attr_map = nothing
            if !isempty(group_attrs)
                attr_map = Dict{String,String}()
                for (im, uid) in pairs
                    v = join(filter(!isempty, String[string(get(im.attr, a, "")) for a in group_attrs]), ".")
                    isempty(v) || (attr_map[string(uid)] = v)
                end
            end
            result = targets === nothing ?
                plot_summary_data(first.(pairs), last.(pairs), pop_type,
                                  [string(p) for p in get(body, "pops", String[])], chart;
                                  scope = scope, value_name = isempty(vn_req) ? nothing : vn_req,
                                  granularity = gran, measure = measure, nbins = nbins,
                                  normalize = normalize, group_by = group_by, collapse_series = collapse, raw_points = raw_pts,
                                  matrix_mode = matrix_mode, measures = measures, category = category,
                                  separator = separator, zscore = zscore, matrix_normalize = matrix_normalize,
                                  attr_map = attr_map) :
                plot_summary_data(first.(pairs), last.(pairs), pop_type, targets, chart;
                                  scope = scope, granularity = gran, measure = measure,
                                  nbins = nbins, normalize = normalize, group_by = group_by,
                                  collapse_series = collapse, raw_points = raw_pts,
                                  matrix_mode = matrix_mode, measures = measures, category = category,
                                  separator = separator, zscore = zscore, matrix_normalize = matrix_normalize,
                                  attr_map = attr_map)
            return 200, JSON3.write(result)
        end
        # single image
        img, err = _gating_image(proj, string(get(body, "imageUid", "")))
        err === nothing || return err
        result = targets === nothing ?
            plot_summary_data(img, pop_type, [string(p) for p in get(body, "pops", String[])], chart;
                              value_name = _resolve_vn(img, vn_req), granularity = gran,
                              measure = measure, nbins = nbins, normalize = normalize,
                              group_by = group_by, collapse_series = collapse, raw_points = raw_pts,
                              matrix_mode = matrix_mode, measures = measures, category = category,
                              separator = separator, zscore = zscore, matrix_normalize = matrix_normalize) :
            plot_summary_data(img, pop_type, targets, chart;
                              granularity = gran, measure = measure, nbins = nbins,
                              normalize = normalize, group_by = group_by, collapse_series = collapse, raw_points = raw_pts,
                              matrix_mode = matrix_mode, measures = measures, category = category,
                              separator = separator, zscore = zscore, matrix_normalize = matrix_normalize)
        return 200, JSON3.write(result)
    catch e
        return _gerr(400, sprint(showerror, e))
    end
end

# ── Segmentation integrity (QC) plot ──────────────────────────────────────────
# POST body: { projectUid, (imageUid | setUid [+imageUids]), valueName?, measure?, chartType?,
#   perTimepoint? }. Thin wrapper over the package `segmentation_qc_data` preset — it targets the
# segmentation's root population and resolves the temporal column server-side (so the frontend
# needn't know the obsm temporal column name). Returns the same shape as /api/plot_data → PlotChart
# renders it unchanged. See docs/todo/SEGMENTATION_QC_PLOT_PLAN.md.
function api_segmentation_qc(body_bytes::Vector{UInt8})
    body = try
        JSON3.read(String(body_bytes), Dict{String,Any})
    catch
        return _gerr(400, "invalid JSON body")
    end
    proj = string(get(body, "projectUid", ""))
    isempty(proj) && return _gerr(400, "projectUid required")
    vn_v    = get(body, "valueName", nothing); vn = (vn_v === nothing || isempty(string(vn_v))) ? nothing : string(vn_v)
    m_v     = get(body, "measure", nothing);   measure = m_v === nothing ? nothing : (isempty(string(m_v)) ? nothing : string(m_v))
    ct_v    = get(body, "chartType", nothing); ct = (ct_v === nothing || isempty(string(ct_v))) ? nothing : string(ct_v)
    per_t   = Bool(get(body, "perTimepoint", false))
    set_uid  = string(get(body, "setUid", ""))
    img_uid  = string(get(body, "imageUid", ""))
    uids_raw = get(body, "imageUids", nothing)
    uids     = uids_raw === nothing ? String[] : String[string(u) for u in uids_raw]
    try
        if !isempty(set_uid)
            s = init_object(proj, set_uid)
            s isa CciaSet || return _gerr(400, "setUid is not a set: $set_uid")
            wantset = isempty(uids) ? nothing : Set(uids)
            imgs = [im for im in s._images if wantset === nothing || im.uid in wantset]
            isempty(imgs) && return _gerr(400, "no matching images in set $set_uid")
            return 200, JSON3.write(segmentation_qc_data(imgs, [im.uid for im in imgs];
                                    value_name = vn, measure = measure, chart_type = ct, per_timepoint = per_t))
        elseif !isempty(uids)
            # a run carries image UIDs but no set — load each image directly.
            imgs = CciaImage[]
            for u in uids
                obj = try init_object(proj, u) catch; continue end
                obj isa CciaImage && push!(imgs, obj)
            end
            isempty(imgs) && return _gerr(400, "no valid images in imageUids")
            return 200, JSON3.write(segmentation_qc_data(imgs, [im.uid for im in imgs];
                                    value_name = vn, measure = measure, chart_type = ct, per_timepoint = per_t))
        else
            isempty(img_uid) && return _gerr(400, "imageUid, imageUids, or setUid required")
            im = init_object(proj, img_uid)
            im isa CciaImage || return _gerr(400, "imageUid is not an image: $img_uid")
            return 200, JSON3.write(segmentation_qc_data(im;
                                    value_name = vn, measure = measure, chart_type = ct, per_timepoint = per_t))
        end
    catch e
        return _gerr(400, sprint(showerror, e))
    end
end
