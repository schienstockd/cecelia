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

# ── GET /api/tracking/issues — the correction worklist ────────────────────────
# What looks wrong in a tracking result, ranked, each row carrying the op that would fix it
# (docs/todo/CORRECTION_PLAN.md → P4). The detector is in the package (`find_track_issues`), so this
# only resolves the image and shapes JSON.
#
# It ALSO returns the path geometry for each candidate's tracks, because nothing else can: there is no
# track-path plotting in the frontend at all (tracks are viewed in napari — `frontend/src/lib/tips.ts`
# says so explicitly), so a worklist row that only carries numbers cannot show the user the one thing
# they have to judge — do these two pieces of track look like one cell. Sending the polylines with the
# candidate keeps it a single request per page load rather than one per row.
#
# Read-only. Finding an issue never changes anything; applying the fix is `tracking.correct`.
function api_track_issues(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    props = img_label_props_path(img, vn)
    isfile(props) || return _gerr(400, "no labelProps for valueName '$vn'")

    _num(key, default) = (v = get(q, key, ""); isempty(v) ? default : something(tryparse(Float64, v), default))
    pixel_res, time_step = img_physical_sizes(img)

    try
        lp = label_props(props)
        ("track_id" in col_names(lp; data_type = :obs)) ||
            return 200, JSON3.write((; valueName = vn, tracked = false, issues = [], paths = Dict()))

        spatial  = centroid_columns(lp; order = [:x, :y, :z])
        temporal = temporal_columns(lp)
        isempty(temporal) &&
            return 200, JSON3.write((; valueName = vn, tracked = false, issues = [], paths = Dict()))
        select_cols(lp, vcat(spatial, temporal, ["track_id"]))
        df = as_df(lp; include_x = false, include_obs = true)
        scale_centroids!(df, pixel_res)          # µm, via the ONE shared conversion
        t_col = first(temporal)
        t_col == "centroid_t" || (df[!, :centroid_t] = df[!, Symbol(t_col)])

        # The thresholds are resolved ONCE and echoed back in the response. The panel seeds its knobs
        # from what the server used and sends only what the user moved, so the measured defaults live
        # exactly here — on the Julia constants — and are never copied into TypeScript to drift.
        thr = (; gapFrames    = Int(_num("gapFrames", Float64(TRACK_GAP_MAX_FRAMES))),
                 gapSteps     = _num("gapSteps", TRACK_GAP_STEPS),
                 jumpFactor   = _num("jumpFactor", TRACK_JUMP_FACTOR),
                 jumpQuantile = _num("jumpQuantile", TRACK_JUMP_QUANTILE),
                 minLen       = Int(_num("minLen", Float64(MIN_USEFUL_TRACK_LENGTH))))

        issues = find_track_issues(df, spatial;
            gap_frames    = thr.gapFrames,
            gap_steps     = thr.gapSteps,
            jump_factor   = thr.jumpFactor,
            jump_quantile = thr.jumpQuantile,
            min_len       = thr.minLen)

        # cap what crosses the wire: the worklist is worked top-down, and shipping 1000 candidates
        # with their geometry to render 20 of them is the kind of quiet cost that shows up as a slow
        # page. The total is reported so the UI can say what it is not showing.
        limit  = Int(_num("limit", 100.0))
        shown  = first(issues, max(limit, 0))

        # geometry for ONLY the tracks the shown candidates reference (same wire shape as
        # /api/tracking/paths — one helper, `track_path_dicts`, so the two cannot drift apart)
        paths = track_path_dicts(df, spatial;
                                 ids = Set{Int}(Iterators.flatten(i.track_ids for i in shown)))

        counts = Dict{String,Int}()
        for i in issues; counts[i.kind] = get(counts, i.kind, 0) + 1; end

        200, JSON3.write(_json_safe((; valueName = vn, tracked = true,
                            nTracks   = length(track_ids_present(df)),
                            stepScale = track_step_scale(df, spatial),
                            timeStep  = time_step,
                            total     = length(issues), counts = counts, thresholds = thr,
                            issues    = [issue_to_dict(i) for i in shown],
                            paths     = paths)))
    catch e
        _gerr(500, "could not scan tracks: " * sprint(showerror, e))
    end
end

# ── The two track PLOTS: one cohort vocabulary, two readouts ──────────────────
#
# `/api/tracking/paths` and `/api/tracking/diagnostics` are both plots on the Analysis board, and a board
# compares things: treatments side by side, populations side by side. They therefore take the SAME
# selectors the summary aggregator takes (`POST /api/plot_data`), in the query form these GET routes
# already speak:
#
#   imageUids=a,b,c        the board's selected images (`imageUid=` alone still works, unchanged)
#   groupAttr=Treatment    image ATTRIBUTES to group by — images sharing the combined value pool into one
#                          group labelled by it, the same join as the summary canvas
#   poolImages=1           no attribute, but pool every image into one group (compare = "pooled")
#   popType=live&pops=B/qc/_tracked,B/qc/other   the population family + value-name-prefixed refs
#   poolPops=1             the selected populations as ONE group instead of one each
#   maxGroups=12           cap on the returned groups (the cost is linear in them)
#
# Everything above resolves in the PACKAGE (`app/src/tracking/track_cohort.jl` → `track_plot_groups`),
# which is where the data work belongs: this layer only names JSON fields. Each response is a list of
# `groups`, and each group carries exactly what the single-image response used to carry at its top level.
#
# NOTE the deliberate asymmetry between the two: paths keep a group's images APART (a track is labelled
# by the movie it came from), diagnostics POOL them (a condition is judged on all its replicates). Both
# are `track_cohort.jl`'s job, not a branch here.

# Shared parse: the group selectors → resolved `TrackPlotGroup`s. Returns `(groups, dropped, vn, nothing)`
# or `(nothing, 0, "", err)` — including for a THROW inside the resolver, which reads cell tables and so
# can fail on a stale column or an unreadable file. A panel must get its own message, not the server's
# generic 500 page.
function _track_plot_groups(q)
    proj = get(q, "projectUid", "")
    uids = String[strip(x) for x in split(get(q, "imageUids", ""), ',') if !isempty(strip(x))]
    isempty(uids) && (uids = String[strip(x) for x in split(get(q, "imageUid", ""), ',') if !isempty(strip(x))])
    isempty(uids) && return (nothing, 0, "", _gerr(400, "imageUid or imageUids required"))
    imgs = Any[]
    for u in uids
        img, err = _gating_image(proj, u)
        err === nothing || return (nothing, 0, "", err)
        push!(imgs, img)
    end
    _csv(key) = String[strip(x) for x in split(get(q, key, ""), ',') if !isempty(strip(x))]
    _flag(key) = get(q, key, "") in ("1", "true")
    max_groups = something(tryparse(Int, get(q, "maxGroups", "")), 12)
    try
        groups, dropped, vn = track_plot_groups(imgs, uids;
            group_attrs = _csv("groupAttr"), pool_images = _flag("poolImages"),
            pops = _csv("pops"), pop_type = get(q, "popType", "live"),
            value_name = get(q, "valueName", ""), pool_pops = _flag("poolPops"),
            max_groups = max(max_groups, 1))
        (groups, dropped, vn, nothing)
    catch e
        (nothing, 0, "", _gerr(500, "could not read the tracks to plot: " * sprint(showerror, e)))
    end
end

# The group's identity, on every group of every response — so the frontend labels, colours and facets
# from one place rather than re-deriving "which image / which population" per plot.
_track_group_meta(g) = (; key = g.key, label = g.label, imageUids = track_group_images(g),
                         valueName = track_group_value_name(g), pop = track_group_pop(g),
                         popType = g.pop_type, nSources = length(g.sources),
                         timeStep = g.time_step)

# ── GET /api/tracking/paths — track geometry for the track plot ───────────────
# The napari tracks layer, as a plot: every track's polyline in µm, optionally coloured by one
# per-track property. Same wire shape as /api/tracking/issues' `paths` (both call `track_path_dicts`),
# so `plots/trackPaths.ts` reads either without a branch — per GROUP since the cohort selectors above.
#
# `colorBy` is any per-track column — a motility measure from the track table, a lineage/cluster obs,
# or a cell measure the track table aggregates on read (`track_cell_measures`, e.g.
# `mean_intensity_0.mean`): the same resolution the track-grained gating axes use. This route does NOT
# return the list of them — the plot's picker reads `/api/gating/channels?popType=track`, the one the
# gating axes already read, so there is no second vocabulary to drift out of step with it.
#
# The cap is by track LENGTH, longest first, and is PER GROUP: an image with thousands of tracks is
# unreadable as a hairball, and the one-or-two-point fragments are the least informative thing in it.
# `total` and `shown` both come back, per group and summed, so the plot can say what it is leaving out
# rather than quietly lying.
#
# `ids=3,17,4021` names tracks explicitly and ignores the cap, so "the track I need is not in the top
# N" has an answer that is not "raise N for everybody".
function api_track_paths(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    groups, dropped, vn, err = _track_plot_groups(q)
    err === nothing || return err
    isempty(groups) &&
        return 200, JSON3.write((; valueName = vn, tracked = false, groups = [], dropped = dropped,
                                   total = 0, shown = 0, colorBy = "", colorKind = "none"))

    limit = something(tryparse(Int, get(q, "limit", "")), 500)
    ids   = String[strip(x) for x in split(get(q, "ids", ""), ',') if !isempty(strip(x))]
    color_by = get(q, "colorBy", "")
    try
        out = Any[]
        eff_color, eff_kind = "", "none"
        for g in groups
            p = track_group_paths(g; limit = limit, ids = ids, color_by = color_by)
            # a column this image lacks falls back to uncoloured for THAT group; the top-level answer is
            # what the picker should show, so it reports the colour any group actually resolved
            if p.color_kind != "none" && eff_kind == "none"
                eff_color, eff_kind = p.color_by, p.color_kind
            end
            push!(out, (; _track_group_meta(g)..., tracked = true,
                          total = p.total, shown = p.shown, stepScale = p.step_scale,
                          colorBy = p.color_by, colorKind = p.color_kind,
                          values = p.values, paths = p.paths))
        end
        200, JSON3.write(_json_safe((; valueName = vn, tracked = true, groups = out,
                            dropped = dropped,
                            total = sum(g -> g.total, out), shown = sum(g -> g.shown, out),
                            colorBy = eff_color, colorKind = eff_kind)))
    catch e
        _gerr(500, "could not read tracks: " * sprint(showerror, e))
    end
end

# ── GET /api/tracking/diagnostics — the celltrackR QC battery ─────────────────
# MSD, velocity autocorrelation, step-angle-to-the-volume-edge, pair angle-vs-distance, and the
# Hotelling drift test — the whole battery from ONE package roll-up (`track_diagnostics`), which is
# the same call `tracking.track_measures` banks as QC. The plot and the finding therefore cannot
# disagree about whether an image drifts.
#
# `findings` comes back with the curves on purpose: the panel shows what the run already concluded
# rather than re-deriving a verdict in TypeScript from the numbers, which is how a second, quietly
# different threshold gets introduced.
#
# Cohort-shaped like the paths route above (see the shared header): one entry per group, each group's
# images POOLED — a condition is judged on all of its replicates, and every diagnostic here is per-track
# or per-step arithmetic, so pooling is the concatenation. The one exception is handled where it belongs:
# `track_group_diagnostics` keeps the pair scan inside one movie.
#
# The pair cloud is DOWNSAMPLED by a stride and reports its total — 374 tracks is ~70k pairs, and
# shipping all of them to draw a scatter nobody reads at that density is a slow page for nothing. The cap
# is per group, so a two-arm plot is not half a plot.
function api_track_diagnostics(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    groups, dropped, vn, err = _track_plot_groups(q)
    err === nothing || return err
    isempty(groups) &&
        return 200, JSON3.write((; valueName = vn, tracked = false, groups = [], dropped = dropped))

    _num(key, default) = (v = get(q, key, ""); isempty(v) ? default : something(tryparse(Float64, v), default))
    cap = Int(_num("pairLimit", 5000.0))
    try
        out = Any[]
        for g in groups
            d = track_group_diagnostics(g; max_lag = Int(_num("maxLag", 10.0)),
                                          step_spacing = Int(_num("stepSpacing", Float64(DRIFT_STEP_SPACING))))
            d === nothing && continue
            pr     = d.pairs                      # a DataFrame; `api/` does not `using DataFrames`
            npairs = length(pr.angle)
            stride = max(1, cld(npairs, max(cap, 1)))
            rows   = 1:stride:npairs
            push!(out, (; _track_group_meta(g)..., tracked = true,
                nTracks = d.summary.nTracks,
                msd  = (; lag = d.msd.lag, value = d.msd.msd, sem = d.msd.sem, n = d.msd.n),
                acor = (; lag = d.acor.lag, value = d.acor.acor, sem = d.acor.sem, n = d.acor.n),
                plane = (; distance = d.plane.profile.distance, angle = d.plane.profile.angle,
                           expected = PLANE_ANGLE_UNBIASED,
                           angleNear = d.plane.verdict.mean_angle_near,
                           angleFar  = d.plane.verdict.mean_angle_far,
                           suspect   = d.plane.verdict.suspect),
                pairs = (; angle = Float64[pr.angle[r] for r in rows],
                           distance = Float64[pr.distance[r] for r in rows],
                           shown = length(rows), total = npairs,
                           meanAngleFar = d.drift.pairs.mean_angle_far,
                           drifting = d.drift.pairs.drifting,
                           skipped = d.summary.pairsSkipped, maxTracks = PAIR_SCAN_MAX_TRACKS),
                drift = (; p = d.drift.test.p, statistic = d.drift.test.statistic, n = d.drift.test.n,
                           meanStep = d.drift.test.mean_step, drifting = d.drift.test.drifting,
                           stepSpacing = d.drift.test.step_spacing, alpha = DRIFT_ALPHA),
                summary = (; msdSlope = d.summary.msdSlope, motionKind = d.summary.motionKind,
                             persistenceLag = d.summary.persistenceLag,
                             nDuplicatePairs = d.summary.nDuplicatePairs),
                findings = track_diagnostic_findings(d)))
        end
        # `_json_safe` (plotting_api.jl): "not assessed" is NaN in the package and MUST be null on the
        # wire — JSON has no NaN literal, so an unassessable drift p would 500 the whole panel
        200, JSON3.write(_json_safe((; valueName = vn, tracked = !isempty(out),
                                       groups = out, dropped = dropped)))
    catch e
        _gerr(500, "could not compute track diagnostics: " * sprint(showerror, e))
    end
end

# ── GET /api/tracking/selection — what is selected in napari, as TRACKS ───────
# The bridge from "I can see that track is wrong" to an edit. Drawing a region in napari already
# stores the enclosed label ids as the transient selection (`POST /api/napari/event` →
# `_set_napari_selection!`); this resolves them to the TRACKS those cells belong to, which is the
# vocabulary the correction ops speak.
#
# Without it the correction surface only answered "fix what the detector found". Finding the track you
# can SEE meant reading its id off the viewer and hunting for it in a table — the exact chore the
# worklist exists to remove, reintroduced for the case the detector misses.
#
# Read-only, and cheap: the selection is in memory and the cell table is read for one obs column.
function api_track_selection(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))

    labels = _get_napari_selection(img._dir, vn)
    (labels === nothing || isempty(labels)) &&
        return 200, JSON3.write((; valueName = vn, labels = Int[], tracks = [],
                                   nLabels = 0, nUntracked = 0))

    props = img_label_props_path(img, vn)
    isfile(props) || return _gerr(400, "no labelProps for valueName '$vn'")
    try
        lp = label_props(props)
        ("track_id" in col_names(lp; data_type = :obs)) ||
            return 200, JSON3.write((; valueName = vn, labels = labels, tracks = [],
                                       nLabels = length(labels), nUntracked = length(labels)))
        select_cols(lp, ["track_id"])
        df = as_df(lp; include_x = false, include_obs = true)

        want = Set{Int}(labels)
        counts = Dict{Int,Int}()
        n_untracked = 0
        # `length(df.label)`, not `nrow` — `api/` does not `using DataFrames`, and the empty-selection
        # path returns before this line, so the 500 would only appear once someone actually drew
        for r in 1:length(df.label)
            Int(round(Float64(df[r, :label]))) in want || continue
            v = df[r, :track_id]
            # the same "is this tracked" rule the rest of the tracking code uses — a 0 or NaN id is
            # a cell with no track, and those are exactly the ones `points.add` exists for
            if v isa Real && !isnan(Float64(v)) && Float64(v) > 0
                tid = Int(round(Float64(v)))
                counts[tid] = get(counts, tid, 0) + 1
            else
                n_untracked += 1
            end
        end
        # most-represented track first: the one the user drew around is the one with the most cells
        # inside the region, not the one with the lowest id
        order = sort!(collect(keys(counts)); by = t -> (-counts[t], t))
        200, JSON3.write((; valueName = vn, labels = labels,
                            tracks = [(; track = t, nCells = counts[t]) for t in order],
                            nLabels = length(labels), nUntracked = n_untracked))
    catch e
        _gerr(500, "could not resolve the selection: " * sprint(showerror, e))
    end
end
