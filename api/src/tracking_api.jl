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

# ── GET /api/tracking/paths — track geometry for the track plot ───────────────
# The napari tracks layer, as a plot: every track's polyline in µm, optionally coloured by one
# per-track property. Same wire shape as /api/tracking/issues' `paths` (both call `track_path_dicts`),
# so `plots/trackPaths.ts` reads either without a branch.
#
# `colorBy` is any per-track column — a motility measure from the track table, a lineage/cluster obs,
# or a cell measure the track table aggregates on read (`track_cell_measures`, e.g.
# `mean_intensity_0.mean`): the same resolution the track-grained gating axes use. This route does NOT
# return the list of them — the plot's picker reads `/api/gating/channels?popType=track`, the one the
# gating axes already read, so there is no second vocabulary to drift out of step with it.
#
# The cap is by track LENGTH, longest first: an image with thousands of tracks is unreadable as a
# hairball, and the one-or-two-point fragments are the least informative thing in it. `total` and
# `shown` both come back so the plot can say what it is leaving out rather than quietly lying.
#
# `ids=3,17,4021` names tracks explicitly and ignores the cap, so "the track I need is not in the top
# N" has an answer that is not "raise N for everybody".
function api_track_paths(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    props = img_label_props_path(img, vn)
    isfile(props) || return _gerr(400, "no labelProps for valueName '$vn'")

    _num(key, default) = (v = get(q, key, ""); isempty(v) ? default : something(tryparse(Float64, v), default))
    color_by = get(q, "colorBy", "")
    pixel_res, time_step = img_physical_sizes(img)

    try
        lp = label_props(props)
        # NOTE the parens: `() -> 200, JSON3.write(…)` parses as a TUPLE of (lambda, string), not a
        # lambda returning a tuple — a probe against real data caught it as "Tuple is not callable".
        _untracked() = (200, JSON3.write((; valueName = vn, tracked = false, paths = Dict(),
                                            total = 0, shown = 0)))
        ("track_id" in col_names(lp; data_type = :obs)) || return _untracked()

        spatial  = centroid_columns(lp; order = [:x, :y, :z])
        temporal = temporal_columns(lp)
        isempty(temporal) && return _untracked()
        select_cols(lp, vcat(spatial, temporal, ["track_id"]))
        df = as_df(lp; include_x = false, include_obs = true)
        scale_centroids!(df, pixel_res)          # µm, via the ONE shared conversion
        t_col = first(temporal)
        t_col == "centroid_t" || (df[!, :centroid_t] = df[!, Symbol(t_col)])

        # Longest first, then capped. Built from the FULL path map rather than a second pass over
        # `track_id` — "which cells are tracked" has one answer (`track_path_dicts`), and a private
        # count loop here would be a second one, free to disagree about e.g. a 0 id.
        all_paths = track_path_dicts(df, spatial)
        order = sort!(collect(keys(all_paths));
                      by = k -> (-length(all_paths[k]["t"]), parse(Int, k)))

        # `ids` names tracks explicitly and BYPASSES the cap. The cap exists so a plot of thousands of
        # tracks is not a hairball and a picker is not a 5000-row table — but "the track I need is not
        # in the top N" then has no answer at all, and raising N is not one: it makes every request
        # slower for everyone to serve one lookup. Naming the track is the answer.
        wanted = [strip(x) for x in split(get(q, "ids", ""), ',') if !isempty(strip(x))]
        if !isempty(wanted)
            ids = String[k for k in wanted if haskey(all_paths, k)]
        else
            limit = Int(_num("limit", 500.0))
            ids   = first(order, max(limit, 0))
        end
        paths = Dict{String,Any}(k => all_paths[k] for k in ids)

        # One value per shown track for the colour scale (empty when nothing is asked for).
        #
        # Only columns the per-track table provides DIRECTLY: the motility measures and the track
        # table's own obs (`clusters.{suffix}` from clustTracks). A cell measure would first have to
        # be aggregated, which means choosing WHICH aggregate — a decision this plot has no way to
        # ask about, and `track_cell_measures` throws on a name it cannot invert (a probe against real
        # data returned a 500 for a stale column). An unknown column comes back as `colorBy: ""`.
        values = Dict{String,Any}()
        color_kind = "none"
        if !isempty(color_by)
            tp = track_props(img; value_name = vn)
            if color_by in names(tp)
                col = tp[!, color_by]
                # the ONE measure-type detector (`track_props`' own) — `eltype <: Real` is not it: a
                # joined column decodes as Union{Missing,Float64}, and the probe duly reported
                # `live.track.speed` as categorical, which would paint 50 distinct colours instead of
                # a gradient
                cat = Cecelia._is_categorical_col(col, color_by)
                color_kind = cat ? "categorical" : "numeric"
                want = Set(ids)                      # String keys, as `paths` is
                for (r, tid) in enumerate(tp[!, :track_id])
                    key = string(Int(tid))
                    key in want || continue
                    v = col[r]
                    values[key] = cat ? string(v) :
                        (v isa Real && !isnan(Float64(v)) ? Float64(v) : nothing)
                end
            else
                color_by = ""
            end
        end

        200, JSON3.write(_json_safe((; valueName = vn, tracked = true,
                            total = length(order), shown = length(ids),
                            nTracks = length(order), timeStep = time_step,
                            stepScale = track_step_scale(df, spatial),
                            colorBy = color_by, colorKind = color_kind,
                            values = values, paths = paths)))
    catch e
        _gerr(500, "could not read tracks: " * sprint(showerror, e))
    end
end

# ── GET /api/tracking/diagnostics — the celltrackR QC battery ─────────────────
# MSD, velocity autocorrelation, step-angle-to-the-volume-edge, pair angle-vs-distance, and the
# Hotelling drift test — the whole battery from ONE package roll-up (`track_diagnostics_for`), which is
# the same call `tracking.track_measures` banks as QC. The plot and the finding therefore cannot
# disagree about whether an image drifts.
#
# `findings` comes back with the curves on purpose: the panel shows what the run already concluded
# rather than re-deriving a verdict in TypeScript from the numbers, which is how a second, quietly
# different threshold gets introduced.
#
# The pair cloud is DOWNSAMPLED by a stride and reports its total — 374 tracks is ~70k pairs, and
# shipping all of them to draw a scatter nobody reads at that density is a slow page for nothing.
function api_track_diagnostics(req::HTTP.Request)
    q = HTTP.queryparams(HTTP.URI(req.target))
    img, err = _gating_image(get(q, "projectUid", ""), get(q, "imageUid", ""))
    err === nothing || return err
    vn = _resolve_vn(img, get(q, "valueName", ""))
    props = img_label_props_path(img, vn)
    isfile(props) || return _gerr(400, "no labelProps for valueName '$vn'")

    _num(key, default) = (v = get(q, key, ""); isempty(v) ? default : something(tryparse(Float64, v), default))
    pixel_res, time_step = img_physical_sizes(img)

    try
        d = track_diagnostics_for(props, pixel_res;
                                 max_lag = Int(_num("maxLag", 10.0)),
                                 step_spacing = Int(_num("stepSpacing", Float64(DRIFT_STEP_SPACING))))
        d === nothing &&
            return 200, JSON3.write((; valueName = vn, tracked = false))

        cap    = Int(_num("pairLimit", 5000.0))
        pr     = d.pairs                      # a DataFrame; `api/` does not `using DataFrames`
        npairs = length(pr.angle)
        stride = max(1, cld(npairs, max(cap, 1)))
        rows   = 1:stride:npairs

        # `_json_safe` (plotting_api.jl): "not assessed" is NaN in the package and MUST be null on the
        # wire — JSON has no NaN literal, so an unassessable drift p would 500 the whole panel
        200, JSON3.write(_json_safe((; valueName = vn, tracked = true, timeStep = time_step,
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
            findings = track_diagnostic_findings(d))))
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
