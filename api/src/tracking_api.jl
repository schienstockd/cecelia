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

        issues = find_track_issues(df, spatial;
            gap_frames    = Int(_num("gapFrames", Float64(TRACK_GAP_MAX_FRAMES))),
            gap_steps     = _num("gapSteps", TRACK_GAP_STEPS),
            jump_factor   = _num("jumpFactor", TRACK_JUMP_FACTOR),
            jump_quantile = _num("jumpQuantile", TRACK_JUMP_QUANTILE),
            min_len       = Int(_num("minLen", Float64(MIN_USEFUL_TRACK_LENGTH))))

        # cap what crosses the wire: the worklist is worked top-down, and shipping 1000 candidates
        # with their geometry to render 20 of them is the kind of quiet cost that shows up as a slow
        # page. The total is reported so the UI can say what it is not showing.
        limit  = Int(_num("limit", 100.0))
        shown  = first(issues, max(limit, 0))

        # geometry for ONLY the tracks the shown candidates reference
        want  = Set{Int}(Iterators.flatten(i.track_ids for i in shown))
        paths = Dict{String,Any}()
        for tid in want
            rows = findall(v -> v isa Real && !isnan(v) && Int(round(Float64(v))) == tid, df.track_id)
            isempty(rows) && continue
            ord = sortperm(Float64[df[r, :centroid_t] for r in rows])
            paths[string(tid)] = Dict{String,Any}(
                "t"     => Float64[df[rows[k], :centroid_t] for k in ord],
                "x"     => Float64[df[rows[k], Symbol(spatial[1])] for k in ord],
                "y"     => length(spatial) > 1 ?
                           Float64[df[rows[k], Symbol(spatial[2])] for k in ord] : Float64[],
                "label" => Int[Int(round(Float64(df[rows[k], :label]))) for k in ord])
        end

        counts = Dict{String,Int}()
        for i in issues; counts[i.kind] = get(counts, i.kind, 0) + 1; end

        200, JSON3.write((; valueName = vn, tracked = true,
                            nTracks   = length(track_ids_present(df)),
                            stepScale = track_step_scale(df, spatial),
                            timeStep  = time_step,
                            total     = length(issues), counts = counts,
                            issues    = [issue_to_dict(i) for i in shown],
                            paths     = paths))
    catch e
        _gerr(500, "could not scan tracks: " * sprint(showerror, e))
    end
end
