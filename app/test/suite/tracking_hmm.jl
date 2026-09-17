# ── tracking + HMM testsets ───────────────────────────────────────────
# Five sections covering the tracking + HMM analysis pipeline: track_props (KDIeEm B),
# track_cell_measures, pop_df pop_type=track (KDIeEm B), HMM states + transitions, and
# HMM entry guards + transition state normalisation. Extracted from suite.jl to keep it
# small enough to merge without EOF conflicts on every append. The extracted file loads
# inside this file's aggregating testset scope, so any helpers defined earlier in
# suite.jl are still in scope (lexical include).

@testset "track_props (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "track_props (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # mock a categorical per-cell column to exercise the freq branch
        cells = label_props(img; value_name="B") |> select_cols(["track_id"]) |> as_df
        label_props(img_label_props_path(img, "B")) |>
            add_obs(DataFrame("label" => cells.label,
                                      "st" => [Float64((l % 2) + 1) for l in cells.label])) |> save!

        tp = track_props(img; value_name="B", cell_measures=["area", "st"], categorical=["st"])
        ntr = nrow(pop_df(img, "live", ["B/_tracked"]; granularity=:track))
        @test nrow(tp) == ntr                                  # one row per track
        @test Set(names(tp)) ⊇ Set(["track_id", "label", "num_cells"])
        @test tp.label == tp.track_id                          # engine membership key
        # numeric aggregates present
        @test Set(names(tp)) ⊇ Set(["area.mean", "area.median", "area.sum", "area.qUp", "area.qLow", "area.sd"])
        # categorical → per-category frequency columns
        @test "st.1" in names(tp) && "st.2" in names(tp)
        # motility joined from the track table
        @test "live.track.speed" in names(tp)
        # num_cells totals the tracked cells
        @test sum(tp.num_cells) == sum(c -> c > 0, Int.(filter(x -> x isa Number && !isnan(x), cells.track_id)))

        # AUTO-DETECTION (no config map; replaces R config.yml labelStats). The split is read
        # off the decoded type + values: strings and integer code sets → categorical; continuous
        # floats → numeric. Mirrors the real data: hmm.transitions "1.3", hmm.state 1/2/3, speed 10.12.
        @test Cecelia._is_categorical_col(["1.3", "2.2"])                  # String → categorical (transitions)
        @test Cecelia._is_categorical_col(["a", missing])                  # Missing-union String too
        @test Cecelia._is_categorical_col([1.0, 2.0, 3.0])                 # integer code set → categorical (hmm.state)
        @test Cecelia._is_categorical_col([1, 2, missing])                 # integer codes (Missing-union) too
        @test !Cecelia._is_categorical_col([10.12, 11.3, 9.8])             # continuous floats → numeric (speed)
        @test !Cecelia._is_categorical_col(Float64.(1:100))               # wide-spread integers → numeric (counts/area)
        # name-rule: cluster code columns are categorical regardless of level count (>cap clusters)
        @test Cecelia._is_categorical_col(Float64.(1:100), "clusters")          # exact name
        @test Cecelia._is_categorical_col(Float64.(1:100), "clusters.default")  # clusters.{suffix}
        @test !Cecelia._is_categorical_col(Float64.(1:100), "area")             # other names keep the heuristic
        # `st` is an integer code (1/2) → auto-detected categorical with NO override → freq cols
        auto = track_props(img; value_name="B", cell_measures=["st"])
        @test "st.1" in names(auto) && "st.2" in names(auto) && !("st.mean" in names(auto))
        # `numeric` escape-hatch forces it back to numeric aggregates when desired
        forced = track_props(img; value_name="B", cell_measures=["st"], numeric=["st"])
        @test "st.mean" in names(forced) && !("st.1" in names(forced))

        # An UNTRACKED segmentation → the empty, well-formed table, and SILENTLY. `track_props`
        # handles this case by design, so it must ASK (`is_tracked`, which reads the obs column list
        # only) instead of selecting `track_id` and inspecting the result: `select_cols` @warns about
        # every column it cannot find, so the by-design path logged
        # `LabelProps: ignoring unknown columns ["track_id"]` once per request — six per page load of
        # a track-grained plot panel, every one of them about a column we already knew might be absent.
        td2 = mktempdir(); mkpath(joinpath(td2, "labelProps"))
        cp(h5, joinpath(td2, "labelProps", "B.h5ad"))
        img2 = CciaImage(uid="KDIeEm", dir=td2)
        img2.label_props["B"] = "B.h5ad"; img2.label_props["_active"] = "B"
        label_props(img_label_props_path(img2, "B")) |> drop_obs(["track_id"]) |> save!
        @test !is_tracked(img2; value_name="B")
        untracked = @test_logs min_level=Logging.Warn track_props(img2; value_name="B", cell_measures=["area"])
        @test nrow(untracked) == 0
        @test Set(names(untracked)) == Set(["track_id", "num_cells", "label"])
    end
end

# ── track_cell_measures: derive base cell measures from track-property column names ──
@testset "track_cell_measures" begin
    mot = ["live.track.speed", "live.track.meanTurningAngle"]
    # motility axes need no cell aggregation
    @test isempty(track_cell_measures(["live.track.speed"], mot))
    # numeric aggregate columns → their base cell measure (suffix stripped)
    @test track_cell_measures(["mean_intensity_0.mean", "area.qUp"], mot) ==
          ["mean_intensity_0", "area"]
    # categorical frequency column `{base}.{cat}` → base
    @test track_cell_measures(["hmm.state.1"], mot) == ["hmm.state"]
    # bookkeeping + motility skipped; dedup across aggregates of the same base
    @test track_cell_measures(["track_id", "num_cells", "live.track.speed",
                               "area.mean", "area.sd"], mot) == ["area"]
end

# ── pop_df pop_type="track": gate DIRECTLY on per-track properties (3b) ────────
@testset "pop_df pop_type=track (KDIeEm B)" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "pop_df pop_type=track (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid="KDIeEm", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"

        # motility-only track props (the common track-gating case: no cell_measures needed)
        tp = track_props(img; value_name="B")
        @test "live.track.speed" in names(tp)
        spd = Float64.(collect(skipmissing(tp[!, "live.track.speed"])))
        thr = sort(spd)[cld(length(spd), 2)]                     # ~median → discriminating
        truth = count(>=(thr), spd)
        @test 0 < truth < length(spd)

        # a TRACK gate (one point per track) on the speed axis, stored under __tracks
        m = PopulationMap(pop_type="track", value_name="B")
        add_pop!(m, "fast"; gate=RectangleGate("live.track.speed", "live.track.speed",
                                               thr, 1e12, -1e12, 1e12))
        save_pop_map!(m, img)
        @test isfile(joinpath(td, "gating", "B__tracks.json"))   # track gate file
        @test !isfile(joinpath(td, "gating", "B.json"))          # NOT the flow file

        # granularity=:track → gated track rows, one point per track, gate genuinely applied
        g = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:track)
        @test nrow(g) == truth
        @test length(unique(g.track_id)) == nrow(g)
        @test unique(g.pop) == ["/fast"]
        @test all(Float64.(g[!, "live.track.speed"]) .>= thr)

        # granularity=:cell → expand gated tracks to their member cells (track pulls its cells)
        gc = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:cell)
        @test Set(names(gc)) ⊇ Set(["label", "track_id", "pop", "value_name"])
        @test Set(unique(gc.track_id)) == Set(Int.(g.track_id))  # same tracks, expanded
        @test nrow(gc) > nrow(g)                                 # many cells per track
        @test all(in(Set(Int.(g.track_id))), Int.(gc.track_id))

        # granularity=:cell + `centroids`: the member cells' COORDINATES come too. They are what the
        # track PLOTS draw — a gated or clustered track's path — and the expansion used to carry only
        # `pop_cols`, so the frame came back with no coordinates at all and `_pop_df_finish` could do
        # nothing but warn about it. Resolved per value_name (a 2D segmentation has no centroid_z).
        gcc = pop_df(img, "track", ["/fast"]; value_name="B", granularity=:cell, centroids=:pixel)
        @test all(c -> c in names(gcc), ["centroid_x", "centroid_y", "centroid_t"])
        @test nrow(gcc) == nrow(gc)
        @test Set(unique(gcc.track_id)) == Set(Int.(g.track_id))
    end
end

@testset "HMM states + transitions" begin
    # deterministic two-state tracks across two images; state flips at t=13. Track-start cells
    # carry NaN like real track measures (no speed at t=1; no angle at t=1,2) → must decode to
    # `missing` (per-cell states are undefined where a measurement can't exist).
    uID = String[]; vn = String[]; tid = Int[]; tt = Float64[]; sp = Float64[]; an = Float64[]
    for img in ("X", "Y"), k in 1:3, t in 1:25
        slow = t <= 12
        s = (slow ? 0.5 : 5.0) + 0.05 * sin(t)        # deterministic, non-degenerate
        a = (slow ? 0.2 : 2.5) + 0.05 * cos(t)
        if t == 1; s = NaN; a = NaN; elseif t == 2; a = NaN; end
        push!(uID, img); push!(vn, "A"); push!(tid, k); push!(tt, Float64(t))
        push!(sp, s); push!(an, a)
    end
    df = DataFrame("uID" => uID, "value_name" => vn, "track_id" => tid, "t" => tt,
                   "live.cell.speed" => sp, "live.cell.angle" => an)

    st = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t")
    @test length(st) == nrow(df)

    # regression: an EMPTY measure selection from the GUI arrives as `Vector{Union{}}` (not
    # Vector{String}); fit must not MethodError on the normalise/scale step.
    let stu = hmm_fit_states(df, ["live.cell.speed", "live.cell.angle"]; num_states=2, time_col="t",
                             scale_measures=Union{}[], normalise=Dict{String,String}())
        @test count(!ismissing, stu) == count(!ismissing, st)
    end
    @test count(ismissing, st) == 12                  # 2 dropped × 6 tracks (t=1 no speed+angle, t=2 no angle)
    @test Set(skipmissing(st)) == Set([1, 2])
    df[!, "live.cell.hmm.state.default"] = st

    one = st[(df.uID .== "X") .& (df.track_id .== 1)]
    @test all(ismissing, one[1:2]) && !any(ismissing, one[3:end])
    decoded = collect(skipmissing(one))
    @test count(i -> decoded[i] != decoded[i-1], 2:length(decoded)) == 1   # exactly one flip

    tr = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                         include_start=false, include_self=true)
    @test length(tr) == nrow(df)
    nonmiss = collect(skipmissing(tr))
    @test all(occursin("_", x) for x in nonmiss)
    @test Set(nonmiss) ⊆ Set(["1_1", "2_2", "1_2", "2_1"])
    @test ("1_2" in nonmiss) || ("2_1" in nonmiss)    # the flip transition exists

    trn = hmm_transitions(df, ["live.cell.hmm.state.default"]; time_col="t",
                          include_start=false, include_self=false)
    nm2 = Set(skipmissing(trn))
    @test nm2 ⊆ Set(["1_2", "2_1"]) && !isempty(nm2)  # self excluded → only the flip survives

    # cross-model hybrid: two state columns paste into "a.b" before transitions
    df[!, "live.cell.hmm.state.second"] = st
    trh = hmm_transitions(df, ["live.cell.hmm.state.default", "live.cell.hmm.state.second"];
                          time_col="t", include_start=false, include_self=true)
    @test any(x -> occursin(".", split(x, "_")[1]), skipmissing(trh))

    # cross-segmentation pops parsing: prefixed pops name their value_name ("A/_tracked" → "A",
    # the derived tracked pop = track_id>0); placeholders/empties are dropped. This is what lets
    # one run fit tracked A, B, C together (the segmentation is the pop prefix, not a separate
    # param). `_tracked` is the reserved derived-pop convention (leaf names beginning with `_`).
    @test Cecelia._hmm_pops(Dict{String,Any}("pops" => ["A/_tracked", "B/_tracked", "NONE", ""])) ==
          ["A/_tracked", "B/_tracked"]
    @test Cecelia._hmm_pops(Dict{String,Any}("pops" => "A/_tracked")) == ["A/_tracked"]
    @test Set(Cecelia._hmm_pop_value_names(["A/_tracked", "B/_tracked", "C/cd4/_tracked"], "default")) ==
          Set(["A", "B", "C"])

    # task registration + set-scope routing
    @test _task_from_fun_name("behaviour.hmm_states") isa Cecelia.HmmStates
    @test _task_from_fun_name("behaviour.hmm_transitions") isa Cecelia.HmmTransitions
    @test _task_from_fun_name("behaviour.hmm") isa Cecelia.CompositeTask
    @test task_scope(_task_from_fun_name("behaviour.hmm")) == "set"
    @test task_scope(_task_from_fun_name("behaviour.hmm_states")) == "set"
    @test task_scope(_task_from_fun_name("tracking.track_measures")) == "image"
end

# The two entry points into hmm.jl expect a `pop_df`-shaped DataFrame (uID / value_name / track_id
# / time_col / measures-or-state-cols). A rename upstream — the audit called this out for `pop_df`
# too — used to fail late with a bare `KeyError`; `_require_cols` names every missing column at once
# so a report reads "missing [t]" not "column name :t not found". The second block pins the
# read-boundary normalisation for `hmm_transitions`: state columns may arrive as Int (fresh fit) or
# String (categorical obs read-back), and both must yield the same "1" / "1.2" hybrid strings.
@testset "HMM entry guards + transition state normalisation" begin
    # A minimal well-formed DataFrame — we build DROP variants by column-subscript, because the
    # test suite only imports DataFrames.DataFrame + nrow (no `select`/`Not`).
    good = DataFrame("uID" => ["A"], "value_name" => ["V"], "track_id" => [1], "t" => [1.0],
                     "live.cell.speed" => [0.5])
    drop(df, col) = df[:, filter(!=(col), names(df))]

    # hmm_fit_states — every missing column is named in one shot (not one at a time on retry).
    err = try
        hmm_fit_states(drop(good, "live.cell.speed"), ["live.cell.speed"];
                       num_states=2, time_col="t")
        nothing
    catch e; e end
    @test err isa ErrorException
    @test occursin("hmm_fit_states", err.msg) && occursin("live.cell.speed", err.msg)

    err = try
        hmm_fit_states(drop(good, "t"), ["live.cell.speed"]; num_states=2, time_col="t")
        nothing
    catch e; e end
    @test err isa ErrorException && occursin("t", err.msg)

    # hmm_transitions — same guard, same shape.
    st_df = copy(good)
    st_df[!, "state"] = [1]
    err = try
        hmm_transitions(drop(st_df, "uID"), ["state"]; time_col="t")
        nothing
    catch e; e end
    @test err isa ErrorException && occursin("hmm_transitions", err.msg) && occursin("uID", err.msg)

    # Int and String state columns yield IDENTICAL transitions — normalisation at the read boundary
    # is what makes the composite (which reads back as categorical String) match a fresh in-memory
    # fit (Int). The float column proves the Int(round(v)) branch survives too.
    base = DataFrame("uID" => fill("A", 4), "value_name" => fill("V", 4),
                     "track_id" => fill(1, 4), "t" => [1.0, 2.0, 3.0, 4.0])
    int_df = copy(base);   int_df[!, "state"]   = [1, 1, 2, 2]
    str_df = copy(base);   str_df[!, "state"]   = ["1", "1", "2", "2"]
    flt_df = copy(base);   flt_df[!, "state"]   = [1.0, 1.0, 2.0, 2.0]
    tr_int = hmm_transitions(int_df, ["state"]; time_col="t", include_self=true)
    tr_str = hmm_transitions(str_df, ["state"]; time_col="t", include_self=true)
    tr_flt = hmm_transitions(flt_df, ["state"]; time_col="t", include_self=true)
    # First cell has no prev — always missing; the rest agree exactly across all three arms.
    @test isequal(tr_int, tr_str) && isequal(tr_int, tr_flt)
    @test collect(skipmissing(tr_int)) == ["1_1", "1_2", "2_2"]

    # Missing / NaN / "" all collapse to missing hybrid (and therefore missing transition), same
    # behaviour whether the state col is numeric or a string.
    for (col, tag) in ((Union{Int,Missing}[1, missing, 2, 2],   "Int+missing"),
                       (Union{Float64,Missing}[1.0, NaN, 2.0, 2.0], "Float+NaN"),
                       (["1", "", "2", "2"],                       "String+empty"))
        d = copy(base); d[!, "state"] = col
        tr = hmm_transitions(d, ["state"]; time_col="t", include_self=true)
        @test ismissing(tr[2])                                     # the gap breaks the chain
        @test collect(skipmissing(tr)) == ["2_2"]                  # only 3→4 survives ($tag)
    end

    # Hybrid column pastes with "." — Float second col rounds to "1"/"2", not "1.0"/"2.0".
    hyb = copy(base)
    hyb[!, "a"] = [1, 1, 2, 2]
    hyb[!, "b"] = [1.0, 1.0, 2.0, 2.0]
    tr_h = collect(skipmissing(hmm_transitions(hyb, ["a", "b"]; time_col="t", include_self=true)))
    @test tr_h == ["1.1_1.1", "1.1_2.2", "2.2_2.2"]
end
