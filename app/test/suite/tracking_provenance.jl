# ── Per-source tracking results (MULTI_POP_TRACKING_PLAN.md Decision 1) ──────────────────────────
#
# Several pops can be tracked on one segmentation; each row's `track_source` says which run wrote it.
# These pin what a re-track or a correction does to the OTHER sources' results, on the KDIeEm B
# fixture (tracked, with a per-track table) — sources are stamped through the sanctioned writer.

_kdieem_b_img() = begin
    lp = fixture_path("testpr", "1", "KDIeEm", "labelProps")
    td = mktempdir(); mkpath(joinpath(td, "labelProps"))
    for f in ("B.h5ad", "B__tracks.h5ad", "B__tracks.clustfeatures.json")
        cp(joinpath(lp, f), joinpath(td, "labelProps", f))
    end
    img = CciaImage(uid = "KDIeEm", dir = td)
    img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
    img
end
_have_kdieem_b() = all(have_fixture, fixture_path.("testpr", "1", "KDIeEm", "labelProps",
    ["B.h5ad", "B__tracks.h5ad", "B__tracks.clustfeatures.json"]))

# stamp `track_source` from a function of track_id (nothing = unmarked)
function _stamp_sources!(props, f)
    df = label_props(props) |> select_cols(["track_id"]) |> as_df
    vals = Any[(t isa Real && isfinite(t) && t > 0) ? f(Int(t)) : nothing for t in df.track_id]
    @test Cecelia.write_categorical_obs(props, [Cecelia.CategoricalObsColumn(;
        name = "track_source", labels = Int.(df.label), values = vals)]; on_log = _ -> nothing)
    df
end

# an HMM run's state column + fit record on the cell table; every path that drops `live.cell.*`
# must take the record with it (`prune_stale_hmmfit!`) — its readers don't check the column
function _seed_hmm_run!(props)
    df = label_props(props) |> select_cols(["track_id"]) |> as_df
    st = DataFrame("label" => Int.(df.label), "live.cell.hmm.state.mv" => fill(1.0, nrow(df)))
    label_props(props) |> add_obs(st) |> save!
    Cecelia.put_props_sidecar_entry!(Cecelia.hmm_fit_path(props), "mv", Dict("numStates" => 3))
    @test Cecelia.hmm_fit_entry(props, "mv") !== nothing
end
_hmm_run_gone(props) = !("live.cell.hmm.state.mv" in col_names(label_props(props); data_type = :obs)) &&
                       Cecelia.hmm_fit_entry(props, "mv") === nothing && !isfile(Cecelia.hmm_fit_path(props))

@testset "deleting the only track set takes the HMM fit records with the state columns" begin
    if !_have_kdieem_b()
        @test_skip "KDIeEm B fixture missing"
    else
        img = _kdieem_b_img(); props = img_label_props_path(img, "B")
        _stamp_sources!(props, _ -> "popA")
        _seed_hmm_run!(props)
        @test delete_analysis_run!(img, "tracks", "popA", "B")
        @test isempty(Cecelia.label_track_sources(img, "B"))
        @test _hmm_run_gone(props)
    end
end

# Deleting one of two track sets rebuilds the track measures for the survivor. That rewrite is a
# track-measures run: unlogged, the record dated the table to the run before the delete.
@testset "deleting one of two track sets logs the track-measures rebuild as a run" begin
    if !_have_kdieem_b()
        @test_skip "KDIeEm B fixture missing"
    else
        img = _kdieem_b_img(); props = img_label_props_path(img, "B")
        _stamp_sources!(props, t -> isodd(t) ? "popA" : "popB")
        fun = "tracking.track_measures"
        append_run_log!(img, fun, "B", "done", Dict{String,Any}("valueName" => "B", "dims" => "auto"))
        n_before = count(e -> Cecelia.run_log_row(e).fn == fun, Cecelia.read_run_log_history(img))
        @test delete_analysis_run!(img, "tracks", "popA", "B")
        srcs = values(Cecelia.label_track_sources(img, "B"))
        @test "popB" in srcs && !("popA" in srcs)
        rows = [Cecelia.run_log_row(e) for e in Cecelia.read_run_log_history(img)]
        tm = filter(r -> r.fn == fun, rows)
        @test length(tm) == n_before + 1
        @test last(tm).status == "done" && last(tm).params["valueName"] == "B"
        @test last(tm).params["dims"] == "auto"
    end
end

@testset "tracking QC counts only the run's own tracks" begin
    if !_have_kdieem_b()
        @test_skip "KDIeEm B fixture missing"
    else
        img = _kdieem_b_img(); props = img_label_props_path(img, "B")
        df = _stamp_sources!(props, t -> isodd(t) ? "popA" : "popB")
        tids = filter(t -> isfinite(t) && t > 0, df.track_id)
        nA = length(unique(filter(isodd ∘ Int, tids)))
        nAcells = count(isodd ∘ Int, tids)
        Cecelia._tracking_qc!(img, "B", props, "popA")
        m = Cecelia.read_qc(img, "tracking.bayesian_tracking", "B")["metrics"]
        @test m["nTracks"] == nA && m["nTrackedCells"] == nAcells
        @test nA < length(unique(tids))                        # popB's tracks are not counted
        # a run that formed nothing warns, even though popA/popB tracks exist
        Cecelia._tracking_qc!(img, "B", props, "popC")
        qc = Cecelia.read_qc(img, "tracking.bayesian_tracking", "B")
        @test qc["metrics"]["nTracks"] == 0
        @test any(f -> f["code"] == "tracking.no_tracks", qc["findings"])
        # one run on two pops where one formed nothing: the counts are the other's, the finding names it
        Cecelia._tracking_qc!(img, "B", props, ["popA", "popC"]; names = Dict("popC" => "/qc/c"))
        qc = Cecelia.read_qc(img, "tracking.bayesian_tracking", "B")
        @test qc["metrics"]["nTracks"] == nA
        @test any(f -> f["code"] == "tracking.no_tracks" && occursin("/qc/c", f["short"]), qc["findings"])
        Cecelia._tracking_qc!(img, "B", props, ["popA", "popB"])
        @test isempty(Cecelia.read_qc(img, "tracking.bayesian_tracking", "B")["findings"])
    end
end

@testset "a re-track removes the per-track table it renumbered" begin
    # The write-back compacts every source's track ids, so the old `B__tracks.h5ad` would join its
    # measures and track clusters onto other tracks. Whole-seg btrack run on the fixture.
    if !_have_kdieem_b()
        @test_skip "KDIeEm B fixture missing"
    else
        img = _kdieem_b_img()
        trk = img_track_props_path(img, "B")
        _seed_hmm_run!(img_label_props_path(img, "B"))
        logs = String[]
        res = Cecelia._run_task(Cecelia.BayesianTracking(), img,
            Dict{String,Any}("valueName" => "B", "minTimepoints" => 2); on_log = l -> push!(logs, l))
        @test res !== nothing || error(join(logs, "\n"))
        @test _hmm_run_gone(img_label_props_path(img, "B"))   # states dropped with `live.cell.*`, record too
        @test !isfile(trk)
        @test !isfile(Cecelia._clustfeatures_path(trk))
        # track measures rebuild it for the new ids
        @test Cecelia._run_task(Cecelia.TrackMeasures(), img,
            Dict{String,Any}("valueName" => "B"); on_log = _ -> nothing) !== nothing
        @test isfile(trk)
    end
end

@testset "a cell added in the correction editor takes its track's track_source" begin
    if !_have_kdieem_b()
        @test_skip "KDIeEm B fixture missing"
    else
        img = _kdieem_b_img(); props = img_label_props_path(img, "B")
        df = _stamp_sources!(props, t -> t == 1 ? "popA" : "popB")
        t_col = first(temporal_columns(label_props(props)))
        tt = label_props(props) |> select_cols(["track_id", t_col]) |> as_df
        held = Set(tt[tt.track_id .== 1, t_col])
        r = findfirst(i -> isfinite(tt.track_id[i]) && tt.track_id[i] != 1 && !(tt[i, t_col] in held),
                      1:nrow(tt))
        lbl = Int(tt.label[r])
        _seed_hmm_run!(props)
        res = Cecelia._run_task(Cecelia.TrackCorrect(), img, Dict{String,Any}("valueName" => "B",
            "trackOps" => [Dict("op" => "points.add", "labels" => [lbl], "trackId" => 1)]); on_log = _ -> nothing)
        @test res !== nothing
        @test _hmm_run_gone(props)            # stale `live.cell.*` (HMM states among them) and their records
        after = label_props(props) |> select_cols(["track_id", "track_source"]) |> as_df
        i = findfirst(==(lbl), Int.(after.label))
        @test after.track_id[i] == 1.0
        @test after.track_source[i] == "popA"
        # every other cell keeps its source
        @test count(==("popA"), skipmissing(after.track_source)) == count(==(1.0), tt.track_id) + 1
    end
end

@testset "one run tracks several populations, each under its own track_source" begin
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    dir = mktempdir(); cp(fixture_path("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir   # in memory only — never set_projects_dir
        img = init_object("testpr", "KDIeEm")
        props = img_label_props_path(img, "B")
        # two halves of the field (disjoint), and a pop overlapping the first
        xs  = (label_props(props) |> select_cols(["centroid_x"]) |> as_df).centroid_x
        mid = sort(xs)[length(xs) ÷ 2]
        m = load_pop_tree(img; value_name = "B", pop_type = "flow")
        add_pop!(m, "left";  filter_measure = "centroid_x", filter_fun = "lt",  filter_values = mid)
        add_pop!(m, "right"; filter_measure = "centroid_x", filter_fun = "gte", filter_values = mid)
        add_pop!(m, "all";   filter_measure = "centroid_x", filter_fun = "gte", filter_values = -1.0)
        add_pop!(m, "none";  filter_measure = "centroid_x", filter_fun = "lt",  filter_values = -1.0)
        save_pop_tree!(m, img)
        run(pops; logs = String[]) = (Cecelia._run_task(Cecelia.BayesianTracking(), img,
            Dict{String,Any}("valueName" => "B", "minTimepoints" => 2, "popsToTrack" => pops);
            on_log = l -> push!(logs, l)), logs)

        # overlapping picks are refused before anything is written
        res, logs = run(Any["B/left", "B/all"])
        @test res === nothing
        @test any(l -> occursin("'/left' and '/all' share", l), logs)

        # …and so is an empty pick (its earlier tracks would survive a "successful" run)
        res, logs = run(Any["B/left", "B/none"])
        @test res === nothing
        @test any(l -> occursin("Empty population(s), nothing to track: /none", l), logs)

        res, logs = run(Any["B/left", "B/right"])
        @test res !== nothing || error(join(logs, "\n"))
        m = load_pop_tree(img; value_name = "B", pop_type = "flow")
        uid_l, uid_r = pop_uid(m, "/left"), pop_uid(m, "/right")
        src = label_track_sources(img, "B")
        @test uid_l in values(src) && uid_r in values(src)
        # no track spans the two pops: every track_id belongs to exactly one source
        df = label_props(props) |> select_cols(["track_id"]) |> as_df
        owner = Dict{Int,Set{String}}()
        for (lbl, t) in zip(df.label, df.track_id)
            (t isa Real && isfinite(t) && t > 0) || continue
            push!(get!(owner, Int(t), Set{String}()), something(get(src, Int(lbl), nothing), ""))
        end
        @test !isempty(owner) && all(s -> length(s) == 1, values(owner))
        # QC counts both pops' tracks
        n_mine = count(s -> only(s) in (uid_l, uid_r), values(owner))
        @test n_mine > 0
        @test Cecelia.read_qc(img, "tracking.bayesian_tracking", "B")["metrics"]["nTracks"] == n_mine

        # run 2 failing after run 1 wrote back: run 1 renumbered every track id, so the per-track table
        # from before must still go. `/rt` is a slice of `/right`, whose tracks own those cells — the
        # write-back refuses them, so run 2 fails.
        m = load_pop_tree(img; value_name = "B", pop_type = "flow")
        add_pop!(m, "rt"; filter_measure = "centroid_x", filter_fun = "gte", filter_values = sort(xs)[3 * length(xs) ÷ 4])
        save_pop_tree!(m, img)
        @test Cecelia._run_task(Cecelia.TrackMeasures(), img,
            Dict{String,Any}("valueName" => "B"); on_log = _ -> nothing) !== nothing
        trk = img_track_props_path(img, "B"); @test isfile(trk)
        res, logs = run(Any["B/left", "B/rt"])
        @test res === nothing
        @test any(l -> occursin("Stopped at run 2 of 2", l), logs)
        @test !isfile(trk)
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end

@testset "tracks whose population was deleted are reported by its last path" begin
    if !_have_kdieem_b()
        @test_skip "KDIeEm B fixture missing"
    else
        img = _kdieem_b_img(); props = img_label_props_path(img, "B")
        m = load_pop_tree(img; value_name = "B", pop_type = "flow")
        add_pop!(m, "gone"; filter_measure = "centroid_x", filter_fun = "gte", filter_values = -1.0)
        add_pop!(m, "kept"; filter_measure = "centroid_x", filter_fun = "gte", filter_values = -1.0)
        save_pop_tree!(m, img)
        gone, kept = pop_uid(m, "/gone"), pop_uid(m, "/kept")
        del_pop!(m, "/gone"); save_pop_tree!(m, img)
        # a live pop, whole-seg, legacy unmarked, the deleted pop, and a uid no history state holds
        src(t) = (gone, kept, Cecelia.WHOLE_SEG_TRACK_SOURCE, nothing, "neverX")[mod(t, 5) + 1]
        df = _stamp_sources!(props, src)
        n_of(k) = count(t -> t isa Real && isfinite(t) && t > 0 && mod(Int(t), 5) == k, df.track_id)
        orph = orphan_track_sources(img, "B")
        got = Dict(o.source => o for o in orph)
        @test Set(keys(got)) == Set([gone, "neverX"])
        @test got[gone].path == "/gone" && got[gone].cells == n_of(0)
        @test got["neverX"].path === nothing && got["neverX"].cells == n_of(4)
        # the population manager's delete warning: every pop uid that authored tracks, gone or not —
        # never whole-seg or a legacy unmarked row, which no one pop owns
        @test pop_track_sources(img, "B") == sort([gone, kept, "neverX"])
        @test pop_track_sources(img, "no-such-seg") == String[]
    end
end
