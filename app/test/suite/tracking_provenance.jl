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
