# ── Re-measure vs the correction composite: per-run sidecar records ───────────────────────────────
#
# A re-measure builds a fresh table, so the HMM fit records (`{props}.hmmfit.json`) and clustfeatures
# entries of runs whose columns it dropped must go — a record never outlives its column
# (docs/DATAMODEL.md). Inside `segment.correct_measures` the restore brings those columns back, and
# their records must come back with them. A carry-over snapshot left on disk by an aborted run must
# not stop a plain re-measure from pruning. Uses `_label_import_project` (label_import.jl).

function _seed_runs!(props::AbstractString)
    n = n_obs(label_props(props))
    df = label_props(props) |> v -> select_cols(v, ["label"]) |> as_df
    label_props(props) |> add_obs(DataFrame("label" => Int.(df.label),
                                            "live.cell.hmm.state.tc" => fill(1.0, n),
                                            "clusters.k1" => fill(2.0, n))) |> save!
    Cecelia.put_props_sidecar_entry!(Cecelia.hmm_fit_path(props), "tc",
                                     Dict("modelMeasurements" => ["live.cell.speed"], "numStates" => 2))
    Cecelia.put_props_sidecar_entry!(Cecelia._clustfeatures_path(props), "clusters.k1",
                                     Dict("features" => ["area"], "partOf" => String[], "labels" => Dict()))
end

@testset "carry-over sidecar helpers: save, restore, clear" begin
    mktempdir() do d
        mkpath(joinpath(d, "labelProps")); mkpath(Cecelia.task_run_dir(d))
        props = joinpath(d, "labelProps", "seg.h5ad")        # no table: the prune step is a no-op
        copy_of(k) = Cecelia._carryover_sidecar_path(d, k)
        # clear with nothing to clear — a first snapshot on a fresh image — must not throw
        @test Cecelia._carryover_clear_sidecars!(d) === nothing
        # save: a non-empty sidecar is copied verbatim; an absent one leaves no copy
        Cecelia.put_props_sidecar_entry!(Cecelia.hmm_fit_path(props), "tc", Dict("numStates" => 2))
        Cecelia._carryover_save_sidecars!(d, props)
        @test Cecelia.read_props_sidecar(copy_of("hmmfit")) == Dict{String,Any}("tc" => Dict{String,Any}("numStates" => 2))
        @test !isfile(copy_of("clustfeatures"))
        # restore: saved records come back beside the keys measureLabels kept
        rm(Cecelia.hmm_fit_path(props))
        Cecelia.put_props_sidecar_entry!(Cecelia.hmm_fit_path(props), "kept", Dict("numStates" => 4))
        Cecelia._carryover_restore_sidecars!(d, props)
        @test sort(collect(keys(Cecelia.read_props_sidecar(Cecelia.hmm_fit_path(props))))) == ["kept", "tc"]
        @test !isfile(Cecelia._clustfeatures_path(props))   # nothing saved → nothing written
        Cecelia._carryover_clear_sidecars!(d)
        @test !isfile(copy_of("hmmfit"))
    end
end

@testset "re-measure prunes run records; the correction composite keeps them" begin
    mktempdir() do dir
        img, paths = _label_import_project(dir)
        logs = String[]
        res = Cecelia._run_task(Cecelia._task_from_fun_name("segment.importLabelsMeasure"), img,
            Dict{String,Any}("labelsPath" => paths.basePath, "outputValueName" => "imp");
            on_log = l -> push!(logs, l))
        @test res !== nothing || error(join(logs, "\n"))
        props = joinpath(img._dir, "labelProps", "imp.h5ad")
        measure = Dict{String,Any}("outputValueName" => "imp")

        # (1) an aborted correction run left its snapshot behind: a plain re-measure still prunes
        _seed_runs!(props)
        mkpath(dirname(Cecelia._carryover_snapshot_path(img._dir)))
        write(Cecelia._carryover_snapshot_path(img._dir), "{}")
        @test Cecelia._run_task(Cecelia.MeasureLabels(), img, measure; on_log = _ -> nothing) !== nothing
        @test !("live.cell.hmm.state.tc" in col_names(label_props(props); data_type = :obs))
        @test Cecelia.hmm_fit_entry(props, "tc") === nothing
        @test Cecelia._clustfeatures_entry(props, "k1") === nothing

        # (2) the composite's phases: snapshot → re-measure → restore — columns and records both back
        _seed_runs!(props)
        cparams = Dict{String,Any}("valueName" => "imp")
        @test Cecelia._run_task(Cecelia.SegmentCorrectCarryOverSnapshot(), img, cparams;
                                on_log = _ -> nothing) !== nothing
        @test Cecelia._run_task(Cecelia.MeasureLabels(), img, measure; on_log = _ -> nothing) !== nothing
        @test Cecelia.hmm_fit_entry(props, "tc") === nothing          # pruned with its column…
        @test Cecelia._run_task(Cecelia.SegmentCorrectCarryOverRestore(), img, cparams;
                                on_log = _ -> nothing) !== nothing
        @test "live.cell.hmm.state.tc" in col_names(label_props(props); data_type = :obs)
        @test Cecelia.hmm_fit_entry(props, "tc")["modelMeasurements"] == ["live.cell.speed"]   # …and back
        @test Cecelia._clustfeatures_entry(props, "k1")["features"] == ["area"]
        @test !isfile(Cecelia._carryover_snapshot_path(img._dir))
        @test !any(k -> isfile(Cecelia._carryover_sidecar_path(img._dir, k)), Cecelia._CARRYOVER_SIDECARS)

        # (3) a record whose column the restore does not bring back still goes
        _seed_runs!(props)
        @test Cecelia._run_task(Cecelia.SegmentCorrectCarryOverSnapshot(), img, cparams;
                                on_log = _ -> nothing) !== nothing
        Cecelia.put_props_sidecar_entry!(Cecelia._carryover_sidecar_path(img._dir, "hmmfit"), "gone",
                                         Dict("numStates" => 3))
        @test Cecelia._run_task(Cecelia.MeasureLabels(), img, measure; on_log = _ -> nothing) !== nothing
        @test Cecelia._run_task(Cecelia.SegmentCorrectCarryOverRestore(), img, cparams;
                                on_log = _ -> nothing) !== nothing
        @test Cecelia.hmm_fit_entry(props, "gone") === nothing
        @test Cecelia.hmm_fit_entry(props, "tc") !== nothing
    end
end
