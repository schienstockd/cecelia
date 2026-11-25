# segment.afCells — per-cell autofluorescence correction (app/src/tasks/segment/af_cells.jl).
# The correction itself is tested in python/cecelia/tests/test_cell_af_utils.py; this pins the Julia
# side: which channels share a dye, param validation, the QC findings and the registration.

using JSON3

@testset "afCells: dye groups from the acquisition record" begin
    # the real reader output for a cyclic PhenoCycler run (qptiff_meta.py), not a hand-written record
    rec = JSON3.read(read(joinpath(@__DIR__, "..", "..", "..", "test-data", "acquisition", "expected",
                                   "qptiff_fusion-2.4.0_cyclic_pages.json"), String), Dict{String,Any})
    acq = get(rec, "acquisition", rec)
    @test Cecelia.af_channel_groups(acq, 7) ==
          ["DAPI", "ATTO550", "AF750", "AF647", "ATTO550", "AF750", "AF647"]
    # fewer image channels than recorded: only those; more: the rest have no dye
    @test Cecelia.af_channel_groups(acq, 2) == ["DAPI", "ATTO550"]
    @test Cecelia.af_channel_groups(acq, 8)[8] === nothing

    # no dyePreset → the emission band, which is what the autofluorescence shares
    band = Dict{String,Any}("channels" => [Dict{String,Any}("emissionFilter" => "570-640 nm"),
                                           Dict{String,Any}("dyePreset" => "", "emissionFilter" => "")])
    @test Cecelia.af_channel_groups(band, 2) == ["570-640 nm", nothing]
    @test all(isnothing, Cecelia.af_channel_groups(nothing, 3))

    # channel order: the k-th marker with the k ± period-th; nuclear stains are taken out first
    @test Cecelia.af_order_groups(["CD4", "CD8", "CD3", "CD20", "CD68", "CD45", "CD11c"]; period = 3) ==
          ["0", "1", "2", "0", "1", "2", "0"]
    @test Cecelia.af_nuclear_channels(["Hoechst 33342", "CD4", "DRAQ5", "nuc-GFP", "Ch1_DAPI", "DNA_2"]) == [0, 2, 3, 4, 5]
    @test isempty(Cecelia.af_nuclear_channels(["CD4", "Nucleolin", "CD8"]))   # a marker, not a stain
end

@testset "afCells: params" begin
    p = Cecelia.parse_af_cells_params(Dict{String,Any}())
    @test p.groupBy == "dye" && p.period == 3 && isempty(p.exclude) && p.valueName == "default"
    p = Cecelia.parse_af_cells_params(Dict{String,Any}("valueName" => "cells", "groupBy" => "order",
                                                       "period" => 4, "exclude" => ["DAPI"]))
    @test p.valueName == "cells" && p.groupBy == "order" && p.period == 4 && p.exclude == ["DAPI"]
    @test Cecelia.parse_af_cells_params(Dict{String,Any}("nuclear" => ["DNA1"])).nuclear == ["DNA1"]
    @test_throws Cecelia.ParamValidationError Cecelia.parse_af_cells_params(Dict{String,Any}("groupBy" => "cycle"))
    @test_throws Cecelia.ParamValidationError Cecelia.parse_af_cells_params(Dict{String,Any}("period" => 1))
end

@testset "afCells: QC findings" begin
    codes(qc) = [f["code"] for f in Cecelia.af_cells_qc_findings(qc)]
    @test codes(Dict{String,Any}("nCorrected" => 0)) == ["afcells.nothing_to_correct"]
    # the validated case: markers on one dye correlated 0.41 before, 0.09 after
    @test isempty(codes(Dict{String,Any}("nCorrected" => 59, "sameDyeBefore" => 0.41, "sameDyeAfter" => 0.09)))
    # less than half removed → the grouping is suspect
    @test codes(Dict{String,Any}("nCorrected" => 59, "sameDyeBefore" => 0.41, "sameDyeAfter" => 0.30)) ==
          ["afcells.little_removed"]
    # nothing shared to begin with is not a failure
    @test isempty(codes(Dict{String,Any}("nCorrected" => 9, "sameDyeBefore" => 0.05, "sameDyeAfter" => 0.04)))
    for code in ("afcells.nothing_to_correct", "afcells.little_removed")
        @test !isempty(Cecelia.qc_text(code).short)
    end
end

@testset "afCells: registration" begin
    @test _task_from_fun_name("segment.afCells") isa Cecelia.AfCells
    @test task_scope(_task_from_fun_name("segment.afCells")) == "image"
    @test haskey(Cecelia.COHORT_METRICS, "segment.afCells")
end

@testset "afCells: end to end on a cyclic multiplex cell table" begin
    fx = fixture_for(:cells, :still, Symbol("marker-names"), :multiplex, :real;
                     technology = "widefield", need = "multiplex-cells-dyes")
    if fx === nothing
        @test_skip "no multiplex cell table with dyes yet (test-data/NEEDS.toml → multiplex-cells-dyes)"
    end
end

# A re-measure rebuilds the cell table, which used to drop the `af_` columns a gate or clustering may
# read (and leave the user to notice). It now re-applies the correction with the last run's settings and
# records that run, so the record sees corrected columns made from the new table — not stale ones.
@testset "afCells: a re-measure puts the corrected columns back" begin
    mktempdir() do dir
        # `_label_import_project`'s synthetic image, with six markers: two per "cycle" position, so channel
        # order with 2 markers per cycle gives two groups of three
        chans = join(["<Channel ID=\"Channel:0:$i\" Name=\"M$i\" SamplesPerPixel=\"1\"/>" for i in 0:5])
        py = replace(_LABEL_IMPORT_FIXTURE_PY, "T, C, Z, Y, X = 2, 2, 1, 32, 40" => "T, C, Z, Y, X = 1, 6, 1, 32, 40")
        py = replace(py, r"<Channel ID=\"Channel:0:0\".*?</Pixels>"s => chans * "\n</Pixels>")
        py = replace(py, "TimeIncrement=\"30\" TimeIncrementUnit=\"s\"" => "")
        uid = "afRem"; img_dir = joinpath(dir, "1", uid); mkpath(img_dir); data_dir = joinpath(dir, "0", uid)
        mkpath(data_dir); mkpath(joinpath(dir, "external"))
        script = joinpath(dir, "make_fixture.py"); write(script, py)
        paths = (imPath = joinpath(data_dir, "ccidImage.ome.zarr"), basePath = joinpath(dir, "external", "cells.tif"),
                 nucPath = joinpath(dir, "external", "nuclei.ome.zarr"), badPath = joinpath(dir, "external", "wrong.tif"))
        flog = String[]
        @test Cecelia.run_py(script, paths, joinpath(dir, "fixture_tasks"); on_log = l -> push!(flog, l)) ||
              error(join(flog, "\n"))
        img = CciaImage(; uid = uid, name = "afrem", dir = img_dir)
        write(state_file(img), JSON3.write(Dict{String,Any}("uID" => uid,
            "filepath" => Dict{String,Any}("default" => "ccidImage.ome.zarr", "_active" => "default"))))
        set_channel_names!(img, ["M$i" for i in 0:5])
        quiet = _ -> nothing
        @test Cecelia._run_task(Cecelia.ImportLabels(), img, Dict{String,Any}(
            "labelsPath" => paths.basePath, "outputValueName" => "cells"); on_log = quiet) !== nothing
        h5 = joinpath(img._dir, "labelProps", "cells.h5ad")
        measure = Dict{String,Any}("outputValueName" => "cells", "intensityValueName" => "default")
        n = Ref(0)
        # as the scheduler records a run: inputs + replaces, then the outcome
        function run!(task, fn, vn, p)
            tid = "t$(n[] += 1)"; pv = run_provenance(fn, img, p)
            Cecelia.open_run_log!(img, fn, vn, p; task_id = tid, inputs = pv.inputs, replaces = pv.replaces)
            log = String[]
            res = Cecelia._run_task(task, img, p; on_log = l -> push!(log, l))
            close_run_log!(img, tid, res === nothing ? "failed" : "done")
            res, log
        end
        @test run!(Cecelia.MeasureLabels(), "segment.measureLabels", "cells", measure)[1] !== nothing
        @test !Cecelia.af_cells_had_columns(h5)
        af = Dict{String,Any}("valueName" => "cells", "groupBy" => "order", "period" => 2)
        @test run!(Cecelia.AfCells(), "segment.afCells", "cells", af)[1] !== nothing
        @test Cecelia.af_cells_had_columns(h5)
        n_af = count(startswith("af_"), col_names(label_props(h5)))
        @test n_af == 6
        @test isempty(stale_outputs_for(img).stale)

        # re-measure: the columns come back, from the run's own settings, logged as their own run
        res, log = run!(Cecelia.MeasureLabels(), "segment.measureLabels", "cells", measure)
        @test res !== nothing
        @test count(startswith("af_"), col_names(label_props(h5))) == n_af
        @test any(l -> occursin("re-applying the autofluorescence correction", l), log)
        last_af = filter(e -> String(get(e, "fun", "")) == "segment.afCells", read_run_log(img))[end]
        @test last_af["status"] == "done" && last_af["params"]["groupBy"] == "order" &&
              last_af["params"]["period"] == 2
        @test isempty(stale_outputs_for(img).stale)   # made from the new table, not left over

        # a record whose settings no longer validate (an older spec) is not run blind: the user is told
        Cecelia.append_run_log!(img, "segment.afCells", "cells", "done",
                                Dict{String,Any}("valueName" => "cells", "groupBy" => "cycle"))
        res, log = run!(Cecelia.MeasureLabels(), "segment.measureLabels", "cells", measure)
        @test res !== nothing                                        # the measure itself still succeeds
        @test any(l -> occursin("no longer apply", l), log)
        @test !Cecelia.af_cells_had_columns(h5)

        # a table that never had them gets none
        rm(h5)
        @test run!(Cecelia.MeasureLabels(), "segment.measureLabels", "cells", measure)[1] !== nothing
        @test !Cecelia.af_cells_had_columns(h5)
    end
end
