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
