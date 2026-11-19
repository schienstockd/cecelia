# ── editImages.splitTissues ─────────────────────────────────────────────────────────────────────
# Dispatch, typed params, a bad param, QC and naming. The detection itself is tested in Python
# (`python/cecelia/tests/test_tissue_split_utils.py`); the masked write was checked on a real slide
# scan (see the PR).

@testset "splitTissues — dispatch, params, QC, names" begin
    t = Cecelia._task_from_fun_name("editImages.splitTissues")
    @test t isa Cecelia.SplitTissues
    @test Cecelia.task_output_effect(t) == "new-image"
    @test Cecelia.task_previewable(t)
    @test !isnothing(Cecelia._task_spec(t))

    p = Cecelia.parse_split_tissues_params(Dict{String,Any}())
    @test p.splitNeck == 0.25 && p.thresholdAdjust == 0.3 && p.nameSuffix == "tissue"
    @test isempty(p.channels)
    p = Cecelia.parse_split_tissues_params(Dict{String,Any}(
        "channels" => ["DAPI"], "mergeGapUm" => 500, "minAreaMm2" => nothing))
    @test p.channels == Any["DAPI"] && p.mergeGapUm == 500.0 && p.minAreaMm2 == 0.1

    # the neck cut-off is a share of a width: above 1 is not a setting, it is a typo
    @test_throws Cecelia.ParamValidationError Cecelia.validate_params(t,
        Dict{String,Any}("splitNeck" => 2.0))

    # QC: nothing found is the one bad case
    f = Cecelia.split_tissues_qc_findings(0)
    @test length(f) == 1 && f[1]["level"] == "warn" && f[1]["code"] == "editImages.split_no_tissue"
    @test isempty(Cecelia.split_tissues_qc_findings(6))
    # one "tissue" over most of the image is the glass read as tissue
    f = Cecelia.split_tissues_qc_findings(1, 0.9)
    @test length(f) == 1 && f[1]["code"] == "editImages.split_whole_slide"
    @test isempty(Cecelia.split_tissues_qc_findings(7, 0.07))   # six sections on a slide

    # names follow the preview's numbers, zero-padded to the count
    @test Cecelia.split_tissue_suffix("tissue", 1, 9) == "tissue01"
    @test Cecelia.split_tissue_suffix("core", 12, 12) == "core12"
    @test Cecelia.split_tissue_suffix("", 3, 120) == "tissue003"
end
