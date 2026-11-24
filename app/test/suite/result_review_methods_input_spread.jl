# RESULT-REVIEW: result-6eda6c11
# The version a step READ is a setting like any other: one image segmented on its drift-corrected
# version and one on raw is stated as both, never as the newest run's input for the whole set.
# (`_mr_two_image_log` is defined in result_review_methods_partial_input.jl.)

@testset "Methods report — images that read different inputs state both" begin
    proj, s, imgs = _mr_two_image_log(_ -> nothing)
    t = history_params_table(proj, [i.uid for i in imgs]; usage_runs = Set{Tuple{String,String,String}}())
    seg = only(row for row in t if row["task"] == "segment.cellpose")
    @test haskey(seg["spread"], "valueName")
    md = methods_report(proj; set_uid = s.uid)["markdown"]
    @test occursin("segmented on the raw (1 image) or driftCorrected (1 image) image", md)
    @test !occursin("segmented on the raw image", md)
    rm(proj.root; recursive = true)
end
