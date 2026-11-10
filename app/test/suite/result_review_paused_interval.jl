# RESULT-REVIEW: result-ee2a25f0
#
# The Julia half of the paused-interval repro (the Python half is
# python/cecelia/tests/test_result_review_immunemap_paused_interval.py): a file whose own timestamps
# the reader refused as irregular gets no declared interval either, through the one entry point
# import and resync share — every speed across a pause would otherwise come out ~20x too fast.
@testset "RESULT-REVIEW result-ee2a25f0: a paused recording gets no declared interval" begin
    dt = Dict{String,Any}("declaredTimeIncrement" => Dict{String,Any}(
        "value" => 30.0, "unit" => "second", "source" => "Immunemap acquisition 309 (timestamps)"))
    m = Dict{String,Any}("SizeT" => 60)
    paused(zm, src, dir; irregular, kw...) = (irregular[] = true; false)
    ok = Cecelia._fill_source_time_increment!(m, dt, "/src.ims", tempdir(); recover = paused)
    @test !ok && !haskey(m, "TimeIncrement")
end
