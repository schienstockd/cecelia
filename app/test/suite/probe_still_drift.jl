# RESULT-REVIEW: result-f9e75bc9
# A still image measured through the acquisition check's Measure gets `drift: null` from the probe —
# "nothing to register", an answer. The result read dropped null keys, so the stored probe had no
# `drift` and the image stayed "not measured" after a successful Measure.

@testset "probe result — a still image's null drift is kept, a probe without drift has none" begin
    chans = Any[Dict{String,Any}("index" => 0, "saturated" => false)]
    still = Cecelia._saturation_from_result(Dict{String,Any}("channels" => chans, "sharedStructure" => Any[],
                                                             "drift" => nothing))
    @test haskey(still, "drift") && still["drift"] === nothing
    @test Cecelia._saturation_has_drift(Dict{String,Any}("saturation" => still))
    imported = Cecelia._saturation_from_result(Dict{String,Any}("channels" => chans, "sharedStructure" => Any[]))
    @test !haskey(imported, "drift") && !Cecelia._saturation_has_drift(Dict{String,Any}("saturation" => imported))
    @test Cecelia._saturation_has_drift(Dict{Symbol,Any}(:saturation => Dict{Symbol,Any}(:drift => nothing)))
    @test Cecelia._saturation_from_result(Dict{String,Any}("channels" => Any[])) === nothing
end
