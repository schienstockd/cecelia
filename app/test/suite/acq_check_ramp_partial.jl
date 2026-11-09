# RESULT-REVIEW: result-fb92cc02
# A ramp that really changes, with each laser unread at a different point, is still a ramp: counted from
# the values that were read. With Bright Z off it is "the saved ramp wasn't applied" (a warning), not
# "no ramp saved" (a note).
@testset "acq_check — a ramp read in part is still a ramp" begin
    pt(z, a, b) = Dict("zFromStartUm" => z, "transmissionPct" => Dict("MaiTai" => a, "InSight" => b))
    bz = Dict("enabled" => false, "points" => [pt(0, nothing, 12), pt(20, 12, 13), pt(35, 15, nothing)])
    @test Cecelia.acq_bright_z_ramp(bz) !== nothing
    f = Cecelia.acq_image_facts("c", "c", Dict{String,Any}("acquisition" => Dict("brightZ" => bz)); channel_names = ["A"])
    @test "acq.brightz_ramp_unused" in [x["code"] for x in Cecelia.acq_check(nothing, [f])["findings"]]
end
