# Under Auto HV the HV a movie records is not the one used, so detector HV is left out of the comparison —
# and the per-point Bright Z HV must be too: stored voltages that differ between depths (or can't be read)
# must not show a "HV a→b" ramp nor make the ramp unknown, which regroups a set by Bright Z.
@testset "acq_check — Bright Z HV is ignored under Auto HV" begin
    pt(z, hv) = Dict("zFromStartUm" => z, "transmissionPct" => Dict("MaiTai" => 12), "hv" => Dict("D1" => hv))
    hv_ramp = Dict("enabled" => true, "points" => [pt(0, 550), pt(20, 600)])
    @test Cecelia.acq_bright_z_ramp(hv_ramp) == "HV 550→600 over 20 µm"
    @test Cecelia.acq_bright_z_ramp(hv_ramp; auto_hv = true) === nothing
    facts(bz, auto) = Cecelia.acq_image_facts("a", "a", Dict{String,Any}("acquisition" =>
        Dict{String,Any}("brightZ" => bz, "autoHV" => auto)); channel_names = ["A"])
    @test facts(hv_ramp, false)["brightZRamp"] !== nothing
    @test facts(hv_ramp, true)["brightZRamp"] === nothing
    # an unreadable HV under Auto HV leaves the ramp known; without Auto HV it doesn't
    gap = Dict("enabled" => true, "points" => [pt(0, nothing), pt(20, nothing)])
    @test facts(gap, false)["brightZRampKnown"] == false
    @test facts(gap, true)["brightZRampKnown"] == true
    # a transmission ramp still shows under Auto HV, without the HV part
    tx = Dict("enabled" => true, "points" => [Dict("zFromStartUm" => 0, "transmissionPct" => Dict("MaiTai" => 12), "hv" => Dict("D1" => 550)),
                                               Dict("zFromStartUm" => 35, "transmissionPct" => Dict("MaiTai" => 15), "hv" => Dict("D1" => 600))])
    @test facts(tx, true)["brightZRamp"] == "12→15 % over 35 µm"
end
