# RESULT-REVIEW: result-75742ebb
# A Bright Z point with a laser's transmission missing (the OIR reader writes `null` when it can't read
# one) must not make a flat ramp read as a ramp — that turned "Bright Z off, no ramp" (a note) into "the
# saved ramp wasn't applied" (a warning) with the text `NaN/12→NaN/12 %`.
@testset "acq_check — a missing ramp value is not a ramp" begin
    pt(z, a) = Dict("zFromStartUm" => z, "transmissionPct" => Dict("MaiTai" => a, "InSight" => 12), "hv" => Dict("D1" => 550))
    @test Cecelia.acq_bright_z_ramp(Dict("points" => [pt(0, nothing), pt(35, nothing)])) === nothing
    # a laser read at some points only is left out; the others still show a real ramp
    ramp = Dict("points" => [pt(0, nothing), Dict("zFromStartUm" => 35, "transmissionPct" => Dict("MaiTai" => 15, "InSight" => 15))])
    @test Cecelia.acq_bright_z_ramp(ramp) == "12→15 % over 35 µm"
end
