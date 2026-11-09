# RESULT-REVIEW: result-33cbef06
# Two movies with Bright Z on and the same saved ramp, one with a laser's transmission unread (`null`) at
# one point, must not be flagged as having different Bright Z settings: an unread value is unknown, not
# a different setting.
@testset "acq_check — an unread ramp value doesn't make two movies differ" begin
    pt(z, a, b) = Dict("zFromStartUm" => z, "transmissionPct" => Dict("MaiTai" => a, "InSight" => b))
    meta(points) = Dict{String,Any}("acquisition" => Dict("brightZ" => Dict("enabled" => true, "points" => points)))
    names = ["A", "B"]
    a = Cecelia.acq_image_facts("a", "a", meta([pt(0, 12, 12), pt(35, 15, 15)]); channel_names = names)
    b = Cecelia.acq_image_facts("b", "b", meta([pt(0, 12, 12), pt(35, nothing, 15)]); channel_names = names)
    c = Cecelia.acq_image_facts("c", "c", meta([pt(0, nothing, 12), pt(35, 15, nothing)]); channel_names = names)
    r = Cecelia.acq_check(nothing, [a, b, c])
    @test !any(f -> f["code"] == "acq.settings_differ" && f["subs"]["variable"] == "Bright Z setting", r["findings"])
end
