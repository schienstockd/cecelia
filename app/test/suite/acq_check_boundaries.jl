# The test-acquisition check (acq_check.jl) at the edges its rules state: band limits, the pixel-size
# tolerance, which design a finding names, channels without an index, the Bright Z off note. Uses the
# helpers of acq_check.jl (`_acq_meta`, `_acq_f`, `_ACQ_*`), included before it.
# Why: docs/todo/TEST_VALUE_PLAN.md → P3.

@testset "acq_check at its edges" begin
    # bands where low is bad: the limit itself is the better band (`v < problem`, `v < borderline`)
    b = Cecelia.ACQ_HEADROOM_BANDS
    @test Cecelia._acq_band_low(b.borderline, b) == :fine
    @test Cecelia._acq_band_low(prevfloat(b.borderline), b) == :borderline
    @test Cecelia._acq_band_low(b.problem, b) == :borderline
    @test Cecelia._acq_band_low(prevfloat(b.problem), b) == :problem

    # pixel size: within 1 % of the larger value is the same setting
    px_differs(a, c) = any(f -> f["code"] == "acq.settings_differ" && f["subs"]["variable"] == "pixel size",
                           Cecelia.acq_check(nothing, [_acq_f("a", _acq_meta(px = a)), _acq_f("b", _acq_meta(px = c))])["findings"])
    @test !px_differs(0.5, 0.505)
    @test !px_differs(0.5, 0.50502)        # 0.00502 apart: inside 1 % of 0.50502, outside 1 % of 0.5
    @test px_differs(0.5, 0.506)

    # a channel the probe wrote without an index is the one at its position; channels past the named
    # ones get a placeholder name
    m = _acq_meta(objs = Dict(0 => (5.0, 20.0, nothing), 1 => (2.0, 9.0, nothing), 2 => (1.0, 4.0, nothing)))
    foreach(ch -> delete!(ch, "index"), m["saturation"]["channels"])
    f = Cecelia.acq_image_facts("a", "a", m; channel_names = ["x", "y"])
    @test sort!(collect(keys(f["channels"]))) == [0, 1, 2]
    @test f["channels"][1]["p50Photons"] == 2.0
    @test f["channelNames"] == ["x", "y", "Channel 3"]

    # with a plan: a within-design finding names ITS design, and the between-design line lists each
    # design with its own values
    plan = Dict{String,Any}(
        "designs" => [Dict("id" => "A", "key" => Cecelia.acq_design_key(_ACQ_CTDR), "label" => "design A"),
                      Dict("id" => "B", "key" => Cecelia.acq_design_key(_ACQ_TOM), "label" => "design B")],
        "objects" => [Dict("name" => "P14", "kind" => "cells", "channels" => Dict("A" => "P14-CTDR", "B" => "P14-ubTomato"))])
    a  = _acq_meta(lasers = [(850, 15), (1200, 15)])
    b1 = _acq_meta(lasers = [(820, 8), (980, 15)])
    b2 = _acq_meta(lasers = [(820, 10), (980, 10)])
    r = Cecelia.acq_check(plan, [_acq_f("a1", a), _acq_f("b1", b1, _ACQ_TOM), _acq_f("b2", b2, _ACQ_TOM)])
    wi = only(filter(f -> f["code"] == "acq.settings_within_design" && f["subs"]["variable"] == "laser setting", r["findings"]))
    @test wi["subs"]["what"] == "design B"
    by = only(filter(f -> startswith(f["code"], "acq.settings_by_design") && f["subs"]["variable"] == "laser setting", r["findings"]))
    @test by["where"] == "design A: 850/1200 nm · 15/15 % · design B: 820/980 nm · 10/10 %, 820/980 nm · 8/15 %"

    # a plan with ONE design takes every movie, whatever its channels are called
    one = Dict{String,Any}("designs" => [Dict("id" => "A", "key" => Cecelia.acq_design_key(_ACQ_CTDR))],
                           "objects" => [Dict("name" => "P14", "kind" => "cells", "channels" => Dict("A" => "P14-CTDR"))])
    r = Cecelia.acq_check(one, [_acq_f("a1", a), _acq_f("x", a, ["one", "two", "three", "four"])])
    @test !any(f -> f["code"] == "acq.not_in_plan", r["findings"])

    # how a scan setting reads in a finding's value list
    @test Cecelia._acq_scan_text((1.234, 2.0)) == "dwell 1.23 µs · ×2"
    @test Cecelia._acq_scan_text((nothing, 3.0)) == "×3"

    # Bright Z off with no ramp saved: a note for the off movie, none for the on one
    off(r) = only(filter(f -> f["code"] == "acq.brightz_off", r["findings"]))
    r = Cecelia.acq_check(nothing, [_acq_f("on", _acq_meta(brightz = true)), _acq_f("off", _acq_meta(brightz = false))])
    @test off(r)["where"] == "off" && off(r)["subs"]["s"] == ""
    r = Cecelia.acq_check(nothing, [_acq_f("o1", _acq_meta(brightz = false)), _acq_f("o2", _acq_meta(brightz = false))])
    @test off(r)["subs"]["s"] == "s"
    # the movie count's plural follows the count
    nm(r) = only(filter(f -> f["code"] == "acq.not_measured", r["findings"]))["subs"]["s"]
    @test nm(Cecelia.acq_check(nothing, [_acq_f("u", _acq_meta(probed = false))])) == ""
    @test nm(Cecelia.acq_check(nothing, [_acq_f("u", _acq_meta(probed = false)), _acq_f("v", _acq_meta(probed = false))])) == "s"
end
