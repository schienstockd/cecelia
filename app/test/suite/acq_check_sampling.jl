# ── Acquisition check: sampling against the optics (acq_sampling.jl) ───────────────────────────────
# Expected values come from the source's formulas worked by hand (Faklaris 2022, doi:10.1083/jcb.202107093),
# not from the code's output.

_acq_conf_ch(nm, au) = Dict{String,Any}("laserLines" => [Dict("wavelengthNm" => nm, "pct" => 1.0)], "pinholeAiry" => au)
_acq_conf(chans; na = 1.4, imm = "oil", ri = nothing, px = 0.04, z = nothing, nz = 1) = Dict{String,Any}(
    "PhysicalSizeX" => px, "PhysicalSizeUnit" => "µm", "SizeZ" => nz, "PhysicalSizeZ" => z, "SizeT" => 1,
    "acquisition" => Dict{String,Any}("technology" => "confocal", "lasers" => Any[], "channels" => chans,
                                      "objective" => Dict("na" => na, "immersion" => imm, "refractiveIndex" => ri)))

@testset "acq_sampling — Nyquist pixel (Faklaris 2022 worked value)" begin
    # "Using a 1.4 NA lens and an excitation wavelength of 488 nm, the criterion value is 43 nm" — closed pinhole
    s = Cecelia.acq_sampling(_acq_conf([_acq_conf_ch(488, 0.25)]))
    @test round(s["nyquistXYUm"] * 1000; digits = 1) == 43.6
    # "For a 1 A.U. pinhole, the Nyquist criterion allows a 1.6× bigger pixel size"
    @test Cecelia.acq_sampling(_acq_conf([_acq_conf_ch(488, 1.0)]))["nyquistXYUm"] ≈ 0.488 / 11.2 * 1.6
    # the most stringent channel sets it: 405 nm beats 561 nm
    @test Cecelia.acq_sampling(_acq_conf([_acq_conf_ch(561, 1.0), _acq_conf_ch(405, 1.0)]))["nyquistXYUm"] ≈ 0.405 / 11.2 * 1.6
    # the pixel in its unit: 40 nm is 0.04 µm, also with Symbol keys (meta read back through JSON3)
    nm = _acq_conf([_acq_conf_ch(488, 1.0)]; px = 40); nm["PhysicalSizeUnit"] = "nm"
    @test Cecelia.acq_sampling(nm)["pixelUm"] ≈ 0.04
    @test Cecelia.acq_sampling(Dict{Any,Any}(Symbol(k) => v for (k, v) in nm))["pixelUm"] ≈ 0.04
end

@testset "acq_sampling — z-step against the axial FWHM" begin
    # confocal, NA 0.8 dry, 488 nm: FWHM_z = 0.88·0.488/(1 − √(1 − 0.64)) = 0.42944/0.4 = 1.0736 µm → ≤ 0.5368 µm
    m = _acq_conf([_acq_conf_ch(488, 1.0)]; na = 0.8, imm = "air", z = 2.0, nz = 20)
    s = Cecelia.acq_sampling(m)
    @test s["nyquistZUm"] ≈ 0.5368 atol = 1e-4
    @test Cecelia.acq_sampling_state(s)[2] == :under
    m["PhysicalSizeZ"] = 0.5
    @test Cecelia.acq_sampling_state(Cecelia.acq_sampling(m))[2] == :fine
    # a single plane has no z-step to judge
    @test Cecelia.acq_sampling_state(Cecelia.acq_sampling(_acq_conf([_acq_conf_ch(488, 1.0)])))[2] === nothing
    # wide field: 1.77·n·λem/NA², λem the band centre (500–550 → 525 nm), NA 1.0 water → 1.77·1.333·0.525/1/2
    wf = Dict{String,Any}("PhysicalSizeX" => 0.1, "SizeZ" => 5, "PhysicalSizeZ" => 1.0,
                          "acquisition" => Dict{String,Any}("technology" => "widefield", "channels" => [Dict("emissionFilter" => "500-550 nm")],
                                                            "objective" => Dict("na" => 1.0, "immersion" => "water")))
    ws = Cecelia.acq_sampling(wf)
    @test ws["nyquistZUm"] ≈ 1.77 * 1.333 * 0.525 / 2
    @test ws["nyquistXYUm"] ≈ 0.51 * 0.525 / 2
end

@testset "acq_sampling — no formula, no finding" begin
    tp = _acq_conf([_acq_conf_ch(920, nothing)]); tp["acquisition"]["technology"] = "two-photon"
    s = Cecelia.acq_sampling(tp)
    @test s["nyquistXYUm"] === nothing && occursin("two-photon", s["basis"])
    nona = _acq_conf([_acq_conf_ch(488, 1.0)]; na = nothing)
    @test Cecelia.acq_sampling(nona)["basis"] == "no NA"
    r = Cecelia.acq_check(nothing, [Cecelia.acq_image_facts("a", "a", tp; channel_names = ["CD4"])])
    @test !any(f -> f["category"] == "sampling", r["findings"]) && "sampling" in r["skipped"]
end

@testset "acq_check — sampling findings on a real LSM 780 multitrack reading" begin
    # channels, objective and technology copied from the CZI reader's output on a real file
    # (test-data/acquisition/expected/czi_lsm780_multitrack.json); the pixel size is chosen
    real = [_acq_conf_ch(633.0, 0.71), _acq_conf_ch(561.0, 0.99), _acq_conf_ch(488.0, 0.99), _acq_conf_ch(405.0, 1.01)]
    meta(px) = _acq_conf(real; na = 0.8, imm = "air", ri = 1.0, px = px)
    # 633 nm at 0.71 AU (closed: ×1) → 0.633/6.4 = 0.0989 µm; 405 nm at 1.01 AU → 0.405/6.4·1.6 = 0.1013 µm
    @test Cecelia.acq_sampling(meta(0.5))["nyquistXYUm"] ≈ 0.633 / 6.4
    names = ["Cy5", "mCherry", "GFP", "DAPI"]
    r = Cecelia.acq_check(nothing, [Cecelia.acq_image_facts("a", "a", meta(0.5); channel_names = names),
                                    Cecelia.acq_image_facts("b", "b", meta(0.03); channel_names = names),
                                    Cecelia.acq_image_facts("c", "c", meta(0.08); channel_names = names)])
    under = only(filter(f -> f["code"] == "acq.undersampled_xy", r["findings"]))
    @test under["level"] == "info" && under["category"] == "sampling" && endswith(under["where"], ": a")
    over = only(filter(f -> f["code"] == "acq.oversampled_xy", r["findings"]))
    @test endswith(over["where"], ": b")
    # c is within the band: neither
    @test !occursin("c", under["where"] * over["where"])
    # 0.71 vs 1.01 AU differ by 1.42× — a note on each image
    @test count(f -> f["code"] == "acq.pinhole_differs", r["findings"]) == 3
    @test "sampling" in r["ran"]
    # notes only: the verdict is unchanged
    @test all(f -> f["level"] == "info", filter(f -> f["category"] == "sampling", r["findings"]))
    # the steps come from other rules (unmeasured fixtures; three pixel sizes differ) — none from sampling
    @test r["steps"] == ["measure the remaining images", "use one pixel size for every compared image"]
end
