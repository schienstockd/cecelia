# ── Test-acquisition check over a set (acq_check.jl) ──────────────────────────────────────────────
# Seeded facts, each rule with a case that fires and one that doesn't. The real-data run is recorded in
# docs/todo/ACQUISITION_TEST_CHECK_PLAN.md.

# One image's meta as the import probe + settings reader write it: `objs` maps a channel to
# (p50, p99, ceiling) photons, `shared` maps "a-b" to r.
function _acq_meta(; lasers = [(850, 15), (1200, 15)], brightz = true, objective = "XLPLN25XWMP2", px = 0.5,
                   depth = Dict(0 => -5.0), objs = Dict(0 => (5.0, 20.0, nothing)), shared = Dict{String,Float64}(),
                   drift = (0.3, 20.0), probed = true)
    chans = Any[]
    for c in sort!(unique(vcat(collect(keys(depth)), collect(keys(objs)))))
        d = get(depth, c, 0.0)
        ch = Dict{String,Any}("index" => c, "profile" => Dict("byZ" => Dict("p99" => [200.0, 200.0 * (1 + d / 100)])))
        if probed && haskey(objs, c)
            p50, p99, ceil = objs[c]
            ch["photons"] = Dict("fit" => Dict("greyPerPhoton" => 30.0),
                                 "objects" => Dict("p50Photons" => p50, "p99Photons" => p99, "ceilingPhotons" => ceil))
        end
        push!(chans, ch)
    end
    sat = Dict{String,Any}("channels" => chans)
    if probed
        sat["sharedStructure"] = [Dict("a" => parse(Int, split(k, "-")[1]), "b" => parse(Int, split(k, "-")[2]), "r" => r)
                                  for (k, r) in shared]
        sat["drift"] = Dict("axes" => ["Z", "Y", "X"], "excursionPx" => [1.0, 0.0, drift[2]], "residualRmsPx" => drift[1])
    end
    Dict{String,Any}("PhysicalSizeX" => px, "saturation" => sat,
        "acquisition" => Dict("lasers" => [Dict("wavelengthNm" => w, "transmissionPct" => p) for (w, p) in lasers],
                              "brightZ" => Dict("enabled" => brightz), "objective" => Dict("name" => objective)))
end

_acq_codes(r; scope = "set") = sort!([f["code"] for f in r["findings"] if f["scope"] == scope])

@testset "acq_check — design keys from file names" begin
    @test Cecelia.acq_design_key("M1-2-B6-naive-gBT-CTV-OTI-uGFP-P14-CTDR-z220_0005") == "B6-naive-gBT-CTV-OTI-uGFP-P14-CTDR"
    @test Cecelia.acq_design_key("M3-1-B6-naive-gBT-uGFP-OTI-CTV-P14-ubTomato-z230_0002.oir") ==
          "B6-naive-gBT-uGFP-OTI-CTV-P14-ubTomato"
    # two runs of one design share a key; two designs don't
    facts = [Cecelia.acq_image_facts("a", "M1-1-x-CTV-uGFP-z200_0001", _acq_meta()),
             Cecelia.acq_image_facts("b", "M2-1-x-CTV-uGFP-z180_0003", _acq_meta()),
             Cecelia.acq_image_facts("c", "M1-3-x-uGFP-CTV-z250_0001", _acq_meta())]
    ds = Cecelia.acq_suggest_designs(facts)
    @test length(ds) == 2 && ds[1]["imageUids"] == ["a", "b"] && ds[2]["imageUids"] == ["c"]
end

@testset "acq_check — no plan: settings, depth, range, drift, unmeasured" begin
    facts = [Cecelia.acq_image_facts("a", "M1-x-A", _acq_meta(brightz = false, depth = Dict(0 => -25.0))),
             Cecelia.acq_image_facts("b", "M2-x-A", _acq_meta(brightz = true, depth = Dict(0 => -2.0))),
             Cecelia.acq_image_facts("c", "M3-x-A", _acq_meta(drift = (3.7, 100.0))),
             Cecelia.acq_image_facts("d", "M4-x-A", _acq_meta(probed = false))]
    r = Cecelia.acq_check(nothing, facts)
    codes = _acq_codes(r)
    @test "acq.settings_differ" in codes                  # Bright Z on in some, off in one — same design
    @test "acq.depth_falls" in codes && "acq.depth_falls_image" in _acq_codes(r; scope = "a")
    @test "acq.drift_unreliable" in _acq_codes(r; scope = "c")
    @test "acq.not_measured" in codes
    @test !("acq.balance" in codes) && !("acq.few_photons" in codes)   # need a plan
    @test "photons" in r["skipped"]
    # Bright Z on: the same fall does not flag (it's the depth ramp's job, and it was on)
    on = Cecelia.acq_check(nothing, [Cecelia.acq_image_facts("a", "x", _acq_meta(brightz = true, depth = Dict(0 => -25.0)))])
    @test !("acq.depth_falls" in _acq_codes(on))
    # a detector range that holds few photons is the gain, not the light
    g = Cecelia.acq_check(nothing, [Cecelia.acq_image_facts("a", "x", _acq_meta(objs = Dict(0 => (1.0, 3.0, 6.5))))])
    @test "acq.gain_high" in _acq_codes(g)
    @test length(Cecelia.acq_open(r)) == count(f -> f["level"] == "warn", r["findings"])
end

@testset "acq_check — with a plan: designs, photons, balance, shared structure" begin
    # design A: P14 ch0, OT-I ch2, gBT ch3, ch1 a reference; design B: P14 ch1, gBT ch2, OT-I ch3
    plan = Dict{String,Any}(
        "designs" => [Dict("id" => "A", "key" => "x-CTV"), Dict("id" => "B", "key" => "x-TOM")],
        "objects" => [Dict("name" => "P14", "kind" => "cells", "channels" => Dict("A" => 0, "B" => 1)),
                      Dict("name" => "OT-I", "kind" => "cells", "channels" => Dict("A" => 2, "B" => 3)),
                      Dict("name" => "gBT", "kind" => "cells", "channels" => Dict("A" => 3, "B" => 2))],
        "references" => [Dict("name" => "empty", "channels" => Dict("A" => 1))])
    a = _acq_meta(lasers = [(850, 15), (1200, 15)], brightz = false,
                  depth = Dict(0 => -20.0, 1 => -5.0, 2 => -10.0, 3 => -8.0),
                  objs = Dict(0 => (3.5, 39.0, nothing), 1 => (1.0, 5.0, nothing), 2 => (1.9, 12.0, nothing), 3 => (2.1, 24.0, nothing)),
                  shared = Dict("1-2" => 0.69, "0-1" => 0.3, "2-3" => 0.48))
    b = _acq_meta(lasers = [(820, 8), (980, 15)], brightz = true,
                  depth = Dict(1 => 0.0, 2 => 2.0, 3 => 5.0),
                  objs = Dict(1 => (1.5, 15.0, nothing), 2 => (2.1, 16.0, nothing), 3 => (1.0, 6.0, nothing)),
                  shared = Dict("1-2" => 0.57))
    facts = [Cecelia.acq_image_facts("a1", "M1-1-x-CTV-z200", a), Cecelia.acq_image_facts("a2", "M2-1-x-CTV-z180", a),
             Cecelia.acq_image_facts("b1", "M1-3-x-TOM-z250", b)]
    r = Cecelia.acq_check(plan, facts)
    codes = _acq_codes(r)
    # lasers and Bright Z split exactly along the designs
    by = filter(f -> f["code"] == "acq.settings_by_design", r["findings"])
    @test Set(f["subs"]["variable"] for f in by) == Set(["laser setting", "Bright Z setting"])
    @test all(f -> haskey(f, "thisData"), by)
    @test "acq.depth_falls" in codes                                      # design A, Bright Z off
    # OT-I is dimmest in all three (12 / 6 against 39 / 16)
    bal = only(filter(f -> f["code"] == "acq.balance", r["findings"]))
    @test bal["subs"]["what"] == "OT-I" && bal["subs"]["n"] == 3
    # uGFP in the next channel: a reference in A (info), another object in B (warn)
    @test any(f -> f["code"] == "acq.shares_reference" && f["subs"]["what"] == "OT-I ~ empty", r["findings"])
    @test any(f -> f["code"] == "acq.shares_object" && f["subs"]["what"] == "P14 ~ gBT", r["findings"])
    @test !any(f -> f["code"] == "acq.shares_object" && occursin("OT-I ~ gBT", f["subs"]["what"]), r["findings"])  # 0.48 < 0.5
    # photon-starved: OT-I's median p50 is 1.9 / 1.9 / 1.0
    @test any(f -> f["code"] == "acq.few_photons" && f["subs"]["what"] == "OT-I", r["findings"])
    # a point object is counted, not read for its noise
    pts = deepcopy(plan); pts["objects"][2]["kind"] = "points"
    @test !any(f -> f["code"] == "acq.few_photons" && f["subs"]["what"] == "OT-I", Cecelia.acq_check(pts, facts)["findings"])
    # per-movie rows: design, ratio, the dimmest object, max r
    row = only(filter(x -> x["uid"] == "b1", r["rows"]))
    @test row["design"] == "B" && row["dimmest"] == "OT-I" && isapprox(row["ratio"], 16 / 6) && row["sharedR"] == 0.57
    @test row["objects"]["P14"]["channel"] == 1
end

@testset "acq_check — every finding renders from the catalog" begin
    plan = Dict{String,Any}("designs" => [Dict("id" => "A", "key" => "")],
                            "objects" => [Dict("name" => "X", "channels" => Dict("A" => 0)),
                                          Dict("name" => "Y", "channels" => Dict("A" => 1))])
    m = _acq_meta(objs = Dict(0 => (1.0, 30.0, 10.0), 1 => (1.0, 3.0, nothing)), shared = Dict("0-1" => 0.9),
                  brightz = false, depth = Dict(0 => -30.0, 1 => -30.0))
    r = Cecelia.acq_check(plan, [Cecelia.acq_image_facts("a", "x", m), Cecelia.acq_image_facts("b", "y", m)])
    @test all(f -> haskey(Cecelia.QC_TEXT, f["key"]) && !occursin("{", f["short"] * f["long"]), r["findings"])
end
