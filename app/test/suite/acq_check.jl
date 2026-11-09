# ── Test-acquisition check over a set (acq_check.jl) ──────────────────────────────────────────────
# Seeded facts, each rule with a case that fires and one that doesn't. The real-data run is recorded in
# docs/todo/ACQUISITION_TEST_CHECK_PLAN.md.

# One image's meta as the import probe + settings reader write it: `objs` maps a channel to
# (p50, p99, ceiling) photons, `shared` maps "a-b" to r.
# A Bright Z block as the OIR reader writes it (oir_meta._bright_z): points of transmission + HV per depth.
_acq_bz_pt(z, t) = Dict("zFromStartUm" => z, "transmissionPct" => Dict("MaiTai" => t, "InSight" => t), "hv" => Dict("D1" => 550))
function _acq_meta(; lasers = [(850, 15), (1200, 15)], brightz = true, ramp = false, objective = "XLPLN25XWMP2", px = 0.5,
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
                              "brightZ" => Dict("enabled" => brightz, "points" => ramp ? [_acq_bz_pt(0, 12), _acq_bz_pt(35, 15)] : [_acq_bz_pt(0, 12)]), "objective" => Dict("name" => objective)))
end

_acq_codes(r; scope = "set") = sort!([f["code"] for f in r["findings"] if f["scope"] == scope])

const _ACQ_CTDR = ["P14-CTDR", "empty-570", "OTI-uGFP", "gBT-CTV"]
const _ACQ_TOM  = ["empty-642", "P14-ubTomato", "gBT-uGFP", "OTI-CTV"]
_acq_f(uid, meta, names = _ACQ_CTDR) = Cecelia.acq_image_facts(uid, uid, meta; channel_names = names)

@testset "acq_check — designs are channel-name lists" begin
    @test Cecelia.acq_design_key(_ACQ_CTDR) == "P14-CTDR · empty-570 · OTI-uGFP · gBT-CTV"
    @test Cecelia.acq_design_key(["CH1", "CH2", "ch3", "Channel 4"]) == "" && Cecelia.acq_design_key(String[]) == ""
    # same names share a design, whatever the file is called; other names (or order) don't
    facts = [_acq_f("a", _acq_meta()), _acq_f("b", _acq_meta()), _acq_f("c", _acq_meta(), _ACQ_TOM)]
    ds = Cecelia.acq_suggest_designs(facts)
    @test length(ds) == 2 && ds[1]["imageUids"] == ["a", "b"] && ds[2]["imageUids"] == ["c"]
    @test !any(f -> f["code"] == "acq.channels_unnamed", Cecelia.acq_check(nothing, facts)["findings"])
    # default names: one design and one finding asking for names — no guessing
    un = [_acq_f("a", _acq_meta(), ["CH1"]), _acq_f("b", _acq_meta(), ["CH1"])]
    @test length(Cecelia.acq_suggest_designs(un)) == 1
    @test count(f -> f["code"] == "acq.channels_unnamed", Cecelia.acq_check(nothing, un)["findings"]) == 1
end

@testset "acq_check — no plan: settings, Bright Z, range, drift, unmeasured" begin
    facts = [_acq_f("a", _acq_meta(brightz = false, ramp = true)),
             _acq_f("b", _acq_meta(brightz = true, ramp = true)),
             _acq_f("c", _acq_meta(drift = (3.7, 100.0))),
             _acq_f("d", _acq_meta(probed = false))]
    r = Cecelia.acq_check(nothing, facts)
    codes = _acq_codes(r)
    # Bright Z off with a ramp saved: the ramp wasn't applied — a warning naming the movie and the ramp
    bz = only(filter(f -> f["code"] == "acq.brightz_ramp_unused", r["findings"]))
    @test bz["level"] == "warn" && bz["whereLabel"] == "Movies" && bz["where"] == "12/12→15/15 % over 35 µm: a"
    @test "acq.brightz_ramp_unused_image" in _acq_codes(r; scope = "a") && isempty(_acq_codes(r; scope = "b"))
    @test !any(f -> startswith(f["code"], "acq.depth"), r["findings"])   # no intensity-with-depth reading
    # movies compared directly with Bright Z on in one, off in the other: a warning naming them
    bzd = only(filter(f -> f["code"] == "acq.settings_differ" && f["subs"]["variable"] == "Bright Z setting", r["findings"]))
    @test bzd["level"] == "warn" && occursin("off · ramp saved: a", bzd["where"])
    # ...also when no ramp is saved at all (a CZI never saves points): on vs off is still a warning
    czi = Cecelia.acq_check(nothing, [_acq_f("a", _acq_meta(brightz = true)), _acq_f("b", _acq_meta(brightz = false))])
    @test any(f -> f["code"] == "acq.settings_differ" && f["subs"]["variable"] == "Bright Z setting" && f["level"] == "warn", czi["findings"])
    @test "acq.drift_unreliable" in _acq_codes(r; scope = "c")
    @test "acq.not_measured" in codes
    @test !("acq.balance" in codes) && !("acq.few_photons" in codes)   # need a plan
    @test !any(f -> startswith(f["code"], "acq.shares"), r["findings"]) && all(x -> isempty(x["objects"]), r["rows"])
    @test "photons" in r["skipped"]
    # Bright Z off with no ramp saved: a note, not a warning
    nr = Cecelia.acq_check(nothing, [_acq_f("a", _acq_meta(brightz = false)), _acq_f("b", _acq_meta())])
    @test only(filter(f -> f["code"] == "acq.brightz_off", nr["findings"]))["level"] == "info"
    @test !("acq.brightz_ramp_unused" in _acq_codes(nr))
    # the ramp as text: lasers joined by /, HV only when it changes; one point (or a flat one) is no ramp
    @test Cecelia.acq_bright_z_ramp(Dict("points" => [_acq_bz_pt(0, 12)])) === nothing
    @test Cecelia.acq_bright_z_ramp(Dict("points" => [_acq_bz_pt(0, 12), _acq_bz_pt(35, 12)])) === nothing
    hv = Dict("points" => [Dict("zFromStartUm" => 0, "hv" => Dict("D1" => 550)), Dict("zFromStartUm" => 20, "hv" => Dict("D1" => 600))])
    @test Cecelia.acq_bright_z_ramp(hv) == "HV 550→600 over 20 µm"
    # a detector range that holds few photons is the gain, not the light
    g = Cecelia.acq_check(nothing, [Cecelia.acq_image_facts("a", "x", _acq_meta(objs = Dict(0 => (1.0, 3.0, 6.5))))])
    @test "acq.gain_high" in _acq_codes(g)
    @test length(Cecelia.acq_open(r)) == count(f -> f["level"] == "warn", r["findings"])
end

@testset "acq_check — with a plan: designs, photons, balance, shared structure" begin
    # the plan picks each object's channel by name, per design: CTDR has P14 ch0, OT-I ch2, gBT ch3 and
    # an empty ch1 as reference; ubTomato has P14 ch1, gBT ch2, OT-I ch3
    plan = Dict{String,Any}(
        "designs" => [Dict("id" => "A", "key" => Cecelia.acq_design_key(_ACQ_CTDR)),
                      Dict("id" => "B", "key" => Cecelia.acq_design_key(_ACQ_TOM))],
        "objects" => [Dict("name" => "P14", "kind" => "cells", "channels" => Dict("A" => "P14-CTDR", "B" => "P14-ubTomato")),
                      Dict("name" => "OT-I", "kind" => "cells", "channels" => Dict("A" => "OTI-uGFP", "B" => "OTI-CTV")),
                      Dict("name" => "gBT", "kind" => "cells", "channels" => Dict("A" => "gBT-CTV", "B" => "gBT-uGFP"))],
        "references" => [Dict("name" => "empty", "channels" => Dict("A" => "empty-570"))])
    a = _acq_meta(lasers = [(850, 15), (1200, 15)], brightz = false,
                  depth = Dict(0 => -20.0, 1 => -5.0, 2 => -10.0, 3 => -8.0),
                  objs = Dict(0 => (3.5, 39.0, nothing), 1 => (1.0, 5.0, nothing), 2 => (1.9, 12.0, nothing), 3 => (2.1, 24.0, nothing)),
                  shared = Dict("1-2" => 0.69, "0-1" => 0.3, "2-3" => 0.48))
    b = _acq_meta(lasers = [(820, 8), (980, 15)], brightz = true,
                  depth = Dict(1 => 0.0, 2 => 2.0, 3 => 5.0),
                  objs = Dict(1 => (1.5, 15.0, nothing), 2 => (2.1, 16.0, nothing), 3 => (1.0, 6.0, nothing)),
                  shared = Dict("1-2" => 0.57))
    b2 = deepcopy(b); b2["acquisition"]["lasers"] = [Dict("wavelengthNm" => 820, "transmissionPct" => 10), Dict("wavelengthNm" => 980, "transmissionPct" => 10)]
    facts = [_acq_f("a1", a), _acq_f("a2", a), _acq_f("b1", b, _ACQ_TOM), _acq_f("b2", b2, _ACQ_TOM)]
    r = Cecelia.acq_check(plan, facts)
    codes = _acq_codes(r)
    # lasers differ between the designs: expected with other dyes, and every compared object is in each
    # design, so it doesn't enter the comparison — info, listing the designs
    by = only(filter(f -> f["code"] == "acq.settings_by_design" && f["subs"]["variable"] == "laser setting", r["findings"]))
    @test by["level"] == "info" && by["whereLabel"] == "Designs"
    @test haskey(by, "thisData")
    # ...and within the ubTomato design two settings: a warning naming the movies per setting
    wi = only(filter(f -> f["code"] == "acq.settings_within_design", r["findings"]))
    @test wi["level"] == "warn" && occursin("8/15 %: b1", wi["where"]) && occursin("10/10 %: b2", wi["where"])
    # Bright Z off in one design, on in the other: still the one fix
    @test !("acq.settings_by_design_cross" in codes)
    # a movie whose channel names match no plan design: reported, and kept out of the designs' comparisons
    stray = vcat(facts, [_acq_f("x1", b2, ["one", "two", "three", "four"]), _acq_f("x2", a, ["five", "six", "seven", "eight"])])
    rs = Cecelia.acq_check(plan, stray)
    @test only(filter(f -> f["code"] == "acq.not_in_plan", rs["findings"]))["where"] == "x1, x2"
    @test !any(f -> f["code"] == "acq.settings_within_design" && f["subs"]["what"] == "Not in the plan", rs["findings"])
    # an object missing from a design: the comparison crosses designs, so a difference there is a warning
    cross = deepcopy(plan); delete!(cross["objects"][3]["channels"], "B")
    @test "acq.settings_by_design_cross" in _acq_codes(Cecelia.acq_check(cross, facts))
    # OT-I is dimmest in all four (12 / 6 against 39 / 16)
    bal = only(filter(f -> f["code"] == "acq.balance", r["findings"]))
    @test bal["subs"]["what"] == "OT-I" && bal["subs"]["n"] == 4
    # uGFP in the next channel: a reference in A (info), another object in B (warn)
    @test any(f -> f["code"] == "acq.shares_reference" && f["subs"]["what"] == "OT-I ~ empty", r["findings"])
    @test any(f -> f["code"] == "acq.shares_object" && f["subs"]["what"] == "P14 ~ gBT", r["findings"])
    @test !any(f -> f["code"] == "acq.shares_object" && occursin("OT-I ~ gBT", f["subs"]["what"]), r["findings"])  # 0.48 < 0.5
    # photon-starved: OT-I's median p50 is 1.9 / 1.9 / 1.0 / 1.0
    @test any(f -> f["code"] == "acq.few_photons" && f["subs"]["what"] == "OT-I", r["findings"])
    # a point object is counted, not read for its noise
    pts = deepcopy(plan); pts["objects"][2]["kind"] = "points"
    @test !any(f -> f["code"] == "acq.few_photons" && f["subs"]["what"] == "OT-I", Cecelia.acq_check(pts, facts)["findings"])
    # per-movie rows: design, ratio, the dimmest object, max r
    row = only(filter(x -> x["uid"] == "b1", r["rows"]))
    @test row["design"] == "B" && row["dimmest"] == "OT-I" && isapprox(row["ratio"], 16 / 6) && row["sharedR"] == 0.57
    @test row["objects"]["P14"]["channel"] == 1
    # an index still resolves (a plan saved before names, or from the REPL)
    idx = deepcopy(plan); idx["objects"][1]["channels"] = Dict("A" => 0, "B" => 1)
    @test only(filter(x -> x["uid"] == "b1", Cecelia.acq_check(idx, facts)["rows"]))["objects"]["P14"]["channel"] == 1
    # a channel the movie doesn't have is reported on that movie, not dropped
    bad = deepcopy(plan); bad["objects"][3]["channels"]["B"] = "gBT-GFP"
    rb = Cecelia.acq_check(bad, facts)
    @test only(_acq_codes(rb; scope = "b1") |> c -> filter(==("acq.channel_missing"), c)) == "acq.channel_missing"
    @test !("acq.channel_missing" in _acq_codes(rb; scope = "a1"))
    far = deepcopy(plan); far["objects"][1]["channels"] = Dict("A" => 9, "B" => 1)
    @test "acq.channel_missing" in _acq_codes(Cecelia.acq_check(far, facts); scope = "a1")
end

@testset "acq_check — every finding renders from the catalog" begin
    plan = Dict{String,Any}("designs" => [Dict("id" => "A", "key" => "")],
                            "objects" => [Dict("name" => "X", "channels" => Dict("A" => 0)),
                                          Dict("name" => "Y", "channels" => Dict("A" => 1))])
    m = _acq_meta(objs = Dict(0 => (1.0, 30.0, 10.0), 1 => (1.0, 3.0, nothing)), shared = Dict("0-1" => 0.9),
                  brightz = false, depth = Dict(0 => -30.0, 1 => -30.0))
    r = Cecelia.acq_check(plan, [Cecelia.acq_image_facts("a", "x", m), Cecelia.acq_image_facts("b", "y", m)])   # unnamed
    @test all(f -> haskey(Cecelia.QC_TEXT, f["key"]) && !occursin("{", f["short"] * f["long"]), r["findings"])
    # every finding names what it is about — a category the model review files its points under too
    @test all(f -> f["category"] in Cecelia.ACQ_CATEGORIES && f["category"] != "other", r["findings"])
    codes = [k for k in keys(Cecelia.QC_TEXT) if startswith(k, "acq.") && !endswith(k, ".data") && !startswith(k, "acq.cat.")]
    @test sort(codes) == sort(collect(keys(Cecelia.ACQ_CATEGORY_OF)))
    @test all(c -> haskey(Cecelia.QC_TEXT, "acq.cat." * c), Cecelia.ACQ_CATEGORIES)
end

@testset "acq_check — the measure task is registered and runs per image" begin
    # The batch path for "Measure N movies": a registered, hidden io-pool task around
    # `ensure_saturation_meta!`. An image with no store has nothing to probe — it reports so, not throws.
    @test Cecelia._fun_name_map()["importImages.measureSignal"] isa Cecelia.MeasureSignal
    spec = JSON3.read(read(Cecelia._spec_path(Cecelia.MeasureSignal()), String))
    @test spec.resource_pool == "io" && spec.hidden == true
    img = CciaImage(; dir = mktempdir())
    logs = String[]
    out = Cecelia._run_task(Cecelia.MeasureSignal(), img, Dict{String,Any}(); on_log = l -> push!(logs, l))
    @test out == Dict{String,Any}("measured" => false) && any(occursin("Already measured", l) for l in logs)
end
