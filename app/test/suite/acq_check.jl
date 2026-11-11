# ── Test-acquisition check over a set (acq_check.jl) ──────────────────────────────────────────────
# Seeded facts, each rule with a case that fires and one that doesn't. The real-data run is recorded in
# docs/todo/ACQUISITION_TEST_CHECK_PLAN.md.

# One image's meta as the import probe + settings reader write it: `objs` maps a channel to
# (p50, p99, ceiling) photons, `shared` maps "a-b" to r.
# A Bright Z block as the OIR reader writes it (oir_meta._bright_z): points of transmission + HV per depth.
_acq_bz_pt(z, t) = Dict("zFromStartUm" => z, "transmissionPct" => Dict("MaiTai" => t, "InSight" => t), "hv" => Dict("D1" => 550))
function _acq_meta(; lasers = [(850, 15), (1200, 15)], brightz = true, ramp = false, objective = "XLPLN25XWMP2", px = 0.5,
                   depth = Dict(0 => -5.0), objs = Dict(0 => (5.0, 20.0, nothing)), shared = Dict{String,Float64}(),
                   probed = true)
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

@testset "acq_check — no plan: settings, Bright Z, range, unmeasured" begin
    facts = [_acq_f("a", _acq_meta(brightz = false, ramp = true)),
             _acq_f("b", _acq_meta(brightz = true, ramp = true)),
             _acq_f("c", _acq_meta()),
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
    @test "acq.not_measured" in codes
    # no drift anywhere in the check: a probe that still stores one (an older Measure) raises nothing
    old = _acq_meta(); old["saturation"]["drift"] = Dict("axes" => ["Y", "X"], "excursionPx" => [0.0, 500.0], "residualRmsPx" => 9.0)
    od = Cecelia.acq_check(nothing, [_acq_f("a", old)])
    @test !any(f -> occursin("drift", f["code"]), od["findings"]) && !("drift" in od["ran"]) && !("drift" in od["skipped"])
    @test !haskey(only(od["rows"]), "driftUm")
    # an unmeasured movie is unchecked, never an all-clear
    um = Cecelia.acq_check(nothing, [_acq_f("a", _acq_meta(probed = false))])
    @test um["nMeasured"] == 0 && Cecelia.acq_verdict(um["findings"], "test")[2] == "Measure the remaining movies."
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
    # a detector range close above the brightest object voxels is the gain, not the light: within 2× a
    # warning, within 4× a note, further off nothing
    function head(p99)
        r = Cecelia.acq_check(nothing, [Cecelia.acq_image_facts("a", "x", _acq_meta(objs = Dict(0 => (1.0, p99, 6.5))))])
        hs = filter(f -> f["code"] == "acq.gain_high", r["findings"])
        isempty(hs) ? nothing : only(hs)
    end
    @test head(4.0)["level"] == "warn" && head(2.5)["level"] == "info" && head(1.0) === nothing
    @test head(4.0)["short"] == "Bright objects come within 1.6× of the detector range (~6 photons)"
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
                  shared = Dict("0-1" => 0.1, "0-2" => 0.12, "0-3" => 0.08, "1-2" => 0.69, "1-3" => 0.15, "2-3" => 0.3))
    b = _acq_meta(lasers = [(820, 8), (980, 15)], brightz = true,
                  depth = Dict(1 => 0.0, 2 => 2.0, 3 => 5.0),
                  objs = Dict(1 => (1.5, 15.0, nothing), 2 => (2.1, 16.0, nothing), 3 => (1.0, 6.0, nothing)),
                  shared = Dict("1-2" => 0.57, "1-3" => 0.1, "2-3" => 0.12))
    b2 = deepcopy(b); b2["acquisition"]["lasers"] = [Dict("wavelengthNm" => 820, "transmissionPct" => 10), Dict("wavelengthNm" => 980, "transmissionPct" => 10)]
    facts = [_acq_f("a1", a), _acq_f("a2", a), _acq_f("b1", b, _ACQ_TOM), _acq_f("b2", b2, _ACQ_TOM)]
    r = Cecelia.acq_check(plan, facts)
    codes = _acq_codes(r)
    # lasers differ between the designs: expected with other dyes, and every compared object is in each
    # design, so it doesn't enter the comparison — info, listing the designs
    by = only(filter(f -> f["code"] == "acq.settings_by_design" && f["subs"]["variable"] == "laser setting", r["findings"]))
    @test by["level"] == "info" && by["whereLabel"] == "Designs"
    @test haskey(by, "thisData")
    # without a plan nothing says the comparison stays within a design
    np = only(filter(f -> f["code"] == "acq.settings_by_design" && f["subs"]["variable"] == "laser setting",
                     Cecelia.acq_check(nothing, facts)["findings"]))
    @test !haskey(np, "thisData")
    # ...and within the ubTomato design two settings: a warning naming the movies per setting
    wi = only(filter(f -> f["code"] == "acq.settings_within_design", r["findings"]))
    @test wi["level"] == "warn" && occursin("8/15 %: b1", wi["where"]) && occursin("10/10 %: b2", wi["where"])
    # ...and the same list by image uid, for the dialog's uid view; a list of designs has none
    named = Cecelia.acq_check(nothing, [Cecelia.acq_image_facts(u, "movie-" * u, m; channel_names = _ACQ_TOM) for (u, m) in (("b1", b), ("b2", b2))])
    nw = only(filter(f -> f["code"] == "acq.settings_differ", named["findings"]))
    @test occursin("movie-b1", nw["where"]) && occursin(": b1", nw["whereIds"]) && !occursin("movie-", nw["whereIds"])
    @test !haskey(by, "whereIds")
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
    # photons as shot-noise SNR (√ median p50): OT-I 1.45 → 1.2 and gBT 2.1 → 1.45 are a problem, P14
    # 2.5 → 1.58 borderline — a note, not a verdict step
    fp(w) = only(filter(f -> f["code"] == "acq.few_photons" && f["subs"]["what"] == w, r["findings"]))
    @test fp("OT-I")["level"] == "warn" && fp("gBT")["level"] == "warn" && fp("P14")["level"] == "info"
    @test fp("OT-I")["short"] == "OT-I carries ~1.4 photons per voxel (SNR 1.2)"
    # balance: OT-I is short of photons where P14 is only borderline — detection differs by object
    bal = only(filter(f -> f["code"] == "acq.balance", r["findings"]))
    @test bal["level"] == "warn" && bal["subs"]["what"] == "OT-I" && bal["subs"]["variable"] == "P14"
    # ...and with every compared object as starved as the next, the ratio is no finding
    eq = deepcopy(plan); eq["compare"] = ["OT-I", "gBT"]
    @test !any(f -> f["code"] == "acq.balance", Cecelia.acq_check(eq, facts)["findings"])
    # shared structure against the movie's own baseline (median r over its plan pairs): in A the pairs
    # sit at ~0.135, OT-I ~ empty (0.69) stands well out — a reference, so a note — and OT-I ~ gBT (0.3)
    # stands a little out (a note); in B P14 ~ gBT (0.57 over 0.12) is a warning
    sh(c, w) = only(filter(f -> f["code"] == c && f["subs"]["what"] == w, r["findings"]))
    @test sh("acq.shares_reference", "OT-I ~ empty")["level"] == "info"
    @test sh("acq.shares_object", "OT-I ~ gBT")["level"] == "info"
    @test sh("acq.shares_object", "P14 ~ gBT")["level"] == "warn" && sh("acq.shares_object", "P14 ~ gBT")["subs"]["base"] == 0.12
    @test !any(f -> f["code"] == "acq.shares_all" || f["code"] == "acq.shares_no_baseline", r["findings"])
    # a point object is counted, not read for its noise
    pts = deepcopy(plan); pts["objects"][2]["kind"] = "points"
    @test !any(f -> f["code"] == "acq.few_photons" && f["subs"]["what"] == "OT-I", Cecelia.acq_check(pts, facts)["findings"])
    # per-movie rows: design, ratio, the dimmest object, max r
    row = only(filter(x -> x["uid"] == "b1", r["rows"]))
    @test row["design"] == "B" && row["dimmest"] == "OT-I" && isapprox(row["ratio"], 16 / 6) && row["sharedR"] == 0.57 && row["sharedBase"] == 0.12
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

    # the verdict's rule part: one step per warning code, the objects listed, per stage
    @test r["verdict"] == "Collect more light for OT-I and gBT; separate the P14 ~ gBT channels; brighten OT-I; use one laser setting per design."
    fv = Cecelia.acq_verdict(Cecelia.acq_check(merge(plan, Dict{String,Any}("stage" => "final")), facts)["findings"], "final")[2]
    @test startswith(fv, "Denoise OT-I and gBT; correct bleed-through between P14 ~ gBT; check detection per object") && !occursin("laser", fv)
    @test Cecelia.acq_verdict(Dict{String,Any}[], "final")[2] == "Analyse as planned." &&
          Cecelia.acq_verdict(Dict{String,Any}[], "test")[2] == "Ready to acquire as planned."
    @test Cecelia._acq_list(["A", "B", "C"]) == "A, B and C"

    # final data: the same findings, each leading with what to do in the analysis
    @test r["stage"] == "test" && Cecelia.acq_stage(nothing) == "test" && Cecelia.acq_stage(Dict("stage" => "x")) == "test"
    fr = Cecelia.acq_check(merge(plan, Dict{String,Any}("stage" => "final")), facts)
    @test fr["stage"] == "final" && _acq_codes(fr) == codes
    @test !any(f -> haskey(f, "thisData"), fr["findings"])
    for (t, f) in zip(r["findings"], fr["findings"])
        @test f["long"] == get(t, "thisData", t["long"])
    end
    @test only(filter(f -> f["code"] == "acq.settings_within_design", fr["findings"]))["long"] ==
          "Compare intensities only between movies with the same laser setting; check detection in each group."
    # a movie's finding reads its set finding's analysis line
    @test first(filter(f -> f["code"] == "acq.shares_object_image", r["findings"]))["thisData"] ==
          Cecelia.qc_text("acq.shares_object.data").long
end

@testset "acq_check — against the set: baselines and movies out of line" begin
    plan = Dict{String,Any}("designs" => [Dict("id" => "A", "key" => Cecelia.acq_design_key(_ACQ_CTDR))],
                            "objects" => [Dict("name" => "X", "channels" => Dict("A" => 0)), Dict("name" => "Y", "channels" => Dict("A" => 2))],
                            "references" => [Dict("name" => "e", "channels" => Dict("A" => 1))])
    m(; p50 = 5.0, sh = 0.1) = _acq_meta(objs = Dict(0 => (p50, 30.0, nothing), 2 => (5.0, 30.0, nothing)),
                                         shared = Dict("0-1" => sh, "0-2" => sh, "1-2" => sh))
    # every pair high: the leak can't be told from shared tissue — one note, nothing stands out
    hi = Cecelia.acq_check(plan, [_acq_f("a", m(sh = 0.6)), _acq_f("b", m(sh = 0.6))])
    @test only(filter(f -> f["code"] == "acq.shares_all", hi["findings"]))["where"] == "a, b"
    @test !any(f -> startswith(f["code"], "acq.shares_object"), hi["findings"])
    # under three pairs there is no baseline — said so, not judged
    two = deepcopy(plan); empty!(two["references"])
    nb = Cecelia.acq_check(two, [_acq_f("a", m(sh = 0.9))])
    @test "acq.shares_no_baseline" in _acq_codes(nb) && !("acq.shares_object" in _acq_codes(nb))
    # a movie dimmer than the set's others: a note on that movie
    fs = [_acq_f("a", m()), _acq_f("b", m()), _acq_f("c", m()), _acq_f("d", m(p50 = 0.8))]
    o = Cecelia.acq_check(plan, fs)
    @test _acq_codes(o; scope = "d") == ["acq.dim_movie"]
    @test only(filter(f -> f["code"] == "acq.dim_movie", o["findings"]))["short"] == "X dimmer than in the set's other movies (~0.8 vs ~5.0 photons)"
    @test isempty(_acq_codes(o; scope = "a"))
end

@testset "acq_check — every finding renders from the catalog" begin
    plan = Dict{String,Any}("designs" => [Dict("id" => "A", "key" => "")],
                            "objects" => [Dict("name" => "X", "channels" => Dict("A" => 0)),
                                          Dict("name" => "Y", "channels" => Dict("A" => 1))])
    m = _acq_meta(objs = Dict(0 => (1.0, 30.0, 10.0), 1 => (1.0, 3.0, nothing)), shared = Dict("0-1" => 0.9),
                  brightz = false, depth = Dict(0 => -30.0, 1 => -30.0))
    r = Cecelia.acq_check(plan, [Cecelia.acq_image_facts("a", "x", m), Cecelia.acq_image_facts("b", "y", m)])   # unnamed
    @test all(f -> haskey(Cecelia.QC_TEXT, f["key"]) && !occursin("{", f["short"] * f["long"] * get(f, "thisData", "")), r["findings"])
    # every `.data` line belongs to a finding
    @test all(k -> haskey(Cecelia.QC_TEXT, k[1:end-5]), [k for k in keys(Cecelia.QC_TEXT) if startswith(k, "acq.") && endswith(k, ".data")])

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
