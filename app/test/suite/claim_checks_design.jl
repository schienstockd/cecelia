# Claim checks over groups of images and the set's experiment design (docs/todo/EXPERIMENT_DESIGN_PLAN.md,
# T3): a claim that compares panel A with panel B counts mice per panel, names an attribute the panels
# can't be told apart from, and says where it departs from the design. Every seeded fault has a clean
# twin. Uses claim_checks.jl's `_cc_*` helpers.

# one image per mouse, with its panel and imaging date
function _cd_image(uid, panel, date, mouse)
    img = _cc_image(uid; mouse = mouse)
    img.attr["Panel"] = panel; img.attr["Date"] = date
    img
end
# panel A on two days, panel B on two others (Date confounded) — or, `mixed`, both panels on both days
function _cd_imgs(; mixed = false)
    da, db = mixed ? (("D1", "D2"), ("D1", "D2")) : (("D1", "D2"), ("D3", "D4"))
    vcat([_cd_image("a$i", "A", da[1 + (i > 2)], "$i") for i in 1:4],
         [_cd_image("b$i", "B", db[1 + (i > 2)], "$(i + 4)") for i in 1:4])
end
_cd_design(; kw...) = ExperimentDesign(; group = ["Panel"], parents = ["Mouse"], status = Cecelia.DESIGN_CONFIRMED, kw...)
_cd_run(imgs; group = ["Panel"], unit = ["Mouse"], design = _cd_design()) =
    claim_checks(ClaimScope(_cc_set(imgs), ["B/qc"], "live.track.speed"; unit_attrs = unit, group_attrs = group, design = design))
_cd_s2(r) = only(vcat(_cc_find(r, "claim.unit_count"), _cc_find(r, "claim.few_units")))["detail"]["evidence"]

@testset "claim checks: groups of images by an attribute" begin
    # one population, compared between the panels: units per panel, disjoint → unpaired, n = the smaller
    r = _cd_run(_cd_imgs(; mixed = true))
    ev = _cd_s2(r)
    @test ev["design"] == "unpaired"
    @test ev["n"] == 4
    @test sort!(collect(keys(ev["byPop"]))) == ["A", "B"]
    @test ev["minP"] ≈ 2 / binomial(8, 4)          # Mann–Whitney floor for 4 vs 4
    @test "S1.betweenGroups" in r["ran"]
    @test isempty(_cc_codes(r))

    # panel A imaged on D1/D2, B on D3/D4: Date can't be told apart from Panel
    conf = _cd_run(_cd_imgs())
    f = only(_cc_find(conf, "claim.group_confounded"))
    @test f["level"] == "warn"
    @test f["detail"]["evidence"]["attr"] == "Date"
    @test f["detail"]["check"] == "S1.betweenGroups"
    # …and not the mouse: it is the replicate, nested in its panel by design
    @test !any(x -> x["detail"]["evidence"]["attr"] == "Mouse", _cc_find(conf, "claim.group_confounded"))

    # an image with no panel is in no group, and S0 says so
    imgs = _cd_imgs(; mixed = true); imgs[1].attr["Panel"] = ""
    blank = _cd_run(imgs)
    @test only(_cc_find(blank, "claim.group_blank"))["detail"]["evidence"]["images"] == ["a1"]
    @test _cd_s2(blank)["byPop"]["A"] == ["2", "3", "4"]

    # three panels, disjoint: the floor of the weakest pairwise comparison, the two smallest groups
    imgs = vcat(_cd_imgs(; mixed = true), [_cd_image("c$i", "C", "D1", "$(i + 8)") for i in 1:2])
    three = _cd_run(imgs)
    f = only(_cc_find(three, "claim.few_units"))
    @test f["detail"]["evidence"]["design"] == "unpaired"
    @test f["detail"]["evidence"]["n"] == 2
    @test f["detail"]["evidence"]["minP"] ≈ 2 / binomial(2 + 4, 2)

    # one panel only: nothing to compare between, said so
    one = _cd_run(filter(i -> i.attr["Panel"] == "A", _cd_imgs(; mixed = true)))
    @test occursin("one Panel only", one["skipped"]["S1.betweenGroups"])
end

@testset "claim checks: the set's experiment design" begin
    imgs = _cd_imgs(; mixed = true)
    design_codes(r) = sort!([f["code"] for f in r["findings"] if startswith(f["code"], "claim.design_")])

    # no design: the check says it didn't run, nothing raised
    r = _cd_run(imgs; design = nothing)
    @test haskey(r["skipped"], "S2.design")
    @test isempty(design_codes(r))

    # a confirmed design the claim follows: ran, quiet
    r = _cd_run(imgs)
    @test "S2.design" in r["ran"]
    @test isempty(design_codes(r))
    @test any(l -> l["id"] == "S2.design" && l["state"] == "ok", r["checks"])

    # exploratory set, unconfirmed design: warnings
    @test design_codes(_cd_run(imgs; design = _cd_design(; mode = Cecelia.DESIGN_EXPLORATORY))) == ["claim.design_exploratory"]
    @test design_codes(_cd_run(imgs; design = _cd_design(; status = Cecelia.DESIGN_INFERRED))) == ["claim.design_unconfirmed"]

    # grouped or counted otherwise than the design: noted, not warned
    r = _cd_run(imgs; group = ["Date"])
    f = only(_cc_find(r, "claim.design_group_differs"))
    @test f["level"] == "info"
    @test f["subs"]["design"] == "Panel"
    r = _cd_run(imgs; unit = ["Date", "Mouse"])
    @test only(_cc_find(r, "claim.design_unit_differs"))["level"] == "info"
    # the same replicate named in another order is the same replicate
    @test isempty(_cc_find(_cd_run(imgs; unit = ["Mouse"], design = _cd_design(; parents = ["Mouse"])), "claim.design_unit_differs"))
    # a claim comparing populations in a designed set says so
    r = claim_checks(ClaimScope(_cc_set(imgs), ["B/qc", "T/qc"], "live.track.speed"; unit_attrs = ["Mouse"], design = _cd_design()))
    @test only(_cc_find(r, "claim.design_group_differs"))["subs"]["group"] == "the populations"
end

@testset "paired, partly paired, unpaired: one rule for the grid and the claim" begin
    # the rule from its definition: every replicate in every group / none in two / between
    @test first(replicate_pairing(Dict("A" => ["1", "2"], "B" => ["2", "1"]))) == :paired
    @test first(replicate_pairing(Dict("A" => ["1", "2"], "B" => ["3", "4"]))) == :unpaired
    k, shared = replicate_pairing(Dict("A" => ["1", "2", "3"], "B" => ["1", "2"]))
    @test k == :partial && shared == Set(["1", "2"])
    # three groups, every pair overlapping but no replicate in all three
    k, shared = replicate_pairing(Dict("A" => ["1"], "B" => ["1", "2"], "C" => ["2"]))
    @test k == :partial && isempty(shared)
    @test first(replicate_pairing(Dict("A" => ["1"]))) == :none

    # mice 1–4 imaged in both panels; mouse 5 in panel A only
    mk(uid, panel, mouse) = _cd_image(uid, panel, "D1", mouse)
    both = vcat([mk("a$i", "A", "$i") for i in 1:4], [mk("b$i", "B", "$i") for i in 1:4])
    part = vcat(both, [mk("a5", "A", "5")])
    d = _cd_design(; parents = ["Mouse"])
    grid(imgs) = experiment_design_grid(imgs, d)["paired"]

    # every mouse in both panels: the grid and the claim both call it paired
    @test grid(both) == ["Mouse"]
    rb = _cd_run(both; design = d)
    @test _cd_s2(rb)["design"] == "paired" && _cd_s2(rb)["n"] == 4
    @test isempty(_cc_find(rb, "claim.partly_paired"))

    # one mouse in one panel only: not paired in the grid, and not paired in the claim either —
    # tested on the four mice that hold both panels, and said so
    @test isempty(grid(part))
    rp = _cd_run(part; design = d)
    ev = _cd_s2(rp)
    @test ev["design"] == "partly paired" && ev["n"] == 4 && ev["minP"] ≈ 2 / 2^4
    f = only(_cc_find(rp, "claim.partly_paired"))
    @test f["level"] == "info"
    @test f["short"] == "4 of 5 replicates (Mouse) hold every group"
end
