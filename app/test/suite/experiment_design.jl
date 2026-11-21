# The experiment design record (app/src/experiment_design.jl): stored on the set, every change in its
# history with a content hash, shape errors refused, and the grid placing images by their attributes.
# The grid's set is shaped like the intravital T cell set the plan works through: two panels imaged on
# different days, mouse names restarting each day, a movie per position, one file not yet named.

@testset "Experiment design — record and history" begin
    proj = create_project!(name = "xd-test-$(rand(1000:9999))")
    s    = add_set!(proj; name = "T cell types")
    @test experiment_design(s) === nothing                       # the no-design state
    @test experiment_design_version(s) == 0

    d = experiment_design_from(Dict("question" => "Do the two panels find the same T cell types?",
                                    "group" => ["Panel"], "unit" => ["Movie"], "parents" => ["Date", "Mouse"],
                                    "plannedParents" => 3))
    @test d.mode == Cecelia.DESIGN_DESIGNED && d.status == Cecelia.DESIGN_INFERRED   # defaults
    set_experiment_design!(proj, s.uid, d; change = "suggested")
    reloaded = init_object(proj.uid, s.uid)
    @test experiment_design(reloaded) == d                       # persisted, round-trips
    @test reloaded.meta["experiment_design"]["mode"] == "designed"   # enums stored by name
    @test reloaded.name == "T cell types"                        # one-field commit

    # an unchanged write records nothing; a change appends version 2 with a new hash
    set_experiment_design!(proj, s.uid, d)
    @test experiment_design_version(reloaded) == 1
    confirmed = experiment_design_from(merge(experiment_design_dict(d), Dict("status" => "confirmed")))
    set_experiment_design!(proj, s, confirmed; change = "confirmed")
    h = experiment_design_history(reloaded)
    @test [e["version"] for e in h] == [1, 2]
    @test h[2]["change"] == "confirmed" && h[2]["design"]["status"] == "confirmed"
    @test h[1]["hash"] == experiment_design_hash(d) != h[2]["hash"] == experiment_design_hash(confirmed)
    @test haskey(h[2]["by"], "profile") && h[2]["by"]["via"] == "app"

    # the hash covers meaning, not dict order
    @test experiment_design_hash(experiment_design_from(Dict("parents" => ["Date", "Mouse"], "unit" => "Movie",
            "group" => "Panel", "question" => d.question, "plannedParents" => "3"))) == experiment_design_hash(d)

    set_experiment_design!(proj, s.uid, nothing; change = "cleared")
    @test experiment_design(init_object(proj.uid, s.uid)) === nothing
    @test experiment_design_history(s)[end]["design"] === nothing
    @test experiment_design_version(s) == 3

    @test_throws ErrorException set_experiment_design!(proj, "nosuchset", d)
    rm(proj.root; recursive = true)
end

@testset "Experiment design — shape errors" begin
    @test_throws ErrorException experiment_design_from(Dict("group" => ["Panel"], "unit" => ["Panel"]))   # two roles
    @test_throws ErrorException experiment_design_from(Dict("parents" => ["Mouse", "Mouse"]))             # twice in one
    @test_throws ErrorException experiment_design_from(Dict("group" => [" "]))                            # blank name
    @test_throws ErrorException experiment_design_from(Dict("mode" => "confirmatory"))
    @test_throws ErrorException experiment_design_from(Dict("expect" => "up"))
    @test_throws ErrorException experiment_design_from(Dict("plannedUnits" => 0))
    @test experiment_design_from(Dict("plannedUnits" => "")).planned_units === nothing
    @test experiment_design_from(Dict("mode" => " Exploratory ")).mode == Cecelia.DESIGN_EXPLORATORY
    # four answers to "what do you expect": not sure is the default and differs from "no difference"
    @test experiment_design_from(Dict()).expect == Cecelia.EXPECT_UNSURE
    @test experiment_design_from(Dict("expect" => "same")).expect == Cecelia.EXPECT_SAME
    up = experiment_design_from(Dict("expect" => "higher", "expectIn" => " B "))
    @test up.expect == Cecelia.EXPECT_HIGHER && up.expect_in == "B"
    @test experiment_design_from(Dict("expect" => "same", "expectIn" => "B")).expect_in == ""   # only with a direction
end

@testset "Experiment design — grid" begin
    proj = create_project!(name = "xd-grid-$(rand(1000:9999))")
    s    = add_set!(proj; name = "T cell types")
    attr(p, day, m, mv) = Dict("Panel" => p, "Date" => day, "Mouse" => m, "Movie" => mv)
    add_image!(s; name = "M1-1", attr = attr("A", "25 Nov", "M1", "M1-1"))
    add_image!(s; name = "M1-2", attr = attr("A", "25 Nov", "M1", "M1-2"))
    add_image!(s; name = "M2-1", attr = attr("A", "25 Nov", "M2", "M2-1"))
    add_image!(s; name = "M1-1b", attr = attr("B", "1 Dec", "M1", "M1-1"))   # M1 again: another day, another mouse
    add_image!(s; name = "M2-1c", attr = Dict("Panel" => "B", "Date" => "1 Dec"))   # mouse not named yet
    d = experiment_design_from(Dict("group" => "Panel", "unit" => "Movie", "parents" => ["Date", "Mouse"],
                                    "plannedParents" => 3))

    g = experiment_design_grid(s, d)
    @test [(c["group"], c["parent"], length(c["units"])) for c in g["cells"]] ==
          [(["A"], ["25 Nov", "M1"], 2), (["A"], ["25 Nov", "M2"], 1), (["B"], ["1 Dec", "M1"], 1)]
    @test [(x["group"], x["nParents"], x["nUnits"], x["nImages"]) for x in g["groups"]] == [(["A"], 2, 3, 3), (["B"], 1, 1, 1)]
    @test [(u["name"], u["missing"]) for u in g["unassigned"]] == [("M2-1c", ["Mouse", "Movie"])]
    @test isempty(g["missingAttrs"]) && g["planned"]["parents"] == 3
    # each panel imaged on its own day: Date is confounded with Panel; Mouse (M1 in both) is not
    @test g["confounded"] == ["Date"]
    @test isempty(g["paired"])            # no date and no mouse holds both panels

    # values the dialog has not written yet place the image without touching it
    uid = s._images[end].uid
    g2 = experiment_design_grid(s, d; attrs = Dict(uid => Dict("Mouse" => "M2", "Movie" => "M2-1")))
    @test isempty(g2["unassigned"]) && g2["groups"][2]["nParents"] == 2
    @test !haskey(s._images[end].attr, "Mouse")

    # no unit attribute: each image is its own unit; two images of one movie count once with one
    rerun = add_image!(s; name = "M1-2_0002", attr = attr("A", "25 Nov", "M1", "M1-2"))
    @test experiment_design_grid(s, d)["groups"][1]["nUnits"] == 3          # same movie, one unit
    by_image = experiment_design_from(Dict("group" => "Panel", "parents" => ["Date", "Mouse"]))
    @test experiment_design_grid(s, by_image)["groups"][1]["nUnits"] == 4

    # a design set up before any image carries its attributes: named, not refused
    early = experiment_design_from(Dict("group" => "Tissue", "parents" => "Donor"))
    ge = experiment_design_grid(s, early)
    @test ge["missingAttrs"] == ["Tissue", "Donor"] && length(ge["unassigned"]) == 6 && isempty(ge["cells"])

    # an attribute shared across groups is not confounded; one group alone has nothing to confound
    add_image!(s; name = "M3-1", attr = attr("B", "25 Nov", "M3", "M3-1"))
    @test isempty(experiment_design_grid(s, d)["confounded"])
    @test isempty(experiment_design_grid(s._images[1:3], d)["confounded"])
    rm(proj.root; recursive = true)
end

@testset "Experiment design — paired and blocked levels" begin
    # every mouse carries both conditions → paired on Mouse; every day images both, but each mouse one
    # condition → blocked on Date, not paired on Date × Mouse
    proj = create_project!(name = "xd-paired-$(rand(1000:9999))")
    s    = add_set!(proj; name = "pairs")
    for (m, c) in (("M1", "ctrl"), ("M1", "drug"), ("M2", "ctrl"), ("M2", "drug"))
        add_image!(s; name = "$(m)-$(c)", attr = Dict("Cond" => c, "Mouse" => m, "Date" => "d1"))
    end
    by_mouse = experiment_design_from(Dict("group" => "Cond", "parents" => "Mouse"))
    @test experiment_design_grid(s, by_mouse)["paired"] == ["Mouse"]

    t = add_set!(proj; name = "blocks")
    for (d, m, c) in (("d1", "M1", "ctrl"), ("d1", "M2", "drug"), ("d2", "M1", "ctrl"), ("d2", "M2", "drug"))
        add_image!(t; name = "$(d)$(m)", attr = Dict("Cond" => c, "Mouse" => m, "Date" => d))
    end
    nested = experiment_design_from(Dict("group" => "Cond", "parents" => ["Date", "Mouse"]))
    gt = experiment_design_grid(t, nested)
    @test gt["paired"] == ["Date"]
    # each mouse holds one condition: nested by design, so the replicate is not reported as confounded
    @test isempty(gt["confounded"])
    # one group: nothing to pair
    @test isempty(experiment_design_grid(t._images[[1, 3]], nested)["paired"])
    rm(proj.root; recursive = true)
end
