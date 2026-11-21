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

@testset "design replicate as the plot unit" begin
    # design_replicates: every parent needed — Mouse 1 with no date is no replicate, never "M1"
    mk(ps...) = (; attr = Dict{String,Any}(ps...))
    imgs = [mk("Date" => "D1", "Mouse" => "M1"), mk("Date" => "D2", "Mouse" => "M1"), mk("Mouse" => "M1")]
    reps = design_replicates(imgs, ["a", "b", "c"], ["Date", "Mouse"])
    @test reps == Dict("a" => "D1 × M1", "b" => "D2 × M1")              # same mouse number, two dates: two mice; the claim check's key
    @test isempty(design_replicates(imgs, ["a", "b", "c"], String[]))

    # four images: u1 + u2 are one mouse (M1) in panel A, u3 is M2 in A, u4 is M1 again in panel B;
    # u5 has no replicate and stays its own unit. Expected values from the definition — one value per
    # replicate per group, the mean over all that replicate's cells (Lord et al. 2020).
    df = DataFrame("value_name" => fill("A", 10), "pop" => fill("/p", 10),
                   "uID" => ["u1", "u1", "u2", "u3", "u3", "u3", "u4", "u4", "u5", "u5"],
                   "m"   => [1.0, 3.0, 5.0, 10.0, 10.0, 10.0, 2.0, 4.0, 7.0, 9.0])
    unit_map = Dict("u1" => "D1 × M1", "u2" => "D1 × M1", "u3" => "D1 × M2", "u4" => "D1 × M1")
    attr_map = Dict("u1" => "A", "u2" => "A", "u3" => "A", "u4" => "B", "u5" => "B")
    r = Cecelia._summary_agg(df, "boxplot"; measure = "m", granularity = :cell, nbins = 10,
                             normalize = :none, by_image = true, stat_unit = :image,
                             unit_map = unit_map, attr_map = attr_map, stats_enabled = true)
    byg = Dict(s["uID"] => s for s in r["series"])
    @test byg["A"]["n"] == 2 && byg["A"]["median"] == (3.0 + 10.0) / 2  # M1 = mean(1,3,5) = 3, M2 = 10
    @test byg["B"]["n"] == 2 && byg["B"]["median"] == (3.0 + 8.0) / 2   # M1 in B = mean(2,4); u5 = mean(7,9)
    codes = [f["code"] for f in r["comparisons"]["findings"]]
    @test !("stats.cells_as_replicates" in codes)                      # n counts mice, not cells
    # the same frame with cells as the unit: the finding is there
    r0 = Cecelia._summary_agg(df, "boxplot"; measure = "m", granularity = :cell, nbins = 10,
                              normalize = :none, by_image = true, attr_map = attr_map, stats_enabled = true)
    @test "stats.cells_as_replicates" in [f["code"] for f in r0["comparisons"]["findings"]]

    # the SuperPlot layer: each replicate mean with its unit, and the cells under it with theirs, so
    # the renderer can colour a cell by the mean it went into (Lord et al. 2020)
    for chart in ("boxplot", "points")
        rp = Cecelia._summary_agg(df, chart; measure = "m", granularity = :cell, nbins = 10, normalize = :none,
                                  by_image = true, stat_unit = :image, unit_map = unit_map, attr_map = attr_map,
                                  raw_points = true)
        a = only(s for s in rp["series"] if s["uID"] == "A")
        @test a["points"] == [3.0, 10.0]
        @test a["pointUnits"] == ["D1 × M1 | A", "D1 × M2 | A"]
        @test a["cellPoints"] == [1.0, 3.0, 5.0, 10.0, 10.0, 10.0]
        @test a["cellUnits"] == ["D1 × M1 | A", "D1 × M1 | A", "D1 × M1 | A", "D1 × M2 | A", "D1 × M2 | A", "D1 × M2 | A"]
        b = only(s for s in rp["series"] if s["uID"] == "B")
        @test Set(b["cellUnits"]) == Set(b["pointUnits"])               # every cell's replicate is a dot
    end
    # cells as the unit, or images as the unit: no layer
    r1 = Cecelia._summary_agg(df, "boxplot"; measure = "m", granularity = :cell, nbins = 10, normalize = :none,
                              by_image = true, stat_unit = :image, attr_map = attr_map, raw_points = true)
    @test !any(s -> haskey(s, "cellPoints"), r1["series"])
end

@testset "Experiment design — what an exported figure records" begin
    proj = create_project!(name = "xd-fig-$(rand(1000:9999))")
    s  = add_set!(proj; name = "T cell types")
    s2 = add_set!(proj; name = "No design")
    a  = add_image!(s; name = "a", meta = Dict{String,Any}("ori_path" => "/tmp/a.tif"))
    b  = add_image!(s2; name = "b", meta = Dict{String,Any}("ori_path" => "/tmp/b.tif"))
    @test isempty(board_designs(proj, [a.uid, b.uid]))                 # no design anywhere: nothing recorded
    d = experiment_design_from(Dict("group" => ["Panel"], "parents" => ["Date", "Mouse"]))
    set_experiment_design!(proj, s.uid, d; change = "suggested")
    rec = only(board_designs(proj, [a.uid, b.uid]))                    # the undesigned set is left out
    @test rec["setUid"] == s.uid && rec["setName"] == "T cell types"
    @test rec["version"] == 1 && rec["hash"] == experiment_design_hash(d)
    @test rec["status"] == "inferred" && rec["group"] == ["Panel"] && rec["parents"] == ["Date", "Mouse"]
    @test isempty(board_designs(proj, [b.uid]))                        # a figure of set 2 only: none
    # the design changes after the figure: a new version and hash, which is how a reader tells
    confirmed = experiment_design_from(merge(experiment_design_dict(d), Dict("status" => "confirmed")))
    set_experiment_design!(proj, s.uid, confirmed; change = "confirmed")
    now = only(board_designs(proj, [a.uid]))
    @test now["version"] == 2 && now["hash"] != rec["hash"]
    @test haskey(board_export_provenance(proj, [a.uid]), "designs")
    rm(proj.root; recursive = true)
end
