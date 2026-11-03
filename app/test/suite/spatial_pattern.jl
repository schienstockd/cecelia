# ── spatialAnalysis.spatialPattern — params, QC findings, sidecar discovery + observer slice ──
# docs/todo/SPATIAL_PATTERN_PLAN.md. The statistics themselves are tested in Python
# (python/cecelia/tests/test_spatial_pattern_utils.py).

@testset "spatialPattern dispatch + params" begin
    @test Cecelia._task_from_fun_name("spatialAnalysis.spatialPattern") isa SpatialPattern
    p = Cecelia.parse_spatial_pattern_params(Dict{String,Any}())
    @test p.graphSuffix == "default" && p.patternSuffix == "default" && p.rMax == 100.0 &&
          p.nPermutations == 1000 && isempty(p.pops)
    good = Dict{String,Any}("pops" => ["B/qc"], "graphSuffix" => "immune")
    @test begin validate_params(SpatialPattern(), good); true end
    @test_throws ParamValidationError validate_params(SpatialPattern(), merge(good, Dict{String,Any}("rMax" => 0)))
    @test_throws ParamValidationError validate_params(SpatialPattern(), Dict{String,Any}("graphSuffix" => "immune"))
end

@testset "spatialPattern QC findings" begin
    codes(f) = Set(String(x["code"]) for x in f)
    rec(; n = 200, cov = 1.0, frac = 0.3) = Dict{String,Any}("pop" => "B/qc", "nTarget" => n,
        "coverage" => cov, "targetFraction" => frac, "significant" => true)
    @test codes(Cecelia._spatial_pattern_findings(Any[])) == Set(["pattern.no_targets"])
    @test isempty(Cecelia._spatial_pattern_findings([rec()]))
    # a population whose segmentation the graph was not built over → its cells are not nodes
    @test codes(Cecelia._spatial_pattern_findings([rec(n = 0, cov = 0.0, frac = 0.0)])) == Set(["pattern.not_in_graph"])
    @test "pattern.low_coverage" in codes(Cecelia._spatial_pattern_findings([rec(cov = 0.5)]))
    @test "pattern.too_few" in codes(Cecelia._spatial_pattern_findings([rec(n = 5)]))
    # the graph is (almost) only this population → the shuffle has nothing to compare against
    @test "pattern.target_is_reference" in codes(Cecelia._spatial_pattern_findings([rec(frac = 0.99)]))
    @test !("pattern.target_is_reference" in codes(Cecelia._spatial_pattern_findings([rec(frac = 0.6)])))
end

@testset "spatialPattern sidecar — own directory, discovery, observer slice, run kind" begin
    td = mktempdir()
    try
        img = CciaImage(; dir = td)
        @test img_pattern_suffixes(img) == String[]
        @test endswith(img_pattern_path(img, "a"), joinpath("spatialPattern", "a.json"))
        mkpath(img_pattern_dir(img))
        rec = Dict("pop" => "B/qc", "nTarget" => 180, "nnRatio" => 0.71, "pValue" => 0.001)
        for s in ("b", "a")
            write(img_pattern_path(img, s), JSON3.write(Dict("graphSuffix" => "immune",
                  "nPermutations" => 999, "rMax" => 100.0, "records" => [rec])))
        end
        @test img_pattern_suffixes(img) == ["a", "b"]
        # NOT an interaction run: the stats readers never see a pattern sidecar
        @test isempty(img_stats_suffixes(img)) && isempty(Cecelia._collect_contact_stats(img))
        ps = Cecelia._collect_pattern_stats(img)
        @test [x.suffix for x in ps] == ["a", "b"]
        @test ps[1].graphSuffix == "immune" && ps[1].records[1][:nnRatio] == 0.71
        # listed + deletable through the analysis-run registry
        kind = only(k for k in Cecelia.RUN_KINDS if k.kind == "patterns")
        @test "spatialAnalysis.spatialPattern" in kind.funs
        @test [r.key for r in kind.list(img)] == ["a", "b"]
        kind.delete(img, only(r for r in kind.list(img) if r.key == "a"))
        @test img_pattern_suffixes(img) == ["b"]
    finally
        rm(td; recursive = true, force = true)
    end
end

@testset "per-image metrics → summary plot grouped by attribute (SPATIAL_PATTERN_PLAN D8)" begin
    tds = [mktempdir() for _ in 1:4]
    try
        uids = ["i1", "i2", "i3", "i4"]
        imgs = [CciaImage(; uid = u, dir = d) for (u, d) in zip(uids, tds)]
        # i1–i2 "PBS" read ~random, i3–i4 "DTx" read clustered; every image has run "a", only i1 has "z"
        ratios = Dict("i1" => 1.02, "i2" => 0.98, "i3" => 0.71, "i4" => 0.69)
        for (img, u) in zip(imgs, uids)
            mkpath(img_pattern_dir(img))
            recs = [Dict("pop" => "B/qc", "nnRatio" => ratios[u], "ceR" => 0.9, "zScore" => nothing),
                    Dict("pop" => "T/qc", "nnRatio" => 1.0, "ceR" => 1.0, "zScore" => 0.1)]
            write(img_pattern_path(img, "a"), JSON3.write(Dict("records" => recs)))
            u == "i1" && write(img_pattern_path(img, "z"), JSON3.write(Dict("records" => recs)))
        end
        df, sfx, sfxs = image_metric_frame(imgs, uids, "spatialPattern")
        @test sfx == "a" && sfxs == ["a", "z"]           # the run on EVERY image, not the first listed
        @test nrow(df) == 8 && Set(df.pop) == Set(["B/qc", "T/qc"])
        @test isnan(only(df[(df.uID .== "i1") .& (df.pop .== "B/qc"), :zScore]))   # null → a gap, not 0

        attr = Dict("i1" => "PBS", "i2" => "PBS", "i3" => "DTx", "i4" => "DTx")
        r = image_metric_summary(imgs, uids, "spatialPattern", "boxplot"; measure = "nnRatio", pop = "B/qc",
                                 attr_map = attr, stats_enabled = true, stats_test = :ttest)
        @test r["pops"] == ["B/qc", "T/qc"] && r["suffix"] == "a" && r["nImages"] == 4
        @test sort([s["uID"] for s in r["series"]]) == ["DTx", "PBS"]    # one series per group
        @test all(s -> s["n"] == 2, r["series"])                       # each dot is an IMAGE
        cmp = r["comparisons"]
        @test cmp["test"] == "ttest" && cmp["n"] == [2, 2] && cmp["pValue"] < 0.05
        # without an attribute, each population pools its images into one series
        r2 = image_metric_summary(imgs, uids, "spatialPattern", "points"; measure = "nnRatio")
        @test sort([s["pop"] for s in r2["series"]]) == ["B/qc", "T/qc"]
        # a run absent from an image drops only that image
        r3 = image_metric_summary(imgs, uids, "spatialPattern", "bar"; measure = "ceR", suffix = "z")
        @test r3["nImages"] == 1
        # the CSV export: one row per (image, population), the image as identity
        raw = image_metric_summary(imgs, uids, "spatialPattern", "boxplot"; measure = "nnRatio", raw = true)
        @test raw["chartType"] == "raw" && length(raw["rows"]) == 8
        @test Set(r["uID"] for r in raw["rows"]) == Set(uids)
        @test_throws KeyError image_metric_frame(imgs, uids, "nope")
    finally
        foreach(d -> rm(d; recursive = true, force = true), tds)
    end
end
