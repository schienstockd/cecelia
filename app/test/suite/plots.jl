# ── plot suite testsets ───────────────────────────────────────────────
# 12 sections covering the plot family: branch/track value_names come from the sidecars,
# plot groupBy (generic categorical sub-axis), plot percent (% positive of a 0/1 measure),
# plot count (raw + proportion normalize) population summary, plot raw (per-datapoint
# export), plot statUnit=image (per-image mean), plot matrix (heatmap: profile + crosstab),
# plot attribute grouping (compare by attr), plot_summary_data cross-image,
# plot_summary_data multi-segmentation targets, plot_summary_data helpers. Extracted from
# suite.jl to keep it small enough to merge without EOF conflicts on every append. The
# extracted file loads inside this file's aggregating testset scope, so any helpers defined
# earlier in suite.jl are still in scope (lexical include).

@testset "branch value_names come from the sidecars" begin
    proj = create_project!(name="bvn-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    img = add_image!(s; name="img-a")
    dir = img_label_props_dir(img); mkpath(dir)
    @test img_branch_value_names(img) == String[]          # nothing banked yet
    for f in ("SHG__branch.h5ad", "DCs__branch.h5ad", "B.h5ad", "B__tracks.h5ad")
        touch(joinpath(dir, f))
    end
    # only the __branch sidecars, and NOT the cell/track tables that sit beside them
    @test img_branch_value_names(img) == ["DCs", "SHG"]
    @test img_branch_props_path(img, "SHG") == joinpath(dir, "SHG__branch.h5ad")
end

# The same question for tracks, and the reason it needs its own answer: "is this image tracked" was
# being asked of the RUN LOG (does a `tracking.*` entry exist), which a project migrated from the R
# version answers "no" for while its `{vn}__tracks.h5ad` sits on disk — so the guide picker declared
# "needs a tracked image" over a project whose tracks had already been clustered. The sidecar is the
# state; the run log is only the provenance of runs this app happened to execute.
@testset "track value_names come from the sidecars" begin
    proj = create_project!(name="tvn-$(rand(1000:9999))")
    s = add_set!(proj; name="set-A")
    # Explicit uid: `@testset` reseeds the RNG, so `gen_uid` would hand this image the SAME uid — and
    # the same directory — as the branch testset above, whose sidecars are already sitting in it
    # (docs/DEV.md → the `@testset` reseeds the global RNG trap; this test hit it on the first run).
    img = add_image!(s; name="img-a", uid="tvnImgA")
    dir = img_label_props_dir(img); mkpath(dir)
    @test img_track_value_names(img) == String[]           # nothing banked yet
    for f in ("B__tracks.h5ad", "T__tracks.h5ad", "B.h5ad", "T.h5ad", "SHG__branch.h5ad")
        touch(joinpath(dir, f))
    end
    # only the __tracks sidecars, and NOT the cell/branch tables that sit beside them
    @test img_track_value_names(img) == ["B", "T"]
    @test img_track_props_path(img, "B") == joinpath(dir, "B__tracks.h5ad")
    # and it does NOT consult the run log: no tracking entry was ever written above
    @test isempty(read_run_log(img))
end

@testset "plot groupBy (generic categorical sub-axis)" begin
    # split a numeric measure by a categorical column (the hmmPlotParams port): each (pop × level)
    # becomes its own series tagged with `group`. Deterministic synthetic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 7), "pop" => fill("/p", 7),
                   "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0, 99.0],
                   "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0,  NaN])   # last row: missing group → dropped
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=false, group_by="st")
    @test r["groupBy"] == "st"
    @test length(r["series"]) == 2                                  # two states, NaN-group row dropped
    @test Set(s["group"] for s in r["series"]) == Set(["1", "2"])
    byg = Dict(s["group"] => s for s in r["series"])
    @test byg["1"]["median"] == 2.0 && byg["2"]["median"] == 11.0   # NaN row excluded from state 2
    @test byg["2"]["n"] == 3
    # no group_by → single series, empty group label (back-compat)
    r0 = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test length(r0["series"]) == 1 && r0["series"][1]["group"] == "" && r0["groupBy"] === nothing

    # collapse_series: pool across pops/segmentations/images → series by groupBy level only. Two
    # pops, two value_names, but collapse + group_by="st" still yields exactly two series (1, 2).
    dfc = DataFrame("value_name" => ["A","A","B","B","A","B"], "pop" => ["/p","/q","/p","/q","/p","/q"],
                    "uID" => ["x","x","y","y","x","y"],
                    "m"  => [1.0, 2.0, 3.0, 10.0, 11.0, 12.0],
                    "st" => [1.0, 1.0, 1.0, 2.0,  2.0,  2.0])
    rc = Cecelia._summary_agg(dfc, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, group_by="st", collapse_series=true)
    @test length(rc["series"]) == 2
    @test Set(s["group"] for s in rc["series"]) == Set(["1", "2"])
    @test all(s["pop"] == "" && s["value_name"] == "" && s["uID"] == "" for s in rc["series"])
    @test Dict(s["group"] => s["n"] for s in rc["series"]) == Dict("1" => 3, "2" => 3)
    # collapse with no group_by → one pooled series over everything
    rc0 = Cecelia._summary_agg(dfc, "bar"; measure="m", granularity=:cell, nbins=10,
                               normalize=:none, by_image=true, collapse_series=true)
    @test length(rc0["series"]) == 1 && rc0["series"][1]["n"] == 6
end

@testset "plot percent (% positive of a 0/1 measure)" begin
    # "% of B cells in contact with a T cell" (`…cell.contact#…`) and "how many T cells are
    # clustered" (`…cell.is.aggregate`) are ONE question: the fraction of a population whose 0/1
    # measure is positive. Both were previously only reachable as a `bar` of the MEAN — an
    # unlabelled 0..1 fraction.

    # ── the detector: a property of the data, not a list of blessed column names ──
    @test Cecelia._is_boolean_measure([0, 1, 1, 0])
    @test Cecelia._is_boolean_measure([0.0, 1.0])
    @test Cecelia._is_boolean_measure([1, missing, NaN, 0])      # missing/non-finite ignored
    @test Cecelia._is_boolean_measure([0, 0, 0])                 # all-negative is still boolean
    @test !Cecelia._is_boolean_measure([0, 1, 2])
    @test !Cecelia._is_boolean_measure([0.0, 0.5, 1.0])
    @test !Cecelia._is_boolean_measure(["a", "b"])               # categorical, not boolean
    @test !Cecelia._is_boolean_measure(Float64[])                # nothing to judge
    @test !Cecelia._is_boolean_measure([missing, NaN])

    # ── Wilson score interval (Wilson 1927), against published values ──
    lo, hi = Cecelia._wilson_ci(5, 10)
    @test isapprox(lo, 0.23659, atol = 1e-4) && isapprox(hi, 0.76341, atol = 1e-4)
    # the case Wald gets WRONG: 0 of 10 → Wald says [0,0] ("certainly never"), Wilson keeps width
    lo0, hi0 = Cecelia._wilson_ci(0, 10)
    @test lo0 == 0.0 && isapprox(hi0, 0.27753, atol = 1e-4)
    # …and at p=1 the interval's exact upper bound IS 1 (float arithmetic lands 1 ulp short)
    lo1, hi1 = Cecelia._wilson_ci(10, 10)
    @test isapprox(lo1, 0.72247, atol = 1e-4) && isapprox(hi1, 1.0)
    @test all(isnan, Cecelia._wilson_ci(0, 0))                   # no data → no interval
    # symmetric about 0.5 (a sanity property of the interval, not of our arithmetic)
    @test isapprox(1 - Cecelia._wilson_ci(3, 10)[2], Cecelia._wilson_ci(7, 10)[1], atol = 1e-12)

    # ── the aggregation ──
    df = DataFrame("value_name" => fill("B", 10), "pop" => fill("/qc", 10),
                   "contact" => [1, 1, 1, 0, 0, 0, 0, 0, 0, 0])
    r = Cecelia._summary_agg(df, "percent"; measure="contact", granularity=:cell, nbins=10,
                             normalize=:none, by_image=false)
    @test r["chartType"] == "percent"
    @test r["valueLabel"] == "% positive"
    @test r["measureBoolean"] === true
    s = only(r["series"])
    @test s["value"] == 30.0 && s["n"] == 10 && s["nPositive"] == 3
    # bounds are the Wilson ones (as percentages) and BRACKET the estimate asymmetrically
    wl, wh = Cecelia._wilson_ci(3, 10)
    @test isapprox(s["lower"], 100wl) && isapprox(s["upper"], 100wh)
    @test s["lower"] < s["value"] < s["upper"]
    @test !isapprox(s["value"] - s["lower"], s["upper"] - s["value"])   # asymmetric — hence 2 bounds
    @test isapprox(s["ci95"], 100 * max(wh - 0.3, 0.3 - wl))            # the wider half-width

    # a percent chart must NOT carry rank/ANOVA comparisons — 0/1 data needs a proportion test
    df2 = vcat(df, DataFrame("value_name" => fill("T", 6), "pop" => fill("/qc", 6),
                             "contact" => [1, 1, 1, 1, 1, 0]))
    r2 = Cecelia._summary_agg(df2, "percent"; measure="contact", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false, stats_enabled=true)
    @test !haskey(r2, "comparisons")
    byvn = Dict(s["value_name"] => s for s in r2["series"])
    @test byvn["B"]["value"] == 30.0
    @test isapprox(byvn["T"]["value"], 500 / 6)

    # an all-missing series reports no percentage rather than a spurious 0%
    dfe = DataFrame("value_name" => fill("T", 3), "pop" => fill("/qc", 3),
                    "contact" => [NaN, NaN, NaN])
    se = only(Cecelia._summary_agg(dfe, "percent"; measure="contact", granularity=:cell, nbins=10,
                                   normalize=:none, by_image=false)["series"])
    @test isnan(se["value"]) && se["n"] == 0

    # ── measureBoolean rides along on the ORDINARY charts, so the panel can offer % positive ──
    rb = Cecelia._summary_agg(df, "bar"; measure="contact", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test rb["measureBoolean"] === true
    rn = Cecelia._summary_agg(DataFrame("value_name" => fill("B", 3), "pop" => fill("/qc", 3),
                                        "dist" => [1.5, 20.0, 3.25]), "bar";
                              measure="dist", granularity=:cell, nbins=10,
                              normalize=:none, by_image=false)
    @test rn["measureBoolean"] === false
    # a POPULATION SUMMARY substitutes a synthetic per-image count; counts of 0/1 are not a boolean
    # MEASURE, and offering "% positive" on them would be nonsense.
    dfp = DataFrame("value_name" => ["B", "T"], "pop" => ["/qc", "/qc"], "uID" => ["i1", "i1"])
    rp = Cecelia._summary_agg(dfp, "bar"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test rp["measureBoolean"] === false
end

@testset "plot count (raw + proportion normalize) — population summary" begin
    # two pops in two images; count → raw row counts; normalize=:fraction → each pop's share of
    # its image's plotted total (the population-summary plot). Deterministic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 10),
                   "pop" => ["/p","/p","/p","/q","/q", "/p","/q","/q","/q","/p"],
                   "uID" => ["x","x","x","x","x",       "y","y","y","y","y"])
    # image x: /p=3, /q=2 (total 5); image y: /p=2, /q=3 (total 5)
    bykey(r) = Dict((s["uID"], s["pop"]) => s["value"] for s in r["series"])
    raw = Cecelia._summary_agg(df, "count"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=true)
    @test raw["chartType"] == "count"
    rk = bykey(raw)
    @test rk[("x","A/p")] == 3.0 && rk[("x","A/q")] == 2.0
    @test rk[("y","A/p")] == 2.0 && rk[("y","A/q")] == 3.0
    prop = Cecelia._summary_agg(df, "count"; measure=nothing, granularity=:cell, nbins=0,
                                normalize=:fraction, by_image=true)
    @test prop["normalize"] == "fraction"
    pk = bykey(prop)
    @test pk[("x","A/p")] ≈ 0.6 && pk[("x","A/q")] ≈ 0.4     # 3/5, 2/5
    @test pk[("y","A/p")] ≈ 0.4 && pk[("y","A/q")] ≈ 0.6     # 2/5, 3/5
    @test Dict((s["uID"], s["pop"]) => s["n"] for s in prop["series"])[("x","A/p")] == 3

    # no measure + a DISTRIBUTION chart → each IMAGE is a point (its pop count), grouped by pop:
    # boxplot/beeswarm show within-pop variability and compare pops. A/p counts = [3(x),2(y)],
    # A/q = [2(x),3(y)] → each pop has 2 points (images), median 2.5.
    bx = Cecelia._summary_agg(df, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test bx["chartType"] == "boxplot"
    bybp = Dict(s["pop"] => s for s in bx["series"])
    @test Set(keys(bybp)) == Set(["A/p", "A/q"])
    @test bybp["A/p"]["n"] == 2 && bybp["A/p"]["median"] == 2.5
    # bar over the same per-image counts → mean (A/p mean of [3,2] = 2.5)
    br = Cecelia._summary_agg(df, "bar"; measure=nothing, granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test Dict(s["pop"] => s["value"] for s in br["series"])["A/p"] == 2.5

    # A SYNTHETIC METRIC IS NUMERIC BY CONSTRUCTION — never sniffed.
    #
    # `_is_categorical_col` guesses from the values, and a per-image count is a handful of small
    # integers, which its integer-level heuristic reads as CATEGORICAL. The panel then intersected
    # the spec's numeric charts with the categorical set and everything except `count` (kept
    # explicitly as measure-independent) disappeared — "population summary always defaults back to
    # count and you cannot select anything else", on the FIRST render, since the spec's first chart
    # is boxplot. The gate is "did the user name this column", so it covers `proportion` and any
    # later synthetic metric with no new name to remember.
    # backend chart names — the frontend's strip/violin both map onto `points` (backendChart)
    for (ct, nrm) in (("boxplot", :none), ("bar", :none), ("points", :none),
                      ("count", :none), ("bar", :fraction))
        r = Cecelia._summary_agg(df, ct; measure=nothing, granularity=:cell, nbins=10,
                                 normalize=nrm, by_image=true)
        @test r["measureType"] == "numeric"
        @test r["measureBoolean"] === false     # counts that happen to be 0/1 are not a boolean measure
    end
    # counts of exactly 1 are the worst case for the heuristic (a single integer level)
    df1 = DataFrame("value_name" => ["A","A"], "pop" => ["/p","/q"], "uID" => ["x","x"])
    @test Cecelia._summary_agg(df1, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:none, by_image=true)["measureType"] == "numeric"
    # …and a REAL categorical measure is still detected as categorical (the gate must not blanket
    # everything to numeric)
    dfc = DataFrame("value_name" => fill("A", 4), "pop" => fill("/p", 4),
                    "live.cell.hmm.state.movement" => [1.0, 2.0, 1.0, 2.0])
    @test Cecelia._summary_agg(dfc, "frequency"; measure="live.cell.hmm.state.movement",
                               granularity=:cell, nbins=10, normalize=:none,
                               by_image=false)["measureType"] == "categorical"

    # SPLIT BY POPULATION: two tracked pops (value_names B, T) each with clusters — proportion is
    # normalised WITHIN each value_name per image, not pooled across B+T.
    df2 = DataFrame("value_name" => ["B","B","B","T","T", "B","B","T","T","T"],
                    "pop" => ["/Dir","/Dir","/Mea","/Dir","/Mea", "/Dir","/Mea","/Dir","/Mea","/Mea"],
                    "uID" => ["x","x","x","x","x",              "y","y","y","y","y"])
    pr2 = Cecelia._summary_agg(df2, "count"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:fraction, by_image=true)
    p2 = Dict((s["uID"], s["pop"]) => s["value"] for s in pr2["series"])
    @test p2[("x","B/Dir")] ≈ 2/3 && p2[("x","B/Mea")] ≈ 1/3   # within B (image x: B tot 3)
    @test p2[("x","T/Dir")] ≈ 1/2 && p2[("x","T/Mea")] ≈ 1/2   # within T (image x: T tot 2)
    @test p2[("y","B/Dir")] ≈ 1/2 && p2[("y","T/Mea")] ≈ 2/3   # within B / T (image y)

    # COMPLETE CASES (R tidyr::complete): image y has no /q — it must still contribute a 0 to /q's
    # distribution, not be dropped. Without completion /q would have n=1 (only x) and median 1.
    df3 = DataFrame("value_name" => fill("A", 5),
                    "pop" => ["/p","/p","/q", "/p","/p"],
                    "uID" => ["x","x","x",    "y","y"])          # x: p=2 q=1 ; y: p=2 q=0 (missing)
    bx3 = Cecelia._summary_agg(df3, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:none, by_image=true)
    by3 = Dict(s["pop"] => s for s in bx3["series"])
    @test by3["A/q"]["n"] == 2 && by3["A/q"]["median"] == 0.5    # /q points [1(x), 0(y)]
    @test by3["A/p"]["n"] == 2 && by3["A/p"]["median"] == 2.0
    # proportion completes too: /q in image y = 0 / (y's A total 2) = 0 → points [1/3, 0]
    pr3 = Cecelia._summary_agg(df3, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                               normalize=:fraction, by_image=true)
    by3f = Dict(s["pop"] => s for s in pr3["series"])
    @test by3f["A/q"]["n"] == 2 && by3f["A/q"]["median"] ≈ 1/6

    # BY ATTRIBUTE (compareMode='by_attr'): with `attr_map`, each per-image row must be RELABELLED by
    # its attribute value and pooled per (attr, pop) — one series per (attr, pop), points = images.
    # Before this pin the substitution forced `by_image=false`, `_series_groups` ignored the attr map,
    # and the frontend showed a single boxplot per pop no matter what the attribute picker was set to.
    df4 = DataFrame("value_name" => fill("A", 10),
                    "pop" => ["/p","/p","/p","/q","/q","/p","/p","/q","/q","/q"],
                    "uID" => ["x1","x1","x1","x1","x1","x2","x2","x2","y1","y1"])
    amap = Dict("x1"=>"WT", "x2"=>"WT", "y1"=>"KO")
    bx4 = Cecelia._summary_agg(df4, "boxplot"; measure=nothing, granularity=:cell, nbins=10,
                                normalize=:none, by_image=false, attr_map=amap)
    seriesk = Set((s["uID"], s["pop"]) for s in bx4["series"])
    # /p exists under WT (images x1, x2) but NOT KO (y1 has 0 /p rows completed → 0 data point in KO)
    # /q exists under WT (x1, x2) AND KO (y1)
    @test ("WT", "A/p") in seriesk && ("WT", "A/q") in seriesk && ("KO", "A/q") in seriesk
    # WT points for /p: [3(x1), 2(x2)] → n=2 median=2.5; KO for /q: [2(y1)] → n=1 median=2
    byattr = Dict((s["uID"], s["pop"]) => s for s in bx4["series"])
    @test byattr[("WT","A/p")]["n"] == 2 && byattr[("WT","A/p")]["median"] == 2.5
    @test byattr[("KO","A/q")]["n"] == 1 && byattr[("KO","A/q")]["median"] == 2.0
end

@testset "plot raw (per-datapoint export)" begin
    # raw=true → one tidy row per datapoint (identity + value) for re-plotting externally, instead of
    # collapsing to box stats. Deterministic frame with label + a groupBy column; last row NaN measure.
    df = DataFrame("value_name" => fill("A", 5), "pop" => fill("/p", 5),
                   "uID" => ["x","x","y","y","y"], "label" => [1, 2, 3, 4, 5],
                   "m"  => [1.0, 2.0, 3.0, 4.0, NaN],
                   "st" => [1.0, 1.0, 2.0, 2.0, 2.0])
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=true, group_by="st", raw=true)
    @test r["chartType"] == "raw" && r["measure"] == "m" && r["groupBy"] == "st"
    @test length(r["rows"]) == 4                       # the NaN-measure row is dropped
    row1 = r["rows"][1]
    @test row1["uID"] == "x" && row1["label"] == "1" && row1["value_name"] == "A"
    @test row1["pop"] == "/p" && row1["value"] == 1.0 && row1["group"] == "1"
    @test [rw["value"] for rw in r["rows"]] == [1.0, 2.0, 3.0, 4.0]
    @test [rw["group"] for rw in r["rows"]] == ["1", "1", "2", "2"]

    # measure-less count chart → raw collapses to per-(image, pop) counts (no label column populated)
    dfc = DataFrame("value_name" => fill("A", 5), "pop" => ["/p","/p","/p","/q","/q"],
                    "uID" => ["x","x","x","x","x"])
    rc = Cecelia._summary_agg(dfc, "count"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=true, raw=true)
    @test rc["chartType"] == "raw" && rc["measure"] == "count"
    cbyp = Dict(rw["pop"] => rw["value"] for rw in rc["rows"])
    @test cbyp["/p"] == 3.0 && cbyp["/q"] == 2.0 && all(!haskey(rw, "label") for rw in rc["rows"])

    # TRACK granularity: `label` duplicates `track_id` in the track table → drop it, keep track_id.
    dft = DataFrame("value_name" => fill("A", 3), "pop" => fill("/_tracked", 3),
                    "uID" => ["x","x","y"], "label" => [10, 11, 12], "track_id" => [10, 11, 12],
                    "m" => [1.0, 2.0, 3.0])
    rt = Cecelia._summary_agg(dft, "boxplot"; measure="m", granularity=:track, nbins=10,
                              normalize=:none, by_image=true, raw=true)
    @test all(!haskey(rw, "label") for rw in rt["rows"])            # no meaningless label
    @test [rw["track_id"] for rw in rt["rows"]] == ["10", "11", "12"]

    # groupBy that ISN'T applied (its column isn't in the frame) → groupBy null + no group column,
    # so the export never carries an empty, misleading category column.
    rna = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                               normalize=:none, by_image=true, group_by="not_a_column", raw=true)
    @test rna["groupBy"] === nothing && all(!haskey(rw, "group") for rw in rna["rows"])
end

@testset "plot statUnit=image (per-image mean = each dot an image)" begin
    # collapse each image to its mean, then plot those per-image means (n = #images). Deterministic
    # frame: image x cells [1,3,5] (mean 3), image y cells [10,20,30] (mean 20).
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "uID" => ["x","x","x","y","y","y"], "m" => [1.0, 3.0, 5.0, 10.0, 20.0, 30.0])
    r = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                             normalize=:none, by_image=true, stat_unit=:image)
    @test length(r["series"]) == 1                     # images pooled into ONE box
    @test r["series"][1]["n"] == 2                      # two datapoints = two images
    @test r["series"][1]["median"] == 11.5 && r["series"][1]["mean"] == 11.5   # of [3, 20]
    # default (individual) + per-image scope → one box PER image, each over its own cells (n = 3);
    # image-mean instead pools those into a single box whose points are the two image means.
    r0 = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true)
    @test length(r0["series"]) == 2 && all(s["n"] == 3 for s in r0["series"])
    # bar over per-image means → mean of the image means
    rb = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image)
    @test rb["series"][1]["value"] == 11.5 && rb["series"][1]["n"] == 2

    # with groupBy: per-image mean WITHIN each level → each level's points are its image means.
    df2 = DataFrame("value_name" => fill("A", 8), "pop" => fill("/p", 8),
                    "uID" => ["x","x","y","y","x","x","y","y"],
                    "m"  => [2.0, 4.0, 6.0, 8.0, 20.0, 20.0, 30.0, 10.0],
                    "st" => [1.0, 1.0, 1.0, 1.0, 2.0,  2.0,  2.0,  2.0])
    r2 = Cecelia._summary_agg(df2, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, group_by="st", stat_unit=:image)
    byg = Dict(s["group"] => s for s in r2["series"])
    @test byg["1"]["n"] == 2 && byg["1"]["median"] == 5.0     # st1: x[2,4]→3, y[6,8]→7 → [3,7]
    @test byg["2"]["n"] == 2 && byg["2"]["median"] == 20.0    # st2: x[20,20]→20, y[30,10]→20

    # with attr_map: one series PER ATTRIBUTE value, points = the images in it.
    dfa = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                    "uID" => ["x","x","y","y","z","z"], "m" => [2.0, 4.0, 6.0, 8.0, 100.0, 100.0])
    am = Dict("x" => "ctrl", "y" => "ctrl", "z" => "treat")
    ra = Cecelia._summary_agg(dfa, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image, attr_map=am)
    bya = Dict(s["uID"] => s for s in ra["series"])
    @test Set(keys(bya)) == Set(["ctrl", "treat"])
    @test bya["ctrl"]["n"] == 2 && bya["ctrl"]["median"] == 5.0   # images x(3), y(7) → [3,7]
    @test bya["treat"]["n"] == 1                                  # image z only

    # raw export honours it too: rows are the per-image means (label empty, value = the mean)
    rr = Cecelia._summary_agg(df, "boxplot"; measure="m", granularity=:cell, nbins=10,
                              normalize=:none, by_image=true, stat_unit=:image, raw=true)
    @test [rw["value"] for rw in rr["rows"]] == [3.0, 20.0]
    @test all(!haskey(rw, "label") for rw in rr["rows"]) && Set(rw["uID"] for rw in rr["rows"]) == Set(["x","y"])

    # image_agg=:median collapses each image by MEDIAN, not mean — distinguishable on skewed images:
    # x cells [1,2,9] (mean 4, median 2), y [10,20,90] (mean 40, median 20).
    dfs = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                    "uID" => ["x","x","x","y","y","y"], "m" => [1.0, 2.0, 9.0, 10.0, 20.0, 90.0])
    rmean = Cecelia._summary_agg(dfs, "bar"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=true, stat_unit=:image, image_agg=:mean)
    rmed  = Cecelia._summary_agg(dfs, "bar"; measure="m", granularity=:cell, nbins=10,
                                 normalize=:none, by_image=true, stat_unit=:image, image_agg=:median)
    @test rmean["series"][1]["value"] == 22.0   # mean of image means [4, 40]
    @test rmed["series"][1]["value"] == 11.0    # mean of image medians [2, 20]
    rmedraw = Cecelia._summary_agg(dfs, "boxplot"; measure="m", granularity=:cell, nbins=10,
                                   normalize=:none, by_image=true, stat_unit=:image, image_agg=:median, raw=true)
    @test [rw["value"] for rw in rmedraw["rows"]] == [2.0, 20.0]   # per-image medians
end

@testset "plot matrix (heatmap: profile + crosstab)" begin
    # PROFILE: rows = measures, cols = category levels; cell = mean(measure | level). Pools the whole
    # frame into one grid (no series). Deterministic synthetic frame — no fixture.
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "speed" => [1.0, 3.0, 10.0, 12.0, NaN, 5.0],
                   "angle" => [0.1, 0.3, 0.9, 1.1, 0.5, 0.5],
                   "st"    => [1.0, 1.0, 2.0,  2.0,  2.0, NaN])   # last row: NaN level → dropped
    pr = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=false,
                              matrix_mode="profile", measures=["speed", "angle"], category="st")
    @test pr["matrixMode"] == "profile"
    @test pr["xLabels"] == ["1", "2"] && pr["yLabels"] == ["speed", "angle"]
    cell(r, x, y) = first(c for c in r["cells"] if c["x"] == x && c["y"] == y)
    @test cell(pr, "1", "speed")["value"] == 2.0           # mean(1,3)
    @test cell(pr, "2", "speed")["value"] == 11.0          # mean(10,12); NaN excluded
    @test cell(pr, "2", "speed")["n"] == 2
    @test isempty(pr["series"])

    # z-score standardises each row across its levels (mean 0) — the comparable "signature"
    prz = Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false, zscore=true,
                               matrix_mode="profile", measures=["speed", "angle"], category="st")
    @test prz["zscore"] == true && prz["valueLabel"] == "z-score"
    zs = [c["value"] for c in prz["cells"] if c["y"] == "speed"]
    @test isapprox(sum(zs), 0.0; atol=1e-9) && all(isfinite, zs)

    # CROSSTAB: a "from_to" categorical → transition matrix; the hybrid uses '.', so the first '_'
    # splits prev|cur ("1.2_3.4" → from "1.2", to "3.4"). Row-normalise → P(to|from).
    dft = DataFrame("value_name" => fill("A", 5), "pop" => fill("/p", 5),
                    "tr" => ["1_1", "1_2", "1_2", "2_1", "x"])   # "x" has no sep → ignored
    ct = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                              normalize=:none, by_image=false,
                              matrix_mode="crosstab", category="tr")
    @test ct["matrixMode"] == "crosstab"
    @test ct["yLabels"] == ["1", "2"] && ct["xLabels"] == ["1", "2"]
    ctc(x, y) = first(c for c in ct["cells"] if c["x"] == x && c["y"] == y)
    @test ctc("1", "1")["value"] == 1.0 && ctc("2", "1")["value"] == 2.0   # counts
    # row-normalised: from state 1 → {1:1, 2:2} → P(2|1) = 2/3
    ctr = Cecelia._summary_agg(dft, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false,
                               matrix_mode="crosstab", category="tr", matrix_normalize=:row)
    @test ctr["valueLabel"] == "P(to|from)"
    ctrc(x, y) = first(c for c in ctr["cells"] if c["x"] == x && c["y"] == y)
    @test isapprox(ctrc("2", "1")["value"], 2/3; atol=1e-9)
    @test isapprox(ctrc("1", "1")["value"], 1/3; atol=1e-9)

    # error cases: unknown mode, missing category, profile with no present measure column
    @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
        nbins=0, normalize=:none, by_image=false, matrix_mode="bogus", category="st")
    @test_throws ErrorException Cecelia._summary_agg(df, "matrix"; measure=nothing, granularity=:cell,
        nbins=0, normalize=:none, by_image=false, matrix_mode="profile", measures=["speed"], category="nope")

    # all-NaN / empty level → JSON-null value (NOT NaN — JSON3 rejects NaN; the renderer skips null)
    dfn = DataFrame("value_name" => fill("A", 3), "pop" => fill("/p", 3),
                    "speed" => [1.0, 2.0, NaN], "st" => [1.0, 1.0, 2.0])
    prn = Cecelia._summary_agg(dfn, "matrix"; measure=nothing, granularity=:cell, nbins=0,
                               normalize=:none, by_image=false, matrix_mode="profile",
                               measures=["speed"], category="st")
    c2 = first(c for c in prn["cells"] if c["x"] == "2")
    @test c2["value"] === nothing && c2["n"] == 0     # state 2 has only a NaN → null cell

    # EMPTY FRAME (a population with no rows in this image) → an empty GRID, never an error. pop_df
    # returns a frame with no columns at all, which used to trip the category check and print
    # "matrix needs a `category` column present in the data" into the panel — the failure a per-image
    # board of cluster pops hits whenever one cluster is absent from one image. Every other chart type
    # answers this with an empty series; the response keys must match the populated ones so the
    # renderer's empty state fires instead of breaking.
    for (mode, extra) in (("crosstab", "normalize"), ("profile", "zscore"))
        e = Cecelia._summary_agg(DataFrame(), "matrix"; measure=nothing, granularity=:cell, nbins=0,
                                 normalize=:none, by_image=false, matrix_mode=mode,
                                 measures=["speed"], category="tr")
        @test e["chartType"] == "matrix" && e["matrixMode"] == mode && e["category"] == "tr"
        @test isempty(e["cells"]) && isempty(e["xLabels"]) && isempty(e["yLabels"])
        @test haskey(e, extra) && haskey(e, "valueLabel")   # same shape as the populated response
    end
    # …but an unknown mode is still an error, empty frame or not
    @test_throws ErrorException Cecelia._summary_agg(DataFrame(), "matrix"; measure=nothing,
        granularity=:cell, nbins=0, normalize=:none, by_image=false, matrix_mode="bogus", category="tr")
end

@testset "plot attribute grouping (compare by attr)" begin
    # group cross-image series by an image attribute: images sharing a value pool into one series
    # labelled by the value; an image with no value falls back to its uID. No fixture.
    df = DataFrame("value_name" => fill("A", 6), "pop" => fill("/p", 6),
                   "uID" => ["x1","x1","x2","x2","y1","y1"],
                   "m"   => [1.0, 2.0, 3.0, 4.0, 10.0, 12.0])
    amap = Dict("x1"=>"T", "x2"=>"T", "y1"=>"C")
    r = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                             normalize=:none, by_image=true, attr_map=amap)
    @test length(r["series"]) == 2
    @test Set(s["uID"] for s in r["series"]) == Set(["T", "C"])
    byu = Dict(s["uID"] => s for s in r["series"])
    @test byu["T"]["n"] == 4 && byu["C"]["n"] == 2          # x1+x2 pooled under "T"
    # image missing the attribute → falls back to its uID
    r2 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                              normalize=:none, by_image=true, attr_map=Dict("x1"=>"T", "x2"=>"T"))
    @test Set(s["uID"] for s in r2["series"]) == Set(["T", "y1"])
    # no attr_map → group by image as before (3 images)
    r3 = Cecelia._summary_agg(df, "bar"; measure="m", granularity=:cell, nbins=0,
                              normalize=:none, by_image=true)
    @test length(r3["series"]) == 3
end

# ── cross-image (set-level) aggregation: pool pop_df across images by uID ──
@testset "plot_summary_data cross-image" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "plot_summary_data cross-image (fixture missing)"
    else
        # two "images" from the same fixture (uX, uY) — exercises set-level pooling mechanics
        mk = function (uid)
            td = mktempdir(); mkpath(joinpath(td, "labelProps"))
            cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
            cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
            img = CciaImage(uid=uid, dir=td)
            img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
            img
        end
        imgs = [mk("uX"), mk("uY")]; uids = ["uX", "uY"]

        # pop_df set-level: uID column tags each image; rows = 2× a single image's tracks
        one = pop_df(imgs[1], "live", ["B/_tracked"]; granularity=:track)
        both = pop_df(imgs, uids, "live", ["B/_tracked"]; granularity=:track)
        @test "uID" in names(both)
        @test Set(unique(both.uID)) == Set(uids)
        @test nrow(both) == 2 * nrow(one)

        # boxplot per_image → one box per image (same data → equal stats), + scope field
        sp = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test sp["scope"] == "per_image" && sp["chartType"] == "boxplot"
        @test length(sp["series"]) == 2
        @test Set(s["uID"] for s in sp["series"]) == Set(uids)
        @test Set(keys(sp["series"][1])) ⊇ Set(["q1","median","q3","lower","upper","mean","n"])
        @test sp["series"][1]["median"] ≈ sp["series"][2]["median"]    # identical fixtures
        @test sp["series"][1]["q1"] <= sp["series"][1]["median"] <= sp["series"][1]["q3"]
        @test sp["series"][1]["n"] == nrow(one)

        # summarised → one pooled box across both images
        ss = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "boxplot";
                               measure="live.track.speed", granularity=:track, scope=:summarised)
        @test length(ss["series"]) == 1 && ss["series"][1]["n"] == 2 * nrow(one)

        # SAME data source, different chart type (bar of mean ± sd) — chart ⊥ data source
        br = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "bar";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test br["chartType"] == "bar" && length(br["series"]) == 2
        @test Set(keys(br["series"][1])) ⊇ Set(["value", "sd", "n"])
        @test br["series"][1]["value"] ≈ br["series"][2]["value"]

        # histogram per_image → 2 overlay series sharing bin edges
        hh = plot_summary_data(imgs, uids, "live", ["B/_tracked"], "histogram";
                               measure="live.track.speed", granularity=:track, scope=:per_image, nbins=10)
        @test length(hh["series"]) == 2 && length(hh["binEdges"]) == 11
        @test all(sum(s["counts"]) == nrow(one) for s in hh["series"])
    end
end

# ── multiple SEGMENTATIONS on one plot: (value_name, pop) targets ─────────
@testset "plot_summary_data multi-segmentation targets" begin
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    if !have_fixture(h5) || !have_fixture(trk)
        @test_skip "plot_summary_data multi-segmentation (fixture missing)"
    else
        # one image with the same data exposed under TWO segmentations (B, B2) — exercises the
        # targets path: a (value_name, pop) per series, vcat across segmentations.
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        cp(h5,  joinpath(td, "labelProps", "B2.h5ad"))
        cp(trk, joinpath(td, "labelProps", "B2__tracks.h5ad"))
        img = CciaImage(uid="uM", dir=td)
        img.label_props["B"] = "B.h5ad"; img.label_props["B2"] = "B2.h5ad"
        img.label_props["_active"] = "B"

        targets = [("B", "/_tracked"), ("B2", "/_tracked")]
        # single image, two segmentations → one series per (segmentation, pop), distinct vn ids
        bp = plot_summary_data(img, "live", targets, "boxplot";
                               measure="live.track.speed", granularity=:track)
        @test bp["chartType"] == "boxplot" && length(bp["series"]) == 2
        @test Set(s["value_name"] for s in bp["series"]) == Set(["B", "B2"])
        @test Set(s["pop"] for s in bp["series"]) == Set(["B/_tracked", "B2/_tracked"])
        @test bp["series"][1]["median"] ≈ bp["series"][2]["median"]   # identical underlying data

        # cross-image AND cross-segmentation: 2 images × 2 segmentations → 4 per_image series
        img2 = CciaImage(uid="uN", dir=td)
        img2.label_props["B"] = "B.h5ad"; img2.label_props["B2"] = "B2.h5ad"
        img2.label_props["_active"] = "B"
        xp = plot_summary_data([img, img2], ["uM", "uN"], "live", targets, "bar";
                               measure="live.track.speed", granularity=:track, scope=:per_image)
        @test xp["scope"] == "per_image" && length(xp["series"]) == 4
        @test Set((s["uID"], s["value_name"]) for s in xp["series"]) ==
              Set([("uM","B"), ("uM","B2"), ("uN","B"), ("uN","B2")])
    end
end

# ── summary-plot aggregation: pure helpers (no fixture) ───────────────────
@testset "plot_summary_data helpers" begin
    @test Cecelia._hist_edges(Float64[], 10) == Float64[]            # no data → no edges
    @test length(Cecelia._hist_edges([5.0], 4)) == 5                 # single value → 1-wide bin
    let edges = Cecelia._hist_edges([0.0, 10.0], 10)
        @test Cecelia._hist_counts([0.0, 5.0, 9.99, NaN, 10.0], edges) |> sum == 4  # NaN skipped
    end
    @test Cecelia._catkey(2.0) == "2" && Cecelia._catkey(1.5) == "1.5"
    @test Cecelia._sort_cats(["10", "2", "1"]) == ["1", "2", "10"]   # numeric, not lexical
    @test Cecelia._sort_cats(["b", "a"]) == ["a", "b"]               # lexical fallback
    # derived-pop registry is generic: /_tracked is a `live` derived pop, none for `flow`
    @test derived_pop_paths("live") == ["/_tracked"]
    @test isempty(derived_pop_paths("flow"))
end
