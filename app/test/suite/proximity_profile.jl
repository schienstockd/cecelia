# ── spatialAnalysis.proximityProfile — band arithmetic, QC, the task on a real timecourse, readers ──
# Expected values below come from the definitions (band edges, share of counted cells, log2 ratio with
# a 0.5 pseudocount), computed by hand or by a brute-force loop — never from the code under test.

@testset "proximityProfile dispatch + params" begin
    @test Cecelia._task_from_fun_name("spatialAnalysis.proximityProfile") isa ProximityProfile
    p = Cecelia.parse_proximity_profile_params(Dict{String,Any}())
    @test p.proximitySuffix == "default" && p.bandWidth == 15.0 && p.nBands == 3 &&
          isempty(p.pops) && isempty(p.structurePops)
    good = Dict{String,Any}("structurePops" => ["B/gc"], "pops" => ["B/T", "B/B"])
    @test begin validate_params(ProximityProfile(), good); true end
    @test_throws ParamValidationError validate_params(ProximityProfile(), merge(good, Dict{String,Any}("bandWidth" => 0)))
    @test_throws ParamValidationError validate_params(ProximityProfile(), Dict{String,Any}("pops" => ["B/T"]))
end

@testset "proximity bands + band index" begin
    b = Cecelia.proximity_bands(15, 3)
    @test [x.label for x in b] == ["0–15 µm", "15–30 µm", "30–45 µm", ">45 µm"]
    @test b[end].lo == 45.0 && isinf(b[end].hi)
    @test [x.lo for x in b] == [0.0, 15.0, 30.0, 45.0] && [x.hi for x in b[1:3]] == [15.0, 30.0, 45.0]
    @test [x.label for x in Cecelia.proximity_bands(2.5, 1)] == ["0–2.5 µm", ">2.5 µm"]
    idx(d) = Cecelia.proximity_band_index(d, 15.0, 3)
    @test idx(0.0) == 1 && idx(15.0) == 1          # the edge belongs to the nearer band
    @test idx(15.01) == 2 && idx(30.0) == 2 && idx(45.0) == 3
    @test idx(45.01) == 4 && idx(1e6) == 4          # everything beyond is one band
    @test idx(NaN) == 0                             # no structure in the frame → left out
end

@testset "proximity records — shares, enrichment, overlapping populations" begin
    # reference = every cell below (c1–c5), each in its one band
    bands = Cecelia.proximity_bands(10, 2)          # 0–10, 10–20, >20
    c(i) = ("B", i)
    # A: c1, c2 near; c3 middle.  B: c4 near, c5 beyond, and c1 too (nested picks share a cell)
    members = Dict("A" => Dict(c(1) => 1, c(2) => 1, c(3) => 2),
                   "B" => Dict(c(4) => 1, c(5) => 3, c(1) => 1),
                   "skipped" => Dict(c(9) => 0))
    reference = Dict(c(1) => 1, c(2) => 1, c(3) => 2, c(4) => 1, c(5) => 3)
    recs = Cecelia.proximity_records(["A", "B"], members, reference, bands)
    @test length(recs) == 6 && [r["pop"] for r in recs] == ["A", "A", "A", "B", "B", "B"]
    r(pop, band) = only(x for x in recs if x["pop"] == pop && x["band"] == band)
    # distinct counted cells per band: near {1,2,4} = 3, middle {3} = 1, beyond {5} = 1; total 5
    @test [r("A", l)["nBand"] for l in ("0–10 µm", "10–20 µm", ">20 µm")] == [3, 1, 1]
    @test r("A", "0–10 µm")["n"] == 2 && r("A", "0–10 µm")["fraction"] ≈ 2 / 3
    @test r("A", "0–10 µm")["imageFraction"] ≈ 3 / 5
    @test r("A", "0–10 µm")["log2Enrichment"] ≈ log2((2.5 / 3.5) / (3.5 / 5.5))
    @test r("A", ">20 µm")["n"] == 0 && r("A", ">20 µm")["log2Enrichment"] ≈ log2((0.5 / 1.5) / (3.5 / 5.5))
    @test r("B", "0–10 µm")["n"] == 2 && r("B", "0–10 µm")["imageFraction"] ≈ 3 / 5
    @test r("A", ">20 µm")["hi"] === nothing && r("A", "10–20 µm")["hi"] == 20.0
    # a band with no reference cells has no enrichment, even when other bands do
    gap = Cecelia.proximity_records(["A"], Dict("A" => Dict(c(1) => 1)), Dict(c(1) => 1, c(2) => 3), bands)
    @test only(x for x in gap if x["band"] == "10–20 µm")["log2Enrichment"] === nothing
    # ONE reference cell is still a defined profile: log2((1.5/1.5) / (1.5/1.5)) = 0
    one = Cecelia.proximity_records(["A"], Dict("A" => Dict(c(1) => 1)), Dict(c(1) => 1), bands)
    @test only(x for x in one if x["band"] == "0–10 µm")["log2Enrichment"] == 0.0
    # nothing counted → shares and enrichment are absent, never 0
    empty = Cecelia.proximity_records(["A"], Dict("A" => Dict{Tuple{String,Int},Int}()),
                                      Dict{Tuple{String,Int},Int}(), bands)
    @test all(x -> x["fraction"] === nothing && x["log2Enrichment"] === nothing &&
                   x["imageFraction"] === nothing, empty)
end

@testset "proximity QC findings" begin
    codes(f) = Set(String(x["code"]) for x in f)
    rec(pop, band, n, nb) = Dict{String,Any}("pop" => pop, "band" => band, "n" => n, "nBand" => nb)
    ok = [rec("T", "0–15 µm", 30, 100), rec("T", ">15 µm", 40, 300)]
    @test isempty(Cecelia.proximity_findings(50, ok))
    @test codes(Cecelia.proximity_findings(0, ok)) == Set(["proximity.no_structure"])
    @test "proximity.empty_pop" in codes(Cecelia.proximity_findings(50, vcat(ok, [rec("gc", "0–15 µm", 0, 100)])))
    @test "proximity.thin_band" in codes(Cecelia.proximity_findings(50, [rec("T", "0–15 µm", 3, 5)]))
    # the floor itself is enough (fewer than PROXIMITY_MIN_BAND_CELLS = 20 is thin)
    @test isempty(Cecelia.proximity_findings(50, [rec("T", "0–15 µm", 3, 20)]))
    @test "proximity.thin_band" in codes(Cecelia.proximity_findings(50, [rec("T", "0–15 µm", 3, 19)]))
end

# End to end on a real tracked timecourse: structure = the brightest third, counted = the other two
# thirds. Distances are re-derived by a brute-force same-frame loop, so the band counts are checked
# against the definition rather than against the KDTree search the task shares with cellContacts.
@testset "proximityProfile on a real timecourse (KDIeEm)" begin
    h5 = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    if !have_fixture(h5)
        @test_skip "proximityProfile end to end (fixture missing)"
    else
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5, joinpath(td, "labelProps", "B.h5ad"))
        img = CciaImage(uid = "KDIeEm", dir = td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
        full = label_props(img; value_name = "B") |> select_cols(["mean_intensity_0"]) |> as_df
        v = sort(full.mean_intensity_0); t1 = v[cld(nrow(full), 3)]; t2 = v[cld(2 * nrow(full), 3)]
        m = PopulationMap(pop_type = "flow", value_name = "B")
        band(lo, hi) = RectangleGate("mean_intensity_0", "mean_intensity_1", lo, hi, -1e12, 1e12)
        add_pop!(m, "lo"; gate = band(-1e12, t1)); add_pop!(m, "mid"; gate = band(t1, t2))
        add_pop!(m, "hi"; gate = band(t2, 1e12))
        add_pop!(m, "one"; gate = band(-1e12, v[1]))   # the dimmest cell(s) only
        save_pop_map!(m, img)

        params = Dict{String,Any}("structurePops" => ["B/hi"], "pops" => ["B/lo", "B/mid", "B/hi", "B/one"],
                                  "bandWidth" => 10.0, "nBands" => 2, "proximitySuffix" => "gc")
        res = Cecelia._run_task(ProximityProfile(), img, params; on_log = _ -> nothing)
        @test res !== nothing && res["nStructureCells"] > 0
        @test img_proximity_suffixes(img) == ["gc"]
        run = only(Cecelia._collect_proximity_stats(img))
        @test run.bands == ["0–10 µm", "10–20 µm", ">20 µm"] && run.structurePops == ["B/hi"]

        # brute force: µm centroids + frames from the same accessor, nearest structure cell per frame
        cloud(pop) = (d = pop_df_multi([img], [img.uid], ["B/$(pop)"]; granularity = :cell, centroids = :physical);
                      Cecelia._centroid_matrix(img, "B", d))
        sC, sL, sT = cloud("hi")
        expect = Dict{String,Vector{Int}}()
        for pop in ("lo", "mid", "one", "root")
            cC, cL, cT = cloud(pop)
            counts = zeros(Int, 3)
            for i in eachindex(cL)
                cL[i] in sL && continue                  # a structure cell is never counted around it
                same = [j for j in eachindex(sL) if sT[j] == cT[i]]
                isempty(same) && continue
                d = minimum(sqrt(sum((cC[i, :] .- sC[j, :]) .^ 2)) for j in same)
                counts[d <= 10 ? 1 : d <= 20 ? 2 : 3] += 1
            end
            expect[pop] = counts
        end
        got(pop) = [Int(r[:n]) for r in run.records if r[:pop] == "B/$(pop)"]
        @test got("lo") == expect["lo"] && got("mid") == expect["mid"]
        # a population of exactly the dimmest cell(s) is counted, not dropped as empty
        @test count(==(v[1]), full.mean_intensity_0) == 1     # exactly one cell: the edge case
        @test sum(got("one")) == sum(expect["one"]) == 1
        # the default reference is every cell of the segmentation (B/root), structure cells left out
        @test run.referencePops == ["B/root"]
        @test [Int(r[:nBand]) for r in run.records if r[:pop] == "B/lo"] == expect["root"]
        @test sum(expect["lo"]) > 0 && expect["mid"][1] > 0
        # a cell on the gate edge is in both "mid" and the structure: it is left out, not counted at 0 µm
        @test sum(got("mid")) < length(cloud("mid")[2])
        # the structure is never counted around itself → a QC warning, not a silent zero row
        @test all(==(0), got("hi"))
        qc = Cecelia.read_qc(img, "spatialAnalysis.proximityProfile", "gc")
        @test qc !== nothing && any(f -> f["code"] == "proximity.empty_pop", qc["findings"])
        @test qc["metrics"]["nStructureCells"] == res["nStructureCells"]

        # listed + deletable through the analysis-run registry
        kind = only(k for k in Cecelia.RUN_KINDS if k.kind == "proximity")
        @test [r.key for r in kind.list(img)] == ["gc"]
        kind.delete(img, only(kind.list(img)))
        @test isempty(img_proximity_suffixes(img))
        rm(td; recursive = true)
    end
end

@testset "proximity plots — per-image metric + mean matrix" begin
    tds = [mktempdir() for _ in 1:3]
    try
        uids = ["i1", "i2", "i3"]
        imgs = [CciaImage(; uid = u, dir = d) for (u, d) in zip(uids, tds)]
        enr = Dict("i1" => 1.0, "i2" => 0.5, "i3" => -0.5)
        for (img, u) in zip(imgs, uids)
            mkpath(img_proximity_dir(img))
            recs = [Dict("pop" => "T", "band" => "0–15 µm", "n" => 10, "log2Enrichment" => enr[u], "fraction" => 0.4),
                    Dict("pop" => "T", "band" => ">15 µm", "n" => 30, "log2Enrichment" => -0.2, "fraction" => 0.2),
                    Dict("pop" => "B", "band" => "0–15 µm", "n" => 5, "log2Enrichment" => nothing, "fraction" => nothing),
                    Dict("pop" => "B", "band" => ">15 µm", "n" => 50, "log2Enrichment" => 0.1, "fraction" => 0.5)]
            write(img_proximity_path(img, "gc"), JSON3.write(Dict("bands" => ["0–15 µm", ">15 µm"], "records" => recs)))
        end
        df, sfx, _ = image_metric_frame(imgs, uids, "proximityProfile")
        @test sfx == "gc" && nrow(df) == 12
        @test Set(df.pop) == Set(["T · 0–15 µm", "T · >15 µm", "B · 0–15 µm", "B · >15 µm"])
        @test isnan(only(df[(df.uID .== "i1") .& (df.pop .== "B · 0–15 µm"), :log2Enrichment]))
        r = image_metric_summary(imgs, uids, "proximityProfile", "boxplot"; measure = "log2Enrichment")
        @test r["pops"] == ["T · 0–15 µm", "T · >15 µm", "B · 0–15 µm", "B · >15 µm"]   # the run's order

        mx = Cecelia._proximity_matrix(imgs; suffix = "gc")
        @test mx["matrixMode"] == "proximity" && mx["nImages"] == 3
        @test mx["xLabels"] == ["0–15 µm", ">15 µm"] && mx["yLabels"] == ["T", "B"]
        cell(x, y) = only(c for c in mx["cells"] if c["x"] == x && c["y"] == y)
        @test cell("0–15 µm", "T")["value"] ≈ (1.0 + 0.5 - 0.5) / 3
        @test cell("0–15 µm", "T")["count"] == 30 && cell("0–15 µm", "T")["nImages"] == 3
        @test !any(c -> c["x"] == "0–15 µm" && c["y"] == "B", mx["cells"])   # no value on any image → no cell
        # a same-named run around a DIFFERENT structure is skipped, never averaged in
        odd = CciaImage(; uid = "i5", dir = mktempdir()); mkpath(img_proximity_dir(odd))
        write(img_proximity_path(odd, "gc"), JSON3.write(Dict("structurePops" => ["B/follicle"],
              "bands" => ["0–15 µm", ">15 µm"],
              "records" => [Dict("pop" => "T", "band" => "0–15 µm", "n" => 10, "log2Enrichment" => 9.0)])))
        # the odd run comes FIRST: the most common run still wins, whatever the selection order
        mo = Cecelia._proximity_matrix(vcat([odd], imgs); suffix = "gc")
        @test mo["nImages"] == 3 && mo["nSkipped"] == 1
        @test only(c for c in mo["cells"] if c["x"] == "0–15 µm" && c["y"] == "T")["value"] ≈ (1.0 + 0.5 - 0.5) / 3
        # the per-image plot leaves the same image out, and says so
        r2 = image_metric_summary(vcat([odd], imgs), vcat(["i5"], uids), "proximityProfile", "boxplot";
                                  measure = "log2Enrichment")
        @test r2["nSkipped"] == 1 && r2["nImages"] == 3
        @test image_metric_summary(imgs, uids, "spatialPattern", "boxplot"; measure = "nnRatio")["nSkipped"] == 0
        # an image without the run is left out of the mean, not read as zero
        @test Cecelia._proximity_matrix(vcat(imgs, [CciaImage(; uid = "i4", dir = mktempdir())]); suffix = "gc")["nImages"] == 3
    finally
        foreach(d -> rm(d; recursive = true, force = true), tds)
    end
end
