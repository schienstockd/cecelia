# ── Gating QC, statistics QC, and cohort QC at the end of a chain ─────────
# The two pipeline stages that had no QC (gating, statistics) and the cohort check chains never ran.
# Pure helpers first (synthetic inputs), then the set-level gating check on the testpr fixture, then a
# chain run that must leave the same cohort findings + lab-log entry the module page's button does.

_gst(counts; missing = @NamedTuple{column::String, pops::Vector{String}}[]) =
    (; counts = Dict{String,Tuple{Int,Int}}(counts), missing = missing)
_codes(r) = [f["code"] for f in r.findings]

@testset "gating_qc_findings — missing column, empty here only, % outlier" begin
    states = Dict(
        "A" => _gst(["/cd4" => (40, 100), "/cd4/act" => (10, 40)]),
        "B" => _gst(["/cd4" => (42, 100), "/cd4/act" => (12, 42)]),
        "C" => _gst(["/cd4" => (38, 100), "/cd4/act" => (0, 38)]),             # empty here only
        "D" => _gst(["/cd4" => (41, 100), "/cd4/act" => (11, 41)]),
        "E" => _gst(["/cd4" => (95, 100), "/cd4/act" => (12, 95)]),             # % outlier
        "F" => _gst(["/cd4" => (0, 100), "/cd4/act" => (0, 0)];
                    missing = [(column = "mean_intensity_3", pops = ["/cd4", "/cd4/act"])]))
    r = gating_qc_findings(states)
    @test isempty(r["A"].findings) && isempty(r["B"].findings) && isempty(r["D"].findings)
    @test _codes(r["C"]) == ["gating.empty_pop"]
    @test r["C"].findings[1]["detail"]["pop"] == "/cd4/act"
    @test _codes(r["E"]) == ["gating.pct_outlier"]
    @test r["E"].findings[1]["detail"]["pop"] == "/cd4"
    # a missing column names the pops it empties, and they are not repeated as "empty here"
    @test _codes(r["F"]) == ["gating.missing_column"]
    @test r["F"].findings[1]["detail"]["pops"] == ["/cd4", "/cd4/act"]
    @test r["F"].metrics == Dict("nPops" => 2, "nEmptyPops" => 2, "nMissingColumns" => 1)
    @test r["C"].flags == Dict("emptyPop" => ["/cd4/act"])
    # every finding renders from the catalog (short = problem, long = action)
    for (_, x) in r, f in x.findings
        @test !isempty(f["short"]) && !isempty(f["long"]) && f["level"] == "warn"
    end
    # empty everywhere is not "empty here only"; a lone image has nothing to compare against
    @test isempty(gating_qc_findings(Dict("A" => _gst(["/x" => (0, 10)]), "B" => _gst(["/x" => (0, 12)])))["A"].findings)
    @test isempty(gating_qc_findings(Dict("A" => _gst(["/x" => (0, 10)])))["A"].findings)
    # fewer than the cohort minimum → no % outlier (the cohort rule, not a gating constant)
    two = gating_qc_findings(Dict("A" => _gst(["/x" => (1, 100)]), "B" => _gst(["/x" => (90, 100)])))
    @test isempty(two["A"].findings) && isempty(two["B"].findings)
end

@testset "stats_qc_findings — cells as replicates and the replicate floor" begin
    codes(fs) = [f["code"] for f in fs]
    # 5 vs 5 images, one value per image: the floor 2/C(10,5) is under 0.05 → nothing
    @test isempty(Cecelia.stats_qc_findings([5, 5], [5, 5]))
    # 3 vs 3 images: no rank test can go below 0.1
    f = Cecelia.stats_qc_findings([3, 3], [3, 3])
    @test codes(f) == ["stats.few_replicates"] && f[1]["subs"]["n"] == 3 && f[1]["subs"]["p"] == 0.1
    # 400 cells from 2 images per group: counted as replicates — the floor waits until values are images
    @test codes(Cecelia.stats_qc_findings([400, 380], [2, 2])) == ["stats.cells_as_replicates"]
    # an approximate (tied, >10 values) rank test has no exact floor
    @test isempty(Cecelia.stats_qc_findings([3, 3], [3, 3]; exact = false))
    # cells from enough images: still pooled cells
    @test codes(Cecelia.stats_qc_findings([400, 380, 390], [6, 6, 7])) == ["stats.cells_as_replicates"]
    # >2 groups: the best floor over the smallest group's pairs, times the Bonferroni factor the brackets carry
    @test codes(Cecelia.stats_qc_findings([8, 2, 2], [8, 2, 2])) == ["stats.few_replicates_pairs"]
    @test isempty(Cecelia.stats_qc_findings([8, 8, 8], [8, 8, 8]))
    # a t-test has no exact floor: only the replicate check applies
    @test isempty(Cecelia.stats_qc_findings([3, 3], [3, 3]; test = :ttest))
end

@testset "_stats_from_series — findings ride the comparisons" begin
    mk(uids, vals) = DataFrame(uID = uids, v = vals)
    # one series per population, cells pooled from 2 images each
    g = [(uid = "", vn = "B", sid = "B/a", grp = "", sub = mk(repeat(["i1", "i2"], 10), collect(1.0:20))),
         (uid = "", vn = "B", sid = "B/b", grp = "", sub = mk(repeat(["i3", "i4"], 10), collect(11.0:30)))]
    r = Cecelia._stats_from_series(g, "v", :auto)
    @test [f["code"] for f in r["findings"]] == ["stats.cells_as_replicates"]
    # a single-image frame has no uID column: every group is one image
    g1 = [(uid = "", vn = "B", sid = "B/a", grp = "", sub = DataFrame(v = collect(1.0:10))),
          (uid = "", vn = "B", sid = "B/b", grp = "", sub = DataFrame(v = collect(5.0:14)))]
    @test "stats.cells_as_replicates" in [f["code"] for f in Cecelia._stats_from_series(g1, "v", :auto)["findings"]]
    # one value per image, 6 images per group: clean
    g2 = [(uid = "", vn = "B", sid = "B/a", grp = "", sub = mk(["a$i" for i in 1:6], collect(1.0:6))),
          (uid = "", vn = "B", sid = "B/b", grp = "", sub = mk(["b$i" for i in 1:6], collect(4.0:9)))]
    @test isempty(Cecelia._stats_from_series(g2, "v", :auto)["findings"])
end

# Three copies of testpr's KDIeEm (label set `B`) in one in-memory set; each gets its own flow gates.
function _gating_qc_fixture(f)
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    dir  = mktempdir()
    proj = joinpath(dir, "testpr")
    cp(fixture_path("testpr"), proj)
    for u in ("GqcImB", "GqcImC")
        tgt = joinpath(proj, "1", u)
        cp(joinpath(proj, "1", "KDIeEm"), tgt)
        ccid = joinpath(tgt, "ccid.json")
        write(ccid, replace(read(ccid, String), "\"uid\": \"KDIeEm\"" => "\"uid\": \"$u\""))
    end
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir   # in memory only — never set_projects_dir
        imgs = [init_object("testpr", u) for u in ("KDIeEm", "GqcImB", "GqcImC")]
        for (img, thr) in zip(imgs, (0, 0, 10^9))          # the third image's /sub selects nothing
            m = load_pop_map(img; value_name = "B", pop_type = "flow")
            add_pop!(m, "sub"; filter_measure = "label", filter_fun = "gt", filter_values = thr)
            add_pop!(m, "gone"; filter_measure = "no_such_column", filter_fun = "gt", filter_values = 0)
            save_pop_map!(m, img)
        end
        set = Cecelia.CciaSet("GqcSet", "gating-qc", [i.uid for i in imgs], Dict{String,Any}(),
                              mktempdir(), imgs)
        f(load_project("testpr"), set, imgs)
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end

@testset "gating_qc! — banks per-image findings, the check logs what flagged" begin
    _gating_qc_fixture() do proj, set, imgs
        @test gating_value_names(imgs[1], "flow") == ["B"]
        @test gating_value_names(imgs[1], "trackclust") == ["B"]       # the fixture's `B__trackclust.json` — not flow
        @test Cecelia.set_has_gating(set, "flow") && !Cecelia.set_has_gating(set, "track")
        # the GET form computes and writes nothing
        ro = cohort_reply(set, "gating.flow")
        @test !isfile(qc_path(imgs[3], "gating.flow", "B"))
        @test haskey(ro["byValueName"]["B"]["metrics"]["emptyPop"]["outliers"], "GqcImC")
        # the check: per-image docs + a lab-log entry
        reply = cohort_check!(proj, set, "gating.flow")
        doc = reply["byValueName"]["B"]
        @test doc["nIncluded"] == 3 && cohort_has_outliers(doc)
        c = read_qc(imgs[3], "gating.flow", "B")
        codes = sort([String(f[:code]) for f in c[:findings]])
        @test codes == ["gating.empty_pop", "gating.missing_column"]
        a = read_qc(imgs[1], "gating.flow", "B")
        @test [String(f[:code]) for f in a[:findings]] == ["gating.missing_column"]
        @test a[:metrics][:nPops] == 2 && a[:metrics][:nMissingColumns] == 1
        log = read_lab_log(proj)
        @test occursin("Cohort check", log) && occursin("gating.flow (B)", log) && occursin("GqcImC", log)
        # the gate fixed on the third image → a re-check clears its empty-pop finding
        m = load_pop_map(imgs[3]; value_name = "B", pop_type = "flow")
        set_filter = m.pops["/sub"]; set_filter.filter_values = 0; save_pop_map!(m, imgs[3])
        gating_qc!(set, "flow")
        @test [String(f[:code]) for f in read_qc(imgs[3], "gating.flow", "B")[:findings]] == ["gating.missing_column"]
        # the check button can reach it
        @test cohort_has_answer("gating.flow") && cohort_has_answer("gating.track") && !cohort_has_answer("gating.clust")
        # a map that fails to evaluate keeps its last doc — "couldn't check" is not "clean"
        write(gating_path(imgs[1]._dir, "B"), "{not json")
        gating_qc!(set, "flow")
        @test [String(f[:code]) for f in read_qc(imgs[1], "gating.flow", "B")[:findings]] == ["gating.missing_column"]
        # maps gone from every image → the old warnings clear
        foreach(img -> rm(gating_path(img._dir, "B")), imgs)
        @test isempty(gating_value_names(imgs[1], "flow"))
        gating_qc!(set, "flow")
        @test all(isempty(read_qc(img, "gating.flow", "B")[:findings]) for img in imgs)
    end
end

@testset "chain run ends with the cohort check the button runs" begin
    fun = "testTasks.image_task"
    proj = create_project!(name = "chain-cohort-$(rand(1000:9999))")
    s    = add_set!(proj; name = "s")
    imgs = [add_image!(s; name = "img-$i") for i in 1:4]
    # banked metrics as a task's QC would leave them; the last image is far off the others
    for (img, v) in zip(imgs, (100, 104, 98, 9))
        write_qc(img, fun, "default", Any[]; metrics = Dict{String,Any}("nThings" => v))
    end
    register_cohort_metrics!(fun, ["nThings"])
    try
        save_chain_template!(proj, ChainTemplate("cohort-chain",
            [ChainNode(id = "n1", fn = fun, scope = "image", params = Dict{String,Any}())], ChainEdge[]))
        logs = String[]
        run = run_chain(proj, [i.uid for i in imgs]; chain = "cohort-chain", on_log = l -> push!(logs, l))
        @test all(run.image_states[i.uid]["n1"].status == NODE_DONE for i in imgs)
        # the same artefacts POST /api/qc/cohort/check leaves: per-image finding, set sidecar, lab log
        flagged = read_qc(imgs[4], "cohort." * fun, "default")
        @test flagged !== nothing && String(flagged[:findings][1][:code]) == "cohort.nThings"
        @test read_qc(imgs[1], "cohort." * fun, "default") === nothing
        @test isfile(cohort_qc_path(s, fun, "default"))
        @test occursin("Cohort check", read_lab_log(proj)) && occursin(imgs[4].uid, read_lab_log(proj))
        @test any(l -> occursin("Cohort QC $fun", l) && occursin("1 image(s) flagged", l), logs)
    finally
        delete!(COHORT_METRICS, fun)
        rm(proj.root; recursive = true)
    end
end
