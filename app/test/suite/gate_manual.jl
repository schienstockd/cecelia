# ── The manual gate step (tasks/gate/manual_gate.jl) + copy_gating! ──────
# Real image: the `testpr` fixture's KDIeEm (label set `B`), copied under a second uid with its gating
# removed — the same two-image shape the API's gating-copy test uses.

function _gate_fixture(f)
    have_fixture(fixture_path("testpr")) || return @test_skip "testpr fixture missing"
    dir  = mktempdir()
    proj = joinpath(dir, "testpr")
    cp(fixture_path("testpr"), proj)
    tgt = joinpath(proj, "1", "TgtImg")
    cp(joinpath(proj, "1", "KDIeEm"), tgt)
    rm(joinpath(tgt, "gating"); recursive = true, force = true)
    ccid = joinpath(tgt, "ccid.json")
    write(ccid, replace(read(ccid, String), "\"uid\": \"KDIeEm\"" => "\"uid\": \"TgtImg\""))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir   # in memory only — never set_projects_dir
        src = init_object("testpr", "KDIeEm")
        m = load_pop_map(src; value_name = "B", pop_type = "flow")
        has_pop(m, "/qc") || add_pop!(m, "qc"; filter_measure = "track_id", filter_fun = "gt", filter_values = 0)
        save_pop_map!(m, src)
        f(src, init_object("testpr", "TgtImg"))
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end

_gated(img) = has_pop(load_pop_map(img; value_name = "B", pop_type = "flow", backfill_save = false), "/qc")

@testset "copy_gating! — replace = false keeps an image's own gates" begin
    _gate_fixture() do src, tgt
        r = copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow", replace = false)
        @test r.copied == ["TgtImg"] && _gated(tgt)
        # now gated: a second prefill must not overwrite it
        m = load_pop_map(tgt; value_name = "B", pop_type = "flow")
        add_pop!(m, "mine"; filter_measure = "track_id", filter_fun = "gt", filter_values = 1); save_pop_map!(m, tgt)
        r2 = copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow", replace = false)
        @test isempty(r2.copied) && r2.skipped["TgtImg"] == "already gated"
        @test has_pop(load_pop_map(tgt; value_name = "B", pop_type = "flow"), "/mine")
        # replace = true (the API's copy) does overwrite
        @test copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow").copied == ["TgtImg"]
        @test !has_pop(load_pop_map(tgt; value_name = "B", pop_type = "flow"), "/mine")
        # a source with no gating on that label set has nothing to copy
        @test_throws ArgumentError copy_gating!(tgt, [src]; value_name = "nope", pop_type = "flow")
    end
end

@testset "gate.manual_gate — prepare copies, check names what is missing" begin
    _gate_fixture() do src, tgt
        task = Cecelia._task_from_fun_name("gate.manual_gate")
        @test task_manual(task)["open"] == "gate"
        @test Cecelia._required_pops(" qc, /qc/cells ,") == ["/qc", "/qc/cells"]
        p = Dict{String,Any}("valueName" => "B", "popType" => "flow", "requiredPops" => "/qc")
        # not gated yet → the check refuses, naming the population
        @test occursin("/qc", something(manual_check(task, tgt, p), ""))
        # prepare with an exemplar copies its gates; the check then passes
        out = Cecelia._run_task(task, tgt, merge(p, Dict{String,Any}("copyFrom" => "KDIeEm")); on_log = _ -> nothing)
        @test out["copied"] === true
        @test isnothing(manual_check(task, tgt, p))
        # nothing required → always passes
        @test isnothing(manual_check(task, tgt, Dict{String,Any}("valueName" => "B", "popType" => "flow")))
        # no exemplar → prepare does nothing
        @test Cecelia._run_task(task, src, p; on_log = _ -> nothing)["copied"] === false
    end
end

@testset "channel_order_mismatches — same index, same channel" begin
    cols = ["mean_intensity_0", "nuc_mean_intensity_1", "area", "track_id"]
    @test isempty(Cecelia.channel_order_mismatches(cols, ["CD4", "CD8"], ["CD4", "CD8"]))
    @test isempty(Cecelia.channel_order_mismatches(cols, ["mem-TOM", "CD8"], ["mem-Tom", "cd8"]))  # case only
    ms = Cecelia.channel_order_mismatches(cols, ["CD4", "CD8"], ["CD8", "CD4"])
    @test [m.column for m in ms] == ["mean_intensity_0", "nuc_mean_intensity_1"]
    @test ms[1].ref == "CD4" && ms[1].other == "CD8"
    @test Cecelia.channel_order_mismatches(["mean_intensity_2"], ["a", "b", "c"], ["a", "b"])[1].other === nothing
    # a track gate / track clustering reads per-track aggregates of the cell column
    @test [m.column for m in Cecelia.channel_order_mismatches(["mean_intensity_0.mean", "speed.mean"],
                                                               ["CD4", "CD8"], ["CD8", "CD4"])] == ["mean_intensity_0.mean"]
    # nothing to compare → no verdict (an image without names must not be blocked)
    @test isempty(Cecelia.channel_order_mismatches(cols, nothing, ["CD8", "CD4"]))
    @test isempty(Cecelia.channel_order_mismatches(cols, ["CD4", "CD8"], String[]))
    @test occursin("mean_intensity_0 is CD8 here, CD4 in the source",
                   Cecelia.describe_channel_mismatches(ms))
end

@testset "copy_gating! — refuses a target whose channel order differs" begin
    _gate_fixture() do src, tgt
        names(img, v) = (img.im_channel_names = Dict{String,Any}("default" => v, "_active" => "default"))
        m = load_pop_map(src; value_name = "B", pop_type = "flow")
        add_pop!(m, "cd4"; gate = RectangleGate("mean_intensity_0", "mean_intensity_1", 0.0, 1e9, 0.0, 1e9))
        save_pop_map!(m, src)
        names(src, ["CD4", "CD8"])
        names(tgt, ["CD8", "CD4"])             # same markers, swapped: the gate would read CD8 as CD4
        r = copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow")
        @test isempty(r.copied)
        @test startswith(r.skipped["TgtImg"], "channel order differs")
        @test occursin("mean_intensity_0 is CD8 here, CD4 in the source", r.skipped["TgtImg"])
        @test !_gated(tgt)                     # nothing written
        names(tgt, ["cd4", "CD8"])             # same order (case aside) → copies as before
        @test copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow").copied == ["TgtImg"]
    end
end

@testset "channel_order_problems — clustPops/clustTracks refuse mixed channel order" begin
    mk(uid, v) = (img = CciaImage(uid = uid, dir = mktempdir());
                  img.im_channel_names = Dict{String,Any}("default" => v, "_active" => "default"); img)
    imgs = Dict("A" => mk("A", ["CD4", "CD8"]), "B" => mk("B", ["CD4", "CD8"]), "C" => mk("C", ["CD8", "CD4"]))
    segs = [Dict{String,Any}("uID" => u, "valueName" => "default") for u in ("A", "B", "C")]
    bad = Cecelia.channel_order_problems(imgs, segs, ["mean_intensity_0", "area"])
    @test length(bad) == 1 && startswith(bad[1], "C/default: mean_intensity_0 is CD8 here, CD4 in A")
    @test isempty(Cecelia.channel_order_problems(imgs, segs[1:2], ["mean_intensity_0"]))
end
