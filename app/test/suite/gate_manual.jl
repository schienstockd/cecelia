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

# ── Intensity columns are named by channel INDEX: match them across images by channel NAME ──
_set_names!(img, v) = (img.im_channel_names = Dict{String,Any}("default" => v, "_active" => "default"); img)

@testset "match_channel_columns — re-index by channel name" begin
    cols = ["mean_intensity_0", "nuc_mean_intensity_1", "area", "mean_intensity_0.mean"]
    same = Cecelia.match_channel_columns(cols, ["CD4", "CD8"], ["cd4", "CD8"])      # case only
    @test same.cols == cols && isempty(same.remapped) && isempty(same.problems)
    r = Cecelia.match_channel_columns(cols, ["CD4", "CD8"], ["CD8", "CD4"])
    @test r.cols == ["mean_intensity_1", "nuc_mean_intensity_0", "area", "mean_intensity_1.mean"]
    @test r.remapped[1] == (from = "mean_intensity_0", to = "mean_intensity_1", channel = "CD4")
    @test isempty(r.problems)
    @test Cecelia.match_channel_columns(["mean_intensity_0"], ["mem-TOM", "x"], ["x", "mem-Tom"]).cols ==
          ["mean_intensity_1"]
    # no right answer: the channel is absent, or its name is on two channels
    a = Cecelia.match_channel_columns(["mean_intensity_0", "mean_intensity_1"], ["CD4", "CD8"], ["GFP", "CD8"])
    @test a.problems == [(column = "mean_intensity_0", channel = "CD4", why = :absent)]
    @test Cecelia.describe_channel_problems(a.problems) == "CD4 (mean_intensity_0) not on this image"
    b = Cecelia.match_channel_columns(["mean_intensity_0"], ["CD4", "CD8"], ["CD8", "CD4", "cd4"])
    @test only(b.problems).why === :ambiguous
    # nothing to match → columns as-is (an image without names must not be blocked)
    @test Cecelia.match_channel_columns(cols, nothing, ["CD8", "CD4"]).cols == cols
    @test Cecelia.match_channel_columns(cols, ["CD4", "CD8"], String[]).cols == cols
    @test Cecelia.match_channel_columns(["mean_intensity_5"], ["CD4", "CD8"], ["CD8", "CD4"]).cols == ["mean_intensity_5"]
end

@testset "copy_gating! — a target with another channel order gates the same channel" begin
    _gate_fixture() do src, tgt
        m = load_pop_map(src; value_name = "B", pop_type = "flow")
        add_pop!(m, "cd4"; gate = RectangleGate("mean_intensity_0", "area", 0.0, 1e9, 0.0, 1e9))
        add_pop!(m, "cd8hi"; filter_measure = "mean_intensity_1", filter_fun = "gt", filter_values = 0.0)
        save_pop_map!(m, src)
        _set_names!(src, ["CD4", "CD8", "GFP", "TOM"])
        # unresolvable: CD4 is not on the target → skipped, nothing written
        _set_names!(tgt, ["CD8", "GFP", "TOM", "x"])
        r0 = copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow")
        @test isempty(r0.copied) && !_gated(tgt)
        @test r0.skipped["TgtImg"] == "channels don't match — CD4 (mean_intensity_0) not on this image"
        # same markers, swapped: the copied gate reads the target's CD4 column, the filter its CD8 one
        _set_names!(tgt, ["CD8", "cd4", "GFP", "TOM"])
        r = copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow")
        @test r.copied == ["TgtImg"]
        tm = load_pop_map(tgt; value_name = "B", pop_type = "flow")
        @test (pop_at(tm, "/cd4").gate.x_channel, pop_at(tm, "/cd4").gate.y_channel) == ("mean_intensity_1", "area")
        @test pop_at(tm, "/cd8hi").filter_measure == "mean_intensity_0"
        @test Set(r.remapped["TgtImg"]) == Set([(from = "mean_intensity_0", to = "mean_intensity_1", channel = "CD4"),
                                                (from = "mean_intensity_1", to = "mean_intensity_0", channel = "CD8")])
        # the source keeps its own columns
        @test pop_at(load_pop_map(src; value_name = "B", pop_type = "flow"), "/cd4").gate.x_channel == "mean_intensity_0"
        # same order → copied as-is, nothing remapped
        _set_names!(tgt, ["CD4", "CD8", "GFP", "TOM"])
        r2 = copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow")
        @test r2.copied == ["TgtImg"] && isempty(r2.remapped)
        @test pop_at(load_pop_map(tgt; value_name = "B", pop_type = "flow"), "/cd4").gate.x_channel == "mean_intensity_0"
    end
end

@testset "rename_pop_columns — every column a pop reads, on a copy" begin
    m = PopulationMap(; pop_type = "flow", value_name = "B")
    add_pop!(m, "g"; gate = PolygonGate("mean_intensity_0", "mean_intensity_1", [(0, 0), (1, 0), (1, 1)]))
    add_pop!(m, "f"; filter_conditions = [Dict("measure" => "mean_intensity_1", "fun" => "gt", "values" => 1.0),
                                          Dict("measure" => "area", "fun" => "lt", "values" => 9.0)])
    sw = Dict("mean_intensity_0" => "mean_intensity_1", "mean_intensity_1" => "mean_intensity_0")
    out = Cecelia.rename_pop_columns(m, sw)
    @test Cecelia._pop_columns(out.pops["/g"]) == ["mean_intensity_1", "mean_intensity_0"]
    @test Cecelia._pop_columns(out.pops["/f"]) == ["mean_intensity_0", "area"]
    @test Cecelia._pop_columns(m.pops["/g"]) == ["mean_intensity_0", "mean_intensity_1"]    # m untouched
end

# two images from the same fixture tables (for clustering's per-segment columns)
function _swapped_pair(f)
    h5  = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B.h5ad")
    trk = fixture_path("testpr", "1", "KDIeEm", "labelProps", "B__tracks.h5ad")
    (have_fixture(h5) && have_fixture(trk)) || return @test_skip "testpr fixture missing"
    mk = function (uid, chans)
        td = mktempdir(); mkpath(joinpath(td, "labelProps"))
        cp(h5,  joinpath(td, "labelProps", "B.h5ad")); cp(trk, joinpath(td, "labelProps", "B__tracks.h5ad"))
        img = CciaImage(uid = uid, dir = td)
        img.label_props["B"] = "B.h5ad"; img.label_props["_active"] = "B"
        _set_names!(img, chans)
    end
    f(mk("uX", ["CD4", "CD8", "GFP", "TOM"]), mk("uY", ["CD8", "CD4", "GFP", "TOM"]))
end

@testset "clustPops — each segment reads its own column for the first segment's channels" begin
    _swapped_pair() do x, y
        df = DataFrame(uID = ["uX", "uX", "uY"], value_name = fill("B", 3), label = [1, 2, 1])
        byuid = Dict("uX" => x, "uY" => y)
        segs = Cecelia._cluster_segments(df, byuid, ["mean_intensity_0", "area"]; on_log = _ -> nothing)
        @test !haskey(segs[1], "featureCols")                            # the reference reads as picked
        @test segs[2]["featureCols"] == ["mean_intensity_1", "area"]     # uY's CD4
        _set_names!(y, ["CD8", "GFP", "TOM", "x"])
        log = String[]
        @test Cecelia._cluster_segments(df, byuid, ["mean_intensity_0"]; on_log = l -> push!(log, l)) === nothing
        @test occursin("uY/B: CD4 (mean_intensity_0) not on this image", only(log))
    end
end

@testset "clustTracks — per-track aggregates pool by channel name" begin
    _swapped_pair() do x, y
        imgs = [x, y]; uids = ["uX", "uY"]; pops = ["B/_tracked"]
        mot = Set(col_names(label_props(img_track_props_path(x, "B")); data_type = :vars))
        df0 = pop_df_multi(imgs, uids, pops; granularity = :track, cell_measures = ["mean_intensity_0"], pop_cols = String[])
        df = Cecelia._match_track_channels(df0, imgs, uids, pops, ["mean_intensity_0"], mot; on_log = _ -> nothing)
        # same tables under both uids: uY's CD4 aggregate is uX's mean_intensity_1 aggregate, track by track
        xs = sort(df[df.uID .== "uX", :], :label); ys = sort(df[df.uID .== "uY", :], :label)
        @test nrow(xs) == nrow(ys) > 0
        @test isequal(ys[!, "mean_intensity_0.mean"], xs[!, "mean_intensity_1.mean"])
        @test isequal(ys[!, "mean_intensity_0.sd"],   xs[!, "mean_intensity_1.sd"])
        @test isequal(xs[!, "mean_intensity_0.mean"], sort(df0[df0.uID .== "uX", :], :label)[!, "mean_intensity_0.mean"])
        @test !isequal(xs[!, "mean_intensity_0.mean"], xs[!, "mean_intensity_1.mean"])   # the swap is visible
        _set_names!(y, ["CD8", "GFP", "TOM", "x"])
        @test Cecelia._match_track_channels(df0, imgs, uids, pops, ["mean_intensity_0"], mot; on_log = _ -> nothing) === nothing
    end
end
