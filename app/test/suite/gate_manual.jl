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
        m = load_pop_tree(src; value_name = "B", pop_type = "flow")
        has_pop(m, "/qc") || add_pop!(m, "qc"; filter_measure = "track_id", filter_fun = "gt", filter_values = 0)
        save_pop_tree!(m, src)
        f(src, init_object("testpr", "TgtImg"))
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
end

_gated(img) = has_pop(load_pop_tree(img; value_name = "B", pop_type = "flow", backfill_save = false), "/qc")

@testset "copy_gating! — replace = false keeps an image's own gates" begin
    _gate_fixture() do src, tgt
        r = copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow", replace = false)
        @test r.copied == ["TgtImg"] && _gated(tgt)
        # now gated: a second prefill must not overwrite it
        m = load_pop_tree(tgt; value_name = "B", pop_type = "flow")
        add_pop!(m, "mine"; filter_measure = "track_id", filter_fun = "gt", filter_values = 1); save_pop_tree!(m, tgt)
        r2 = copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow", replace = false)
        @test isempty(r2.copied) && r2.skipped["TgtImg"] == "already gated"
        @test has_pop(load_pop_tree(tgt; value_name = "B", pop_type = "flow"), "/mine")
        # replace = true (the API's copy) does overwrite
        @test copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow").copied == ["TgtImg"]
        @test !has_pop(load_pop_tree(tgt; value_name = "B", pop_type = "flow"), "/mine")
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
        m = load_pop_tree(src; value_name = "B", pop_type = "flow")
        add_pop!(m, "cd4"; gate = RectangleGate("mean_intensity_0", "area", 0.0, 1e9, 0.0, 1e9))
        add_pop!(m, "cd8hi"; filter_measure = "mean_intensity_1", filter_fun = "gt", filter_values = 0.0)
        save_pop_tree!(m, src)
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
        tm = load_pop_tree(tgt; value_name = "B", pop_type = "flow")
        @test (pop_at(tm, "/cd4").gate.x_channel, pop_at(tm, "/cd4").gate.y_channel) == ("mean_intensity_1", "area")
        @test pop_at(tm, "/cd8hi").filter_measure == "mean_intensity_0"
        @test Set(r.remapped["TgtImg"]) == Set([(from = "mean_intensity_0", to = "mean_intensity_1", channel = "CD4"),
                                                (from = "mean_intensity_1", to = "mean_intensity_0", channel = "CD8")])
        # the source keeps its own columns
        @test pop_at(load_pop_tree(src; value_name = "B", pop_type = "flow"), "/cd4").gate.x_channel == "mean_intensity_0"
        # same order → copied as-is, nothing remapped
        _set_names!(tgt, ["CD4", "CD8", "GFP", "TOM"])
        r2 = copy_gating!(src, [tgt]; value_name = "B", pop_type = "flow")
        @test r2.copied == ["TgtImg"] && isempty(r2.remapped)
        @test pop_at(load_pop_tree(tgt; value_name = "B", pop_type = "flow"), "/cd4").gate.x_channel == "mean_intensity_0"
    end
end

@testset "plan_gating_copy — what a copy would write, or why it skips, with nothing written" begin
    _gate_fixture() do src, tgt
        m = load_pop_tree(src; value_name = "B", pop_type = "flow")
        add_pop!(m, "cd4"; gate = RectangleGate("mean_intensity_0", "area", 0.0, 1e9, 0.0, 1e9))
        _set_names!(src, ["CD4", "CD8", "GFP", "TOM"])
        plan(t) = plan_gating_copy(m, t; value_name = "B", src_names = channel_names(src; value_name = "B"))
        _set_names!(tgt, ["CD8", "GFP", "TOM", "x"])
        @test plan(tgt).skip == "channels don't match — CD4 (mean_intensity_0) not on this image"
        _set_names!(tgt, ["CD8", "cd4", "GFP", "TOM"])                 # CD4 is the target's second channel
        p = plan(tgt)
        @test p.skip === nothing && p.remapped == [(from = "mean_intensity_0", to = "mean_intensity_1", channel = "CD4")]
        @test pop_at(p.tree, "/cd4").gate.x_channel == "mean_intensity_1"
        @test pop_at(m, "/cd4").gate.x_channel == "mean_intensity_0"   # the source map untouched
        @test !_gated(tgt)                                             # nothing written
        @test plan_gating_copy(m, tgt; value_name = "nope", src_names = ["CD4"]).skip == "no segmentation 'nope'"
    end
end

@testset "rename_pop_columns — every column a pop reads, on a copy" begin
    m = PopulationTree(; pop_type = "flow", value_name = "B")
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

@testset "channel columns — one resolver: a name ↔ this image's column" begin
    canon(cols, chans; kw...) = Cecelia.canonical_channel_columns(cols, chans; kw...)
    resolve(cols, vars, chans) = Cecelia.resolve_channel_columns(cols, vars, chans)
    vars  = ["mean_intensity_0", "mean_intensity_1", "nuc_mean_intensity_0", "nuc_mean_intensity_1", "area"]
    chans = ["CD4", "CD8"]
    # raw → name: plain, prefixed, per-track aggregate; everything else as-is
    @test canon(["mean_intensity_1", "nuc_mean_intensity_0", "mean_intensity_0.mean", "area", "live.cell.speed"], chans) ==
          ["CD8", "nuc_CD4", "CD4.mean", "area", "live.cell.speed"]
    @test canon(["CD4"], chans) == ["CD4"]                                   # already a name
    # nothing to name it by — no names, past the names, a duplicated name, a name a real column carries
    @test canon(["mean_intensity_0"], nothing) == ["mean_intensity_0"]
    @test canon(["mean_intensity_5"], chans) == ["mean_intensity_5"]
    @test canon(["mean_intensity_0"], ["CD4", "cd4"]) == ["mean_intensity_0"]
    @test canon(["mean_intensity_0"], ["Area", "CD8"]; shadow = ["area"]) == ["mean_intensity_0"]
    # name → this image's column: any case, prefixed, aggregates; a real column wins; unknowns pass
    r = resolve(["cd8", "nuc_CD4", "CD4.mean", "area", "live.cell.speed", "mean_intensity_1"], vars, chans)
    @test r.cols == ["mean_intensity_1", "nuc_mean_intensity_0", "mean_intensity_0.mean", "area",
                     "live.cell.speed", "mean_intensity_1"]
    @test isempty(r.problems)
    @test resolve(["CD4"], vars, ["CD8", "CD4"]).cols == ["mean_intensity_1"]   # another image's order
    # a dotted channel name is a NAME, not an aggregate of `CD45`
    @test resolve(["CD45.2", "CD45.2.mean"], vars, ["CD45.1", "CD45.2"]).cols ==
          ["mean_intensity_1", "mean_intensity_1.mean"]
    @test canon(["mean_intensity_1.mean"], ["CD45.1", "CD45.2"]) == ["CD45.2.mean"]
    # a name on two channels is ambiguous — never the last one
    a = resolve(["CD4"], vars, ["CD4", "cd4"])
    @test a.cols == ["CD4"] && only(a.problems).why === :ambiguous
    # round trip on one image
    for c in vars
        @test resolve(canon([c], chans), vars, chans).cols == [c]
    end
end

@testset "the reader resolves a channel name per image, any case; a name on two channels throws" begin
    _swapped_pair() do x, y
        rx = label_props(x; value_name = "B") |> lp -> select_cols(lp, ["mean_intensity_1"]) |> as_df
        ry = label_props(y; value_name = "B") |> lp -> select_cols(lp, ["cd4"]) |> as_df
        @test isequal(ry[!, "cd4"], rx[!, "mean_intensity_1"])       # uY's CD4 sits at index 1
        _set_names!(y, ["CD4", "cd4", "GFP", "TOM"])
        @test_throws ArgumentError label_props(y; value_name = "B") |> lp -> select_cols(lp, ["CD4"]) |> as_df
    end
end

@testset "clustPops — features by channel name, each segment reading its own column" begin
    _swapped_pair() do x, y
        df = DataFrame(uID = ["uX", "uX", "uY"], value_name = fill("B", 3), label = [1, 2, 1])
        byuid = Dict("uX" => x, "uY" => y)
        # a legacy raw pick is named against the first segment
        segs, names = Cecelia._cluster_segments(df, byuid, ["mean_intensity_0", "area"]; on_log = _ -> nothing)
        @test names == ["CD4", "area"]
        @test segs[1]["featureCols"] == ["mean_intensity_0", "area"]
        @test segs[2]["featureCols"] == ["mean_intensity_1", "area"]     # uY's CD4
        # a pick by name, any case, resolves the same
        @test last(Cecelia._cluster_segments(df, byuid, ["cd4"]; on_log = _ -> nothing)) == ["cd4"]
        @test first(Cecelia._cluster_segments(df, byuid, ["cd4"]; on_log = _ -> nothing))[2]["featureCols"] == ["mean_intensity_1"]
        _set_names!(y, ["CD8", "GFP", "TOM", "x"])
        log = String[]
        @test Cecelia._cluster_segments(df, byuid, ["mean_intensity_0"]; on_log = l -> push!(log, l)) === nothing
        @test occursin("uY/B: CD4 not on this image", only(log))
    end
end

@testset "clustTracks — per-track aggregates pool by channel name" begin
    _swapped_pair() do x, y
        imgs = [x, y]; uids = ["uX", "uY"]; pops = ["B/_tracked"]
        ch = Cecelia.pooled_channel_columns(imgs, pops, ["mean_intensity_0"])
        @test isempty(ch.problems) && only(ch.cols) == ["CD4"]
        df  = pop_df_multi(imgs, uids, pops; granularity = :track, cell_measures = only(ch.cols), pop_cols = String[])
        raw = pop_df_multi(imgs, uids, pops; granularity = :track, cell_measures = ["mean_intensity_1"],
                           pop_cols = String[], raw_channel_names = true)
        # same tables under both uids: uY's CD4 aggregate is uX's mean_intensity_1 aggregate, track by track
        xs = sort(df[df.uID .== "uX", :], :label); ys = sort(df[df.uID .== "uY", :], :label)
        xr = sort(raw[raw.uID .== "uX", :], :label)
        @test nrow(xs) == nrow(ys) > 0
        @test isequal(ys[!, "CD4.mean"], xr[!, "mean_intensity_1.mean"])
        @test isequal(ys[!, "CD4.sd"],   xr[!, "mean_intensity_1.sd"])
        @test !isequal(xs[!, "CD4.mean"], ys[!, "CD4.mean"])          # the swap is visible
        _set_names!(y, ["CD8", "GFP", "TOM", "x"])
        @test occursin("uY/B: CD4 not on this image",
                       only(Cecelia.pooled_channel_columns(imgs, pops, ["CD4"]).problems))
    end
end

@testset "behaviour.hmm_states — an intensity measure pools by channel name (user report, 0.2.12)" begin
    # Reported: "hmm_fit_states: DataFrame is missing column(s) [mean_intensity_1, mean_intensity_2]" —
    # the picker stored raw columns, the reader returned them under channel names.
    _swapped_pair() do x, y
        params = Dict{String,Any}("pops" => ["B/_tracked"], "colName" => "chan", "numStates" => 2,
                                  "modelMeasurements" => ["mean_intensity_0"])     # a legacy raw pick
        log = String[]
        out = Cecelia._run_task(Cecelia.HmmStates(), [x, y], params; on_log = l -> push!(log, l))
        @test out !== nothing
        @test !any(l -> occursin("[ERROR]", l), log)
        rec = Cecelia.hmm_fit_entry(img_label_props_path(x, "B"), "chan")
        @test rec["modelMeasurements"] == ["CD4"]                       # recorded by name
        # an image without the channel is refused by name, not pooled with a missing column
        _set_names!(y, ["CD8", "GFP", "TOM", "x"])
        log2 = String[]
        @test Cecelia._run_task(Cecelia.HmmStates(), [x, y], params; on_log = l -> push!(log2, l)) === nothing
        @test any(l -> occursin("uY/B: CD4 not on this image", l), log2)
    end
end

# A project image as `save!` writes it: channel names under `default`, label set(s) `vns`.
function _run_img!(proj, uid, names; vns = ["B"])
    img = _set_names!(CciaImage(uid = uid, dir = joinpath(proj, "1", uid)), names)
    for vn in vns; img.label_props[vn] = "$(vn).h5ad"; end
    mkpath(Cecelia.img_label_props_dir(img)); Cecelia.save!(img); img
end

@testset "run_channel_names — a run's images by uid, read from the sibling's ccid.json" begin
    proj = mktempdir()
    x = _run_img!(proj, "uX", ["CD4", "CD8"])
    _run_img!(proj, "uY", ["CD8", "CD4"]); _run_img!(proj, "uE", String[])
    of = Cecelia.run_channel_names(x, "B")
    @test of("uX") == ["CD4", "CD8"]                      # the image itself
    @test of("uY") == ["CD8", "CD4"]                      # a sibling, from its own ccid.json
    @test of("uE") === nothing                            # no names registered → nothing to name by
    @test of("uZ") === nothing                            # no such image
end

@testset "run_channel_names — a legacy run is named against the segment it was picked on, not partOf[1]" begin
    # A set run on [uY, uZ, uX] where uY had no cells in the clustered pops: the task's reference for the
    # raw `mean_intensity_N` columns was uZ (the first segment with cells), and uY's sidecar has no entry.
    proj = mktempdir()
    part = ["uY", "uZ", "uX"]
    _run_img!(proj, "uY", ["CD8", "CD4"])                       # partOf[1], no entry: not the reference
    z = _run_img!(proj, "uZ", ["CD4", "CD8"]; vns = ["A"])      # the reference, on another label set
    x = _run_img!(proj, "uX", ["GFP", "CD4"])                   # the reader
    for (im, vn) in ((z, "A"), (x, "B")), p in (Cecelia.img_label_props_path(im, vn), Cecelia.img_track_props_path(im, vn))
        Cecelia._write_clust_features!(p, "m", ["mean_intensity_0"], part; at = "2026-01-01T00:00:00")
    end
    # Look-alikes on uY, which comes first in partOf. Each one has the same suffix and partOf but is
    # a different run:
    #   - another segmentation's run with other features
    #   - a later re-run with the same features but a different `at`
    # Neither one is the reference.
    y = _run_img!(proj, "uY", ["CD8", "CD4"]; vns = ["B", "C"])
    Cecelia._write_clust_features!(Cecelia.img_label_props_path(y, "B"), "m", ["mean_intensity_1"], part;
                                   at = "2026-01-01T00:00:00")
    Cecelia._write_clust_features!(Cecelia.img_label_props_path(y, "C"), "m", ["mean_intensity_0"], part;
                                   at = "2026-02-01T00:00:00")
    of = Cecelia.run_channel_names(x, "B")
    for p in (Cecelia.img_label_props_path(x, "B"), Cecelia.img_track_props_path(x, "B"))
        @test Cecelia.run_reference_names(of, part, p, "m") == ["CD4", "CD8"]
        @test Cecelia._clustfeatures_features(p, "m"; chans_of = of, own_uid = "uX") == ["CD4"]
    end
    # a run whose images can't be read falls back to partOf[1], under the reader's value_name
    @test Cecelia.run_reference_names(of, ["uY"], Cecelia.img_label_props_path(x, "B"), "m") == ["CD8", "CD4"]
    # a run that already stores names needs no reference (nothing loaded)
    Cecelia._write_clust_features!(Cecelia.img_label_props_path(x, "B"), "n", ["CD4", "area"], ["uQ"])
    @test Cecelia._clustfeatures_features(Cecelia.img_label_props_path(x, "B"), "n"; chans_of = of) == ["CD4", "area"]
end

@testset "_clustfeatures_axes — a track run's axis features named like its features" begin
    props = joinpath(mktempdir(), "B__tracks.h5ad")
    @test Cecelia._clustfeatures_axes(props, "m") === nothing                       # no run recorded
    Cecelia._write_clust_features!(props, "plain", ["live.track.speed"], ["uX"])
    @test Cecelia._clustfeatures_axes(props, "plain") === nothing                   # a run without axes
    axes = Dict{String,Any}("axes" => [Dict{String,Any}("name" => "DC1", "top" => [
               Dict{String,Any}("feature" => "mean_intensity_1.mean", "rho" => 0.9),
               Dict{String,Any}("feature" => "live.track.speed", "rho" => -0.4)])],
           "root" => Dict{String,Any}("end" => "low", "cluster" => nothing))
    Cecelia._write_clust_features!(props, "m", ["mean_intensity_1.mean"], ["uX"]; behaviour_axes = axes)
    feats(a) = [t["feature"] for t in a["axes"][1]["top"]]
    @test feats(Cecelia._clustfeatures_axes(props, "m")) == ["mean_intensity_1.mean", "live.track.speed"]
    named = Cecelia._clustfeatures_axes(props, "m"; chans_of = u -> u == "uX" ? ["CD4", "CD8"] : nothing)
    @test feats(named) == ["CD8.mean", "live.track.speed"]
    @test named["root"]["end"] == "low"
end
