# ── spatialAnalysis.regionEdge — params, QC, column naming, the task end to end, run deletion ──
# The geometry (shape, distances, frame rule, holes) is tested against synthetic layouts in
# python/cecelia/tests/test_region_edge_utils.py; here: the wiring on a real calibrated timecourse.

@testset "regionEdge dispatch + params" begin
    @test Cecelia._task_from_fun_name("spatialAnalysis.regionEdge") isa RegionEdge
    @test Cecelia.task_previewable(RegionEdge())
    p = Cecelia.parse_region_edge_params(Dict{String,Any}())
    @test p.edgeName == "tissue" && p.closeRadius == 75.0 && p.fillHoles == "auto" && isempty(p.regionPops)
    # auto: a tear in the tissue is filled; a region nested inside a population's ring is not its own
    @test Cecelia.region_edge_fill_holes("auto", true) && !Cecelia.region_edge_fill_holes("auto", false)
    @test Cecelia.region_edge_fill_holes("on", false) && !Cecelia.region_edge_fill_holes("off", true)
    @test Cecelia.parse_region_edge_params(Dict{String,Any}("fillHoles" => false)).fillHoles == "off"
    good = Dict{String,Any}("regionPops" => ["B/"])
    @test begin validate_params(RegionEdge(), good); true end
    @test_throws ParamValidationError validate_params(RegionEdge(), merge(good, Dict{String,Any}("closeRadius" => 0)))
    @test_throws ParamValidationError validate_params(RegionEdge(), Dict{String,Any}())
    @test Cecelia.region_edge_col("flow", "edge_dist", "tissue") == "flow.cell.edge_dist#tissue"
    m = match(Cecelia.REGION_EDGE_COL_RE, "live.cell.inside#gc")
    @test m !== nothing && m.captures == ["live", "inside", "gc"]
end

@testset "regionEdge QC findings" begin
    codes(qc) = [f["code"] for f in Cecelia.region_edge_qc_findings(qc)]
    @test isempty(codes(Dict{String,Any}("fracNoBoundary" => 0.2, "fracInSmallPieces" => 0.05)))
    # a solid region with many stray specks is fine: only the share of its cells in specks counts
    @test isempty(codes(Dict{String,Any}("maxPieces" => 140, "fracInSmallPieces" => 0.02)))
    @test codes(Dict{String,Any}("fracNoBoundary" => 1.0)) == ["regionEdge.no_boundary"]
    @test codes(Dict{String,Any}("fracInSmallPieces" => 0.8)) == ["regionEdge.scattered"]
end

@testset "edge distances read as a numeric spatial measure" begin
    # integer-looking 0/1 and few µm levels would otherwise be called a code set
    @test !Cecelia._is_categorical_col([0, 1, 1, 0], "flow.cell.inside#gc")
    @test !Cecelia._is_categorical_col([0.0, 10.0, 20.0], "flow.cell.edge_dist#tissue")
end

@testset "regionEdge on a real calibrated timecourse" begin
    fx = fixture_for(:cells, :movie, :calibrated)    # a fresh temp copy: the task writes into it
    if fx === nothing
        @test_skip "regionEdge end to end (calibrated fixture missing)"
    else
        img = Cecelia._load_image(fx.dir)
        full = label_props(img; value_name = "B") |> select_cols(["mean_intensity_0"]) |> as_df
        v = sort(full.mean_intensity_0); cut = v[cld(2 * nrow(full), 3)]
        m = PopulationTree(pop_type = "flow", value_name = "B")
        add_pop!(m, "hi"; gate = RectangleGate("mean_intensity_0", "mean_intensity_1", cut, 1e12, -1e12, 1e12))
        save_pop_tree!(m, img)

        # the tissue: every cell, so only a distance column
        res = Cecelia._run_task(RegionEdge(), img, Dict{String,Any}("regionPops" => ["B/"], "closeRadius" => 20.0);
                                on_log = _ -> nothing)
        @test res !== nothing && img_region_edge_suffixes(img) == ["tissue"]
        cols = col_names(label_props(img; value_name = "B"); data_type = :obs)
        @test "flow.cell.edge_dist#tissue" in cols && !("flow.cell.inside#tissue" in cols)

        # the preview gets what the run computes from: the region's labels, the resolved hole setting
        pp = Cecelia.preview_params(RegionEdge(), Dict{String,Any}("regionPops" => ["B/hi"], "closeRadius" => 20.0), img)
        @test pp["fillHoles"] == false && pp["closeRadius"] == 20.0 && pp["propsPath"] == img_label_props_path(img, "B")
        @test sort(pp["regionLabels"]) == sort(Int.(label_props(img; value_name = "B") |> select_cols(["mean_intensity_0"]) |>
                                                    as_df |> d -> d[d.mean_intensity_0 .>= cut, :label]))
        pt = Cecelia.preview_params(RegionEdge(), Dict{String,Any}("regionPops" => ["B/"]), img)
        @test pt["regionLabels"] === nothing && pt["fillHoles"] == true

        # a region: its own cells are always inside; distances are µm ≥ 0
        res = Cecelia._run_task(RegionEdge(), img,
                                Dict{String,Any}("regionPops" => ["B/hi"], "edgeName" => "hi", "closeRadius" => 20.0);
                                on_log = _ -> nothing)
        @test res !== nothing
        df = label_props(img; value_name = "B") |>
             select_cols(["flow.cell.edge_dist#hi", "flow.cell.inside#hi", "mean_intensity_0"]) |> as_df
        members = df.mean_intensity_0 .>= cut
        @test all(df[members, "flow.cell.inside#hi"] .== 1)
        @test 0 < count(==(1), df[!, "flow.cell.inside#hi"]) < nrow(df)    # some cells sit outside
        d = filter(isfinite, df[!, "flow.cell.edge_dist#hi"])
        @test !isempty(d) && all(>=(0), d)

        # the boundary file: lines in native pixels, each on its frame and plane
        edge = JSON3.read(read(img_region_edge_path(img, "hi"), String))
        @test edge.closeRadius == 20.0 && !isempty(edge.lines)
        @test all(l -> l.t >= 0 && l.plane >= 0 && length(l.x) == length(l.y) >= 2, edge.lines)
        qc = Cecelia.read_qc(img, "spatialAnalysis.regionEdge", "hi")
        @test qc !== nothing && qc["metrics"]["nRegionCells"] == count(members)

        # listed + deletable through the analysis-run registry: file, QC and both columns go
        kind = only(k for k in Cecelia.RUN_KINDS if k.kind == "regionEdges")
        @test sort([r.key for r in kind.list(img)]) == ["hi", "tissue"]
        kind.delete(img, only(r for r in kind.list(img) if r.key == "hi"))
        cols = col_names(label_props(img; value_name = "B"); data_type = :obs)
        @test !any(c -> endswith(c, "#hi"), cols) && "flow.cell.edge_dist#tissue" in cols
        @test img_region_edge_suffixes(img) == ["tissue"]
        rm(fx.project; recursive = true)
    end
end
