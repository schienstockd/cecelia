# Region boundary lines (`spatialAnalysis.regionEdge` → `regionEdges/<name>.json`) — the viewer route and
# the movie closure read the same `region_edge_segments`, so the two draw the same lines. The file is
# this package's own output format (`region_edge_run.py`), written here by hand: two polylines, pixels.

@testset "API: region edge lines — segments, look → movie, and the viewer route" begin
    # the look's `regionEdges` reach the plan; a config with only an edge still draws overlays
    ov = _overlays_raw_from_config(Dict{String,Any}("regionEdges" => Dict("tissue" => "#00ff00")), false)
    @test ov isa AbstractDict && ov["regionEdges"] == Dict("tissue" => "#00ff00")
    @test overlay_track_plan(ov).region_edges == [("tissue", "#00ff00")]
    @test overlay_track_plan(ov).region_edge_width == REGION_EDGE_WIDTH
    @test plan_draws_lines(overlay_track_plan(ov))

    conf = cecelia_conf()
    dirs = get!(conf, "dirs", Dict{String,Any}())
    had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
    dirs["projects"] = mktempdir()
    try
        proj = create_project!(name = "edge-lines")
        img = add_image!(add_set!(proj; name = "s"); name = "im", uid = "edgeA")
        # calibrated at 0.5 µm/px: the route's µm must be twice the stored pixels
        merge!(img.meta, Dict("PhysicalSizeX" => "0.5", "PhysicalSizeY" => "0.5"))
        save!(img)
        mkpath(img_region_edge_dir(img))
        write(img_region_edge_path(img, "tissue"), JSON3.write(Dict(
            "closeRadius" => 75.0, "step" => 5.0, "fillHoles" => true, "axes" => ["y", "x"],
            "lines" => [Dict("t" => -1, "plane" => 0, "x" => [10.0, 20.0, 30.0], "y" => [5.0, 5.0, 15.0]),
                        Dict("t" => -1, "plane" => 0, "x" => [100.0, 110.0], "y" => [50.0, 60.0])])))
        @test img_region_edge_suffixes(img) == ["tissue"]

        # one segment per consecutive pair of points: 2 + 1, every frame, plane 0, in native pixels
        s = region_edge_segments(img, "tissue")
        @test length(s) == 3 && !s.flat
        @test s.x0 == Float32[10, 20, 100] && s.x1 == Float32[20, 30, 110]
        @test s.y0 == Float32[5, 5, 50] && s.y1 == Float32[5, 15, 60]
        @test all(==(-1), s.t) && all(==(0), s.plane)
        @test region_edge_segments(img, "none") === nothing

        # a 2D run drawn on a stack is flat: every plane; a 3D run's lines keep their own plane
        @test region_edge_segments(img_region_edge_path(img, "tissue"); is_stack = true).flat
        @test all(==(-1), region_edge_segments(img_region_edge_path(img, "tissue"); is_stack = true).plane)
        p3 = joinpath(mktempdir(), "gc.json")
        write(p3, JSON3.write(Dict("axes" => ["z", "y", "x"],
            "lines" => [Dict("t" => 2, "plane" => 4, "x" => [1.0, 2.0], "y" => [1.0, 1.0])])))
        s3 = region_edge_segments(p3; is_stack = true)
        @test !s3.flat && s3.plane == [4] && s3.z0 == Float32[4] && s3.t == [2]

        # the movie closure: static, at the edge's own width, in the look's colour
        cl = viewer_overlay_closure(img; value_name = "", plan = overlay_track_plan(ov))
        @test cl !== nothing
        _, s0 = cl(0)
        @test length(s0.x0) == 3 && all(==(-(1 + REGION_EDGE_WIDTH)), s0.t1)
        @test all(c -> c == RGB{N0f8}(0, 1, 0), s0.colour)
        ovw = _overlays_raw_from_config(Dict{String,Any}("regionEdges" => Dict("tissue" => ""),
                                                         "regionEdgeWidth" => 3), false)
        _, sw = viewer_overlay_closure(img; value_name = "", plan = overlay_track_plan(ovw))(0)
        @test all(==(-4.0), sw.t1)
        @test all(c -> c == RGB{N0f8}(1, 0xd3 / 255, 0x3d / 255), sw.colour)    # REGION_EDGE_COLOUR

        # the viewer route: the same lines, in µm; an absent run is a 404
        q = "/api/viewer/region-edges?projectUid=$(proj.uid)&imageUid=$(img.uid)&suffix=tissue"
        code, body = api_viewer_region_edges(HTTP.Request("GET", q))
        @test code == 200
        j = JSON3.read(body)
        @test j.nSegments == 3 && collect(j.x0) == [5.0, 10.0, 50.0] && collect(j.y1) == [2.5, 7.5, 30.0]
        code2, _ = api_viewer_region_edges(HTTP.Request("GET", replace(q, "tissue" => "none")))
        @test code2 == 404
    finally
        had ? (dirs["projects"] = old) : delete!(dirs, "projects")
    end
end
