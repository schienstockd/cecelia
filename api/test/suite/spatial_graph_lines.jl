# Neighbour-graph lines (`spatialAnalysis.cellNeighbours` → drawn edges) — the viewer route and the movie
# closure read the same `spatial_graph_segments`, so the two draw the same lines.
# Fixtures: `spatialGraph/{net,pooled}.h5ad`, the real graph builder's output over `B.h5ad` frames 0–2
# (test-data/README.md, `make_spatial_graph_fixture.py`): 219 edges per timepoint, 591 pooled.

@testset "API: neighbour-graph lines — edges, frames, look → movie, and the viewer route" begin
    src = api_fixture("testpr", "1", "KDIeEm")
    if !api_have_fixture(joinpath(src, "spatialGraph", "net.h5ad"))
        @test_skip "spatialGraph fixtures (missing)"
    else
        # the look's `spatialGraphs` reach the plan; a config with only a graph still draws overlays
        ov = _overlays_raw_from_config(Dict{String,Any}("spatialGraphs" => Dict("net" => "#00ff00")), false)
        @test ov isa AbstractDict && ov["spatialGraphs"] == Dict("net" => "#00ff00")
        @test overlay_track_plan(ov).spatial_graphs == [("net", "#00ff00")]
        @test plan_draws_lines(overlay_track_plan(ov))
        @test !plan_draws_lines(overlay_track_plan(Dict{String,Any}()))

        conf = cecelia_conf()
        dirs = get!(conf, "dirs", Dict{String,Any}())
        had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
        dirs["projects"] = mktempdir()
        try
            proj = create_project!(name = "graph-lines")
            img = add_image!(add_set!(proj; name = "s"); name = "im", uid = "grphA")
            # calibrated, z ≠ x: on 1 µm/px the route's "in µm" below could not tell µm from pixels
            merge!(img.meta, Dict("PhysicalSizeX" => "0.5", "PhysicalSizeY" => "0.5", "PhysicalSizeZ" => "2.0"))
            save!(img)
            mkpath(img_label_props_dir(img))
            cp(joinpath(src, "labelProps", "B.h5ad"), img_label_props_path(img, "B"))
            cp(joinpath(src, "spatialGraph"), img_spatial_graph_dir(img))
            @test img_spatial_graph_suffixes(img) == ["knn", "net", "pooled"]

            # each cell's centroid and frame, by label, from its cell table (the definition of where
            # an edge is drawn: centroid to centroid, on the cells' frame)
            lp = label_props(img_label_props_path(img, "B"))
            view_centroid_cols(lp; order = [:x, :y, :z])
            df = as_df(lp; include_x = false, include_obs = false)
            cell = Dict(Int(r.label) => (Float32(r.centroid_x), Float32(r.centroid_y), Float32(r.centroid_z),
                                         round(Int, r.centroid_t)) for r in eachrow(df))

            g = spatial_graph_segments(img, "net")
            s = g.segs
            pairs = Set(zip(g.a_label, g.b_label))
            @test length(pairs) == 219 && g.n_cross_frame == 0 && g.n_unplaced == 0
            @test all(==("B"), g.a_vn) && all(==("B"), g.b_vn)
            for (a, b) in pairs
                ix = [k for k in 1:length(s) if g.a_label[k] == a && g.b_label[k] == b]
                # ≈: a piece's far end is interpolated (`_plane_pieces`), a Float32 ulp off the centroid
                ends = vcat([[s.x0[k], s.y0[k], s.z0[k]] for k in ix], [[s.x1[k], s.y1[k], s.z1[k]] for k in ix])
                at(c) = any(e -> isapprox(e, collect(c[1:3]); atol = 1f-3), ends)
                @test at(cell[a]) && at(cell[b])
                @test all(k -> s.t[k] == cell[a][4] == cell[b][4], ix)
            end
            # a piece lies within the plane it draws on (an edge across planes is split per plane)
            @test all(k -> abs((s.z0[k] + s.z1[k]) / 2 - s.plane[k]) <= 0.5, 1:length(s))
            @test length(s) > length(pairs)                  # 3D cells: some edges cross planes

            # pooled over time: edges joining two frames are counted, not drawn
            gp = spatial_graph_segments(img, "pooled")
            np = length(Set(zip(gp.a_label, gp.b_label)))
            @test gp.n_cross_frame > 0 && np + gp.n_cross_frame == 591
            @test all(k -> cell[gp.a_label[k]][4] == cell[gp.b_label[k]][4] == gp.segs.t[k], 1:length(gp))

            @test spatial_graph_segments(img, "none") === nothing

            # the movie closure: each frame's own edges, static, in the look's colour
            cl = viewer_overlay_closure(img; value_name = "", plan = overlay_track_plan(ov))
            @test cl !== nothing
            _, s0 = cl(0)
            n0 = count(==(0), s.t)
            # static, at the graph's own width: t1 = -(1 + width) (`lineWidth` in overlay_quads.wgsl)
            @test length(s0.x0) == n0 && all(==(-(1 + SPATIAL_GRAPH_WIDTH)), s0.t1)
            @test all(c -> c == RGB{N0f8}(0, 1, 0), s0.colour)
            @test s0.plane == Float64.(s.plane[s.t .== 0])
            ovw = _overlays_raw_from_config(Dict{String,Any}("spatialGraphs" => Dict("net" => "#00ff00"),
                                                             "spatialGraphWidth" => 0.5), false)
            @test all(==(-1.5), viewer_overlay_closure(img; value_name = "", plan = overlay_track_plan(ovw))(0)[2].t1)

            # the viewer route: the same lines, in µm; an absent graph is a 404
            q = "/api/viewer/spatial-graphs?projectUid=$(proj.uid)&imageUid=$(img.uid)&suffix=pooled"
            code, body = api_viewer_spatial_graphs(HTTP.Request("GET", q))
            @test code == 200
            j = JSON3.read(body)
            ux = physical_size_for_axis(first(img_physical_sizes(img)), :x)
            @test ux == 0.5
            @test j.nSegments == length(gp) && isapprox(collect(j.x0), gp.segs.x0 .* ux; rtol = 1e-6)   # Float32 µm via JSON
            @test j.nCrossFrame == gp.n_cross_frame && occursin("join two frames", j.note)
            code2, _ = api_viewer_spatial_graphs(HTTP.Request("GET", replace(q, "pooled" => "none")))
            @test code2 == 404

            # segmented again after the graph was built (its mask store rewritten): drawn, but flagged —
            # a label may name another cell. Every note that applies is said, cross-frame included.
            @test !spatial_graph_segments(img, "pooled").stale
            sleep(1.1)
            mkpath(img_labels_path(img, "B")); write(joinpath(img_labels_path(img, "B"), ".zgroup"), "{}")
            @test spatial_graph_segments(img, "pooled").stale
            js = JSON3.read(last(api_viewer_spatial_graphs(HTTP.Request("GET", q))))
            @test js.stale && occursin("re-run the graph", js.note) && occursin("join two frames", js.note)
        finally
            had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        end
    end
end
