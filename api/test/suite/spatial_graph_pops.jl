# "Only between shown populations" — a neighbour graph's edges whose two cells are both in a pop drawn
# as a point. The viewer filters its payload by each segment's two cells (`graphEdgesBetween`); the movie
# closure keeps the same edges (`spatial_graph_between`) from the pops it draws by the look's eyes.
# Fixture: testpr/KDIeEm's track-cluster pops (Scanning / Directed / Meandering on B) and its graphs.

@testset "API: neighbour-graph lines — only between shown populations" begin
    if !api_have_fixture(api_fixture("testpr", "1", "KDIeEm", "spatialGraph", "net.h5ad"))
        @test_skip "spatialGraph fixtures (missing)"
    else
        dir = mktempdir()
        cp(api_fixture("testpr"), joinpath(dir, "testpr"))
        old = get(Cecelia.cecelia_conf()["dirs"], "projects", nothing)
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            img = init_object("testpr", "KDIeEm")
            pops = Dict(String(p.path) => Set(Int.(p.labels)) for p in _overlay_pops(img, "B", "trackclust"))
            scanning = pops["/Scanning"]
            gs = spatial_graph_segments(img, "net")

            # the route names each segment's two cells: the movie and the viewer filter on the same pairs
            q = "/api/viewer/spatial-graphs?projectUid=testpr&imageUid=KDIeEm&suffix=net"
            j = JSON3.read(last(api_viewer_spatial_graphs(HTTP.Request("GET", q))))
            @test collect(j.vns) == ["B"]
            @test collect(j.aLabel) == gs.a_label && collect(j.bLabel) == gs.b_label
            @test all(==(0), j.aVn) && all(==(0), j.bVn)

            # only Scanning's point eye on: the edges drawn at t 0 are exactly those whose two cells are
            # both Scanning cells — counted here from the cells' labels, per segment
            look(pops_only) = Dict{String,Any}(
                "showPopulations" => true, "popTypes" => ["trackclust"], "popAllSegmentations" => true,
                "popEyes" => Dict("B" => Dict("trackclust::/Directed" => false, "trackclust::/Meandering" => false)),
                "spatialGraphs" => Dict("net" => "#00ff00"), "spatialGraphPopsOnly" => pops_only)
            plan_of(pops_only) = overlay_track_plan(_overlays_raw_from_config(look(pops_only), false))
            @test plan_of(true).spatial_graph_pops_only && !plan_of(false).spatial_graph_pops_only
            green = RGB{N0f8}(0, 1, 0)
            n_lines(cl, t) = cl === nothing ? 0 : (s = cl(t)[2]; s === nothing ? 0 : count(==(green), s.colour))
            want = count(k -> gs.segs.t[k] == 0 && gs.a_label[k] in scanning && gs.b_label[k] in scanning,
                         1:length(gs))
            every = count(==(0), gs.segs.t)
            @test 0 < want < every
            cl = viewer_overlay_closure(img; value_name = "", plan = plan_of(true), show_pops = true)
            @test n_lines(cl, 0) == want
            # off: every edge on the frame, whatever the pops
            @test n_lines(viewer_overlay_closure(img; value_name = "", plan = plan_of(false), show_pops = true), 0) == every
            # on with no pops drawn: no edge has a shown cell at either end
            @test n_lines(viewer_overlay_closure(img; value_name = "", plan = plan_of(true), show_pops = false), 0) == 0

            # the subset itself: both ends in the set, nothing else
            sub = spatial_graph_between(gs, Set(("B", l) for l in scanning))
            @test length(sub) == count(k -> gs.a_label[k] in scanning && gs.b_label[k] in scanning, 1:length(gs))
            @test length(spatial_graph_between(gs, Set{Tuple{String,Int}}())) == 0
        finally
            old === nothing ? delete!(Cecelia.cecelia_conf()["dirs"], "projects") :
                              (Cecelia.cecelia_conf()["dirs"]["projects"] = old)
        end
    end
end
