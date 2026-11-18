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

# A graph over TWO segmentations: the same label can name a B cell and a T cell. Fixture
# `test-data/graph-cross/` (`make_spatial_graph_fixture.py`): the real builder over B's frames 0–2 and
# T = B's cells whose label is divisible by 3, moved 7 px along x. Every end must sit on ITS
# segmentation's cell — a lookup by label alone puts a T end on the B cell of that label, 7 px off —
# and the "only between shown populations" filter must match on segmentation and label together.
@testset "API: neighbour-graph lines across two segmentations" begin
    # MAINT-EXEMPT: the graph was cut from exactly this B table; any other table would leave it unplaced
    src = api_fixture("testpr", "1", "KDIeEm")
    cross = normpath(joinpath(api_test_projects_dir(), "..", "graph-cross"))
    if !api_have_fixture(joinpath(cross, "cross.h5ad"))
        @test_skip "graph-cross fixture (missing)"
    else
        conf = cecelia_conf()
        dirs = get!(conf, "dirs", Dict{String,Any}())
        had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
        dirs["projects"] = mktempdir()
        try
            proj = create_project!(name = "graph-cross")
            img = add_image!(add_set!(proj; name = "s"); name = "im", uid = "grphX")
            mkpath(img_label_props_dir(img)); mkpath(img_spatial_graph_dir(img))
            cp(joinpath(src, "labelProps", "B.h5ad"), img_label_props_path(img, "B"))
            cp(joinpath(cross, "T.h5ad"), img_label_props_path(img, "T"))
            cp(joinpath(cross, "cross.h5ad"), joinpath(img_spatial_graph_dir(img), "cross.h5ad"))

            # where each cell is, by definition: B's centroid; T's = B's of that label + 7 px in x
            lp = label_props(img_label_props_path(img, "B"))
            view_centroid_cols(lp; order = [:x, :y, :z])
            df = as_df(lp; include_x = false, include_obs = false)
            b_at = Dict(Int(r.label) => Float32[r.centroid_x, r.centroid_y, r.centroid_z] for r in eachrow(df))
            at(vn, l) = vn == "B" ? b_at[l] : b_at[l] .+ Float32[7, 0, 0]

            g = spatial_graph_segments(img, "cross")
            s = g.segs
            @test g.n_unplaced == 0 && g.n_cross_frame == 0
            @test any(k -> g.a_vn[k] != g.b_vn[k], 1:length(g))                 # edges join B and T
            twins = [k for k in 1:length(g) if g.a_vn[k] != g.b_vn[k] && g.a_label[k] == g.b_label[k]]
            @test !isempty(twins)                                                  # one label, two cells
            ends = Set((g.a_vn[k], g.a_label[k], g.b_vn[k], g.b_label[k]) for k in 1:length(g))
            for (av, a, bv, b) in ends
                ix = [k for k in 1:length(g) if (g.a_vn[k], g.a_label[k], g.b_vn[k], g.b_label[k]) == (av, a, bv, b)]
                pts = vcat([[s.x0[k], s.y0[k], s.z0[k]] for k in ix], [[s.x1[k], s.y1[k], s.z1[k]] for k in ix])
                on(c) = any(e -> isapprox(e, c; atol = 1f-3), pts)
                @test on(at(av, a)) && on(at(bv, b))
            end

            # the route names each end's segmentation by index into `vns`
            q = "/api/viewer/spatial-graphs?projectUid=$(proj.uid)&imageUid=$(img.uid)&suffix=cross"
            j = JSON3.read(last(api_viewer_spatial_graphs(HTTP.Request("GET", q))))
            vns = collect(String, j.vns)
            @test sort(vns) == ["B", "T"]
            @test [vns[i + 1] for i in j.aVn] == g.a_vn && [vns[i + 1] for i in j.bVn] == g.b_vn
            @test collect(j.aLabel) == g.a_label && collect(j.bLabel) == g.b_label

            # shown = T's cells only, by (segmentation, label): the T–T edges, counted from the graph file
            e = spatial_graph_edges(joinpath(img_spatial_graph_dir(img), "cross.h5ad"))
            t_cells = Set(("T", e.label[r]) for r in eachindex(e.label) if e.value_name[r] == "T")
            tt = Set((e.label[p], e.label[q]) for (p, q) in zip(e.i, e.j)
                     if e.value_name[p] == "T" && e.value_name[q] == "T")
            sub = spatial_graph_between(g, t_cells)
            @test !isempty(tt)
            @test length(sub) == count(k -> g.a_vn[k] == "T" && g.b_vn[k] == "T", 1:length(g))
            @test Set((g.a_label[k], g.b_label[k]) for k in 1:length(g) if g.a_vn[k] == g.b_vn[k] == "T") == tt
            # the same labels named as B cells select B's edges among them, never a T end
            b_same = spatial_graph_between(g, Set(("B", l) for (_, l) in t_cells))
            @test length(b_same) == count(k -> g.a_vn[k] == g.b_vn[k] == "B" &&
                                              (g.a_label[k] % 3 == 0) && (g.b_label[k] % 3 == 0), 1:length(g))
        finally
            had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        end
    end
end
