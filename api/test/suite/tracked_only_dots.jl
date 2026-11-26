# "Tracked only" (docs/todo/TRACK_SUBPOP_GATING_PLAN.md Decision 9): while a segmentation's tracks are
# drawn, a gated cell the tracker never followed keeps its dot, dimmed. The viewer dims in
# `buildPointBufferLayers` (`UNTRACKED_DIM`); a recording reads the look's `dimUntracked` segmentations
# and dims the same dots in `_overlay_parts`. Fixture: a tracked movie, with one flow gate over every
# cell added to a temp copy, so the pop holds tracked and untracked cells alike.

@testset "API: Tracked only — untracked dots dimmed in a recording" begin
    fx = fixture_for(:cells, :movie, :tracked)
    if fx === nothing
        @test_skip "tracked movie fixture (missing)"
    else
        dir = mktempdir()
        cp(fx.project, joinpath(dir, basename(fx.project)))
        old = get(Cecelia.cecelia_conf()["dirs"], "projects", nothing)
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            img = init_object(basename(fx.project), fx.uid)
            vn = fx.value_name
            m = Cecelia.load_pop_tree(img; value_name = vn, pop_type = "flow")
            Cecelia.add_pop!(m, "all"; colour = "#ff0000",
                             gate = Cecelia.RectangleGate("area", "area", -1e12, 1e12, -1e12, 1e12))
            Cecelia.save_pop_tree!(m, img)

            # the look's key reaches the plan
            look(dim) = Dict{String,Any}("showPopulations" => true, "popTypes" => ["flow"],
                                         "popAllSegmentations" => true, "dimUntracked" => dim)
            plan_of(dim) = overlay_track_plan(_overlays_raw_from_config(look(dim), false))
            @test plan_of([vn]).dim_untracked == Set([vn])
            @test isempty(plan_of(String[]).dim_untracked)

            # which cells have a track, frame 0, from the table itself
            df = Cecelia.label_props(img; value_name = vn) |> lp -> Cecelia.select_cols(lp, ["track_id", "centroid_t"]) |> Cecelia.as_df
            t0 = [i for i in 1:size(df, 1) if round(Int, df.centroid_t[i]) == 0]
            tracked_t0 = count(i -> _is_tracked_id(df.track_id[i]), t0)
            @test 0 < tracked_t0 < length(t0)                       # the fixture has both kinds

            red = RGB{N0f8}(1, 0, 0)
            on  = _overlay_parts(img; value_name = vn, pop_type = "flow", include_tracks = false, dim_untracked = true)
            off = _overlay_parts(img; value_name = vn, pop_type = "flow", include_tracks = false)
            # every dot is drawn either way — dimmed, never hidden
            @test length(on.pts_by_t[0].colour) == length(off.pts_by_t[0].colour) == length(t0)
            @test all(==(red), off.pts_by_t[0].colour)
            # on: the tracked cells keep the pop colour; exactly the untracked ones take the dimmed one
            @test count(==(red), on.pts_by_t[0].colour) == tracked_t0
            @test count(==(_dim_untracked(red)), on.pts_by_t[0].colour) == length(t0) - tracked_t0
            @test _dim_untracked(red) == RGB{N0f8}(UNTRACKED_DIM, 0, 0)
        finally
            old === nothing ? delete!(Cecelia.cecelia_conf()["dirs"], "projects") :
                              (Cecelia.cecelia_conf()["dirs"]["projects"] = old)
        end
    end
end
