# Skeleton lines (`segment.branching` → drawn lines) — the viewer route and the movie closure read the
# same `skeleton_segments`, so the two draw the same lines (docs/todo/DRAW_SPEC_PLAN.md Phase 3).
# Fixture: `skel__branch.h5ad`, the real branching writer's output (test-data/README.md).

@testset "API: skeleton lines — look → plan → movie segments, and the viewer route" begin
    h5 = api_fixture("testpr", "1", "KDIeEm", "labelProps", "skel__branch.h5ad")
    if !api_have_fixture(h5)
        @test_skip "skel__branch fixture (missing)"
    else
        # the look's `skeletons` reach the plan; a config with only skeletons still draws overlays
        ov = _overlays_raw_from_config(Dict{String,Any}("skeletons" => Dict("SHG" => "#ff0000")), false)
        @test ov isa AbstractDict && ov["skeletons"] == Dict("SHG" => "#ff0000")
        @test isempty(_overlays_raw_from_config(Dict{String,Any}("showPopulations" => true), false)["skeletons"])
        @test overlay_track_plan(ov).skeletons == [("SHG", "#ff0000")]
        @test isempty(overlay_track_plan(Dict{String,Any}()).skeletons)

        conf = cecelia_conf()
        dirs = get!(conf, "dirs", Dict{String,Any}())
        had = haskey(dirs, "projects"); old = get(dirs, "projects", nothing)
        dirs["projects"] = mktempdir()
        try
            proj = create_project!(name = "skeleton-lines")
            img = add_image!(add_set!(proj; name = "s"); name = "im", uid = "skelA")
            mkpath(img_label_props_dir(img))
            cp(h5, img_branch_props_path(img, "SHG"))

            # the movie closure: each frame's own lines, static (t1 -1, unfaded), in the look's colour
            cl = viewer_overlay_closure(img; value_name = "", plan = overlay_track_plan(ov))
            @test cl !== nothing
            p0, s0 = cl(0)
            @test p0 === nothing
            @test length(s0.x0) == 3 && all(==(-1), s0.t1) && all(==(0), s0.tail)
            @test all(c -> c == RGB{N0f8}(1, 0, 0), s0.colour)
            @test all(==(0.0), s0.plane)                     # 2D on a 2D image: plane 0
            @test cl(1)[2] === nothing                       # no lines at t 1
            st = _overlays3d_state(cl, 2)["segments"]
            @test st["t1"] == fill(-1.0, 3) && st["length"] == 0 && st["plane"] == zeros(3)
            # no skeleton named → nothing to draw
            @test viewer_overlay_closure(img; value_name = "", plan = overlay_track_plan(Dict{String,Any}())) === nothing

            # the viewer route: the same lines, in µm
            q = "/api/viewer/skeletons?projectUid=$(proj.uid)&imageUid=$(img.uid)&valueName=SHG"
            code, body = api_viewer_skeletons(HTTP.Request("GET", q))
            @test code == 200
            j = JSON3.read(body)
            @test j.nSegments == 6 && sort(unique(collect(j.t))) == [0, 2] && !j.flat
            ux = physical_size_for_axis(first(img_physical_sizes(img)), :x)
            segs = skeleton_segments(img, "SHG")
            @test collect(j.x0) ≈ segs.x0 .* ux
            # a skeleton with no table answers empty, not an error
            code2, body2 = api_viewer_skeletons(HTTP.Request("GET", replace(q, "SHG" => "none")))
            @test code2 == 200 && JSON3.read(body2).nSegments == 0
        finally
            had ? (dirs["projects"] = old) : delete!(dirs, "projects")
        end
    end
end
