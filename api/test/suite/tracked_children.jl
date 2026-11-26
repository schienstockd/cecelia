# "Add tracked cells" on a multi-selection (docs/todo/TRACK_SUBPOP_GATING_PLAN.md P4): `/pop/add` with
# `under: [{parent, colour}]` adds the same filter child under every parent in ONE change — one undo
# step — and a refusal for any one parent writes nothing.

@testset "API: pop/add under several parents — one change, all or nothing" begin
    fx = fixture_for(:cells, :movie, :tracked)
    if fx === nothing
        @test_skip "tracked movie fixture (missing)"
    else
        dir = mktempdir()
        cp(fx.project, joinpath(dir, basename(fx.project)))
        old = get(Cecelia.cecelia_conf()["dirs"], "projects", nothing)
        empty!(_GATING_HISTORY)
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            puid, uid, vn = basename(fx.project), fx.uid, fx.value_name
            base = Dict{String,Any}("projectUid" => puid, "imageUid" => uid, "valueName" => vn, "popType" => "flow")
            post(h, extra) = h(Vector{UInt8}(JSON3.write(merge(base, extra))))
            tree() = load_pop_tree(_gating_image(puid, uid)[1]; value_name = vn, pop_type = "flow")
            gate = Dict{String,Any}("kind" => "rectangle", "x_channel" => "area", "y_channel" => "area",
                                    "x_min" => -1e12, "x_max" => 1e12, "y_min" => -1e12, "y_max" => 1e12)
            post(api_gating_pop_add, Dict{String,Any}("name" => "a", "gate" => gate))
            post(api_gating_pop_add, Dict{String,Any}("name" => "b", "gate" => gate))
            tracked = Dict{String,Any}("conditions" => [Dict("measure" => "track_id", "fun" => "gt", "values" => 0)],
                                       "measure" => "track_id", "fun" => "gt", "values" => 0)
            under(parents) = Dict{String,Any}("name" => "tracked", "filter" => tracked,
                                              "under" => [Dict("parent" => p, "colour" => "#00ff00") for p in parents])

            st, _ = post(api_gating_pop_add, under(["/a", "/b"]))
            @test st == 200
            m = tree()
            @test Set(pop_paths(m)) == Set(["/a", "/b", "/a/tracked", "/b/tracked"])
            @test pop_at(m, "/a/tracked").filter_measure == "track_id" && pop_at(m, "/b/tracked").colour == "#00ff00"

            # ONE undo step takes both away
            st, _ = post(api_gating_undo, Dict{String,Any}())
            @test st == 200 && Set(pop_paths(tree())) == Set(["/a", "/b"])

            # a parent that already has one (or does not exist) refuses the lot — nothing written
            post(api_gating_pop_add, under(["/a"]))
            st, _ = post(api_gating_pop_add, under(["/b", "/a"]))
            @test st == 400 && !has_pop(tree(), "/b/tracked")
            st, _ = post(api_gating_pop_add, under(["/b", "/nope"]))
            @test st == 400 && !has_pop(tree(), "/b/tracked")
            # a filter only, and a real list
            st, _ = post(api_gating_pop_add, merge(under(["/b"]), Dict{String,Any}("gate" => gate)))
            @test st == 400
            st, _ = post(api_gating_pop_add, Dict{String,Any}("name" => "tracked", "filter" => tracked, "under" => []))
            @test st == 400 && !has_pop(tree(), "/b/tracked")
        finally
            old === nothing ? delete!(Cecelia.cecelia_conf()["dirs"], "projects") :
                              (Cecelia.cecelia_conf()["dirs"]["projects"] = old)
            empty!(_GATING_HISTORY)
        end
    end
end
