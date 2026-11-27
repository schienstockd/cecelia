# GET /api/gating/track-classes + pop/update `trackClass` (docs/todo/TRACK_SUBPOP_GATING_PLAN.md P5):
# the route answers what `track_classes` computes, previews a threshold without storing it, and a
# stored setting is what the next read uses.

@testset "API: track classes — read, preview, store" begin
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
            get_(extra) = (r = api_gating_track_classes(HTTP.Request("GET",
                "/api/gating/track-classes?projectUid=$puid&imageUid=$uid&valueName=$vn&" * extra));
                (r[1], r[1] == 200 ? JSON3.read(r[2], Dict{String,Any}) : nothing))
            rect(lo, hi) = Dict{String,Any}("kind" => "rectangle", "x_channel" => "area", "y_channel" => "area",
                                            "x_min" => lo, "x_max" => hi, "y_min" => -1e12, "y_max" => 1e12)
            img = _gating_image(puid, uid)[1]
            a = sort(Cecelia.as_df(Cecelia.select_cols(Cecelia.label_props(img; value_name = vn), ["area"])).area)
            cut = a[length(a) ÷ 2]
            post(api_gating_pop_add, Dict{String,Any}("name" => "all", "gate" => rect(-1e12, 1e12)))
            post(api_gating_pop_add, Dict{String,Any}("name" => "small", "parent" => "/all", "gate" => rect(-1e12, cut)))
            post(api_gating_pop_add, Dict{String,Any}("name" => "big", "parent" => "/all", "gate" => rect(cut, 1e12)))

            st, d = get_("pop=/all&tracks=1")
            @test st == 200 && d["threshold"] == 0.5 && d["level"] == 1 && d["maxLevel"] == 1
            @test [c["path"] for c in d["children"]] == ["/all/small", "/all/big"]
            r = track_classes(img; value_name = vn, parent = "/all")
            @test d["total"] == size(r.table, 1) && d["trackIds"] == r.table.track_id
            @test sum(c["tracks"] for c in d["children"]) + d["mixed"] == d["total"]
            @test count(==(-1), d["classes"]) == d["mixed"]

            # a preview at 0.95 stores nothing
            st, d95 = get_("pop=/all&threshold=0.95")
            @test st == 200 && d95["threshold"] == 0.95 && d95["mixed"] >= d["mixed"]
            @test get_("pop=/all")[2]["threshold"] == 0.5
            @test get_("pop=/all&threshold=1")[1] == 400 && get_("pop=/nope")[1] == 404

            # stored on the population: the next read uses it; null puts the defaults back
            st, _ = post(api_gating_pop_update, Dict{String,Any}("path" => "/all", "trackClass" => Dict("threshold" => 0.95)))
            @test st == 200 && get_("pop=/all")[2]["mixed"] == d95["mixed"]
            @test post(api_gating_pop_update, Dict{String,Any}("path" => "/all", "trackClass" => Dict("level" => 0)))[1] == 400
            st, _ = post(api_gating_pop_update, Dict{String,Any}("path" => "/all", "trackClass" => nothing))
            @test st == 200 && get_("pop=/all")[2]["threshold"] == 0.5

            # "mostly <child>": one per sub-population, in its colour, one change; they are not
            # sub-populations themselves, so the classes do not change
            st, _ = post(api_gating_pop_add_track_classes, Dict{String,Any}("parent" => "/all"))
            @test st == 200
            m = load_pop_tree(img; value_name = vn, pop_type = "flow")
            @test m.pops["/all/mostly small"].track_class_of == "/all/small"
            @test m.pops["/all/mostly big"].colour == m.pops["/all/big"].colour
            @test [c["path"] for c in get_("pop=/all")[2]["children"]] == ["/all/small", "/all/big"]
            big = Set(t for (t, c) in zip(d["trackIds"], d["classes"]) if c == 1)
            cells = Cecelia.as_df(Cecelia.select_cols(Cecelia.label_props(img; value_name = vn), ["track_id"]))
            @test Set(pop_df(img, "flow", ["$vn/all/mostly big"]).label) ==
                  Set(l for (l, t) in zip(cells.label, cells.track_id) if t isa Real && isfinite(t) && Int(t) in big)
            # nothing left to add → 400; one undo takes both away
            @test post(api_gating_pop_add_track_classes, Dict{String,Any}("parent" => "/all"))[1] == 400
            @test post(api_gating_pop_add_track_classes, Dict{String,Any}("parent" => "/all/big"))[1] == 400
            @test post(api_gating_pop_add_track_classes, Dict{String,Any}("parent" => "/nope"))[1] == 404
            post(api_gating_undo, Dict{String,Any}())
            @test !has_pop(load_pop_tree(img; value_name = vn, pop_type = "flow"), "/all/mostly big")
        finally
            old === nothing ? delete!(Cecelia.cecelia_conf()["dirs"], "projects") :
                              (Cecelia.cecelia_conf()["dirs"]["projects"] = old)
            empty!(_GATING_HISTORY)
        end
    end
end
