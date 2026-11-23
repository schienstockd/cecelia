# ── POST /api/gating/pop/add-split — every region of a slice / quadrant as one change ──────────────
# What the route promises (docs/POPULATION.md → Split gates): one population per region, all or nothing;
# the regions share a `group`, so a set-gate on one moves them all; and they tile the parent — the
# counts add up to it on a real cell table.
@testset "API: pop/add-split adds every region, moves them together, refuses partially" begin
    fx = fixture_for(:cells)
    if fx === nothing
        @test_skip "cell-table fixture missing"
    else
        # a temp copy: a plain (non-derived) fixture comes back as the committed directory itself
        dir = mktempdir()
        cp(fx.project, joinpath(dir, basename(fx.project)))
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        empty!(_GATING_HISTORY)
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            puid, uid, vn = basename(fx.project), fx.uid, fx.value_name
            q = "projectUid=$puid&imageUid=$uid&valueName=$vn&popType=flow"
            chb = JSON3.read(api_gating_channels(HTTP.Request("GET", "/api/gating/channels?$q"))[2])
            # x = the column with the most distinct values (in the small fixture table only one varies); the line
            # is vertical through its median, so the cells fall on both sides
            cols = String.(chb.columns)
            all_df = as_df(select_cols(label_props(_gating_image(puid, uid)[1]; value_name = vn), cols))
            xc, yc = sort(cols; by = c -> -length(unique(all_df[!, c])))[1:2]
            base = Dict{String,Any}("projectUid" => puid, "imageUid" => uid, "valueName" => vn, "popType" => "flow")
            post(h, extra) = h(Vector{UInt8}(JSON3.write(merge(base, extra))))
            count(pop) = JSON3.read(api_gating_stats(HTTP.Request("GET", "/api/gating/stats?$q&pop=$pop"))[2]).count
            img() = _gating_image(puid, uid)[1]
            tree() = load_pop_tree(img(); value_name = vn, pop_type = "flow")
            spec(kind; kw...) = Dict{String,Any}("kind" => kind, "x_channel" => xc, "y_channel" => yc,
                                                  "x_transform" => Dict("kind" => "linear"),
                                                  "y_transform" => Dict("kind" => "linear"),
                                                  (String(k) => v for (k, v) in kw)...)
            # a vertical line through the middle of the cloud, drawn upward: its left (side 1) is x < mx
            pts = as_df(select_cols(label_props(img(); value_name = vn), [xc, yc]))
            med(v) = (f = sort(Float64[x for x in v if isfinite(x)]); f[length(f) ÷ 2 + 1])
            mx, my = med(pts[!, xc]), med(pts[!, yc])
            line = [[mx, my - 1], [mx, my + 1]]

            # a missing region, or an unknown kind, is refused before anything is written
            st, _ = post(api_gating_pop_add_split, Dict{String,Any}("gate" => spec("slice"; vertices = line),
                         "pops" => [Dict("name" => "hi", "region" => 1)]))
            @test st == 400 && isempty(pop_paths(tree()))
            st, _ = post(api_gating_pop_add_split, Dict{String,Any}("gate" => spec("polygon"; vertices = line),
                         "pops" => [Dict("name" => "a", "region" => 1)]))
            @test st == 400

            st, _ = post(api_gating_pop_add_split, Dict{String,Any}("gate" => spec("slice"; vertices = line),
                         "pops" => [Dict("name" => "hi", "region" => 1, "colour" => "#f00"),
                                    Dict("name" => "lo", "region" => -1, "colour" => "#0f0")]))
            @test st == 200
            m = tree()
            @test pop_at(m, "/hi").gate isa SliceGate && pop_at(m, "/lo").gate.side == -1
            @test pop_at(m, "/hi").gate.group == pop_at(m, "/lo").gate.group != ""
            @test count("/hi") > 0 && count("/lo") > 0
            @test count("/hi") + count("/lo") == count("root")          # every (finite) cell, exactly once

            # moving one region's line moves the other's — one set-gate, both members follow
            moved = merge(spec("slice"; vertices = [[mx - 1e9, my - 1], [mx - 1e9, my + 1]]),
                          Dict("side" => 1, "group" => pop_at(m, "/hi").gate.group))
            @test post(api_gating_pop_set_gate, Dict{String,Any}("path" => "/hi", "gate" => moved))[1] == 200
            @test count("/hi") == 0 && count("/lo") == count("root")
            @test tree().pops["/lo"].gate.vertices == [(mx - 1e9, my - 1), (mx - 1e9, my + 1)]

            # a taken name refuses the WHOLE split — no half-made quadrant
            st, _ = post(api_gating_pop_add_split, Dict{String,Any}(
                "gate" => spec("quadrant"; centre = [mx, my], arms = [[mx + 1, my], [mx, my + 1], [mx - 1, my], [mx, my - 1]]),
                "pops" => [Dict("name" => n, "region" => q) for (q, n) in enumerate(["pp", "mp", "hi", "pm"])]))
            @test st == 400 && Set(pop_paths(tree())) == Set(["/hi", "/lo"])

            st, _ = post(api_gating_pop_add_split, Dict{String,Any}(
                "gate" => spec("quadrant"; centre = [mx, my], arms = [[mx + 1, my], [mx, my + 1], [mx - 1, my], [mx, my - 1]]),
                "pops" => [Dict("name" => n, "region" => q) for (q, n) in enumerate(["pp", "mp", "mm", "pm"])]))
            @test st == 200
            @test sum(count("/$n") for n in ("pp", "mp", "mm", "pm")) == count("root")
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end
