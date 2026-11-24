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
            count(pop) = JSON3.read(api_gating_stats(HTTP.Request("GET", "/api/gating/stats?$q&pop=$(HTTP.escapeuri(pop))"))[2]).count
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

            # ── 1D gates on xc's histogram (x = y = xc) ──
            # a bisector following the valley in the whole range: its halves still tile the parent, and
            # the plot draws the cut where the rule puts it on the parent's cells — valley_cut on them
            lo, hi = extrema(filter(isfinite, Float64.(pts[!, xc])))
            hspec(kind; kw...) = Dict{String,Any}("kind" => kind, "x_channel" => xc, "y_channel" => xc,
                                                  "x_transform" => Dict("kind" => "linear"),
                                                  "y_transform" => Dict("kind" => "linear"),
                                                  (String(k) => v for (k, v) in kw)...)
            st, _ = post(api_gating_pop_add_split, Dict{String,Any}(
                "gate" => hspec("bisector"; cut = mx, rule = Dict("kind" => "valley", "lo" => lo, "hi" => hi)),
                "pops" => [Dict("name" => "v+", "region" => 1), Dict("name" => "v-", "region" => -1)]))
            @test st == 200
            @test count("/v+") + count("/v-") == count("root")
            pm = JSON3.read(api_gating_plotmeta(HTTP.Request("GET", "/api/gating/plotmeta?$q&x=$xc&y=$xc&pop=root"))[2])
            out = [o for o in pm.gates if o.path in ("/v+", "/v-")]
            @test length(out) == 2 && all(o -> o.kind == "bisector", out)
            want = valley_cut(Float64.(pts[!, xc]), lo, hi)
            if want === nothing                   # no valley in this table: the stored cut stands, flagged
                @test all(o -> o.rule_ok == false && o.cut ≈ mx, out)
                @test count("/v+") == Base.count(>(mx), filter(isfinite, pts[!, xc]))
            else
                @test all(o -> o.rule_ok == true && o.cut ≈ want, out)
                @test count("/v+") == Base.count(>(want), filter(isfinite, pts[!, xc]))
            end
            # a range: one population, the cells in [lo, mx]
            st, _ = post(api_gating_pop_add, Dict{String,Any}("name" => "r", "gate" => hspec("range"; lo = lo, hi = mx)))
            @test st == 200 && count("/r") == Base.count(v -> lo <= v <= mx, filter(isfinite, pts[!, xc]))
            # a 1D gate on two different channels is refused
            st, _ = post(api_gating_pop_add, Dict{String,Any}("name" => "bad", "gate" => merge(hspec("range"; lo = 0, hi = 1), Dict("y_channel" => yc))))
            @test st == 400
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end
