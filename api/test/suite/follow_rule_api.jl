# A rectangle / polygon that follows its cloud, through the routes (docs/POPULATION.md → Rule gates): every
# save anchors a follow rule sent without an anchor — drawn, toggled on, or dragged on the plot, which
# never sends the anchor back — on that image's parent cells; the plot is told only that it follows and
# whether it found its cloud, and on the image it was saved on the outline is where it was drawn.
@testset "API: a follow rule is anchored where it is saved; the plot gets the rule, never the anchor" begin
    fx = fixture_for(:cells, :real)               # real cells: enough of them for a density with a cloud
    if fx === nothing
        @test_skip "real cell-table fixture missing"
    else
        dir = mktempdir()
        cp(fx.project, joinpath(dir, basename(fx.project)))
        old = Cecelia.cecelia_conf()["dirs"]["projects"]
        empty!(_GATING_HISTORY)
        try
            Cecelia.cecelia_conf()["dirs"]["projects"] = dir
            puid, uid, vn = basename(fx.project), fx.uid, fx.value_name
            q = "projectUid=$puid&imageUid=$uid&valueName=$vn&popType=flow"
            cols = String.(JSON3.read(api_gating_channels(HTTP.Request("GET", "/api/gating/channels?$q"))[2]).columns)
            img() = _gating_image(puid, uid)[1]
            pts = as_df(select_cols(label_props(img(); value_name = vn), cols))
            xc, yc = sort(cols; by = c -> -length(unique(pts[!, c])))[1:2]
            base = Dict{String,Any}("projectUid" => puid, "imageUid" => uid, "valueName" => vn, "popType" => "flow")
            post(h, extra) = h(Vector{UInt8}(JSON3.write(merge(base, extra))))
            tree() = load_pop_tree(img(); value_name = vn, pop_type = "flow")
            # a rectangle over the middle of the cells: its centre at the medians, half the 5–95 % spread wide
            function mid(v)
                f = sort(Float64[x for x in v if isfinite(x)]); at(p) = f[clamp(round(Int, p * length(f)), 1, length(f))]
                (at(0.5), max(at(0.95) - at(0.05), 1.0) / 4)
            end
            (mx, dx), (my, dy) = mid(pts[!, xc]), mid(pts[!, yc])
            x0, x1 = mx - dx, mx + dx
            lin = Dict("kind" => "linear")
            rect = Dict{String,Any}("kind" => "rectangle", "x_channel" => xc, "y_channel" => yc,
                "x_transform" => lin, "y_transform" => lin, "x_min" => x0, "x_max" => x1,
                "y_min" => my - dy, "y_max" => my + dy, "rule" => Dict("kind" => "follow"))

            @test post(api_gating_pop_add, Dict{String,Any}("name" => "f", "gate" => rect))[1] == 200
            g = pop_at(tree(), "/f").gate
            @test g isa RectangleGate && g.rule.anchor !== nothing            # anchored by the save
            # the plot: the rule without its anchor, a flag, and — on this image — the drawn geometry,
            # whichever way round the axes are
            for (x, y) in ((xc, yc), (yc, xc))
                pm = JSON3.read(api_gating_plotmeta(HTTP.Request("GET", "/x?$q&x=$x&y=$y&pop=root"))[2])
                o = only(filter(o -> o.path == "/f", pm.gates))
                @test Dict(String(k) => v for (k, v) in o.rule) == Dict("kind" => "follow", "adjust" => 1.0)
                @test o.rule_ok isa Bool
                xs = x == xc ? (o.x_min, o.x_max) : (o.y_min, o.y_max)
                @test all(isapprox.(xs, (x0, x1); atol = 1e-9))
                # the peak it locked onto: on the image it was saved on, its anchor (linear axes: as is),
                # in the plot's axis order
                if o.rule_ok
                    a = g.rule.anchor
                    @test collect(o.rule_peak) ≈ (x == xc ? [a[1], a[2]] : [a[2], a[1]])
                else
                    @test !haskey(o, :rule_peak)
                end
            end
            # where it would land on each image, before any copy: drawn on that image's same parent
            rq = "projectUid=$puid&imageUid=$uid&valueName=$vn&path=/f&x=$xc&y=$yc&targets=$uid,nope"
            st, b = api_gating_rule_preview(HTTP.Request("GET", "/x?$rq"))
            @test st == 200
            ims = JSON3.read(b).images
            @test [i.uid for i in ims] == [uid, "nope"]
            @test !isempty(ims[1].png) && only(ims[1].gates).path == "/f" && ims[1].rule_ok isa Bool
            @test ims[2].error == "Image not found"
            @test api_gating_rule_preview(HTTP.Request("GET", "/x?$q&path=/nothere&x=$xc&y=$yc&targets=$uid"))[1] == 404
            # track gating: its rule runs on per-track rows, which a cell picture cannot show
            tq = replace(rq, "imageUid=" => "popType=track&imageUid=")
            @test api_gating_rule_preview(HTTP.Request("GET", "/x?$tq"))[1] == 400
            # dragged on the plot: the plot's spec carries no anchor, and the save anchors it again
            moved = merge(rect, Dict("x_min" => x0 - 1.0))
            @test post(api_gating_pop_set_gate, Dict{String,Any}("path" => "/f", "gate" => moved))[1] == 200
            g2 = pop_at(tree(), "/f").gate
            @test g2.x_min == x0 - 1.0 && g2.rule.anchor !== nothing
            # switched off: a plain rectangle again
            plain = filter(kv -> kv[1] != "rule", rect)
            @test post(api_gating_pop_set_gate, Dict{String,Any}("path" => "/f", "gate" => plain))[1] == 200
            @test pop_at(tree(), "/f").gate.rule === nothing

            # Claude's add-gate: the rule is accepted on a shape, anchored like a GUI one; a dry run too
            ids = Dict{String,Any}("projectUid" => puid, "imageUid" => uid, "valueName" => vn)
            add(obj) = _post(api_gating_pop_add_gate, merge(ids, obj))
            st, b = add(Dict("name" => "cf", "gate" => rect, "dryRun" => true))
            @test st == 200 && JSON3.read(b).gate.rule.anchor !== nothing
            st, b = add(Dict("name" => "cf", "gate" => rect))
            @test st == 200 && pop_at(tree(), "/cf").gate.rule.anchor !== nothing
            st, b = add(Dict("name" => "bad", "gate" => merge(rect, Dict("rule" => Dict("kind" => "valley", "lo" => 0, "hi" => 1)))))
            @test st == 422 && occursin("follow", String(JSON3.read(b).error))
            @test !has_pop(tree(), "/bad")
        finally
            Cecelia.cecelia_conf()["dirs"]["projects"] = old
        end
    end
end
