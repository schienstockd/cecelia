# `POST /api/gating/pop/add-gate` — the observer's `add_gate`: ONE proposed gate, additive only.
# On the testpr fixture (a copy, in a temp projects dir): the new pop is stored like a GUI gate plus a
# `createdBy` stamp, its membership is computed, the user's existing pops are untouched, and every
# refusal (404 / 409 / 422) happens before anything is written.

@testset "API: add-gate is ADD-ONLY — one stamped gate, existing pops untouched" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    cp(api_fixture("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        ids = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm", "valueName" => "B")
        rect(x0, x1, y0, y1; xt = Dict("kind" => "linear")) = Dict{String,Any}("kind" => "rectangle",
            "x_channel" => "mean_intensity_0", "y_channel" => "area", "x_transform" => xt,
            "x_min" => x0, "x_max" => x1, "y_min" => y0, "y_max" => y1)
        # the user's own tree: a GUI gate with a child, drawn through the general route
        @test _post(api_gating_pop_add, merge(ids, Dict("popType" => "flow", "name" => "dim",
                    "gate" => rect(0.0, 1000.0, 0.0, 1e9), "colour" => "#e11d48")))[1] == 200
        @test _post(api_gating_pop_add, merge(ids, Dict("popType" => "flow", "name" => "small", "parent" => "/dim",
                    "gate" => rect(0.0, 1000.0, 0.0, 200.0))))[1] == 200
        img = Cecelia.init_object("testpr", "KDIeEm")
        sidecar() = read(gating_path(img._dir, "B"), String)
        before = JSON3.read(sidecar(), Dict{String,Any})
        # a GUI gate is not stamped — the sidecar a person writes keeps its shape
        @test !haskey(before["populations"][1], "createdBy")

        # ── the proposal, as the observer sends it (X-Cecelia-Client: claude + the caller headers) ──
        sid = "578e0b74-c216-49f3-b993-745a618bdd7e"
        claude_hdrs = ["X-Cecelia-Client" => "claude", "X-Cecelia-Model" => "claude-opus-5-5",
                       "X-Cecelia-Session" => sid]
        add_gate(obj; hdrs = claude_hdrs) = handle_http(HTTP.Request("POST", "/api/gating/pop/add-gate", hdrs),
                                                         Vector{UInt8}(JSON3.write(merge(ids, obj))))
        # the polygon's right edge at the parent's median in asinh space — what gate_plot/summary report
        sm = JSON3.read(api_gating_summary(HTTP.Request("GET", "/x?projectUid=testpr&imageUid=KDIeEm&" *
            "valueName=B&x=mean_intensity_0&y=area&pop=/dim&xt=asinh&xcof=150"))[2])
        lo, mid = sm.x.min - 1, sm.x.quantiles[:p50]
        poly = Dict{String,Any}("kind" => "polygon", "x_channel" => "mean_intensity_0", "y_channel" => "area",
            "x_transform" => Dict("kind" => "asinh", "cof" => 150),   # the plot routes' key spelling
            "vertices" => [[lo, -1.0], [mid, -1.0], [mid, 1e9], [lo, 1e9]])
        st, b = add_gate(Dict("parent" => "/dim", "name" => "proposed", "gate" => poly))
        @test st == 200
        r = JSON3.read(b)
        @test r.ok && r.path == "/dim/proposed" && r.parent == "/dim" && r.valueName == "B"
        # counts over the PARENT, recomputed for the new pop — the same numbers /api/gating/stats gives
        stats = JSON3.read(api_gating_stats(HTTP.Request("GET",
            "/x?projectUid=testpr&imageUid=KDIeEm&valueName=B&pop=/dim/proposed"))[2])
        @test r.count == stats.count && r.parentCount == stats.parentCount
        @test r.outside == r.parentCount - r.count && 0 < r.count < r.parentCount
        @test r.gate.x_transform.kind == "asinh" && r.gate.x_transform.cofactor == 150.0   # stored spelling
        @test String(r.createdBy.via) == "claude" && String(r.createdBy.model) == "claude-opus-5-5" &&
              String(r.createdBy.session) == sid

        # stored exactly like a GUI gate — `gate_spec` shape — plus the stamp
        after = JSON3.read(sidecar(), Dict{String,Any})
        dim = after["populations"][1]
        node = only(filter(c -> c["name"] == "proposed", dim["children"]))
        @test node["gate"] == gate_spec(gate_from_spec(node["gate"]))
        @test node["createdBy"]["via"] == "claude" && node["createdBy"]["model"] == "claude-opus-5-5"
        @test node["show"] == true && occursin(r"^#[0-9a-f]{6}$", node["colour"])
        # the user's pops untouched: same gates, uids, colours, children (bar the one added)
        strip_new(p) = merge(p, Dict("children" => [strip_new(c) for c in p["children"] if c["name"] != "proposed"]))
        @test strip_new(dim) == before["populations"][1]
        # the stamp survives a reload (round-trips through from_tree / to_tree)
        m = load_pop_map(img; value_name = "B", pop_type = "flow")
        @test pop_at(m, "/dim/proposed").created_by["via"] == "claude"
        @test pop_at(m, "/dim").created_by === nothing

        # ── refusals: nothing written ──
        snap = sidecar()
        refuse(obj; kw...) = (res = add_gate(obj; kw...); (res[1], String(JSON3.read(res[2]).error)))
        st, msg = refuse(Dict("parent" => "/dim", "name" => "proposed", "gate" => poly))
        @test st == 409 && occursin("already exists", msg)
        @test JSON3.read(add_gate(Dict("parent" => "/dim", "name" => "proposed", "gate" => poly))[2]).code == "pop_exists"
        st, msg = refuse(Dict("parent" => "/nope", "name" => "x", "gate" => poly))
        @test st == 404 && occursin("Population not found: /nope", msg) && occursin("/dim", msg)
        st, msg = refuse(Dict("parent" => "root", "name" => "x", "gate" => poly, "valueName" => "nope"))
        @test st == 404 && occursin("No segmentation \"nope\"", msg)
        bad_rect = rect(1.0, 0.0, 0.0, 1.0)
        @test refuse(Dict("name" => "x", "gate" => bad_rect))[1] == 422                       # x_min > x_max
        @test refuse(Dict("name" => "x", "gate" => merge(poly, Dict("vertices" => [[0, 0], [1, 1]]))))[1] == 422
        st, msg = refuse(Dict("name" => "x", "gate" => merge(poly, Dict("x_channel" => "intensity"))))
        @test st == 422 && occursin("`intensity` is not a column", msg)
        @test refuse(Dict("name" => "x", "gate" => merge(poly, Dict("y_transform" => Dict("kind" => "sqrt")))))[1] == 422
        @test refuse(Dict("name" => "x", "gate" => merge(poly, Dict("kind" => "ellipse"))))[1] == 422
        @test refuse(Dict("name" => "_tracked", "gate" => poly))[1] == 422                    # reserved
        @test refuse(Dict("name" => "a/b", "gate" => poly))[1] == 422
        @test refuse(Dict("name" => "x", "gate" => poly, "colour" => "red"))[1] == 422
        @test refuse(Dict("name" => "x"))[1] == 422                                            # no gate
        @test sidecar() == snap

        # a name another pop type already holds in this segmentation → 409 pop_name_conflict
        @test _post(api_gating_pop_add, merge(ids, Dict("popType" => "track", "name" => "trk",
                    "gate" => Dict{String,Any}("kind" => "rectangle", "x_channel" => "mean_intensity_0.mean",
                    "y_channel" => "area.mean", "x_min" => 0.0, "x_max" => 1.0, "y_min" => 0.0, "y_max" => 1.0))))[1] == 200
        res = add_gate(Dict("name" => "trk", "gate" => poly))
        @test res[1] == 409 && JSON3.read(res[2]).code == "pop_name_conflict"

        # an app request (no Claude header) is stamped `via: app` — the stamp is who asked, never assumed
        st, b = add_gate(Dict("name" => "byapp", "gate" => poly); hdrs = Pair{String,String}[])
        @test st == 200 && String(JSON3.read(b).createdBy.via) == "app"
        # …and the general route never stamps: it is the GUI's, and no MCP server is allowed it
        st, _ = handle_http(HTTP.Request("POST", "/api/gating/pop/add", claude_hdrs),
            Vector{UInt8}(JSON3.write(merge(ids, Dict("popType" => "flow", "name" => "auto", "gate" => poly)))))
        @test st == 200
        m = load_pop_map(img; value_name = "B", pop_type = "flow")
        @test pop_at(m, "/auto").created_by === nothing && pop_at(m, "/dim/small").created_by === nothing
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end

# `set-own-gate` / `delete-own` — the MCP servers' only way to change a gate, and only one Claude drew:
# every population at and below the path must carry a Claude `createdBy`, else 403 naming the user's.
# `dryRun` on all three writes answers with the numbers and leaves the sidecar byte-identical.
@testset "API: own-gate routes change ONLY what Claude drew" begin
  if !api_have_fixture(api_fixture("testpr"))
    @test_skip "testpr fixture missing"
  else
    dir = mktempdir()
    cp(api_fixture("testpr"), joinpath(dir, "testpr"))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        ids = Dict{String,Any}("projectUid" => "testpr", "imageUid" => "KDIeEm", "valueName" => "B")
        rect(x0, x1) = Dict{String,Any}("kind" => "rectangle", "x_channel" => "mean_intensity_0", "y_channel" => "area",
                                        "x_min" => x0, "x_max" => x1, "y_min" => 0.0, "y_max" => 1e9)
        claude = ["X-Cecelia-Client" => "claude", "X-Cecelia-Session" => "s1"]
        function call(route, obj; hdrs = claude)
            r = handle_http(HTTP.Request("POST", route, hdrs), Vector{UInt8}(JSON3.write(merge(ids, obj))))
            (r[1], JSON3.read(r[2]))
        end
        img = Cecelia.init_object("testpr", "KDIeEm")
        sidecar() = read(gating_path(img._dir, "B"), String)
        # the user's gate, and Claude's proposal beside it
        @test _post(api_gating_pop_add, merge(ids, Dict("popType" => "flow", "name" => "mine", "gate" => rect(0.0, 1e6))))[1] == 200
        snap = sidecar()
        # dry run of the add: the numbers, nothing written — and the same numbers the real add reports
        st, dry = call("/api/gating/pop/add-gate", Dict("name" => "prop", "gate" => rect(0.0, 500.0), "dryRun" => true))
        @test st == 200 && dry.dryRun == true && dry.path == "/prop"
        @test sidecar() == snap
        st, real = call("/api/gating/pop/add-gate", Dict("name" => "prop", "gate" => rect(0.0, 500.0)))
        @test st == 200 && real.dryRun == false && (real.count, real.parentCount) == (dry.count, dry.parentCount)

        # move it — dry first (counts change, file does not), then for real
        snap = sidecar()
        st, d = call("/api/gating/pop/set-own-gate", Dict("path" => "/prop", "gate" => rect(0.0, 50.0), "dryRun" => true))
        @test st == 200 && d.count < real.count && sidecar() == snap
        st, r = call("/api/gating/pop/set-own-gate", Dict("path" => "/prop", "gate" => rect(0.0, 50.0)))
        @test st == 200 && r.count == d.count && r.gate.x_max == 50.0
        @test pop_at(load_pop_map(img; value_name = "B", pop_type = "flow"), "/prop").gate.x_max == 50.0
        @test String(r.createdBy.via) == "claude"                     # moving keeps who drew it

        # the user's gate: neither moved nor removed, and nothing written
        snap = sidecar()
        st, e = call("/api/gating/pop/set-own-gate", Dict("path" => "/mine", "gate" => rect(0.0, 1.0)))
        @test st == 403 && e.code == "not_claude_made" && collect(e.populations) == ["/mine"]
        st, e = call("/api/gating/pop/delete-own", Dict("path" => "/mine"))
        @test st == 403 && e.code == "not_claude_made"
        @test call("/api/gating/pop/set-own-gate", Dict("path" => "/nope", "gate" => rect(0.0, 1.0)))[1] == 404
        @test call("/api/gating/pop/set-own-gate", Dict("path" => "/prop", "gate" => rect(1.0, 0.0)))[1] == 422
        @test sidecar() == snap

        # once the user gates under Claude's proposal, that subtree is theirs too
        @test _post(api_gating_pop_add, merge(ids, Dict("popType" => "flow", "name" => "kept", "parent" => "/prop",
                                                         "gate" => rect(0.0, 10.0))))[1] == 200
        snap = sidecar()
        st, e = call("/api/gating/pop/delete-own", Dict("path" => "/prop"))
        @test st == 403 && collect(e.populations) == ["/prop/kept"] && occursin("/prop/kept", String(e.error))
        @test call("/api/gating/pop/set-own-gate", Dict("path" => "/prop", "gate" => rect(0.0, 5.0)))[1] == 403
        @test sidecar() == snap

        # a clean Claude subtree goes whole — dry run lists it and keeps it, then it is gone
        @test call("/api/gating/pop/add-gate", Dict("name" => "p2", "gate" => rect(0.0, 500.0)))[1] == 200
        @test call("/api/gating/pop/add-gate", Dict("name" => "p2c", "parent" => "/p2", "gate" => rect(0.0, 100.0)))[1] == 200
        snap = sidecar()
        st, d = call("/api/gating/pop/delete-own", Dict("path" => "/p2", "dryRun" => true))
        @test st == 200 && collect(d.deleted) == ["/p2", "/p2/p2c"] && sidecar() == snap
        st, _ = call("/api/gating/pop/delete-own", Dict("path" => "/p2"))
        m = load_pop_map(img; value_name = "B", pop_type = "flow")
        @test st == 200 && !has_pop(m, "/p2") && has_pop(m, "/mine") && has_pop(m, "/prop/kept")
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end
