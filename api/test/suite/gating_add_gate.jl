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
        m = load_pop_tree(img; value_name = "B", pop_type = "flow")
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
        m = load_pop_tree(img; value_name = "B", pop_type = "flow")
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
        @test pop_at(load_pop_tree(img; value_name = "B", pop_type = "flow"), "/prop").gate.x_max == 50.0
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
        m = load_pop_tree(img; value_name = "B", pop_type = "flow")
        @test st == 200 && !has_pop(m, "/p2") && has_pop(m, "/mine") && has_pop(m, "/prop/kept")
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end

# A ✦ gate stops being Claude's once the user edits or copies it (`updatedBy`), or once a task has read it — a
# clustering on it is a snapshot of its cells, so moving the gate would leave that result out of step.
# Both refuse with a 403 and write nothing; showing/hiding is not an edit.
@testset "API: own-gate routes refuse what the user edited or ran a task on" begin
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
        function call(route, obj)
            r = handle_http(HTTP.Request("POST", route, claude), Vector{UInt8}(JSON3.write(merge(ids, obj))))
            (r[1], JSON3.read(r[2]))
        end
        img = Cecelia.init_object("testpr", "KDIeEm")
        sidecar() = read(gating_path(img._dir, "B"), String)
        flow(o) = merge(ids, Dict{String,Any}("popType" => "flow"), o)

        # ── adoption ──
        @test call("/api/gating/pop/add-gate", Dict("name" => "seen", "gate" => rect(0.0, 500.0)))[1] == 200
        @test _post(api_gating_pop_update, flow(Dict("path" => "/seen", "show" => false)))[1] == 200
        @test call("/api/gating/pop/set-own-gate", Dict("path" => "/seen", "gate" => rect(0.0, 400.0)))[1] == 200

        @test call("/api/gating/pop/add-gate", Dict("name" => "edited", "gate" => rect(0.0, 500.0)))[1] == 200
        @test _post(api_gating_pop_set_gate, flow(Dict("path" => "/edited", "gate" => rect(0.0, 300.0))))[1] == 200
        p = pop_at(load_pop_tree(img; value_name = "B", pop_type = "flow"), "/edited")
        @test p.created_by["via"] == "claude" && p.updated_by["via"] == "app"
        snap = sidecar()
        st, e = call("/api/gating/pop/set-own-gate", Dict("path" => "/edited", "gate" => rect(0.0, 1.0)))
        @test st == 403 && e.code == "not_claude_made" && collect(e.populations) == ["/edited"]
        @test call("/api/gating/pop/delete-own", Dict("path" => "/edited"))[1] == 403
        @test sidecar() == snap

        @test call("/api/gating/pop/add-gate", Dict("name" => "renamed", "gate" => rect(0.0, 500.0)))[1] == 200
        @test _post(api_gating_pop_rename, flow(Dict("path" => "/renamed", "newName" => "mine")))[1] == 200
        @test call("/api/gating/pop/delete-own", Dict("path" => "/mine"))[1] == 403
        # Claude moving its own gate records its own edit, and the gate stays Claude's
        st, r = call("/api/gating/pop/set-own-gate", Dict("path" => "/seen", "gate" => rect(0.0, 300.0)))
        @test st == 200 && String(r.updatedBy.via) == "claude"
        # a user's GUI gate is never stamped by an edit — its sidecar keeps its shape
        @test _post(api_gating_pop_add, flow(Dict("name" => "plain", "gate" => rect(0.0, 500.0))))[1] == 200
        @test _post(api_gating_pop_set_gate, flow(Dict("path" => "/plain", "gate" => rect(0.0, 200.0))))[1] == 200
        @test pop_at(load_pop_tree(img; value_name = "B", pop_type = "flow"), "/plain").updated_by === nothing

        # ── a task read it ──
        @test call("/api/gating/pop/add-gate", Dict("name" => "used", "gate" => rect(0.0, 500.0)))[1] == 200
        @test call("/api/gating/pop/add-gate", Dict("name" => "sub", "parent" => "/used", "gate" => rect(0.0, 100.0)))[1] == 200
        # a failed run left nothing that depends on it
        append_run_log!(img, "clustPops.cluster", "B", "failed", Dict("popsToCluster" => ["B/used/sub"]))
        @test call("/api/gating/pop/set-own-gate", Dict("path" => "/used/sub", "gate" => rect(0.0, 90.0), "dryRun" => true))[1] == 200
        # a done one does — on the child, so the parent is refused too (moving it moves the child's cells)
        append_run_log!(img, "clustPops.cluster", "B", "done", Dict("popsToCluster" => ["B/used/sub/_tracked"]))
        snap = sidecar()
        st, e = call("/api/gating/pop/set-own-gate", Dict("path" => "/used", "gate" => rect(0.0, 50.0), "dryRun" => true))
        @test st == 403 && e.code == "used_by_runs" && collect(e.populations) == ["/used/sub"] &&
              collect(e.tasks) == ["clustPops.cluster"]
        st, e = call("/api/gating/pop/delete-own", Dict("path" => "/used/sub"))
        @test st == 403 && e.code == "used_by_runs"
        @test sidecar() == snap
        # another segmentation's population of the same path is not this one
        append_run_log!(img, "clustPops.cluster", "T", "done", Dict("popsToCluster" => ["T/seen"]))
        @test call("/api/gating/pop/delete-own", Dict("path" => "/seen", "dryRun" => true))[1] == 200

        # ── copied to another image: the copies are the user's ──
        proj = joinpath(dir, "testpr")
        tgt = joinpath(proj, "1", "TgtImg")
        cp(joinpath(proj, "1", "KDIeEm"), tgt)
        rm(joinpath(tgt, "gating"); recursive = true, force = true)
        rm(joinpath(tgt, "runlog.json"); force = true)
        ccid = joinpath(tgt, "ccid.json")
        write(ccid, replace(read(ccid, String), "\"uid\": \"KDIeEm\"" => "\"uid\": \"TgtImg\""))
        @test _post(api_gating_copy, flow(Dict("toImageUids" => ["TgtImg"])))[1] == 200
        st, e = call("/api/gating/pop/delete-own", Dict("imageUid" => "TgtImg", "path" => "/seen", "dryRun" => true))
        @test st == 403 && e.code == "not_claude_made"
        # the source is not re-saved by a copy, so its proposal is still Claude's
        @test call("/api/gating/pop/delete-own", Dict("path" => "/seen", "dryRun" => true))[1] == 200
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end

# `POST /api/gating/pop/add-boolean` — add-gate's twin for a combination ("qc but not OTI"). The new pop's
# cells are exactly parent ∖ the excluded gate (no cell lost at a polygon's edge, none double-counted),
# it follows that gate when the user moves it, it carries the same stamp so delete-own treats it as
# Claude's, and every refusal happens before anything is written.
@testset "API: add-boolean is ADD-ONLY — one stamped combination, exact complement" begin
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
        stats(pop) = JSON3.read(api_gating_stats(HTTP.Request("GET",
            "/x?projectUid=testpr&imageUid=KDIeEm&valueName=B&pop=$pop"))[2])
        img = Cecelia.init_object("testpr", "KDIeEm")
        sidecar() = read(gating_path(img._dir, "B"), String)
        # the user's tree: qc, and a gate inside it
        @test _post(api_gating_pop_add, merge(ids, Dict("popType" => "flow", "name" => "qc", "gate" => rect(0.0, 1e6))))[1] == 200
        mid = stats("/qc")
        @test _post(api_gating_pop_add, merge(ids, Dict("popType" => "flow", "name" => "OTI", "parent" => "/qc",
                                                         "gate" => rect(0.0, 300.0))))[1] == 200
        oti = stats("/qc/OTI")
        @test 0 < oti.count < mid.count

        # dry run: the numbers, nothing written
        snap = sidecar()
        st, dry = call("/api/gating/pop/add-boolean", Dict("parent" => "/qc", "name" => "non-OTI",
                                                           "not" => ["/qc/OTI"], "dryRun" => true))
        @test st == 200 && dry.dryRun == true && dry.path == "/qc/non-OTI" && sidecar() == snap
        st, r = call("/api/gating/pop/add-boolean", Dict("parent" => "/qc", "name" => "non-OTI", "not" => ["/qc/OTI"]))
        @test st == 200 && r.dryRun == false && (r.count, r.parentCount) == (dry.count, dry.parentCount)
        # the exact complement: every qc cell is in OTI or non-OTI, never both
        @test r.parentCount == mid.count && r.count + oti.count == mid.count && r.outside == oti.count
        @test r.gate === nothing && String(r.boolean.op) == "and" && isempty(r.boolean.pops) &&
              collect(r.boolean.not) == ["/qc/OTI"]
        @test String(r.createdBy.via) == "claude"
        # stored as the GUI stores a combination, plus the stamp — and get_populations shows it
        m = load_pop_tree(img; value_name = "B", pop_type = "flow")
        p = pop_at(m, "/qc/non-OTI")
        @test p.boolean_op == BOOL_AND && p.boolean_not == ["/qc/OTI"] && p.created_by["via"] == "claude"
        summ = Cecelia._pop_summary(p)
        @test summ.boolean == boolean_spec(p) && summ.gate === nothing && summ.filter === nothing
        @test pop_at(m, "/qc/OTI").created_by === nothing          # the user's gate untouched

        # it follows the gate it excludes: the user widens OTI, non-OTI shrinks to match
        @test _post(api_gating_pop_set_gate, merge(ids, Dict("path" => "/qc/OTI", "gate" => rect(0.0, 600.0))))[1] == 200
        @test stats("/qc/non-OTI").count == mid.count - stats("/qc/OTI").count

        # OR of two terms, and a child gated under the combination (the copied tree's order)
        st, o = call("/api/gating/pop/add-boolean", Dict("name" => "either", "op" => "or", "pops" => ["/qc/OTI", "/qc/non-OTI"]))
        @test st == 200 && o.count == mid.count
        @test call("/api/gating/pop/add-gate", Dict("parent" => "/qc/non-OTI", "name" => "sub", "gate" => rect(0.0, 1e6)))[1] == 200

        # moving a gate moves every combination that reads it — wherever it sits, not just under it — so
        # set-own-gate refuses when one of those was read by a task, or is the user's
        @test call("/api/gating/pop/add-gate", Dict("name" => "cg", "gate" => rect(0.0, 500.0)))[1] == 200
        @test call("/api/gating/pop/add-boolean", Dict("name" => "notcg", "not" => ["/cg"]))[1] == 200
        @test call("/api/gating/pop/set-own-gate", Dict("path" => "/cg", "gate" => rect(0.0, 400.0), "dryRun" => true))[1] == 200
        append_run_log!(img, "clustPops.cluster", "B", "done", Dict("popsToCluster" => ["B/notcg"]))
        st, e = call("/api/gating/pop/set-own-gate", Dict("path" => "/cg", "gate" => rect(0.0, 400.0), "dryRun" => true))
        @test st == 403 && e.code == "used_by_runs" && collect(e.populations) == ["/notcg"]
        @test _post(api_gating_pop_add, merge(ids, Dict("popType" => "flow", "name" => "usernot",
                    "boolean" => Dict("op" => "and", "pops" => String[], "not" => ["/cg"]))))[1] == 200
        st, e = call("/api/gating/pop/set-own-gate", Dict("path" => "/cg", "gate" => rect(0.0, 400.0), "dryRun" => true))
        @test st == 403 && e.code == "not_claude_made" && collect(e.populations) == ["/usernot"]

        # ── refusals: nothing written ──
        snap = sidecar()
        refuse(obj) =(res = call("/api/gating/pop/add-boolean", obj); (res[1], get(res[2], :code, nothing)))
        @test refuse(Dict("parent" => "/qc", "name" => "non-OTI", "not" => ["/qc/OTI"]))[2] == "pop_exists"
        @test refuse(Dict("parent" => "/nope", "name" => "x", "not" => ["/qc/OTI"]))[1] == 404
        @test refuse(Dict("parent" => "/qc", "name" => "x", "not" => ["/qc/nope"]))[1] == 404
        @test refuse(Dict("parent" => "/qc", "name" => "x"))[1] == 422                     # no term at all
        @test refuse(Dict("parent" => "/qc", "name" => "x", "op" => "xor", "pops" => ["/qc/OTI"]))[1] == 422
        @test refuse(Dict("parent" => "/qc", "name" => "x", "not" => "/qc/OTI"))[1] == 422  # a list, not a string
        @test refuse(Dict("parent" => "/qc", "name" => "a/b", "not" => ["/qc/OTI"]))[1] == 422
        @test refuse(Dict("parent" => "/qc", "name" => "_x", "not" => ["/qc/OTI"]))[1] == 422
        @test refuse(Dict("parent" => "root", "name" => "loop", "pops" => ["/loop"]))[1] == 422   # itself
        # not a gate: set-own-gate refuses it (delete and add again instead)
        @test call("/api/gating/pop/set-own-gate", Dict("path" => "/qc/non-OTI", "gate" => rect(0.0, 1.0)))[1] == 422
        # delete-own: refused while `either` combines it, then — `either` gone — the subtree goes whole
        st, e = call("/api/gating/pop/delete-own", Dict("path" => "/qc/non-OTI"))
        @test st == 409 && e.code == "boolean_dependents" && occursin("/either", String(e.error))
        @test sidecar() == snap
        @test call("/api/gating/pop/delete-own", Dict("path" => "/either"))[1] == 200
        st, d = call("/api/gating/pop/delete-own", Dict("path" => "/qc/non-OTI"))
        @test st == 200 && collect(d.deleted) == ["/qc/non-OTI", "/qc/non-OTI/sub"]
        @test has_pop(load_pop_tree(img; value_name = "B", pop_type = "flow"), "/qc/OTI")
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end

# `POST /api/gating/pop/add-filter` — add-gate's twin for a filter population ("area > a and
# mean_intensity_0 <= b"). The new pop's cells are exactly the parent's cells meeting every condition — counted
# here straight off the cell table, not through the engine — it is stored as the GUI's filter form stores
# one, plus the stamp, get_populations shows every condition, and every refusal happens before anything is written.
@testset "API: add-filter is ADD-ONLY — one stamped filter pop, every condition AND-ed" begin
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
        # the user's qc gate: mean_intensity_0 in [0, 1e6]
        @test _post(api_gating_pop_add, merge(ids, Dict("popType" => "flow", "name" => "qc", "gate" => rect(0.0, 1e6))))[1] == 200

        # the expected cells, from the table itself: in qc, area > median and mean_intensity_0 <= its median
        df = label_props(img; value_name = "B") |> v -> select_cols(v, ["mean_intensity_0", "area"]) |> as_df
        x, a = df.mean_intensity_0, df.area
        inqc = (0.0 .<= x .<= 1e6) .& (0.0 .<= a .<= 1e9)
        mid(v) = sort(v)[cld(length(v), 2)]                    # a midpoint, so both cuts keep cells
        amed, xmed = mid(a[inqc]), mid(x[inqc])
        want = count(inqc .& (a .> amed) .& (x .<= xmed))
        @test 0 < want < count(inqc)
        conds = [Dict("measure" => "area", "fun" => "gt", "values" => amed),
                 Dict("measure" => "mean_intensity_0", "fun" => "lte", "values" => [xmed])]   # one value, list-wrapped

        # dry run: the numbers, nothing written
        snap = sidecar()
        st, dry = call("/api/gating/pop/add-filter", Dict("parent" => "/qc", "name" => "big-dim",
                                                          "conditions" => conds, "dryRun" => true))
        @test st == 200 && dry.dryRun == true && dry.path == "/qc/big-dim" && sidecar() == snap
        st, r = call("/api/gating/pop/add-filter", Dict("parent" => "/qc", "name" => "big-dim", "conditions" => conds))
        @test st == 200 && r.dryRun == false && (r.count, r.parentCount) == (dry.count, dry.parentCount)
        @test r.count == want && r.parentCount == count(inqc) && r.outside == count(inqc) - want
        @test r.gate === nothing && r.boolean === nothing && String(r.createdBy.via) == "claude"
        @test [String(c.fun) for c in r.filter.conditions] == ["gt", "lte"]
        @test r.filter.conditions[2].values isa Real && r.filter.conditions[2].values ≈ xmed   # unwrapped from [xmed]; JSON round-trip

        # stored as the GUI's filter form stores one, plus the stamp — and get_populations shows BOTH conditions
        m = load_pop_tree(img; value_name = "B", pop_type = "flow")
        p = pop_at(m, "/qc/big-dim")
        @test p.gate === nothing && length(p.filter_conditions) == 2 && p.filter_measure == "area" &&
              p.created_by["via"] == "claude"
        summ = Cecelia._pop_summary(p)
        @test summ.filter == filter_spec(p) && length(summ.filter["conditions"]) == 2 && summ.boolean === nothing
        @test pop_at(m, "/qc").created_by === nothing          # the user's gate untouched

        # `in` over a column: a list of values, membership by the same table read
        lab = df.label[inqc][1:3]
        st, i = call("/api/gating/pop/add-filter", Dict("name" => "picked",
                     "conditions" => [Dict("measure" => "label", "fun" => "in", "values" => lab)]))
        @test st == 200 && i.count == 3
        # a number sent as a string is the number (the form's rule): "not label X" drops exactly one cell
        st, s = call("/api/gating/pop/add-filter", Dict("name" => "notone",
                     "conditions" => [Dict("measure" => "label", "fun" => "neq", "values" => string(lab[1]))]))
        @test st == 200 && s.count == length(df.label) - 1
        st, s = call("/api/gating/pop/add-filter", Dict("name" => "picked-str",
                     "conditions" => [Dict("measure" => "label", "fun" => "in", "values" => string.(lab))]))
        @test st == 200 && s.count == 3

        # ── refusals: nothing written ──
        snap = sidecar()
        refuse(obj) = (res = call("/api/gating/pop/add-filter", obj); (res[1], get(res[2], :code, nothing)))
        one = [Dict("measure" => "area", "fun" => "gt", "values" => 1)]
        @test refuse(Dict("parent" => "/qc", "name" => "big-dim", "conditions" => one))[2] == "pop_exists"
        @test refuse(Dict("parent" => "/nope", "name" => "x", "conditions" => one))[1] == 404
        @test refuse(Dict("parent" => "/qc", "name" => "x"))[1] == 422                                  # no condition
        @test refuse(Dict("parent" => "/qc", "name" => "x", "conditions" => []))[1] == 422
        @test refuse(Dict("parent" => "/qc", "name" => "x",
                          "conditions" => [Dict("measure" => "nope_col", "fun" => "gt", "values" => 1)]))[1] == 422
        @test refuse(Dict("parent" => "/qc", "name" => "x",
                          "conditions" => [Dict("measure" => "area", "fun" => "between", "values" => 1)]))[1] == 422
        @test refuse(Dict("parent" => "/qc", "name" => "x",
                          "conditions" => [Dict("measure" => "area", "fun" => "gt", "values" => "big")]))[1] == 422
        @test refuse(Dict("parent" => "/qc", "name" => "x",
                          "conditions" => [Dict("measure" => "area", "fun" => "gt", "values" => [1, 2])]))[1] == 422
        @test refuse(Dict("parent" => "/qc", "name" => "x",
                          "conditions" => [Dict("measure" => "area", "fun" => "in", "values" => [])]))[1] == 422
        @test refuse(Dict("parent" => "/qc", "name" => "_x", "conditions" => one))[1] == 422
        # not a gate: set-own-gate refuses it (delete and add again instead)
        @test call("/api/gating/pop/set-own-gate", Dict("path" => "/qc/big-dim", "gate" => rect(0.0, 1.0)))[1] == 422
        @test sidecar() == snap
        st, d = call("/api/gating/pop/delete-own", Dict("path" => "/qc/big-dim"))
        @test st == 200 && collect(d.deleted) == ["/qc/big-dim"]
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end

# add-gate's 1D and split proposals: a range is one population; a bisector / slice / quadrant adds a
# population per region (`name+` / `name-`, `name++` …), linked by one group, all or none — and the
# regions tile the parent (docs/POPULATION.md → Split gates, 1D gates).
@testset "API: add-gate proposes ranges, bisectors, slices and quadrants" begin
  fx = fixture_for(:cells)                       # any cell table: the assertions count its own cells
  if fx === nothing
    @test_skip "cell-table fixture missing"
  else
    dir = mktempdir()
    pid, iid, vn = basename(fx.project), fx.uid, fx.value_name
    cp(fx.project, joinpath(dir, pid))
    old = Cecelia.cecelia_conf()["dirs"]["projects"]
    try
        Cecelia.cecelia_conf()["dirs"]["projects"] = dir
        ids = Dict{String,Any}("projectUid" => pid, "imageUid" => iid, "valueName" => vn)
        q = "projectUid=$pid&imageUid=$iid&valueName=$vn"
        add(obj) = _post(api_gating_pop_add_gate, merge(ids, obj))
        count(p) = JSON3.read(api_gating_stats(HTTP.Request("GET", "/x?$q&pop=$(HTTP.escapeuri(p))"))[2]).count
        cols = JSON3.read(api_gating_channels(HTTP.Request("GET", "/x?$q&popType=flow"))[2]).columns
        summ(x, y) = JSON3.read(api_gating_summary(HTTP.Request("GET", "/x?$q&x=$x&y=$y&pop=root"))[2])
        # the first column with a spread (min < median, so a range up to the median is not empty)
        xc = String(cols[something(findfirst(c -> (s = summ(c, c).x; s.min < s.quantiles[:p50]), cols), 1)])
        yc = String(first(c for c in cols if String(c) != xc))
        sm = summ(xc, yc)
        mid, lo, hi = sm.x.quantiles[:p50], sm.x.min, sm.x.max
        lin = Dict("kind" => "linear")
        one(kind; kw...) = Dict{String,Any}("kind" => kind, "x_channel" => xc, "x_transform" => lin,
                                            (String(k) => v for (k, v) in kw)...)
        root = count("root")

        # a range: one population (y_channel may be left out — one channel)
        st, b = add(Dict("name" => "rng", "gate" => one("range"; lo = lo, hi = mid)))
        @test st == 200 && JSON3.read(b).path == "/rng" && isempty(JSON3.read(b).regions)
        # a bisector following the valley in the whole range: two halves that tile the parent
        st, b = add(Dict("name" => "cut", "gate" => one("bisector"; cut = mid,
                                                         rule = Dict("kind" => "valley", "lo" => lo, "hi" => hi))))
        r = JSON3.read(b)
        @test st == 200 && [x.path for x in r.regions] == ["/cut+", "/cut-"]
        @test count("/cut+") + count("/cut-") == root
        @test r.gate.rule.kind == "valley" && r.gate.side == 1
        # a slice (vertical line through the median, drawn upward → + is to its right) and a quadrant
        sl = Dict{String,Any}("kind" => "slice", "x_channel" => xc, "y_channel" => yc,
                              "vertices" => [[mid, 0.0], [mid, 1.0]])
        st, b = add(Dict("name" => "sl", "gate" => sl))
        @test st == 200 && count("/sl+") + count("/sl-") == root
        # left of the line is strict, so a cell ON it is on the right (+) side
        @test count("/sl+") == Base.count(>=(mid), filter(isfinite, Cecelia.pop_plot_cols(
            Cecelia.init_object(pid, iid), "flow", "root", [xc]; value_name = vn)[1]))
        qd = Dict{String,Any}("kind" => "quadrant", "x_channel" => xc, "y_channel" => yc,
                              "centre" => [mid, sm.y.quantiles[:p50]])
        st, b = add(Dict("name" => "q", "gate" => qd))
        @test st == 200 && length(JSON3.read(b).regions) == 4
        @test sum(count("/q$s") for s in ("++", "-+", "--", "+-")) == root
        # sectors are named by where they point, whichever arm the list starts at (here +y)
        qg = Cecelia.gate_from_spec(Dict{String,Any}("kind" => "quadrant", "x_channel" => "a", "y_channel" => "b",
            "x_transform" => lin, "y_transform" => lin, "centre" => [0, 0], "arms" => [[0, 1], [-1, 0], [0, -1], [1, 0]]))
        @test _proposed_regions(qg) == [("-+", 1), ("--", 2), ("+-", 3), ("++", 4)]
        # Claude moving one half of ITS bisector: the half keeps its side, the other half follows, they still
        # tile the parent; a split region can't become another kind
        claude = ["X-Cecelia-Client" => "claude", "X-Cecelia-Session" => "s1"]
        call(route, obj) = (r = handle_http(HTTP.Request("POST", route, claude), Vector{UInt8}(JSON3.write(merge(ids, obj))));
                            (r[1], JSON3.read(r[2])))
        @test call("/api/gating/pop/add-gate", Dict("name" => "own", "gate" => one("bisector"; cut = mid)))[1] == 200
        q1 = (lo + mid) / 2
        st, r = call("/api/gating/pop/set-own-gate", Dict("path" => "/own-", "gate" => one("bisector"; cut = q1)))
        @test st == 200 && r.gate.side == -1 && r.gate.cut ≈ q1
        @test count("/own+") + count("/own-") == root
        @test call("/api/gating/pop/set-own-gate", Dict("path" => "/own+", "gate" => one("range"; lo = lo, hi = mid)))[1] == 422
        # a slice moved with its line drawn the other way round: `+` still names the higher-x side
        slc(x, v) = Dict{String,Any}("kind" => "slice", "x_channel" => xc, "y_channel" => yc, "vertices" => v)
        @test call("/api/gating/pop/add-gate", Dict("name" => "os", "gate" => slc(mid, [[mid, 0.0], [mid, 1.0]])))[1] == 200
        st, _ = call("/api/gating/pop/set-own-gate", Dict("path" => "/os+", "gate" => slc(q1, [[q1, 1.0], [q1, 0.0]])))
        xs = filter(isfinite, Cecelia.pop_plot_cols(Cecelia.init_object(pid, iid), "flow", "root", [xc]; value_name = vn)[1])
        @test st == 200 && count("/os+") == Base.count(>=(q1), xs) && count("/os+") + count("/os-") == root
        # swapped axes would mirror the regions: refused
        sw = Dict{String,Any}("kind" => "slice", "x_channel" => yc, "y_channel" => xc, "vertices" => [[0.0, q1], [1.0, q1]])
        @test call("/api/gating/pop/set-own-gate", Dict("path" => "/os+", "gate" => sw))[1] == 422
        # one taken region name refuses the whole split; a bad rule window is a 422
        img = Cecelia.init_object(pid, iid)
        n0 = length(pop_paths(load_pop_tree(img; value_name = vn, pop_type = "flow")))
        @test add(Dict("name" => "cut", "gate" => one("bisector"; cut = mid)))[1] == 409
        @test add(Dict("name" => "c2", "gate" => one("bisector"; cut = mid, rule = Dict("lo" => "a"))))[1] == 422
        @test add(Dict("name" => "c3", "gate" => merge(one("range"; lo = 0, hi = 1), Dict("y_channel" => yc))))[1] == 422
        @test length(pop_paths(load_pop_tree(img; value_name = vn, pop_type = "flow"))) == n0
    finally
        Cecelia.cecelia_conf()["dirs"]["projects"] = old
    end
  end
end
